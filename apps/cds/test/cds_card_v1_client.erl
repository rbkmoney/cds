-module(cds_card_v1_client).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_domain_thrift.hrl").

-export([get_card_data/2]).
-export([get_session_card_data/3]).
-export([put_card_data/3]).
-export([get_session_data/2]).
-export([put_card/2]).
-export([put_session/3]).
-export([get_test_credit_card/1]).
-export([get_test_credit_card/2]).

%%
%% Internal types
%%

-type card_data() :: #{
    pan := binary(),
    exp_date := #{
        month := integer(),
        year := integer()
    },
    cardholder_name => binary() | undefined,
    cvv => binary() | undefined
}.

-type session_data() :: #{
    auth_data := {auth_3ds, #{
        cryptogram => binary(),
        eci => binary() | undefined
    }} |
    {card_security_code, #{
        value => binary()
    }}
}.

-type bank_card() :: #{
    token := dmsl_domain_thrift:'Token'(),
    payment_system := atom(),
    bin := binary(),
    masked_pan := binary(),
    token_provider => atom() | undefined,
    issuer_country => atom() | undefined,
    bank_name => binary() | undefined,
    metadata => #{binary() => dmsl_msgpack_thrift:'Value'()} | undefined,
    is_cvv_empty => boolean() | undefined
}.

-type put_card_data_result() :: #{
    bank_card := bank_card(),
    session_id := dmsl_domain_thrift:'PaymentSessionID'()
}.

%%
%% API
%%

-spec get_card_data(cds:token(), woody:url()) ->
    card_data() | {error, card_data_not_found}.
get_card_data(Token, RootUrl) ->
    try call(card, 'GetCardData', [Token], RootUrl) of
        EncodedCardData ->
            decode_card_data(EncodedCardData)
    catch
        #'CardDataNotFound'{} ->
            {error, card_data_not_found}
    end.

-spec get_session_card_data(cds:token(), cds:session(), woody:url()) ->
    card_data() | {error, session_data_not_found}.
get_session_card_data(Token, Session, RootUrl) ->
    try call(card, 'GetSessionCardData', [Token, Session], RootUrl) of
        EncodedCardData ->
            decode_card_data(EncodedCardData)
    catch
        #'CardDataNotFound'{} ->
            {error, session_data_not_found}
    end.

-spec put_card_data(card_data(), session_data() | undefined, woody:url()) ->
    put_card_data_result() | {error, {invalid_card_data, binary()}}.
put_card_data(CardData, SessionData, RootUrl) ->
    try call(card, 'PutCardData',
        [encode_card_data(CardData), encode_session_data(SessionData)], RootUrl) of
        #'PutCardDataResult'{bank_card = BankCard, session_id = PaymentSessionId} ->
            #{
                bank_card => decode_bank_card(BankCard),
                session_id => PaymentSessionId
            }
    catch
        #'InvalidCardData'{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

-spec get_session_data(cds:session(), woody:url()) ->
    session_data() | {error, session_data_not_found}.
get_session_data(Session, RootUrl) ->
    try call(card, 'GetSessionData', [Session], RootUrl) of
        SessionData ->
            decode_session_data(SessionData)
    catch
        #'SessionDataNotFound'{} ->
            {error, session_data_not_found}
    end.

-spec put_card(card_data(), woody:url()) ->
    #{bank_card := bank_card()} | {error, {invalid_card_data, binary()}}.
put_card(CardData, RootUrl) ->
    try call(card, 'PutCard', [encode_card_data(CardData)], RootUrl) of
        #'PutCardResult'{bank_card = BankCard} ->
            #{
                bank_card => decode_bank_card(BankCard)
            }
    catch
        #'InvalidCardData'{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

-spec put_session(cds:session(), session_data(), woody:url()) ->
    ok.
put_session(SessionID, SessionData, RootUrl) ->
    call(card, 'PutSession', [SessionID, encode_session_data(SessionData)], RootUrl).

encode_card_data(
    #{
        pan := Pan,
        exp_date := #{
            month := ExpDateMonth,
            year := ExpDateYear
        }
    } = CardData) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = ExpDateMonth,
            year = ExpDateYear
        },
        cardholder_name = maps:get(cardholder_name, CardData, undefined),
        cvv = maps:get(cvv, CardData, undefined)
    }.

encode_session_data(undefined) ->
    undefined;
encode_session_data(
    #{
        auth_data := {auth_3ds, #{
            cryptogram := Cryptogram,
            eci := Eci
        }}
    }) ->
    #'SessionData'{
        auth_data = {auth_3ds, #'Auth3DS'{
            cryptogram = Cryptogram,
            eci = Eci
        }}
    };
encode_session_data(
    #{
        auth_data := {card_security_code, #{
            value := Value
        }}
    }) ->
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = Value
        }}
    }.

decode_bank_card(
    #'domain_BankCard'{
        token = Token,
        payment_system = PaymentSystem,
        bin = Bin,
        masked_pan = MaskedPan,
        token_provider = TokenProvider,
        issuer_country = IssuerCountry,
        bank_name = BankName,
        metadata = Metadata,
        is_cvv_empty = IsCVVEmpty
    }) ->
    DecodedBankCard = #{
        token => Token,
        payment_system => PaymentSystem,
        bin => Bin,
        masked_pan => MaskedPan,
        token_provider => TokenProvider,
        issuer_country => IssuerCountry,
        bank_name => BankName,
        metadata => Metadata,
        is_cvv_empty => IsCVVEmpty
    },
    genlib_map:compact(DecodedBankCard).

decode_card_data(
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = ExpDateMonth,
            year = ExpDateYear
        },
        cardholder_name = CardHolderName,
        cvv = CVV
    }) ->
    DecodedCardData = #{
        pan => Pan,
        exp_date => #{
            month => ExpDateMonth,
            year => ExpDateYear
        },
        cardholder_name => CardHolderName,
        cvv => CVV
    },
    genlib_map:compact(DecodedCardData).

decode_session_data(
    #'SessionData'{
        auth_data = {auth_3ds, #'Auth3DS'{
            cryptogram = Cryptogram,
            eci = Eci
        }}
    }) ->
    #{
        auth_data => {auth_3ds, #{
            cryptogram => Cryptogram,
            eci => Eci
        }}
    };
decode_session_data(
    #'SessionData'{
        auth_data = {card_security_code, #'CardSecurityCode'{
            value = Value
        }}
    }) ->
    #{
        auth_data => {card_security_code, #{
            value => Value
        }}
    }.

call(Service, Method, Args, RootUrl) ->
    cds_ct_utils:call(Service, Method, Args, RootUrl).

-spec get_test_credit_card(binary() | undefined) -> card_data().

get_test_credit_card(CVV) ->
    Card = cds_card_v2_client:get_test_credit_card(CVV),
    get_test_credit_card(Card, CVV).

-spec get_test_credit_card(card_data(), binary() | undefined) -> card_data().

get_test_credit_card(CardData, CVV) ->
    CardData#{cvv => CVV}.
