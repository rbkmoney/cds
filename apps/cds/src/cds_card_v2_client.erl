-module(cds_card_v2_client).

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_base_thrift.hrl").

-export([get_card_data/2]).
-export([put_card_data/2]).
-export([put_card_data/3]).
-export([get_session_data/2]).
-export([put_card/2]).
-export([put_session/3]).

%%
%% Internal types
%%

-type decoded_card_data() :: #{
    pan := binary(),
    exp_date := #{
        month := integer(),
        year := integer()
    },
    cardholder_name => binary() | undefined,
    cvv => binary() | undefined
}.

-type decoded_session_data() :: #{
    auth_data := {auth_3ds, #{
        cryptogram => binary(),
        eci => binary() | undefined
    }} |
    {card_security_code, #{
        value => binary()
    }}
}.

-type bank_card() :: #{
    token := cds_proto_base_thrift:'Token'(),
    bin := binary(),
    last_digits := binary()
}.

-type put_card_data_result() :: #{
    bank_card := bank_card(),
    session_id := dmsl_domain_thrift:'PaymentSessionID'()
}.

%%
%% API
%%

-spec get_card_data(cds:token(), woody:url()) ->
    decoded_card_data() | {error, card_data_not_found}.
get_card_data(Token, RootUrl) ->
    try cds_woody_client:call(card_v2, 'GetCardData', [Token], RootUrl) of
        EncodedCardData ->
            decode_card_data(EncodedCardData)
    catch
        #'CardDataNotFound'{} ->
            {error, card_data_not_found}
    end.

-spec put_card_data(decoded_card_data(), woody:url()) ->
    put_card_data_result() | {error, {invalid_card_data, binary()}}.
put_card_data(CardData, RootUrl) ->
    put_card_data(CardData, undefined, RootUrl).

-spec put_card_data(decoded_card_data(), decoded_session_data() | undefined, woody:url()) ->
    put_card_data_result() | {error, {invalid_card_data, binary()}}.
put_card_data(CardData, SessionData, RootUrl) ->
    try cds_woody_client:call(card_v2, 'PutCardData',
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
    decoded_session_data() | {error, session_data_not_found}.
get_session_data(Session, RootUrl) ->
    try cds_woody_client:call(card_v2, 'GetSessionData', [Session], RootUrl) of
        SessionData ->
            decode_session_data(SessionData)
    catch
        #'SessionDataNotFound'{} ->
            {error, session_data_not_found}
    end.

-spec put_card(decoded_card_data(), woody:url()) ->
    #{bank_card := bank_card()} | {error, {invalid_card_data, binary()}}.
put_card(CardData, RootUrl) ->
    try cds_woody_client:call(card_v2, 'PutCard', [encode_card_data(CardData)], RootUrl) of
        #'PutCardResult'{bank_card = BankCard} ->
            #{
                bank_card => decode_bank_card(BankCard)
            }
    catch
        #'InvalidCardData'{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

-spec put_session(cds:session(), decoded_session_data(), woody:url()) ->
    ok.
put_session(SessionID, SessionData, RootUrl) ->
    cds_woody_client:call(card_v2, 'PutSession', [SessionID, encode_session_data(SessionData)], RootUrl).

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
    #'BankCard'{
        token = Token,
        bin = Bin,
        last_digits = LastDigits
    }) ->
    #{
        token => Token,
        bin => Bin,
        last_digits => LastDigits
    }.

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