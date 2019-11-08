-module(cds_card_v2_client).

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_base_thrift.hrl").

-export([get_card_data/2]).
-export([put_card_data/3]).
-export([get_session_data/2]).
-export([put_card/2]).
-export([put_session/3]).
-export([get_test_card/1]).
-export([get_test_card/2]).

%%
%% Internal types
%%

-type card_data() :: #{
    pan := binary(),
    exp_date := #{
        month := integer(),
        year := integer()
    },
    cardholder_name => binary() | undefined
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
    token := cds_proto_base_thrift:'Token'(),
    bin := binary(),
    last_digits := binary()
}.

-type put_card_data_result() :: #{
    bank_card := bank_card(),
    session_id := cds_proto_base_thrift:'PaymentSessionID'()
}.

%%
%% API
%%

-spec get_card_data(cds:token(), woody:url()) ->
    card_data() | {error, card_data_not_found}.
get_card_data(Token, RootUrl) ->
    try call(card_v2, 'GetCardData', [Token], RootUrl) of
        EncodedCardData ->
            decode_card_data(EncodedCardData)
    catch
        #cds_CardDataNotFound{} ->
            {error, card_data_not_found}
    end.

-spec put_card_data(card_data(), session_data() | undefined, woody:url()) ->
    put_card_data_result() | {error, {invalid_card_data, binary()}}.
put_card_data(CardData, SessionData, RootUrl) ->
    try call(card_v2, 'PutCardData',
        [encode_card_data(CardData), encode_session_data(SessionData)], RootUrl) of
        #cds_PutCardDataResult{bank_card = BankCard, session_id = PaymentSessionId} ->
            #{
                bank_card => decode_bank_card(BankCard),
                session_id => PaymentSessionId
            }
    catch
        #cds_InvalidCardData{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

-spec get_session_data(cds:session(), woody:url()) ->
    session_data() | {error, session_data_not_found}.
get_session_data(Session, RootUrl) ->
    try call(card_v2, 'GetSessionData', [Session], RootUrl) of
        SessionData ->
            decode_session_data(SessionData)
    catch
        #cds_SessionDataNotFound{} ->
            {error, session_data_not_found}
    end.

-spec put_card(card_data(), woody:url()) ->
    #{bank_card := bank_card()} | {error, {invalid_card_data, binary()}}.
put_card(CardData, RootUrl) ->
    try call(card_v2, 'PutCard', [encode_card_data(CardData)], RootUrl) of
        #cds_PutCardResult{bank_card = BankCard} ->
            #{
                bank_card => decode_bank_card(BankCard)
            }
    catch
        #cds_InvalidCardData{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

-spec put_session(cds:session(), session_data(), woody:url()) ->
    ok.
put_session(SessionID, SessionData, RootUrl) ->
    call(card_v2, 'PutSession', [SessionID, encode_session_data(SessionData)], RootUrl).

encode_card_data(#{pan := Pan}) ->
    #cds_PutCardData{
        pan = Pan
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
    #cds_SessionData{
        auth_data = {auth_3ds, #cds_Auth3DS{
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
    #cds_SessionData{
        auth_data = {card_security_code, #cds_CardSecurityCode{
            value = Value
        }}
    }.

decode_bank_card(
    #cds_BankCard{
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
    #cds_CardData{
        pan = Pan
    }) ->
    DecodedCardData = #{
        pan => Pan
    },
    genlib_map:compact(DecodedCardData).

decode_session_data(
    #cds_SessionData{
        auth_data = {auth_3ds, #cds_Auth3DS{
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
    #cds_SessionData{
        auth_data = {card_security_code, #cds_CardSecurityCode{
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

-spec get_test_card(binary() | undefined) -> card_data().

get_test_card(CVV) ->
    get_test_card(
    #{
        pan => <<"5321301234567892">>
    }, CVV).

-spec get_test_card(card_data(), binary() | undefined) -> card_data().

get_test_card(#{pan := PAN}, _CVV) ->
    #{pan => PAN}.
