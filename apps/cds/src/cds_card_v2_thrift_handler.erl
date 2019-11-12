-module(cds_card_v2_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("cds_proto/include/cds_proto_storage_thrift.hrl").
-include_lib("cds_proto/include/cds_proto_base_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(OperationID, Args, Context, Opts) ->
    scoper:scope(
        card_data_v2,
        cds_thrift_handler_utils:filter_fun_exceptions(fun() -> handle_function_(OperationID, Args, Context, Opts) end)
    ).

handle_function_('PutCardData', [CardData, SessionData], _Context, _Opts) ->
    OwnCardData = decode_card_data(CardData),
    OwnSessionData = decode_session_data(SessionData),
    try
        case cds_card_data:validate(OwnCardData, OwnSessionData) of
            {ok, CardInfo} ->
                {Token, Session} = put_card_data(OwnCardData, OwnSessionData),
                BankCard = #cds_BankCard{
                    token       = cds_utils:encode_token({Token, OwnCardData}),
                    bin         = maps:get(iin           , CardInfo),
                    last_digits = maps:get(last_digits   , CardInfo)
                },
                {ok, #cds_PutCardDataResult{
                    bank_card   = BankCard,
                    session_id  = Session
                }};
            {error, ValidationError} ->
                cds_thrift_handler_utils:raise(#cds_InvalidCardData{
                    reason      = cds_thrift_handler_utils:map_validation_error(ValidationError)
                })
        end
    catch
        no_keyring ->
            cds_thrift_handler_utils:raise_keyring_unavailable()
    end;

handle_function_('PutCard', [CardData], _Context, _Opts) ->
    OwnCardData = decode_card_data(CardData),
    try
        case cds_card_data:validate(OwnCardData) of
            {ok, CardInfo} ->
                Token = put_card(OwnCardData),
                BankCard = #cds_BankCard{
                    token          = cds_utils:encode_token({Token, OwnCardData}),
                    bin            = maps:get(iin           , CardInfo),
                    last_digits    = maps:get(last_digits   , CardInfo)
                },
                {ok, #cds_PutCardResult{
                    bank_card = BankCard
                }};
            {error, ValidationError} ->
                cds_thrift_handler_utils:raise(#cds_InvalidCardData{
                    reason = cds_thrift_handler_utils:map_validation_error(ValidationError)
                })
        end
    catch
        no_keyring ->
            cds_thrift_handler_utils:raise_keyring_unavailable()
    end;

handle_function_('GetCardData', [Token], _Context, _Opts) ->
    try
        {DecodedToken, _DecodedPayload} = cds_utils:decode_token(Token),
        {ok, encode_cardholder_data(get_cardholder_data(DecodedToken))}
    catch
        not_found ->
            cds_thrift_handler_utils:raise(#cds_CardDataNotFound{});
        no_keyring ->
            cds_thrift_handler_utils:raise_keyring_unavailable()
    end;

handle_function_('PutSession', [Session, SessionData], _Context, _Opts) ->
    OwnSessionData = decode_session_data(SessionData),
    try
        ok = put_session(Session, OwnSessionData),
        {ok, ok}
    catch
        no_keyring ->
            cds_thrift_handler_utils:raise_keyring_unavailable()
    end;

handle_function_('GetSessionData', [Session], _Context, _Opts) ->
    try
        SessionData = get_session_data(Session),
        {ok, encode_session_data(SessionData)}
    catch
        not_found ->
            cds_thrift_handler_utils:raise(#cds_SessionDataNotFound{});
        no_keyring ->
            cds_thrift_handler_utils:raise_keyring_unavailable()
    end.

%%
%% Internals
%%

decode_card_data(#'cds_PutCardData'{
    pan             = PAN,
    exp_date        = ExpDate,
    cardholder_name = CardholderName
}) ->
    genlib_map:compact(
        #{
            cardnumber => PAN,
            exp_date   => decode_exp_date(ExpDate),
            cardholder => CardholderName
        }
    ).

decode_session_data(#cds_SessionData{auth_data = AuthData}) ->
    #{auth_data => decode_auth_data(AuthData)}.

decode_auth_data({card_security_code, #cds_CardSecurityCode{value = Value}}) ->
    #{type => cvv, value => Value};
decode_auth_data({auth_3ds, #cds_Auth3DS{cryptogram = Cryptogram, eci = ECI}}) ->
    genlib_map:compact(#{type => '3ds', cryptogram => Cryptogram, eci => ECI}).

encode_cardholder_data(#{cardnumber := PAN}) ->
    #cds_CardData{
        pan = PAN
    }.

encode_session_data(#{auth_data := AuthData}) ->
    #cds_SessionData{auth_data = encode_auth_data(AuthData)}.

encode_auth_data(#{type := cvv, value := Value}) ->
    {card_security_code, #cds_CardSecurityCode{value = Value}};
encode_auth_data(#{type := '3ds', cryptogram := Cryptogram} = Data) ->
    ECI = genlib_map:get(eci, Data),
    {auth_3ds, #cds_Auth3DS{cryptogram = Cryptogram, eci = ECI}}.

%

get_cardholder_data(Token) ->
    {_, CardholderData} = cds:get_cardholder_data(Token),
    cds_card_data:unmarshal_cardholder_data(CardholderData).

put_card_data(CardholderData, SessionData) ->
    cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionData)
    }).

put_card(CardholderData) ->
    cds:put_card(cds_card_data:marshal_cardholder_data(CardholderData)).

put_session(Session, SessionData) ->
    cds:put_session(Session, cds_card_data:marshal_session_data(SessionData)).

get_session_data(Session) ->
    {_, SessionData} = cds:get_session_data(Session),
    cds_card_data:unmarshal_session_data(SessionData).

decode_exp_date(undefined) ->
    undefined;
decode_exp_date(#'cds_ExpDate'{month = Month, year = Year}) ->
    {Month, Year}.
