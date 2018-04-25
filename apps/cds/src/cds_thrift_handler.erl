-module(cds_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function('Init', [Threshold, Count], _Context, _Opts) when Threshold =< Count ->
    try cds_keyring_manager:initialize(Threshold, Count) of
        Shares ->
            {ok, Shares}
    catch
        already_initialized ->
            raise(#'KeyringExists'{})
    end;
handle_function('Lock', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:lock()} catch
        not_initialized ->
            raise(#'NoKeyring'{});
        locked ->
            {ok, ok}
    end;
handle_function('Unlock', [Share], _Context, _Opts) ->
    case cds_keyring_manager:unlock(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {unlocked, #'Unlocked'{}}}
    end;
handle_function('Rotate', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:rotate()} catch
        not_initialized ->
            raise(#'NoKeyring'{});
        locked ->
            raise(#'KeyringLocked'{})
    end;

handle_function('GetCardData', [Token], _Context, _Opts) ->
    try
        {ok, encode_cardholder_data(
            get_cardholder_data(
                cds_utils:decode_token(Token)
            )
        )}
    catch
        not_found ->
            raise(#'CardDataNotFound'{});
        Reason when Reason == locked; Reason == not_initialized ->
            raise_keyring_unavailable(Reason)
    end;
handle_function('GetSessionCardData', [Token, Session], _Context, _Opts) ->
    try
        {ok, encode_card_data(
            get_card_data(
                cds_utils:decode_token(Token),
                cds_utils:decode_session(Session)
            )
        )}
    catch
        not_found ->
            raise(#'CardDataNotFound'{});
        Reason when Reason == locked; Reason == not_initialized ->
            raise_keyring_unavailable(Reason)
    end;
handle_function('PutCardData', [CardData, SessionData], _Context, _Opts) ->
    OwnCardData = decode_card_data(CardData),
    OwnSessionData = decode_session_data(SessionData),
    try
        case cds_card_data:validate(OwnCardData, OwnSessionData) of
            {ok, CardInfo} ->
                {Token, Session} = put_card_data(OwnCardData, OwnSessionData),
                BankCard = #'domain_BankCard'{
                    token          = cds_utils:encode_token(Token),
                    payment_system = maps:get(payment_system, CardInfo),
                    bin            = maps:get(iin           , CardInfo),
                    masked_pan     = maps:get(last_digits   , CardInfo)
                },
                {ok, #'PutCardDataResult'{
                    bank_card      = BankCard,
                    session_id     = cds_utils:encode_session(Session)
                }};
            {error, _} ->
                raise(#'InvalidCardData'{})
        end
    catch
        Reason when Reason == locked; Reason == not_initialized ->
            raise_keyring_unavailable(Reason)
    end;
handle_function('GetSessionData', [Session], _Context, _Opts) ->
    try
        {ok, encode_session_data(
            get_session_data(
                cds_utils:decode_session(Session)
            )
        )}
    catch
        not_found ->
            raise(#'SessionDataNotFound'{});
        Reason when Reason == locked; Reason == not_initialized ->
            raise_keyring_unavailable(Reason)
    end.

% local

decode_card_data(#'CardData'{
    pan             = PAN,
    exp_date        = #'ExpDate'{month = Month, year = Year},
    cardholder_name = CardholderName
}) ->
    #{
        cardnumber => PAN,
        exp_date   => {Month, Year},
        cardholder => CardholderName
    }.

decode_session_data(#'SessionData'{auth_data = AuthData}) ->
    #{auth_data => decode_auth_data(AuthData)}.

decode_auth_data({card_security_code, #'CardSecurityCode'{value = Value}}) ->
    #{type => cvv, value => Value};
decode_auth_data({auth_3ds, #'Auth3DS'{cryptogram = Cryptogram, eci = ECI}}) ->
    genlib_map:compact(#{type => '3ds', cryptogram => Cryptogram, eci => ECI}).

encode_card_data({CardData, #{auth_data := AuthData}}) ->
    V = encode_cardholder_data(CardData),
    case maps:get(type, AuthData) of
        cvv ->
            V#'CardData'{cvv = maps:get(value, AuthData)};
        '3ds' ->
            V
    end.

encode_cardholder_data(#{
    cardnumber := PAN,
    exp_date   := {Month, Year},
    cardholder := CardholderName
}) ->
    #'CardData'{
        pan             = PAN,
        exp_date        = #'ExpDate'{month = Month, year = Year},
        cardholder_name = CardholderName,
        cvv             = <<>>
    }.

encode_session_data(#{auth_data := AuthData}) ->
    #'SessionData'{auth_data = encode_auth_data(AuthData)}.

encode_auth_data(#{type := cvv, value := Value}) ->
    {card_security_code, #'CardSecurityCode'{value = Value}};
encode_auth_data(#{type := '3ds', cryptogram := Cryptogram} = Data) ->
    ECI = genlib_map:get(eci, Data),
    {auth_3ds, #'Auth3DS'{cryptogram = Cryptogram, eci = ECI}}.

%

get_cardholder_data(Token) ->
    CardholderData = cds:get_cardholder_data(Token),
    cds_card_data:unmarshal_cardholder_data(CardholderData).

get_card_data(Token, Session) ->
    {CardholderData, SessionData} = cds:get_card_data(Token, Session),
    {cds_card_data:unmarshal_cardholder_data(CardholderData), cds_card_data:unmarshal_session_data(SessionData)}.

put_card_data(CardholderData, SessionData) ->
    cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_session_data(SessionData)
    }).

get_session_data(Session) ->
    SessionData = cds:get_session_data(Session),
    cds_card_data:unmarshal_session_data(SessionData).

-spec raise_keyring_unavailable(locked | not_initialized) ->
    no_return().

raise_keyring_unavailable(KeyringState) ->
    woody_error:raise(system, {internal, resource_unavailable, get_details(KeyringState)}).

get_details(locked) ->
    <<"Keyring is locked">>;
get_details(not_initialized) ->
    <<"Keyring is not initialized">>.

-spec raise(_) -> no_return().

raise(Exception) ->
    woody_error:raise(business, Exception).
