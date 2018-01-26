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
        Exists when Exists =:= already_exists; Exists =:= locked ->
            raise(#'KeyringExists'{})
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
        locked ->
            raise(#'KeyringLocked'{})
    end;
handle_function('Lock', [], _Context, _Opts) ->
    {ok, cds_keyring_manager:lock()};

handle_function('GetCardData', [Token], _Context, _Opts) ->
    _ = assert_keyring_available(),
    try
        {ok, encode_cardholder_data(
            get_cardholder_data(
                cds_utils:decode_token(Token)
            )
        )}
    catch
        not_found ->
            raise(#'CardDataNotFound'{});
        locked ->
            raise_keyring_unavailable(locked)
    end;
handle_function('GetSessionCardData', [Token, Session], _Context, _Opts) ->
    _ = assert_keyring_available(),
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
        locked ->
            raise_keyring_unavailable(locked)
    end;
handle_function('PutCardData', [V], _Context, _Opts) ->
    _ = assert_keyring_available(),
    {CardholderData, CVV} = decode_card_data(V),
    try cds_card_data:validate(CardholderData, CVV) of
        {ok, CardInfo} ->
            {Token, Session} = put_card_data(CardholderData, CVV),
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
    catch
        locked ->
            raise_keyring_unavailable(locked)
    end.

% local

decode_card_data(#'CardData'{
    pan             = PAN,
    exp_date        = #'ExpDate'{month = Month, year = Year},
    cardholder_name = CardholderName,
    cvv             = CVV
}) ->
    {#{
        cardnumber => PAN,
        exp_date   => {Month, Year},
        cardholder => CardholderName
    }, CVV}.

encode_card_data({CardData, CVV}) ->
    V = encode_cardholder_data(CardData),
    V#'CardData'{cvv = CVV}.

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

%

get_cardholder_data(Token) ->
    CardholderData = cds:get_cardholder_data(Token),
    cds_card_data:unmarshal_cardholder_data(CardholderData).

get_card_data(Token, Session) ->
    {CardholderData, CVV} = cds:get_card_data(Token, Session),
    {cds_card_data:unmarshal_cardholder_data(CardholderData), cds_card_data:unmarshal_cvv(CVV)}.

put_card_data(CardholderData, CVV) ->
    cds:put_card_data({
        cds_card_data:marshal_cardholder_data(CardholderData),
        cds_card_data:marshal_cvv(CVV)
    }).

assert_keyring_available() ->
    case cds_keyring_manager:get_state() of
        unlocked ->
            ok;
        State ->
            raise_keyring_unavailable(State)
    end.

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
