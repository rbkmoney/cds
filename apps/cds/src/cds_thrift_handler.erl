-module(cds_thrift_handler).
-behaviour(woody_server_thrift_handler).
-behaviour(woody_event_handler).

-include("cds_cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%% woody_event_handler callback
-export([handle_event/3]).


-spec handle_function(
    woody_t:func(),
    woody_server_thrift_handler:args(),
    woody_client:context(),
    woody_server_thrift_handler:handler_opts()
) -> {ok | {ok, woody_server_thrift_handler:result()}, woody_client:context()} | no_return().
handle_function('Init', {Threshold, Count}, Context, _Opts) ->
    try cds:init_keyring(Threshold, Count) of
        Shares ->
            {{ok, Shares}, Context}
    catch
        Exists when Exists =:= already_exists; Exists =:= locked ->
            throw({#'KeyringExists'{}, Context})
    end;
handle_function('Unlock', {Share}, Context, _Opts) ->
    case cds:unlock_keyring(Share) of
        {more, More} ->
            {{ok, {more_keys_needed, More}}, Context};
        ok ->
            {{ok, {ok, #'Ok'{}}}, Context}
    end;
handle_function('Rotate', {}, Context, _Opts) ->
    try cds:rotate_keyring() of
        ok ->
            {ok, Context}
    catch
        locked ->
            throw({#'KeyringLocked'{}, Context})
    end;
handle_function('Lock', {}, Context, _Opts) ->
    ok = cds:lock_keyring(),
    {ok, Context};
handle_function('GetCardData', {Token}, Context, _Opts) ->
    try cds:get_card_data(base62_decode(Token)) of
        CardData ->
            {{ok, CardData}, Context}
    catch
        not_found ->
            throw({#'NotFound'{}, Context});
        locked ->
            throw({#'KeyringLocked'{}, Context})
    end;
handle_function('GetSessionCardData', {Token, Session}, Context, _Opts) ->
    try cds:get_session_card_data(base62_decode(Token), base62_decode(Session)) of
            CardData ->
                {{ok, CardData}, Context}
    catch
        not_found ->
            throw({#'NotFound'{}, Context});
        locked ->
            throw({#'KeyringLocked'{}, Context})
    end;
handle_function('PutCardData', {CardData}, Context, _Opts) ->
    try
        {PaymentSystem, BIN, MaskedPan} = cds_card_data:validate(CardData),
        {Token, Session} = cds:put_card_data(CardData),
        BankCard = #'BankCard'{
            token = base62_encode(Token),
            payment_system = PaymentSystem,
            bin = BIN,
            masked_pan = MaskedPan
        },
        {{ok, #'PutCardDataResult'{bank_card = BankCard, session = base62_encode(Session)}}, Context}
    catch
        invalid_card_data ->
            throw({#'InvalidCardData'{}, Context});
        locked ->
            throw({#'KeyringLocked'{}, Context})
    end.

handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}) ->
    lager:error(
        maps:to_list(RpcID),
        "[server] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );

handle_event(EventType, RpcID, EventMeta) ->
    lager:debug(maps:to_list(RpcID), "[server] ~s: ~p", [EventType, EventMeta]).

% local

base62_encode(Data) ->
    genlib_format:format_int_base(binary:decode_unsigned(Data), 62).

base62_decode(Data) ->
    binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)).
