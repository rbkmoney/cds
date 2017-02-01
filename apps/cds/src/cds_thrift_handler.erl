-module(cds_thrift_handler).
-behaviour(woody_server_thrift_handler).
-behaviour(woody_event_handler).

-include("cds_cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%% woody_event_handler callback
-export([handle_event/4]).


-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('Init', [Threshold, Count], _Context, _Opts) ->
    try cds:init_keyring(Threshold, Count) of
        Shares ->
            {ok, Shares}
    catch
        Exists when Exists =:= already_exists; Exists =:= locked ->
            woody_error:raise(business, #'KeyringExists'{})
    end;
handle_function('Unlock', [Share], _Context, _Opts) ->
    case cds:unlock_keyring(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {unlocked, #'Unlocked'{}}}
    end;
handle_function('Rotate', [], _Context, _Opts) ->
    try {ok, cds:rotate_keyring()} catch
        locked ->
            woody_error:raise(business, #'KeyringLocked'{})
    end;
handle_function('Lock', [], _Context, _Opts) ->
    {ok, cds:lock_keyring()};
handle_function('GetCardData', [Token], _Context, _Opts) ->
    try {ok, cds:get_card_data(base62_decode(Token))} catch
        not_found ->
            woody_error:raise(business, #'CardDataNotFound'{});
        locked ->
            woody_error:raise(business, #'KeyringLocked'{})
    end;
handle_function('GetSessionCardData', [Token, Session], _Context, _Opts) ->
    try {ok, cds:get_session_card_data(base62_decode(Token), base62_decode(Session))} catch
        not_found ->
            woody_error:raise(business, #'CardDataNotFound'{});
        locked ->
            woody_error:raise(business, #'KeyringLocked'{})
    end;
handle_function('PutCardData', [CardData], _Context, _Opts) ->
    try
        {PaymentSystem, BIN, MaskedPan} = cds_card_data:validate(CardData),
        {Token, Session} = cds:put_card_data(CardData),
        BankCard = #'BankCard'{
            token = base62_encode(Token),
            payment_system = PaymentSystem,
            bin = BIN,
            masked_pan = MaskedPan
        },
        {ok, #'PutCardDataResult'{
            bank_card = BankCard,
            session = base62_encode(Session)
        }}
    catch
        invalid_card_data ->
            woody_error:raise(business, #'InvalidCardData'{});
        locked ->
            woody_error:raise(business, #'KeyringLocked'{})
    end.

-spec handle_event(
    woody_event_handler:event(),
    woody:rpc_id(),
    woody_event_handler:event_meta(),
    woody:options()
) -> _.
handle_event(EventType, RpcID, #{status := error, class := Class, reason := Reason, stack := Stack}, _Opts) ->
    lager:error(
        construct_md(RpcID),
        "[server] ~s with ~s:~p at ~s",
        [EventType, Class, Reason, genlib_format:format_stacktrace(Stack, [newlines])]
    );
handle_event(EventType, RpcID, EventMeta, _Opts) ->
    lager:debug(construct_md(RpcID), "[server] ~s: ~p", [EventType, EventMeta]).

construct_md(undefined) ->
    [];
construct_md(Map = #{}) ->
    maps:to_list(Map).

% local

base62_encode(Data) ->
    genlib_format:format_int_base(binary:decode_unsigned(Data), 62).

base62_decode(Data) ->
    binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)).
