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
    try {ok, get_cardholder_data(decode_token(Token))} catch
        not_found ->
            raise(#'CardDataNotFound'{});
        locked ->
            raise(#'KeyringLocked'{})
    end;
handle_function('GetSessionCardData', [Token, Session], _Context, _Opts) ->
    _ = assert_keyring_available(),
    try
        {ok, get_card_data(decode_token(Token), decode_session(Session))}
    catch
        not_found ->
            raise(#'CardDataNotFound'{});
        locked ->
            raise(#'KeyringLocked'{})
    end;
handle_function('PutCardData', [CardData], _Context, _Opts) ->
    _ = assert_keyring_available(),
    try
        {PaymentSystem, BIN, MaskedPan} = cds_card_data:validate(CardData),
        {Token, Session} = put_card_data(CardData),
        BankCard = #'BankCard'{
            token = encode_token(Token),
            payment_system = PaymentSystem,
            bin = BIN,
            masked_pan = MaskedPan
        },
        {ok, #'PutCardDataResult'{
            bank_card = BankCard,
            session = encode_session(Session)
        }}
    catch
        invalid_card_data ->
            raise(#'InvalidCardData'{});
        locked ->
            raise(#'KeyringLocked'{})
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

decode_token(Token) ->
    base62_decode(Token).

encode_token(Token) ->
    base62_encode(Token).

decode_session(Session) ->
    base62_decode(Session).

encode_session(Session) ->
    base62_encode(Session).

base62_encode(Data) ->
    genlib_format:format_int_base(binary:decode_unsigned(Data), 62).

base62_decode(Data) ->
    genlib_string:pad_left(binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)), 0, 16).

get_cardholder_data(Token) ->
    Marshalled = cds:get_cardholder_data(Token),
    cds_card_data:unmarshall(Marshalled).

get_card_data(Token, Session) ->
    {MarshalledCardData, Cvv} = cds:get_card_data(Token, Session),
    cds_card_data:unmarshall(MarshalledCardData, Cvv).

put_card_data(CardData) ->
    cds:put_card_data(unique(CardData), cds_card_data:marshall(CardData)).

unique(CardData) ->
    cds_card_data:unique(CardData).

assert_keyring_available() ->
    case cds_keyring_manager:get_state() of
        unlocked ->
            ok;
        locked ->
            raise(#'KeyringLocked'{})
    end.

-spec raise(_) -> no_return().

raise(Exception) ->
    woody_error:raise(business, Exception).

% test

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec isomorphic_marshalling_test_() -> _.

isomorphic_marshalling_test_() ->
    Vs = [
        crypto:strong_rand_bytes(16),
        << <<C>> || C <- lists:seq(1, 16) >>,
        <<1:16/integer-unit:8>>,
        <<0:16/integer-unit:8>>
    ],
    [?_assertEqual(decode_token(encode_token(V)), V) || V <- Vs].

-endif.
