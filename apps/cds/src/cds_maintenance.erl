-module(cds_maintenance).

-export([refresh_sessions_created_at/0]).
-export([refresh_session_data_encryption/0]).

-export([refresh_cardholder_encryption/0]).
-export([get_sessions_info/0]).

-define(REFRESH_BATCH, 500).


%% api

-spec refresh_sessions_created_at() -> ok.

refresh_sessions_created_at() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_card_storage:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) -> cds_card_storage:refresh_session_created_at(Key) end,
    refresh(Getter, Refresher).

-spec refresh_session_data_encryption() -> ok.

refresh_session_data_encryption() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_card_storage:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) ->
        SessionData = cds:get_session_data(Key),
        cds:update_session_data(Key, SessionData)
    end,
    refresh(Getter, Refresher).

-spec refresh_cardholder_encryption() -> ok.

refresh_cardholder_encryption() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_card_storage:get_tokens(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) ->
        CardData = cds:get_cardholder_data(Key),
        cds:update_cardholder_data(Key, CardData)
    end,
    refresh(Getter, Refresher).

-spec get_sessions_info() -> ok.

get_sessions_info() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_card_storage:get_sessions_info(?REFRESH_BATCH, Continuation) end,
    Refresher = fun({Session, SessionInfo}) ->
        Key = cds_utils:encode_session(Session),
        case SessionInfo of
            #{lifetime := Lifetime} ->
                _ = io:fwrite("Session: ~s; Lifetime (sec): ~p~n", [Key, Lifetime]);
            #{error := Error} ->
                _ = io:fwrite("Session: ~s; Error: ~p~n", [Key, Error]);
            _ ->
                _ = io:fwrite("Session: ~s~n", [Key])
        end
    end,
    refresh(Getter, Refresher).

%% internal

refresh(Getter, Refresher) ->
    refresh(Getter, Refresher, undefined).

refresh(Getter, Refresher, Continuation0) ->
    {Keys, Continuation} = Getter(Continuation0),
    [_ = Refresher(Key) || Key <- Keys],
    case Continuation of
        undefined ->
            ok;
        _ ->
            refresh(Getter, Refresher, Continuation)
    end.

assert_keyring_available() ->
    case cds_keyring_manager:get_state() of
        unlocked ->
            ok;
        locked ->
            throw(locked)
    end.
