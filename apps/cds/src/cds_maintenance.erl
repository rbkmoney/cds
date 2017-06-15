-module(cds_maintenance).

-export([refresh_sessions_created_at/0]).
-export([refresh_cvv_encryption/0]).
-export([refresh_cardholder_encryption/0]).

-define(REFRESH_BATCH, 500).


%% api

-spec refresh_sessions_created_at() -> ok.

refresh_sessions_created_at() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_storage:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) -> cds_storage:refresh_session_created_at(Key) end,
    refresh(Getter, Refresher).

-spec refresh_cvv_encryption() -> ok.

refresh_cvv_encryption() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_storage:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) ->
        CVV = cds:get_cvv(Key),
        cds:update_cvv(Key, CVV)
    end,
    refresh(Getter, Refresher).

-spec refresh_cardholder_encryption() -> ok.

refresh_cardholder_encryption() ->
    ok = assert_keyring_available(),
    Getter = fun(Continuation) -> cds_storage:get_tokens(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) ->
        CardData = cds:get_cardholder_data(Key),
        cds:update_cardholder_data(Key, CardData)
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
