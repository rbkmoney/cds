-module(cds_maintenance).

-export([refresh_sessions_created_at/0]).
-export([refresh_cvv_encryption/0]).
-export([refresh_cardholder_encryption/0]).

-define(REFRESH_BATCH, 500).


%% api

refresh_sessions_created_at() ->
    Getter = fun(Continuation) -> cds:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) -> cds:refresh_session_created_at(Key) end,
    refresh(Getter, Refresher).

refresh_cvv_encryption() ->
    Getter = fun(Continuation) -> cds:get_sessions(?REFRESH_BATCH, Continuation) end,
    Refresher = fun(Key) ->
        CVV = cds:get_cvv(Key),
        cds:update_cvv(Key, CVV)
    end,
    refresh(Getter, Refresher).

refresh_cardholder_encryption() ->
    Getter = fun(Continuation) -> cds:get_tokens(?REFRESH_BATCH, Continuation) end,
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
