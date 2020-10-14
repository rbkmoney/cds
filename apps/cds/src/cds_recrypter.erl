-module(cds_recrypter).

-behaviour(cds_periodic_job).

%% api

-export([start_link/1]).

%% cds_periodic_job callbacks

-export([init/1]).
-export([handle_timeout/1]).

-define(DEFAULT_INTERVAL, 3000).
-define(DEFAULT_BATCH_SIZE, 5000).

-type state() :: #{
    subject := session | carddata,
    continuation := undefined | term()
}.

-spec start_link(state()) -> {ok, pid()} | {error, Reason :: any()}.
start_link(Options) ->
    cds_periodic_job:start_link(?MODULE, [Options]).

-spec init(_) -> {ok, non_neg_integer(), state()}.
init([#{subject := Subject}]) when Subject =:= session; Subject =:= carddata ->
    _ = logger:info("Starting recrypter for ~s ...", [Subject]),
    _ = cds_utils:logtag_process(subject, Subject),
    {ok, get_interval(), #{subject => Subject, continuation => undefined}}.

-spec handle_timeout(state()) -> {ok, done | more, state()} | {error, Reason :: any(), state()}.
handle_timeout(State = #{subject := Subject, continuation := Continuation0}) ->
    _ = logger:info("Starting recrypting", []),
    case process_recrypting(Subject, get_batch_size(), Continuation0) of
        {ok, done} ->
            _ = logger:info("Recrypted all"),
            {ok, done, State#{continuation => undefined}};
        {ok, more, Continuation} ->
            _ = logger:info("Recrypted some items. Need to repeat"),
            {ok, more, State#{continuation => Continuation}};
        {error, Error} ->
            _ = logger:error("Recrypting error: ~p", [Error]),
            {error, Error, State#{continuation => undefined}}
    end.

%% Internals

process_recrypting(Subject, BatchSize, Continuation) ->
    try
        {Intervals, Meta} = cds_keyring:get_outdated_keys(),
        process_recrypting(Subject, Intervals, BatchSize, Continuation, Meta)
    catch
        throw:Reason ->
            {error, Reason};
        error:{woody_error, {_, Class, _}} = Reason when Class == resource_unavailable; Class == result_unknown ->
            {error, Reason}
    end.

process_recrypting(_, [], _, _, _) ->
    {ok, done};
process_recrypting(_, _, Left, Continuation, _) when Left =< 0 ->
    {ok, more, Continuation};
process_recrypting(Subject, [Interval | Rest], BatchSize, Continuation0, Meta) ->
    {Items, Continuation} = get_data_by_key_id_between(Subject, Interval, BatchSize, Continuation0),
    Count = length(Items),
    _ = logger:info("Got ~p ~s items to recrypt", [Count, Subject]),
    _ = [recrypt_item(Subject, ItemID, Meta) || ItemID <- Items],
    process_recrypting(Subject, Rest, BatchSize - Count, Continuation, Meta).

get_data_by_key_id_between(session, {From, To}, BatchSize, Continuation) ->
    cds_card_storage:get_sessions_by_key_id_between(From, To, BatchSize, Continuation);
get_data_by_key_id_between(carddata, {From, To}, BatchSize, Continuation) ->
    cds_card_storage:get_tokens_by_key_id_between(From, To, BatchSize, Continuation).

recrypt_item(Subject, ItemID, Meta) ->
    try
        _ = recrypt(Subject, ItemID, Meta),
        _ = logger:debug("Recrypted ~s with id = ~s", [Subject, encode(Subject, ItemID)]),
        ok
    catch
        throw:not_found ->
            ok
    end.

encode(session, Session) ->
    cds_utils:encode_session(Session);
encode(carddata, Token) ->
    cds_utils:encode_token(Token).

recrypt(Subject, ItemID, Meta) ->
    {KeyID, Data} = get_data(Subject, ItemID),
    case maps:get(KeyID, Meta) of
        #{retired := true} ->
            _ = logger:warning("Found ~p ~p encrypted with retired key ~p", [Subject, ItemID, KeyID]);
        #{retired := false} ->
            ok
    end,
    update_data(Subject, ItemID, Data).

get_data(session, Session) ->
    cds:get_session_data(Session);
get_data(carddata, Token) ->
    cds:get_cardholder_data(Token).

update_data(session, Session, SessionData) ->
    cds:update_session_data(Session, SessionData);
update_data(carddata, Token, CardData) ->
    cds:update_cardholder_data(Token, CardData).

get_interval() ->
    maps:get(interval, get_config(), ?DEFAULT_INTERVAL).

get_batch_size() ->
    maps:get(batch_size, get_config(), ?DEFAULT_BATCH_SIZE).

get_config() ->
    genlib_app:env(cds, recrypting, #{}).
