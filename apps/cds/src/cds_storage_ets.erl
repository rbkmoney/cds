-module(cds_storage_ets).
-behaviour(cds_storage).
-behaviour(gen_server).

%% cds_storage behaviour
-export([start/1]).
-export([put/5]).
-export([get/2]).
-export([update/5]).
-export([delete/2]).
-export([search_by_index_value/5]).
-export([search_by_index_range/6]).
-export([get_keys/3]).

%% gen_server behaviour
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2]).
-export([code_change/3]).

%%
%% cds_storage behaviour
%%
-type namespace()   :: cds_storage:namespace().
-type key()         :: cds_storage:key().
-type data()        :: cds_storage:data().
-type metadata()    :: cds_storage:metadata().
-type indexes()     :: cds_storage:indexes().
-type index_id()        :: cds_storage:index_id().
-type index_value()     :: cds_storage:index_value().
-type limit()           :: cds_storage:limit().
-type continuation()    :: term().

-spec start([namespace()]) -> ok.
start(NSlist) ->
    ChildSpec = #{
        id => ?MODULE,
        start => {gen_server, start_link, [?MODULE, NSlist, []]}
    },
    {ok, _Child} = supervisor:start_child(cds, ChildSpec),
    ok.

-spec put(namespace(), key(), data(), metadata(), indexes()) -> ok.
put(NS, Key, Data, Meta, Indexes) ->
    true = put_(NS, Key, Data, Meta, Indexes),
    ok.

-spec get(namespace(), key()) -> {ok, {data(), metadata()}} | {error, not_found}.
get(NS, Key) ->
    case get_(NS, Key) of
        {ok, Data, Meta, _Indexes} ->
            {ok, {Data, Meta}};
        {error, not_found} ->
            {error, not_found}
    end.

-spec update(namespace(), key(), data(), metadata(), indexes()) -> ok | {error, not_found}.
update(NS, Key, Data, Meta, Indexes) ->
    case get_(NS, Key) of
        {ok, _, _, OldIndex} ->
            NewIndexes = update_indexes(OldIndex, Indexes),
            true = put_(NS, Key, Data, Meta, NewIndexes),
            ok;
        {error, not_found} ->
            {error, not_found}
    end.

-spec delete(namespace(), key()) -> ok | {error, not_found}.
delete(NS, Key) ->
    true = ets:delete(table_name(NS), Key),
    ok.

-spec search_by_index_value(
    namespace(),
    index_id(),
    index_value(),
    limit(),
    continuation()
) ->
    {ok, {[key()], continuation()}}.
search_by_index_value(NS, IndexName, IndexValue, Limit, Continuation) ->
    get_keys_by_index_range(NS, IndexName, IndexValue, IndexValue, Limit, Continuation).

-spec search_by_index_range(
    namespace(),
    index_id(),
    StartValue :: index_value(),
    EndValue :: index_value(),
    limit(),
    continuation()
) ->
    {ok, {[key()], continuation()}}.
search_by_index_range(NS, IndexName, StartValue, EndValue, Limit, Continuation) ->
    get_keys_by_index_range(NS, IndexName, StartValue, EndValue, Limit, Continuation).

-spec get_keys(namespace(), limit(), continuation()) -> {ok, {[key()], continuation()}}.
get_keys(NS, Limit, Continuation) ->
    MatchSpec = [{
        {'$1', '_', '_'},
        [],
        ['$1']
    }],
    prepare_keys_result(
        select(table_name(NS), MatchSpec, Limit, Continuation)
    ).

%%
%% gen_server behaviour
%%

-type state() :: {}.

-spec init([namespace()]) -> {ok, state()}.

init(NSlist) ->
    lists:foreach(
        fun(NS) ->
            TableName = table_name(NS),
            TableName = ets:new(TableName, [named_table, public])
        end,
        NSlist
    ),
    {ok, {}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal
%%

put_(NS, Key, Value, Meta, Index) ->
    true = ets:insert(table_name(NS), {Key, {Value, Meta}, Index}).

get_(NS, Key) ->
    case ets:lookup(table_name(NS), Key) of
        [{_K, {Value, Meta}, Index}] ->
            {ok, Value, Meta, Index};
        [] ->
            {error, not_found}
    end.

get_keys_by_index_range(NS, IndexName, From, To, Limit, Continuation) ->
    MatchSpec = [{
        {'$1', '_', #{IndexName => '$2'}},
        [{'=<', {const, From}, '$2'}, {'=<', '$2', {const, To}}],
        ['$1']
    }],
    prepare_keys_result(select(table_name(NS), MatchSpec, Limit, Continuation)).

select(Tab, MatchSpec, undefined, undefined) ->
    ets:select(Tab, MatchSpec);

select(Tab, MatchSpec, Limit, undefined) when Limit > 0 ->
    ets:select(Tab, MatchSpec, Limit);

select(_, _, _, Continuation) ->
    ets:select(Continuation).

update_indexes(Old, Indexes) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            maps:put(K, V, Acc)
        end,
        Old,
        Indexes
    ).

prepare_keys_result(Keys) when is_list(Keys) ->
    {ok, {Keys, undefined}};
prepare_keys_result({Keys, Continuation}) when is_list(Keys) ->
    {ok, {Keys, Continuation}};
prepare_keys_result('$end_of_table') ->
    {ok, {[], undefined}}.

table_name(NS) ->
    binary_to_atom(NS, utf8).
