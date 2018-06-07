-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/1]).
-export([put/5]).
-export([get/2]).
-export([update/5]).
-export([delete/2]).
-export([search_by_index_value/5]).
-export([search_by_index_range/6]).
-export([get_keys/3]).

-include_lib("riakc/include/riakc.hrl").

-define(POOLER_TIMEOUT, {5, sec}).
-define(DEFAULT_TIMEOUT, 5000). % milliseconds

-type storage_params() :: #{
    conn_params := conn_params(),
    timeout     => pos_integer(),
    pool_params => pool_params()
}.

-type conn_params() :: #{
    host := string(),
    port := pos_integer()
}.

-type pool_params() :: #{
    max_count            => non_neg_integer     (),
    init_count           => non_neg_integer     (),
    cull_interval        => pooler_time_interval(),
    max_age              => pooler_time_interval(),
    member_start_timeout => pooler_time_interval()
}.

-type pooler_time_interval() :: {non_neg_integer(), min | sec | ms | mu}.

-define(DEFAULT_POOL_PARAMS, #{
    max_count     => 10,
    init_count    => 10,
    cull_interval => {0, min}
}).

%%
%% cds_storage behaviour
%%
-type namespace()   :: cds_storage:namespace().
-type data()        :: cds_storage:data().
-type metadata()    :: cds_storage:metadata().
-type indexes()     :: cds_storage:indexes().
-type index_id()    :: cds_storage:index_id().
-type index_value() :: cds_storage:index_value().
-type limit()       :: cds_storage:limit().

-spec start([namespace()]) -> ok.

start(NSlist) ->
    _ = start_pool(get_storage_params()),
    lists:foreach(fun set_bucket/1, NSlist).

-spec put(namespace(), key(), data(), metadata(), indexes()) -> ok.
put(NS, Key, Data, Meta, Indexes) ->
    Obj = prepare_object(NS, Key, Data, Meta, Indexes),
    case batch_put([[Obj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec get(namespace(), key()) -> {ok, {data(), metadata(), indexes()}} | {error, not_found}.
get(NS, Key) ->
    case get_(NS, Key) of
        {ok, DataObj} ->
            Data = riakc_obj:get_value(DataObj),
            Meta = get_metadata(DataObj),
            Indexes = get_indexes(DataObj),
            {ok, {Data, Meta, Indexes}};
        Error ->
            Error
    end.

-spec update(namespace(), key(), data(), metadata(), indexes()) -> ok | {error, not_found}.
update(NS, Key, Data, Meta, Indexes) ->
    case get_(NS, Key) of
        {ok, Obj} ->
            update_(Obj, Data, Meta, Indexes);
        Error ->
            Error
    end.

-spec delete(namespace(), key()) -> ok | {error, not_found}.
delete(NS, Key) ->
    case batch_delete([[NS, Key]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec search_by_index_value(
    namespace(),
    index_id(),
    index_value(),
    limit(),
    continuation()
) ->
    {ok, {[key()], continuation()}}.
search_by_index_value(NS, IndexName, IndexValue, Limit, Continuation) ->
    Result = get_index_eq(
        NS,
        IndexName,
        IndexValue,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

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
    Result = get_index_range(
        NS,
        IndexName,
        StartValue,
        EndValue,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

-spec get_keys(namespace(), limit(), continuation()) -> {ok, {[key()], continuation()}}.
get_keys(NS, Limit, Continuation) ->
    Result = get_index_eq(
        NS,
        <<"$bucket">>,
        <<"_">>,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

%%
%% Internal
%%

-spec get_storage_params() -> storage_params().
get_storage_params() ->
    genlib_app:env(cds, cds_storage_riak).

get_default_timeout() ->
    Params = get_storage_params(),
    {timeout, genlib_map:get(timeout, Params, ?DEFAULT_TIMEOUT)}.

-spec start_pool(storage_params()) -> ok | no_return().

start_pool(#{conn_params := #{host := Host, port := Port}} = StorageParams) ->
    PoolParams = maps:get(pool_params, StorageParams, ?DEFAULT_POOL_PARAMS),

    PoolConfig = [
        {name, riak},
        {start_mfa, {riakc_pb_socket, start_link, [Host, Port]}}
    ] ++ maps:to_list(PoolParams),

    {ok, _Pid} = pooler:new_pool(PoolConfig),
    ok.

get_(Bucket, Key) ->
    case batch_get([[Bucket, Key]]) of
        {ok, [Obj]} ->
            {ok, Obj};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

update_(Obj0, Data, Meta, Indexes) ->
    Obj1 = riakc_obj:update_value(Obj0, Data),
    Obj2 = set_metadata(Obj1, Meta),
    Obj3 = set_indexes(Obj2, Indexes),
    case batch_put([[Obj3]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

set_bucket(Bucket) ->
    batch_request(set_bucket, [[Bucket, [{allow_mult, false}]]], ok).

batch_get(Args) ->
    batch_request(get, Args, []).

batch_put(Args) ->
    batch_request(put, Args, ok).

batch_delete(Args) ->
    batch_request(delete, Args, ok).

get_index_range(Bucket, Index, StartKey, EndKey, Opts) ->
    batch_request(get_index_range, [[Bucket, Index, StartKey, EndKey, Opts]], []).

get_index_eq(Bucket, Index, Key, Opts) ->
    batch_request(get_index_eq, [[Bucket, Index, Key, Opts]], []).

batch_request(Method, Args, Acc) ->
    Client = pooler:take_member(riak, ?POOLER_TIMEOUT),
    batch_request(Method, Client, Args, Acc).

batch_request(_Method, Client, [], Acc) ->
    ok = pooler:return_member(riak, Client, ok),
    case Acc of
        ok ->
            ok;
        Acc ->
            {ok, lists:reverse(Acc)}
    end;
batch_request(Method, Client, [Args | Rest], Acc) ->
    try
        case apply(riakc_pb_socket, Method, [Client | Args]) of
            ok when Acc =:= ok ->
                batch_request(Method, Client, Rest, Acc);
            {ok, Response} when is_list(Acc) ->
                batch_request(Method, Client, Rest, [Response | Acc]);
            Error ->
                ok = pooler:return_member(riak, Client, fail),
                Error
        end
    catch
        Class:Exception ->
            pooler:return_group_member(riak, Client, fail),
            erlang:raise(Class, Exception, erlang:get_stacktrace())
    end.

-spec prepare_index_result({ok, index_results()} | {error, Reason :: term()}) ->
    {ok, {[key()], continuation()}} | no_return().
prepare_index_result(Result) ->
    case Result of
        {ok, [#index_results_v1{keys = Keys, continuation = Continuation}]} when Keys =/= undefined ->
            {ok, {Keys, Continuation}};
        {ok, _} ->
            {ok, {[], undefined}};
        {error, Error} ->
            error(Error)
    end.

construct_index_query_options(Limit, Continuation) ->
    prepare_options([{max_results, Limit}, {continuation, Continuation}, get_default_timeout()]).

prepare_options(Opts) ->
    [Item || {_, V} = Item <- Opts, V =/= undefined].

prepare_object(NS, Key, Data, Meta, Indexes) ->
    case riakc_obj:new(NS, Key, Data)  of
        Error = {error, _} ->
            error(Error);
        Obj0 ->
            Obj1 = set_metadata(Obj0, Meta),
            set_indexes(Obj1, Indexes)
    end.

-spec set_indexes(riakc_obj(), indexes()) -> riakc_obj().
set_indexes(Obj, Indexes) ->
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
        MD1,
        [{Index, [Value]} || {Index, Value} <- Indexes]
    ),
    riakc_obj:update_metadata(Obj, MD2).

-spec get_indexes(riakc_obj()) -> indexes().
get_indexes(Obj) ->
    MD = riakc_obj:get_metadata(Obj),
    Indexes = riakc_obj:get_secondary_indexes(MD),
    [{Index, Value} || {Index, [Value]} <- Indexes].

-spec set_metadata(riakc_obj(), metadata()) -> riakc_obj().
set_metadata(Obj, Meta) when is_binary(Meta) ->
    MD0 = riakc_obj:get_update_metadata(Obj),
    MD1 = riakc_obj:set_user_metadata_entry(MD0, {<<"encrypted-application-metadata">>, Meta}),
    riakc_obj:update_metadata(Obj, MD1);
set_metadata(Obj, undefined) ->
    MD0 = riakc_obj:get_update_metadata(Obj),
    MD1 = riakc_obj:delete_user_metadata_entry(MD0, <<"encrypted-application-metadata">>),
    riakc_obj:update_metadata(Obj, MD1).

-spec get_metadata(riakc_obj()) -> metadata().
get_metadata(Obj) ->
    MD = riakc_obj:get_update_metadata(Obj),
    case riakc_obj:get_user_metadata_entry(MD, <<"encrypted-application-metadata">>) of
        Meta when is_binary(Meta) ->
            Meta;
        notfound ->
            undefined
    end.
