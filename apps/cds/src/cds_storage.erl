-module(cds_storage).

%% TODO remove circular dependencies in types (cds:hash, cds:token etc.)

%% e.g. <<"stuff_you_want_to_store">>, but dont be too fancy
-type namespace() :: binary().
-type key() :: binary().
-type data() :: binary().
-type metadata() :: binary() | undefined.
-type indexes() :: [index()].
-type index() :: integer_index() | binary_index().
-type integer_index() :: {integer_index_id(), integer()}.
-type integer_index_id() :: {integer_index, string()}.
-type binary_index() :: {binary_index_id(), binary()}.
-type binary_index_id() :: {binary_index, string()}.
-type index_id() :: integer_index_id() | binary_index_id().
-type index_value() :: integer() | binary().
-type limit() :: non_neg_integer() | undefined.
-type continuation() :: term().

-export_type([namespace/0]).
-export_type([key/0]).
-export_type([data/0]).
-export_type([metadata/0]).
-export_type([indexes/0]).
-export_type([index/0]).
-export_type([integer_index/0]).
-export_type([integer_index_id/0]).
-export_type([binary_index/0]).
-export_type([binary_index_id/0]).
-export_type([index_id/0]).
-export_type([index_value/0]).
-export_type([limit/0]).

-callback start(list(namespace())) -> ok.
-callback put(namespace(), key(), data(), metadata(), indexes()) -> ok.
-callback get(namespace(), key()) -> {ok, {data(), metadata(), indexes()}} | {error, not_found}.
-callback update(namespace(), key(), data(), metadata(), indexes()) -> ok | {error, not_found}.
-callback delete(namespace(), key()) -> ok | {error, not_found}.

-callback search_by_index_value(
    namespace(),
    index_id(),
    index_value(),
    limit(),
    continuation()
) -> {ok, {[key()], continuation()}}.

-callback search_by_index_range(
    namespace(),
    index_id(),
    StartValue :: index_value(),
    EndValue :: index_value(),
    limit(),
    continuation()
) -> {ok, {[key()], continuation()}}.

-callback get_keys(namespace(), limit(), continuation()) -> {ok, {[key()], continuation()}}.

-export([start/1]).
-export([put/5]).
-export([get/2]).
-export([update/5]).
-export([delete/2]).
-export([search_by_index_value/5]).
-export([search_by_index_range/6]).
-export([get_keys/3]).

-spec start(list(namespace())) -> ok.
start(NSlist) ->
    cds_backend:call(storage, start, [NSlist]).

-spec put(namespace(), key(), data(), metadata(), indexes()) -> ok | no_return().
put(NS, Key, Data, Meta, Indexes) ->
    cds_backend:call(storage, put, [NS, Key, Data, Meta, Indexes]).

-spec get(namespace(), key()) -> {data(), metadata(), indexes()} | no_return().
get(NS, Key) ->
    cds_backend:call(storage, get, [NS, Key]).

-spec update(namespace(), key(), data(), metadata(), indexes()) -> ok | no_return().
update(NS, Key, Data, Meta, Indexes) ->
    cds_backend:call(storage, update, [NS, Key, Data, Meta, Indexes]).

-spec delete(namespace(), key()) -> ok | no_return().
delete(NS, Key) ->
    cds_backend:call(storage, delete, [NS, Key]).

-spec search_by_index_value(
    namespace(),
    index_id(),
    index_value(),
    limit(),
    continuation()
) -> {[key()], continuation()} | no_return().
search_by_index_value(NS, Key, Value, Limit, Continuation) ->
    cds_backend:call(storage, search_by_index_value, [NS, Key, Value, Limit, Continuation]).

-spec search_by_index_range(
    namespace(),
    index_id(),
    StartValue :: index_value(),
    EndValue :: index_value(),
    limit(),
    continuation()
) -> {[key()], continuation()} | no_return().
search_by_index_range(NS, Key, StartValue, EndValue, Limit, Continuation) ->
    cds_backend:call(storage, search_by_index_range, [NS, Key, StartValue, EndValue, Limit, Continuation]).

-spec get_keys(namespace(), limit(), continuation()) -> {[cds:token()], continuation()} | no_return().
get_keys(NS, Limit, Continuation) ->
    cds_backend:call(storage, get_keys, [NS, Limit, Continuation]).
