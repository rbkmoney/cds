-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/0]).
-export([get/2]).
-export([put/3]).
-export([delete/2]).

-spec start() -> ok.
start() ->
    {ok, #{conn_params := ConnParams}} = application:get_env(cds, cds_riak_storage),
    lists:foreach(fun start_pool/1, ConnParams).

-spec get(atom(), binary()) -> {ok, binary()} | {error, not_found}.
get(Type, Key) ->
    Client = pooler:take_group_member(riak),
    case riakc_pb_socket:get(Client, resolve_bucket(Type), Key) of
        {ok, Obj} ->
            pooler:return_group_member(riak, Client, ok),
            Value = riakc_obj:get_value(Obj),
            {ok, Value};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            pooler:return_group_member(riak, Client, fail),
            error(Reason)
    end.

-spec put(atom(), binary(), binary()) -> ok.
put(Type, Key, Data) ->
    Object = riakc_obj:new(resolve_bucket(Type), Key, Data),
    Client = pooler:take_group_member(riak),
    case riakc_pb_socket:put(Client, Object) of
        ok ->
            pooler:return_group_member(riak, Client, ok),
            ok;
        {error, Reason} ->
            pooler:return_group_member(riak, Client, fail),
            error(Reason)
    end.

-spec delete(atom(), binary()) -> ok.
delete(Type, Key) ->
    Client = pooler:take_group_member(riak),
    case riakc_pb_socket:delete(Client, resolve_bucket(Type), Key) of
        ok ->
            pooler:return_group_member(riak, Client, ok),
            ok;
        {error, Reason} ->
            pooler:return_group_member(riak, Client, fail),
            error(Reason)
    end.

resolve_bucket(hash) ->
    <<"h">>;
resolve_bucket(token) ->
    <<"t">>.

start_pool({Name, Host, Port}) ->
    PoolConfig = [
        {name, Name},
        {group, riak},
        {max_count, 5},
        {init_count, 2},
        {start_mfa, {riakc_pb_socket, start_link, [Host, Port]}}
    ],
    pooler:new_pool(PoolConfig).
