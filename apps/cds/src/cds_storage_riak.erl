-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/0]).
-export([get_token/1]).
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/5]).
-export([delete_card_data/3]).
-export([delete_cvv/1]).
-export([get_sessions_keys/3]).

-include_lib("riakc/include/riakc.hrl").

-define(TOKEN_BUCKET, <<"t">>).
-define(HASH_BUCKET, <<"h">>).
-define(SESSION_BUCKET, <<"s">>).

-define(SESSION_CREATED_AT_INDEX, {integer_index, "created_at"}).

-define(POOLER_TIMEOUT, {5, sec}).
%%
%% cds_storage behaviour
%%

-spec start() -> ok.
start() ->
    {ok, #{conn_params := ConnParams}} = application:get_env(cds, cds_storage_riak),
    start_pool(ConnParams),
    lists:foreach(fun set_bucket/1, [?TOKEN_BUCKET, ?HASH_BUCKET, ?SESSION_BUCKET]).

-spec get_token(binary()) -> {ok, binary()} | {error, not_found}.
get_token(Hash) ->
    case get(?HASH_BUCKET, Hash) of
        {ok, TokenObj} ->
            Token = riakc_obj:get_value(TokenObj),
            {ok, Token};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

-spec get_card_data(binary()) -> {ok, binary()} | {error, not_found}.
get_card_data(Token) ->
    case get(?TOKEN_BUCKET, Token) of
        {ok, CardDataObj} ->
            CardData = riakc_obj:get_value(CardDataObj),
            {ok, CardData};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

-spec get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found}.
get_session_card_data(Token, Session) ->
    case batch_get([[?SESSION_BUCKET, Session], [?TOKEN_BUCKET, Token]]) of
        {ok, [CvvObj, CardDataObj]} ->
            CardData = riakc_obj:get_value(CardDataObj),
            Cvv = riakc_obj:get_value(CvvObj),
            {ok, {CardData, Cvv}};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

-spec put_card_data(binary(), binary(), binary(), binary(), binary()) -> ok.
put_card_data(Token, Session, Hash, CardData, Cvv) ->
    TokenObj = riakc_obj:new(?TOKEN_BUCKET, Token, CardData),
    HashObj = riakc_obj:new(?HASH_BUCKET, Hash, Token),
    SessionObj = prepare_session_obj(Session, Cvv),
    case batch_put([[TokenObj], [HashObj], [SessionObj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec delete_card_data(binary(), binary(), binary()) -> ok.
delete_card_data(Token, Hash, Session) ->
    case batch_delete([[?TOKEN_BUCKET, Token], [?HASH_BUCKET, Hash], [?SESSION_BUCKET, Session]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec delete_cvv(binary()) -> ok.
delete_cvv(Session) ->
    case delete(?SESSION_BUCKET, Session) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec get_sessions_keys(
    non_neg_integer(),
    pos_integer(),
    pos_integer() | undefined
) -> {ok, [term()]} | {error, Reason :: term()}.
get_sessions_keys(From, To, Limit) ->
    case get_index_range(
            ?SESSION_BUCKET,
            ?SESSION_CREATED_AT_INDEX,
            From,
            To,
            [{max_results, Limit}]
    ) of
        {ok, #index_results_v1{keys = Keys}} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        {error, Reason} ->
            error(Reason)
    end.

%%
%% Internal
%%

start_pool({Host, Port}) ->
    PoolConfig = [
        {name, riak},
        {max_count, 10},
        {init_count, 5},
        {start_mfa, {riakc_pb_socket, start_link, [Host, Port]}}
    ],
    {ok, _Pid} = pooler:new_pool(PoolConfig),
    ok.

get(Bucket, Key) ->
    case batch_get([[Bucket, Key]]) of
        {ok, [Response]} ->
            {ok, Response};
        Error ->
            Error
    end.

delete(Bucket, Key) ->
    batch_delete([[Bucket, Key]]).

get_index_range(Bucket, Index, StartKey, EndKey, Opts) ->
    batch_get_index_range([[Bucket, Index, StartKey, EndKey, Opts]]).

set_bucket(Bucket) ->
    batch_request(set_bucket, [[Bucket, [{allow_mult, false}]]], ok).

batch_get(Args) ->
    batch_request(get, Args, []).

batch_put(Args) ->
    batch_request(put, Args, ok).

batch_delete(Args) ->
    batch_request(delete, Args, ok).

batch_get_index_range(Args) ->
    batch_request(get_index_range, Args, []).

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

prepare_session_obj(Session, Cvv) ->
    Obj = riakc_obj:new(?SESSION_BUCKET, Session, Cvv),
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
        MD1,
        [{?SESSION_CREATED_AT_INDEX, [current_time()]}]
    ),
    riakc_obj:update_metadata(Obj, MD2).

current_time() ->
    genlib_time:unow().
