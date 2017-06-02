-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([get_cvv/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/4]).
-export([get_sessions_created_between/3]).
-export([get_tokens_by_key_id_between/3]).
-export([get_sessions_by_key_id_between/3]).
-export([refresh_session_created_at/1]).
-export([get_sessions/1]).
-export([get_sessions/2]).
-export([get_tokens/1]).
-export([get_tokens/2]).


-include_lib("riakc/include/riakc.hrl").

-define(TOKEN_BUCKET, <<"t">>).
-define(HASH_BUCKET, <<"h">>).
-define(SESSION_BUCKET, <<"s">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).
-define(CARD_DATA_HASH_INDEX, {binary_index, "card_data_salted_hash"}).

-define(POOLER_TIMEOUT, {5, sec}).

-type storage_params() :: #{
    conn_params := conn_params(),
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

-spec start() -> ok.

start() ->
    _ = start_pool(get_storage_params()),
    lists:foreach(fun set_bucket/1, [?TOKEN_BUCKET, ?HASH_BUCKET, ?SESSION_BUCKET]).

-spec get_token(binary()) -> {ok, binary()} | {error, not_found}.

get_token(Hash) ->
    case get_keys_by_index_value(?TOKEN_BUCKET, ?CARD_DATA_HASH_INDEX, Hash, 2) of
        {ok, [Token]} ->
            {ok, Token};
        {ok, []} ->
            get_token_old_style(Hash);
        {ok, [_ | _OtherTokens]} ->
            % This shouldnt happen, but we need to react somehow.
            error({<<"Hash collision detected">>, Hash});
        {error, Reason} ->
            error(Reason)
    end.

get_token_old_style(Hash) ->
    case get(?HASH_BUCKET, Hash) of
        {ok, TokenObj} ->
            {ok, riakc_obj:get_value(TokenObj)};
        {error, notfound} ->
            {error, not_found}
    end.

-spec get_cardholder_data(binary()) -> {ok, binary()} | {error, not_found} | no_return().

get_cardholder_data(Token) ->
    case get(?TOKEN_BUCKET, Token) of
        {ok, CardDataObj} ->
            CardData = riakc_obj:get_value(CardDataObj),
            {ok, CardData};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

-spec get_session_card_data(binary(), binary()) -> {ok, {binary(), binary()}} | {error, not_found} | no_return().

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

-spec put_card_data(binary(), binary(), binary(), binary(), binary(), byte(), pos_integer()) -> ok | no_return().

put_card_data(Token, Session, Hash, CardData, Cvv, KeyID, CreatedAt) ->
    TokenObj = prepare_token_obj(Token, CardData, Hash, KeyID),
    SessionObj = prepare_session_obj(Session, Cvv, CreatedAt, KeyID),
    case batch_put([[TokenObj], [SessionObj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec delete_session(binary()) -> ok | no_return().

delete_session(Session) ->
    case delete(?SESSION_BUCKET, Session) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec get_cvv(Session :: binary()) -> {ok, binary()} | {error, not_found}.

get_cvv(Session) ->
    case get(?SESSION_BUCKET, Session) of
        {ok, SessionObj} ->
            Cvv = riakc_obj:get_value(SessionObj),
            {ok, Cvv};
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

-spec update_cvv(Session :: binary(), NewCvv :: binary(), KeyID :: byte()) -> ok | {error, not_found}.

update_cvv(Session, NewCvv, KeyID) ->
    update(?SESSION_BUCKET, Session, NewCvv, [{?KEY_ID_INDEX, KeyID}]).

-spec update_cardholder_data(binary(), binary(), binary(), byte()) -> ok | {error, not_found}.

update_cardholder_data(Token, NewCardData, NewHash, KeyID) ->
    update(?TOKEN_BUCKET, Token, NewCardData, [{?KEY_ID_INDEX, KeyID}, {?CARD_DATA_HASH_INDEX, NewHash}]).

-spec get_sessions_created_between(non_neg_integer(), non_neg_integer(), non_neg_integer() | undefined) ->
    {ok, [binary()]} | no_return().

get_sessions_created_between(From, To, Limit) ->
    Result = get_keys_by_index_range(?SESSION_BUCKET, ?CREATED_AT_INDEX, From, To, Limit),
    case Result of
        OK = {ok, _} ->
            OK;
        {error, Error} ->
            error(Error)
    end.

-spec get_tokens_by_key_id_between(non_neg_integer(), non_neg_integer(), non_neg_integer() | undefined) ->
    {ok, [binary()]} | no_return().

get_tokens_by_key_id_between(From, To, Limit) ->
    Result = get_keys_by_index_range(?TOKEN_BUCKET, ?KEY_ID_INDEX, From, To, Limit),
    case Result of
        OK = {ok, _} ->
            OK;
        {error, Error} ->
            error(Error)
    end.

-spec get_sessions_by_key_id_between(non_neg_integer(), non_neg_integer(), non_neg_integer() | undefined) ->
    {ok, [binary()]} | no_return().

get_sessions_by_key_id_between(From, To, Limit) ->
    Result = get_keys_by_index_range(?SESSION_BUCKET, ?KEY_ID_INDEX, From, To, Limit),
    case Result of
        OK = {ok, _} ->
            OK;
        {error, Error} ->
            error(Error)
    end.

-spec get_sessions(non_neg_integer() | undefined) ->
    {ok, {[binary()], continuation()}}.

get_sessions(Limit) ->
    get_sessions(Limit, undefined).

-spec get_sessions(non_neg_integer() | undefined, continuation()) ->
    {ok, {[binary()], continuation()}}.

get_sessions(Limit, Continuation) ->
    get_keys(?SESSION_BUCKET, Limit, Continuation).

-spec get_tokens(non_neg_integer() | undefined) ->
    {ok, {[binary()], continuation()}}.

get_tokens(Limit) ->
    get_tokens(Limit, undefined).

-spec get_tokens(non_neg_integer() | undefined, continuation()) ->
    {ok, {[binary()], continuation()}}.

get_tokens(Limit, Continuation) ->
    get_keys(?TOKEN_BUCKET, Limit, Continuation).

-spec refresh_session_created_at(binary()) -> ok | no_return().

refresh_session_created_at(Session) ->
    case get(?SESSION_BUCKET, Session) of
        {ok, Obj} ->
            update_indexes(Obj, [{?CREATED_AT_INDEX, cds_utils:current_time()}]);
        {error, notfound} ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.
%%
%% Internal
%%

-spec start_pool(storage_params()) -> ok | no_return().

start_pool(#{conn_params := #{host := Host, port := Port}} = StorageParams) ->
    PoolParams = maps:get(pool_params, StorageParams, ?DEFAULT_POOL_PARAMS),

    PoolConfig = [
        {name, riak},
        {start_mfa, {riakc_pb_socket, start_link, [Host, Port]}}
    ] ++ maps:to_list(PoolParams),

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
    batch_request(get_index_range, [[Bucket, Index, StartKey, EndKey, Opts]], []).

get_index_eq(Bucket, Index, Key, Opts) ->
    batch_request(get_index_eq, [[Bucket, Index, Key, Opts]], []).

set_bucket(Bucket) ->
    batch_request(set_bucket, [[Bucket, [{allow_mult, false}]]], ok).

batch_get(Args) ->
    batch_request(get, Args, []).

batch_put(Args) ->
    batch_request(put, Args, ok).

batch_delete(Args) ->
    batch_request(delete, Args, ok).

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

prepare_session_obj(Session, Cvv, CreatedAt, KeyID) ->
    case riakc_obj:new(?SESSION_BUCKET, Session, Cvv)  of
        Error = {error, _} ->
            error(Error);
        Obj ->
            set_indexes(
                Obj,
                [{?CREATED_AT_INDEX, CreatedAt}, {?KEY_ID_INDEX, KeyID}]
            )
    end.

prepare_token_obj(Token, CardData, Hash, KeyID) ->
    case riakc_obj:new(?TOKEN_BUCKET, Token, CardData)  of
        Error = {error, _} ->
            error(Error);
        Obj ->
            set_indexes(
                Obj,
                [{?CARD_DATA_HASH_INDEX, Hash}, {?KEY_ID_INDEX, KeyID}]
            )
    end.

set_indexes(Obj, Indexes) ->
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
        MD1,
        [{Index, [Value]} || {Index, Value} <- Indexes]
    ),
    riakc_obj:update_metadata(Obj, MD2).

update(Tab, Key, Value, Indexes) ->
    case get(Tab, Key) of
        {ok, Obj0} ->
            update(Obj0, Value, Indexes);
        {error, Reason} ->
            error(Reason)
    end.

update(Obj0, Value, Indexes) ->
    Obj = riakc_obj:update_value(Obj0, Value),
    update_indexes(Obj, Indexes).

update_indexes(Obj0, Indexes) ->
    Obj = set_indexes(Obj0, Indexes),
    case batch_put([[Obj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec get_storage_params() -> storage_params().
get_storage_params() ->
    genlib_app:env(cds, cds_storage_riak).

get_keys(Bucket, Limit, Continuation) ->
    Options = options([{max_results, Limit}, {continuation, Continuation}]),

    Result = get_index_eq(
        Bucket,
        <<"$bucket">>,
        <<"_">>,
        Options
    ),
    prepare_index_result(Result).


%% @TODO add continuation here?
get_keys_by_index_range(Bucket, IndexName, From, To, Limit) ->
    Options = options([{max_results, Limit}]),

    Result = get_index_range(
        Bucket,
        IndexName,
        From,
        To,
        Options
    ),
    case Result of
        {ok, [#index_results_v1{keys = Keys}]} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        Error = {error, _} ->
            Error
    end.

get_keys_by_index_value(Bucket, IndexName, IndexValue, Limit) ->
    Options = options([{max_results, Limit}]),

    Result = get_index_eq(
        Bucket,
        IndexName,
        IndexValue,
        Options
    ),
    case Result of
        {ok, [#index_results_v1{keys = Keys}]} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        Error = {error, _} ->
            Error
    end.

prepare_index_result(Result) ->
    case Result of
        {ok, [#index_results_v1{keys = Keys, continuation = Continuation}]} when Keys =/= undefined ->
            {ok, {Keys, Continuation}};
        {ok, _} ->
            {ok, {[], undefined}};
        Error = {error, _} ->
            Error
    end.

options(Opts) ->
    [Item || {_, V} = Item <- Opts, V =/= undefined].
