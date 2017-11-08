-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/0]).

%% Single object functions
-export([get_cvv/1]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([update_cvv/3]).
-export([update_cardholder_data/4]).
-export([refresh_session_created_at/1]).

%% Many objects functions
-export([get_sessions_created_between/4]).
-export([get_tokens_by_key_id_between/4]).
-export([get_sessions_by_key_id_between/4]).
-export([get_sessions/2]).
-export([get_sessions_info/2]).
-export([get_tokens/2]).


-include_lib("riakc/include/riakc.hrl").

-define(TOKEN_BUCKET, <<"t">>).
-define(HASH_BUCKET, <<"h">>).
-define(SESSION_BUCKET, <<"s">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).
-define(CARD_DATA_HASH_INDEX, {binary_index, "card_data_salted_hash"}).

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

-type limit() :: non_neg_integer() | undefined.
-type batch_response(DataType) :: {ok, {[DataType], continuation()}}.
-type timestamp() :: pos_integer().

-spec start() -> ok.

start() ->
    _ = start_pool(get_storage_params()),
    lists:foreach(fun set_bucket/1, [?TOKEN_BUCKET, ?HASH_BUCKET, ?SESSION_BUCKET]).

%%
%% Single object functions
%%

-spec get_token(cds:hash()) -> {ok, cds:token()} | {error, not_found}.

get_token(Hash) ->
    case get_keys_by_index_value(?TOKEN_BUCKET, ?CARD_DATA_HASH_INDEX, Hash, 2, undefined) of
        {ok, {[Token], _}} ->
            {ok, Token};
        {ok, {[], _}} ->
            get_token_old_style(Hash);
        {ok, {[_ | _OtherTokens], _}} ->
            % This shouldnt happen, but we need to react somehow.
            error({<<"Hash collision detected">>, Hash})
    end.

get_token_old_style(Hash) ->
    case get(?HASH_BUCKET, Hash) of
        {ok, TokenObj} ->
            {ok, riakc_obj:get_value(TokenObj)};
        {error, notfound} ->
            {error, not_found}
    end.

-spec get_cardholder_data(cds:token()) -> {ok, cds:ciphertext()} | {error, not_found}.

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

-spec get_session_card_data(cds:token(), cds:session()) ->
    {ok, {cds:ciphertext(), cds:ciphertext()}} | {error, not_found}.

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

-spec put_card_data(
    cds:token(),
    cds:session(),
    cds:hash(),
    CardData :: cds:ciphertext(),
    CVV :: cds:ciphertext(),
    cds_keyring:key_id(),
    timestamp()
) -> ok.

put_card_data(Token, Session, Hash, CardData, Cvv, KeyID, CreatedAt) ->
    TokenObj = prepare_token_obj(Token, CardData, Hash, KeyID),
    SessionObj = prepare_session_obj(Session, Cvv, CreatedAt, KeyID),
    case batch_put([[TokenObj], [SessionObj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec delete_session(cds:session()) -> ok.

delete_session(Session) ->
    case delete(?SESSION_BUCKET, Session) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec get_cvv(cds:session()) -> {ok, cds:ciphertext()} | {error, not_found}.

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

-spec update_cvv(cds:session(), NewCvv :: cds:ciphertext(), cds_keyring:key_id()) -> ok | {error, not_found}.

update_cvv(Session, NewCvv, KeyID) ->
    update(?SESSION_BUCKET, Session, NewCvv, [{?KEY_ID_INDEX, KeyID}]).

-spec update_cardholder_data(cds:token(), cds:ciphertext(), cds:hash(), cds_keyring:key_id()) ->
    ok | {error, not_found}.

update_cardholder_data(Token, NewCardData, NewHash, KeyID) ->
    update(?TOKEN_BUCKET, Token, NewCardData, [{?KEY_ID_INDEX, KeyID}, {?CARD_DATA_HASH_INDEX, NewHash}]).

-spec refresh_session_created_at(cds:session()) -> ok.

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
%% Batch functions
%%

-spec get_sessions_created_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response(cds:session()) | no_return().

get_sessions_created_between(From, To, Limit, Continuation) ->
    get_keys_by_index_range(?SESSION_BUCKET, ?CREATED_AT_INDEX, From, To, Limit, Continuation).

-spec get_tokens_by_key_id_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response(cds:token()) | no_return().

get_tokens_by_key_id_between(From, To, Limit, Continuation) ->
    get_keys_by_index_range(?TOKEN_BUCKET, ?KEY_ID_INDEX, From, To, Limit, Continuation).

-spec get_sessions_by_key_id_between(non_neg_integer(), non_neg_integer(), limit(), continuation()) ->
    batch_response(cds:session()) | no_return().

get_sessions_by_key_id_between(From, To, Limit, Continuation) ->
    get_keys_by_index_range(?SESSION_BUCKET, ?KEY_ID_INDEX, From, To, Limit, Continuation).

-spec get_sessions(limit(), continuation()) ->
    batch_response(cds:session()) | no_return().

get_sessions(Limit, Continuation) ->
    get_keys(?SESSION_BUCKET, Limit, Continuation).

-spec get_sessions_info(limit(), continuation()) ->
    {ok, {[{cds:session(), Info :: map()}], continuation()}} | no_return().

get_sessions_info(Limit, Continuation) ->
    {ok, {Keys, Cont}} = get_keys(?SESSION_BUCKET, Limit, Continuation),
    SessionsInfo = lists:foldl(
        fun(K, Acc) ->
            case get(?SESSION_BUCKET, K) of
                {ok, Obj} ->
                    Meta = riakc_obj:get_metadata(Obj),
                    [CreatedAt] = riakc_obj:get_secondary_index(Meta, ?CREATED_AT_INDEX),
                    [{K, #{lifetime => cds_utils:current_time() - CreatedAt}} | Acc];
                {error, notfound} ->
                    Acc;
                {error, Error} ->
                    [{K, #{error => Error}} | Acc]
            end
        end,
        [],
        Keys
    ),
    {ok, {SessionsInfo, Cont}}.

-spec get_tokens(limit(), continuation()) ->
    batch_response(cds:token()) | no_return().

get_tokens(Limit, Continuation) ->
    get_keys(?TOKEN_BUCKET, Limit, Continuation).

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

get_default_timeout() ->
    Params = get_storage_params(),
    {timeout, genlib_map:get(timeout, Params, ?DEFAULT_TIMEOUT)}.

get_keys(Bucket, Limit, Continuation) ->
    Result = get_index_eq(
        Bucket,
        <<"$bucket">>,
        <<"_">>,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

get_keys_by_index_range(Bucket, IndexName, From, To, Limit, Continuation) ->
    Result = get_index_range(
        Bucket,
        IndexName,
        From,
        To,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

get_keys_by_index_value(Bucket, IndexName, IndexValue, Limit, Continuation) ->
    Result = get_index_eq(
        Bucket,
        IndexName,
        IndexValue,
        construct_index_query_options(Limit, Continuation)
    ),
    prepare_index_result(Result).

-spec prepare_index_result({ok, index_results()} | {error, Reason :: term()}) -> batch_response(term()) | no_return().

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
