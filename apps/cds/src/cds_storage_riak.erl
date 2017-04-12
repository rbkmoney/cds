-module(cds_storage_riak).
-behaviour(cds_storage).

-export([start/0]).
-export([get_token/1]).
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/7]).
-export([delete_card_data/3]).
-export([delete_session/1]).
-export([get_sessions_created_between/3]).
-export([get_tokens_by_key_id_between/3]).
-export([get_sessions_by_key_id_between/3]).
-export([refresh_sessions/0]).

-include_lib("riakc/include/riakc.hrl").

-define(TOKEN_BUCKET, <<"t">>).
-define(HASH_BUCKET, <<"h">>).
-define(SESSION_BUCKET, <<"s">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).

-define(POOLER_TIMEOUT, {5, sec}).

-define(REFRESH_BATCH, 300).

%%
%% cds_storage behaviour
%%

-spec start() -> ok.
start() ->
    {ok, #{conn_params := ConnParams}} = application:get_env(cds, cds_storage_riak),
    start_pool(ConnParams),
    lists:foreach(fun set_bucket/1, [?TOKEN_BUCKET, ?HASH_BUCKET, ?SESSION_BUCKET]).

-spec get_token(binary()) -> {ok, binary()} |
    {error, not_found} |
    no_return().
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

-spec get_card_data(binary()) ->
    {ok, binary()} |
    {error, not_found} |
    no_return().
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

-spec get_session_card_data(binary(), binary()) ->
    {ok, {binary(), binary()}} |
    {error, not_found} |
    no_return().
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
    TokenObj = prepare_token_obj(Token, CardData, KeyID),
    HashObj = riakc_obj:new(?HASH_BUCKET, Hash, Token),
    SessionObj = prepare_session_obj(Session, Cvv, CreatedAt, KeyID),
    case batch_put([[TokenObj], [HashObj], [SessionObj]]) of
        ok ->
            ok;
        {error, Reason} ->
            error(Reason)
    end.

-spec delete_card_data(binary(), binary(), binary()) -> ok | no_return().
delete_card_data(Token, Hash, Session) ->
    case batch_delete([[?TOKEN_BUCKET, Token], [?HASH_BUCKET, Hash], [?SESSION_BUCKET, Session]]) of
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

update_card_data(Token, CardData, KeyID) ->
    case batch_get([[?TOKEN_BUCKET, Token]]) of
        {ok, TokenObj0} ->
            TokenObj1 = riakc_obj:update_value(TokenObj0, CardData),
            TokenObj = set_indexes(TokenObj1, [{?KEY_ID_INDEX, KeyID}]),
            case batch_put([[TokenObj]]) of
                ok ->
                    ok;
                {error, Reason} ->
                    error(Reason)
            end;
        {error, Reason} ->
            error(Reason)
    end.

update_cvv(Session, NewCvv, KeyID) ->
    case batch_get([[?SESSION_BUCKET, Session]]) of
        {ok, SessionObj0} ->
            SessionObj1 = riakc_obj:update_value(SessionObj0, NewCvv),
            SessionObj = set_indexes(SessionObj1, [{?KEY_ID_INDEX, KeyID}]),
            case batch_put([[SessionObj]]) of
                ok ->
                    ok;
                {error, Reason} ->
                    error(Reason)
            end;
        {error, Reason} ->
            error(Reason)
    end.

-spec get_sessions_created_between(
    non_neg_integer(),
    non_neg_integer(),
    non_neg_integer() | undefined
) -> {ok, [binary()]} | no_return().
get_sessions_created_between(From, To, Limit) ->
    Options = case Limit of
        undefined -> [];
        _ -> [{max_results, Limit}]
    end,

    Result = get_index_range(
        ?SESSION_BUCKET,
        ?CREATED_AT_INDEX,
        From,
        To,
        Options
    ),

    case Result of
        {ok, [#index_results_v1{keys = Keys}]} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        {error, Reason} ->
            error(Reason)
    end.

get_tokens_by_key_id_between(From, To, Limit) ->
    Options0 = case Limit of
        undefined -> [];
        _ -> [{max_results, Limit}]
    end,
    Options = [{return_terms, true} | Options0],
    Result = get_index_range(
        ?TOKEN_BUCKET,
        ?KEY_ID_INDEX,
        From,
        To,
        Options
    ),

    case Result of
        {ok, [#index_results_v1{keys = Keys}]} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        {error, Reason} ->
            error(Reason)
    end.

get_sessions_by_key_id_between(From, To, Limit) ->
    Options0 = case Limit of
        undefined -> [];
        _ -> [{max_results, Limit}]
    end,
    Options = [{return_terms, true} | Options0],

    Result = get_index_range(
        ?SESSION_BUCKET,
        ?KEY_ID_INDEX,
        From,
        To,
        Options
    ),

    case Result of
        {ok, [#index_results_v1{terms = Keys}]} when Keys =/= undefined ->
            {ok, Keys};
        {ok, _} ->
            {ok, []};
        {error, Reason} ->
            error(Reason)
    end.


refresh_sessions() ->
    refresh_sessions(undefined, init).

refresh_sessions(undefined, processing) ->
    ok;

refresh_sessions(Continuation0, _Step) ->
    Result = get_keys(?SESSION_BUCKET, ?REFRESH_BATCH, Continuation0),

    case Result of
        {ok, [#index_results_v1{keys = Keys, continuation = Continuation}]} when Keys =/= undefined ->
            [ok = refresh_session(Key) || Key <- Keys],
            refresh_sessions(Continuation, processing);
        {ok, _} ->
            {ok, {[], undefined}};
        {error, Reason} ->
            error(Reason)
    end.

refresh_session(SessionKey) ->
    case get_session_obj(SessionKey) of
        {ok, Obj0} ->
            CreatedAt = cds_utils:current_time(),
            SessionObj = set_indexes(Obj0, [{?CREATED_AT_INDEX, CreatedAt}]),
            case batch_put([[SessionObj]]) of
                ok ->
                    ok;
                {error, Reason} ->
                    error(Reason)
            end;
        {error, not_found} ->
            ok
    end.

get_session_obj(SessionKey) ->
    case get(?SESSION_BUCKET, SessionKey) of
        OK = {ok, _SessionObj} ->
            OK;
        {error, notfound} ->
            {error, not_found};
        {error, Reason} ->
            error(Reason)
    end.

get_keys(Bucket, Batch, Continuation) ->
    Options = case Continuation of
        undefined -> [{max_results, Batch}];
        _ -> [
            {max_results,  Batch},
            {continuation, Continuation}
        ]
    end,

    get_index_eq(
        Bucket,
        <<"$bucket">>,
        <<"_">>,
        Options
    ).
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

prepare_session_obj(Session, Cvv, CurrentTime, KeyID) ->
    case riakc_obj:new(?SESSION_BUCKET, Session, Cvv)  of
        Error = {error, _} ->
            error(Error);
        Obj ->
            set_indexes(
                Obj,
                [{?CREATED_AT_INDEX, CurrentTime}, {?KEY_ID_INDEX, KeyID}]
            )
    end.

prepare_token_obj(Token, CardData, KeyID) ->
    case riakc_obj:new(?SESSION_BUCKET, Token, CardData)  of
        Error = {error, _} ->
            error(Error);
        Obj ->
            set_indexes(
                Obj,
                [{?KEY_ID_INDEX, KeyID}]
            )
    end.

set_indexes(Obj, Indexes) ->
    MD1 = riakc_obj:get_update_metadata(Obj),
    MD2 = riakc_obj:set_secondary_index(
        MD1,
        [{Index, [Value]} || {Index, Value} <- Indexes]
    ),
    riakc_obj:update_metadata(Obj, MD2).

