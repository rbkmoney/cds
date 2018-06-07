-module(cds_card_storage).

%% TODO remove circular dependencies in types (cds:hash, cds:token etc.)

-type limit() :: non_neg_integer() | undefined.
-type continuation() :: term().
-type timestamp() :: pos_integer().

-export([get_namespaces/0]).
-export([get_token/1]).
-export([get_cardholder_data/1]).
-export([get_session_card_data/2]).
-export([get_session_data/1]).
-export([put_card_data/7]).
-export([delete_session/1]).
-export([get_sessions_created_between/4]).
-export([get_tokens_by_key_id_between/4]).
-export([get_sessions_by_key_id_between/4]).
-export([update_cardholder_data/4]).
-export([update_session_data/3]).
-export([refresh_session_created_at/1]).
-export([get_sessions/2]).
-export([get_sessions_info/2]).
-export([get_tokens/2]).

%% TODO Those names looks so shitty for backward compatibility
-define(TOKEN_NS, <<"t">>).
-define(SESSION_NS, <<"s">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).
-define(CARD_DATA_HASH_INDEX, {binary_index, "card_data_salted_hash"}).

-spec get_namespaces() -> [cds_storage:namespace()].
get_namespaces() ->
    [?TOKEN_NS, ?SESSION_NS].

-spec get_token(cds:hash()) -> cds:token() | no_return().
get_token(Hash) ->
    case cds_storage:search_by_index_value(?TOKEN_NS, ?CARD_DATA_HASH_INDEX, Hash, undefined, undefined) of
        {[Token], _} ->
            Token;
        {[], _} ->
            throw(not_found);
        {ManyTokens, _} ->
            _ = assert_card_data_equal(ManyTokens, Hash),
            hd(ManyTokens)
    end.

-spec get_cardholder_data(cds:token()) -> cds:ciphertext() | no_return().
get_cardholder_data(Token) ->
    {CardData, _, _} = cds_storage:get(?TOKEN_NS, Token),
    CardData.

-spec get_session_data(cds:session()) -> cds:ciphertext() | no_return().
get_session_data(Session) ->
    case cds_storage:get(?SESSION_NS, Session) of
        {Data, undefined, _} ->
            Data;
        {Data, Meta, _} ->
            {Data, Meta}
    end.

-spec get_session_card_data(cds:token(), cds:session()) ->
    {CardData :: cds:ciphertext(), SessionData :: cds:ciphertext()} | no_return().
get_session_card_data(Token, Session) ->
    {get_cardholder_data(Token), get_session_data(Session)}.

-spec put_card_data(
    cds:token(),
    cds:session(),
    cds:hash(),
    CardData :: cds:ciphertext(),
    SessionData :: cds:ciphertext(),
    cds_keyring:key_id(),
    timestamp()
) -> ok | no_return().
put_card_data(Token, Session, Hash, CardData, {SessionData, SessionMeta}, KeyID, CreatedAt) ->
    ok = cds_storage:put(
        ?TOKEN_NS,
        Token,
        CardData,
        undefined,
        prepare_card_data_indexes(Hash, KeyID)
    ),
    ok = cds_storage:put(
        ?SESSION_NS,
        Session,
        SessionData,
        SessionMeta,
        [{?CREATED_AT_INDEX, CreatedAt}, {?KEY_ID_INDEX, KeyID}]
    ).

-spec delete_session(cds:session()) -> ok | no_return().
delete_session(Session) ->
    cds_storage:delete(?SESSION_NS, Session).

-spec get_sessions_created_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:session()], continuation()} | no_return().
get_sessions_created_between(From, To, Limit, Continuation) when From =< To ->
    cds_storage:search_by_index_range(?SESSION_NS, ?CREATED_AT_INDEX, From, To, Limit, Continuation).

-spec get_sessions_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:session()], continuation()} | no_return().
get_sessions_by_key_id_between(From, To, Limit, Continuation) when From =< To ->
    cds_storage:search_by_index_range(?SESSION_NS, ?KEY_ID_INDEX, From, To, Limit, Continuation).

-spec get_tokens_by_key_id_between(
    non_neg_integer(),
    pos_integer(),
    limit(),
    continuation()
) -> {[cds:token()], continuation()} | no_return().
get_tokens_by_key_id_between(From, To, Limit, Continuation) when From =< To ->
    cds_storage:search_by_index_range(?TOKEN_NS, ?KEY_ID_INDEX, From, To, Limit, Continuation).

-spec refresh_session_created_at(cds:session()) -> ok.
refresh_session_created_at(Session) ->
    {Data, Meta, _} = cds_storage:get(?SESSION_NS, Session),
    ok = cds_storage:update(
        ?SESSION_NS,
        Session,
        Data,
        Meta,
        [{?CREATED_AT_INDEX, cds_utils:current_time()}]
    ).

-spec get_sessions(limit(), continuation()) -> {[cds:session()], continuation()} | no_return().
get_sessions(Limit, Continuation) ->
    cds_storage:get_keys(?SESSION_NS, Limit, Continuation).

-spec get_sessions_info(limit(), continuation()) -> {[{cds:session(), Info :: map()}], continuation()} | no_return().
get_sessions_info(Limit, Continuation) ->
    {Keys, Cont} = cds_storage:get_keys(?SESSION_NS, Limit, Continuation),
    SessionsInfo = lists:foldl(
        fun(K, Acc) ->
            try
                {_, _, Indexes} = cds_storage:get(?SESSION_NS, K),
                {?CREATED_AT_INDEX, CreatedAt} = lists:keyfind(?CREATED_AT_INDEX, 1, Indexes),
                [{K, #{lifetime => cds_utils:current_time() - CreatedAt}} | Acc]
            catch
                not_found ->
                    Acc;
                Class:Error ->
                    [{K, #{error => {Class, Error}}} | Acc]
            end
        end,
        [],
        Keys
    ),
    {SessionsInfo, Cont}.

-spec get_tokens(limit(), continuation()) -> {[cds:token()], continuation()} | no_return().
get_tokens(Limit, Continuation) ->
    cds_storage:get_keys(?TOKEN_NS, Limit, Continuation).

-spec update_cardholder_data(cds:token(), cds:ciphertext(), cds:hash(), cds_keyring:key_id()) -> ok.
update_cardholder_data(Token, CardData, Hash, KeyID) ->
    ok = cds_storage:update(
        ?TOKEN_NS,
        Token,
        CardData,
        <<"">>,
        prepare_card_data_indexes(Hash, KeyID)
    ).

-spec update_session_data(
    cds:session(),
    cds:ciphertext(),
    cds_keyring:key_id()
) -> ok.
update_session_data(Session, {SessionData, SessionMeta}, KeyID) ->
    ok = cds_storage:update(
        ?SESSION_NS,
        Session,
        SessionData,
        SessionMeta,
        [{?KEY_ID_INDEX, KeyID}]
    ).

%%
%% Internals
%%

prepare_card_data_indexes(Hash, KeyID) ->
    [{?CARD_DATA_HASH_INDEX, Hash}, {?KEY_ID_INDEX, KeyID}].

assert_card_data_equal([Token | OtherTokens], Hash) ->
    % TODO same card data could be encrypted with different keys
    FirstData = get_cardholder_data(Token),
    lists:all(
        fun(T) ->
              FirstData =:= get_cardholder_data(T)
        end,
        OtherTokens
    ) orelse error({<<"Hash collision detected">>, Hash}).
