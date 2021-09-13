-module(cds_keyring).

-behaviour(gen_server).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% API
-export([get_key/1]).
-export([get_keys_except/1]).
-export([get_current_key/0]).
-export([get_current_key_with_meta/0]).
-export([get_outdated_keys/0]).
-export([get_version/0]).
-export([deduplication_hash_opts/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_continue/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export_type([key/0]).
-export_type([key_id/0]).
-export_type([meta/0]).
-export_type([version/0]).

-type key() :: cds_proto_keyring_thrift:'KeyData'().
-type key_id() :: cds_proto_keyring_thrift:'KeyId'().
-type keyring() :: cds_proto_keyring_thrift:'Keyring'().
-type version() :: integer().
-type keys() :: #{
    key_id() => cds_proto_keyring_thrift:'Key'()
}.

-type meta() :: #{
    retired := boolean(),
    security_parameters := #{
        deduplication_hash_opts := cds_hash:scrypt_options()
    }
}.

-type state() :: #{
    timer := reference() | undefined
}.

-define(DEFAULT_FETCH_INTERVAL, 60 * 1000).
-define(DEFAULT_TIMEOUT, 10 * 1000).

-define(KEYRING_TAB, ?MODULE).
-define(KEYRING_TAB_CONFIG, [
    protected,
    ordered_set,
    named_table,
    {read_concurrency, true},
    {write_concurrency, false}
]).

-define(KEYRING_VERSION_KEY, version).
-define(KEYRING_CURRENT_KEY, current_key_id).
-define(KEYRING_META_KEY(ID), {meta, ID}).

-define(KEYRING_KEY(ID), {?MODULE, key, ID}).

%%% API

-spec get_key(key_id()) -> {ok, {key_id(), key()}} | {error, not_found}.
get_key(KeyID) ->
    try
        Key = persistent_term:get(?KEYRING_KEY(KeyID)),
        {ok, {KeyID, Key}}
    catch
        error:badarg ->
            _ = logger:error("Could not get key with id: ~p", [KeyID]),
            {error, not_found}
    end.

-spec get_keys_except(key_id()) -> [{key(), meta()}].
get_keys_except(ExceptID) ->
    ets:foldl(
        fun
            ({?KEYRING_META_KEY(KeyID), #{retired := false} = Meta}, Acc) when KeyID /= ExceptID ->
                Key = persistent_term:get(?KEYRING_KEY(KeyID)),
                [{Key, Meta} | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        ?KEYRING_TAB
    ).

-spec get_current_key() -> {key_id(), key()}.
get_current_key() ->
    KeyID = get_current_key_id(),
    Key = persistent_term:get(?KEYRING_KEY(KeyID)),
    {KeyID, Key}.

-spec get_current_key_with_meta() -> {{key_id(), key()}, meta()}.
get_current_key_with_meta() ->
    {KeyID, _} = Key = get_current_key(),
    {Key, get_meta(KeyID)}.

-spec get_outdated_keys() -> {[{key_id(), key_id()}], #{key_id() => meta()}}.
get_outdated_keys() ->
    CurrentKeyID = get_current_key_id(),
    ets:foldl(
        fun
            ({?KEYRING_META_KEY(KeyID), Meta}, {Intervals, MetaAcc}) when KeyID < CurrentKeyID ->
                NewIntervals =
                    case Intervals of
                        [] ->
                            [{KeyID, KeyID}];
                        [{KeyID1, KeyID2}] ->
                            [{erlang:min(KeyID1, KeyID), erlang:max(KeyID2, KeyID)}]
                    end,
                {NewIntervals, MetaAcc#{KeyID => Meta}};
            (_, Acc) ->
                Acc
        end,
        {[], #{}},
        ?KEYRING_TAB
    ).

-spec get_version() -> version() | undefined.
get_version() ->
    try ets:lookup(?KEYRING_TAB, ?KEYRING_VERSION_KEY) of
        [{_, Version}] ->
            Version;
        [] ->
            undefined
    catch
        error:badarg ->
            undefined
    end.

-spec deduplication_hash_opts(meta()) -> cds_hash:scrypt_options().
deduplication_hash_opts(#{security_parameters := SecParams}) ->
    maps:get(deduplication_hash_opts, SecParams).

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

-spec init(any()) -> {ok, state(), {continue, init}}.
init(_) ->
    ok = create_table(),
    {ok, #{timer => undefined}, {continue, init}}.

-spec handle_call(term(), term(), state()) -> {reply, {error, undefined}, state()}.
handle_call(_Msg, _From, State) ->
    {reply, {error, undefined}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_continue(init, state()) -> {noreply, state()}.
handle_continue(init, State) ->
    _ = fetch_keyring(),
    {noreply, State#{timer => start_timer()}}.

-spec handle_info(timeout | any(), state()) -> {noreply, state()}.
handle_info({timeout, Timer, fetch}, #{timer := Timer} = State) ->
    _ = fetch_keyring(),
    {noreply, State#{timer => start_timer()}};
handle_info(Info, State) ->
    _ = logger:warning("Unhandled info: ~p", [Info]),
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

-spec create_table() -> ok.
create_table() ->
    ?KEYRING_TAB = ets:new(?KEYRING_TAB, ?KEYRING_TAB_CONFIG),
    ok.

-spec set_version(version()) -> ok.
set_version(Version) ->
    true = ets:insert(?KEYRING_TAB, {?KEYRING_VERSION_KEY, Version}),
    ok.

-spec get_current_key_id() -> key_id() | no_return().
get_current_key_id() ->
    try ets:lookup(?KEYRING_TAB, ?KEYRING_CURRENT_KEY) of
        [{_, CurrentKeyID}] ->
            CurrentKeyID;
        [] ->
            erlang:throw(no_keyring)
    catch
        error:badarg ->
            erlang:throw(no_keyring)
    end.

-spec set_current_key_id(key_id()) -> ok.
set_current_key_id(KeyID) ->
    true = ets:insert(?KEYRING_TAB, {?KEYRING_CURRENT_KEY, KeyID}),
    ok.

-spec fetch_keyring() -> ok | {error, {invalid_status, not_initialized} | tuple()}.
fetch_keyring() ->
    CurrentVersion = get_version(),
    try get_keyring() of
        #cds_Keyring{version = CurrentVersion} ->
            % no changes
            ok;
        Keyring ->
            ok = store_keyring(Keyring),
            _ = logger:info("New keyring version received: ~p", [get_version()]),
            ok
    catch
        #cds_InvalidStatus{status = Status} ->
            _ = logger:error("Could not fetch keyring: ~p: ~p", [invalid_status, Status]),
            {error, {invalid_status, Status}};
        Class:Error ->
            _ = logger:error("Could not fetch keyring: ~p: ~p", [Class, Error]),
            {error, {Class, Error}}
    end.

-spec store_keyring(keyring()) -> ok.
store_keyring(#cds_Keyring{version = Version, keys = Keys, current_key_id = CurrentKeyID}) ->
    ok = store_keys(Keys),
    ok = set_current_key_id(CurrentKeyID),
    ok = set_version(Version).

-spec store_keys(keys()) -> ok.
store_keys(Keys) ->
    maps:fold(
        fun(KeyID, #cds_Key{data = Key, meta = Meta}, _) ->
            ok = store_key(KeyID, Key),
            ok = store_meta(KeyID, Meta)
        end,
        ok,
        Keys
    ).

-spec store_key(key_id(), key()) -> ok.
store_key(KeyID, Key) ->
    try
        _ = persistent_term:get(?KEYRING_KEY(KeyID)),
        ok
    catch
        error:badarg ->
            ok = persistent_term:put(?KEYRING_KEY(KeyID), Key)
    end.

-spec store_meta(key_id(), cds_proto_keyring_thrift:'KeyMeta'()) -> ok.
store_meta(KeyID, #cds_KeyMeta{retired = Retired, security_parameters = SecParams}) ->
    #cds_SecurityParameters{
        deduplication_hash_opts = #cds_ScryptOptions{
            n = N,
            r = R,
            p = P
        }
    } = SecParams,
    Meta = #{
        retired => Retired,
        security_parameters => #{
            deduplication_hash_opts => {N, R, P}
        }
    },
    true = ets:insert(?KEYRING_TAB, {?KEYRING_META_KEY(KeyID), Meta}),
    ok.

-spec get_meta(key_id()) -> meta().
get_meta(KeyID) ->
    [{_, Meta}] = ets:lookup(?KEYRING_TAB, ?KEYRING_META_KEY(KeyID)),
    Meta.

-spec get_keyring() -> keyring() | no_return().
get_keyring() ->
    {ok, Opts} = application:get_env(cds, keyring),
    RootUrl = maps:get(url, Opts),
    SslOptions = maps:get(ssl_options, Opts),
    TransOpts = maps:get(transport_opts, Opts, #{}),
    ExtraOpts = #{
        transport_opts => maps:merge(TransOpts, #{
            ssl_options => SslOptions
        })
    },
    WoodyContext1 = woody_context:new(),
    Deadline = woody_deadline:from_timeout(maps:get(timeout, Opts, ?DEFAULT_TIMEOUT)),
    WoodyContext2 = woody_context:set_deadline(Deadline, WoodyContext1),
    cds_woody_client:call(keyring_storage, 'GetKeyring', {}, RootUrl, ExtraOpts, WoodyContext2).

-spec start_timer() -> reference().
start_timer() ->
    erlang:start_timer(fetch_interval(), self(), fetch).

-spec fetch_interval() -> timeout().
fetch_interval() ->
    application:get_env(cds, keyring_fetch_interval, ?DEFAULT_FETCH_INTERVAL).
