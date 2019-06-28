-module(cds_keyring).
-behaviour(gen_server).

-compile(no_auto_import).

-include_lib("cds_proto/include/cds_proto_keyring_thrift.hrl").

%% API
-export([is_available/0]).
-export([get_key/1]).
-export([get_keys_except/1]).
-export([get_current_key/0]).
-export([get_outdated_keys/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export_type([key/0]).
-export_type([key_id/0]).

-type key()     :: cds_proto_keyring_thrift:'KeyData'().
-type key_id()  :: cds_proto_keyring_thrift:'KeyId'().
-type meta()    :: cds_proto_keyring_thrift:'KeyMeta'().
-type keyring() :: cds_proto_keyring_thrift:'Keyring'().
-type version() :: integer().
-type keys()    :: #{
    key_id() => cds_proto_keyring_thrift:'Key'()
}.

-type state() :: #{}.

-define(DEFAULT_FETCH_INTERVAL, 60 * 1000).

-define(KEYRING_TAB, ?MODULE).
-define(KEYRING_TAB_CONFIG, [
    public,
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

-spec is_available() ->
    boolean().

is_available() ->
    try
        _ = get_current_key_id(),
        true
    catch
        throw:no_keyring ->
            false
    end.

-spec get_key(key_id()) ->
    {ok, {key_id(), key()}} | {error, not_found}.

get_key(KeyID) ->
    try
        Key = persistent_term:get(?KEYRING_KEY(KeyID)),
        {ok, {KeyID, Key}}
    catch
        error:badarg ->
            {error, not_found}
    end.

-spec get_keys_except(key_id()) ->
    [key()].

get_keys_except(ExceptId) ->
    ets:foldl(
        fun({?KEYRING_META_KEY(KeyID), #'KeyMeta'{retired = false}}, Acc) when KeyID /= ExceptId ->
                Key = persistent_term:get(?KEYRING_KEY(KeyID)),
                [Key | Acc];
           (_, Acc) ->
               Acc
        end,
        [],
        ?KEYRING_TAB
    ).

-spec get_current_key() ->
    {key_id(), key()}.

get_current_key() ->
    CurrentKeyID = get_current_key_id(),
    CurrentKey   = persistent_term:get(?KEYRING_KEY(CurrentKeyID)),
    {CurrentKeyID, CurrentKey}.

-spec get_outdated_keys() ->
    [{key_id(), key_id()}].

get_outdated_keys() ->
    CurrentKeyID = get_current_key_id(),
    ets:foldl(
        fun({?KEYRING_META_KEY(KeyID), #'KeyMeta'{retired = false}}, Acc) when KeyID < CurrentKeyID ->
                case Acc of
                    [] ->
                        [{KeyID, KeyID}];
                    [{KeyID1, KeyID2}] ->
                        [{erlang:min(KeyID1, KeyID), erlang:max(KeyID2, KeyID)}]
                end;
           (_, Acc) ->
               Acc
        end,
        [],
        ?KEYRING_TAB
    ).

-spec start_link() ->
    {ok, pid()} | {error, {already_started, pid()}}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

-spec init(any()) ->
    {ok, state(), timeout()}.

init(_) ->
    ok = create_table(),
    _ = fetch_keyring(),
    {ok, #{}, fetch_interval()}.

-spec handle_call(term(), term(), state()) ->
    {reply, {error, undefined}, state()}.

handle_call(_Msg, _From, State) ->
    {reply, {error, undefined}, State}.

-spec handle_cast(term(), state()) ->
    {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(timeout | any(), state()) ->
    {noreply, state, timeout()} | {noreply, state()}.

handle_info(timeout, State) ->
    _ = fetch_keyring(),
    {noreply, State, fetch_interval()};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) ->
    ok.

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) ->
    {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

-spec create_table() ->
    ok.

create_table() ->
    ?KEYRING_TAB = ets:new(?KEYRING_TAB, ?KEYRING_TAB_CONFIG),
    ok.

-spec get_version() ->
    version() | undefined.

get_version() ->
    case ets:lookup(?KEYRING_TAB, ?KEYRING_VERSION_KEY) of
        [{_, Version}] ->
            Version;
        [] ->
            undefined
    end.

-spec set_version(version()) ->
    ok.

set_version(Version) ->
    true = ets:insert(?KEYRING_TAB, {?KEYRING_VERSION_KEY, Version}),
    ok.

-spec get_current_key_id() ->
    key_id() | no_return().

get_current_key_id() ->
    case ets:lookup(?KEYRING_TAB, ?KEYRING_CURRENT_KEY) of
        [{_, CurrentKeyID}] ->
            CurrentKeyID;
        [] ->
            erlang:throw(no_keyring)
    end.

-spec set_current_key_id(key_id()) ->
    ok.

set_current_key_id(KeyID) ->
    true = ets:insert(?KEYRING_TAB, {?KEYRING_CURRENT_KEY, KeyID}),
    ok.

-spec fetch_keyring() ->
    ok | {error, {invalid_status, not_initialized} | tuple()}.

fetch_keyring() ->
    CurrentVersion = get_version(),
    try get_keyring() of
        #'Keyring'{version = CurrentVersion} ->
            ok; % no changes
        Keyring ->
            ok = store_keyring(Keyring)
    catch
        #'InvalidStatus'{status = Status} ->
            _ = logger:error("Could not fetch keyring: ~p: ~p", [invalid_status, Status]),
            {error, {invalid_status, Status}};
        Class:Error ->
            _ = logger:error("Could not fetch keyring: ~p: ~p", [Class, Error]),
            {error, {Class, Error}}
    end.

-spec store_keyring(keyring()) ->
    ok.

store_keyring(#'Keyring'{version = Version, keys = Keys, current_key_id = CurrentKeyID}) ->
    ok = store_keys(Keys),
    ok = set_current_key_id(CurrentKeyID),
    ok = set_version(Version).

-spec store_keys(keys()) ->
    ok.

store_keys(Keys) ->
    maps:fold(
        fun(KeyID, #'Key'{data = Key, meta = Meta}, _) ->
            ok = store_key(KeyID, Key),
            ok = store_meta(KeyID, Meta)
        end,
        ok,
        Keys
    ).

-spec store_key(key_id(), key()) ->
    ok.

store_key(KeyID, Key) ->
    try
        _ = persistent_term:get(?KEYRING_KEY(KeyID)),
        ok
    catch
        error:badarg ->
            ok = persistent_term:put(?KEYRING_KEY(KeyID), Key)
    end.

-spec store_meta(key_id(), meta()) ->
    ok.

store_meta(KeyID, Meta) ->
    true = ets:insert(?KEYRING_TAB, {?KEYRING_META_KEY(KeyID), Meta}),
    ok.

-spec get_keyring() ->
    keyring() | no_return().

get_keyring() ->
    {ok, Opts} = application:get_env(cds, keyring),
    RootUrl    = maps:get(url, Opts),
    ServerCN   = maps:get(server_cn, Opts),
    CACert     = maps:get(cacertfile, Opts),
    ClientCert = maps:get(certfile, Opts),
    ExtraOpts  = #{
        transport_opts => #{
            ssl_options => [
                {server_name_indication, ServerCN},
                {verify,                 verify_peer},
                {cacertfile,             CACert},
                {certfile,               ClientCert}
            ]
        }
    },
    cds_woody_client:call(keyring_storage, 'GetKeyring', [], RootUrl, ExtraOpts).

-spec fetch_interval() ->
    timeout().

fetch_interval() ->
    application:get_env(cds, keyring_fetch_interval, ?DEFAULT_FETCH_INTERVAL).
