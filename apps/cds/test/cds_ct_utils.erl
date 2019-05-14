-module(cds_ct_utils).

-export([start_clear/1]).
-export([stop_clear/1]).

-export([set_riak_storage/1]).
-export([set_ets_storage/1]).

-export([store/2]).
-export([store/3]).
-export([lookup/2]).

%%
%% Types
%%

-type config() :: [{atom(), any()}] | atom().

-export_type([config/0]).

%%
%% API
%%

-spec start_clear(config()) -> config().
start_clear(Config) ->
    IP = "127.0.0.1",
    Port = 8022,
    RootUrl = "http://" ++ IP ++ ":" ++ integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    ok = clean_storage(StorageConfig),
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {crash_log, false},
            {handlers, [
                % {lager_common_test_backend, [debug, {lager_logstash_formatter, []}]}
                {lager_common_test_backend, warning}
            ]}
        ]) ++
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_lager}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, IP},
            {port, Port},
            {keyring_storage, cds_keyring_storage_env},
            {net_opts, [
                % Bump keepalive timeout up to a minute
                {timeout, 60000}
            ]}
        ] ++ StorageConfig ++ CleanConfig ++ Recrypting)
    ,
    start_stash([
        {apps, lists:reverse(Apps)},
        {root_url, genlib:to_binary(RootUrl)}
    ]).

-spec stop_clear(config()) -> ok.
stop_clear(C) ->
    _ = (catch cds_keyring_storage_env:delete()),
    [ok = application:stop(App) || App <- config(apps, C)],
    stop_stash(C).

-spec set_riak_storage(config()) -> config().
set_riak_storage(C) ->
    StorageConfig = [
        {storage, cds_storage_riak},
        {cds_storage_riak, #{
            conn_params => #{
                host => "riakdb",
                port => 8087,
                options => #{
                    connect_timeout => 1000,
                    keepalive => true
                }
            },
            timeout => 5000
        }}
    ],
    [{storage_config, StorageConfig} | C].

-spec set_ets_storage(config()) -> config().
set_ets_storage(C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    [{storage_config, StorageConfig} | C].

-spec store([{any(), any()}], config()) -> ok.
store(KVs, C) when is_list(KVs) ->
    [store(Key, Value, C) || {Key, Value} <- KVs],
    ok.

-spec store(any(), any(), config()) -> ok.
store(Key, Value, C) ->
    cds_ct_stash:put(config(stash, C), Key, Value).

-spec lookup(any(), config()) -> any().
lookup(Key, C) ->
    cds_ct_stash:get(config(stash, C), Key).

%%
%% Internals
%%

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

clean_storage(CdsEnv) ->
    case genlib_opts:get(storage, CdsEnv) of
        cds_storage_riak ->
            clean_riak_storage(CdsEnv);
        cds_storage_ets ->
            ok
    end.

clean_riak_storage(CdsEnv) ->
    _ = application:start(riakc),
    _ = application:set_env(riakc, allow_listing, true),
    #{conn_params := #{
        host := Host,
        port := Port
    }} = genlib_opts:get(cds_storage_riak, CdsEnv),
    {ok, Client} = riakc_pb_socket:start_link(Host, Port),
    {ok, Buckets} = riakc_pb_socket:list_buckets(Client),
    lists:foreach(
        fun(B) ->
            {ok, Keys} = riakc_pb_socket:list_keys(Client, B),
            [
                ok = riakc_pb_socket:delete(Client, B, K)
                    || K <- Keys
            ]
        end,
        Buckets
    ),
    ok.

start_stash(C) ->
    [{stash, cds_ct_stash:start()} | C].

stop_stash(C) ->
    cds_ct_stash:stop(config(stash, C)).
