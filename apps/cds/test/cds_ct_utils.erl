-module(cds_ct_utils).

-export([start_clear/1]).
-export([stop_clear/1]).

-export([set_riak_storage/1]).
-export([set_ets_storage/1]).

-export([store/1]).
-export([store/2]).
-export([lookup/1]).

-export([start_stash/0]).

-export([call/4]).

%%
%% Types
%%

-type config() :: [{atom(), any()}] | atom().

-export_type([config/0]).

-define(file_path(Config, Filename), filename:join(config(data_dir, Config), Filename)).
-define(read_file(Config, Filename), file:read_file(?file_path(Config, Filename))).

%%
%% API
%%

-spec start_clear(config()) -> config().
start_clear(Config) ->
    IP = "127.0.0.1",
    Port = 8022,
    RootUrl = "http://" ++ IP ++ ":" ++ erlang:integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    ok = clean_storage(StorageConfig),
    {ok, EncPrivateKey1} = ?read_file(Config, "enc.1.priv.json"),
    EncPrivateKeys = #{<<"1">> => EncPrivateKey1},
    {ok, SigPrivateKey1} = ?read_file(Config, "sig.1.priv.json"),
    SigPrivateKeys = #{<<"1">> => SigPrivateKey1},
    Apps =
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_logger}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, IP},
            {port, Port},
            {transport_opts, #{}},
            {protocol_opts, #{
                request_timeout => 60000
            }},
            {shutdown_timeout, 0},
            {keyring, #{
                url => <<"https://kds:8023">>,
                server_cn => "Test Server",
                cacertfile => ?file_path(Config, "ca.crt"),
                certfile => ?file_path(Config, "client.pem")
            }},
            {keyring_fetch_interval, 1000}
        ] ++ StorageConfig ++ CleanConfig ++ Recrypting)
    ,
    [
        {apps, lists:reverse(Apps)},
        {root_url, genlib:to_binary(RootUrl)},
        {kds_root_url, <<"http://kds:8022">>},
        {enc_private_keys, EncPrivateKeys},
        {sig_private_keys, SigPrivateKeys}
    ] ++ Config.

-spec stop_clear(config()) -> ok.
stop_clear(C) ->
    [ok = application:stop(App) || App <- config(apps, C)],
    ok.

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

-spec store([{any(), any()}]) -> ok.
store(KVs) when is_list(KVs) ->
    [store(Key, Value) || {Key, Value} <- KVs],
    ok.

-spec store(any(), any()) -> ok.
store(Key, Value) ->
    cds_ct_stash:put(Key, Value).

-spec lookup(any()) -> any().
lookup(Key) ->
    cds_ct_stash:get(Key).

-spec start_stash() -> ok.
start_stash() ->
    case cds_ct_stash:start() of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            ok
    end.

-spec call(atom(), atom(), list(), woody:url()) -> cds_woody_client:result().
call(Service, Method, Args, RootUrl) ->
    Strategy = genlib_retry:linear(3, 1000),
    call(Service, Method, Args, RootUrl, Strategy).

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

call(Service, Method, Args, RootUrl, Strategy) ->
    try
        ExtraOpts = #{
            transport_opts => #{
                max_connections => 1000
            }
        },
        cds_woody_client:call(Service, Method, Args, RootUrl, ExtraOpts)
    catch
        error:{woody_error, {external, resource_unavailable, <<"Keyring is unavailable">>}} = Error ->
            case genlib_retry:next_step(Strategy) of
                {wait, Timeout, NewStrategy} ->
                    ok = timer:sleep(Timeout),
                    call(Service, Method, Args, RootUrl, NewStrategy);
                finish ->
                    erlang:error(Error)
            end
    end.
