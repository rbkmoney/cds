-module(cds_ct_utils).

-export([start_clear/1]).
-export([start_clear/2]).
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
    start_clear(Config, start_stash()).

-spec start_clear(config(), pid()) -> config().
start_clear(Config, Stash) ->
    IP = "127.0.0.1",
    Port = 8022,
    RootUrl = "http://" ++ IP ++ ":" ++ integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    ok = clean_storage(StorageConfig),
    PrivateKeys = [
        <<"-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----">>,
        <<"-----BEGIN RSA PRIVATE KEY-----
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
-----END RSA PRIVATE KEY-----">>,
        <<"-----BEGIN RSA PRIVATE KEY-----
MIIJKQIBAAKCAgEArVb6bKFTNEkJ3Ok5IVxjBs+Emfo3Ya3DTV8duDlN7A/L7wzD
/qH+K0+K9b7n0igvs5vZtXKm7+pRakQoJDhs3QWLiRxZnrU6Vm32vy6Y0C5fTRiR
o4VfdMYKY0Ah1K5lS8buLTGrjHy/sBA+n4BclALY+WuaBYpoVaqNjBqmmJq+xyoj
tZVkXBMG0HoBwj+705nW9spYLr4o2opW+xxarOxfiWtTDH0Wfa4jsvwphGL0pR8/
lZL8sHr45IsA1/h9EuvqLZzYSCrbqaHFCEExWC1lRIWF/Gadu92UkPSwrft1Yn/c
kf90Bc4mU/eiuay7k0FYgmG1fj4isl+KZ2UHlmCSXziY+smlEXQ/qLrokXD9B8yp
ygSB5Vk6rKZzDOObsFMFJFGAibvW1BgimuCe9ROK/xgy1+tFEtZSJf1b+5YPHxKn
TuvTJ3VsJR3P5vTVq5Pt5CNKVG6WEyrAUTkdmOVdjcTrLVsd/PG9GOTHXW+PJdXN
bsdIUlQet2Kc4fWc9Je+InWTpaZxNO5T4VPkwANrZ2JD0xElXOUfV/dHggSSU6k6
/St+UhGSCgqEMIXeIBVGQr9KOMCDbvN5SK2bKy1ZDi7DbzxUFgx+X8FybVQiH/GC
MREnZKM+3pi+ir3Ay8T5uwzkp3Q9uGl+lUiO3VU+SO5HukksCrWOQ/ZhQzcCAwEA
AQKCAgEAp5b3if2z11Jikc5/qOyM5bHPb5vG3c7Fi9HRhyMIXpqrPlVmTZNLjY5b
ONu5gKvP6uRc6hyOoFk5G/gHo3VWc57/2LGrSFHJtHSe4xrnLigXhg+iB6pUry8m
Le18iAeQ8mHEN7fVN4AJSO17znSpjYTvSL8q6/50f5blQbcmqVa76w2n9tEYdhkD
Np8vgiJis+QCPRtHhPHfx+OG00QHuzbR66fzZJiwHRU3eL5qWY/fHqKGtFOnse1D
JJvsVeLu8lNbV3xUc/nT5T79o8x8K9fAGC7Ma0q/aDqEgF3DUINVa8VZeWK1R0OY
NQiwsLn7pRTBwVU8x3DF1RWgV/ULDlamjmTFwBEMV8qtMDlvjUMdLr/pKN0hPoqY
TIQQ2s+a1e+5xqQwfmY3KSlSEYAR5H4ohWWnQqz4B4T7qz0M6Qw0+LgqqBNCJqIb
Fu49Rzxu3/9pyvBrA1ieQNxCDU00Ub8br5Rq3abo6wlZTB1yksQP6+iikWKUaD98
OXNmGbMcd0+iPfZg7VFkyaHy1INxKjwDIldQ7Kc7PoqEkUrREJ2mU+4fWtqsPwfA
C36cSyi8+hOBr4yVaQYosf7NREfTgqtP7DdCCinwNbpWo+c1tGrnCpNqnm3lJLx0
wnrlmTKy+s6QMPqZICjZOkhZvaxUA7IPYzG/1XF8ZWPZbnqLrlkCggEBANbyqfJk
TRr7yTUmxxdHdJmxTEt4JKW0zcOIXEMPtcs4wEozkjSnh4lkOwJYTA3XL8L5IGFF
yrFRC/gAQ/Lan6h9GWL55THAW7MoDLQOdXDOUZHlevXgZ73KE8rWoQY/UiDmVqSF
wVGZwBzZxqNK2pU8hVVghkzaFN4jpWGbRRTRSvMEmQ7GqXjieNOtu4uM+zfD9D0M
bq1yHvq2Yup6kwkfCTZZvAWhJw1AZJygpM6BWl0otgIkMPPcK9G6T2IGl0/yMyQC
OhBb/6pzmoxX6E/s5RAUOCZzKzYUpy20bNZh4U/wcih2h98K5dsAvNSyZdo0j20h
boMw2yQ37fTvUrsCggEBAM5x/Sb/o6GLie+rtQloQYcJA06UGUp6eqvvPGZbKc+J
8I2peFxKZQ/vE07EuZ0by/0r8wctmMY/LD2H3xa58ieevsF1/QZ4CTi43IUIyJh5
zQ5Nx6Yaaw3FmI6okNydhBzAK7RWwXn5O/xxwMsDLsT2oq5eaWiuzKFZu5bfKVjt
M+tZd1TLP77EM+4G68KO/d3qFF08pQECoxJJVMoUObHrMJsRFi8l33Pp0Ne9PtfP
DO4W74qKgAFtgbuf4D7bzO310QFhYVbmHSbz1USURd2Osc1p4E1AJHWXPeuTFGNM
rimyzDlsd8XOBMYsd6tEEd50hcf4aiBOCfj5NS3Tf7UCggEAW2eTx2TJuSjLFlQo
I4kaV2Ui2ZD8nI63VWf+O/JJT2MRujwV91gqdc1unn+nz9brE5FXKKZCHIcnfM24
F2TKsh6IliNISJDRLe1hAYyIenDyFQWt9TL5SqbNJduBXCO/LAhnXc0XOTkVje85
3iStsPqshHLoiXPPneswn8IkbUbXBWK2J6dpV+OOmA1wDFbmWX91tCQ+SHmYHOtE
QwvezOkqxLcCEq7F6YexgT169+nf4G+4fk6floPRCn2c1kWRQMjw4GYmBODqZrSE
SxvyyVIm0mCWxBsDjBraQMBR7BRxeMn58aT+AGYk+q1kn1pBn7nsJ+8ojJdRCSTT
hu3ikwKCAQBXyfdqBxmxT6JXjgANQQ1ke/uxBZ9/oXfa94AK0seD3texlhh9vpEd
ZoAiZdu8qJ5lSbQuIECwRRruC4JdqrMEECh0IlV308GwIkR8OhD7bgg6Be9ZfjB6
CcXnHF2Fw2Hzy/CJ3r6lvMu6fTEL442W6n6hgbp3bqXME1mHPdzaWPxUH5Z0M34n
BE5E8SJ+eMoR/UGtqGVOea2Y0CBDUrh6JVZlCq4RXJyaMRrDM1Gu+fg4ov44Ps9S
TBv4CnOUp2mM45R+bsey6WYKM3RElnzjS5Pmkxh0IUufybHmJIRt+aSyTLs1zB9e
uU4ka7Jz3R1wKMLfF48hr63j9y2bEC5lAoIBAQCYl5L1mIdcSlDR2H/mv8SfFspR
gMU2p84S667bY7C+4hgyVMCbSQguXPyCBrokX9EmGrG6LK7guPHIcNmlSgOvJPHS
9gz1yBXc0NtWYfNAgIwBpOlglNYChnXmo/gpO/cQPJy+hCU/X/gw8N5ygXBUN4oU
TkB2oKVDIxVHenmLAEmu6dGjEcqV001nXV5zk9rNU2LBJ9zyJL4GHp3pYMhPExfT
+wez48POdQL9I5yaTESyNgUEVnGMLQGYGbQN0lh6a78SRzFu6rSbJuu+4XC2aqpp
h99o+05pw3ZYRljyqzXWcNPjffvkhaUREJG/3MgIZWW1JGXw9AAf6vjHh441
-----END RSA PRIVATE KEY-----">>
    ],
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
            ]},
            {keyring_rotator_timeout, 1000},
            {keyring_initialize_lifetime, 1000},
            {shareholders, [
                #{
                    id => <<"1">>,
                    owner => <<"ndiezel">>,
                    public_key => <<"-----BEGIN PUBLIC KEY-----
MIICITANBgkqhkiG9w0BAQEFAAOCAg4AMIICCQKCAgBLFPTHtz+ldYk1HJDI5Xng
F7F9tm6LBVTm5wiw7ezDSCnPGNKDNOsFF5/AiDGiBSRDB2XyGOOoMATl6ZEKLZlO
lS90gUtigTuLL9OkgujptZN5gMu1qfHuVr2cKIU2y1j1JnnbCm7aW7HO7Go11aB+
p8on03Nq/Iw5cw+SsN+dGG/XEjuhvuAVksPBEEhjdHIxYSpzOWUx++DAonXrzz8r
IVhTL0YDYx7CA4Zo+bDYNO0sLhSyS/mo+ryYpnFsroW1Kl9IZ+hjMTv5sTKztYan
VQLOsKNqCewuVJC/FgkGIeQQy8y4DOS9nQ7hCQ5x89gdxmDKP17LSYlgPc04lnPE
XdQmBBVJV8+Rj/L60gEsLb5sVgrsY87rNJfvXUbGLzf8RhkAHybHxAgKphX82rw5
F6iHWbugN+q2QfXbjg7iPGD6ew3mdZVkao4q75DTqZ1xTU+7uicF3spUs9meguQz
RdFFHHesYyXCE98s+0aBeb88ytxocUpcv+nS6KZZoAMq4KuEvR8n89p9znAZjYVC
LJQPkORMzfoCL2HvEQ4GTg6CL/cXwwMjjiThUcdso7eqRT0eG+GFMpILIAb1OQut
7T2F9bAjD4SzCVuY7orIX1BLXiGf4eTkFUp9MxM5k/ZGAdt1B4XhvWv35TlzNdt7
iM80ES1KtAGoR+uwc1xAoQIDAQAB
-----END PUBLIC KEY-----">>
                },
                #{
                    id => <<"2">>,
                    owner => <<"ndiezel2">>,
                    public_key => <<"-----BEGIN PUBLIC KEY-----
MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEA41Z5vCUkXNemyPHkxOPc
FrcY4mminZKHVs+9vns64RJwDRCV/1WmBMP1F1Dpe9/eRBIV8q45C/rxJ4JzZk8M
0B0N1bZ+2PVzCO/rJ+GDN7mYtDCH4h7O/7rFtbzSLpbSUR6XGhBcD4WvmfuVqCW6
vxwpAKWUWA5+WlWqMEAOZ+CQfOMpRddKYCrf6klvADoBuO/ayaiTY8plsDsKdcEy
G5CXv22SY326I+tovjZl7MHCXO+iZTvLkrgGYSvmJfDXK4kNxnVwHlqGVGJgMNek
hX4GRby1L6gO0RBhwPcnwBbTvLNXAPCbhAbO/p6EMz0kx1+egNQ5xeFkxn0I8LXX
bt64lKLlydW/+SAhgYQeSlZF5p3dSokF9SXj61UXpSGBqJT5yRpsBa0H580abmvF
Lc8Gre3l6dPdRpoB/4dQ9eAueQi5BbsWyd2yrjVKbi6FbZftxkum4qdSQowLadbh
24aY4b40wVQfGA8u8wxIYgvYD7zaxWsypiiXF9qUjR9hQoqWRSNE+k5lDOoFPOdf
f1gQHqiDxWI5Q0esGbftE/vTI1JGkjKIy1+W4EU7ve91e3AG5sLQDwYSwGuI6VwC
6ImnAzEmKjorncPn4C537lOlhX2rYi2iSwZykNHZRX3/nMpsuJqRV+iw206NqND2
XmXuWeWtxX1OToV+xBFxNMcCAwEAAQ==
-----END PUBLIC KEY-----">>
                },
                #{
                    id => <<"3">>,
                    owner => <<"ndiezel3">>,
                    public_key => <<"-----BEGIN PUBLIC KEY-----
MIICIjANBgkqhkiG9w0BAQEFAAOCAg8AMIICCgKCAgEArVb6bKFTNEkJ3Ok5IVxj
Bs+Emfo3Ya3DTV8duDlN7A/L7wzD/qH+K0+K9b7n0igvs5vZtXKm7+pRakQoJDhs
3QWLiRxZnrU6Vm32vy6Y0C5fTRiRo4VfdMYKY0Ah1K5lS8buLTGrjHy/sBA+n4Bc
lALY+WuaBYpoVaqNjBqmmJq+xyojtZVkXBMG0HoBwj+705nW9spYLr4o2opW+xxa
rOxfiWtTDH0Wfa4jsvwphGL0pR8/lZL8sHr45IsA1/h9EuvqLZzYSCrbqaHFCEEx
WC1lRIWF/Gadu92UkPSwrft1Yn/ckf90Bc4mU/eiuay7k0FYgmG1fj4isl+KZ2UH
lmCSXziY+smlEXQ/qLrokXD9B8ypygSB5Vk6rKZzDOObsFMFJFGAibvW1BgimuCe
9ROK/xgy1+tFEtZSJf1b+5YPHxKnTuvTJ3VsJR3P5vTVq5Pt5CNKVG6WEyrAUTkd
mOVdjcTrLVsd/PG9GOTHXW+PJdXNbsdIUlQet2Kc4fWc9Je+InWTpaZxNO5T4VPk
wANrZ2JD0xElXOUfV/dHggSSU6k6/St+UhGSCgqEMIXeIBVGQr9KOMCDbvN5SK2b
Ky1ZDi7DbzxUFgx+X8FybVQiH/GCMREnZKM+3pi+ir3Ay8T5uwzkp3Q9uGl+lUiO
3VU+SO5HukksCrWOQ/ZhQzcCAwEAAQ==
-----END PUBLIC KEY-----">>
                }
            ]}
        ] ++ StorageConfig ++ CleanConfig ++ Recrypting)
    ,
    [
        {stash, Stash},
        {apps, lists:reverse(Apps)},
        {root_url, genlib:to_binary(RootUrl)},
        {private_keys, PrivateKeys}
    ].

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

start_stash() ->
    cds_ct_stash:start().

stop_stash(C) ->
    cds_ct_stash:stop(config(stash, C)).
