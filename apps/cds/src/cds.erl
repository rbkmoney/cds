-module(cds).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop /0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop /1]).

%% Storage operations
-export([get/1]).
-export([put/1]).
-export([delete/1]).

%% Keyring operations
-export([unlock_keyring/1]).
-export([init_keyring/2]).
-export([update_keyring/0]).
-export([rotate_keyring/0]).
-export([lock_keyring/0]).
-export([destroy_keyring/0]).

-compile({no_auto_import, [get/1]}).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% API
%%
-spec start() ->
    {ok, _}.
start() ->
    application:ensure_all_started(cds).

-spec stop() ->
    ok.
stop() ->
    application:stop(cds).


%%
%% Supervisor callbacks
%%
init([]) ->
    {ok, ThriftHost} = inet:parse_address(application:get_env(cds, thrift_host, "127.0.0.1")),
    ThriftPort = application:get_env(cds, thrift_port, 8022),
    ThriftService = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                {"/v1/storage", {{cds_thrift, 'Storage'}, cds_thrift_handler, []}},
                {"/v1/keyring", {{cds_thrift, 'Keyring'}, cds_thrift_handler, []}}
            ],
            event_handler => cds_thrift_handler,
            ip => ThriftHost,
            port => ThriftPort,
            net_opts => []
        }
    ),
    KeyringManager = #{
        id => cds_keyring_manager,
        start => {cds_keyring_manager, start_link, []}
    },
    Procs = [
        ThriftService,
        KeyringManager
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.


%%
%% Application callbacks
%%
-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(normal, _StartArgs) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Sup} ->
            cds_storage:start(),
            {ok, Sup}
    end.


-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.


%%
%% Storage operations
%%
-spec get(cds_crypto:token()) -> binary().
get(Token) ->
    decrypt(cds_storage:get(token, Token)).

-spec put(binary()) -> cds_crypto:token().
put(Data) ->
    Token = tokenize(Data),
    ok = cds_storage:put(token, Token, encrypt(Data)),
    ok = cds_storage:put(hash, hash(Data), Token),
    Token.

-spec delete(cds_crypto:token()) -> ok.
delete(Token) ->
    Data = get(Token),
    ok = cds_storage:delete(token, Token),
    ok = cds_storage:delete(hash, hash(Data)),
    ok.


%%
%% Keyring operations
%%
-spec unlock_keyring(binary()) -> {more, byte()} | unlocked.
unlock_keyring(Share) ->
    cds_keyring_manager:unlock(Share).

-spec init_keyring(integer(), integer()) -> [binary()].
init_keyring(Threshold, Count) when Threshold =< Count ->
    try
        ok = cds_keyring_storage:lock(),
        ok = try cds_keyring_storage:get() of
            _Keyring ->
                throw(already_exists)
        catch
            not_found ->
                ok
        end,
        MasterKey = cds_crypto:key(),
        Keyring = cds_keyring:new(),
        MarshalledKeyring = cds_keyring:marshall(Keyring),
        EncryptedKeyring = cds_crypto:encrypt(MasterKey, MarshalledKeyring),
        ok = cds_keyring_storage:put(EncryptedKeyring),
        Shares = cds_keysharing:share(MasterKey, Threshold, Count),
        cds_keyring_manager:update_keyring(EncryptedKeyring),
        Shares
    after
        cds_keyring_storage:unlock()
    end.

-spec update_keyring() -> ok.
update_keyring() ->
    Keyring = cds_keyring_storage:get(),
    cds_keyring_manager:update_keyring(Keyring).

%% TODO? backup and automatic rollback on fail
-spec rotate_keyring() -> ok.
rotate_keyring() ->
    try
        ok = cds_keyring_storage:lock(),
        CurrentKeyring = cds_keyring_storage:get(),
        ok = cds_keyring_manager:update_keyring(CurrentKeyring),
        NewKeyring = cds_keyring_manager:rotate_keyring(),
        ok = cds_keyring_storage:put(NewKeyring),
        NewKeyring = cds_keyring_storage:get(), %% double-check
        ok = cds_keyring_manager:update_keyring(NewKeyring)
    after
        cds_keyring_storage:unlock()
    end.

lock_keyring() ->
    cds_keyring_manager:lock().

-spec destroy_keyring() -> ok.
destroy_keyring() ->
    try
        ok = cds_keyring_storage:lock(),
        ok = try cds_keyring_storage:delete() of
            ok ->
                ok
        catch
            not_found ->
                ok
        end,
        ok = try cds_keyring_manager:lock() of
            ok ->
                ok
        catch
            locked ->
                ok
        end
    after
        cds_keyring_storage:unlock()
    end.

%%
%% Internal
%%
-spec encrypt(binary()) -> binary().
encrypt(Plain) ->
    {KeyId, Key} = cds_keyring_manager:get_current_key(),
    Cipher = cds_crypto:encrypt(Key, Plain),
    <<KeyId, Cipher/binary>>.

-spec decrypt(binary()) -> binary().
decrypt(<<KeyId, Cipher/binary>>) ->
    {KeyId, Key} = cds_keyring_manager:get_key(KeyId),
    cds_crypto:decrypt(Key, Cipher).

-spec hash(binary()) -> binary().
hash(Plain) ->
    {_KeyId, Key} = cds_keyring_manager:get_current_key(),
    hash(Plain, Key).

-spec hash(binary(), binary()) -> binary().
hash(Plain, Salt) ->
    {N, R, P} = application:get_env(cds, scrypt_opts, {16384, 8, 1}),
    scrypt:scrypt(Plain, Salt, N, R, P, 16).

-spec all_hashes(binary()) -> [binary()].
all_hashes(Plain) ->
    [hash(Plain, Key) || {_KeyId, Key} <- cds_keyring_manager:get_all_keys()].

tokenize(Data) ->
    find_or_create_token(all_hashes(Data)).

find_or_create_token([]) ->
    cds_crypto:token();
find_or_create_token([Hash | Rest]) ->
    try cds_storage:get(hash, Hash) of
        Token ->
            Token
    catch
        not_found ->
            find_or_create_token(Rest)
    end.
