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
    ThriftService = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [{"/v1/cds", {{cds_thrift, cds}, cds_thrift_service_handler, []}}],
            event_handler => cds_thrift_event_handler,
            ip => {127, 0, 0, 1},
            port => 8022,
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
        cds_shamir:share(MasterKey, Threshold, Count)
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

-spec hash(binary()) -> cds_hash:hash().
hash(Plain) ->
    {_KeyId, Key} = cds_keyring_manager:get_current_key(),
    cds_hash:hash(Plain, Key).

-spec all_hashes(binary()) -> [cds_hash:hash()].
all_hashes(Plain) ->
    [cds_hash:hash(Plain, Key) || {_KeyId, Key} <- cds_keyring_manager:get_all_keys()].

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



%%
%% Tests
%%
-ifdef(TEST).

prop_encryption() ->
    ?FORALL({KeyId, Key, Data}, {byte(), binary(32), binary()}, ok =:= encryption_cycle(KeyId, Key, Data)).

encryption_cycle(KeyId, Key, Data) ->
    meck:new(cds_keyring_manager),
    meck:expect(cds_keyring_manager, get_current_key, fun() -> {KeyId, Key} end),
    meck:expect(cds_keyring_manager, get_key, fun(Id) when Id =:= KeyId -> {KeyId, Key} end),
    Encrypted = encrypt(Data),
    Data = decrypt(Encrypted),
    meck:unload(cds_keyring_manager),
    ok.

encryption_test() ->
    true = proper:quickcheck(prop_encryption()).


prop_hashing() ->
    ?FORALL({KeyId, Key, Data}, {byte(), binary(32), binary()}, ok =:= hashing_cycle(KeyId, Key, Data)).

hashing_cycle(KeyId, Key, Data) ->
    meck:new(cds_keyring_manager),
    meck:expect(cds_keyring_manager, get_current_key, fun() -> {KeyId, Key} end),
    meck:new(cds_hash),
    meck:expect(cds_hash, hash, fun(Plain, Salt) when Salt =:= Key, Plain =:= Data -> <<"hash_stub">> end),
    <<"hash_stub">> = hash(Data),
    meck:unload(cds_keyring_manager),
    meck:unload(cds_hash),
    ok.

hashing_test() ->
    true = proper:quickcheck(prop_hashing()).

-endif.
