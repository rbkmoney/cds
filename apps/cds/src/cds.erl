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
-export([get_card_data/1]).
-export([get_session_card_data/2]).
-export([put_card_data/1]).
-export([delete_card_data/2]).
-export([delete_cvv/1]).

%% Keyring operations
-export([unlock_keyring/1]).
-export([init_keyring/2]).
-export([update_keyring/0]).
-export([rotate_keyring/0]).
-export([lock_keyring/0]).

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
    {ok, ThriftHost} = inet:parse_address(application:get_env(cds, thrift_listen_host, "0.0.0.0")),
    ThriftPort = application:get_env(cds, thrift_listen_port, 8022),
    ThriftService = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                {"/v1/storage", {{cds_cds_thrift, 'Storage'}, cds_thrift_handler, []}},
                {"/v1/keyring", {{cds_cds_thrift, 'Keyring'}, cds_thrift_handler, []}}
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
-spec get_card_data(cds_crypto:token()) -> cds_cds_thrift:'CardData'().
get_card_data(Token) ->
    Encrypted = cds_storage:get_card_data(Token),
    Marshalled = decrypt(Encrypted),
    cds_card_data:unmarshall(Marshalled).

-spec get_session_card_data(cds_crypto:token(), cds_crypto:token()) -> cds_cds_thrift:'CardData'().
get_session_card_data(Token, Session) ->
    {EncryptedCardData, EncryptedCvv} = cds_storage:get_session_card_data(Token, Session),
    MarshalledCardData = decrypt(EncryptedCardData),
    Cvv = decrypt(EncryptedCvv),
    cds_card_data:unmarshall(MarshalledCardData, Cvv).

-spec put_card_data(cds_cds_thrift:'CardData'()) -> {cds_crypto:token(), cds_crypto:token()}.
put_card_data(CardData) ->
    {MarshalledCardData, Cvv} = cds_card_data:marshall(CardData),
    UniqueCardData = cds_card_data:unique(CardData),
    Token = tokenize(UniqueCardData),
    Session = cds_crypto:token(),
    Hash = hash(UniqueCardData),
    EncryptedCardData = encrypt(MarshalledCardData),
    EncryptedCvv = encrypt(Cvv),
    ok = cds_storage:put_card_data(Token, Session, Hash, EncryptedCardData, EncryptedCvv),
    {Token, Session}.

-spec delete_card_data(cds_crypto:token(), cds_crypto:token()) -> ok.
delete_card_data(Token, Session) ->
    CardData = get_card_data(Token),
    UniqueCardData = cds_card_data:unique(CardData),
    Hash = hash(UniqueCardData),
    ok = cds_storage:delete_card_data(Token, Hash, Session),
    ok.
-spec delete_cvv(cds_crypto:token()) -> ok.
delete_cvv(Session) ->
    ok = cds_storage:delete_cvv(Session),
    ok.



%%
%% Keyring operations
%%
-spec unlock_keyring(binary()) -> {more, byte()} | ok.
unlock_keyring(Share) ->
    cds_keyring_manager:unlock(Share).

-spec init_keyring(integer(), integer()) -> [binary()].
init_keyring(Threshold, Count) when Threshold =< Count ->
    cds_keyring_manager:initialize(Threshold, Count).

-spec update_keyring() -> ok.
update_keyring() ->
    cds_keyring_manager:update().

-spec rotate_keyring() -> ok.
rotate_keyring() ->
    cds_keyring_manager:rotate().

-spec lock_keyring() -> ok.
lock_keyring() ->
    cds_keyring_manager:lock().

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
    try cds_storage:get_token(Hash) of
        Token ->
            Token
    catch
        not_found ->
            find_or_create_token(Rest)
    end.
