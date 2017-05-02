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
-export([get_cardholder_data/1]).
-export([get_card_data/2]).
-export([put_card_data/2]).
-export([delete_session/1]).

-export([get_sessions_created_between/3]).
-export([get_sessions_by_key_id_between/3]).
-export([get_tokens_by_key_id_between/3]).
-export([get_cvv/1]).
-export([update_cvv/2]).
-export([update_cardholder_data/2]).
-export([refresh_session_created_at/1]).

%% Keyring operations
-export([unlock_keyring/1]).
-export([init_keyring/2]).
-export([update_keyring/0]).
-export([rotate_keyring/0]).
-export([lock_keyring/0]).
-export([get_outdated_encrypting_keys/0]).

-compile({no_auto_import, [get/1]}).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
-export_type([token/0]).
-export_type([session/0]).
-export_type([masterkey_share/0]).

-type token() :: <<_:128>>.
-type session() :: <<_:128>>.
-type masterkey_share() :: binary().
-type card_data() :: {cardholder_data(), cvv()}.
-type unique_card_info() :: binary().
-type cardholder_data() :: binary().
-type cvv() :: binary().

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
    {ok, IP} = inet:parse_address(application:get_env(cds, ip, "::")),
    Service = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                {"/v1/storage", {{cds_cds_thrift, 'Storage'}, {cds_thrift_handler, []}}},
                {"/v1/keyring", {{cds_cds_thrift, 'Keyring'}, {cds_thrift_handler, []}}}
            ],
            event_handler => cds_thrift_handler,
            ip => IP,
            port => genlib_app:env(?MODULE, port, 8022),
            net_opts => genlib_app:env(?MODULE, net_opts, #{})
        }
    ),
    KeyringManager = #{
        id => cds_keyring_manager,
        start => {cds_keyring_manager, start_link, []}
    },
    Maintenance = #{
        id => cds_maintenance_sup,
        start => {cds_maintenance_sup, start_link, []},
        type => supervisor
    },
    Procs = [
        Service,
        KeyringManager,
        Maintenance
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

-spec get_cardholder_data(token()) -> cardholder_data().
get_cardholder_data(Token) ->
    ok = keyring_available(),
    Encrypted = cds_storage:get_cardholder_data(Token),
    decrypt(Encrypted).

-spec get_card_data(token(), session()) -> card_data().
get_card_data(Token, Session) ->
    ok = keyring_available(),
    {EncryptedCardData, EncryptedCvv} = cds_storage:get_session_card_data(Token, Session),
    {decrypt(EncryptedCardData), decrypt(EncryptedCvv)}.

-spec put_card_data(unique_card_info(), card_data()) -> {token(), session()}.
put_card_data(UniqueCardData, {MarshalledCardData, Cvv}) ->
    ok = keyring_available(),
    Token = find_or_create_token(all_hashes(UniqueCardData)),
    Session = session(),
    Hash = hash(UniqueCardData),
    {KeyID, _} = Current = cds_keyring_manager:get_current_key(),
    EncryptedCardData = encrypt(Current, MarshalledCardData),
    EncryptedCvv = encrypt(Current, Cvv),
    ok = cds_storage:put_card_data(
        Token,
        Session,
        Hash,
        EncryptedCardData,
        EncryptedCvv,
        KeyID,
        cds_utils:current_time()
    ),
    {Token, Session}.

-spec delete_session(session()) -> ok.
delete_session(Session) ->
    ok = keyring_available(),
    ok = cds_storage:delete_session(Session),
    ok.

get_sessions_created_between(From, To, BatchSize) ->
    cds_storage:get_sessions_created_between(From, To, BatchSize).

get_sessions_by_key_id_between(From, To, BatchSize) ->
    cds_storage:get_sessions_by_key_id_between(From, To, BatchSize).

get_tokens_by_key_id_between(From, To, BatchSize) ->
    cds_storage:get_tokens_by_key_id_between(From, To, BatchSize).

get_cvv(Session) ->
    ok = keyring_available(),
    EncryptedCvv = cds_storage:get_cvv(Session),
    decrypt(EncryptedCvv).

update_cvv(Session, Cvv) ->
    ok = keyring_available(),
    {KeyID, _} = CurrentKeys = cds_keyring_manager:get_current_key(),
    EncryptedCvv = encrypt(CurrentKeys, Cvv),
    cds_storage:update_cvv(Session, EncryptedCvv, KeyID).

update_cardholder_data(Token, CardData) ->
    ok = keyring_available(),
    {KeyID, _} = CurrentKeys = cds_keyring_manager:get_current_key(),
    EncryptedCardData = encrypt(CurrentKeys, CardData),
    cds_storage:update_cardholder_data(Token, EncryptedCardData, KeyID).

refresh_session_created_at(Session) ->
    cds_storage:refresh_session_created_at(Session).

%%
%% Keyring operations
%%
-spec unlock_keyring(masterkey_share()) -> {more, byte()} | ok.
unlock_keyring(Share) ->
    cds_keyring_manager:unlock(Share).

-spec init_keyring(integer(), integer()) -> [masterkey_share()].
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

-spec get_outdated_encrypting_keys() -> [{From :: byte(), To :: byte()}].
get_outdated_encrypting_keys() ->
    ok = keyring_available(),
    {KeyID, _} = cds_keyring_manager:get_current_key(),
    #{min := MinID, max := MaxID} = cds_keyring:get_key_id_config(),
    [ I || {From, To} = I <- [{MinID, KeyID}, {KeyID, MaxID}], From =/= To].

-spec encrypt(_, binary()) -> binary(). %%
encrypt({KeyID, Key}, Plain) ->
    Cipher = cds_crypto:encrypt(Key, Plain),
    <<KeyID, Cipher/binary>>.

-spec decrypt(binary()) -> binary().
decrypt(<<KeyID, Cipher/binary>>) ->
    {KeyID, Key} = cds_keyring_manager:get_key(KeyID),
    cds_crypto:decrypt(Key, Cipher).

-spec hash(binary()) -> binary().
hash(Plain) ->
    {_KeyID, Key} = cds_keyring_manager:get_current_key(),
    hash(Plain, Key).

-spec hash(binary(), binary()) -> binary().
hash(Plain, Salt) ->
    {N, R, P} = application:get_env(cds, scrypt_opts, {16384, 8, 1}),
    scrypt:scrypt(Plain, Salt, N, R, P, 16).

-spec all_hashes(binary()) -> [binary()].
all_hashes(Plain) ->
    [hash(Plain, Key) || {_KeyID, Key} <- cds_keyring_manager:get_all_keys()].

find_or_create_token([]) ->
    token();
find_or_create_token([Hash | Rest]) ->
    try cds_storage:get_token(Hash) of
        Token ->
            Token
    catch
        not_found ->
            find_or_create_token(Rest)
    end.

-spec token() -> token().
token() ->
    crypto:strong_rand_bytes(16).

-spec session() -> session().
session() ->
    crypto:strong_rand_bytes(16).

-spec keyring_available() -> ok | no_return().
keyring_available() ->
    case cds_keyring_manager:get_state() of
        locked ->
            throw(locked);
        unlocked ->
            ok
    end.
