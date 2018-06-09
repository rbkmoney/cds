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
-export([put_card_data/1]).
-export([get_session_data/1]).

-export([update_cardholder_data/2]).
-export([update_session_data/2]).

%%
-export_type([hash/0]).
-export_type([token/0]).
-export_type([session/0]).
-export_type([plaintext/0]).
-export_type([ciphertext/0]).

-type hash() :: binary().
-type token() :: <<_:128>>.
-type session() :: <<_:128>>.
-type metadata() :: #{binary() := binary()}.
-type ciphermeta() :: binary().
-type plaintext() :: binary() | {binary(), metadata()}.
-type ciphertext() :: binary() | {binary(), ciphermeta()}. % <<KeyID/byte, EncryptedData/binary>>

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

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, IP} = inet:parse_address(application:get_env(cds, ip, "::")),
    HealthCheckers = genlib_app:env(?MODULE, health_checkers, []),
    Service = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                cds_thrift_services:handler_spec(card),
                cds_thrift_services:handler_spec(keyring),
                cds_thrift_services:handler_spec(identity_doc)
            ],
            event_handler     => cds_woody_event_handler,
            ip                => IP,
            port              => genlib_app:env(?MODULE, port, 8022),
            net_opts          => genlib_app:env(?MODULE, net_opts, []),
            additional_routes => [erl_health_handle:get_route(HealthCheckers)]
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
            NSlist = lists:flatten([
                cds_card_storage:get_namespaces(),
                cds_id_storage:get_namespaces()
            ]),
            cds_storage:start(NSlist),
            {ok, Sup}
    end.


-spec stop(any()) ->
    ok.
stop(_State) ->
    ok.


%%
%% Storage operations
%%

-spec get_cardholder_data(token()) -> plaintext().
get_cardholder_data(Token) ->
    Keyring = cds_keyring_manager:get_keyring(),
    Encrypted = cds_card_storage:get_cardholder_data(Token),
    decrypt(Encrypted, Keyring).

-spec get_card_data(token(), session()) -> {plaintext(), plaintext()}.
get_card_data(Token, Session) ->
    Keyring = cds_keyring_manager:get_keyring(),
    {EncryptedCardData, EncryptedSessionData} = cds_card_storage:get_session_card_data(Token, Session),
    {decrypt(EncryptedCardData, Keyring), decrypt(EncryptedSessionData, Keyring)}.

-spec put_card_data({plaintext(), plaintext()}) -> {token(), session()}.
put_card_data({MarshalledCardData, MarshalledSessionData}) ->
    UniqueCardData = cds_card_data:unique(MarshalledCardData),
    {Token, Hash} = find_or_create_token(UniqueCardData),
    Session = session(),
    {KeyID, _} = CurrentKey = cds_keyring_manager:get_current_key(),
    EncryptedCardData = encrypt(MarshalledCardData, CurrentKey),
    EncryptedSessionData = encrypt(MarshalledSessionData, CurrentKey),
    ok = cds_card_storage:put_card_data(
        Token,
        Session,
        Hash,
        EncryptedCardData,
        EncryptedSessionData,
        KeyID,
        cds_utils:current_time()
    ),
    {Token, Session}.

-spec get_session_data(session()) -> plaintext().
get_session_data(Session) ->
    Keyring = cds_keyring_manager:get_keyring(),
    Encrypted = cds_card_storage:get_session_data(Session),
    decrypt(Encrypted, Keyring).

-spec update_cardholder_data(token(), plaintext()) -> ok.
update_cardholder_data(Token, CardData) ->
    {KeyID, Key} = cds_keyring_manager:get_current_key(),
    Hash = hash(cds_card_data:unique(CardData), Key),
    EncryptedCardData = encrypt(CardData, {KeyID, Key}),
    cds_card_storage:update_cardholder_data(Token, EncryptedCardData, Hash, KeyID).

-spec update_session_data(session(), plaintext()) -> ok.
update_session_data(Session, SessionData) ->
    {KeyID, _} = CurrentKey = cds_keyring_manager:get_current_key(),
    EncryptedSessionData = encrypt(SessionData, CurrentKey),
    cds_card_storage:update_session_data(Session, EncryptedSessionData, KeyID).

%%
%% Internals
%%

-spec encrypt(plaintext(), {cds_keyring:key_id(), cds_keyring:key()}) -> ciphertext().

encrypt({Data, Metadata}, Keyring) ->
    {encrypt(Data, Keyring), encrypt(msgpack:pack(Metadata), Keyring)};
encrypt(Plain, {KeyID, Key}) ->
    Cipher = cds_crypto:encrypt(Key, Plain),
    <<KeyID, Cipher/binary>>.

-spec decrypt(ciphertext(), cds_keyring:keyring()) -> plaintext().
decrypt({Data, Metadata}, Keyring) ->
    {ok, DecryptedMetadata} = msgpack:unpack(decrypt(Metadata, Keyring)),
    {decrypt(Data, Keyring), DecryptedMetadata};
decrypt(<<KeyID, Cipher/binary>>, Keyring) ->
    {ok, {KeyID, Key}} = cds_keyring:get_key(KeyID, Keyring),
    cds_crypto:decrypt(Key, Cipher).

-spec hash(binary(), binary()) -> hash().
hash(Plain, Salt) ->
    {N, R, P} = application:get_env(cds, scrypt_opts, {16384, 8, 1}),
    scrypt:scrypt(Plain, Salt, N, R, P, 16).

-spec find_or_create_token(binary()) -> {token(), hash()}.
find_or_create_token(UniqueCardData) ->
    Keyring = cds_keyring_manager:get_keyring(),
    {CurrentKeyID, CurrentKey} = cds_keyring:get_current_key(Keyring),
    Keys = [Key || {KeyID, Key} <- cds_keyring:get_keys(Keyring), KeyID =/= CurrentKeyID],
    CurrentHash = hash(UniqueCardData, CurrentKey),
    FindResult = try
        % let's check current key first
        {cds_card_storage:get_token(CurrentHash), CurrentHash}
    catch
        not_found ->
            % if not found, check other keys
            find_token(UniqueCardData, Keys)
    end,
    case FindResult of
        {_Token, _Hash} ->
            FindResult;
        not_found ->
            {token(), CurrentHash}
    end.

find_token(_, []) ->
    not_found;
find_token(UniqueCardData, [Key | Others]) ->
    Hash = hash(UniqueCardData, Key),
    try
        {cds_card_storage:get_token(Hash), Hash}
    catch
        not_found ->
            find_token(UniqueCardData, Others)
    end.

-spec token() -> token().
token() ->
    crypto:strong_rand_bytes(16).

-spec session() -> session().
session() ->
    crypto:strong_rand_bytes(16).
