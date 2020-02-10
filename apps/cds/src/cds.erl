-module(cds).
-behaviour(supervisor).
-behaviour(application).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop /1]).

%% Storage operations
-export([get_cardholder_data/1]).
-export([put_card/1]).
-export([put_session/2]).
-export([get_session_data/1]).

-export([update_cardholder_data/2]).
-export([update_session_data/2]).

-export([find_or_create_token/4]).

%%
-export_type([hash/0]).
-export_type([token/0]).
-export_type([session/0]).
-export_type([plaintext/0]).
-export_type([ciphertext/0]).

-type hash() :: binary().
-type token() :: <<_:128>>.
-type session() :: binary().
-type metadata() :: #{binary() := binary()}.
-type ciphermeta() :: binary().
-type plaintext() :: binary() | {binary(), metadata()}.
-type ciphertext() :: binary() | {binary(), ciphermeta()}. % <<KeyID/byte, EncryptedData/binary>>

%%
%% Supervisor callbacks
%%

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, IP} = inet:parse_address(application:get_env(cds, ip, "::")),
    HealthCheck = genlib_app:env(?MODULE, health_check, #{}),
    HealthRoute = erl_health_handle:get_route(enable_health_logging(HealthCheck)),
    Service = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                cds_thrift_services:handler_spec(token),
                cds_thrift_services:handler_spec(card),
                cds_thrift_services:handler_spec(card_v2),
                cds_thrift_services:handler_spec(ident_doc)
            ],
            event_handler     => cds_woody_event_handler,
            ip                => IP,
            port              => genlib_app:env(?MODULE, port, 8022),
            transport_opts    => genlib_app:env(?MODULE, transport_opts, #{}),
            protocol_opts     => genlib_app:env(?MODULE, protocol_opts, #{}),
            shutdown_timeout  => genlib_app:env(?MODULE, shutdown_timeout, 0),
            additional_routes => [HealthRoute]
        }
    ),
    KeyringSupervisor = #{
        id => cds_keyring_sup,
        start => {cds_keyring_sup, start_link, []}
    },
    Maintenance = #{
        id => cds_maintenance_sup,
        start => {cds_maintenance_sup, start_link, []},
        type => supervisor
    },
    HashSup = #{
        id => cds_hash_sup,
        start => {cds_hash, start_link, []},
        type => supervisor
    },
    Procs = [
        KeyringSupervisor,
        HashSup,
        Maintenance,
        Service
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.

-spec enable_health_logging(erl_health:check()) -> erl_health:check().
enable_health_logging(Check) ->
    EvHandler = {erl_health_event_handler, []},
    maps:map(fun (_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Check).


%%
%% Application callbacks
%%
-spec start(normal, any()) ->
    {ok, pid()} | {error, any()}.
start(normal, _StartArgs) ->
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Sup} ->
            NSlist = lists:flatten([
                cds_token_storage:get_namespaces(),
                cds_card_storage:get_namespaces(),
                cds_ident_doc_storage:get_namespaces()
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

-spec get_cardholder_data(token()) -> {cds_keyring:key_id(), plaintext()}.
get_cardholder_data(Token) ->
    Encrypted = cds_card_storage:get_cardholder_data(Token),
    decrypt(Encrypted).

-spec put_card(plaintext()) -> token().
put_card(MarshalledCardData) ->
    {{KeyID, Key} = CurrentKey, Meta} = cds_keyring:get_current_key_with_meta(),
    {Token, Hash} = find_or_create_token(KeyID, Key, Meta, MarshalledCardData),
    EncryptedCardData = encrypt(MarshalledCardData, CurrentKey),
    ok = cds_card_storage:put_card(
        Token,
        Hash,
        EncryptedCardData,
        KeyID
    ),
    Token.

-spec put_session(session(), plaintext()) -> ok.
put_session(Session, MarshalledSessionData) ->
    {KeyID, _} = CurrentKey = cds_keyring:get_current_key(),
    EncryptedSessionData = encrypt(MarshalledSessionData, CurrentKey),
    ok = cds_card_storage:put_session(
        Session,
        EncryptedSessionData,
        KeyID,
        cds_utils:current_time()
    ).

-spec get_session_data(session()) -> {cds_keyring:key_id(), plaintext()}.
get_session_data(Session) ->
    Encrypted = cds_card_storage:get_session_data(Session),
    decrypt(Encrypted).

-spec update_cardholder_data(token(), plaintext()) -> ok.
update_cardholder_data(Token, CardData) ->
    {{KeyID, Key}, Meta} = cds_keyring:get_current_key_with_meta(),
    Hash = cds_hash:hash(CardData, Key, scrypt_options(Meta)),
    EncryptedCardData = encrypt(CardData, {KeyID, Key}),
    cds_card_storage:update_cardholder_data(Token, EncryptedCardData, Hash, KeyID).

-spec update_session_data(session(), plaintext()) -> ok.
update_session_data(Session, SessionData) ->
    {KeyID, _} = CurrentKey = cds_keyring:get_current_key(),
    EncryptedSessionData = encrypt(SessionData, CurrentKey),
    cds_card_storage:update_session_data(Session, EncryptedSessionData, KeyID).

%%
%% Internals
%%

-spec encrypt(plaintext(), {cds_keyring:key_id(), cds_keyring:key()}) -> ciphertext().
encrypt({Data, Metadata}, Key) ->
    {encrypt(Data, Key), encrypt(msgpack:pack(Metadata), Key)};
encrypt(Plain, {KeyID, Key}) ->
    Cipher = cds_crypto:encrypt(Key, Plain),
    <<"version: 1", KeyID:4/integer-unit:8, Cipher/binary>>.

-spec decrypt(ciphertext()) -> {cds_keyring:key_id(), plaintext()}.
decrypt({Data, Metadata}) ->
    {_, DecryptedMetadata} = decrypt(Metadata),
    {ok, UnpackedMetadata} = msgpack:unpack(DecryptedMetadata),
    {KeyID, DecryptedData} = decrypt(Data),
    {KeyID, {DecryptedData, UnpackedMetadata}};
decrypt(<<"version: 1", KeyID:4/integer-unit:8, Cipher/binary>>) ->
    {ok, {KeyID, Key}} = cds_keyring:get_key(KeyID),
    {KeyID, cds_crypto:decrypt(Key, Cipher)};
decrypt(<<KeyID, Cipher/binary>>) ->
    {ok, {KeyID, Key}} = cds_keyring:get_key(KeyID),
    {KeyID, cds_crypto:decrypt(Key, Cipher)}.

-spec find_or_create_token(cds_keyring:key_id(), cds_keyring:key(), cds_keyring:meta(), binary()) ->
    {token(), hash()}.
find_or_create_token(CurrentKeyID, CurrentKey, CurrentKeyMeta, MarshalledCardData) ->
    OtherKeys = cds_keyring:get_keys_except(CurrentKeyID),
    CurrentHash = cds_hash:hash(MarshalledCardData, CurrentKey, scrypt_options(CurrentKeyMeta)),
    % let's check current key first
    FindResult = find_tokens(MarshalledCardData, CurrentHash, OtherKeys),
    case FindResult of
        {[Token], Hash} ->
            {Token, Hash};
        {ManyTokens, Hash} ->
            case is_card_data_equal(ManyTokens) of
                true ->
                    {hd(ManyTokens), Hash};
                false ->
                    error({hash_collision_detected, Hash})
            end;
        not_found ->
            {token(), CurrentHash}
    end.

find_tokens(_, []) ->
    not_found;
find_tokens(CardData, [{Key, Meta} | OtherKeys]) ->
    Hash = cds_hash:hash(CardData, Key, scrypt_options(Meta)),
    find_tokens(CardData, Hash, OtherKeys).

find_tokens(CardData, Hash, OtherKeys) ->
    case cds_card_storage:get_tokens_by_hash(Hash) of
        [] ->
            find_tokens(CardData, OtherKeys);
        NotEmptyList ->
            {NotEmptyList, Hash}
    end.


-spec token() -> token().
token() ->
    crypto:strong_rand_bytes(16).

is_card_data_equal([Token | OtherTokens]) ->
    {_, FirstData} = get_cardholder_data(Token),
    lists:all(
        fun(T) ->
              {_, OtherData} = get_cardholder_data(T),
              FirstData =:= OtherData
        end,
        OtherTokens
    ).

scrypt_options(Meta) ->
    cds_keyring:deduplication_hash_opts(Meta).
