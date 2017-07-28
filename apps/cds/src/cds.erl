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

-export([get_cvv/1]).
-export([update_cvv/2]).
-export([update_cardholder_data/2]).

%%
-export_type([hash/0]).
-export_type([token/0]).
-export_type([session/0]).
-export_type([encrypted_data/0]).

-type hash() :: binary().
-type token() :: <<_:128>>.
-type session() :: <<_:128>>.
-type encrypted_data() :: binary(). % <<KeyID/byte, EncryptedData/binary>>

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
    Service = woody_server:child_spec(
        cds_thrift_service_sup,
        #{
            handlers => [
                {"/v1/storage", {{cds_cds_thrift, 'Storage'}, {cds_thrift_handler, []}}},
                {"/v1/keyring", {{cds_cds_thrift, 'Keyring'}, {cds_thrift_handler, []}}}
            ],
            event_handler => cds_woody_event_handler,
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

-spec get_cardholder_data(token()) -> cds_card_data:cardholder_data().
get_cardholder_data(Token) ->
    Encrypted = cds_storage:get_cardholder_data(Token),
    decrypt(Encrypted).

-spec get_card_data(token(), session()) -> cds_card_data:card_data().
get_card_data(Token, Session) ->
    {EncryptedCardData, EncryptedCvv} = cds_storage:get_session_card_data(Token, Session),
    {decrypt(EncryptedCardData), decrypt(EncryptedCvv)}.

-spec put_card_data(cds_card_data:card_data()) -> {token(), session()}.
put_card_data({MarshalledCardData, Cvv}) ->
    UniqueCardData = cds_card_data:unique(MarshalledCardData),
    {Token, Hash} = find_or_create_token(UniqueCardData),
    Session = session(),
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

-spec get_cvv(session()) -> cds_card_data:cvv().
get_cvv(Session) ->
    EncryptedCvv = cds_storage:get_cvv(Session),
    decrypt(EncryptedCvv).

-spec update_cvv(session(), cds_card_data:cvv()) -> ok.
update_cvv(Session, Cvv) ->
    {KeyID, _} = CurrentKey = cds_keyring_manager:get_current_key(),
    EncryptedCvv = encrypt(CurrentKey, Cvv),
    cds_storage:update_cvv(Session, EncryptedCvv, KeyID).

-spec update_cardholder_data(token(), cds_card_data:cardholder_data()) -> ok.
update_cardholder_data(Token, CardData) ->
    {KeyID, Key} = CurrentKey = cds_keyring_manager:get_current_key(),
    Hash = hash(cds_card_data:unique(CardData), Key),
    EncryptedCardData = encrypt(CurrentKey, CardData),
    cds_storage:update_cardholder_data(Token, EncryptedCardData, Hash, KeyID).

%%
%% Internals
%%

-spec encrypt({cds_keyring:key_id(), cds_keyring:key()}, binary()) -> encrypted_data().
encrypt({KeyID, Key}, Plain) ->
    Cipher = cds_crypto:encrypt(Key, Plain),
    <<KeyID, Cipher/binary>>.

-spec decrypt(encrypted_data()) -> binary().
decrypt(<<KeyID, Cipher/binary>>) ->
    {KeyID, Key} = cds_keyring_manager:get_key(KeyID),
    cds_crypto:decrypt(Key, Cipher).

-spec hash(binary(), binary()) -> hash().
hash(Plain, Salt) ->
    {N, R, P} = application:get_env(cds, scrypt_opts, {16384, 8, 1}),
    scrypt:scrypt(Plain, Salt, N, R, P, 16).

-spec find_or_create_token(cds_card_data:unique_card_data()) -> {token(), hash()}.
find_or_create_token(CardData) ->
    {CurrentKeyID, CurrentKey} = cds_keyring_manager:get_current_key(),
    Keys = [Key || {KeyID, Key} <- cds_keyring_manager:get_all_keys(), KeyID =/= CurrentKeyID],
    CurrentHash = hash(CardData, CurrentKey),
    FindResult = try
        % let's check current key first
        {cds_storage:get_token(CurrentHash), CurrentHash}
    catch
        not_found ->
            % if not found, check other keys
            find_token(CardData, Keys)
    end,
    case FindResult of
        {_Token, _Hash} ->
            FindResult;
        not_found ->
            {token(), CurrentHash}
    end.

find_token(_, []) ->
    not_found;
find_token(CardData, [Key | Others]) ->
    Hash = hash(CardData, Key),
    try
        {cds_storage:get_token(Hash), Hash}
    catch
        not_found ->
            find_token(CardData, Others)
    end.

-spec token() -> token().
token() ->
    crypto:strong_rand_bytes(16).

-spec session() -> session().
session() ->
    crypto:strong_rand_bytes(16).
