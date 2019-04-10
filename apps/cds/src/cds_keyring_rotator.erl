-module(cds_keyring_rotator).

-behavior(gen_statem).

-include_lib("shamir/include/shamir.hrl").

-define(STATEM, ?MODULE).

%% API
-export([init/1, callback_mode/0]).

-export([start_link/0]).
-export([initialize/1]).
-export([validate/1]).
-export([get_state/0]).
-export([cancel/0]).
-export([handle_event/4]).

-record(data, {
    keyring,
    shares = #{}
}).

-type data() :: #data{}.
-type state() :: uninitialized | validation.

-type masterkey() :: binary().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: #{integer() => masterkey_share()}.
-type keyring() :: cds_keyring:keyring().
-type encrypted_keyring() :: cds_keyring:encrypted_keyring().
-type rotate_errors() ::
    wrong_masterkey | failed_to_recover.
-type rotate_resp() ::
    ok |
    {ok, {encrypted_keyring(), keyring()}} |
    {ok, {more, non_neg_integer()}}|
    {error, rotate_errors()}.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(keyring()) ->
    ok | {error, }.

initialize(Keyring) ->
    call({initialize, Keyring}).

-spec validate(masterkey_share()) -> rotate_resp().

validate(Share) ->
    call({validate, Share}).

-spec cancel() -> ok.

cancel() ->
    call(cancel).

-spec get_state() -> atom().

get_state() ->
    call(get_state).

call(Msg) ->
    gen_statem:call(?STATEM, Msg).

-spec init(_) -> {ok, data()}.

init([]) ->
    {ok, uninitialized, #data{}}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) ->
    gen_statem:event_handler_result(state()).

%% Successful workflow events

handle_event({call, From}, {initialize, Keyring}, uninitialized, _Data) ->
    {next_state,
        validation,
        #data{keyring = Keyring},
        [
            {reply, From, ok},
            {{timeout, lifetime}, get_timeout(), expired}
        ]};

handle_event({call, From}, {rotate, Share}, validation, #data{keyring = OldKeyring, shares = Shares} = StateData) ->
    #share{threshold = Threshold, x = X} = cds_keysharing:convert(Share),
    case Shares#{X => Share} of
        AllShares when map_size(AllShares) =:= Threshold ->
            case update_keyring(OldKeyring, AllShares) of
                {ok, NewKeyring} ->
                    {next_state, uninitialized, #data{},
                        [
                            {reply, From, {ok, NewKeyring}},
                            {{timeout, lifetime}, infinite, expired}
                        ]};
                {error, Error} ->
                    {next_state, uninitialized, #data{},
                        [
                            {reply, From, {error, {operation_aborted, Error}}},
                            {{timeout, lifetime}, infinite, expired}
                        ]}
            end;
        More ->
            {keep_state,
                StateData#data{shares = More},
                [{reply, From, {ok, {more, Threshold - map_size(More)}}}]}
    end;

%% InvalidActivity events

handle_event({call, From}, {validate, _Share}, uninitialized, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {initialization, uninitialized}}}}
    ]};
handle_event({call, From}, {initialize, _Threshold}, validation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {initialization, validation}}}}
    ]};


%% Common events

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [
        {reply, From, State}
    ]};
handle_event({call, From}, cancel, _State, _Data) ->
    {next_state, uninitialized, #data{}, [
        {reply, From, ok},
        {{timeout, lifetime}, infinity, []}
    ]};
handle_event({timeout, lifetime}, expired, _State, _Data) ->
    {next_state, uninitialized, #data{}, [{{timeout, lifetime}, infinity, []}]}.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    application:get_env(cds, keyring_rotation_lifetime, 60000).

-spec update_keyring(keyring(), masterkey_shares()) ->
    {ok, {encrypted_keyring(), keyring()}} | {error, rotate_errors()}.

update_keyring(OldKeyring, AllShares) ->
    case cds_keysharing:recover(AllShares) of
        {ok, MasterKey} ->
            case validate_masterkey(MasterKey, OldKeyring) of
                {ok, OldKeyring} ->
                    rotate_keyring(MasterKey, OldKeyring);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec validate_masterkey(masterkey(), keyring()) -> {ok, keyring()} | {error, wrong_masterkey | no_keyring}.

validate_masterkey(MasterKey, Keyring) ->
    EncryptedOldKeyring = cds_keyring_storage:read(),
    try cds_keyring:decrypt(MasterKey, EncryptedOldKeyring) of
        Keyring ->
            {ok, Keyring};
        _NotMatchingKeyring ->
            {error, wrong_masterkey}
    catch
        decryption_failed ->
            {error, wrong_masterkey}
    end.

-spec rotate_keyring(masterkey(), keyring()) -> {ok, {encrypted_keyring(), keyring()}}.

rotate_keyring(MasterKey, Keyring) ->
    NewKeyring = cds_keyring:rotate(Keyring),
    EncryptedNewKeyring = cds_keyring:encrypt(MasterKey, NewKeyring),
    {ok, {EncryptedNewKeyring, NewKeyring}}.
