-module(cds_keyring_unlocker).

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
-type masterkey_shares() :: #{cds_keysharing:share_id() => masterkey_share()}.
-type keyring() :: cds_keyring:keyring().
-type locked_keyring() :: cds_keyring:encrypted_keyring().
-type unlock_errors() ::
    wrong_masterkey | failed_to_recover.
-type unlock_resp() ::
    {ok, {done, keyring()}} |
    {ok, {more, non_neg_integer()}}|
    {error, {operation_aborted, unlock_errors()}}.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(locked_keyring()) -> ok.

initialize(Keyring) ->
    call({initialize, Keyring}).

-spec validate(masterkey_share()) -> unlock_resp().

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

-spec init(_) -> {ok, state(), data()}.

init([]) ->
    {ok, uninitialized, #data{}}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) ->
    gen_statem:event_handler_result(state()).

%% Successful workflow events

handle_event({call, From}, {initialize, LockedKeyring}, uninitialized, _Data) ->
    {next_state,
        validation,
        #data{keyring = LockedKeyring},
        [
            {reply, From, ok},
            {{timeout, lifetime}, get_timeout(), expired}
        ]};

handle_event({call, From}, {validate, Share}, validation,
    #data{keyring = LockedKeyring, shares = Shares} = StateData) ->
    #share{threshold = Threshold, x = X} = cds_keysharing:convert(Share),
    case Shares#{X => Share} of
        AllShares when map_size(AllShares) =:= Threshold ->
            case unlock(LockedKeyring, AllShares) of
                {ok, UnlockedKeyring} ->
                    {next_state, uninitialized, #data{},
                        [
                            {reply, From, {ok, {done, UnlockedKeyring}}},
                            {{timeout, lifetime}, infinity, expired}
                        ]};
                {error, Error} ->
                    {next_state, uninitialized, #data{},
                        [
                            {reply, From, {error, {operation_aborted, Error}}},
                            {{timeout, lifetime}, infinity, expired}
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
        {reply, From, {error, {invalid_activity, {unlock, uninitialized}}}}
    ]};
handle_event({call, From}, {initialize, _Threshold}, validation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {unlock, validation}}}}
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
    {next_state, uninitialized, #data{}, []}.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    application:get_env(cds, keyring_unlock_lifetime, 60000).

-spec unlock(locked_keyring(), masterkey_shares()) ->
    {ok, {done, keyring()}} | {error, unlock_errors()}.

unlock(LockedKeyring, AllShares) ->
    case cds_keysharing:recover(AllShares) of
        {ok, MasterKey} ->
            case try_decrypt_keyring(MasterKey, LockedKeyring) of
                {ok, UnlockedKeyring} ->
                    {ok, UnlockedKeyring};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec try_decrypt_keyring(masterkey(), locked_keyring()) -> {ok, keyring()} | {error, wrong_masterkey}.

try_decrypt_keyring(MasterKey, LockedKeyring) ->
    try cds_keyring:decrypt(MasterKey, LockedKeyring) of
        Keyring ->
            {ok, Keyring}
    catch
        decryption_failed ->
            {error, wrong_masterkey}
    end.