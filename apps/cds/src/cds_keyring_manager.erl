-module(cds_keyring_manager).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([get_key/1]).
-export([get_keyring/0]).
-export([get_current_key/0]).
-export([get_outdated_keys/0]).
-export([unlock/1]).
-export([lock/0]).
-export([update/0]).
-export([rotate/1]).
-export([initialize/1]).
-export([validate_init/1]).
-export([cancel_init/0]).
-export([get_state/0]).

%% gen_fsm.
-export([init/1]).
-export([locked/2]).
-export([unlocked/2]).
-export([not_initialized/2]).
-export([handle_event/3]).
-export([locked/3]).
-export([unlocked/3]).
-export([not_initialized/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-define(FSM, ?MODULE).

-record(state, {
    masterkey,
    keyring,
    shares = #{}
}).

-type state() :: #state{}.

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?FSM}, ?MODULE, [], []).

-spec get_key(cds_keyring:key_id()) -> {cds_keyring:key_id(), cds_keyring:key()}.
get_key(KeyId) ->
    sync_send_event({get_key, KeyId}).

-spec get_keyring() -> cds_keyring:keyring().
get_keyring() ->
    sync_send_event(get_keyring).

-spec get_current_key() -> {cds_keyring:key_id(), cds_keyring:key()}.
get_current_key() ->
    sync_send_event(get_current_key).

-spec get_outdated_keys() -> [{From :: byte(), To :: byte()}].
get_outdated_keys() ->
    {KeyID, _} = get_current_key(),
    #{min := MinID, max := MaxID} = cds_keyring:get_key_id_config(),
    [I || {From, To} = I <- [{MinID, KeyID - 1}, {KeyID + 1, MaxID}], From =< To].

-spec unlock(cds_keysharing:masterkey_share()) -> {more, byte()} | ok.
unlock(Share) ->
    sync_send_event({unlock, Share}).

-spec lock() -> ok.
lock() ->
    sync_send_event(lock).

-spec update() -> ok.
update() ->
    sync_send_event(update).

-spec rotate(cds_keysharing:masterkey_share()) -> {more, byte()} | ok.
rotate(Share) ->
    sync_send_event({rotate, Share}).

-spec initialize(integer()) -> cds_keyring_initializator:encrypted_master_key_shares().
initialize(Threshold) ->
    sync_send_event({initialize, Threshold}).

-spec validate_init(cds_keyring_utils:masterkey_share()) -> {more, non_neg_integer()} | ok.

validate_init(Share) ->
    sync_send_event({validate_init, Share}).

-spec cancel_init() -> ok.

cancel_init() ->
    sync_send_event(cancel_init).

-spec get_state() -> locked | unlocked | not_initialized.
get_state() ->
    sync_send_all_state_event(get_state).


sync_send_event(Event) ->
    case gen_fsm:sync_send_event(?FSM, Event) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {error, Reason} ->
            throw(Reason);
        ignored ->
            ok
    end.

sync_send_all_state_event(Event) ->
    case gen_fsm:sync_send_all_state_event(?FSM, Event) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {error, Reason} ->
            throw(Reason);
        ignored ->
            ok
    end.

%% gen_fsm.

-spec init(_) -> {ok, locked | not_initialized, state()}.

init([]) ->
    try cds_keyring_storage:read() of
        Keyring ->
            {ok, locked, #state{keyring = Keyring}}
    catch
        not_found ->
            {ok, not_initialized, #state{}}
    end.

-spec not_initialized(_, state()) -> {next_state, not_initialized, state()}.

not_initialized(_Event, StateData) ->
    {next_state, not_initialized, StateData}.

-spec locked(_, state()) -> {next_state, locked, state()}.

locked(_Event, StateData) ->
    {next_state, locked, StateData}.

-spec unlocked(_, state()) -> {next_state, unlocked, state()}.

unlocked(_Event, StateData) ->
    {next_state, unlocked, StateData}.

-spec handle_event(_, atom(), state()) -> {next_state, atom(), state()}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

-spec not_initialized(term(), term(), state()) -> term().

not_initialized({initialize, Threshold}, _From, StateData) ->
    Result = cds_keyring_initializator:initialize(Threshold),
    {reply, Result, not_initialized, StateData};
not_initialized({validate_init, Share}, _From, StateData) ->
    case cds_keyring_initializator:validate(Share) of
        {ok, {more, _More}} = Result ->
            {reply, Result, not_initialized, StateData};
        {ok, Keyring} ->
            NewStateData = StateData#state{keyring = Keyring},
            {reply, ok, unlocked, NewStateData};
        {error, _Error} = Result ->
            {reply, Result, not_initialized, StateData}
    end;
not_initialized(cancel_init, _From, StateData) ->
    ok = cds_keyring_initializator:cancel(),
    {reply, ok, not_initialized, StateData};
not_initialized(_Event, _From, StateData) ->
    {reply, {error, {invalid_status, uninitialized}}, not_initialized, StateData}.

-spec locked(term(), term(), term()) -> term().

locked(update, _From, StateData) ->
    try cds_keyring_storage:read() of
        Keyring ->
            {reply, ok, locked, StateData#state{keyring = Keyring}}
    catch
        not_found ->
            {reply, ok, not_initialized, StateData}
    end;
locked({unlock, <<Threshold, X, _Y/binary>> = Share}, _From, #state{shares = Shares, keyring = Keyring} = StateData) ->
    case Shares#{X => Share} of
        AllShares when map_size(AllShares) =:= Threshold ->
            try
                MasterKey = cds_keysharing:recover(maps:values(AllShares)),
                DecryptedKeyring = cds_keyring:decrypt(MasterKey, Keyring),
                NewStateData = StateData#state{shares = #{}, keyring = DecryptedKeyring, masterkey = MasterKey},
                {reply, ok, unlocked, NewStateData}
            catch Error ->
                {stop, normal, {error, Error}, StateData}
            end;
        More ->
            {reply, {ok, {more, Threshold - maps:size(More)}}, locked, StateData#state{shares = More}}
    end;
locked(_Event, _From, StateData) ->
    {reply, {error, {invalid_status, locked}}, locked, StateData}.

-spec unlocked(term(), term(), state()) -> term().

unlocked(lock, _From, StateData) ->
    EncryptedKeyring = cds_keyring_storage:read(),
    {reply, ok, locked, StateData#state{keyring = EncryptedKeyring, masterkey = undefined}};
unlocked(update, _From, #state{masterkey = MasterKey} = StateData) ->
    try cds_keyring_storage:read() of
        Keyring ->
            DecryptedKeyring = cds_keyring:decrypt(MasterKey, Keyring),
            {reply, ok, unlocked, StateData#state{keyring = DecryptedKeyring}}
    catch
        not_found ->
            {reply, ok, not_initialized, StateData#state{keyring = undefined, masterkey = undefined}}
    end;
unlocked(get_keyring, _From, #state{keyring = Keyring} = StateData) ->
    {reply, {ok, Keyring}, unlocked, StateData};
unlocked({get_key, KeyId}, _From, #state{keyring = Keyring} = StateData) ->
    {reply, cds_keyring:get_key(KeyId, Keyring), unlocked, StateData};
unlocked(get_current_key, _From, #state{keyring = Keyring} = StateData) ->
    {reply, {ok, cds_keyring:get_current_key(Keyring)}, unlocked, StateData};
unlocked({rotate, Share}, _From, #state{keyring = OldKeyring} = StateData) ->
    case cds_keyring_rotator:rotate(Share, OldKeyring) of
        {ok, {more, _More}} = Result ->
            {reply, Result, unlocked, StateData};
        {ok, NewKeyring} ->
            {reply, ok, unlocked, StateData#state{keyring = NewKeyring}};
        Result ->
            {reply, Result, unlocked, StateData}
    end;
unlocked(_Event, _From, StateData) ->
    {reply, {error, {invalid_status, unlocked}}, unlocked, StateData}.

-spec handle_sync_event(term(), term(), atom(), state()) -> {reply, term(), atom(), state()}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateName}, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.

-spec handle_info(term(), atom(), state()) -> {next_state, atom(), state()}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

-spec terminate(term(), atom(), term()) -> ok.

terminate(_Reason, _StateName, _StateData) ->
    ok.

-spec code_change(term(), atom(), state(), term()) -> {ok, atom(), state()}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
