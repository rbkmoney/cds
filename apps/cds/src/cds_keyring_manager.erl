-module(cds_keyring_manager).
-behaviour(gen_fsm).

-include_lib("shamir/include/shamir.hrl").

%% API.
-export([start_link/0]).
-export([get_key/1]).
-export([get_keyring/0]).
-export([get_current_key/0]).
-export([get_outdated_keys/0]).
-export([start_unlock/0]).
-export([validate_unlock/1]).
-export([cancel_unlock/0]).
-export([lock/0]).
-export([start_rotate/0]).
-export([validate_rotate/1]).
-export([cancel_rotate/0]).
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
    keyring :: cds_keyring:keyring() | undefined
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

-spec start_unlock() -> ok.
start_unlock() ->
    sync_send_event(start_unlock).

-spec validate_unlock(cds_keysharing:masterkey_share()) -> {more, non_neg_integer()} | ok.
validate_unlock(Share) ->
    sync_send_event({validate_unlock, Share}).

-spec cancel_unlock() -> ok.
cancel_unlock() ->
    sync_send_event(cancel_unlock).

-spec lock() -> ok.
lock() ->
    sync_send_event(lock).

-spec start_rotate() -> ok.
start_rotate() ->
    sync_send_event(start_rotate).

-spec validate_rotate(cds_keysharing:masterkey_share()) -> {more, non_neg_integer()} | ok.
validate_rotate(Share) ->
    sync_send_event({validate_rotate, Share}).

-spec cancel_rotate() -> ok.
cancel_rotate() ->
    sync_send_event(cancel_rotate).

-spec initialize(integer()) -> cds_keyring_initializer:encrypted_master_key_shares().
initialize(Threshold) ->
    sync_send_event({initialize, Threshold}).

-spec validate_init(cds_keysharing:masterkey_share()) -> {more, non_neg_integer()} | ok.
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
        _Keyring ->
            {ok, locked, #state{keyring = undefined}}
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
    Result = cds_keyring_initializer:initialize(Threshold),
    {reply, Result, not_initialized, StateData};
not_initialized({validate_init, Share}, _From, StateData) ->
    case cds_keyring_initializer:validate(Share) of
        {ok, {more, _More}} = Result ->
            {reply, Result, not_initialized, StateData};
        {ok, {done, {EncryptedKeyring, DecryptedKeyring}}} ->
            ok = cds_keyring_storage:create(EncryptedKeyring),
            NewStateData = StateData#state{keyring = DecryptedKeyring},
            {reply, ok, unlocked, NewStateData};
        {error, _Error} = Result ->
            {reply, Result, not_initialized, StateData}
    end;
not_initialized(cancel_init, _From, StateData) ->
    ok = cds_keyring_initializer:cancel(),
    {reply, ok, not_initialized, StateData};
not_initialized(_Event, _From, StateData) ->
    {reply, {error, {invalid_status, not_initialized}}, not_initialized, StateData}.

-spec locked(term(), term(), term()) -> term().

locked(start_unlock, _From, StateData) ->
    LockedKeyring = cds_keyring_storage:read(),
    Result = cds_keyring_unlocker:initialize(LockedKeyring),
    {reply, Result, locked, StateData};
locked({validate_unlock, Share}, _From, StateData) ->
    case cds_keyring_unlocker:validate(Share) of
        {ok, {more, _More}} = Result ->
            {reply, Result, locked, StateData};
        {ok, {done, UnlockedKeyring}} ->
            NewStateData = StateData#state{keyring = UnlockedKeyring},
            {reply, ok, unlocked, NewStateData};
        {error, Error} ->
            {reply, {error, Error}, locked, StateData}
    end;
locked(cancel_unlock, _From, StateData) ->
    ok = cds_keyring_unlocker:cancel(),
    {reply, ok, locked, StateData};
locked(_Event, _From, StateData) ->
    {reply, {error, {invalid_status, locked}}, locked, StateData}.

-spec unlocked(term(), term(), state()) -> term().

unlocked(lock, _From, StateData) ->
    {reply, ok, locked, StateData#state{keyring = undefined}};
unlocked(get_keyring, _From, #state{keyring = Keyring} = StateData) ->
    {reply, {ok, Keyring}, unlocked, StateData};
unlocked({get_key, KeyId}, _From, #state{keyring = Keyring} = StateData) ->
    {reply, cds_keyring:get_key(KeyId, Keyring), unlocked, StateData};
unlocked(get_current_key, _From, #state{keyring = Keyring} = StateData) ->
    {reply, {ok, cds_keyring:get_current_key(Keyring)}, unlocked, StateData};
unlocked(start_rotate, _From, #state{keyring = OldKeyring} = StateData) ->
    EncryptedKeyring = cds_keyring_storage:read(),
    Result = cds_keyring_rotator:initialize(OldKeyring, EncryptedKeyring),
    {reply, Result, unlocked, StateData};
unlocked({validate_rotate, Share}, _From, StateData) ->
    case cds_keyring_rotator:validate(Share) of
        {ok, {more, _More}} = Result ->
            {reply, Result, unlocked, StateData};
        {ok, {done, {EncryptedNewKeyring, NewKeyring}}} ->
            ok = cds_keyring_storage:update(EncryptedNewKeyring),
            NewStateData = StateData#state{keyring = NewKeyring},
            {reply, ok, unlocked, NewStateData};
        {error, Error} ->
            {reply, {error, Error}, unlocked, StateData}
    end;
unlocked(cancel_rotate, _From, StateData) ->
    ok = cds_keyring_rotator:cancel(),
    {reply, ok, unlocked, StateData};
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
