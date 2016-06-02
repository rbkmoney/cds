-module(cds_keyring_manager).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([get_key/1]).
-export([get_all_keys/0]).
-export([get_current_key/0]).
-export([unlock/1]).
-export([lock/0]).
-export([update/0]).
-export([rotate/0]).
-export([initialize/2]).
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

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?FSM}, ?MODULE, [], []).

-spec get_key(cds_keyring:key_id()) -> {cds_keyring:key_id(), cds_crypto:key()}.
get_key(KeyId) ->
    sync_send_event({get_key, KeyId}).

-spec get_all_keys() -> [{cds_keyring:key_id(), cds_crypto:key()}].
get_all_keys() ->
    sync_send_event(get_all_keys).

-spec get_current_key() -> {cds_keyring:key_id(), cds_crypto:key()}.
get_current_key() ->
    sync_send_event(get_current_key).

-spec unlock(binary()) -> {more, byte()} | ok.
unlock(Share) ->
    sync_send_event({unlock, Share}).

-spec lock() -> ok.
lock() ->
    sync_send_event(lock).

-spec update() -> ok.
update() ->
    sync_send_event(update).

-spec rotate() -> ok.
rotate() ->
    sync_send_event(rotate).

-spec initialize(integer(), integer()) -> [binary()].
initialize(Threshold, Count) ->
    sync_send_event({initialize, Threshold, Count}).

-spec get_state() -> locked | unlocked.
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

init([]) ->
    try cds_keyring_storage:read() of
        Keyring ->
            {ok, locked, #state{keyring = Keyring}}
    catch
        not_found ->
            {ok, not_initialized, #state{}}
    end.

not_initialized(_Event, StateData) ->
    {next_state, not_initialized, StateData}.

locked(_Event, StateData) ->
    {next_state, locked, StateData}.

unlocked(_Event, StateData) ->
    {next_state, unlocked, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

not_initialized({initialize, Threshold, Count}, _From, StateData) ->
    MasterKey = cds_crypto:key(),
    Keyring = cds_keyring:new(),
    Shares = cds_keysharing:share(MasterKey, Threshold, Count),
    EncryptedKeyring = cds_keyring:encrypt(MasterKey, Keyring),
    try cds_keyring_storage:create(EncryptedKeyring) of
        ok ->
            {reply, {ok, Shares}, unlocked, StateData#state{masterkey = MasterKey, keyring = Keyring}}
    catch
        already_exists ->
            {stop, normal, {error, already_initialized}, StateData}
    end;
not_initialized(_Event, _From, StateData) ->
    {reply, {error, not_initialized}, not_initialized, StateData}.

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
    {reply, {error, locked}, locked, StateData}.

unlocked(lock, _From, #state{masterkey = MasterKey, keyring = Keyring} = StateData) ->
    EncryptedKeyring = cds_keyring:encrypt(MasterKey, Keyring),
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
unlocked({get_key, KeyId}, _From, #state{keyring = #{keys := Keys}} = StateData) ->
    try maps:get(KeyId, Keys) of
        Key ->
            {reply, {ok, {KeyId, Key}}, unlocked, StateData}
    catch
        error:{badkey, KeyId} ->
            {reply, {error, not_found}, unlocked, StateData}
    end;
unlocked(get_all_keys, _From, #state{keyring = #{keys := Keys}} = StateData) ->
    {reply, {ok, maps:to_list(Keys)}, unlocked, StateData};
unlocked(get_current_key, _From, #state{keyring = #{current_key := CurrentKeyId, keys := Keys}} = StateData) ->
    CurrentKey = maps:get(CurrentKeyId, Keys),
    {reply, {ok, {CurrentKeyId, CurrentKey}}, unlocked, StateData};
unlocked(rotate, _From, #state{keyring = OldKeyring, masterkey = MasterKey} = StateData) ->
    NewKeyring = cds_keyring:rotate(OldKeyring),
    EncryptedNewKeyring = cds_keyring:encrypt(MasterKey, NewKeyring),
    try cds_keyring_storage:update(EncryptedNewKeyring) of
        ok ->
            {reply, ok, unlocked, StateData#state{keyring = NewKeyring}}
    catch
        conditional_check_failed ->
            {reply, {error, out_of_date}, unlocked, StateData}
    end;
unlocked(_Event, _From, StateData) ->
    {reply, ignored, unlocked, StateData}.

handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateName}, StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ignored, StateName, StateData}.

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
