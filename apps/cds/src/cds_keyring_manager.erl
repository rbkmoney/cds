-module(cds_keyring_manager).
-behaviour(gen_fsm).

%% API.
-export([start_link/0]).
-export([get_key/1]).
-export([get_all_keys/0]).
-export([get_current_key/0]).
-export([unlock/1]).
-export([lock/0]).
-export([update_keyring/1]).
-export([rotate_keyring/0]).
-export([get_state/0]).

%% gen_fsm.
-export([init/1]).
-export([locked/2]).
-export([unlocked/2]).
-export([handle_event/3]).
-export([locked/3]).
-export([unlocked/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-include("cds_keyring.hrl").

-define(FSM, ?MODULE).
-define(UNLOCK_TIMEOUT, 60*1000).

-record(state, {
    masterkey,
    keyring,
    shares = #{}
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_fsm:start_link({local, ?FSM}, ?MODULE, [], []).

-spec get_key(key_id()) -> {key_id(), cds_crypto:key()}.
get_key(KeyId) ->
    sync_send_event({get_key, KeyId}).

-spec get_all_keys() -> [{key_id(), cds_crypto:key()}].
get_all_keys() ->
    sync_send_event(get_all_keys).

-spec get_current_key() -> {key_id(), cds_crypto:key()}.
get_current_key() ->
    sync_send_event(get_current_key).

-spec unlock(binary()) -> {more, byte()} | unlocked.
unlock(Share) ->
    sync_send_event({unlock, Share}).

-spec lock() -> ok.
lock() ->
    sync_send_event(lock).

-spec update_keyring(binary()) -> ok.
update_keyring(Keyring) ->
    sync_send_event({update_keyring, Keyring}).

-spec rotate_keyring() -> binary().
rotate_keyring() ->
    sync_send_event(rotate_keyring).

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
    {ok, locked, #state{}}.

locked(timeout, _StateData) ->
    {next_state, locked, #state{}};
locked(_Event, StateData) ->
    {next_state, locked, StateData}.

unlocked(_Event, StateData) ->
    {next_state, unlocked, StateData}.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

locked({update_keyring, Keyring}, _From, _StateData) ->
    {reply, ok, locked, #state{keyring = Keyring}, ?UNLOCK_TIMEOUT};
locked({unlock, _Share}, _From, #state{keyring = undefined} = _StateData) ->
    {reply, {error, no_keyring}, locked, #state{}};
locked({unlock, <<Threshold, X, _Y/binary>> = Share}, _From, #state{shares = Shares, keyring = Keyring} = StateData) ->
    case Shares#{X => Share} of
        AllShares when map_size(AllShares) =:= Threshold ->
            try
                MasterKey = cds_shamir:recover(maps:values(AllShares)),
                DecryptedKeyring = cds_crypto:decrypt(MasterKey, Keyring),
                UnmarshalledKeyring = cds_keyring:unmarshall(DecryptedKeyring),
                {reply, {ok, unlocked}, unlocked, #state{keyring = UnmarshalledKeyring, masterkey = MasterKey}}
            catch Error ->
                {reply, {error, Error}, locked, #state{}}
            end;
        More ->
            {reply, {ok, {more, Threshold - maps:size(More)}}, locked, StateData#state{shares = More}, ?UNLOCK_TIMEOUT}
    end;
locked(_Event, _From, State) ->
    {reply, {error, locked}, locked, State, ?UNLOCK_TIMEOUT}.

unlocked(lock, _From, #state{masterkey = MasterKey, keyring = Keyring}) ->
    try
        MarshalledKeyring = cds_keyring:marshall(Keyring),
        EncryptedKeyring = cds_crypto:encrypt(MasterKey, MarshalledKeyring),
        {reply, ok, locked, #state{keyring = EncryptedKeyring}}
    catch Error ->
        {stop, lock_failed, {error, Error}, #state{}}
    end;
unlocked({update_keyring, Keyring}, _From, #state{masterkey = MasterKey} = StateData) ->
    try
        DecryptedKeyring = cds_crypto:decrypt(MasterKey, Keyring),
        UnmarshalledKeyring = cds_keyring:unmarshall(DecryptedKeyring),
        {reply, ok, unlocked, StateData#state{keyring = UnmarshalledKeyring}}
    catch Error ->
        {reply, {error, Error}, unlocked, StateData}
    end;
unlocked({get_key, KeyId}, _From, #state{keyring = #keyring{keys = Keys}} = StateData) ->
    try
        Key = maps:get(KeyId, Keys),
        {reply, {ok, {KeyId, Key}}, unlocked, StateData}
    catch error:{badkey, KeyId} ->
        {reply, {error, key_not_found}, unlocked, StateData}
    end;
unlocked(get_all_keys, _From, #state{keyring = #keyring{keys = Keys}} = StateData) ->
    {reply, {ok, maps:to_list(Keys)}, unlocked, StateData};
unlocked(get_current_key, _From, #state{keyring = #keyring{current_key = CurrentKeyId, keys = Keys}} = StateData) ->
    CurrentKey = maps:get(CurrentKeyId, Keys),
    {reply, {ok, {CurrentKeyId, CurrentKey}}, unlocked, StateData};
unlocked(rotate_keyring, _From, #state{keyring = Keyring, masterkey = MasterKey} = StateData) ->
    try
        NewKeyring = cds_keyring:rotate(Keyring),
        MarshalledNewKeyring = cds_keyring:marshall(NewKeyring),
        EncryptedNewKeyring = cds_crypto:encrypt(MasterKey, MarshalledNewKeyring),
        {reply, {ok, EncryptedNewKeyring}, unlocked, StateData}
    catch
        Error ->
            {reply, {error, Error}, unlocked, StateData}
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



