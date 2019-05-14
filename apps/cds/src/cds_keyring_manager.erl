-module(cds_keyring_manager).
-behaviour(gen_statem).

-include_lib("shamir/include/shamir.hrl").

%% API.
-export([start_link/0]).
-export([get_key/1]).
-export([get_keyring/0]).
-export([get_current_key/0]).
-export([get_outdated_keys/0]).
-export([start_unlock/0]).
-export([confirm_unlock/2]).
-export([cancel_unlock/0]).
-export([lock/0]).
-export([start_rotate/0]).
-export([confirm_rotate/2]).
-export([cancel_rotate/0]).
-export([initialize/1]).
-export([validate_init/2]).
-export([cancel_init/0]).
-export([start_rekey/1]).
-export([confirm_rekey/2]).
-export([start_validate_rekey/0]).
-export([validate_rekey/2]).
-export([cancel_rekey/0]).
-export([get_status/0]).

%% gen_statem.
-export([init/1]).
-export([callback_mode/0]).
-export([locked/3]).
-export([unlocked/3]).
-export([not_initialized/3]).
-export([terminate/3]).
-export([code_change/4]).
-export_type([status/0]).

-define(STATEM, ?MODULE).
-define(HANDLE_COMMON,
    ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, ?FUNCTION_NAME, D)).

-record(state, {
    keyring :: cds_keyring:keyring() | undefined
}).

-type state() :: #state{}.
-type status() :: #{
    status => locked | unlocked | not_initialized,
    activities => #{
        initialization => cds_keyring_initializer:status(),
        rotation => cds_keyring_rotator:status(),
        unlock => cds_keyring_unlocker:status(),
        rekeying => cds_keyring_rekeyer:status()
    }
}.

%% API.

-spec callback_mode() -> state_functions.
callback_mode() -> state_functions.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec get_key(cds_keyring:key_id()) -> {cds_keyring:key_id(), cds_keyring:key()}.
get_key(KeyId) ->
    call({get_key, KeyId}).

-spec get_keyring() -> cds_keyring:keyring().
get_keyring() ->
    call(get_keyring).

-spec get_current_key() -> {cds_keyring:key_id(), cds_keyring:key()}.
get_current_key() ->
    call(get_current_key).

-spec get_outdated_keys() -> [{From :: byte(), To :: byte()}].
get_outdated_keys() ->
    {KeyID, _} = get_current_key(),
    #{min := MinID, max := MaxID} = cds_keyring:get_key_id_config(),
    [I || {From, To} = I <- [{MinID, KeyID - 1}, {KeyID + 1, MaxID}], From =< To].

-spec start_unlock() -> ok.
start_unlock() ->
    call(start_unlock).

-spec confirm_unlock(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()) ->
    {more, non_neg_integer()} | ok.
confirm_unlock(ShareholderId, Share) ->
    call({confirm_unlock, ShareholderId, Share}).

-spec cancel_unlock() -> ok.
cancel_unlock() ->
    call(cancel_unlock).

-spec lock() -> ok.
lock() ->
    call(lock).

-spec start_rotate() -> ok.
start_rotate() ->
    call(start_rotate).

-spec confirm_rotate(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()) ->
    {more, non_neg_integer()} | ok.
confirm_rotate(ShareholderId, Share) ->
    call({confirm_rotate, ShareholderId, Share}).

-spec cancel_rotate() -> ok.
cancel_rotate() ->
    call(cancel_rotate).

-spec initialize(integer()) -> cds_keyring_initializer:encrypted_master_key_shares().
initialize(Threshold) ->
    call({initialize, Threshold}).

-spec validate_init(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()) ->
    {more, non_neg_integer()} | ok.
validate_init(ShareholderId, Share) ->
    call({validate_init, ShareholderId, Share}).

-spec cancel_init() -> ok.
cancel_init() ->
    call(cancel_init).

-spec start_rekey(integer()) -> ok.
start_rekey(Threshold) ->
    call({start_rekey, Threshold}).

-spec confirm_rekey(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()) ->
    {more, non_neg_integer()} | ok.
confirm_rekey(ShareholderId, Share) ->
    call({confirm_rekey, ShareholderId, Share}).

-spec start_validate_rekey() -> cds_keyring_initializer:encrypted_master_key_shares().
start_validate_rekey() ->
    call(start_validate_rekey).

-spec validate_rekey(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share()) ->
    {more, non_neg_integer()} | ok.
validate_rekey(ShareholderId, Share) ->
    call({validate_rekey, ShareholderId, Share}).

-spec cancel_rekey() -> ok.
cancel_rekey() ->
    call(cancel_rekey).

-spec get_status() -> status().
get_status() ->
    call(get_status).

call(Event) ->
    case gen_statem:call(?STATEM, Event) of
        ok ->
            ok;
        {ok, Reply} ->
            Reply;
        {error, Reason} ->
            throw(Reason)
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

-spec not_initialized(term(), term(), state()) -> term().
not_initialized({call, From}, {initialize, Threshold}, _StateData) ->
    Result = cds_keyring_initializer:initialize(Threshold),
    {keep_state_and_data, {reply, From, Result}};
not_initialized({call, From}, {validate_init, ShareholderId, Share}, StateData) ->
    case cds_keyring_initializer:validate(ShareholderId, Share) of
        {ok, {more, _More}} = Result ->
            {keep_state_and_data, {reply, From, Result}};
        {ok, {done, {EncryptedKeyring, DecryptedKeyring}}} ->
            ok = cds_keyring_storage:create(EncryptedKeyring),
            NewStateData = StateData#state{keyring = DecryptedKeyring},
            {next_state, unlocked, NewStateData, {reply, From, ok}};
        {error, _Error} = Result ->
            {keep_state_and_data, {reply, From, Result}}
    end;
not_initialized({call, From}, cancel_init, _StateData) ->
    ok = cds_keyring_initializer:cancel(),
    {keep_state_and_data, {reply, From, ok}};
?HANDLE_COMMON.

-spec locked(term(), term(), term()) -> term().
locked({call, From}, start_unlock, _StateData) ->
    LockedKeyring = cds_keyring_storage:read(),
    Result = cds_keyring_unlocker:initialize(LockedKeyring),
    {keep_state_and_data, {reply, From, Result}};
locked({call, From}, {confirm_unlock, ShareholderId, Share}, StateData) ->
    case cds_keyring_unlocker:confirm(ShareholderId, Share) of
        {ok, {more, _More}} = Result ->
            {keep_state_and_data, {reply, From, Result}};
        {ok, {done, UnlockedKeyring}} ->
            NewStateData = StateData#state{keyring = UnlockedKeyring},
            {next_state, unlocked, NewStateData, {reply, From, ok}};
        {error, Error} ->
            {keep_state_and_data, {reply, From, {error, Error}}}
    end;
locked({call, From}, cancel_unlock, _StateData) ->
    ok = cds_keyring_unlocker:cancel(),
    {keep_state_and_data, {reply, From, ok}};
?HANDLE_COMMON.

-spec unlocked(term(), term(), state()) -> term().
unlocked({call, From}, lock, StateData) ->
    {next_state, locked, StateData#state{keyring = undefined}, {reply, From, ok}};
unlocked({call, From}, get_keyring, #state{keyring = Keyring}) ->
    {keep_state_and_data, {reply, From, {ok, Keyring}}};
unlocked({call, From}, {get_key, KeyId}, #state{keyring = Keyring}) ->
    {keep_state_and_data, {reply, From, cds_keyring:get_key(KeyId, Keyring)}};
unlocked({call, From}, get_current_key, #state{keyring = Keyring}) ->
    {keep_state_and_data, {reply, From, {ok, cds_keyring:get_current_key(Keyring)}}};
unlocked({call, From}, start_rotate, #state{keyring = OldKeyring}) ->
    EncryptedKeyring = cds_keyring_storage:read(),
    Result = cds_keyring_rotator:initialize(OldKeyring, EncryptedKeyring),
    {keep_state_and_data, {reply, From, Result}};
unlocked({call, From}, {confirm_rotate, ShareholderId, Share}, StateData) ->
    case cds_keyring_rotator:confirm(ShareholderId, Share) of
        {ok, {more, _More}} = Result ->
            {keep_state_and_data, {reply, From, Result}};
        {ok, {done, {EncryptedNewKeyring, NewKeyring}}} ->
            ok = cds_keyring_storage:update(EncryptedNewKeyring),
            NewStateData = StateData#state{keyring = NewKeyring},
            {keep_state, NewStateData, {reply, From, ok}};
        {error, Error} ->
            {keep_state_and_data, {reply, From, {error, Error}}}
    end;
unlocked({call, From}, cancel_rotate, _StateData) ->
    ok = cds_keyring_rotator:cancel(),
    {keep_state_and_data, {reply, From, ok}};
unlocked({call, From}, {start_rekey, Threshold}, _StateData) ->
    EncryptedKeyring = cds_keyring_storage:read(),
    Result = cds_keyring_rekeyer:initialize(Threshold, EncryptedKeyring),
    {keep_state_and_data, {reply, From, Result}};
unlocked({call, From}, {confirm_rekey, ShareholderId, Share}, _StateData) ->
    Result =  cds_keyring_rekeyer:confirm(ShareholderId, Share),
    {keep_state_and_data, {reply, From, Result}};
unlocked({call, From}, start_validate_rekey, _StateData) ->
    Result = cds_keyring_rekeyer:start_validation(),
    {keep_state_and_data, {reply, From, Result}};
unlocked({call, From}, {validate_rekey, ShareholderId, Share}, _StateData) ->
    case cds_keyring_rekeyer:validate(ShareholderId, Share) of
        {ok, {more, _More}} = Result ->
            {keep_state_and_data, {reply, From, Result}};
        {ok, {done, EncryptedNewKeyring}} ->
            ok = cds_keyring_storage:update(EncryptedNewKeyring),
            {keep_state_and_data, {reply, From, ok}};
        {error, Error} ->
            {keep_state_and_data, {reply, From, {error, Error}}}
    end;
unlocked({call, From}, cancel_rekey, _StateData) ->
    ok = cds_keyring_rekeyer:cancel(),
    {keep_state_and_data, {reply, From, ok}};
?HANDLE_COMMON.

-spec (handle_common(term(), term(), atom(), state()) -> term()).
handle_common({call, From}, get_status, FunctionName, _Data) ->
    {keep_state_and_data, {reply, From, {ok, generate_status(FunctionName)}}};
handle_common({call, From}, _Msg, FunctionName, _Data) ->
    {keep_state_and_data, {reply, From, {error, {invalid_status, FunctionName}}}}.

-spec generate_status(atom()) -> status().
generate_status(StateName) ->
    #{
        status => StateName,
        activities => #{
            initialization => cds_keyring_initializer:get_status(),
            rotation => cds_keyring_rotator:get_status(),
            unlock => cds_keyring_unlocker:get_status(),
            rekeying => cds_keyring_rekeyer:get_status()
        }
    }.

-spec terminate(term(), atom(), term()) -> ok.
terminate(_Reason, _StateName, _StateData) ->
    ok.

-spec code_change(term(), atom(), state(), term()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
