-module(cds_keyring_rotator).

-behavior(gen_server).

-include_lib("shamir/include/shamir.hrl").

-define(SERVER, ?MODULE).

%% API
-export([init/1, handle_call/3, handle_cast/2,
    handle_info/2, code_change/3, terminate/2]).

-export([start_link/0]).
-export([rotate/2]).

-record(state, {
    shares = #{}
}).

-type state() :: #state{}.

-type masterkey() :: binary().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: #{integer() => masterkey_share()}.
-type keyring() :: cds_keyring:keyring().
-type encrypted_keyring() :: cds_keyring:encrypted_keyring().
-type rotate_errors() ::
    wrong_masterkey | failed_to_recover | unsigned_content.
-type rotate_resp() ::
    ok |
    {ok, {encrypted_keyring(), keyring()}} |
    {ok, {more, non_neg_integer()}}|
    {error, rotate_errors()}.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec rotate(masterkey_share(), keyring()) -> rotate_resp().

rotate(Share, Keyring) ->
    call({rotate, Share, Keyring}).

call(Msg) ->
    case gen_server:call(?SERVER, Msg) of
        ignored ->
            ok;
        {Result, Payload} ->
            {Result, Payload}
    end.

-spec init(_) -> {ok, state()}.

init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), term(), state()) ->
    {reply, {ok, keyring()}, state()} |
    {reply, {error, {operation_aborted, rotate_errors()}}, state()} |
    {reply, {ok, {more, non_neg_integer()}}, state(), non_neg_integer()}.

handle_call({rotate, {verified_share, {Id, Share}}, OldKeyring}, _From, #state{shares = Shares} = StateData) ->
    #share{threshold = Threshold} = cds_keysharing:convert(Share),
    case Shares#{Id => Share} of
        AllShares when map_size(AllShares) =:= Threshold ->
            case update_keyring(OldKeyring, AllShares) of
                {ok, NewKeyring} ->
                    {reply, {ok, NewKeyring}, #state{}};
                {error, Error} ->
                    {reply, {error, {operation_aborted, Error}}, #state{}}
            end;
        More ->
            {reply, {ok, {more, Threshold - map_size(More)}}, StateData#state{shares = More}, timeout()}
    end;
handle_call({rotate, SignedShare, OldKeyring}, From, StateData) ->
    case cds_crypto:verify(SignedShare) of
        {error, Error} ->
            {reply, {error, {operation_aborted, Error}}, #state{}};
        {ok, {Id, Share}} ->
            handle_call({rotate, {verified_share, {Id, Share}}, OldKeyring}, From, StateData)
    end;
handle_call(_Request, _Form, State) ->
    {reply, ignored, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(timeout, _StateData) ->
    {noreply, #state{}};
handle_info(_Info, StateData) ->
    {noreply, StateData}.

-spec terminate(term(), term()) -> ok.

terminate(_Reason, _StateData) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

-spec timeout() -> non_neg_integer().

timeout() ->
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
