-module(cds_keyring_rotator).

-behavior(gen_statem).

-include_lib("shamir/include/shamir.hrl").

-define(STATEM, ?MODULE).

%% API
-export([init/1, callback_mode/0]).

-export([start_link/0]).
-export([initialize/2]).
-export([confirm/2]).
-export([get_status/0]).
-export([cancel/0]).
-export([handle_event/4]).
-export_type([status/0]).

-record(data, {
    encrypted_keyring :: encrypted_keyring() | undefined,
    keyring :: keyring() | undefined,
    shares = #{} :: #{cds_keysharing:share_id() => {shareholder_id(), masterkey_share()}},
    timer :: reference() | undefined
}).

-type data() :: #data{}.
-type seconds() :: non_neg_integer().
-type status() :: #{
    phase => state(),
    lifetime => seconds() | undefined,
    confirmation_shares => #{cds_keysharing:share_id() => shareholder_id()}
}.

-type state() :: uninitialized | validation.

-type shareholder_id() :: cds_shareholder:shareholder_id().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: cds_keysharing:masterkey_shares().
-type keyring() :: cds_keyring:keyring().
-type encrypted_keyring() :: cds_keyring:encrypted_keyring().
-type rotate_errors() ::
    wrong_masterkey | failed_to_recover.
-type invalid_activity() :: {error, {invalid_activity, {rotation, state()}}}.
-type rotate_resp() ::
    {ok, {done, {encrypted_keyring(), keyring()}}} |
    {ok, {more, non_neg_integer()}}|
    {error, {operation_aborted, rotate_errors()}}.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(keyring(), encrypted_keyring()) -> ok | invalid_activity().

initialize(Keyring, EncryptedKeyring) ->
    call({initialize, Keyring, EncryptedKeyring}).

-spec confirm(shareholder_id(), masterkey_share()) -> rotate_resp() | invalid_activity().

confirm(ShareholderId, Share) ->
    call({confirm, ShareholderId, Share}).

-spec cancel() -> ok.

cancel() ->
    call(cancel).

-spec get_status() -> status().

get_status() ->
    call(get_status).

call(Msg) ->
    gen_statem:call(?STATEM, Msg).

-spec init(_) -> {ok, state(), data()}.

init([]) ->
    {ok, uninitialized, #data{}}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) ->
    gen_statem:event_handler_result(state()).

%% Successful workflow events

handle_event({call, From}, {initialize, Keyring, EncryptedKeyring}, uninitialized, _Data) ->
    TimerRef = erlang:start_timer(get_timeout(), self(), lifetime_expired),
    {next_state,
        validation,
        #data{keyring = Keyring, encrypted_keyring = EncryptedKeyring, timer = TimerRef}, {reply, From, ok}};

handle_event({call, From}, {confirm, ShareholderId, Share}, validation,
    #data{encrypted_keyring = EncryptedOldKeyring,
        keyring = OldKeyring,
        shares = Shares,
        timer = TimerRef} = StateData) ->
    #share{threshold = Threshold, x = X} = cds_keysharing:convert(Share),
    case Shares#{X => {ShareholderId, Share}} of
        AllShares when map_size(AllShares) =:= Threshold ->
            _ = erlang:cancel_timer(TimerRef),
            ListShares = cds_keysharing:get_shares(AllShares),
            Result = update_keyring(OldKeyring, EncryptedOldKeyring, ListShares),
            {next_state, uninitialized, #data{}, {reply, From, Result}};
        More ->
            {keep_state,
                StateData#data{shares = More},
                {reply, From, {ok, {more, Threshold - map_size(More)}}}}
    end;

%% Common events

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, {reply, From, State}};
handle_event({call, From}, get_status, State, #data{timer = TimerRef, shares = ValidationShares}) ->
    Lifetime = get_lifetime(TimerRef),
    ValidationSharesStripped = cds_keysharing:get_id_map(ValidationShares),
    Status = #{
        phase => State,
        lifetime => Lifetime,
        confirmation_shares => ValidationSharesStripped
    },
    {keep_state_and_data, {reply, From, Status}};
handle_event({call, From}, cancel, _State, #data{timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    {next_state, uninitialized, #data{}, {reply, From, ok}};
handle_event(info, {timeout, _TimerRef, lifetime_expired}, _State, _Data) ->
    {next_state, uninitialized, #data{}, []};

%% InvalidActivity events

handle_event({call, From}, _Event, uninitialized, _Data) ->
    {keep_state_and_data,
        {reply, From, {error, {invalid_activity, {rotation, uninitialized}}}}
    };
handle_event({call, From}, _Event, validation, _Data) ->
    {keep_state_and_data,
        {reply, From, {error, {invalid_activity, {rotation, validation}}}}
    }.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    application:get_env(cds, keyring_rotation_lifetime, 60000).

-spec get_lifetime(reference() | undefined) -> seconds() | undefined.

get_lifetime(TimerRef) ->
    case TimerRef of
        undefined ->
            undefined;
        TimerRef ->
            erlang:read_timer(TimerRef) div 1000
    end.

-spec update_keyring(keyring(), encrypted_keyring(), masterkey_shares()) ->
    {ok, {done, {encrypted_keyring(), keyring()}}} | {error, {operation_aborted, rotate_errors()}}.

update_keyring(OldKeyring, EncryptedOldKeyring, AllShares) ->
    case cds_keysharing:recover(AllShares) of
        {ok, MasterKey} ->
            case cds_keyring:validate_masterkey(MasterKey, OldKeyring, EncryptedOldKeyring) of
                {ok, OldKeyring} ->
                    NewKeyring = cds_keyring:rotate(OldKeyring),
                    EncryptedNewKeyring = cds_keyring:encrypt(MasterKey, NewKeyring),
                    {ok, {done, {EncryptedNewKeyring, NewKeyring}}};
                {error, Error} ->
                    {error, {operation_aborted, Error}}
            end;
        {error, Error} ->
            {error, {operation_aborted, Error}}
    end.
