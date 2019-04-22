-module(cds_keyring_rekeyer).

-behavior(gen_statem).

-include_lib("shamir/include/shamir.hrl").

%% API
-export([init/1, callback_mode/0]).
-export([start_link/0]).
-export([initialize/2]).
-export([confirm/2]).
-export([start_validation/0]).
-export([validate/2]).
-export([get_status/0]).
-export([cancel/0]).
-export([handle_event/4]).
-export_type([encrypted_master_key_shares/0]).
-export_type([status/0]).

-define(STATEM, ?MODULE).

-record(data, {
    threshold,
    encrypted_keyring,
    keyring,
    shareholders,
    confirmation_shares = #{},
    validation_shares = #{},
    timer
}).

-type shareholder_id() :: cds_shareholder:shareholder_id().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: [masterkey_share()].
-type encrypted_master_key_shares() :: cds_keysharing:encrypted_master_key_shares().

-type data() :: #data{}.
-type status() :: #{
    phase => state(),
    lifetime => timer:seconds(),
    confirmation_shares => #{cds_keysharing:share_id() => shareholder_id()},
    validation_shares => #{cds_keysharing:share_id() => shareholder_id()}
}.

-type encrypted_keyring() :: cds_keyring:encrypted_keyring().

-type state() :: uninitialized | validation.

-type threshold() :: cds_keysharing:threshold().

-type validate_errors() :: {operation_aborted,
    non_matching_masterkey | failed_to_decrypt_keyring | failed_to_recover}.
-type confirm_errors() :: {operation_aborted,
    failed_to_recover | wrong_masterkey}.
-type initialize_errors() :: invalid_args.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(threshold(), encrypted_keyring()) ->
    ok | {error, initialize_errors()}.

initialize(Threshold, EncryptedKeyring) ->
    call({initialize, Threshold, EncryptedKeyring}).

-spec confirm(shareholder_id(), masterkey_share()) ->
    {ok, {more, integer()}} | ok | {error, confirm_errors()}.

confirm(ShareholderId, Share) ->
    call({confirm, ShareholderId, Share}).

-spec start_validation() -> {ok, encrypted_master_key_shares()}.

start_validation() ->
    call(start_validatation).

-spec validate(shareholder_id(), masterkey_share()) ->
    {ok, {more, integer()}} |
    {ok, {done, encrypted_keyring()}} |
    {error, validate_errors()} |
    {error, {invalid_activity, state()}}.

validate(ShareholderId, Share) ->
    call({validate, ShareholderId, Share}).

-spec cancel() -> ok.

cancel() ->
    call(cancel).

-spec get_status() -> status().

get_status() ->
    call(get_status).

call(Message) ->
    gen_statem:call(?STATEM, Message).

-spec init(term()) -> {ok, state(), data()}.

init([]) ->
    {ok, uninitialized, #data{}}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) ->
    gen_statem:event_handler_result(state()).

%% Successful workflow events

handle_event({call, From}, {initialize, Threshold, EncryptedKeyring}, uninitialized, Data) ->
    Shareholders = cds_shareholder:get_all(),
    ShareholdersLength = length(Shareholders),
    case (Threshold >= 1) and (ShareholdersLength >= 1) and (Threshold =< ShareholdersLength) of
        true ->
            TimerRef = erlang:start_timer(get_timeout(), self(), lifetime_expired),
            NewData = Data#data{
                encrypted_keyring = EncryptedKeyring,
                threshold = Threshold,
                shareholders = Shareholders,
                timer = TimerRef},
            {next_state,
                confirmation,
                NewData,
                {reply, From, ok}};
        false ->
            {next_state,
                uninitialized,
                #data{},
                {reply, From, {error, invalid_args}}}
    end;
handle_event({call, From}, {confirm, ShareholderId, Share}, confirmation,
    #data{confirmation_shares = Shares, encrypted_keyring = EncryptedKeyring, timer = TimerRef} = Data) ->
    #share{x = X, threshold = Threshold} = cds_keysharing:convert(Share),
    case Shares#{X => {ShareholderId, Share}} of
        AllShares when map_size(AllShares) =:= Threshold ->
            ListShares = cds_keysharing:get_shares(AllShares),
            case confirm_operation(EncryptedKeyring, ListShares) of
                {ok, Keyring} ->
                    NewData = Data#data{confirmation_shares = AllShares, keyring = Keyring},
                    {next_state,
                        postconfirmation,
                        NewData,
                        {reply, From, ok}};
                {error, Error} ->
                    _Time = erlang:cancel_timer(TimerRef),
                    {next_state,
                        uninitialized,
                        #data{},
                        {reply, From, {error, Error}}}
            end;
        Shares1 ->
            NewData = Data#data{confirmation_shares = Shares1},
            {next_state,
                confirmation,
                NewData,
                {reply, From, {ok, {more, Threshold - maps:size(Shares1)}}}}
    end;
handle_event({call, From}, start_validatation, postconfirmation,
    #data{shareholders = Shareholders, threshold = Threshold, keyring = Keyring} = Data) ->
    MasterKey = cds_crypto:key(),
    EncryptedKeyring = cds_keyring:encrypt(MasterKey, Keyring),
    Shares = cds_keysharing:share(MasterKey, Threshold, length(Shareholders)),
    EncryptedShares = cds_keysharing:encrypt_shares_for_shareholders(Shares, Shareholders),
    NewData = Data#data{encrypted_keyring = EncryptedKeyring, keyring = undefined},
    {next_state,
        validation,
        NewData,
        {reply, From, {ok, EncryptedShares}}};
handle_event({call, From}, {validate, ShareholderId, Share}, validation,
    #data{
        shareholders = Shareholders,
        threshold = Threshold,
        validation_shares = Shares,
        encrypted_keyring = EncryptedKeyring,
        timer = TimerRef} = Data) ->
    #share{x = X} = cds_keysharing:convert(Share),
    ShareholdersCount = length(Shareholders),
    case Shares#{X => {ShareholderId, Share}} of
        AllShares when map_size(AllShares) =:= ShareholdersCount ->
            _Time = erlang:cancel_timer(TimerRef),
            ListShares = cds_keysharing:get_shares(AllShares),
            Result = validate_operation(Threshold, ListShares, EncryptedKeyring),
            {next_state,
                uninitialized,
                #data{},
                {reply, From, Result}};
        Shares1 ->
            NewData = Data#data{validation_shares = Shares1},
            {next_state,
                validation,
                NewData,
                {reply, From, {ok, {more, ShareholdersCount - maps:size(Shares1)}}}}
    end;

%% Common events

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [
        {reply, From, State}
    ]};
handle_event({call, From}, get_status, State,
    #data{timer = TimerRef, confirmation_shares = ConfirmationShares, validation_shares = ValidationShares}) ->
    Lifetime = get_lifetime(TimerRef),
    ConfirmationSharesStripped = cds_keysharing:get_id_map(ConfirmationShares),
    ValidationSharesStripped = cds_keysharing:get_id_map(ValidationShares),
    Status = #{
        phase => State,
        lifetime => Lifetime,
        confirmation_shares => ConfirmationSharesStripped,
        validation_shares => ValidationSharesStripped
    },
    {keep_state_and_data, {reply, From, Status}};
handle_event({call, From}, cancel, _State, #data{timer = TimerRef}) ->
    _ = erlang:cancel_timer(TimerRef),
    {next_state, uninitialized, #data{}, {reply, From, ok}};
handle_event(info, {timeout, _TimerRef, lifetime_expired}, _State, _Data) ->
    {next_state, uninitialized, #data{}, []};

%% InvalidActivity events

handle_event({call, From}, _Event, uninitialized, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {rekeying, uninitialized}}}}
    ]};
handle_event({call, From}, _Event, confirmation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {rekeying, confirmation}}}}
    ]};
handle_event({call, From}, _Event, postconfirmation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {rekeying, postconfirmation}}}}
    ]};
handle_event({call, From}, _Event, validation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {rekeying, validation}}}}
    ]}.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    genlib_app:env(cds, keyring_rekeying_lifetime, 3 * 60 * 1000).

-spec get_lifetime(reference() | undefined) -> timer:seconds().

get_lifetime(TimerRef) ->
    case TimerRef of
        undefined ->
            get_timeout() div 1000;
        TimerRef ->
            erlang:read_timer(TimerRef) div 1000
    end.

-spec confirm_operation(encrypted_keyring(), masterkey_shares()) ->
    ok | {error, confirm_errors()}.

confirm_operation(EncryptedOldKeyring, AllShares) ->
    case cds_keysharing:recover(AllShares) of
        {ok, MasterKey} ->
            case cds_keyring:validate_masterkey(MasterKey, EncryptedOldKeyring) of
                {ok, Keyring} ->
                    {ok, Keyring};
                {error, wrong_masterkey} ->
                    {error, {operation_aborted, wrong_masterkey}}
            end;
        {error, failed_to_recover} ->
            {error, {operation_aborted, failed_to_recover}}
    end.

-spec validate_operation(threshold(), masterkey_shares(), encrypted_keyring()) ->
    {ok, {done, encrypted_keyring()}} | {error, validate_errors()}.

validate_operation(Threshold, Shares, EncryptedKeyring) ->
    case cds_keysharing:validate_shares(Threshold, Shares) of
        {ok, MasterKey} ->
            case cds_keyring:decrypt(MasterKey, EncryptedKeyring) of
                {ok, _DecryptedKeyring} ->
                    {ok, {done, EncryptedKeyring}};
                {error, decryption_failed} ->
                    {error, {operation_aborted, failed_to_decrypt_keyring}}
            end;
        {error, Error} ->
            {error, {operation_aborted, Error}}
    end.
