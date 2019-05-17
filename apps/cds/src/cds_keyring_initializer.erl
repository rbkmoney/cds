-module(cds_keyring_initializer).

-behavior(gen_statem).

-include_lib("shamir/include/shamir.hrl").

%% API
-export([init/1, callback_mode/0]).
-export([start_link/0]).
-export([initialize/1]).
-export([validate/2]).
-export([get_status/0]).
-export([cancel/0]).
-export([handle_event/4]).
-export_type([encrypted_master_key_shares/0]).
-export_type([status/0]).
-export_type([state/0]).

-define(STATEM, ?MODULE).

-record(data, {
    num,
    threshold,
    keyring,
    shares = #{},
    timer
}).

-type shareholder_id() :: cds_shareholder:shareholder_id().

-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: [masterkey_share()].

-type encrypted_master_key_shares() :: cds_keysharing:encrypted_master_key_shares().

-type data() :: #data{}.
-type seconds() :: non_neg_integer().
-type status() :: #{
    phase => state(),
    lifetime => seconds() | undefined,
    validation_shares => #{cds_keysharing:share_id() => shareholder_id()}
}.

-type encrypted_keyring() :: cds_keyring:encrypted_keyring().
-type decrypted_keyring() :: cds_keyring:keyring().

-type state() :: uninitialized | validation.

-type threshold() :: cds_keysharing:threshold().

-type validate_errors() :: {operation_aborted,
    non_matching_masterkey | failed_to_decrypt_keyring | failed_to_recover}.
-type initialize_errors() :: invalid_args.
-type invalid_activity() :: {error, {invalid_activity, {initialization, state()}}}.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(threshold()) ->
    {ok, encrypted_master_key_shares()} | {error, initialize_errors()} | invalid_activity().

initialize(Threshold) ->
    call({initialize, Threshold}).

-spec validate(shareholder_id(), masterkey_share()) ->
    {ok, {more, integer()}} |
    {ok, {done, {encrypted_keyring(), decrypted_keyring()}}} |
    {error, validate_errors()} | invalid_activity().

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

handle_event({call, From}, {initialize, Threshold}, uninitialized, Data) ->
    Shareholders = cds_shareholder:get_all(),
    ShareholdersLength = length(Shareholders),
    case (Threshold >= 1) and (ShareholdersLength >= 1) and (Threshold =< ShareholdersLength) of
        true ->
            MasterKey = cds_crypto:key(),
            Keyring = cds_keyring:new(),
            EncryptedKeyring = cds_keyring:encrypt(MasterKey, Keyring),
            Shares = cds_keysharing:share(MasterKey, Threshold, length(Shareholders)),
            EncryptedShares = cds_keysharing:encrypt_shares_for_shareholders(Shares, Shareholders),
            TimerRef = erlang:start_timer(get_timeout(), self(), lifetime_expired),
            NewData = Data#data{
                num = length(EncryptedShares),
                threshold = Threshold,
                keyring = EncryptedKeyring,
                timer = TimerRef},
            {next_state,
                validation,
                NewData,
                {reply, From, {ok, EncryptedShares}}};
        false ->
            {next_state, uninitialized, #data{}, {reply, From, {error, invalid_args}}}
    end;
handle_event({call, From}, {validate, ShareholderId, Share}, validation,
    #data{num = Num, threshold = Threshold, shares = Shares, keyring = Keyring, timer = TimerRef} = Data) ->
    #share{x = X} = cds_keysharing:convert(Share),
    case Shares#{X => {ShareholderId, Share}} of
        AllShares when map_size(AllShares) =:= Num ->
            _ = erlang:cancel_timer(TimerRef),
            ListShares = cds_keysharing:get_shares(AllShares),
            Result = validate(Threshold, ListShares, Keyring),
            {next_state,
                uninitialized,
                #data{},
                {reply, From, Result}};
        Shares1 ->
            NewData = Data#data{shares = Shares1},
            {next_state,
                validation,
                NewData,
                {reply, From, {ok, {more, Num - maps:size(Shares1)}}}}
    end;

%% Common events

handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [
        {reply, From, State}
    ]};
handle_event({call, From}, get_status, State, #data{timer = TimerRef, shares = ValidationShares}) ->
    Lifetime = get_lifetime(TimerRef),
    ValidationSharesStripped = cds_keysharing:get_id_map(ValidationShares),
    Status = #{
        phase => State,
        lifetime => Lifetime,
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
        {reply, From, {error, {invalid_activity, {initialization, uninitialized}}}}
    ]};
handle_event({call, From}, _Event, validation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {initialization, validation}}}}
    ]}.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    genlib_app:env(cds, keyring_initialize_lifetime, 3 * 60 * 1000).

-spec get_lifetime(reference() | undefined) -> seconds() | undefined.

get_lifetime(TimerRef) ->
    case TimerRef of
        undefined ->
            undefined;
        TimerRef ->
            erlang:read_timer(TimerRef) div 1000
    end.

-spec validate(threshold(), masterkey_shares(), encrypted_keyring()) ->
    {ok, {done, {encrypted_keyring(), decrypted_keyring()}}} | {error, validate_errors()}.

validate(Threshold, Shares, EncryptedKeyring) ->
    case cds_keysharing:validate_shares(Threshold, Shares) of
        {ok, MasterKey} ->
            case cds_keyring:decrypt(MasterKey, EncryptedKeyring) of
                {ok, DecryptedKeyring} ->
                    {ok, {done, {EncryptedKeyring, DecryptedKeyring}}};
                {error, decryption_failed} ->
                    {error, {operation_aborted, failed_to_decrypt_keyring}}
            end;
        {error, Error} ->
            {error, {operation_aborted, Error}}
    end.
