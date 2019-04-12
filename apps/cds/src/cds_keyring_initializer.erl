-module(cds_keyring_initializer).

-behavior(gen_statem).

-include_lib("shamir/include/shamir.hrl").

%% API
-export([init/1, callback_mode/0]).
-export([start_link/0]).
-export([initialize/1]).
-export([validate/1]).
-export([get_state/0]).
-export([cancel/0]).
-export([handle_event/4]).
-export_type([encrypted_master_key_shares/0]).

-define(STATEM, ?MODULE).

-record(data, {
    num,
    threshold,
    keyring,
    shares = #{}
}).

-type shareholder() :: cds_shareholder:shareholder().
-type shareholders() :: cds_shareholder:shareholders().

-type masterkey() :: cds_keysharing:masterkey().

-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: [masterkey_share()].

-type encrypted_master_key_share() :: cds_keysharing:encrypted_master_key_share().
-type encrypted_master_key_shares() :: cds_keysharing:encrypted_master_key_shares().

-type data() :: #data{}.

-type encrypted_keyring() :: cds_keyring:encrypted_keyring().
-type decrypted_keyring() :: cds_keyring:keyring().

-type state() :: uninitialized | validation.

-type threshold() :: non_neg_integer().

-type validate_errors() :: {operation_aborted,
    non_matching_masterkey | failed_to_decrypt_keyring | failed_to_recover}.
-type initialize_errors() :: invalid_args.

-spec callback_mode() -> handle_event_function.

callback_mode() -> handle_event_function.

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_statem:start_link({local, ?STATEM}, ?MODULE, [], []).

-spec initialize(threshold()) ->
    {ok, encrypted_master_key_shares()} | {error, initialize_errors()}.

initialize(Threshold) ->
    call({initialize, Threshold}).

-spec validate(masterkey_share()) ->
    {ok, {more, integer()}} | {ok, {done, {encrypted_keyring(), decrypted_keyring()}}} | {error, validate_errors()}.

validate(Share) ->
    call({validate, Share}).

-spec cancel() -> ok.

cancel() ->
    call(cancel).

-spec get_state() -> atom().

get_state() ->
    call(get_state).

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
            EncryptedShares = encrypt_shares(Shares, Shareholders),
            NewData = Data#data{num = length(EncryptedShares), threshold = Threshold, keyring = EncryptedKeyring},
            {next_state,
                validation,
                NewData,
                [
                    {reply, From, {ok, EncryptedShares}},
                    {{timeout, lifetime}, get_timeout(), expired}
                ]};
        false ->
            {next_state,
                uninitialized,
                #data{},
                [
                    {reply, From, {error, invalid_args}}
                ]}
    end;
handle_event({call, From}, {validate, Share}, validation,
    #data{num = Num, threshold = Threshold, shares = Shares, keyring = Keyring} = Data) ->
    #share{x = X} = cds_keysharing:convert(Share),
    case Shares#{X => Share} of
        AllShares when map_size(AllShares) =:= Num ->
            ListShares = maps:values(AllShares),
            Result = validate(Threshold, ListShares, Keyring),
            {next_state,
                uninitialized,
                #data{},
                [
                    {reply, From, Result},
                    {{timeout, lifetime}, infinity, []}
                ]};
        Shares1 ->
            NewData = Data#data{shares = Shares1},
            {next_state,
                validation,
                NewData,
                [
                    {reply, From, {ok, {more, Num - maps:size(Shares1)}}}
                ]}
    end;

%% InvalidActivity events

handle_event({call, From}, {validate, _Share}, uninitialized, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {initialization, uninitialized}}}}
    ]};
handle_event({call, From}, {initialize, _Threshold}, validation, _Data) ->
    {keep_state_and_data, [
        {reply, From, {error, {invalid_activity, {initialization, validation}}}}
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
    {next_state, uninitialized, #data{}, [{{timeout, lifetime}, infinity, []}]}.

-spec get_timeout() -> non_neg_integer().

get_timeout() ->
    genlib_app:env(cds, keyring_initialize_lifetime, 3 * 60 * 1000).

-spec validate(threshold(), masterkey_shares(), encrypted_keyring()) ->
    {ok, {done, {encrypted_keyring(), decrypted_keyring()}}} | {error, validate_errors()}.

validate(Threshold, Shares, EncryptedKeyring) ->
    AllSharesCombos = lib_combin:cnr(Threshold, Shares),
    case restore_and_compare_masterkey(AllSharesCombos) of
        {ok, MasterKey} ->
            case decrypt_keyring(MasterKey, EncryptedKeyring) of
                {ok, DecryptedKeyring} ->
                    {ok, {done, {EncryptedKeyring, DecryptedKeyring}}};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec restore_and_compare_masterkey([masterkey_shares(), ...]) ->
    {ok, masterkey()} | {error, {operation_aborted, non_matching_masterkey | failed_to_recover}}.

restore_and_compare_masterkey([FirstCombo | CombosOfShares]) ->
    lists:foldl(
        fun
            (ComboOfShares, {ok, MasterKey}) ->
                case cds_keysharing:recover(ComboOfShares) of
                    {ok, MasterKey} ->
                        {ok, MasterKey};
                    {ok, _NonMatchingMasterkey} ->
                        {error, {operation_aborted, non_matching_masterkey}};
                    {error, failed_to_recover} ->
                        {error, {operation_aborted, failed_to_recover}}
                end;
            (_ComboOfShares, Error) ->
                Error
        end,
        cds_keysharing:recover(FirstCombo),
        CombosOfShares
    ).

-spec decrypt_keyring(masterkey(), encrypted_keyring()) ->
    {ok, decrypted_keyring()} | {error, {operation_aborted, failed_to_decrypt_keyring}}.

decrypt_keyring(MasterKey, EncryptedKeyring) ->
    try cds_keyring:decrypt(MasterKey, EncryptedKeyring) of
        DecryptedKeyring ->
            {ok, DecryptedKeyring}
    catch
        decryption_failed ->
            {error, {operation_aborted, failed_to_decrypt_keyring}}
    end.

-spec encrypt_shares(masterkey_shares(), shareholders()) -> encrypted_master_key_shares().

encrypt_shares(Shares, Shareholders) ->
    lists:map(fun encrypt_share/1, lists:zip(Shares, Shareholders)).

-spec encrypt_share({masterkey_share(), shareholder()}) -> encrypted_master_key_share().

encrypt_share({Share, #{id := Id, owner := Owner} = Shareholder}) ->
    PublicKey = cds_shareholder:get_public_key(Shareholder, enc),
    #{
        id => Id,
        owner => Owner,
        encrypted_share => cds_crypto:public_encrypt(PublicKey, Share)
    }.
