-module(cds_keyring_initializator).

-behavior(gen_statem).

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

%%-type shareholder() :: cds_keyring_utils:shareholder().
-type shareholders() :: cds_keyring_utils:shareholders().

-type masterkey() :: binary().

-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: [masterkey_share()].

-type encrypted_master_key_share() :: #{
  id => string(),
  owner => string(),
  encrypted_share =>binary()
}.
-type encrypted_master_key_shares() :: list(encrypted_master_key_share()).

-type data() :: #data{}.

-type encrypted_keyring() :: binary().
-type decrypted_keyring() :: cds_keyring:keyring().

-type state() :: uninitialized | validation.

-type threshold() :: non_neg_integer().

-type validate_errors() :: {operation_aborted,
  non_matching_masterkey | masterkey_failed_to_decrypt | failed_to_recover}.
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
  {ok, {more, integer()}} | {ok, decrypted_keyring()} | {error, validate_errors()}.

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

-spec init(term()) -> {ok, atom(), data()}.

init([]) ->
  {ok, uninitialized, #data{}}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) ->
  gen_statem:event_handler_result(state()).

%% Uninitialized state events

handle_event({call, From}, {validate, _Share}, uninitialized, _Data) ->
  {keep_state_and_data, [
    {reply, From, {error, {invalid_activity, {initialization, uninitialized}}}},
    {state_timeout, timeout(), []}
  ]};
handle_event({call, From}, {initialize, Threshold}, uninitialized, Data) ->
  Shareholders = shareholders(),
  ShareholdersLength = length(Shareholders),
  if
    (Threshold >= 1) and (ShareholdersLength >= 1) and (Threshold =< ShareholdersLength) ->
      MasterKey = cds_crypto:key(),
      Keyring = cds_keyring:new(),
      EncryptedKeyring = cds_keyring:encrypt(MasterKey, Keyring),
      Shares = cds_keysharing:share(MasterKey, Threshold, length(Shareholders)),
      EncryptedShares = encrypt_shares(Shares, Shareholders),
      NewData = Data#data{num = length(EncryptedShares), threshold = Threshold, keyring = EncryptedKeyring},
      {new_state,
        validation,
        NewData,
        [
          {reply, From, {ok, EncryptedShares}},
          {state_timeout, timeout(), []}
        ]};
    true ->
      {new_state,
        unitialized,
        #data{},
        [
          {reply, From, {error, invalid_args}}
        ]}
  end;


%% Validation state events

handle_event({call, From}, {validate, <<_Threshold, X, _Y/binary>> = Share}, validation,
    #data{num = Num, threshold = Threshold, shares = Shares, keyring = Keyring} = Data) ->
  case Shares#{X => Share} of
    AllShares when map_size(AllShares) =:= Num ->
      ListShares = maps:values(AllShares),
      Result = validation(Threshold, ListShares, Keyring),
      {new_state,
        unitiliazied,
        #data{},
        [
          {reply, From, Result}
        ]};
    ExtraShares ->
      NewData = Data#data{shares = ExtraShares},
      {new_state,
        validation,
        NewData,
        [
          {reply, From, {ok, {more, Num - maps:size(ExtraShares)}}},
          {state_timeout, timeout(), []}
        ]}
  end;
handle_event({call, From}, {initialize, _Threshold}, validation, _Data) ->
  {keep_state_and_data, [
    {reply, From, {error, {invalid_activity, {initialization, validation}}}},
    {state_timeout, timeout(), []}
  ]};

%% Common events

handle_event({call, From}, {initialize, Params}, _State, _Data) ->
  handle_event({call, From}, {initialize, Params}, uninitialized, #data{});
handle_event({call, From}, get_state, State, _Data) ->
  {keep_state_and_data, [
    {reply, From, State},
    {state_timeout, timeout(), []}
  ]};
handle_event({call, From}, cancel, _State, _Data) ->
  {new_state, uninitialized, #data{}, {reply, From, ok}};
handle_event(state_timeout, _EventContent, _State, _Data) ->
  {new_state, uninitialized, #data{}}.

-spec timeout() -> non_neg_integer().

timeout() ->
  application:get_env(cds, keyring_initializer_timeout, 60000).

-spec shareholders() -> shareholders().

shareholders() ->
  application:get_env(cds, shareholders, []).

-spec validation(threshold(), masterkey_shares(), encrypted_keyring()) ->
  {ok, decrypted_keyring()} | {error, validate_errors()}.

validation(Threshold, Shares, EncryptedKeyring) ->
  AllSharesCombos = share_combos(Threshold, Shares),
  case restore_and_compare_masterkey(AllSharesCombos) of
    {ok, MasterKey} ->
      case decrypt_keyring(MasterKey, EncryptedKeyring) of
        {ok, DecryptedKeyring} ->
          ok = save_keyring(EncryptedKeyring),
          {ok, DecryptedKeyring};
        {error, Error} ->
          {error, Error}
      end;
    {error, Error} ->
      {error, Error}
  end.

-spec restore_and_compare_masterkey([masterkey_shares()]) ->
  {ok, masterkey()} | last | {error, {operation_aborted, non_matching_masterkey | failed_to_recover}}.

restore_and_compare_masterkey([]) ->
  last;
restore_and_compare_masterkey([ComboOfShares | Shares]) ->
  case cds_keyring_utils:recover_masterkey(ComboOfShares) of
    {ok, MasterKey} ->
      case restore_and_compare_masterkey(Shares) of
        {ok, MasterKey} ->
          {ok, MasterKey};
        last ->
          {ok, MasterKey};
        {ok, _NonMatchingMasterkey} ->
          {error, {operation_aborted, non_matching_masterkey}};
        {error, Error} ->
          {error, Error}
      end;
    {error, failed_to_recover} ->
      {error, {operation_aborted, failed_to_recover}}
  end.

-spec save_keyring(encrypted_keyring()) -> ok.

save_keyring(EncryptedKeyring) ->
  cds_keyring_storage:create(EncryptedKeyring).

-spec decrypt_keyring(masterkey(), encrypted_keyring()) ->
  {ok, decrypted_keyring()} | {error, {operation_aborted, masterkey_failed_to_decrypt}}.

decrypt_keyring(MasterKey, EncryptedKeyring) ->
  try cds_keyring:decrypt(MasterKey, EncryptedKeyring) of
    DecryptedKeyring ->
      {ok, DecryptedKeyring}
  catch
    decryption_failed ->
      {error, {operation_aborted, masterkey_failed_to_decrypt}}
  end.

-spec share_combos(threshold(), masterkey_shares()) -> [masterkey_shares()].

share_combos(1, Shares) -> [[X] || X <- Shares];
share_combos(Threshold, Shares) when Threshold == length(Shares) -> [Shares];
share_combos(Threshold, [Share | Shares]) ->
  [[Share | OtherShares] || OtherShares <- share_combos(Threshold - 1, Shares)].

-spec encrypt_shares(masterkey_shares(), shareholders()) -> encrypted_master_key_shares().

encrypt_shares([], []) ->
  [];
encrypt_shares([Share | Shares], [Shareholder | Shareholders]) ->
  {ok, Id} = maps:find(id, Shareholder),
  {ok, Owner} = maps:find(owner, Shareholder),
  {ok, PublicKey} = maps:find(public_key, Shareholder),
  EncryptedShare = cds_crypto:public_encrypt(PublicKey, Share),
  [#{id => Id, owner => Owner, encrypted_share => EncryptedShare} |
    encrypt_shares(Shares, Shareholders)].