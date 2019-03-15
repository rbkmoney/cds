-module(cds_keyring_rotator).
-behavior(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).

-export([start/0]).
-export([rotate/2]).

-record(state, {
  shares = #{}
}).

-type state() :: #state{}.

-spec start() -> {ok, pid()}.

start() ->
  case gen_server:start({local, ?SERVER}, ?MODULE, [], []) of
    {ok, Pid} ->
      {ok, Pid};
    {error, {already_started, Pid}} ->
      {ok, Pid}
  end.

-spec rotate(term(), term()) -> ok | term().

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
  {stop, normal, term(), state()} | {reply, {ok, {more, integer()}}, state(), non_neg_integer()}.

handle_call({rotate, <<Threshold, X, _Y/binary>> = Share, OldKeyring}, _From, #state{shares = Shares} = StateData) ->
  case Shares#{X => Share} of
    AllShares when map_size(AllShares) =:= Threshold ->
      case recover_masterkey(AllShares) of
        {ok, MasterKey} ->
          case validate_masterkey(MasterKey, OldKeyring) of
            {ok, OldKeyring} ->
              case rotate_keyring(MasterKey, OldKeyring) of
                {ok, NewKeyring} ->
                  {stop, normal, {ok, NewKeyring}, StateData};
                {error, Error} ->
                  {stop, normal, {error, Error}, StateData}
              end;
            {error, Error} ->
              {stop, normal, {error, Error}, StateData}
          end;
        {error, Error} ->
          {stop, normal, {error, Error}, StateData}
      end;
    More ->
      {reply, {ok, {more, Threshold - map_size(More)}}, StateData#state{shares = More}, timeout()}
  end;
handle_call(_Request, _Form, State) ->
  {reply, ignored, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info(timeout, StateData) ->
  {stop, normal, StateData};
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
  application:get_env(cds, keyring_rotator_timeout, 60000).

-spec recover_masterkey(term()) -> {ok, binary()} | {error, atom()}.

recover_masterkey(Shares) ->
  try
    MasterKey = cds_keysharing:recover(maps:values(Shares)),
    {ok, MasterKey}
  catch
    shamir_failed ->
      {error, failed_to_recover}
  end.

-spec validate_masterkey(binary(), term()) -> {ok, term()} | {error, atom()}.

validate_masterkey(MasterKey, Keyring) ->
  try cds_keyring_storage:read() of
    EncryptedOldKeyring ->
      try cds_keyring:decrypt(MasterKey, EncryptedOldKeyring) of
        Keyring ->
          {ok, Keyring};
        _NotMatchingKeyring ->
          {error, wrong_masterkey}
      catch
        decryption_failed ->
          {error, wrong_masterkey}
      end
  catch
    not_found ->
      {error, no_keyring}
  end.

-spec rotate_keyring(binary(), term()) -> {ok, term()} | {error, atom()}.

rotate_keyring(MasterKey, Keyring) ->
  try
    NewKeyring = cds_keyring:rotate(Keyring),
    EncryptedNewKeyring = cds_keyring:encrypt(MasterKey, NewKeyring),
    ok = cds_keyring_storage:update(EncryptedNewKeyring),
    {ok, NewKeyring}
  catch
    Error ->
      {error, Error}
  end.