-module(cds_keyring_rotator).
-behavior(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([init/1, handle_call/3, handle_cast/2]).

-export([start_link/0]).
-export([rotate/2]).

-record(state, {
  shares = #{}
}).

-type state() :: #state{}.

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

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
  {stop, normal, term(), state()} | {reply, {ok, {more, integer()}}, state(), integer()}.

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

-spec timeout() -> integer().

timeout() ->
  genlib_app:env(cds, keyring_rotator, timeout, 60000).

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
      case cds_keyring:decrypt(MasterKey, EncryptedOldKeyring) of
        Keyring ->
          {ok, Keyring};
        _NotMatchingKeyring ->
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
    try cds_keyring_storage:update(EncryptedNewKeyring) of
      ok ->
        {ok, NewKeyring}
    catch
      conditional_check_failed ->
        {error, out_of_date}
    end
  catch
    Error ->
      {error, Error}
  end.