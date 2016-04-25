-module(cds_keyring).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

-export([current_key/0]).
-export([get_key/1]).
-export([get_keys/0]).
-export([unlock/1]).
-export([lock/0]).
-export([rotate/0]).
%% TODO: -export([rekey/0]).
-export([update/0]).
-export([initialize/2]).
-export([load_keyring/0]).


%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).


-define(SERVER, ?MODULE).
-define(TABLE, ?SERVER).

-record(keyring_element, {
	key_id,
	key,
	current
}).
-type keyring_element() :: #keyring_element{
	key_id :: cds:key_id(),
	key :: cds:key(),
	current :: boolean()
}.

-include_lib("shamir/include/shamir.hrl").
-type shamir_share() :: #share{
	threshold :: integer(),
	x :: integer(),
	y :: binary()
}.

-record(state, {
	keyring = undefined :: undefined | binary(),
	masterkey = undefined :: undefined | binary(),
	shares = [] :: [shamir_share()]
}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec current_key() -> {ok, cds:key_id(), cds:key()} | {error, term()}.
current_key() ->
	try ets:match_object(?TABLE, #keyring_element{current = true, _ = '_'}) of
		[#keyring_element{key_id = KeyId, key = Key}] ->
			{ok, KeyId, Key}
	catch error:badarg ->
		{error, locked}
	end.

-spec get_key(cds:key_id()) -> {ok, cds:key()} | {error, term()}.
get_key(KeyId) ->
	try ets:lookup(?TABLE, KeyId) of
		[#keyring_element{key_id = KeyId, key = Key}] ->
			{ok, Key};
		[] ->
			{error, no_key}
	catch error:badarg ->
		{error, locked}
	end.

-spec get_keys() -> {ok, [{cds:key_id(), cds:key()}]} | {error, term()}.
get_keys() ->
	try ets:tab2list(?TABLE) of
		[] ->
			{error, no_keys};
		Keyring ->
			{ok, [{KeyId, Key} || #keyring_element{key_id = KeyId, key = Key} <- Keyring]}
	catch error:badarg ->
		{error, locked}
	end.

-spec unlock(binary()) -> {more, integer()} | ok | {error, term()}.
unlock(Share) ->
	case shamir(Share) of
		unlocked ->
			decrypt_keyring();
		Otherwise ->
			Otherwise
	end.

-spec lock() -> ok | {error, term()}.
lock() ->
	gen_server:call(?SERVER, lock).

-spec rotate() -> ok | {error, term()}.
rotate() ->
	try cds_keyring_storage:lock() of
		ok ->
			ok = update(),
			ok = safe_rotate();
		{error, Reason} ->
			{error, Reason}
	after
		cds_keyring_storage:unlock()
	end.

-spec safe_rotate() -> ok | {error, term()}.
safe_rotate() ->
	case gen_server:call(?SERVER, rotate) of
		ok ->
			update();
		{error, Reason} ->
			{error, Reason}
	end.

-spec update() -> ok | {error, term()}.
update() ->
	case load_keyring() of
		ok ->
			decrypt_keyring();
		{error, Reason} ->
			{error, Reason}
	end.

-spec shamir(binary()) -> {more, integer()} | unlocked | {error, term()}.
shamir(Share) ->
	gen_server:call(?SERVER, {shamir, unmarshall_share(Share)}).

-spec load_keyring() -> ok | {error, term()}.
load_keyring() ->
	gen_server:call(?SERVER, load_keyring).

-spec decrypt_keyring() -> ok | {error, term()}.
decrypt_keyring() ->
	gen_server:call(?SERVER, decrypt_keyring).


%% todo: accept list of pgp public keys to encode each shamir share
-spec initialize(integer(), integer()) -> {ok, [shamir_share()]} | {error, term()}.
initialize(Threshold, NumParts) when Threshold =< NumParts ->
	try cds_keyring_storage:lock() of
		ok ->
			safe_initialize(NumParts, Threshold);
		{error, Reason} ->
			{error, Reason}
	after
		cds_keyring_storage:unlock()
	end.

-spec safe_initialize(integer(), integer()) -> {ok, [shamir_share()]} | {error, term()}.
safe_initialize(Threshold, NumParts) ->
	Keyring = keyring(),
	MasterKey = cds:key(),
	Encrypted = encrypt(MasterKey, Keyring),
	case cds_keyring_storage:put(Encrypted) of
		ok ->
			Shares = shamir:share(MasterKey, Threshold, NumParts),
			{ok, marshall_shares(Shares)};
		{error, Reason} ->
			{error, Reason}
	end.

%% todo: rekey


%% gen_server.

init([]) ->
	{ok, Keyring} = cds_keyring_storage:get(),
	{ok, #state{keyring = Keyring}}.

handle_call(load_keyring, _From, State) ->
	case cds_keyring_storage:get() of
		{ok, Keyring} ->
			{reply, ok, State#state{keyring = Keyring}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call(decrypt_keyring, _From, #state{keyring = undefined} = State) ->
	{reply, {error, keyring_not_loaded}, State};
handle_call(decrypt_keyring, _From, #state{masterkey = undefined} = State) ->
	{reply, {error, no_masterkey}, State};
handle_call(decrypt_keyring, _From, #state{keyring = Encrypted, masterkey = MasterKey} = State) ->
	case decrypt(MasterKey, Encrypted) of
		{ok, Keyring} ->
			ok = update_keyring_table(Keyring),
			{reply, ok, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call({shamir, #share{threshold = T} = S}, _From, #state{shares = Ss} = State) when length(Ss) + 1 < T ->
	{reply, {more, T - length(Ss) - 1}, State#state{shares = [S | Ss]}};
handle_call({shamir, #share{threshold = T} = S}, _From, #state{shares = Ss} = State) when length(Ss) + 1 >= T ->
	try shamir:recover([S | Ss]) of
		MasterKey ->
			{reply, unlocked, State#state{masterkey = MasterKey, shares = []}}
	catch _Error ->
			{reply, {error, recovery_failed}, State#state{shares = []}}
	end;

handle_call(rotate, _From, #state{masterkey = MasterKey} = State) ->
	{ok, NewKeyring} = rotate_keyring(ets:tab2list(?TABLE)),
	Encrypted = encrypt(MasterKey, NewKeyring),
	case cds_keyring_storage:put(Encrypted) of
		ok ->
			{reply, ok, State};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call(lock, _From, State) ->
	ok = remove_keyring_table(),
	{reply, ok, State#state{masterkey = undefined}};


handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% internal

-spec rotate_keyring([keyring_element()]) -> ok | {error, term()}.
rotate_keyring(Keyring) ->
	NextId = case  find_current(Keyring) of
		#keyring_element{key_id = CurrentId} ->
			rotate_key_id(CurrentId)
	end,
	case lists:keyfind(NextId, #keyring_element.key_id, Keyring) of
		false ->
			{ok, [keyring_element(NextId) | uncurrent(Keyring)]};
		#keyring_element{} ->
			{error, keyring_full}
	end.


-spec update_keyring_table([keyring_element()]) -> ok.
update_keyring_table(Keyring) ->
	case lists:member(?TABLE, ets:all()) of
		false ->
			?TABLE = ets:new(?TABLE, [named_table, {keypos, #keyring_element.key_id}, {read_concurrency, true}]);
		true ->
			true = ets:delete_all_objects(?TABLE)
	end,
	case ets:insert(?TABLE, Keyring) of
		true ->
			ok
	end.

-spec remove_keyring_table() -> ok.
remove_keyring_table() ->
	case lists:member(?TABLE, ets:all()) of
		false ->
			ok;
		true ->
			true = ets:delete(?TABLE),
			ok
	end.


%% keyring utils

-spec keyring() -> [keyring_element()].
keyring() ->
	[keyring_element(0)].

-spec keyring_element(cds:key_id()) -> keyring_element().
keyring_element(KeyId) ->
	#keyring_element{key_id = KeyId, key = cds:key(), current = true}.

rotate_key_id(KeyId) when KeyId < 255 ->
	KeyId + 1;
rotate_key_id(KeyId) when KeyId =:= 255 ->
	0.

-spec encrypt(cds:key(), [keyring_element()]) -> binary().
encrypt(Key, Keyring) ->
	cds:encrypt(Key, marshall_keyring(Keyring)).

-spec decrypt(cds:key(), binary()) -> {ok, [keyring_element()]} | error.
decrypt(Key, Keyring) ->
	case cds:decrypt(Key, Keyring) of
		{ok, Decrypted} ->
			{ok, unmarshall_keyring(Decrypted)};
		{error, Reason} ->
			{error, Reason}
	end.

find_current(Keyring) ->
	case lists:filter(fun is_current/1, Keyring) of
		[Current] ->
			Current
	end.

is_current(#keyring_element{current = Current}) when is_boolean(Current) ->
	Current.


uncurrent(Keyring) ->
	lists:map(fun unflag_current/1, Keyring).

unflag_current(#keyring_element{current = true} = KeyringElement) ->
	KeyringElement#keyring_element{current = false};
unflag_current(KeyringElement) ->
	KeyringElement.


%% marshalling

marshall_keyring(Keyring) ->
	marshall_keyring(Keyring, <<>>).

marshall_keyring([], Acc) ->
	Acc;
marshall_keyring([#keyring_element{key_id = KeyId, key = Key, current = true} | Rest], Acc) when size(Key) =:= 32 ->
	marshall_keyring(Rest, <<Acc/binary, KeyId, 1, Key/binary>>);
marshall_keyring([#keyring_element{key_id = KeyId, key = Key, current = false} | Rest], Acc) when size(Key) =:= 32 ->
	marshall_keyring(Rest, <<Acc/binary, KeyId, 0, Key/binary>>).

unmarshall_keyring(Keyring) ->
	unmarshall_keyring(Keyring, []).

unmarshall_keyring(<<>>, Acc) ->
	Acc;
unmarshall_keyring(<<KeyId, 1, Key:32/binary, Rest/binary>>, Acc) ->
	unmarshall_keyring(Rest, [#keyring_element{key_id = KeyId, key = Key, current = true} | Acc]);
unmarshall_keyring(<<KeyId, 0, Key:32/binary, Rest/binary>>, Acc) ->
	unmarshall_keyring(Rest, [#keyring_element{key_id = KeyId, key = Key, current = false} | Acc]).


marshall_share(#share{threshold = Threshold, x = X, y = Y}) ->
	<<Threshold, X, Y/binary>>.

unmarshall_share(<<Threshold, X, Y/binary>>) ->
	#share{threshold = Threshold, x = X, y = Y}.

marshall_shares(Shares) ->
	[marshall_share(Share) || Share <- Shares].

