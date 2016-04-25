-module(cds).

-export([start/0]).

-export([encrypt/1, encrypt/2]).
-export([decrypt/1, decrypt/2]).
-export([hash/1]).
-export([all_hashes/1]).
-export([key/0]).
-export([token/0]).

-export_type([key/0, key_id/0, hash/0, token/0, ccd/0]).

-type key_id() :: byte().
-type key() :: <<_:256>>.
-type iv() :: <<_:128>>.
-type token() :: <<_:128>>.
-type hash() :: <<_:136>>.
-type ccd() :: binary().

%% ecd for encrypted cardholder data
-record(ecd, {
	key_id,
	cipher,
	tag,
	iv,
	aad
}).
-type ecd() :: #ecd{
	key_id :: key_id(),
	cipher :: binary(),
	tag :: binary(),
	iv :: iv(),
	aad :: binary()
}.

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

start() ->
	application:ensure_all_started(cds).

-spec key() -> key().
key() ->
	crypto:strong_rand_bytes(32).

-spec iv() -> iv().
iv() ->
	crypto:strong_rand_bytes(16).

-spec aad() -> binary().
aad() ->
	crypto:strong_rand_bytes(4).

-spec token() -> token().
token() ->
	crypto:strong_rand_bytes(16).

-spec encrypt(binary()) -> binary().
encrypt(Plain) ->
	{ok, KeyId, Key} = cds_keyring:current_key(),
	encrypt(KeyId, Key, Plain).

-spec encrypt(key(), binary()) -> binary().
encrypt(Key, Plain) ->
	encrypt(0, Key, Plain).

-spec encrypt(key_id(), key(), binary()) -> binary().
encrypt(KeyId, Key, Plain) ->
	IV = iv(),
	AAD = aad(),
	{Cipher, Tag} = crypto:block_encrypt(aes_gcm, Key, IV, {AAD, Plain}),
	marshall_ecd(#ecd{key_id = KeyId, cipher = Cipher, tag = Tag, iv = IV, aad = AAD}).

-spec decrypt(binary()) -> {ok, binary()} | {error, term()}.
decrypt(MarshalledECD) ->
	#ecd{key_id = KeyId}  = unmarshall_ecd(MarshalledECD),
	{ok, Key} = cds_keyring:get_key(KeyId),
	decrypt(Key, MarshalledECD).

-spec decrypt(key(), binary()) -> {ok, binary()} | {error, term()}.
decrypt(Key, MarshalledECD) ->
	#ecd{iv = IV, aad = AAD, cipher = Cipher, tag = Tag} = unmarshall_ecd(MarshalledECD),
	case crypto:block_decrypt(aes_gcm, Key, IV, {AAD, Cipher, Tag}) of
		error ->
			{error, decryption_failed};
		Data ->
			{ok, Data}
	end.

-spec hash(binary()) -> hash().
hash(Plain) ->
	{ok, KeyId, Key} = cds_keyring:current_key(),
	hash(Plain, KeyId, Key).

-spec hash(binary(), key_id(), key()) -> hash().
hash(Plain, KeyId, Key) ->
	Hash = cds_hash:hash(Plain, Key),
	<<KeyId:8, Hash/binary>>.

-spec all_hashes(binary()) -> {ok, [hash()]} | {error, term()}.
all_hashes(Plain) ->
	case cds_keyring:get_keys() of
		{ok, Keys} ->
			{ok, [hash(Plain, KeyId, Key) || {KeyId, Key} <- Keys]};
		{error, Reason} ->
			{error, Reason}
	end.

-spec marshall_ecd(ecd()) -> binary().
marshall_ecd(#ecd{key_id = KeyId, cipher = Cipher, tag = Tag, iv = IV, aad = AAD}) ->
	LengthPrefixed = length_prefix_marshall([Cipher, Tag, IV, AAD]),
	<<KeyId:8, LengthPrefixed/binary>>.

-spec unmarshall_ecd(binary()) -> ecd().
unmarshall_ecd(<<KeyId:8, LengthPrefixed/binary>>) ->
	[Cipher, Tag, IV, AAD] = length_prefix_unmarshall(LengthPrefixed),
	#ecd{key_id = KeyId, cipher = Cipher, tag = Tag, iv = IV, aad = AAD}.

-spec length_prefix_marshall([binary()]) -> binary().
length_prefix_marshall(Data) ->
	length_prefix_marshall(Data, <<>>).

length_prefix_marshall([], Acc) ->
	Acc;
length_prefix_marshall([Data | Rest], Acc) ->
	DataSize = size(Data),
	length_prefix_marshall(Rest, <<Acc/binary, DataSize:16, Data/binary>>).

-spec length_prefix_unmarshall(binary()) -> [binary()].
length_prefix_unmarshall(Data) ->
	lists:reverse(length_prefix_unmarshall(Data, [])).

length_prefix_unmarshall(<<>>, Acc) ->
	Acc;
length_prefix_unmarshall(<<DataSize:16, Data:DataSize/binary, Rest/binary>>, Acc) ->
	length_prefix_unmarshall(Rest, [Data | Acc]).


-ifdef(TEST).

prop_encryption() ->
	?FORALL({KeyId, Key, Data}, {byte(), binary(32), binary()}, ok =:= encryption_cycle(KeyId, Key, Data)).

encryption_cycle(KeyId, Key, Data) ->
	meck:new(cds_keyring),
	meck:expect(cds_keyring, current_key, fun() -> {ok, KeyId, Key} end),
	meck:expect(cds_keyring, get_key, fun(Id) when Id =:= KeyId -> {ok, Key} end),
	Encrypted = encrypt(Data),
	{ok, Data} = decrypt(Encrypted),
	meck:unload(cds_keyring),
	ok.

encryption_test() ->
	true = proper:quickcheck(prop_encryption()).


prop_hashing() ->
	?FORALL({KeyId, Key, Data}, {byte(), binary(32), binary()}, ok =:= hashing_cycle(KeyId, Key, Data)).

hashing_cycle(KeyId, Key, Data) ->
	meck:new(cds_keyring),
	meck:expect(cds_keyring, current_key, fun() -> {ok, KeyId, Key} end),
	meck:new(cds_hash),
	meck:expect(cds_hash, hash, fun(Plain, Salt) when Salt =:= Key, Plain =:= Data -> <<"hash_stub">> end),
	<<KeyId, "hash_stub">> = hash(Data),
	meck:unload(cds_keyring),
	meck:unload(cds_hash),
	ok.

hashing_test() ->
	true = proper:quickcheck(prop_hashing()).

-endif.
