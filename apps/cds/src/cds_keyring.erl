-module(cds_keyring).

-export([new/0]).
-export([rotate/1]).
-export([encrypt/2]).
-export([decrypt/2]).
-export([marshall/1]).
-export([unmarshall/1]).

-export_type([key_id/0]).

-type key_id() :: byte().

-type keyring() :: #{
    current_key => key_id(),
    keys => #{key_id() => cds_crypto:key()}
}.

-define(KEY_BYTESIZE, 32).

-spec new() -> keyring().
new() ->
    #{current_key => 0, keys => #{0 => cds_crypto:key()}}.


-spec rotate(keyring()) -> keyring().
rotate(#{current_key := CurrentKeyId, keys := Keys}) ->
    <<NewCurrentKeyId>> = <<(CurrentKeyId + 1)>>,
    case maps:is_key(NewCurrentKeyId, Keys) of
        false ->
            #{current_key => NewCurrentKeyId, keys => Keys#{NewCurrentKeyId => cds_crypto:key()}};
        true ->
            throw(keyring_full)
    end.

-spec encrypt(cds_crypto:key(), keyring()) -> binary().
encrypt(MasterKey, Keyring) ->
    cds_crypto:encrypt(MasterKey, marshall(Keyring)).

-spec decrypt(cds_crypto:key(), binary()) -> keyring().
decrypt(MasterKey, EncryptedKeyring) ->
    unmarshall(cds_crypto:decrypt(MasterKey, EncryptedKeyring)).

-spec marshall(keyring()) -> binary().
marshall(#{current_key := CurrentKey, keys := Keys}) ->
    <<CurrentKey, (maps:fold(fun marshall_keys/3, <<>>, Keys))/binary>>.

-spec unmarshall(binary()) -> keyring().
unmarshall(<<CurrentKey, Keys/binary>>) ->
    #{current_key => CurrentKey, keys => unmarshall_keys(Keys, #{})}.

-spec marshall_keys(key_id(), cds_crypto:key(), binary()) -> binary().
marshall_keys(KeyId, Key, Acc) ->
    <<Acc/binary, KeyId, Key:?KEY_BYTESIZE/binary>>.

-spec unmarshall_keys(binary(), map()) -> map().
unmarshall_keys(<<>>, Acc) ->
    Acc;
unmarshall_keys(<<KeyId, Key:?KEY_BYTESIZE/binary, Rest/binary>>, Acc) ->
    unmarshall_keys(Rest, Acc#{KeyId => Key}).

