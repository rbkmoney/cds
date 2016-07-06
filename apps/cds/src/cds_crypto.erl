-module(cds_crypto).

-export([key/0]).
-export([encrypt/2]).
-export([decrypt/2]).

-export_type([key/0]).

-type key() :: binary().

%%internal

-type iv() :: binary().
-type tag() :: binary().
-type aad() :: binary().

%% cedf is for CDS Encrypted Data Format
-record(cedf, {
    tag,
    iv,
    aad,
    cipher
}).
-type cedf() :: #cedf{
    tag :: tag(),
    iv :: iv(),
    aad :: aad(),
    cipher :: binary()
}.

%% interface

-spec key() -> key().
key() ->
    crypto:strong_rand_bytes(32).

-spec encrypt(key(), binary()) -> binary().
encrypt(Key, Plain) ->
    IV = iv(),
    AAD = aad(),
    try
        {Cipher, Tag} = crypto:block_encrypt(aes_gcm, Key, IV, {AAD, Plain}),
        marshall_cedf(#cedf{iv = IV, aad = AAD, cipher = Cipher, tag = Tag})
    catch Class:Reason ->
        _ = lager:error("encryption failed with ~p ~p", [Class, Reason]),
        throw(encryption_failed)
    end.

-spec decrypt(key(), binary()) -> binary().
decrypt(Key, MarshalledCEDF) ->
    try
        #cedf{iv = IV, aad = AAD, cipher = Cipher, tag = Tag} = unmarshall_cedf(MarshalledCEDF),
         crypto:block_decrypt(aes_gcm, Key, IV, {AAD, Cipher, Tag})
    of
        error ->
            throw(decryption_failed);
        Plain ->
            Plain
    catch Type:Error ->
        _ = lager:error("decryption failed with ~p ~p", [Type, Error]),
        throw(decryption_failed)
    end.

%% internal

-spec iv() -> iv().
iv() ->
    crypto:strong_rand_bytes(16).

-spec aad() -> aad().
aad() ->
    crypto:strong_rand_bytes(4).


-spec marshall_cedf(cedf()) -> binary().
marshall_cedf(#cedf{tag = Tag, iv = IV, aad = AAD, cipher = Cipher})
    when
        bit_size(Tag) =:= 128,
        bit_size(IV) =:= 128,
        bit_size(AAD) =:= 32
    ->
        <<Tag:16/binary, IV:16/binary, AAD:4/binary, Cipher/binary>>.

-spec unmarshall_cedf(binary()) -> cedf().
unmarshall_cedf(<<Tag:16/binary, IV:16/binary, AAD:4/binary, Cipher/binary>>) ->
    #cedf{tag = Tag, iv = IV, aad = AAD, cipher = Cipher}.
