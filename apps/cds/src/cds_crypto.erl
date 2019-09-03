-module(cds_crypto).

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jws.hrl").

-export([encrypt/2]).
-export([decrypt/2]).
-export([private_decrypt/2]).
-export([sign/2]).

-type key() :: <<_:256>>.
-type iv()  :: binary().
-type tag() :: binary().
-type aad() :: binary().
-type jwk_encoded() :: binary().
-type jwe_compacted() :: binary().

%% cedf is for CDS Encrypted Data Format
-record(cedf, {
    tag    :: tag(),
    iv     :: iv(),
    aad    :: aad(),
    cipher :: binary()
}).
-type cedf() :: #cedf{}.

%%% API

-spec encrypt(key(), binary()) -> binary().
encrypt(Key, Plain) ->
    IV = iv(),
    AAD = aad(),
    try
        {Cipher, Tag} = crypto:block_encrypt(aes_gcm, Key, IV, {AAD, Plain}),
        marshall_cedf(#cedf{iv = IV, aad = AAD, cipher = Cipher, tag = Tag})
    catch Class:Reason ->
        _ = logger:error("encryption failed with ~p ~p", [Class, Reason]),
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
        _ = logger:error("decryption failed with ~p ~p", [Type, Error]),
        throw(decryption_failed)
    end.

-spec private_decrypt(jwk_encoded(), jwe_compacted()) -> binary().
private_decrypt(PrivateKey, JWECompacted) ->
    private_decrypt(PrivateKey, <<"">>, JWECompacted).

-spec private_decrypt(jwk_encoded(), binary(), jwe_compacted()) -> binary().
private_decrypt(PrivateKey, Password, JWECompacted) ->
    {_Module, JWKPrivateKey} = jose_jwk:from(Password, PrivateKey),
    {#{}, JWEPlain} = jose_jwe:expand(JWECompacted),
    {Result, _JWE} = jose_jwk:block_decrypt(JWEPlain, JWKPrivateKey),
    Result.

-spec sign(jwk_encoded(), binary()) -> binary().
sign(PrivateKey, Plain) ->
    JWKPrivateKey = jose_jwk:from(PrivateKey),
    SignerWithoutKid = jose_jwk:signer(JWKPrivateKey),
    Signer = SignerWithoutKid#{<<"kid">> => JWKPrivateKey#jose_jwk.fields},
    {_JWKModule, SignedPlain} = jose_jwk:sign(Plain, Signer, JWKPrivateKey),
    {_JWSModule, SignedCompacted} = jose_jws:compact(SignedPlain),
    SignedCompacted.

%%% Internal functions

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
