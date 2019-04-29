-module(cds_crypto).

-include_lib("jose/include/jose_jwk.hrl").
-include_lib("jose/include/jose_jws.hrl").

-export([key/0]).
-export([encrypt/2]).
-export([public_encrypt/2]).
-export([decrypt/2]).
-export([private_decrypt/2]).
-export([private_decrypt/3]).
-export([verify/2]).
-export([sign/2]).
-export([generate_keypair/1]).

-export_type([jwk_encoded/0]).
-export_type([jwe_compacted/0]).

%%internal

-type key() :: <<_:256>>.
-type iv()  :: binary().
-type tag() :: binary().
-type aad() :: binary().
-type jwk_encoded() :: binary().
-type jwk_map() :: map().
-type jwe_compacted() :: binary().
-type jws_compacted() :: binary().

-type jwk_generation_params() :: term().
-type jwk_pub_binary() :: binary().
-type jwk_priv_binary() :: binary().
-type json_object_key() :: binary() | atom() | integer().

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

-spec public_encrypt(jwk_map(), binary()) -> jwe_compacted().
public_encrypt(PublicKey, Plain) ->
    JWKPublicKey = jose_jwk:from(PublicKey),
    Encryptor = case JWKPublicKey#jose_jwk.fields of
        #{<<"kid">> := Kid} ->
            EncryptorWithoutKid = jose_jwk:block_encryptor(JWKPublicKey),
            EncryptorWithoutKid#{<<"kid">> => Kid};
        _ ->
            jose_jwk:block_encryptor(JWKPublicKey)
    end,
    {_EncryptionAlgo, JWEPlain} =
        jose_jwe:block_encrypt(JWKPublicKey, Plain, jose_jwe:from(Encryptor)),
    {#{}, JWECompacted} = jose_jwe:compact(JWEPlain),
    JWECompacted.

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

-spec private_decrypt(jwk_encoded(), jwe_compacted()) -> binary().
private_decrypt(PrivateKey, JWECompacted) ->
    private_decrypt(PrivateKey, <<"">>, JWECompacted).

-spec private_decrypt(jwk_encoded(), binary(), jwe_compacted()) -> binary().
private_decrypt(PrivateKey, Password, JWECompacted) ->
    {_Module, JWKPrivateKey} = jose_jwk:from(Password, PrivateKey),
    {#{}, JWEPlain} = jose_jwe:expand(JWECompacted),
    {Result, _JWE} = jose_jwk:block_decrypt(JWEPlain, JWKPrivateKey),
    Result.

-spec verify(jwk_map() | jwk_pub_binary(), jws_compacted()) -> {ok, binary()} | {error, failed_to_verify}.
verify(PublicKey, SignedPlain) ->
    JWKPublicKey = jose_jwk:from(PublicKey),
    case jose_jwk:verify(SignedPlain, JWKPublicKey) of
        {true, Content, _JWS} ->
            {ok, Content};
        {false, _Content, _JWS} ->
            {error, failed_to_verify}
    end.

-spec sign(jwk_encoded(), binary()) -> binary().
sign(PrivateKey, Plain) ->
    JWKPrivateKey = jose_jwk:from(PrivateKey),
    SignerWithoutKid = jose_jwk:signer(JWKPrivateKey),
    Signer = SignerWithoutKid#{<<"kid">> => JWKPrivateKey#jose_jwk.fields},
    {_JWKModule, SignedPlain} = jose_jwk:sign(Plain, Signer, JWKPrivateKey),
    {_JWSModule, SignedCompacted} = jose_jws:compact(SignedPlain),
    SignedCompacted.

-spec generate_keypair(jwk_generation_params()) -> {jwk_pub_binary(), jwk_priv_binary()}.
generate_keypair(Params) ->
    JWKPrivateKey = jose_jwk:generate_key(Params),
    JWKPrivateKeyWithKid =
        add_jwk_field(JWKPrivateKey, <<"kid">>, jose_jwk:thumbprint(JWKPrivateKey)),
    JWKPublicKeyWithKid = jose_jwk:to_public(JWKPrivateKeyWithKid),
    {_PubModule, PubBinary} = jose_jwk:to_binary(JWKPublicKeyWithKid),
    {_PrivModule, PrivBinary} = jose_jwk:to_binary(JWKPrivateKeyWithKid),
    {PubBinary, PrivBinary}.

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

-spec add_jwk_field(jose_jwk:jose_jwk(), json_object_key(), jsx:json_term()) -> jose_jwk:jose_jwk().
add_jwk_field(JWK, Field, Content) ->
    Fields = JWK#jose_jwk.fields,
    JWK#jose_jwk{fields = Fields#{Field => Content}}.
