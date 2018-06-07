-module(cds_id_storage).

-export([get_namespaces/0]).
-export([put_identity_document/1]).
-export([get_identity_document/1]).

-define(IDENTITY_DOCUMENT_NS, <<"identity_document">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).

-spec get_namespaces() -> [cds_storage:namespace()].
get_namespaces() ->
    [?IDENTITY_DOCUMENT_NS].

-spec put_identity_document(cds_identity_document:identity_document()) ->
    {binary(), cds_identity_document:safe_identity_document()}.
put_identity_document(Doc) ->
    Token = token(),
    {KeyID, Key} = cds_keyring_manager:get_current_key(),
    Data = encode_document(Doc, {KeyID, Key}),
    ok = cds_storage:put(
        ?IDENTITY_DOCUMENT_NS,
        Token,
        Data,
        undefined,
        [{?CREATED_AT_INDEX, cds_utils:current_time()}, {?KEY_ID_INDEX, KeyID}]
    ),
    {Token, cds_identity_document:get_safe_data(Doc)}.

-spec get_identity_document(binary()) -> cds_identity_document:identity_document() | no_return().
get_identity_document(Token) ->
    {Data, _, _} = cds_storage:get(?IDENTITY_DOCUMENT_NS, Token),
    decode_document(Data).

token() ->
    crypto:strong_rand_bytes(16).

encode_document(Doc, {KeyID, Key}) ->
    Bin = cds_identity_document:marshal(Doc),
    Cipher = cds_crypto:encrypt(Key, Bin),
    <<KeyID, Cipher/binary>>.

decode_document(<<KeyID, Cipher/binary>>) ->
    {KeyID, Key} = cds_keyring_manager:get_key(KeyID),
    Bin = cds_crypto:decrypt(Key, Cipher),
    cds_identity_document:unmarshal(Bin).
