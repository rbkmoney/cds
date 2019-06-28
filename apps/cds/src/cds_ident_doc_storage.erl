-module(cds_ident_doc_storage).

-export([get_namespaces/0]).
-export([put_identity_document/1]).
-export([get_identity_document/1]).

-define(IDENTITY_DOCUMENT_NS, <<"identdoc">>).

-define(CREATED_AT_INDEX, {integer_index, "created_at"}).
-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).

-spec get_namespaces() -> [cds_storage:namespace()].
get_namespaces() ->
    [?IDENTITY_DOCUMENT_NS].

-spec put_identity_document(cds_ident_doc:identity_document()) -> cds:token().
put_identity_document(Doc) ->
    Token = token(),
    {KeyID, Key} = cds_keyring:get_current_key(),
    Data = encode_document(Doc, {KeyID, Key}),
    ok = cds_storage:put(
        ?IDENTITY_DOCUMENT_NS,
        Token,
        Data,
        undefined,
        [{?CREATED_AT_INDEX, cds_utils:current_time()}, {?KEY_ID_INDEX, KeyID}]
    ),
    Token.

-spec get_identity_document(cds:token()) -> cds_ident_doc:identity_document() | no_return().
get_identity_document(Token) ->
    {Data, _, _} = cds_storage:get(?IDENTITY_DOCUMENT_NS, Token),
    decode_document(Data).

-spec token() -> cds:token().
token() ->
    crypto:strong_rand_bytes(16).

encode_document(Doc, {KeyID, Key}) ->
    Bin = cds_ident_doc:marshal(Doc),
    Cipher = cds_crypto:encrypt(Key, Bin),
    <<KeyID, Cipher/binary>>.

decode_document(<<KeyID, Cipher/binary>>) ->
    {ok, {KeyID, Key}} = cds_keyring:get_key(KeyID),
    Bin = cds_crypto:decrypt(Key, Cipher),
    cds_ident_doc:unmarshal(Bin).
