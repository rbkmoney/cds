-module(cds_token_storage).

-export([get_namespaces/0]).
-export([put_token/2]).
-export([get_token/1]).

-define(TOKEN_NS, <<"token">>).

-define(KEY_ID_INDEX, {integer_index, "encoding_key_id"}).

-spec get_namespaces() -> [cds_storage:namespace()].
get_namespaces() ->
    [?TOKEN_NS].

-type token_id()        :: binary().
-type token_content()   :: binary().

-spec put_token(token_id(), token_content()) -> ok.
put_token(TokenId, TokenContent) ->
    {KeyID, Key} = cds_keyring:get_current_key(),
    TokenData = encode_token_content(TokenContent, {KeyID, Key}),
    ok = cds_storage:put(
        ?TOKEN_NS,
        TokenId,
        TokenData,
        undefined,
        [{?KEY_ID_INDEX, KeyID}]
    ).

-spec get_token(token_id()) -> token_content() | no_return().
get_token(TokenId) ->
    {TokenData, _, _} = cds_storage:get(?TOKEN_NS, TokenId),
    decode_token_content(TokenData).

encode_token_content(TokenContent, {KeyID, Key}) ->
    Cipher = cds_crypto:encrypt(Key, TokenContent),
    <<KeyID, Cipher/binary>>.

decode_token_content(<<KeyID, Cipher/binary>>) ->
    {ok, {KeyID, Key}} = cds_keyring:get_key(KeyID),
    cds_crypto:decrypt(Key, Cipher).
