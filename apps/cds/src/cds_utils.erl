-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-export([decode_token/1]).
-export([encode_token/1]).
-export([decode_session/1]).
-export([encode_session/1]).
-export([split_token/1]).
-export([merge_tokens/2]).

-spec current_time() -> pos_integer().
current_time() ->
    genlib_time:unow().

-spec logtag_process(atom(), any()) -> ok.
logtag_process(Key, Value) when is_atom(Key) ->
    logger:update_process_metadata(#{Key => Value}).

-spec decode_token(binary()) -> cds:token().
decode_token(Token) ->
    base62_decode(Token).

-spec encode_token(cds:token()) -> binary().
encode_token(Token) ->
    base62_encode(Token).

-spec decode_session(binary()) -> cds:session().
decode_session(Session) ->
    base62_decode(Session).

-spec encode_session(cds:session()) -> binary().
encode_session(Session) ->
    base62_encode(Session).

base62_encode(Data) ->
    genlib_format:format_int_base(binary:decode_unsigned(Data), 62).

base62_decode(Data) ->
    EncodedData = binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)),
    Padding = calc_padding(byte_size(EncodedData), 16),
    genlib_string:pad_left(EncodedData, 0, Padding).

calc_padding(Size, N) ->
    case Size rem N of
        0 ->
            Size;
        _ ->
            N * (Size div N + 1)
    end.

-spec split_token(cds:token()) -> {cds:token(), cds:token() | undefined} | no_return().
split_token(<< Token1:16/binary, Token2:16/binary >>) ->
    {Token1, Token2};
split_token(<< Token1:16/binary >>) ->
    {Token1, undefined};
split_token(InvalidToken) ->
    throw({invalid_token, InvalidToken}).

-spec merge_tokens(cds:token(), cds:token()) -> cds:token().
merge_tokens(Token1, Token2) ->
    << Token1/binary, Token2/binary  >>.

% test

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec isomorphic_marshalling_test_() -> _.

isomorphic_marshalling_test_() ->
    Vs = [
        crypto:strong_rand_bytes(16),
        << <<C>> || C <- lists:seq(1, 16) >>,
        <<1:16/integer-unit:8>>,
        <<0:16/integer-unit:8>>,
        crypto:strong_rand_bytes(32),
        << <<C>> || C <- lists:seq(1, 48) >>
    ],
    [?_assertEqual(decode_token(encode_token(V)), V) || V <- Vs].

-spec isomorphic_marshalling_with_padding_test_() -> _.

isomorphic_marshalling_with_padding_test_() ->
    Vs = [
        {<<1:16/integer-unit:8>>, <<1:10/integer-unit:8>>},
        {<<0:14/integer-unit:8, 1, 0:17/integer-unit:8>>, <<1, 0:17/integer-unit:8>>},
        {<<0:4/integer-unit:8, 1, 0:27/integer-unit:8>>, <<1, 0:27/integer-unit:8>>}
        ],
    [?_assertEqual(E, decode_token(encode_token(V))) || {E, V} <- Vs].

-endif.
