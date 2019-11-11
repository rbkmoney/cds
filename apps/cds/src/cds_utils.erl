-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-export([decode_token/1]).
-export([encode_token/1]).
-export([decode_session/1]).
-export([encode_session/1]).
-export([extract_token/1]).
-export([extract_payload/1]).
-export([add_payload/2]).

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
    DecodedData = binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)),
    case binary:split(DecodedData, <<".">>) of
        [Token] ->
            genlib_string:pad_left(Token, 0, 16);
        [Token, Rest] ->
            PaddedToken = genlib_string:pad_left(Token, 0, 16),
            << PaddedToken/binary, $., Rest/binary >>
    end.

-spec extract_token(binary()) -> cds:token() | no_return().
extract_token(<< Token:16/binary, $., _/binary >>) ->
    Token;
extract_token(<< Token:16/binary >>) ->
    Token.

-spec extract_payload(binary()) -> cds_card_data:payload_data() | no_return().
extract_payload(<< _Token:16/binary, $., Payload/binary >>) ->
    cds_card_data:unmarshal_payload(Payload);
extract_payload(<< _Token:16/binary >>) ->
    cds_card_data:unmarshal_payload(<<>>).

-spec add_payload(cds:token(), cds_card_data:cardholder_data()) -> binary().
add_payload(
    Token,
    #{
        exp_date   := {Month, Year},
        cardholder := undefined
}) ->
    << Token/binary, $., Month:8, Year:16 >>;
add_payload(
    Token,
    #{
        exp_date   := {Month, Year},
        cardholder := CardholderName
}) ->
    << Token/binary, $., Month:8, Year:16, CardholderName/binary >>;
add_payload(Token, _) ->
    Token.

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
        <<0:16/integer-unit:8>>
    ],
    [?_assertEqual(decode_token(encode_token(V)), V) || V <- Vs].

-endif.
