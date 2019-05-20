-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-export([decode_token/1]).
-export([encode_token/1]).
-export([decode_session/1]).
-export([encode_session/1]).

-spec current_time() -> pos_integer().
current_time() ->
    genlib_time:unow().

-spec logtag_process(atom(), any()) -> ok.
logtag_process(Key, Value) when is_atom(Key) ->
    lager:md(orddict:store(Key, Value, lager:md())).

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
    genlib_string:pad_left(binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)), 0, 16).

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
