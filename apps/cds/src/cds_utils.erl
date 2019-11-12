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
    logger:update_process_metadata(#{Key => Value}).

-spec decode_token(binary()) -> {cds:token(), cds_card_data:payload_data()}.
decode_token(Token) ->
    case binary:split(Token, <<".">>) of
        [T] ->
            {base62_decode(T), #{}};
        [T, P] ->
            {base62_decode(T), unmarshal_payload(P)}
    end.

-spec encode_token(Data) -> binary() when
    Data :: {cds:token(), cds_card_data:payload_data()} | cds:token().
encode_token({Token, Payload}) when map_size(Payload) == 0 ->
    base62_encode(Token);
encode_token({Token, Payload}) ->
    <<
        (base62_encode(Token))/binary, $.,
        (marshal_payload(Payload))/binary
    >>;
encode_token(Token) when is_binary(Token) ->
    encode_token({Token, #{}}).

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

marshal_payload(
    #{
        exp_date   := {Month, Year},
        cardholder := CardholderName
}) when is_binary(CardholderName) ->
    << Month:8, Year:16, CardholderName/binary >>;
marshal_payload(
    #{
        exp_date   := {Month, Year}
    }) ->
    << Month:8, Year:16 >>;
marshal_payload(_) ->
    <<>>.

unmarshal_payload(<< Month:8, Year:16 >>) ->
    #{
        exp_date   => {Month, Year}
    };
unmarshal_payload(<< Month:8, Year:16, Cardholder/binary >>) ->
    #{
        exp_date   => {Month, Year},
        cardholder => Cardholder
    };
unmarshal_payload(<<>>) ->
    #{}.


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
    [?_assertEqual(decode_session(encode_session(V)), V) || V <- Vs].

-spec isomorphic_token_marshalling_test_() -> _.

isomorphic_token_marshalling_test_() ->
    Vs = [
        {crypto:strong_rand_bytes(16), #{exp_date => {8, 2021}, cardholder => <<"T S">>}},
        {<<1:16/integer-unit:8>>, #{exp_date => {8, 2021}}},
        {<<0:16/integer-unit:8>>, #{}}
    ],
    [?_assertEqual(V, decode_token(encode_token(V))) || V <- Vs].

-endif.
