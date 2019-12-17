-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-export([decode_token/1]).
-export([encode_token/1]).
-export([decode_session/1]).
-export([encode_session/1]).
-export([encode_token_with_payload/2]).
-export([decode_token_with_payload/1]).

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
    genlib_string:pad_left(binary:encode_unsigned(genlib_format:parse_int_base(Data, 62)), 0, 16).

%% NOTE: The *_payload function family added for transition from old damsel based
%% CDS protocol to new CDS protocol and will be removed shortly after adopting
%% last one by adapters/core services.

-spec encode_token_with_payload(cds:token(), cds_card_data:cardholder_data()) -> binary().

encode_token_with_payload(Token, Payload) when map_size(Payload) == 0 ->
    base62_encode(Token);
encode_token_with_payload(Token, Payload) ->
    <<
        (base62_encode(Token))/binary, $.,
        (encode_payload(Payload))/binary
    >>.

-spec decode_token_with_payload(binary()) -> {cds:token(), cds_card_data:payload_data()}.
decode_token_with_payload(Token) ->
    case binary:split(Token, <<".">>) of
        [T] ->
            {base62_decode(T), #{}};
        [T, P] ->
            {base62_decode(T), decode_payload(P)}
    end.

-spec decode_payload(binary()) -> cds_card_data:payload_data().
decode_payload(<< Month:8, Year:16 >>) ->
    #{
        exp_date   => {Month, Year}
    };
decode_payload(<< Month:8, Year:16, Cardholder/binary >>) ->
    #{
        exp_date   => {Month, Year},
        cardholder => Cardholder
    };
decode_payload(<<>>) ->
    #{}.

encode_payload(
    #{
        exp_date   := {Month, Year},
        cardholder := CardholderName
}) when is_binary(CardholderName) ->
    << Month:8, Year:16, CardholderName/binary >>;
encode_payload(
    #{
        exp_date   := {Month, Year}
    }) ->
    << Month:8, Year:16 >>;
encode_payload(_) ->
    <<>>.

% test

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-spec test() -> _.
-spec isomorphic_session_marshalling_test_() -> _.

isomorphic_session_marshalling_test_() ->
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
        crypto:strong_rand_bytes(16),
        <<1:16/integer-unit:8>>,
        <<0:16/integer-unit:8>>
    ],
    [?_assertEqual(V, decode_token(encode_token(V))) || V <- Vs].

-spec isomorphic_payload_marshalling_test_() -> _.

isomorphic_payload_marshalling_test_() ->
    Vs = [
        #{exp_date => {8, 2021}, cardholder => <<"T S">>},
        #{exp_date => {8, 2021}},
         #{}
    ],
    [?_assertEqual(V, decode_payload(encode_payload(V))) || V <- Vs].

-endif.
