-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-export([decode_token/1]).
-export([encode_token/1]).
-export([decode_session/1]).
-export([encode_session/1]).
-export([pr_stacktrace/2]).

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

-spec pr_stacktrace(term(), term()) -> term().

pr_stacktrace(Stacktrace, {Class, Reason}) ->
    lists:flatten(
        pr_stacktrace_(lists:reverse(Stacktrace)) ++ "\n" ++ io_lib:format("~s:~p", [Class, Reason])).

pr_stacktrace_(Stacktrace) ->
    Indent = "\n    ",
    lists:foldl(
        fun(Entry, Acc) ->
            Acc ++ Indent ++ format_mfa(Entry)
        end,
        [],
        Stacktrace).

format_mfa(MFA) ->
    element(2, format_mfa_md(MFA)).

-spec format_mfa_md(any()) -> {[{atom(), any()}], list()}.
format_mfa_md({M, F, A}) when is_list(A) ->
    {FmtStr, Args} = format_args(A, [], []),
    {[{module, M}, {function, F}], io_lib:format("~w:~w(" ++ FmtStr ++ ")", [M, F | Args])};
format_mfa_md({M, F, A}) when is_integer(A) ->
    {[{module, M}, {function, F}], io_lib:format("~w:~w/~w", [M, F, A])};
format_mfa_md({M, F, A, Props}) when is_list(Props) ->
    case proplists:get_value(line, Props) of
        undefined ->
            format_mfa_md({M, F, A});
        Line ->
            {Md, Formatted} = format_mfa_md({M, F, A}),
            {[{line, Line} | Md], [Formatted, io_lib:format(" line ~w", [Line])]}
    end;
format_mfa_md([{M, F, A}| _]) ->
    %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
    format_mfa_md({M, F, A});
format_mfa_md([{M, F, A, Props}| _]) when is_list(Props) ->
    %% this kind of weird stacktrace can be generated by a uncaught throw in a gen_server
    %% TODO we might not always want to print the first MFA we see here, often it is more helpful
    %% to print a lower one, but it is hard to programatically decide.
    format_mfa_md({M, F, A, Props});
format_mfa_md(Other) ->
    {[], io_lib:format("~w", [Other])}.

format_args([], FmtAcc, ArgsAcc) ->
    {string:join(lists:reverse(FmtAcc), ", "), lists:reverse(ArgsAcc)};
format_args([H|T], FmtAcc, ArgsAcc) ->
    {Str, _} = io_lib:print(H, 1, 100, -1),
    format_args(T, ["~s"|FmtAcc], [Str|ArgsAcc]).

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
