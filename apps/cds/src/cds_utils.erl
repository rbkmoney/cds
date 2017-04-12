-module(cds_utils).

-export([current_time/0]).
-export([logtag_process/2]).

-spec current_time() -> pos_integer().
current_time() ->
    genlib_time:unow().

-spec logtag_process(atom(), any()) -> ok.
logtag_process(Key, Value) when is_atom(Key) ->
    lager:md(orddict:store(Key, Value, lager:md())).
