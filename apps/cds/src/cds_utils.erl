-module(cds_utils).

-export([current_time/0]).

-spec current_time() -> pos_integer().
current_time() ->
    genlib_time:unow().
