-module(cds_session_cleaner_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_) ->
    ChildSpec = #{
        id => cds_session_cleaner,
        start => {cds_session_cleaner, start_link, []}
    },
    {ok, {{one_for_one, 1, 5}, [ChildSpec]}}.
