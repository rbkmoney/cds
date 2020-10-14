-module(cds_maintenance_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, Reason :: any()}.
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec init(_) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_) ->
    Cleaner = spec(
        [
            #{
                id => cds_session_cleaner,
                start => {cds_session_cleaner, start_link, []}
            }
        ],
        session_cleaning
    ),
    Recrypter = spec(
        [
            #{
                id => cds_cvv_recrypter,
                start => {cds_recrypter, start_link, [#{subject => session}]}
            },
            #{
                id => cds_card_data_recrypter,
                start => {cds_recrypter, start_link, [#{subject => carddata}]}
            }
        ],
        recrypting
    ),
    {ok, {{one_for_one, 1, 5}, Cleaner ++ Recrypter}}.

spec(Spec, Name) ->
    Config = application:get_env(cds, Name, #{}),
    case maps:get(enabled, Config, false) of
        true -> Spec;
        false -> []
    end.
