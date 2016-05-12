-module(cds_config).

-compile({no_auto_import, [get/1]}).

-export([get/1]).

-spec get(atom()) -> any().
get(Key) ->
	case application:get_env(cds, Key) of
		{ok, Value} ->
			Value;
		undefined ->
			exit({bad_config, Key})
	end.