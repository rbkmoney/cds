-module(cds_thrift_handler_utils).

-export([raise/1]).
-export([raise_keyring_unavailable/0]).

-export([map_validation_error/1]).

%%
%% API
%%

-spec raise_keyring_unavailable() ->
    no_return().
raise_keyring_unavailable() ->
    woody_error:raise(system, {internal, resource_unavailable, <<"Keyring is unavailable">>}).

-spec raise(_) -> no_return().
raise(Exception) ->
    woody_error:raise(business, Exception).

-spec map_validation_error(cds_card_data:reason()) ->
    ErrorMessage :: binary().

map_validation_error(unrecognized) ->
    <<"Unrecognized payment system">>;
map_validation_error({invalid, Field, Check}) ->
    Formatted = io_lib:format("Invalid ~p: ~p", [
        Field,
        map_validation_check(Check)
    ]),
    list_to_binary(Formatted).

%%
%% Internals
%%

map_validation_check({length, _}) -> length;
map_validation_check(Check) -> Check.
