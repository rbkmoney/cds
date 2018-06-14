-module(cds_thrift_handler_utils).

-export([raise/1]).
-export([raise_keyring_unavailable/1]).

%%
%% API
%%

-spec raise_keyring_unavailable(locked | not_initialized) ->
    no_return().
raise_keyring_unavailable(KeyringState) ->
    woody_error:raise(system, {internal, resource_unavailable, get_details(KeyringState)}).

-spec raise(_) -> no_return().
raise(Exception) ->
    woody_error:raise(business, Exception).

%%
%% Internals
%%

get_details(locked) ->
    <<"Keyring is locked">>;
get_details(not_initialized) ->
    <<"Keyring is not initialized">>.

