-module(cds_thrift_handler_keyring).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").
-include_lib("dmsl/include/dmsl_identity_document_storage_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().
handle_function('Init', [Threshold, Count], _Context, _Opts) when Threshold =< Count ->
    try cds_keyring_manager:initialize(Threshold, Count) of
        Shares ->
            {ok, Shares}
    catch
        already_initialized ->
            cds_thrift_handler_utils:raise(#'KeyringExists'{})
    end;
handle_function('Lock', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:lock()} catch
        not_initialized ->
            cds_thrift_handler_utils:raise(#'NoKeyring'{});
        locked ->
            {ok, ok}
    end;
handle_function('Unlock', [Share], _Context, _Opts) ->
    case cds_keyring_manager:unlock(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {unlocked, #'Unlocked'{}}}
    end;
handle_function('Rotate', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:rotate()} catch
        not_initialized ->
            cds_thrift_handler_utils:raise(#'NoKeyring'{});
        locked ->
            cds_thrift_handler_utils:raise(#'KeyringLocked'{})
    end.

%%
%% Internals
%%
