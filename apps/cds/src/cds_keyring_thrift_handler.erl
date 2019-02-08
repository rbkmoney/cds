-module(cds_keyring_thrift_handler).
-behaviour(woody_server_thrift_handler).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).

%%
%% woody_server_thrift_handler callbacks
%%

-spec handle_function(woody:func(), woody:args(), woody_context:ctx(), woody:options()) ->
    {ok, woody:result()} | no_return().

handle_function(OperationID, Args, Context, Opts) ->
    scoper:scope(
        keyring,
        fun() -> handle_function_(OperationID, Args, Context, Opts) end
    ).

handle_function_('Init', [Threshold, Count], _Context, _Opts) when Threshold =< Count ->
    try cds_keyring_manager:initialize(Threshold, Count) of
        Shares ->
            {ok, Shares}
    catch
        already_initialized ->
            cds_thrift_handler_utils:raise(#'KeyringExists'{})
    end;
handle_function_('Lock', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:lock()} catch
        not_initialized ->
            cds_thrift_handler_utils:raise(#'NoKeyring'{});
        locked ->
            {ok, ok}
    end;
handle_function_('Unlock', [Share], _Context, _Opts) ->
    case cds_keyring_manager:unlock(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {unlocked, #'Unlocked'{}}}
    end;
handle_function_('Rotate', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:rotate()} catch
        not_initialized ->
            cds_thrift_handler_utils:raise(#'NoKeyring'{});
        locked ->
            cds_thrift_handler_utils:raise(#'KeyringLocked'{})
    end.
