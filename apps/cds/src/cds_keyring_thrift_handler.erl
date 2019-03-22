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

handle_function_('Init', [Threshold], _Context, _Opts) ->
    try {ok, cds_keyring_manager:initialize(Threshold)} catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {invalid_activity, Activity} ->
            cds_thrift_handler_utils:raise(#'InvalidActivity'{activity = Activity});
        invalid_args ->
            cds_thrift_handler_utils:raise(#'InvalidArguments'{})
    end;
handle_function_('ValidateInit', [Share], _Context, _Opts) ->
    try cds_keyring_manager:validate_init(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {operation_aborted, Reason} ->
            cds_thrift_handler_utils:raise(#'OperationAborted'{reason = Reason})
    end;
handle_function_('CancelInit', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:cancel_init()} catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status})
    end;
handle_function_('Lock', [], _Context, _Opts) ->
    try {ok, cds_keyring_manager:lock()} catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        locked ->
            {ok, ok}
    end;
handle_function_('Unlock', [Share], _Context, _Opts) ->
    try cds_keyring_manager:unlock(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status})
    end;
handle_function_('Rotate', [Share], _Context, _Opts) ->
    try cds_keyring_manager:rotate(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        ok ->
            {ok, {success, #'Success'{}}}
    catch
        {invalid_status, Status} ->
            cds_thrift_handler_utils:raise(#'InvalidStatus'{status = Status});
        {operation_aborted, Reason} ->
            cds_thrift_handler_utils:raise(#'OperationAborted'{reason = Reason})
    end.
