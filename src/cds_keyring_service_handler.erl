-module(cds_keyring_service_handler).
-behaviour(woody_server_thrift_handler).

-include("cds_interface_types.hrl").

-export([handle_function/5, handle_error/5]).

-spec handle_function(
	woody_t:func(),
	woody_server_thrift_handler:args(),
	woody_t:rpc_id(),
	woody_client:client(),
	woody_server_thrift_handler:handler_opts()
) -> ok | {ok, woody_server_thrift_handler:result()} | {error, woody_server_thrift_handler:result()} | no_return().
handle_function(init, {Threshold, NumParts}, _RpcId, _Client, _Opts) when Threshold > NumParts ->
	throw(#init_exception{error = ?CDS_INTERFACE_INITERROR_BAD_ARGUMENTS});
handle_function(init, {Threshold, NumParts}, _RpcId, _Client, _Opts) ->
	try cds_keyring:initialize(Threshold, NumParts) of
		{ok, Shares} ->
			{ok, Shares}
		% {error, keyring_exists} ->
		% 	throw(#init_exception{error = ?CDS_INTERFACE_INITERROR_KEYRING_EXISTS});
	catch Error ->
		lager:error("init error: ~p~n", [Error]),
		throw(#init_exception{error = ?CDS_INTERFACE_INITERROR_UNKNOWN})
	end;
handle_function(unlock, {Share}, _RpcId, _Client, _Opts) ->
	try cds_keyring:unlock(Share) of
		ok ->
			{ok, #unlock_status{unlocked = true}};
		{more, More} ->
			{ok, #unlock_status{unlocked = false, more_keys_needed = More}};
		{error, recovery_failed} ->
			throw(#unlock_exception{error = ?CDS_INTERFACE_UNLOCKERROR_CANNOT_RECOVER});
		{error, keyring_not_loaded} ->
			throw(#unlock_exception{error = ?CDS_INTERFACE_UNLOCKERROR_NO_KEYRING});
		{error, decryption_failed} ->
			throw(#unlock_exception{error = ?CDS_INTERFACE_UNLOCKERROR_CANNOT_DECRYPT})
	catch Error ->
		lager:error("unlock error: ~p~n", [Error]),
		throw(#unlock_exception{error = ?CDS_INTERFACE_UNLOCKERROR_UNKNOWN})
	end;
handle_function(rotate, {}, _RpcId, _Client, _Opts) ->
	try cds_keyring:rotate() of
		ok ->
			ok
	catch Error ->
		lager:error("rotate error: ~p~n", [Error]),
		throw(#rotate_exception{error = ?CDS_INTERFACE_ROTATEERROR_UNKNOWN})
	end.


-spec handle_error(
	woody_t:func(),
	woody_server_thrift_handler:error_reason(),
	woody_t:rpc_id(),
	woody_client:client(),
	woody_server_thrift_handler:handler_opts()
) -> _.
handle_error(Method, Error, RpcId, _Client, _Opts) ->
	lager:info("[~p] got ~p error from thrift: ~p", [my_event_handler:format_id(RpcId), Method, Error]).