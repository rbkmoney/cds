-module(cds_storage_service_handler).
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
handle_function(get_card_data, {Token}, _RpcId, _Client, _Opts) ->
	try cds_storage:get_ccd(Token) of
		{ok, CCD} ->
			{ok, CCD};
		{error, locked} ->
			throw(#get_card_data_exception{error = ?CDS_INTERFACE_GETCARDDATAERROR_LOCKED});
		{error, not_found} ->
			throw(#get_card_data_exception{error = ?CDS_INTERFACE_GETCARDDATAERROR_NOT_FOUND})
	catch Error ->
		lager:error("get_ccd error: ~p~n", [Error]),
		throw(#get_card_data_exception{error = ?CDS_INTERFACE_GETCARDDATAERROR_UNKNOWN})
	end;
handle_function(put_card_data, {CCD}, _RpcId, _Client, _Opts) ->
	try cds_storage:put_ccd(CCD) of
		{ok, Token} ->
			{ok, Token};
		{error, locked} ->
			throw(#put_card_data_exception{error = ?CDS_INTERFACE_PUTCARDDATAERROR_LOCKED})
	catch Error ->
		lager:error("put_ccd error: ~p~n", [Error]),
		throw(#put_card_data_exception{error = ?CDS_INTERFACE_PUTCARDDATAERROR_UNKNOWN})
	end.

-spec handle_error(
	woody_t:func(),
	woody_server_thrift_handler:error_reason(),
	woody_t:rpc_id(),
	woody_client:client(),
	woody_server_thrift_handler:handler_opts()
) -> _.
handle_error(get_card_data_exception, Error, RpcId, _Client, _Opts) ->
	lager:info("[~p] got error from thrift: ~p", [my_event_handler:format_id(RpcId), Error]);
handle_error(put_card_data_exception, Error, RpcId, _Client, _Opts) ->
	lager:info("[~p] got error from thrift: ~p", [my_event_handler:format_id(RpcId), Error]).
