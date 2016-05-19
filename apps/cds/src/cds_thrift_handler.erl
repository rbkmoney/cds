-module(cds_thrift_handler).
-behaviour(woody_server_thrift_handler).
-behaviour(woody_event_handler).

-include("cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).
-export([handle_error/4]).

%% woody_event_handler callback
-export([handle_event/3]).


-spec handle_function(
    woody_t:func(),
    woody_server_thrift_handler:args(),
    woody_client:context(),
    woody_server_thrift_handler:handler_opts()
) -> ok | {ok, woody_server_thrift_handler:result()} | no_return().
handle_function(unlock, {Share}, _Context, _Opts) ->
    case cds:unlock_keyring(Share) of
        {more, More} ->
            {ok, #unlock_status{unlocked = false, more_keys_needed = More}};
        unlocked ->
            {ok, #unlock_status{unlocked = true, more_keys_needed = 0}}
    end;
handle_function(get_card_data, {Token}, _Context, _Opts) ->
    try cds:get(Token) of
        CardData ->
            {ok, CardData}
    catch
        not_found ->
            throw(#not_found{});
        locked ->
            throw(#locked{})
    end;
handle_function(put_card_data, {CardData}, _Context, _Opts) ->
    try cds:put(CardData) of
        Token ->
            {ok, Token}
    catch
        locked ->
            throw(#locked{})
    end.

-spec handle_error(
    woody_t:func(),
    woody_server_thrift_handler:error_reason(),
    woody_t:rpc_id(),
    woody_server_thrift_handler:handler_opts()
) -> _.
handle_error(get_card_data_exception, Error, RpcId, _Opts) ->
    lager:info("[~p] got error from thrift: ~p", [RpcId, Error]);
handle_error(put_card_data_exception, Error, RpcId, _Opts) ->
    lager:info("[~p] got error from thrift: ~p", [RpcId, Error]).

handle_event(Event, RpcId, Meta) ->
    lager:info("[~p] woody event ~p ~p~n", [RpcId, Event, Meta]).
