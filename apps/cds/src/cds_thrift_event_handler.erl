-module(cds_thrift_event_handler).
-behaviour(woody_event_handler).

-export([handle_event/3]).

handle_event(Event, RpcId, Meta) ->
    lager:info("[~p] woody event ~p ~p~n", [RpcId, Event, Meta]).
