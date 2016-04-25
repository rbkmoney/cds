-module(cds_service_event_handler).
-behaviour(woody_event_handler).

-export([handle_event/2]).

handle_event(EventType, Event) ->
	lager:info("Event ~p ~p~n", [EventType, Event]).