-module(cds_woody_event_handler).
-behaviour(woody_event_handler).

-include_lib("woody/src/woody_defs.hrl").
-export([handle_event/4]).

-define(SERVER, 'rpc.server').

-spec handle_event(Event, RpcId, Meta, Opts) -> ok when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

handle_event(EventType, RpcID, EventMeta, _Opts) ->
    Msg = woody_event_handler:format_event(EventType, EventMeta, RpcID),
    format_event(EventType, RpcID, EventMeta, Msg).

%% client
%% occures only in tests

format_event(EventType, RpcID, _EventMeta, Msg) when
    EventType == ?EV_CALL_SERVICE;
    EventType == ?EV_CLIENT_SEND;
    EventType == ?EV_CLIENT_RECEIVE;
    EventType == ?EV_SERVICE_RESULT
->
    _ = log(RpcID, Msg, orddict:new());

%% server

format_event(EventType = ?EV_SERVER_RECEIVE, RpcID, #{status := Status}, Msg) ->
    ok = enter(?SERVER, #{}),
    _ = log(RpcID, Msg, collect(?SERVER, #{
        event => EventType,
        status => Status
    }));

format_event(EventType = ?EV_INVOKE_SERVICE_HANDLER, RpcID, #{
    service  := Service,
    function := Function,
    metadata := Metadata
} = _Meta, Msg) ->
    ok = enter(?SERVER, maps:merge(Metadata, #{
        service  => Service,
        function => Function
    })),
    _ = log(RpcID, Msg, collect(?SERVER, #{
        event => EventType
    }));

format_event(EventType = ?EV_SERVICE_HANDLER_RESULT, RpcID, #{status := Status}, Msg) ->
    _ = log(RpcID, Msg, collect(?SERVER, #{
        event => EventType,
        status => Status
    }));

format_event(EventType = ?EV_SERVER_SEND, RpcID, #{status := Status}, Msg) ->
    _ = log(RpcID, Msg, collect(?SERVER, #{
        event => EventType,
        status => Status
    })),
    leave(?SERVER);

%% common

format_event(EventType, RpcID, _EventMeta, Msg) when
    EventType == ?EV_INTERNAL_ERROR;
    EventType == ?EV_TRACE
->
    log(RpcID, Msg, collect(?SERVER, #{
        event => EventType
    })).

%%

log(RpcID, {Level, {Format, Args}}, MD) ->
    lager:log(Level, [{pid, self()}] ++ rpc_id_to_md(RpcID) ++ orddict:to_list(MD), Format, Args).

rpc_id_to_md(RpcID = #{}) ->
    maps:to_list(RpcID);

rpc_id_to_md(undefined) ->
    [].

%%

enter(Name, Meta) ->
    lager:md(collect(Name, Meta)).

leave(Name) ->
    lager:md(orddict:erase(Name, lager:md())).

collect(Name, Meta) ->
    orddict:store(Name, maps:merge(find_scope(Name), Meta), lager:md()).

find_scope(Name) ->
    case orddict:find(Name, lager:md()) of
        {ok, V = #{}} -> V;
        error         -> #{}
    end.
