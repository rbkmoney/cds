-module(cds_woody_event_handler).

-behaviour(woody_event_handler).

%% woody_event_handler behaviour callbacks
-export([handle_event/4]).

-define(CALL_BLACKLIST, [
    {'Keyring', 'ValidateInit'},
    {'Keyring', 'ValidateRekey'},
    {'Keyring', 'ConfirmRekey'},
    {'Keyring', 'ConfirmRotate'},
    {'Keyring', 'ConfirmUnlock'}
]).
-define(RESULT_BLACKLIST, [
    {'Keyring', 'StartInit'},
    {'Keyring', 'StartRekeyValidation'}
]).

%%
%% woody_event_handler behaviour callbacks
%%
-spec handle_event(Event, RpcId, Meta, Opts) ->
    ok
    when
    Event :: woody_event_handler:event(),
    RpcId :: woody:rpc_id() | undefined,
    Meta  :: woody_event_handler:event_meta(),
    Opts  :: woody:options().

%% client scoping
handle_event(Event = 'client begin', RpcID, RawMeta, _Opts) ->
    ok = scoper:add_scope(get_scope_name(client)),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'client cache begin', RpcID, RawMeta, _Opts) ->
    ok = scoper:add_scope(get_scope_name(caching_client)),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'client end', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    scoper:remove_scope();
handle_event(Event = 'client cache end', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    scoper:remove_scope();

%% server scoping
handle_event(Event = 'server receive', RpcID, RawMeta, _Opts) ->
    ok = add_server_meta(RpcID),
    handle_event(Event, RpcID, RawMeta);
handle_event(Event = 'server send', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    remove_server_meta();

%% special cases
handle_event(Event = 'internal error', RpcID, RawMeta, _Opts) ->
    ok = handle_event(Event, RpcID, RawMeta),
    final_error_cleanup(RawMeta);
handle_event(Event = 'trace event', RpcID, RawMeta = #{role := Role}, _Opts) ->
    case lists:member(get_scope_name(Role), scoper:get_scope_names()) of
        true ->
            handle_event(Event, RpcID, RawMeta);
        false ->
            scoper:scope(
                get_scope_name(Role),
                fun() -> handle_event(Event, RpcID, RawMeta) end
            )
    end;
%% the rest
handle_event(Event, RpcID, RawMeta, _Opts) ->
    handle_event(Event, RpcID, RawMeta).


%%
%% Internal functions
%%
handle_event(Event, _RpcID, _RawMeta) when
    Event =:= 'client begin' orelse
        Event =:= 'client end' orelse
        Event =:= 'client cache begin' orelse
        Event =:= 'client cache end'
    ->
    ok;
handle_event(Event, RpcID, RawMeta = #{role := Role}) ->
    {Level, {Format, Args}, Meta} = woody_event_handler:format_event_and_meta(
        Event,
        meta_filter(RawMeta),
        RpcID,
        [event, service, function, type, metadata, url, deadline, execution_duration_ms]
    ),
    ok = scoper:add_meta(Meta),
    logger:log(Level, Format, Args, collect_md(Role, RpcID));
handle_event(_Event, _RpcID, _RawMeta) ->
    ok.

meta_filter(#{service := Service, function := Function, result := _Result} = RawMeta) ->
    case lists:any(fun (Blacklisted) -> Blacklisted =:= {Service, Function} end, ?RESULT_BLACKLIST) of
        true ->
            RawMeta#{result => {ok, ok}};
        false ->
            RawMeta
    end;
meta_filter(#{service := Service, function := Function, args := _Result} = RawMeta) ->
    case lists:any(fun (Blacklisted) -> Blacklisted =:= {Service, Function} end, ?CALL_BLACKLIST) of
        true ->
            RawMeta#{args => []};
        false ->
            RawMeta
    end;
meta_filter(RawMeta) ->
    RawMeta.

%% Log metadata should contain rpc ID properties (trace_id, span_id and parent_id)
%% _on the top level_ according to the requirements.
%% In order to add rpc ID to log messages from woody handler, it is stored
%% in lager:md() in case of woody server. Since woody client can be invoked during
%% processing of parent request by a woody server handler, rpc ID of the child request
%% is added directly to the log meta before logging. It is _not stored_ in lager:md()
%% in that case, so child rpc ID does not override parent rpc ID
%% for the server handler processing context.
collect_md(client, RpcID) ->
    collect_md(add_rpc_id(RpcID, scoper:collect()));
collect_md(server, _RpcID) ->
    collect_md(scoper:collect()).

collect_md(MD) ->
    MD#{pid => self()}.

get_scope_name(client) ->
    'rpc.client';
get_scope_name(caching_client) ->
    'rpc.caching_client';
get_scope_name(server) ->
    'rpc.server'.

final_error_cleanup(#{role := server, error := _, final := true}) ->
    remove_server_meta();
final_error_cleanup(_) ->
    ok.

add_server_meta(RpcID) ->
    ok = scoper:add_scope(get_scope_name(server)),
    logger:set_process_metadata(add_rpc_id(RpcID, scoper:collect())).

remove_server_meta() ->
    _ = case scoper:get_current_scope() of
            'rpc.server' ->
                ok;
            _  ->
                logger:warning(
                    "Scoper woody event handler: removing uncleaned scopes on the server: ~p",
                    [scoper:get_scope_names()]
                )
        end,
    ok = scoper:clear().

add_rpc_id(undefined, MD) ->
    MD;
add_rpc_id(RpcID, MD) ->
    maps:merge(MD, RpcID).
