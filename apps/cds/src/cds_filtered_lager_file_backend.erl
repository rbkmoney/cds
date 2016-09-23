%% Lager file backend decorator module
-module(cds_filtered_lager_file_backend).

-behaviour(gen_event).

-compile([{parse_transform, lager_transform}]).

-export([init/1]).
-export([handle_call/2]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).
-export([config_to_id/1]).

-record(state, {
    regexes,
    backend_state
}).

%% need elaboration
-define(REGEXES, [
    "[0-9]{4}.[0-9]{4}.[0-9]{4}.[0-9]{4}", %% pan
    "[0-9]{2}.[0-9]{2,4}", %% expiration date
    "[0-9]{3}" %% cvv
]).
-define(CENSORED_TEXT, "[censored]").

init(Options) ->
    Regexes = [Regex || {ok, Regex} <- [re:compile(Regex) || Regex <- ?REGEXES]],
    call_backend(init, Options, #state{regexes = Regexes}).
    

handle_call(Request, State) ->
    call_backend(handle_call, Request, State).

handle_event({log, Msg}, #state{regexes = Regexes} = State) ->
    Message = lager_msg:message(Msg),
    NewMessage = lists:foldl(fun apply_regex/2, Message, Regexes),
    call_backend(handle_event, {log, update_message(NewMessage, Msg)}, State);
handle_event(Event, State) ->
    call_backend(handle_event, Event, State).
    

handle_info(Info, State) ->
    call_backend(handle_info, Info, State).

terminate(Reason, State) ->
    call_backend(terminate, Reason, State).

code_change(OldVsn, State, Extra) ->
    call_backend(code_change, {OldVsn, Extra}, State).


config_to_id({Name,_Severity}) when is_list(Name) ->
    {?MODULE, Name};
config_to_id({Name,_Severity,_Size,_Rotation,_Count}) ->
    {?MODULE, Name};
config_to_id([{Name,_Severity,_Size,_Rotation,_Count}, _Format]) ->
    {?MODULE, Name};
config_to_id([{Name,_Severity}, _Format]) when is_list(Name) ->
    {?MODULE, Name};
config_to_id(Config) ->
    case proplists:get_value(file, Config) of
        undefined ->
            erlang:error(no_file);
        File ->
            {?MODULE, File}
    end.


%% util
call_backend(init, Options, State) ->
    case lager_file_backend:init(Options) of
        {ok, BackendState} ->
            {ok, State#state{backend_state = BackendState}};
        Error ->
            Error
    end;
call_backend(handle_call, Arg, #state{backend_state = BackendState} = State) ->
    case lager_file_backend:handle_call(Arg, BackendState) of
        {ok, Response, NewBackendState} ->
            {ok, Response, State#state{backend_state = NewBackendState}}
    end;
call_backend(code_change, {OldVsn, Extra}, #state{backend_state = BackendState} = State) ->
    case lager_file_backend:code_change(OldVsn, BackendState, Extra) of
        {ok, NewBackendState} ->
            {ok, State#state{backend_state = NewBackendState}}
    end;
call_backend(terminate, Reason, #state{backend_state = BackendState}) ->
    lager_file_backend:terminate(Reason, BackendState);
call_backend(Method, Arg, #state{backend_state = BackendState} = State) ->
    case lager_file_backend:Method(Arg, BackendState) of
        {ok, NewBackendState} ->
            {ok, State#state{backend_state = NewBackendState}}
    end.

apply_regex(Regex, Message) ->
    binary_to_list(iolist_to_binary(re:replace(Message, Regex, ?CENSORED_TEXT))).

update_message(Message, Msg) ->
    Timestamp = lager_msg:timestamp(Msg),
    Severity = lager_msg:severity(Msg),
    Metadata = lager_msg:metadata(Msg),
    Destinations = lager_msg:destinations(Msg),
    lager_msg:new(Message, Timestamp, Severity, Metadata, Destinations).
