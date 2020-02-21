-module(cds_thrift_handler_utils).

-export([filter_fun_exceptions/1]).
-export([raise/1]).
-export([raise_keyring_unavailable/0]).

-export([map_validation_error/1]).

%%
%% API
%%

-spec filter_fun_exceptions(fun()) -> fun().
filter_fun_exceptions(Fun) ->
    fun() ->
        try
            Fun()
        catch
            throw:Exception ->
                throw(Exception);
            error:{woody_error, _} = WoodyError:Stacktrace ->
                erlang:raise(error, WoodyError, Stacktrace);
            Class:Exception:Stacktrace ->
                erlang:raise(Class, filter_error_reason(Exception), filter_stacktrace(Stacktrace))
        end
    end.

-spec raise_keyring_unavailable() ->
    no_return().
raise_keyring_unavailable() ->
    woody_error:raise(system, {internal, resource_unavailable, <<"Keyring is unavailable">>}).

-spec raise(_) -> no_return().
raise(Exception) ->
    woody_error:raise(business, Exception).

-spec map_validation_error(cds_card_data:reason()) ->
    ErrorMessage :: binary().

map_validation_error(unrecognized) ->
    <<"Unrecognized payment system">>;
map_validation_error({invalid, Field, Check}) ->
    Formatted = io_lib:format("Invalid ~p: ~p", [
        Field,
        map_validation_check(Check)
    ]),
    list_to_binary(Formatted).

%%
%% Internals
%%

map_validation_check({length, _}) -> length;
map_validation_check(Check) -> Check.

% Known safe errors
filter_error_reason({hash_collision_detected, _Hash} = Reason) ->
    Reason;
% Generic safe errors
filter_error_reason(Reason) when is_tuple(Reason) ->
    erlang:list_to_tuple([filter_error_reason(R) || R <- erlang:tuple_to_list(Reason)]);
filter_error_reason(Reason) when is_list(Reason) ->
    [filter_error_reason(R) || R <- Reason];
filter_error_reason(Reason) when is_map(Reason) ->
    maps:map(
        fun(_Key, Value) ->
            filter_error_reason(Value)
        end,
        Reason
    );
filter_error_reason(Reason) when
    is_atom(Reason) orelse
    is_number(Reason) orelse
    is_reference(Reason) orelse
    is_pid(Reason)
->
    Reason;
% Other
filter_error_reason(_Reason) ->
    '***'.

filter_stacktrace(Stacktrace) when is_list(Stacktrace) ->
    [filter_stacktrace_item(ST) || ST <- Stacktrace];
filter_stacktrace(_) ->
    '***'.

filter_stacktrace_item({Module, Function, Arity, Location}) when
    is_atom(Module) andalso
    is_atom(Function) andalso
    is_integer(Arity) andalso
    is_list(Location)
->
    {Module, Function, Arity, filter_stacktrace_location(Location)};
filter_stacktrace_item({Module, Function, Args, Location}) when
    is_atom(Module) andalso
    is_atom(Function) andalso
    is_list(Args) andalso
    is_list(Location)
->
    {Module, Function, filter_stacktrace_args(Args), filter_stacktrace_location(Location)};
filter_stacktrace_item(_) ->
    '***'.

filter_stacktrace_args(Args) when is_list(Args) ->
    [filter_stacktrace_args(Arg) || Arg <- Args];
filter_stacktrace_args(Arg) when
    is_atom(Arg) orelse
    is_number(Arg) orelse
    is_reference(Arg) orelse
    is_pid(Arg)
->
    Arg;
filter_stacktrace_args(_) ->
    '***'.

filter_stacktrace_location([{file, _File}, {line, Line}] = Location) when is_integer(Line) ->
    Location;
filter_stacktrace_location(_) ->
    '***'.
