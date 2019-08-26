-module(cds_thrift_handler_utils).

-export([filter_fun_exceptions/1]).
-export([raise/1]).
-export([raise_keyring_unavailable/1]).

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
                erlang:raise(Class, filter_error_reason(Exception), Stacktrace)
        end
    end.

-spec raise_keyring_unavailable(locked | not_initialized) ->
    no_return().
raise_keyring_unavailable(KeyringState) ->
    woody_error:raise(system, {internal, resource_unavailable, get_details(KeyringState)}).

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

get_details(locked) ->
    <<"Keyring is locked">>;
get_details(not_initialized) ->
    <<"Keyring is not initialized">>.

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
