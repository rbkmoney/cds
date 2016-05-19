-module(cds_client).

%% Storage operations
-export([get/1]).
-export([put/1]).
-export([delete/1]).

%% Keyring operations
-export([init/2]).
-export([unlock/1]).
-export([lock/0]).
-export([rotate/0]).

-define(STATIC_CONTEXT, #{
    event_handler => cds_thrift_handler,
    parent_id => <<"undefined">>,
    root_rpc => true,
    rpc_id => undefined,
    seq => 0,
    span_id => <<"kek">>,
    trace_id => <<"kek">>
}).

get(Token) ->
    call(get_card_data, [Token]).

put(Data) ->
    call(put_card_data, [Data]).

delete(Token) ->
    call(delete, [Token]).

init(Threshold, Number) ->
    call(init, [Threshold, Number]).

unlock(Share) ->
    call(unlock, [Share]).

lock() ->
    call(lock, []).

rotate() ->
    call(rotate, []).


call(Function, Args) ->
    Host = application:get_env(cds, thrift_host, "127.0.0.1"),
    Port = integer_to_list(application:get_env(cds, thrift_port, 8022)),
    Call = {{cds_thrift, cds}, Function, Args},
    Server = #{url => Host ++ ":" ++ Port ++ "/v1/cds"},
    try woody_client:call(?STATIC_CONTEXT, Call, Server) of
        {ok, _Context} ->
            ok;
        {{ok, Response}, _Context} ->
            Response
    catch {{exception, Exception}, _Context} ->
        throw(Exception)
    end.