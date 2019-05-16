-module(cds_woody_client).

-export([call/4]).
-export([call/5]).

%%
%% Types
%%

-type result() :: woody:result()
    | {exception, woody_error:business_error()}
    | no_return().

-export_type([result/0]).

%%
%% API
%%

-spec call(atom(), atom(), list(), woody:url()) -> result().
call(ServiceCode, Function, Args, RootUrl) ->
    call(ServiceCode, Function, Args, RootUrl, #{}).

-spec call(atom(), atom(), list(), woody:url(), woody_client_thrift_http_transport:options()) -> result().
call(ServiceCode, Function, Args, RootUrl, TransportOpts) ->
    Request = {cds_thrift_services:thrift_service(ServiceCode), Function, Args},
    Path = genlib:to_binary(cds_thrift_services:service_path(ServiceCode)),
    CallOpts = #{
        url => <<RootUrl/binary, Path/binary>>,
        event_handler => scoper_woody_event_handler,
        transport_opts => TransportOpts
    },
    case woody_client:call(Request, CallOpts) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            throw(Exception)
    end.
