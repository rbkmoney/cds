-module(cds_client_utils).

-export([call/4]).

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
    Request = {cds_thrift_services:thrift_service(ServiceCode), Function, Args},
    Path = genlib:to_binary(cds_thrift_services:service_path(ServiceCode)),
    TransportOpts = application:get_env(cds, net_opts, []),
    CallOpts = #{
        url => <<RootUrl/binary, Path/binary>>,
        event_handler => cds_woody_event_handler,
        transport_opts => TransportOpts
    },
    case woody_client:call(Request, CallOpts) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            throw(Exception)
    end.

%%
%% Internals
%%
