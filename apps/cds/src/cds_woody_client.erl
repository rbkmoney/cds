-module(cds_woody_client).

-export([call/4]).
-export([call/5]).
-export([call/6]).

%%
%% Types
%%

-type result() ::
    woody:result()
    | {exception, woody_error:business_error()}
    | no_return().

-export_type([result/0]).

%%
%% API
%%

-spec call(atom(), atom(), tuple(), woody:url()) -> result().
call(ServiceCode, Function, Args, RootUrl) ->
    call(ServiceCode, Function, Args, RootUrl, #{}).

-spec call(atom(), atom(), tuple(), woody:url(), woody_client:options() | map()) -> result().
call(ServiceCode, Function, Args, RootUrl, ExtraOpts) ->
    call(ServiceCode, Function, Args, RootUrl, ExtraOpts, woody_context:new()).

-spec call(atom(), atom(), tuple(), woody:url(), woody_client:options() | map(), woody_context:ctx()) -> result().
call(ServiceCode, Function, Args, RootUrl, ExtraOpts, WoodyContext) ->
    Request = {cds_thrift_services:thrift_service(ServiceCode), Function, Args},
    Path = genlib:to_binary(cds_thrift_services:service_path(ServiceCode)),
    CallOpts = maps:merge(ExtraOpts, #{
        url => <<RootUrl/binary, Path/binary>>,
        event_handler => cds_woody_event_handler
    }),
    case woody_client:call(Request, CallOpts, WoodyContext) of
        {ok, Result} ->
            Result;
        {exception, Exception} ->
            throw(Exception)
    end.
