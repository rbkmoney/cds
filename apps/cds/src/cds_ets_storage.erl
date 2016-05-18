-module(cds_ets_storage).
-behaviour(cds_storage).
-behaviour(gen_server).

%% cds_storage behaviour
-export([start/0]).
-export([get/2]).
-export([put/3]).
-export([delete/2]).

%% gen_server behaviour
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([terminate/2]).
-export([handle_info/2]).
-export([code_change/3]).

%%
%% cds_storage behaviour
%%

-spec start() -> cds_backend:ok().
start() ->
    ChildSpec = #{
        id => ?MODULE,
        start => {gen_server, start_link, [?MODULE, [], []]}
    },
    case supervisor:start_child(cds, ChildSpec) of
        {ok, _Child} ->
            cds_backend:ok()
    end.

-spec get(atom(), binary()) -> cds_backend:response(binary()) | cds_backend:error(not_found).
get(Type, Key) ->
    case ets:lookup(resolve_bucket(Type), Key) of
        [{Key, Data}] ->
            cds_backend:response(Data);
        [] ->
            cds_backend:error(not_found)
    end.
    

-spec put(atom(), binary(), binary()) -> cds_backend:ok().
put(Type, Key, Data) ->
    case ets:insert(resolve_bucket(Type), {Key, Data}) of
        true ->
            cds_backend:ok()
    end.
    

-spec delete(atom(), binary()) -> cds_backend:ok().
delete(Type, Key) ->
    case ets:delete(resolve_bucket(Type), Key) of
        true ->
            cds_backend:ok()
    end.

%%
%% gen_server behaviour
%%

init([]) ->
    HashTable = resolve_bucket(hash),
    TokenTable = resolve_bucket(token),
    HashTable = ets:new(HashTable, [named_table, public]),
    TokenTable = ets:new(TokenTable, [named_table, public]),
    {ok, {}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%
%% Internal
%%

-spec resolve_bucket(atom()) -> binary().
resolve_bucket(hash) ->
    cds_ets_storage_hashes;
resolve_bucket(token) ->
    cds_ets_storage_tokens.
