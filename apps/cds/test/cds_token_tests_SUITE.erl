-module(cds_token_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("tds_proto/include/tds_proto_storage_thrift.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(TOKEN_ID, <<"some_id">>).
-define(TOKEN, #tds_Token{content = <<"some_content">>}).

%% common_test callbacks
-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests

-export([init/1]).
-export([token_not_found/1]).
-export([put_token/1]).
-export([get_token/1]).

-spec test() -> _.

%%
%% Internal types
%%

-type config() :: [{atom(), any()}] | atom().

%%
%% common_test callbacks
%%

-spec all() -> [{group, atom()}].
all() ->
    [
        {group, all_groups}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].
groups() ->
    [
        {all_groups, [], [
            {group, riak_storage_backend},
            {group, ets_storage_backend}
        ]},
        {riak_storage_backend, [], [{group, general_flow}]},
        {ets_storage_backend, [], [{group, general_flow}]},
        {general_flow, [], [
            {group, basic_token_lifecycle}
        ]},
        {basic_token_lifecycle, [sequence], [
            init,
            token_not_found,
            put_token,
            get_token
        ]}
    ].

-spec init_per_group(atom(), config()) -> config().
init_per_group(riak_storage_backend, C) ->
    cds_ct_utils:set_riak_storage(C);
init_per_group(ets_storage_backend, C) ->
    cds_ct_utils:set_ets_storage(C);
init_per_group(Group, C) when
    Group =:= all;
    Group =:= general_flow;
    Group =:= all_groups
->
    C;
init_per_group(_, C) ->
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear(C).

-spec end_per_group(atom(), config()) -> _.
end_per_group(Group, C) when
    Group =:= all;
    Group =:= ets_storage_backend;
    Group =:= riak_storage_backend;
    Group =:= general_flow;
    Group =:= all_groups
 ->
    C;
end_per_group(_, C) ->
    cds_ct_utils:stop_clear(C).

%%
%% Tests
%%

-spec init(config()) -> any() | no_return().
init(C) ->
    cds_ct_keyring:ensure_init(C).

-spec token_not_found(config()) -> any() | no_return().
token_not_found(C) ->
    ?assertException(
        throw,
        #tds_TokenNotFound{},
        cds_token_client:get_token(?TOKEN_ID, root_url(C))
    ).

-spec put_token(config()) -> any() | no_return().
put_token(C) ->
    ok = cds_token_client:put_token(?TOKEN_ID, ?TOKEN, root_url(C)).

-spec get_token(config()) -> any() | no_return().
get_token(C) ->
    ?TOKEN = cds_token_client:get_token(?TOKEN_ID, root_url(C)).

%%
%% Internals
%%

root_url(C) ->
    config(root_url, C).

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.
