-module(cds_cross_clients_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("cds.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([init/1]).
-export([put_card_data_v2/1]).
-export([get_card_data_v1_encode_error/1]).
-export([get_card_data_v2/1]).

%%
%% tests descriptions
%%

-type config() :: term().

-spec test() -> _.

-spec all() -> [{group, atom()}].

all() ->
    [
        {group, v1_get_new_card_data}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].

groups() ->
    [
        {v1_get_new_card_data, [], [
            init,
            put_card_data_v2,
            get_card_data_v2,
            get_card_data_v1_encode_error
        ]}
    ].
%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(v1_get_new_card_data, C) ->
    C1 = cds_ct_utils:set_ets_storage(C),
    ok = cds_ct_utils:start_stash(),
    cds_ct_utils:start_clear(C1).

-spec end_per_group(atom(), config()) -> _.

end_per_group(_, C) ->
    cds_ct_utils:stop_clear(C).

%%
%% tests
%%

-spec init(config()) -> _.

init(C) ->
    ok = cds_ct_keyring:ensure_init(C),
    ok = cds_ct_utils:wait_for_keyring(C).

-spec put_card_data_v2(config()) -> _.

put_card_data_v2(C) ->
    % check without cardholder
    CardData = #{
        pan => <<"5321301234567892">>
    },
    #{bank_card := #{token := Token}} = cds_card_v2_client:put_card(
        CardData,
        root_url(C)
    ),
    cds_ct_utils:store([{token, Token}]).

-spec get_card_data_v1_encode_error(config()) -> _.

get_card_data_v1_encode_error(C) ->
    ?assertMatch(
        {'EXIT',
            {{woody_error, {external,result_unexpected,
            <<"error:function_clause in call to cds_card_v1_thrift_handler:encode_cardholder_data(#{cardnumber => '***'})", _/binary>>}},
            _ST}},
        (catch cds_card_v1_client:get_card_data(
            cds_ct_utils:lookup(token),
            root_url(C)
        ))
    ).

-spec get_card_data_v2(config()) -> _.

get_card_data_v2(C) ->
    ?assertEqual(
        cds_card_v2_client:get_test_card(<<>>),
        cds_card_v2_client:get_card_data(
            cds_ct_utils:lookup(token),
            root_url(C)
        )
    ).

%%
%% helpers
%%


config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

root_url(C) ->
    config(root_url, C).
