-module(cds_keyring_storage_tests_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([groups/0]).
-export([init_per_group/2]).
-export([end_per_group/2]).

-export([create/1]).
-export([already_exists/1]).
-export([read/1]).
-export([update/1]).
-export([delete/1]).

-type config() :: term().

-spec all() -> [{group, atom()}].

all() ->
    [
        {group, env_storage_lifecycle},
        {group, file_storage_lifecycle}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].

groups() ->
    [
        {env_storage_lifecycle, [sequence],[
            create,
            already_exists,
            read,
            update,
            delete
        ]},
        {file_storage_lifecycle, [sequence],[
            create,
            already_exists,
            read,
            update,
            delete
        ]}
    ].

%%
%% starting/stopping
%%

-spec init_per_group(atom(), config()) -> config().

init_per_group(env_storage_lifecycle, C) ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_env),
    C;

init_per_group(file_storage_lifecycle, C) ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_file),
    application:set_env(cds, keyring_path, keyring_storage_file_path(C)),
    C;

init_per_group(_, C) ->
    C.

-spec end_per_group(atom(), config()) -> config().

end_per_group(_, _C) ->
    ok.

-spec create(config()) -> _.

create(_C) ->
    Keyring = <<"initial">>,
    ok = cds_keyring_storage:create(Keyring).

-spec already_exists(config()) -> _.

already_exists(_C) ->
    Keyring = <<"bla">>,
    already_exists = (catch cds_keyring_storage:create(Keyring)).

-spec read(config()) -> _.

read(_C) ->
    <<"initial">> = cds_keyring_storage:read().

-spec update(config()) -> _.

update(_C) ->
    NewKeyring = <<"updated keyring">>,
    cds_keyring_storage:update(NewKeyring),
    NewKeyring = cds_keyring_storage:read().

-spec delete(config()) -> _.

delete(_C) ->
    ok = cds_keyring_storage:delete().

-spec keyring_storage_file_path(config()) -> _.

keyring_storage_file_path(C) ->
    filename:join([?config(priv_dir, C), "keyring"]).
