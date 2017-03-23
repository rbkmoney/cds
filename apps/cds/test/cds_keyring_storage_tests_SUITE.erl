-module(cds_keyring_storage_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [
        {group, env_storage_lifecycle},
        {group, file_storage_lifecycle}
    ].

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

init_per_group(env_storage_lifecycle, C) ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_env),
    C;

init_per_group(file_storage_lifecycle, C) ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_file),
    application:set_env(cds, keyring_path, keyring_storage_file_path(C)),
    C;

init_per_group(_, C) ->
    C.

end_per_group(_, _C) ->
    ok.

create(_C) ->
    Keyring = <<"initial">>,
    ok = cds_keyring_storage:create(Keyring).

already_exists(_C) ->
    Keyring = <<"bla">>,
    already_exists = (catch cds_keyring_storage:create(Keyring)).

read(_C) ->
    <<"initial">> = cds_keyring_storage:read().

update(_C) ->
    NewKeyring = <<"updated keyring">>,
    cds_keyring_storage:update(NewKeyring),
    NewKeyring = cds_keyring_storage:read().

delete(_C) ->
    ok = cds_keyring_storage:delete().

keyring_storage_file_path(C) ->
    filename:join([?config(priv_dir, C), "keyring"]).
