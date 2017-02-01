-module(cds_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("cds/src/cds_cds_thrift.hrl").
-compile(export_all).

-define(config(K, C), begin element(2, lists:keyfind(K, 1, C)) end).
-define(root_url(C), ?config(root_url, C)).

%%

-define(CVV, <<"777">>).
-define(CREDIT_CARD(CVV), #'CardData'{
    pan = <<"5321301234567892">>,
    exp_date = #'ExpDate'{
        month = 12,
        year = 3000
    },
    cardholder_name = <<"Tony Stark">>, %% temporarily hardcoded instead of saved
    cvv = CVV
}).

%%
%% tests descriptions
%%
all() ->
    [
        {group, basic_lifecycle},
        {group, keyring_errors},
        {group, card_data_validation}
    ].

groups() ->
    [
        {basic_lifecycle, [sequence], [
            init,
            lock,
            unlock,
            put,
            get,
            rotate
        ]},
        {keyring_errors, [parallel], [
            init_keyring_exists,
            rotate_keyring_locked,
            get_card_data_keyring_locked,
            get_session_card_data_keyring_locked
        ]},
        {card_data_validation, [parallel], [
            full_card_data_validation,
            payment_system_detection
        ]}
    ].
%%
%% starting/stopping
%%
init_per_suite(C) ->
    timer:sleep(10000), %% sleep again ;(
    C.

init_per_group(keyring_errors, C) ->
    C1 = start_clear(),
    _MasterKeys = cds_client:init(2, 3, ?root_url(C1)),
    ok = cds_client:lock(?root_url(C1)),
    C1 ++ C;
init_per_group(_, C) ->
    C1 = start_clear(),
    C1 ++ C.

end_per_group(_, C) ->
    cds_keyring_storage_env:delete(),
    [ok = application:stop(App) || App <- ?config(apps, C)].

%%
%% tests
%%
init(C) ->
    MasterKeys = cds_client:init(2, 3, ?root_url(C)),
    3 = length(MasterKeys),
    {save_config, MasterKeys}.

lock(C) ->
    {init, MasterKeys} = ?config(saved_config, C),
    ok = cds_client:lock(?root_url(C)),
    #'KeyringLocked'{} = (catch cds_client:put(?CREDIT_CARD(?CVV), ?root_url(C))),
    {save_config, MasterKeys}.

unlock(C) ->
    {lock, [MasterKey1, MasterKey2, _MasterKey3]} = ?config(saved_config, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1, ?root_url(C)),
    {unlocked, #'Unlocked'{}} = cds_client:unlock(MasterKey2, ?root_url(C)),
    ok.

put(C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        }
    } = cds_client:put(?CREDIT_CARD(?CVV), ?root_url(C)),
    {save_config, Token}.

get(C) ->
    {put, Token} = ?config(saved_config, C),
    ?CREDIT_CARD(<<>>) = cds_client:get(Token, ?root_url(C)),
    ok.

rotate(C) ->
    ok = cds_client:rotate(?root_url(C)),
    ok.

init_keyring_exists(C) ->
    #'KeyringExists'{} = (catch cds_client:init(2, 3, ?root_url(C))).

rotate_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate(?root_url(C))).

get_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get(<<"No matter what">>, ?root_url(C))).

get_session_card_data_keyring_locked(C) ->
    #'KeyringLocked'{} = (catch cds_client:get_session(
        <<"No matter what">>,
        <<"No matter what">>,
        ?root_url(C))
    ).


full_card_data_validation(_C) ->
    #'CardData'{pan = <<IIN:6/binary, _:6/binary, Mask/binary>>} = ValidCard = ?CREDIT_CARD(?CVV),
    {mastercard, IIN, Mask} = cds_card_data:validate(ValidCard),
    %%length
    invalid_card_data = (catch cds_card_data:validate(ValidCard#'CardData'{pan = <<"53213012345678905">>})),
    %%luhn
    invalid_card_data = (catch cds_card_data:validate(ValidCard#'CardData'{pan = <<"5321301234567890">>})),
    %%expiration
    invalid_card_data = (catch cds_card_data:validate(ValidCard#'CardData'{exp_date = #'ExpDate'{month = 1, year = 2000}})),
    %%cvv length
    invalid_card_data = (catch cds_card_data:validate(ValidCard#'CardData'{cvv = <<"12">>})),
    ok.

payment_system_detection(_C) ->
    [
        {Target, _, _} = cds_card_data:validate(Sample)
    || {Target, Sample} <- get_card_data_samples()].

%%
%% helpers
%%

start_clear() ->
    IP = "::1",
    Port = 8022,
    RootUrl = "http://[" ++ IP ++ "]:" ++ integer_to_list(Port),
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {handlers, [
                {lager_common_test_backend, [debug, false]}
            ]}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, "::1"},
            {port, 8022},
            {keyring_storage, cds_keyring_storage_env},
            {storage, cds_storage_riak},
            {cds_storage_riak, #{
                conn_params => {"riakdb", 8087}
            }}
        ]),
    [{apps, Apps}, {root_url, RootUrl}].

get_card_data_samples() ->
    Samples = [
        {amex               , <<"378282246310005">>  , <<"228">>  },
        {amex               , <<"371449635398431">>  , <<"3434">> },
        {amex               , <<"378734493671000">>  , <<"228">>  },
        {dinersclub         , <<"30569309025904">>   , <<"228">>  },
        {dinersclub         , <<"38520000023237">>   , <<"228">>  },
        {dinersclub         , <<"36213154429663">>   , <<"228">>  },
        {discover           , <<"6011111111111117">> , <<"228">>  },
        {discover           , <<"6011000990139424">> , <<"228">>  },
        {jcb                , <<"3530111333300000">> , <<"228">>  },
        {jcb                , <<"3566002020360505">> , <<"228">>  },
        {mastercard         , <<"5555555555554444">> , <<"228">>  },
        {mastercard         , <<"5105105105105100">> , <<"228">>  },
        {visa               , <<"4716219619821724">> , <<"228">>  },
        {visa               , <<"4929221444411666">> , <<"228">>  },
        {visa               , <<"4929003096554179">> , <<"228">>  },
        {visaelectron       , <<"4508085628009599">> , <<"228">>  },
        {visaelectron       , <<"4508964269455370">> , <<"228">>  },
        {visaelectron       , <<"4026524202025897">> , <<"228">>  },
        {unionpay           , <<"6279227608204863">> , <<"228">>  },
        {unionpay           , <<"6238464198841867">> , <<"228">>  },
        {unionpay           , <<"6263242460178483">> , <<"228">>  },
        {dankort            , <<"5019717010103742">> , <<"228">>  },
        {nspkmir            , <<"2202243736741990">> , <<"228">>  },
        {forbrugsforeningen , <<"6007220000000004">> , <<"228">>  }
    ],
    [
        begin
        {
            Target,
            #'CardData'{pan = Pan, cvv = CVV, exp_date = #'ExpDate'{month = 1, year = 3000}}
        }
        end
    || {Target, Pan, CVV} <- Samples].
