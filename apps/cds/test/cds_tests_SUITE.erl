-module(cds_tests_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("cds/src/cds_cds_thrift.hrl").
-compile(export_all).

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
application_stop(App=sasl) ->
    %% hack for preventing sasl deadlock
    %% http://erlang.org/pipermail/erlang-questions/2014-May/079012.html
    error_logger:delete_report_handler(cth_log_redirect),
    application:stop(App),
    error_logger:add_report_handler(cth_log_redirect),
    ok;
application_stop(App) ->
    application:stop(App).

init_per_suite(C) ->
    timer:sleep(30000), %% wait for riak cluster to stabilize
    C.

init_per_group(keyring_errors, C) ->
    {ok, Apps} = clear_start(),
    cds_client:init(2,3),
    ok = cds_client:lock(),
    [{apps, Apps} | C];

init_per_group(_, C) ->
    {ok, Apps} = clear_start(),
    [{apps, Apps} | C].

end_per_group(_, C) ->
    cds_keyring_storage_env:delete(),
    [application_stop(App) || App <- proplists:get_value(apps, C)].

%%
%% tests
%%
init(_C) ->
    MasterKeys = cds_client:init(2,3),
    3 = length(MasterKeys),
    {save_config, MasterKeys}.

lock(C) ->
    {init, MasterKeys} = ?config(saved_config, C),
    ok = cds_client:lock(),
    #'KeyringLocked'{} = (catch cds_client:put(?CREDIT_CARD(?CVV))),
    {save_config, MasterKeys}.

unlock(C) ->
    {lock, [MasterKey1, MasterKey2, _MasterKey3]} = ?config(saved_config, C),
    {more_keys_needed, 1} = cds_client:unlock(MasterKey1),
    {ok, #'Ok'{}} = cds_client:unlock(MasterKey2),
    ok.

put(_C) ->
    #'PutCardDataResult'{
        bank_card = #'BankCard'{
            token = Token
        }
    } = cds_client:put(?CREDIT_CARD(?CVV)),
    timer:sleep(10000), %% wait a sec
    {save_config, Token}.

get(C) ->
    {put, Token} = ?config(saved_config, C),
    ?CREDIT_CARD(<<>>) = cds_client:get(Token),
    ok.

rotate(_C) ->
    ok = cds_client:rotate(),
    ok.

init_keyring_exists(_C) ->
    #'KeyringExists'{} = (catch cds_client:init(2,3)).

rotate_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:rotate()).

get_card_data_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:get(<<"No matter what">>)).

get_session_card_data_keyring_locked(_C) ->
    #'KeyringLocked'{} = (catch cds_client:get_session(<<"No matter what">>, <<"No matter what">>)).


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

clear_start() ->
    test_configuration(),
    application:ensure_all_started(cds).

test_configuration() ->
    application:set_env(cds, keyring_storage, cds_keyring_storage_env),
    application:set_env(cds, storage, cds_storage_riak),
    application:set_env(cds, cds_storage_riak, #{
        conn_params => [
            {riak1, "riak1", 8087},
            {riak2, "riak2", 8087},
            {riak3, "riak3", 8087},
            {riak4, "riak4", 8087},
            {riak5, "riak5", 8087}
        ]
    }).

get_card_data_samples() ->
    Samples = [
        {amex, <<"378282246310005">>, <<"228">>},
        {amex, <<"371449635398431">>, <<"3434">>},
        {amex, <<"378734493671000">>, <<"228">>},
        {dinersclub, <<"30569309025904">>, <<"228">>},
        {dinersclub, <<"38520000023237">>, <<"228">>},
        {dinersclub, <<"36213154429663">>, <<"228">>},
        {discover, <<"6011111111111117">>, <<"228">>},
        {discover, <<"6011000990139424">>, <<"228">>},
        {jcb, <<"3530111333300000">>, <<"228">>},
        {jcb, <<"3566002020360505">>, <<"228">>},
        {mastercard, <<"5555555555554444">>, <<"228">>},
        {mastercard, <<"5105105105105100">>, <<"228">>},
        {visa, <<"4716219619821724">>, <<"228">>},
        {visa, <<"4929221444411666">>, <<"228">>},
        {visa, <<"4929003096554179">>, <<"228">>},
        {visaelectron, <<"4508085628009599">>, <<"228">>},
        {visaelectron, <<"4508964269455370">>, <<"228">>},
        {visaelectron, <<"4026524202025897">>, <<"228">>},
        {unionpay, <<"6279227608204863">>, <<"228">>},
        {unionpay, <<"6238464198841867">>, <<"228">>},
        {unionpay, <<"6263242460178483">>, <<"228">>},
        {dankort, <<"5019717010103742">>, <<"228">>},
        {nspkmir, <<"2202243736741990">>, <<"228">>},
        {forbrugsforeningen, <<"6007220000000004">>, <<"228">>}
    ],

    [
        begin
        {
            Target,
            #'CardData'{pan = Pan, cvv = CVV, exp_date = #'ExpDate'{month = 1, year = 3000}}
        }
        end
    || {Target, Pan, CVV} <- Samples].
