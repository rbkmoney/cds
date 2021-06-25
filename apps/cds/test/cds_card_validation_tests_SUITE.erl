-module(cds_card_validation_tests_SUITE).

-include_lib("damsel/include/dmsl_cds_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([full_card_data_validation/1]).
-export([payment_system_detection/1]).

%%

%%
%% tests descriptions
%%

-define(AUTH_CVV(CVV), #{auth_data => #{cvv => CVV}}).

-type config() :: term().

-spec all() -> [{group, atom()}].
all() ->
    [
        {group, card_data_validation}
    ].

-spec groups() -> [{atom(), list(), [atom()]}].
groups() ->
    [
        {card_data_validation, [parallel], [
            full_card_data_validation,
            payment_system_detection
        ]}
    ].

%%
%% tests
%%

-spec full_card_data_validation(config()) -> _.
full_card_data_validation(_C) ->
    MC = #{
        cardnumber => <<"5321301234567892">>,
        exp_date => {12, 3000},
        cardholder => <<"Benedict Wizardcock">>
    },
    {ok, #{
        payment_system := mastercard,
        iin := <<"532130">>,
        last_digits := <<"7892">>
    }} = cds_card_data:get_card_info(MC),
    MIR = #{
        cardnumber => <<"2204301234567891">>,
        exp_date => {12, 3000},
        cardholder => <<"Benedict Wizardcock">>
    },
    {ok, #{
        payment_system := nspkmir,
        iin := <<"22043012">>,
        last_digits := <<"91">>
    }} = cds_card_data:get_card_info(MIR),
    {ok, #{
        payment_system := mastercard,
        iin := <<"532130">>,
        last_digits := <<"7892">>
    }} = cds_card_data:get_card_info(MC),
    ok.

-spec payment_system_detection(config()) -> _.
payment_system_detection(_C) ->
    [
        {ok, #{payment_system := Target}} = cds_card_data:get_card_info(Sample)
        || {Target, Sample} <- get_card_data_samples()
    ].

%%
%% helpers
%%

get_card_data_samples() ->
    Samples = [
        {amex, <<"378282246310005">>},
        {amex, <<"371449635398431">>},
        {amex, <<"378734493671000">>},
        {dinersclub, <<"30569309025904">>},
        {dinersclub, <<"38520000023237">>},
        {dinersclub, <<"36213154429663">>},
        {discover, <<"6011111111111117">>},
        {discover, <<"6011000990139424">>},
        {jcb, <<"3530111333300000">>},
        {jcb, <<"3566002020360505">>},
        {mastercard, <<"5555555555554444">>},
        {mastercard, <<"5105105105105100">>},
        {visa, <<"4716219619821724">>},
        {visa, <<"4929221444411666">>},
        {visa, <<"4929003096554179">>},
        {visaelectron, <<"4508085628009599">>},
        {visaelectron, <<"4508964269455370">>},
        {visaelectron, <<"4026524202025897">>},
        {unionpay, <<"6279227608204863">>},
        {unionpay, <<"6238464198841867">>},
        {unionpay, <<"6263242460178483">>},
        {dankort, <<"5019717010103742">>},
        {nspkmir, <<"2202243736741990">>},
        {nspkmir, <<"2200330595609485549">>},
        {forbrugsforeningen, <<"6007220000000004">>}
    ],
    [
        begin
            {
                Target,
                #{cardnumber => CN}
            }
        end
        || {Target, CN} <- Samples
    ].
