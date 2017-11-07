-module(cds_card_validation_tests_SUITE).

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").

-export([all/0]).
-export([groups/0]).
-export([full_card_data_validation/1]).
-export([payment_system_detection/1]).

%%

%%
%% tests descriptions
%%

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
    CVV = <<"345">>,
    MC = #{
        cardnumber => <<"5321301234567892">>,
        exp_date   => {12, 3000},
        cardholder => <<"Benedict Wizardcock">>
    },
    {ok, #{
        payment_system := mastercard,
        iin            := <<"532130">>,
        last_digits    := <<"7892">>
    }} = cds_card_data:validate(MC, CVV),
    {error, {invalid, cardnumber, {length, _}}} = cds_card_data:validate(MC#{cardnumber := <<"53213012345678905">>}, CVV),
    {error, {invalid, cardnumber, luhn}}        = cds_card_data:validate(MC#{cardnumber := <<"5321301234567890">>}, CVV),
    {error, {invalid, exp_date, expiration}}    = cds_card_data:validate(MC#{exp_date   := {1, 2000}}, CVV),
    {error, {invalid, cvv, {length, _}}}        = cds_card_data:validate(MC, <<"12">>),
    MIR = #{
        cardnumber => <<"2204301234567891">>,
        exp_date   => {12, 3000},
        cardholder => <<"Benedict Wizardcock">>
    },
    {ok, #{
        payment_system := nspkmir,
        iin            := <<"22043012">>,
        last_digits    := <<"91">>
    }} = cds_card_data:validate(MIR, CVV),
    ok.

-spec payment_system_detection(config()) -> _.

payment_system_detection(_C) ->
    [
        {ok, #{payment_system := Target}} = cds_card_data:validate(Sample, CVV)
            || {Target, {Sample, CVV}} <- get_card_data_samples()
    ].

%%
%% helpers
%%

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
            {#{cardnumber => CN, exp_date => {1, 3000}, cardholder => undefined}, CVV}
        }
        end
    || {Target, CN, CVV} <- Samples].
