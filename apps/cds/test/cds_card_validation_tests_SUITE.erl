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
    ValidCard = #'CardData'{
        pan = <<"5321301234567892">>,
        exp_date = #'ExpDate'{
            month = 12,
            year = 3000
        },
        cardholder_name = <<"Benedict Wizardcock">>, %%
        cvv = <<"045">>
    },
    #'CardData'{pan = <<IIN:6/binary, _:6/binary, Mask:4/binary>>} = ValidCard,
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

-spec payment_system_detection(config()) -> _.

payment_system_detection(_C) ->
    [
        {Target, _, <<_Masked:4/binary>>} = cds_card_data:validate(Sample)
    || {Target, Sample} <- get_card_data_samples()].

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
            #'CardData'{pan = Pan, cvv = CVV, exp_date = #'ExpDate'{month = 1, year = 3000}}
        }
        end
    || {Target, Pan, CVV} <- Samples].
