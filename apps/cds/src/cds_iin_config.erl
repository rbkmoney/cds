-module(cds_iin_config).

-export([detect_ps/1]).
-export([get_validation_by_ps/1]).
-export([get_ps_by_iin/1]).

-type payment_system() :: atom().
-type validation_algo() ::
    {length, card_number, [pos_integer()]} |
    {length, cvv, [pos_integer()]} |
    luhn.

-type validation() :: [validation_algo()].

-export_type([payment_system/0]).

-spec get_validation_by_ps(payment_system()) -> {ok, validation()} | error.
get_validation_by_ps(PaymentSystem) ->
    case get_validation_map() of
        #{PaymentSystem := ValidationParams} ->
            {ok, ValidationParams};
        _ ->
            error
    end.

-spec get_ps_by_iin(IIN :: binary()) -> {ok, payment_system()} | error.
get_ps_by_iin(IIN) ->
    case get_inn_map() of
        #{IIN := PaymentSystem} ->
            {ok, PaymentSystem};
        _ -> error
    end.

-spec detect_ps(IIN :: binary) -> payment_system() | unknown.
detect_ps(<<>>) ->
    unknown;
detect_ps(IIN) ->
    case cds_iin_config:get_ps_by_iin(IIN) of
        {ok, PaymentSystem} ->
            PaymentSystem;
        error ->
            detect_ps(binary:part(IIN, {0, size(IIN) - 1}))
    end.

% local

get_validation_map() ->
    #{
        visaelectron => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ],
        maestro => [
            {length, card_number, [12, 13, 14, 15, 16, 17, 18, 19]},
            {length, cvv, [3]},
            luhn
        ],
        forbrugsforeningen => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ],
        dankort => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ],
        visa => [
            {length, card_number, [13, 16]},
            {length, cvv, [3]},
            luhn
        ],
        mastercard => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ],
        amex => [
            {length, card_number, [15]},
            {length, cvv, [3, 4]},
            luhn
        ],
        dinersclub => [
            {length, card_number, [14]},
            {length, cvv, [3]},
            luhn
        ],
        discover => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ],
        unionpay => [
            {length, card_number, [16, 17, 18, 19]},
            {length, cvv, [3]}
        ],
        jcb => [
            {length, card_number, [16]},
            {length, cvv, [3]},
            luhn
        ]
    }.

get_inn_map() ->
    #{
        <<"4026">> => visaelectron,
        <<"417500">> => visaelectron,
        <<"4405">> => visaelectron,
        <<"4508">> => visaelectron,
        <<"4844">> => visaelectron,
        <<"4913">> => visaelectron,
        <<"4917">> => visaelectron,

        <<"5018">> => maestro,
        <<"502">> => maestro,
        <<"503">> => maestro,
        <<"506">> => maestro,
        <<"56">> => maestro,
        <<"58">> => maestro,
        <<"639">> => maestro,
        <<"6220">> => maestro,
        <<"67">> => maestro,

        <<"600">> => forbrugsforeningen,

        <<"5019">> => dankort,

        <<"4">> => visa,

        <<"51">> => mastercard,
        <<"52">> => mastercard,
        <<"53">> => mastercard,
        <<"54">> => mastercard,
        <<"55">> => mastercard,
        <<"22">> => mastercard,
        <<"23">> => mastercard,
        <<"24">> => mastercard,
        <<"25">> => mastercard,
        <<"26">> => mastercard,
        <<"27">> => mastercard,

        <<"34">> => amex,
        <<"37">> => amex,

        <<"30">> => dinersclub,
        <<"36">> => dinersclub,
        <<"38">> => dinersclub,
        <<"39">> => dinersclub,

        <<"60">> => discover,
        <<"64">> => discover,
        <<"65">> => discover,
        <<"622">> => discover,

        <<"62">> => unionpay,
        <<"88">> => unionpay,

        <<"35">> => jcb,

        <<"2200">> => nspkmir,
        <<"2201">> => nspkmir,
        <<"2202">> => nspkmir,
        <<"2203">> => nspkmir
    }.
