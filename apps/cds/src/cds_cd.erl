-module(cds_cd).

-export([validate/1]).

-include("cds_domain_thrift.hrl").
-include("cds_cds_thrift.hrl").
-include("cds_domain_config_thrift.hrl").


-spec validate(cds_cds_thrift:'CardData'()) -> {binary(), binary(), binary()}.
validate(#'CardData'{pan = <<IIN:6/binary, _Skip:6/binary, Masked/binary>> = CN, exp_date = ExpDate, cvv = CVV}) ->
    case detect_ps(IIN) of
        unknown ->
            throw(invalid_card_data);
        PaymentSystem ->
            #{length := Length, cvv_length := CvvLength, luhn := Luhn} = validation_parameters(PaymentSystem),
            ok = assert(lists:member(size(CN), Length)),
            ok = assert(lists:member(size(CVV), CvvLength)),
            ok = assert(luhn_check(Luhn, CN)),
            ok = assert(date_valid(ExpDate)),
            {PaymentSystem, IIN, Masked}
    end.

assert(true) ->
    ok;
assert(false) ->
    throw(invalid_card_data).

luhn_check(CheckNeeded, CN) ->
    case CheckNeeded of
        false ->
            true;
        true ->
            luhn_valid(CN)
    end.

luhn_valid(CN) ->
    luhn_valid(CN, 0).

luhn_valid(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
luhn_valid(<<N, Rest/binary>>, Sum) when size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            luhn_valid(Rest, Sum + M div 10 + M rem 10);
        M ->
            luhn_valid(Rest, Sum + M)
    end;
luhn_valid(<<N, Rest/binary>>, Sum) ->
    luhn_valid(Rest, Sum + N - $0).

detect_ps(<<>>) ->
    unknown;
detect_ps(IIN) ->
    case application:get_env(cds, iin_map, #{}) of
        #{IIN := PaymentSystem} ->
            PaymentSystem;
        _ ->
            detect_ps(binary:part(IIN, {0, size(IIN) - 1}))
    end.

date_valid(#'ExpDate'{year = ExpYear, month = ExpMonth}) ->
    {{YearNow, MonthNow, _D}, _T} = calendar:universal_time(),
    {ExpYear, ExpMonth} >= {YearNow, MonthNow}.

validation_parameters(PaymentSystem) ->
    case application:get_env(cds, validation_parameters, #{}) of
        #{PaymentSystem := ValidationParameters} ->
            ValidationParameters;
        _ ->
            error(validation_parameters_not_found)
    end.
