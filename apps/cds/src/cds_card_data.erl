-module(cds_card_data).

-export([validate/1]).
-export([marshall/1]).
-export([unmarshall/1]).
-export([unmarshall/2]).
-export([unique/1]).

-include("cds_cds_thrift.hrl").
-include("cds_domain_thrift.hrl").


-spec validate(cds_cds_thrift:'CardData'()) ->
    {cds_iin_config:payment_system(), IIN :: binary(), MaskedNumber :: binary()} | no_return().
validate(#'CardData'{pan = <<IIN:6/binary, _Skip:6/binary, Masked/binary>> = CN, exp_date = ExpDate, cvv = CVV}) ->
    case detect_ps(IIN) of
        unknown ->
            throw(invalid_card_data);
        PaymentSystem ->
            #{
                length := Length,
                cvv_length := CvvLength,
                validation := AdditionalChecks
            } = validation_parameters(PaymentSystem),
            ok = assert(lists:member(size(CN), Length)),
            ok = assert(lists:member(size(CVV), CvvLength)),
            _ = [assert(validate_algo(A, CN)) || A <- AdditionalChecks],
            ok = assert(date_valid(ExpDate)),
            {PaymentSystem, IIN, Masked}
    end.

-spec marshall(#'CardData'{}) -> {MarshalledCardData :: binary(), Cvv :: binary()}.
marshall(CardData) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName,
        cvv = Cvv
    } = CardData,
    %% TODO: validate
    {<<(size(Pan)), Pan/binary, Month:8, Year:16, CardholderName/binary>>, Cvv}.

-spec unmarshall(MarshalledCardData :: binary()) -> #'CardData'{}.
unmarshall(Marshalled) ->
    unmarshall(Marshalled, <<>>).

-spec unmarshall(MarshalledCardData :: binary(), Cvv :: binary()) -> #'CardData'{}.
unmarshall(<<PanSize, Pan:PanSize/binary, Month:8, Year:16, CardholderName/binary>>, Cvv) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName,
        cvv = Cvv
    }.

-spec unique(#'CardData'{}) -> Unqiue :: binary().
unique(CardData) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = _CardholderName,
        cvv = _Cvv
    } = CardData,
    %% TODO: validate
    <<(size(Pan)), Pan/binary, Month:8, Year:16>>.

% local

assert(true) ->
    ok;
assert(false) ->
    throw(invalid_card_data).

validate_algo(luhn, CN) ->
    luhn_valid(CN).

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
    case cds_iin_config:get_ps_by_iin(IIN) of
        {ok, PaymentSystem} ->
            PaymentSystem;
        error ->
            detect_ps(binary:part(IIN, {0, size(IIN) - 1}))
    end.

date_valid(#'ExpDate'{year = ExpYear, month = ExpMonth}) ->
    {{YearNow, MonthNow, _D}, _T} = calendar:universal_time(),
    {ExpYear, ExpMonth} >= {YearNow, MonthNow}.

validation_parameters(PaymentSystem) ->
    case cds_iin_config:get_validation_by_ps(PaymentSystem) of
        {ok, ValidationParameters} ->
            ValidationParameters;
        error ->
            error(validation_parameters_not_found)
    end.
