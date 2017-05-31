-module(cds_card_data).

-export([validate/1]).
-export([marshall/1]).
-export([unmarshall/1]).
-export([unmarshall/2]).
-export([unique/1]).

-include("cds_cds_thrift.hrl").
-include("cds_domain_thrift.hrl").

-type card_data() :: cds_cds_thrift:'CardData'().
-type exp_date() :: cds_cds_thrift:'ExpDate'().

-spec validate(card_data()) ->
    {cds_iin_config:payment_system(), IIN :: binary(), MaskedNumber :: binary()} | no_return().
validate(#'CardData'{pan = <<IIN:6/binary, Number/binary>> = CN, exp_date = ExpDate, cvv = CVV}) ->
    case cds_iin_config:detect_ps(IIN) of
        unknown ->
            throw(invalid_card_data);
        PaymentSystem ->
            ValidationParameters = validation_parameters(PaymentSystem),
            CardData = #{
                card_number => CN,
                cvv => CVV,
                exp_date => ExpDate
            },
            _ = [assert(validate_algo(A, CardData)) || A <- ValidationParameters],

            {{YearNow, MonthNow, _D}, _T} = calendar:universal_time(),
            ok = assert(date_valid(ExpDate, {YearNow, MonthNow})),
            {PaymentSystem, IIN, get_masked_number(Number)}
    end.

-spec marshall(card_data()) -> {MarshalledCardData :: binary(), Cvv :: binary()}.
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
    {<<(byte_size(Pan)), Pan/binary, Month:8, Year:16, CardholderName/binary>>, Cvv}.

-spec unmarshall(MarshalledCardData :: binary()) -> card_data().
unmarshall(Marshalled) ->
    unmarshall(Marshalled, <<>>).

-spec unmarshall(MarshalledCardData :: binary(), Cvv :: binary()) -> card_data().
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

-spec unique(card_data() | binary()) -> Unqiue :: binary().
unique(#'CardData'{
    pan = Pan,
    exp_date = #'ExpDate'{
        month = Month,
        year = Year
    },
    cardholder_name = _CardholderName,
    cvv = _Cvv
}) ->
    %% TODO: validate
    <<(byte_size(Pan)), Pan/binary, Month:8, Year:16>>;
unique(<<PanSize, Pan:PanSize/binary, Month:8, Year:16, _/binary>>) ->
    <<PanSize, Pan:PanSize/binary, Month:8, Year:16>>.

% local

assert(true) ->
    ok;
assert(false) ->
    throw(invalid_card_data).

get_masked_number(Number) when byte_size(Number) >= 4 ->
    binary:part(Number, {byte_size(Number), -4});
get_masked_number(_) ->
    throw(invalid_card_data).

validate_algo({length, card_number, Lengths}, #{card_number := CN}) ->
    lists:member(byte_size(CN), Lengths);

validate_algo({length, cvv, Lengths}, #{cvv := CVV}) ->
    lists:member(byte_size(CVV), Lengths);

validate_algo(luhn, #{card_number := CN}) ->
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
luhn_valid(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            luhn_valid(Rest, Sum + M div 10 + M rem 10);
        M ->
            luhn_valid(Rest, Sum + M)
    end;
luhn_valid(<<N, Rest/binary>>, Sum) ->
    luhn_valid(Rest, Sum + N - $0).

-spec date_valid(exp_date(), {non_neg_integer(), 1..12}) -> true | false.
date_valid(#'ExpDate'{year = ExpYear, month = ExpMonth}, CurrentDate) ->
    {ExpYear, ExpMonth} >= CurrentDate.

validation_parameters(PaymentSystem) ->
    case cds_iin_config:get_validation_by_ps(PaymentSystem) of
        {ok, ValidationParameters} ->
            ValidationParameters;
        error ->
            error(validation_parameters_not_found)
    end.
