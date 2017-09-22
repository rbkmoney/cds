-module(cds_card_data).

-export([validate/1]).
-export([marshall/1]).
-export([unmarshall/1]).
-export([unique/1]).

-export_type([card_data/0]).
-export_type([cardholder_data/0]).
-export_type([cvv/0]).
-export_type([unique_card_data/0]).

-type card_data() :: {cardholder_data(), cvv()}.
-type cardholder_data() :: binary().
-type cvv() :: binary().
-type unique_card_data() :: binary().
%% TODO something wrong with this type
-type thrift_card_data() :: dmsl_cds_thrift:'CardData'().

-include_lib("dmsl/include/dmsl_cds_thrift.hrl").


-spec validate(thrift_card_data()) ->
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

-spec marshall(thrift_card_data()) -> card_data().
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
    Cardholder = marshall_cardholder(CardholderName),
    {<<(byte_size(Pan)), Pan/binary, Month:8, Year:16, Cardholder/binary>>, Cvv}.

-spec unmarshall(card_data() | cardholder_data()) -> thrift_card_data().
unmarshall(Marshalled) when is_binary(Marshalled)->
    unmarshall({Marshalled, <<>>});

unmarshall({<<PanSize, Pan:PanSize/binary, Month:8, Year:16, Cardholder/binary>>, Cvv}) ->
    CardholderName = unmarshall_cardholder(Cardholder),
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = CardholderName,
        cvv = Cvv
    }.

-spec unique(thrift_card_data() | cardholder_data()) -> unique_card_data().
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

-spec date_valid(dmsl_cds_thrift:'ExpDate'(), {non_neg_integer(), 1..12}) -> true | false.
date_valid(#'ExpDate'{year = ExpYear, month = ExpMonth}, CurrentDate) ->
    {ExpYear, ExpMonth} >= CurrentDate.

validation_parameters(PaymentSystem) ->
    case cds_iin_config:get_validation_by_ps(PaymentSystem) of
        {ok, ValidationParameters} ->
            ValidationParameters;
        error ->
            error(validation_parameters_not_found)
    end.

marshall_cardholder(CardholderName) when CardholderName =/= undefined ->
    CardholderName;
marshall_cardholder(undefined) ->
    <<"">>.

unmarshall_cardholder(CardholderName) when CardholderName =/= <<"">> ->
    CardholderName;
unmarshall_cardholder(<<"">>) ->
    undefined.
