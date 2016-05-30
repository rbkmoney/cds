-module(cds_card_data).

-export([marshall/1]).
-export([unmarshall/1]).
-export([unmarshall/2]).
-export([unique/1]).

-include("cds_cds_thrift.hrl").

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

unmarshall(Marshalled) ->
    unmarshall(Marshalled, <<>>).

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