-module(cds_thrift_handler).
-behaviour(woody_server_thrift_handler).
-behaviour(woody_event_handler).

-include("cds_thrift.hrl").

%% woody_server_thrift_handler callbacks
-export([handle_function/4]).
-export([handle_error/4]).

%% woody_event_handler callback
-export([handle_event/3]).


-spec handle_function(
    woody_t:func(),
    woody_server_thrift_handler:args(),
    woody_client:context(),
    woody_server_thrift_handler:handler_opts()
) -> ok | {ok, woody_server_thrift_handler:result()} | no_return().
handle_function('Init', {Threshold, Count}, _Context, _Opts) ->
    try cds:init_keyring(Threshold, Count) of
        Shares ->
            {ok, Shares}
    catch
        already_exists ->
            throw(#'KeyringExists'{})
    end;
handle_function('Unlock', {Share}, _Context, _Opts) ->
    case cds:unlock_keyring(Share) of
        {more, More} ->
            {ok, {more_keys_needed, More}};
        unlocked ->
            {ok, {unlocked, true}}
    end;
handle_function('Rotate', {}, _Context, _Opts) ->
    try cds:rotate_keyring() of
        ok ->
            ok
    catch
        locked ->
            throw(#'Locked'{})
    end;
handle_function('Lock', {}, _Context, _Opts) ->
    ok = cds:lock_keyring(),
    ok;
handle_function('GetCardData', {Token}, _Context, _Opts) ->
    try cds:get(base64:decode(Token)) of
        MarshalledCardData ->
            {ok, unmarshall_card_data(MarshalledCardData)}
    catch
        not_found ->
            throw(#'NotFound'{});
        locked ->
            throw(#'Locked'{})
    end;
handle_function('GetSessionCardData', {Token, Session}, _Context, _Opts) ->
    try cds:get(base64:decode(Token)) of
            MarshalledCardData ->
                CardData = unmarshall_card_data(MarshalledCardData),
                {ok, CardData#'CardData'{cvv = Session}}
    catch
        not_found ->
            throw(#'NotFound'{});
        locked ->
            throw(#'Locked'{})
    end;
handle_function('PutCardData', {CardData}, _Context, _Opts) ->
    %% TODO: store cardholder name, but hash only pan + expdate
    MarshalledCardData = marshall_card_data(CardData),
    try cds:put(MarshalledCardData) of
        Token ->
            BankCard = #'BankCard'{
                token = base64:encode(Token),
                payment_system = visa,
                bin = <<"123456">>,
                masked_pan = <<"1234">>
            },
            {ok, #'PutCardDataResult'{bank_card = BankCard, session = CardData#'CardData'.cvv}}
    catch
        locked ->
            throw(#'Locked'{})
    end.

-spec handle_error(
    woody_t:func(),
    woody_server_thrift_handler:error_reason(),
    woody_t:rpc_id(),
    woody_server_thrift_handler:handler_opts()
) -> _.

handle_error(Function, Error, RpcId, _Opts) ->
    lager:info("[~p] (~p) got error from thrift: ~p", [RpcId, Function, Error]).

handle_event(Event, RpcId, Meta) ->
    lager:info("[~p] woody event ~p ~p~n", [RpcId, Event, Meta]).

%%
%% internal
%%

marshall_card_data(CardData) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = _CardholderName,
        cvv = _CVV
    } = CardData,
    %% TODO: validate
    <<(size(Pan)), Pan/binary, Month:8, Year:16>>.

unmarshall_card_data(<<PanSize, Pan:PanSize/binary, Month:8, Year:16>>) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = Month,
            year = Year
        },
        cardholder_name = <<"Tony Stark">>,
        cvv = <<>>
    }.