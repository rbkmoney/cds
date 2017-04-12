-module(cds_recrypter).

-behaviour(cds_periodic_job).

%% api

-export([start_link/1]).

%% cds_periodic_job callbacks

-export([init/1]).
-export([handle_timeout/1]).

-define(DEFAULT_INTERVAL, 3000).
-define(DEFAULT_BATCH_SIZE, 5000).

-type state() :: #{
    encoding_type := cvv | card_data
}.

start_link(Options) ->
    cds_periodic_job:start_link(?MODULE, [Options]).

-spec init(_) -> {ok, non_neg_integer(), state()}.

init(#{encoding_type := EncodingType}) when
    EncodingType =:= cvv;
    EncodingType =:= card_data
->
    _ = lager:info("Starting recrypter for ~p...", [EncodingType]),
    _ = cds_utils:logtag_process(encoding_type, EncodingType),
    {ok, get_interval(), #{encoding_type => EncodingType}}.

handle_timeout(State = #{encoding_type := EncodingType}) ->
    _ = lager:info("Starting recrypting", []),

    case process_recrypting(EncodingType, get_batch_size()) of
        {ok, done} ->
            _ = lager:info("Recrypted all"),
            {ok, done, State};
        {ok, more} ->
            _ = lager:info("Recrypted some items. Need to repeat"),
            {ok, more, State};
        {error, Error} ->
            _ = lager:error("Recrypting error: ~p", [Error]),
            {error, Error, State}
    end.

process_recrypting(EncodingType, BatchSize) ->
    Intervals = cds:get_outdated_encrypting_keys(),
    process_recrypting(EncodingType, Intervals, BatchSize).

process_recrypting(_, [], _) ->
    {ok, done};

process_recrypting(_, _, Left) when
    Left =< 0
->
    {ok, more};

process_recrypting(EncodingType, [Interval | Rest], BatchSize) ->
    try
        Items = get_data_by_key_id_between(EncodingType, Interval, BatchSize),
        ItemsSize = length(Items),
        _ = lager:info("Got ~p items to recrypt", [ItemsSize]),
        _ = [
            begin
            _ = recrypt(EncodingType, ItemID),
            lager:debug("Recrypted item ~p", [ItemID])
            end
        || ItemID <- Items],
        process_recrypting(EncodingType, Rest, BatchSize - ItemsSize)
    catch
        throw:Reason ->
            {error, Reason}
    end.

get_data_by_key_id_between(cvv, {From, To}, BatchSize) ->
    cds:get_sessions_by_key_id_between(From, To, BatchSize);

get_data_by_key_id_between(card_data, {From, To}, BatchSize) ->
    cds:get_tokens_by_key_id_between(From, To, BatchSize).

recrypt(cvv, Session) ->
    CVV = cds:get_cvv(Session),
    cds:put_cvv(Session, CVV);

recrypt(card_data, Token) ->
    CardData = cds:get_card_data(Token),
    cds:put_cardholder_data(Token, CardData).

get_interval() ->
    maps:get(interval, get_config(), ?DEFAULT_INTERVAL).

get_batch_size() ->
    maps:get(batch_size, get_config(), ?DEFAULT_BATCH_SIZE).

get_config() ->
    genlib_app:env(cds, recrypter, #{}).
