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
    encoding_type := cvv | card_data,
    continuation := undefined | term()
}.

-spec start_link(state()) -> {ok, pid()} | {error, Reason :: any()}.

start_link(Options) ->
    cds_periodic_job:start_link(?MODULE, [Options]).

-spec init(_) -> {ok, non_neg_integer(), state()}.

init([#{encoding_type := EncodingType}]) when
    EncodingType =:= cvv;
    EncodingType =:= card_data
->
    _ = lager:info("Starting recrypter for ~p...", [EncodingType]),
    _ = cds_utils:logtag_process(encoding_type, EncodingType),
    {ok, get_interval(), #{encoding_type => EncodingType, continuation => undefined}}.

-spec handle_timeout(state()) -> {ok, done | more, state()} | {error, Reason :: any(), state()}.

handle_timeout(State = #{encoding_type := EncodingType, continuation := Continuation0}) ->
    _ = lager:info("Starting recrypting", []),
    case process_recrypting(EncodingType, get_batch_size(), Continuation0) of
        {ok, done} ->
            _ = lager:info("Recrypted all"),
            {ok, done, State#{continuation => undefined}};
        {ok, more, Continuation} ->
            _ = lager:info("Recrypted some items. Need to repeat"),
            {ok, more, State#{continuation => Continuation}};
        {error, Error} ->
            _ = lager:error("Recrypting error: ~p", [Error]),
            {error, Error, State#{continuation => undefined}}
    end.

%% Internals

process_recrypting(EncodingType, BatchSize, Continuation) ->
    try
        Intervals = cds_keyring_manager:get_outdated_keys(),
        process_recrypting(EncodingType, Intervals, BatchSize, Continuation)
    catch
        throw:Error when Error =:= locked ->
            {error, Error}
    end.

process_recrypting(_, [], _, _) ->
    {ok, done};

process_recrypting(_, _, Left, Continuation) when
    Left =< 0
->
    {ok, more, Continuation};

process_recrypting(EncodingType, [Interval | Rest], BatchSize, Continuation0) ->
    {Items, Continuation} = get_data_by_key_id_between(EncodingType, Interval, BatchSize, Continuation0),
    ItemsSize = length(Items),
    _ = lager:info("Got ~p ~p items to recrypt", [ItemsSize, EncodingType]),
    _ = [recrypt_item(EncodingType, ItemID) || ItemID <- Items],
    process_recrypting(EncodingType, Rest, BatchSize - ItemsSize, Continuation).

get_data_by_key_id_between(cvv, {From, To}, BatchSize, Continuation) ->
    cds_storage:get_sessions_by_key_id_between(From, To, BatchSize, Continuation);

get_data_by_key_id_between(card_data, {From, To}, BatchSize, Continuation) ->
    cds_storage:get_tokens_by_key_id_between(From, To, BatchSize, Continuation).

recrypt_item(EncodingType, ItemID) ->
    try
        _ = recrypt(EncodingType, ItemID),
        _ = lager:debug("Recrypted ~p item ~p", [EncodingType, ItemID]),
        ok
    catch
        throw:not_found ->
            ok
    end.

recrypt(cvv, Session) ->
    CVV = cds:get_cvv(Session),
    cds:update_cvv(Session, CVV);

recrypt(card_data, Token) ->
    CardData = cds:get_cardholder_data(Token),
    cds:update_cardholder_data(Token, CardData).

get_interval() ->
    maps:get(interval, get_config(), ?DEFAULT_INTERVAL).

get_batch_size() ->
    maps:get(batch_size, get_config(), ?DEFAULT_BATCH_SIZE).

get_config() ->
    genlib_app:env(cds, recrypting, #{}).
