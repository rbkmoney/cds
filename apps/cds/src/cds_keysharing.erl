-module(cds_keysharing).

-include_lib("shamir/include/shamir.hrl").

-export([share/3]).
-export([recover/1]).
-export([convert/1]).

-export_type([masterkey_share/0]).

-type masterkey() :: cds_keyring_utils:masterkey().
-type masterkey_share() :: binary().
-type share() :: #share{
    threshold :: byte(),
    x :: byte(),
    y :: binary()
}.

-spec share(binary(), byte(), byte()) -> [masterkey_share()].
share(Secret, Threshold, Count) ->
    try
        [convert(Share) || Share <- shamir:share(Secret, Threshold, Count)]
    catch Class:Reason ->
        _ = lager:error("keysharing failed with ~p ~p", [Class, Reason]),
        throw(keysharing_failed)
    end.


-spec recover([masterkey_share()] | #{integer() => masterkey_share()}) ->
    {ok, masterkey()} | {error, failed_to_recover}.

recover(Shares) when is_map(Shares) ->
    recover(maps:values(Shares));
recover(Shares) ->
    try
        {ok, shamir:recover([convert(Share) || Share <- Shares])}
    catch Class:Reason ->
        _ = lager:error("keysharing recover failed ~p ~p", [Class, Reason]),
        {error, failed_to_recover}
    end.

-spec convert
    (share()) -> masterkey_share();
    (masterkey_share()) -> share().
convert(#share{threshold = Threshold, x = X, y = Y}) ->
    <<Threshold, X, Y/binary>>;
convert(<<Threshold, X, Y/binary>>) ->
    #share{threshold = Threshold, x = X, y = Y}.
