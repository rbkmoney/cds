-module(cds_keysharing).

-include_lib("shamir/include/shamir.hrl").

-export([share/3]).
-export([recover/1]).
-export([convert/1]).

-export_type([masterkey_share/0]).
-export_type([signed_masterkey_share/0]).
-export_type([encrypted_master_key_share/0]).
-export_type([encrypted_master_key_shares/0]).
-export_type([masterkey/0]).

-type masterkey() :: binary().
-type masterkey_share() :: binary().
-type signed_masterkey_share() :: binary().
-type share() :: #share{
    threshold :: byte(),
    x :: byte(),
    y :: binary()
}.

-type encrypted_master_key_share() :: #{
    id := binary(),
    owner := binary(),
    encrypted_share := binary()
}.
-type encrypted_master_key_shares() :: list(encrypted_master_key_share()).



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
