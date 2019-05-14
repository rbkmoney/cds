-module(cds_keysharing).

-include_lib("shamir/include/shamir.hrl").

-export([share/3]).
-export([recover/1]).
-export([convert/1]).
-export([encrypt_shares_for_shareholders/2]).
-export([get_shares/1]).
-export([get_id_map/1]).
-export([validate_shares/2]).
-export([validate_share_combos/1]).

-export_type([masterkey_share/0]).
-export_type([masterkey_shares/0]).
-export_type([signed_masterkey_share/0]).
-export_type([encrypted_master_key_share/0]).
-export_type([encrypted_master_key_shares/0]).
-export_type([masterkey/0]).
-export_type([share_id/0]).
-export_type([threshold/0]).

-type masterkey() :: binary().
-type masterkey_share() :: binary().
-type masterkey_shares() :: [masterkey_share()].
-type masterkey_shares_map() :: #{share_id() => {shareholder_id(), masterkey_share()}}.
-type signed_masterkey_share() :: binary().
-type share_id() :: byte().
-type threshold() :: byte().
-type share() :: #share{
    threshold :: threshold(),
    x :: share_id(),
    y :: binary()
}.

-type shareholder_id() :: cds_shareholder:shareholder_id().
-type shareholder() :: cds_shareholder:shareholder().
-type shareholders() :: cds_shareholder:shareholders().

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
        _ = logger:error("keysharing failed with ~p ~p", [Class, Reason]),
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
        _ = logger:error("keysharing recover failed ~p ~p", [Class, Reason]),
        {error, failed_to_recover}
    end.

-spec convert
    (share()) -> masterkey_share();
    (masterkey_share()) -> share().
convert(#share{threshold = Threshold, x = X, y = Y}) ->
    base64:encode(<<Threshold, X, Y/binary>>);
convert(Share) when is_binary(Share) ->
    <<Threshold, X, Y/binary>> = base64:decode(Share),
    #share{threshold = Threshold, x = X, y = Y}.

-spec encrypt_shares_for_shareholders(masterkey_shares(), shareholders()) -> encrypted_master_key_shares().

encrypt_shares_for_shareholders(Shares, Shareholders) ->
    lists:map(fun encrypt_share_for_shareholder/1, lists:zip(Shares, Shareholders)).

-spec encrypt_share_for_shareholder({masterkey_share(), shareholder()}) -> encrypted_master_key_share().

encrypt_share_for_shareholder({Share, #{id := Id, owner := Owner} = Shareholder}) ->
    PublicKey = cds_shareholder:get_public_key(Shareholder, enc),
    #{
        id => Id,
        owner => Owner,
        encrypted_share => cds_crypto:public_encrypt(PublicKey, Share)
    }.

-spec get_shares(masterkey_shares_map()) -> masterkey_shares().

get_shares(Shares) ->
    lists:map(fun ({_ShareholderId, Share}) -> Share end, maps:values(Shares)).

-spec get_id_map(masterkey_shares_map()) -> #{share_id() => shareholder_id()}.

get_id_map(Shares) ->
    maps:map(fun (_K, {ShareholderId, _Share}) -> ShareholderId end, Shares).

-spec validate_shares(threshold(), masterkey_shares()) ->
    {ok, masterkey()} | {error, non_matching_masterkey | failed_to_recover}.

validate_shares(Threshold, Shares) ->
    AllSharesCombos = lib_combin:cnr(Threshold, Shares),
    validate_share_combos(AllSharesCombos).

-spec validate_share_combos([masterkey_shares(), ...]) ->
    {ok, masterkey()} | {error, non_matching_masterkey | failed_to_recover}.

validate_share_combos([FirstCombo | CombosOfShares]) ->
    lists:foldl(
        fun
            (ComboOfShares, {ok, MasterKey}) ->
                case cds_keysharing:recover(ComboOfShares) of
                    {ok, MasterKey} ->
                        {ok, MasterKey};
                    {ok, _NonMatchingMasterkey} ->
                        {error, non_matching_masterkey};
                    {error, failed_to_recover} ->
                        {error, failed_to_recover}
                end;
            (_ComboOfShares, Error) ->
                Error
        end,
        cds_keysharing:recover(FirstCombo),
        CombosOfShares
    ).