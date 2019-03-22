-module(cds_keyring_utils).

%% API
-export([recover_masterkey/1]).
-export_type([shareholder/0]).
-export_type([shareholders/0]).

-type shareholder() :: #{
  id => string(),
  owner => string(),
  public_key => string()
}.
-type shareholders() :: list(shareholder()).

-type masterkey() :: binary().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: #{integer() => masterkey_share()} | list(masterkey_share()).

-spec recover_masterkey(masterkey_shares()) -> {ok, masterkey()} | {error, failed_to_recover}.

recover_masterkey(Shares) when is_map(Shares) ->
  recover_masterkey(maps:values(Shares));
recover_masterkey(Shares)  ->
  try
    MasterKey = cds_keysharing:recover(Shares),
    {ok, MasterKey}
  catch
    shamir_failed ->
      {error, failed_to_recover}
  end.
