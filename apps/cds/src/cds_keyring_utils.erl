-module(cds_keyring_utils).

%% API
-export([recover_masterkey/1]).
-export_type([shareholder/0]).
-export_type([shareholders/0]).
-export_type([encrypted_master_key_share/0]).
-export_type([encrypted_master_key_shares/0]).

-type shareholder() :: #{
  id => binary(),
  owner => binary(),
  public_key => binary()
}.
-type shareholders() :: list(shareholder()).

-type masterkey() :: binary().
-type masterkey_share() :: cds_keysharing:masterkey_share().
-type masterkey_shares() :: #{integer() => masterkey_share()} | list(masterkey_share()).

-type encrypted_master_key_share() :: #{
  id => binary(),
  owner => binary(),
  encrypted_share =>binary()
}.
-type encrypted_master_key_shares() :: list(encrypted_master_key_share()).

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
