-module(cds_keyring_utils).

%% API
-export_type([encrypted_master_key_share/0]).
-export_type([encrypted_master_key_shares/0]).
-export_type([masterkey/0]).

-type masterkey() :: binary().
-type encrypted_master_key_share() :: #{
    id := binary(),
    owner := binary(),
    encrypted_share := binary()
}.
-type encrypted_master_key_shares() :: list(encrypted_master_key_share()).
