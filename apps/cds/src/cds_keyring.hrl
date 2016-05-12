-record(keyring, {
	current_key,
	keys
}).

-type key_id() :: byte().

-type keyring() :: #keyring{
	current_key :: key_id(),
	keys :: #{key_id() => cds_crypto:key()}
}.