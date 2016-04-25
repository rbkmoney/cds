-module(cds_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	StorageService = woody_server:child_spec(
		cds_storage_service_sup,
		#{
			handlers => [{"/v1/storage", cds_interface_storage_service, cds_storage_service_handler, []}],
			event_handler => cds_service_event_handler,
			ip => {127, 0, 0, 1},
			port => 8022,
			net_opts => []
		}
	),
	KeyringService = woody_server:child_spec(
		cds_keyring_service_sup,
		#{
			handlers => [{"/v1/keyring", cds_interface_keyring_service, cds_keyring_service_handler, []}],
			event_handler => cds_service_event_handler,
			ip => {127, 0, 0, 1},
			port => 8023,
			net_opts => []
		}
	),
	KeyringManager = #{
		id => cds_keyring,
		start => {cds_keyring, start_link, []}
	},
	Procs = [
		KeyringManager,
		StorageService,
		KeyringService
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
