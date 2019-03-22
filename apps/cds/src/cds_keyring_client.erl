-module(cds_keyring_client).

-export([init/2]).
-export([validate_init/2]).
-export([cancel_init/1]).
-export([unlock/2]).
-export([lock/1]).
-export([rotate/2]).

%%
%% Internal types
%%

-type result() :: cds_woody_client:result().

%%
%% API
%%

-spec init(integer(), woody:url()) -> result().
init(Threshold, RootUrl) ->
    cds_woody_client:call(keyring, 'Init', [Threshold], RootUrl).

-spec validate_init(cds_keysharing:masterkey_share(), woody:url()) -> result().
validate_init(Share, RootUrl) ->
    cds_woody_client:call(keyring, 'ValidateInit', [Share], RootUrl).

-spec cancel_init(woody:url()) -> result().
cancel_init(RootUrl) ->
    cds_woody_client:call(keyring, 'CancelInit', [], RootUrl).

-spec unlock(cds_keysharing:masterkey_share(), woody:url()) -> result().
unlock(Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Unlock', [Share], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    cds_woody_client:call(keyring, 'Lock', [], RootUrl).

-spec rotate(cds_keysharing:masterkey_share(), woody:url()) -> result().
rotate(Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Rotate', [Share], RootUrl).
