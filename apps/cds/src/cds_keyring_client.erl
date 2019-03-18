-module(cds_keyring_client).

-export([init/3]).
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

-spec init(integer(), integer(), woody:url()) -> result().
init(Threshold, Number, RootUrl) ->
    cds_woody_client:call(keyring, 'Init', [Threshold, Number], RootUrl).

-spec unlock(cds_keysharing:masterkey_share(), woody:url()) -> result().
unlock(Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Unlock', [Share], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    cds_woody_client:call(keyring, 'Lock', [], RootUrl).

-spec rotate(cds_keysharing:masterkey_share(), woody:url()) -> result().
rotate(Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Rotate', [Share], RootUrl).
