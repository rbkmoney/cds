-module(cds_keyring_client).

-export([start_init/2]).
-export([validate_init/3]).
-export([cancel_init/1]).
-export([unlock/3]).
-export([lock/1]).
-export([rotate/3]).

%%
%% Internal types
%%

-type result() :: cds_woody_client:result().

%%
%% API
%%

-spec start_init(integer(), woody:url()) -> result().
start_init(Threshold, RootUrl) ->
    cds_woody_client:call(keyring, 'StartInit', [Threshold], RootUrl).

-spec validate_init(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) -> result().
validate_init(ShareholderId, Share, RootUrl) ->
    cds_woody_client:call(keyring, 'ValidateInit', [ShareholderId, Share], RootUrl).

-spec cancel_init(woody:url()) -> result().
cancel_init(RootUrl) ->
    cds_woody_client:call(keyring, 'CancelInit', [], RootUrl).

-spec unlock(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) -> result().
unlock(ShareholderId, Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Unlock', [ShareholderId, Share], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    cds_woody_client:call(keyring, 'Lock', [], RootUrl).

-spec rotate(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) -> result().
rotate(ShareholderId, Share, RootUrl) ->
    cds_woody_client:call(keyring, 'Rotate', [ShareholderId, Share], RootUrl).
