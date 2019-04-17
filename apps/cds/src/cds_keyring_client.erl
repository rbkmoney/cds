-module(cds_keyring_client).

-export([start_init/2]).
-export([validate_init/3]).
-export([cancel_init/1]).
-export([start_unlock/1]).
-export([validate_unlock/3]).
-export([cancel_unlock/1]).
-export([lock/1]).
-export([start_rotate/1]).
-export([validate_rotate/3]).
-export([cancel_rotate/1]).

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

-spec start_unlock(woody:url()) -> result().
start_unlock(RootUrl) ->
    cds_woody_client:call(keyring, 'StartUnlock', [], RootUrl).

-spec validate_unlock(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) -> result().
validate_unlock(ShareholderId, Share, RootUrl) ->
    cds_woody_client:call(keyring, 'ValidateUnlock', [ShareholderId, Share], RootUrl).

-spec cancel_unlock(woody:url()) -> result().
cancel_unlock(RootUrl) ->
    cds_woody_client:call(keyring, 'CancelUnlock', [], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    cds_woody_client:call(keyring, 'Lock', [], RootUrl).

-spec start_rotate(woody:url()) -> result().
start_rotate(RootUrl) ->
    cds_woody_client:call(keyring, 'StartRotate', [], RootUrl).

-spec validate_rotate(cds_shareholder:shareholder_id(), cds_keysharing:masterkey_share(), woody:url()) -> result().
validate_rotate(ShareholderId, Share, RootUrl) ->
    cds_woody_client:call(keyring, 'ValidateRotate', [ShareholderId, Share], RootUrl).

-spec cancel_rotate(woody:url()) -> result().
cancel_rotate(RootUrl) ->
    cds_woody_client:call(keyring, 'CancelRotate', [], RootUrl).
