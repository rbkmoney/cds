-module(cds_keyring_client).

-export([init/3]).
-export([unlock/2]).
-export([lock/1]).
-export([rotate/1]).

%%
%% Internal types
%%

-type result() :: cds_client_utils:result().

%%
%% API
%%

-spec init(integer(), integer(), woody:url()) -> result().
init(Threshold, Number, RootUrl) ->
    cds_client_utils:call(keyring, 'Init', [Threshold, Number], RootUrl).

-spec unlock(cds_keysharing:masterkey_share(), woody:url()) -> result().
unlock(Share, RootUrl) ->
    cds_client_utils:call(keyring, 'Unlock', [Share], RootUrl).

-spec lock(woody:url()) -> result().
lock(RootUrl) ->
    cds_client_utils:call(keyring, 'Lock', [], RootUrl).

-spec rotate(woody:url()) -> result().
rotate(RootUrl) ->
    cds_client_utils:call(keyring, 'Rotate', [], RootUrl).

%%
%% Internals
%%
