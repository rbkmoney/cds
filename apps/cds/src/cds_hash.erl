-module(cds_hash).

-export([hash/2]).

-export_type([hash/0]).

-type hash() :: <<_:128>>.

-spec hash(binary(), binary()) -> hash().
hash(Plain, Salt) ->
    {ok, {N, R, P}} = application:get_env(cds, scrypt_opts),
    scrypt:scrypt(Plain, Salt, N, R, P, 16).
