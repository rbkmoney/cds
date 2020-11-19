-module(cds_hash).

-export([hash/3]).

-type scrypt_options() :: {
    N :: integer(),
    R :: integer(),
    P :: integer()
}.

-export_type([scrypt_options/0]).

-spec hash(binary(), binary(), scrypt_options()) -> Hash :: binary().
hash(Plain, Salt, {N, R, P}) ->
    scrypt:scrypt(Plain, Salt, N, R, P, 16).
