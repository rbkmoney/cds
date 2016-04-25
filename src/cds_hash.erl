-module(cds_hash).

-export([hash/2]).

-spec hash(binary(), binary()) -> <<_:128>>.
hash(Plain, Salt) ->
	{ok, {N, R, P}} = application:get_env(cds, scrypt_opts),
	Port = pooler:take_member(scrypt),
	Hash = scrypt:scrypt(Port, Plain, Salt, N, R, P, 16),
	pooler:return_member(scrypt, Port, ok),
	Hash.
