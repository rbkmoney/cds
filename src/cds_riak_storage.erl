-module(cds_riak_storage).
-behaviour(cds_storage).

-export([find/1]).
-export([get/1]).
-export([put/3]).
-export([delete/2]).

-define(TOKEN_DEFAULT_BUCKET, <<"tkn">>).
-define(HASH_DEFAULT_BUCKET, <<"hsh">>).

-spec find([cds:hash()]) -> {found, cds:token()} | not_found | {error, term()}.
find(Hashes) ->
	Client = pooler:take_group_member(riak),
	find(Client, Hashes).

-spec find(pid(), [cds:hash()]) -> {found, cds:token()} | not_found | {error, term()}.
find(Client, []) ->
	pooler:return_group_member(riak, Client, ok),
	not_found;
find(Client, [Hash | Rest]) ->
	case riakc_pb_socket:get(Client, bucket(hash), Hash) of
		{ok, Obj} ->
			pooler:return_group_member(riak, Client, ok),
			{found, riakc_obj:get_value(Obj)};
		{error, notfound} ->
			find(Client, Rest);
		{error, Reason} ->
			pooler:return_group_member(riak, Client, fail),
			{error, Reason}
	end.

-spec get(cds:token()) -> {ok, binary()} | {error, term()}.
get(Token) ->
	Client = pooler:take_group_member(riak),
	case riakc_pb_socket:get(Client, bucket(token), Token) of
		{ok, Obj} ->
			pooler:return_group_member(riak, Client, ok),
			{ok, riakc_obj:get_value(Obj)};
		{error, Reason} ->
			pooler:return_group_member(riak, Client, fail),
			{error, Reason}
	end.

-spec put(cds:token(), cds:hash(), binary()) -> {ok, cds:token()} | {error, term()}.
put(Token, Hash, Data) ->
	TokenObj = riakc_obj:new(bucket(token), Token, Data),
	HashObj = riakc_obj:new(bucket(hash), Hash, Token),
	Client = pooler:take_group_member(riak),
	case riakc_pb_socket:put(Client, TokenObj) of
		ok ->
			case riakc_pb_socket:put(Client, HashObj) of
				ok ->
					pooler:return_group_member(riak, Client, ok),
					{ok, Token};
				{error, Reason} ->
					pooler:return_group_member(riak, Client, fail),
					catch delete(Token, []),
					{error, Reason}
			end;
		{error, Reason} ->
			pooler:return_group_member(riak, Client, fail),
			{error, Reason}
	end.

-spec delete(cds:token(), [cds:hash()]) -> ok | {error, term()}.
delete(Token, Hashes) ->
	Client = pooler:take_group_member(riak),
	delete(Client, Token, Hashes).

-spec delete(pid(), cds:token(), [cds:hash()]) -> ok | {error, term()}.
delete(Client, Token, []) ->
	case riakc_pb_socket:delete(Client, bucket(token), Token) of
		ok ->
			pooler:return_group_member(riak, Client, ok),
			ok;
		{error, Reason} ->
			pooler:return_group_member(riak, Client, fail),
			{error, Reason}
	end;
delete(Client, Token, [Hash | Rest]) ->
	case riakc_pb_socket:delete(Client, bucket(hash), Hash) of
		ok ->
			delete(Client, Token, Rest);
		{error, Reason} ->
			pooler:return_group_member(riak, Client, fail),
			{error, Reason}
	end.


bucket(token) ->
	case application:get_env(cds, token_bucket) of
		{ok, Bucket} ->
			Bucket;
		undefined ->
			?TOKEN_DEFAULT_BUCKET
	end;
bucket(hash) ->
	case application:get_env(cds, hash_bucket) of
		{ok, Bucket} ->
			Bucket;
		undefined ->
			?HASH_DEFAULT_BUCKET
	end.


