-module(cds_storage).

-compile({no_auto_import, [get/1]}).

-callback find([cds:hash()]) -> {found, cds:token()} | not_found | {error, term()}.
-callback get(cds:token()) -> {ok, binary()} | {error, term()}.
-callback put(cds:token(), cds:hash(), binary()) -> {ok, cds:token()} | {error, term()}.
-callback delete(cds:token(), [cds:hash()]) -> ok | {error, term()}.

-export([get_ccd/1]).
-export([put_ccd/1]).
-export([delete_ccd/1]).
-export([ccd_exists/1]).

-spec get_ccd(cds:token()) -> {ok, cds:ccd()} | {error, term()}.
get_ccd(Token) ->
	case get(Token) of
		{ok, Data} ->
			cds:decrypt(Data);
		{error, Reason} ->
			{error, Reason}
	end.

-spec put_ccd(cds:ccd()) -> {ok, cds:token()} | {error, term()}.
put_ccd(CCD) ->
	case find_ccd(CCD) of
		{found, Token} ->
			{ok, Token};
		not_found ->
			put(cds:token(), cds:hash(CCD), cds:encrypt(CCD));
		{error, Reason} ->
			{error, Reason}
	end.

-spec delete_ccd(cds:token()) -> ok | {error, term()}.
delete_ccd(Token) ->
	case get_ccd(Token) of
		{ok, CCD} ->
			{ok, Hashes} = cds:all_hashes(CCD),
			delete(Token, Hashes);
		{error, Reason} ->
			{error, Reason}
	end.

-spec ccd_exists(cds:ccd()) -> boolean() | {error, term()}.
ccd_exists(CCD) ->
	case find_ccd(CCD) of
		{found, _Token} ->
			true;
		not_found ->
			false;
		{error, Reason} ->
			{error, Reason}
	end.

-spec find_ccd(cds:ccd()) -> {found, cds:token()} | not_found | {error, term()}.
find_ccd(CCD) ->
	case cds:all_hashes(CCD) of
		{ok, Hashes} ->
			find(Hashes);
		{error, Reason} ->
			{error, Reason}
	end.


%% backend calls

-spec find([cds:hash()]) -> {found, cds:token()} | not_found | {error, term()}.
find(Hashes) ->
	call_backend(find, [Hashes]).

-spec get(cds:token()) -> {ok, binary()} | {error, term()}.
get(Token) ->
	call_backend(get, [Token]).

-spec put(cds:token(), cds:hash(), binary()) -> ok | {error, term()}.
put(Token, Hash, Data) ->
	call_backend(put, [Token, Hash, Data]).

-spec delete(cds:token(), [cds:hash()]) -> ok | {error, term()}.
delete(Token, Hashes) ->
	call_backend(delete, [Token, Hashes]).

call_backend(Method, Args) ->
	case application:get_env(cds, storage) of
		{ok, Backend} ->
			erlang:apply(Backend, Method, Args);
		undefined ->
			{error, {not_configured, storage}}
	end.
