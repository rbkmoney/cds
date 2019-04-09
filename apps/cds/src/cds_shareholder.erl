-module(cds_shareholder).

%% API
-export([get_all/0]).
-export([get_by_id/1]).
-export([get_public_key/2]).
-export([get_public_key_by_id/2]).
-export_type([shareholder_id/0]).
-export_type([shareholder/0]).
-export_type([shareholders/0]).

-type public_key_type() :: enc | sig.
-type public_key() :: map().
-type shareholder_id() :: binary().
-type shareholder() :: #{
    id := shareholder_id(),
    owner := binary(),
    public_keys := #{
        public_key_type() := public_key()
    }
}.
-type shareholders() :: list(shareholder()).

-spec get_all() -> shareholders().
get_all() ->
    Shareholders = genlib_app:env(cds, shareholders, #{}),
    MergedShareholders = lists:map(
        fun convert_to_map/1,
        lists:zip(maps:keys(Shareholders), maps:values(Shareholders))),
    case validate_shareholders(MergedShareholders) of
        true ->
            MergedShareholders;
        false ->
            throw({invalid_configuration, shareholders})
    end.

-spec get_by_id(binary()) -> {ok, shareholder()} | {error, not_found}.
get_by_id(Id) ->
    Shareholders = genlib_app:env(cds, shareholders, #{}),
    case maps:find(Id, Shareholders) of
        {ok, Shareholder} ->
            ConvertedShareholder = convert_to_map({Id, Shareholder}),
            case validate_shareholders([ConvertedShareholder]) of
                true ->
                    {ok, ConvertedShareholder};
                false ->
                    erlang:error({invalid_configuration, shareholders})
            end;
        error ->
            {error, not_found}
    end.

-spec validate_shareholders(shareholders()) -> boolean().

validate_shareholders(Shareholders) ->
    lists:all(
        fun(Shareholder) ->
            case Shareholder of
                #{
                    id := _Id,
                    owner := _Owner,
                    public_keys := #{
                        enc := _EncPublicKey,
                        sig := _SigPublicKey
                    }
                } ->
                    true;
                _InvalidShareholder ->
                    false
            end
        end, Shareholders).

-spec convert_to_map({shareholder_id(),
    #{owner := binary(), public_keys:= #{enc := binary(), sig := binary()}}}) -> shareholder().
convert_to_map({Id, #{public_keys := #{enc := EncPublicKey, sig := SigPublicKey}} = Shareholder}) ->
    Shareholder#{
        id => Id,
        public_keys => #{
            enc => jsx:decode(EncPublicKey, [return_maps]),
            sig => jsx:decode(SigPublicKey, [return_maps])
        }
    }.

-spec get_public_key(shareholder(), public_key_type()) -> public_key().
get_public_key(Shareholder, PublicKeyType) ->
    #{public_keys := #{PublicKeyType := PublicKey}} = Shareholder,
    PublicKey.

-spec get_public_key_by_id(shareholder_id(), public_key_type()) -> {ok, public_key()} | {error, not_found}.
get_public_key_by_id(ShareholderId, PublicKeyType) ->
    case get_by_id(ShareholderId) of
        {ok, Shareholder} ->
            {ok, get_public_key(Shareholder, PublicKeyType)};
        {error, not_found} ->
            {error, not_found}
    end.
