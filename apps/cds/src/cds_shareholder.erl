-module(cds_shareholder).

%% API
-export([get_all/0]).
-export([get_by_id/1]).
-export_type([shareholder/0]).
-export_type([shareholders/0]).

-type shareholder() :: #{
    id := binary(),
    owner := binary(),
    sig_public_key:= map(),
    enc_public_key:= map()
}.
-type id() :: binary().
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

-spec get_by_id(binary()) -> shareholder().
get_by_id(Id) ->
    Shareholders = genlib_app:env(cds, shareholders, #{}),
    Shareholder = maps:get(Id, Shareholders),
    ConvertedShareholder = convert_to_map({Id, Shareholder}),
    case validate_shareholders([ConvertedShareholder]) of
        true ->
            ConvertedShareholder;
        false ->
            throw({invalid_configuration, shareholders})
    end.

-spec validate_shareholders(shareholders()) -> boolean().

validate_shareholders(Shareholders) ->
    lists:all(
        fun(Shareholder) ->
            case Shareholder of
                #{
                    id := _Id,
                    owner := _Owner,
                    enc_public_key := _EncPublicKey,
                    sig_public_key := _SigPublicKey
                } ->
                    true;
                _InvalidShareholder ->
                    false
            end
        end, Shareholders).

-spec convert_to_map({id(),
    #{owner := binary(), sig_public_key:= binary(), enc_public_key:= binary()}}) -> shareholder().
convert_to_map({Id, #{enc_public_key := EncPublicKey, sig_public_key := SigPublicKey} = Shareholder}) ->
    Shareholder#{
        id => Id,
        enc_public_key => jsx:decode(EncPublicKey, [return_maps]),
        sig_public_key => jsx:decode(SigPublicKey, [return_maps])
    }.
