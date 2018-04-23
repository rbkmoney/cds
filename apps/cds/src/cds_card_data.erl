%% TODO
%%  - Revise card network detection
%%  - Provide more links to the regulator issued documents

-module(cds_card_data).

-export([validate/2]).
-export([validate/3]).

-export([marshal_cardholder_data/1]).
-export([marshal_session_data/1]).
-export([unmarshal_cardholder_data/1]).
-export([unmarshal_session_data/1]).
-export([unique/1]).

-type cardnumber() :: binary().
-type exp_date()   :: {1..12, pos_integer()}.
-type cardholder() :: binary() | undefined.

-type cardholder_data() :: #{
    cardnumber := cardnumber(),
    exp_date   := exp_date(),
    cardholder := cardholder()
}.

-type cvv() :: #{
    type := cvv,
    value := binary()
}.

-type '3ds'() :: #{
    type := '3ds',
    cryptogram := binary(),
    eci := binary() | undefined
}.

-type session_data() :: #{
    auth_data := cvv() | '3ds'()
}.

-type card_info() :: #{
    payment_system := payment_system(),
    iin            := binary(),
    last_digits    := binary()
}.

-export_type([cardnumber/0]).
-export_type([cvv/0]).
-export_type([cardholder/0]).
-export_type([exp_date/0]).
-export_type([payment_system/0]).
-export_type([card_info/0]).
-export_type([cardholder_data/0]).

%%

-type reason() ::
    unrecognized |
    {invalid, cardnumber | cvv | exp_date, check()}.

-spec validate(cardholder_data(), session_data()) ->
    {ok, card_info()} | {error, reason()}.

validate(CardData, SessionData) ->
    validate(CardData, SessionData, #{now => calendar:universal_time()}).

-spec validate(cardholder_data(), session_data(), Env :: #{now := calendar:datetime()}) ->
    {ok, card_info()} | {error, reason()}.

validate(CardData = #{cardnumber := CardNumber}, #{auth_data := AuthData}, Env) ->
    case detect_payment_system(CardNumber) of
        {ok, PaymentSystem} ->
            #{PaymentSystem := Ruleset} = get_payment_system_map(),
            case validate_card_data(maps:merge(CardData, AuthData), Ruleset, Env) of
                ok ->
                    {ok, get_card_info(CardData, PaymentSystem, Ruleset)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

get_card_info(CardData, PaymentSystem, Ruleset) ->
    #{
        payment_system => PaymentSystem,
        iin            => get_card_iin(CardData, Ruleset),
        last_digits    => get_last_digits(CardData, Ruleset)
    }.

get_card_iin(#{cardnumber := CardNumber}, #{iin_length := IINLength}) ->
    binary:part(CardNumber, 0, IINLength).

get_last_digits(#{cardnumber := CardNumber}, #{exposed_length := ExposedLength}) ->
    binary:part(CardNumber, byte_size(CardNumber), -ExposedLength).

detect_payment_system(CardNumber) ->
    detect_payment_system(byte_size(CardNumber), CardNumber).

detect_payment_system(Size, CardNumber) when Size > 0 ->
    <<Pre:Size/binary, _/binary>> = CardNumber,
    case get_inn_map() of
        #{Pre := PaymentSystem} ->
            {ok, PaymentSystem};
        #{} ->
            detect_payment_system(Size - 1, CardNumber)
    end;
detect_payment_system(0, _) ->
    {error, unrecognized}.

%%

-type metadata() :: #{
    content_type := string()
}.

-type marshalled() :: binary() | {binary(), metadata()}.

-spec marshal_cardholder_data(cardholder_data()) -> marshalled().

marshal_cardholder_data(#{
    cardnumber := CN,
    exp_date   := {Month, Year},
    cardholder := Cardholder
}) ->
    <<
        (byte_size(CN)),
        CN/binary,
        Month:8, Year:16,
        (marshal(cardholder, Cardholder))/binary
    >>.

marshal(cardholder, V) when is_binary(V) ->
    V;
marshal(cardholder, undefined) ->
    <<>>.

-spec marshal_session_data(session_data()) -> marshalled().

marshal_session_data(SessionData) ->
    {msgpack:pack(SessionData), #{content_type => "application/msgpack"}}.

-spec unmarshal_cardholder_data(marshalled()) -> cardholder_data().

unmarshal_cardholder_data(<<CNSize, CN:CNSize/binary, Month:8, Year:16, Cardholder/binary>>) ->
    #{
        cardnumber => CN,
        exp_date   => {Month, Year},
        cardholder => unmarshal(cardholder, Cardholder)
    }.

unmarshal(cardholder, V) when is_binary(V), V =/= <<>> ->
    V;
unmarshal(cardholder, <<>>) ->
    undefined.

-spec unmarshal_session_data(marshalled()) -> session_data().

unmarshal_session_data(SessionData) when is_binary(SessionData) ->
    SessionData;
unmarshal_session_data({SessionData, #{content_type := "application/msgpack"}}) ->
    {ok, UnpackedSessionData} = msgpack:unpack(SessionData),
    normalize_session_data(UnpackedSessionData).

normalize_session_data(#{<<"auth_data">> := AuthData}) ->
    #{auth_data => normalize_auth_data(AuthData)}.

normalize_auth_data(#{<<"type">> := <<"cvv">>, <<"value">> := Value}) ->
    #{type => cvv, value => Value};
normalize_auth_data(#{<<"type">> := <<"3ds">>, <<"cryptogram">> := Cryptogram, <<"eci">> := ECI}) ->
    #{type => '3ds', cryptogram => Cryptogram, eci => ECI}.

-spec unique(binary()) -> binary().

unique(<<CNSize, CN:CNSize/binary, Month:8, Year:16, _/binary>>) ->
    <<CNSize, CN:CNSize/binary, Month:8, Year:16>>.

%%

validate_card_data(CardData, #{assertions := Assertions}, Env) ->
    try run_assertions(CardData, Assertions, Env) catch
        Reason ->
            {error, Reason}
    end.

run_assertions(CardData, Assertions, Env) ->
    genlib_map:foreach(
        fun (K, Checks) ->
            V = maps:get(K, CardData, undefined),
            lists:foreach(
                fun (C) -> check_value(V, C, Env) orelse throw({invalid, K, C}) end,
                Checks
            )
        end,
        Assertions
    ).

check_value(undefined, _, _) ->
    true;
check_value(V, {length, Ls}, _) ->
    lists:any(fun (L) -> check_length(V, L) end, Ls);
check_value(V, luhn, _) ->
    check_luhn(V, 0);
check_value({M, Y}, expiration, #{now := {{Y0, M0, _DD}, _Time}}) ->
    M >=  1 andalso
    M =< 12 andalso
    {Y, M} >= {Y0, M0}.

check_length(V, {range, L, U}) ->
    L =< byte_size(V) andalso byte_size(V) =< U;
check_length(V, L) ->
    byte_size(V) =:= L.

check_luhn(<<CheckSum>>, Sum) ->
    case Sum * 9 rem 10 of
        M when M =:= CheckSum - $0 ->
            true;
        _M ->
            false
    end;
check_luhn(<<N, Rest/binary>>, Sum) when byte_size(Rest) rem 2 =:= 1 ->
    case (N - $0) * 2 of
        M when M >= 10 ->
            check_luhn(Rest, Sum + M div 10 + M rem 10);
        M ->
            check_luhn(Rest, Sum + M)
    end;
check_luhn(<<N, Rest/binary>>, Sum) ->
    check_luhn(Rest, Sum + N - $0).

% config

-type payment_system() ::
    visa               |
    mastercard         |
    visaelectron       |
    nspkmir            |
    amex               |
    dinersclub         |
    discover           |
    unionpay           |
    jcb                |
    maestro            |
    forbrugsforeningen |
    dankort            .

-type check() ::
    {length, [pos_integer() | {range, pos_integer(), pos_integer()}]} |
    luhn |
    expiration.

get_payment_system_map() ->
    #{

        visa => #{
            assertions => #{
                cardnumber => [{length, [13, 16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        mastercard => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        visaelectron => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        %% Maestro Global Rules
        %% https://www.mastercard.com/hr/merchants/_assets/Maestro_rules.pdf
        %%
        %% 6.2.1.3 Primary Account Number (PAN)
        %%
        %% The PAN must be no less than twelve (12) and no more than nineteen (19)
        %% digits in length. All digits of the PAN must be numeric. It is strongly
        %% recommended that Members issue Cards with a PAN of nineteen (19) digits.
        %%
        %% The IIN appears in the first six (6) digits of the PAN and must be assigned
        %% by the ISO Registration Authority, and must be unique.
        maestro => #{
            assertions => #{
                cardnumber => [{length, [{range, 12, 19}]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        nspkmir => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 8,
            exposed_length => 2
        },

        amex => #{
            assertions => #{
                cardnumber => [{length, [15]}, luhn],
                cvv         => [{length, [3, 4]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        dinersclub => #{
            assertions => #{
                cardnumber => [{length, [{range, 14, 19}]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        discover => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        unionpay => #{
            assertions => #{
                cardnumber => [{length, [{range, 16, 19}]}],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        jcb => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        forbrugsforeningen => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        },

        dankort => #{
            assertions => #{
                cardnumber => [{length, [16]}, luhn],
                cvv         => [{length, [3]}],
                exp_date    => [expiration]
            },
            iin_length     => 6,
            exposed_length => 4
        }

    }.

get_inn_map() ->
    #{
        <<"4026">>   => visaelectron,
        <<"417500">> => visaelectron,
        <<"4405">>   => visaelectron,
        <<"4508">>   => visaelectron,
        <<"4844">>   => visaelectron,
        <<"4913">>   => visaelectron,
        <<"4917">>   => visaelectron,

        %% Maestro Global Rules
        %% https://www.mastercard.com/hr/merchants/_assets/Maestro_rules.pdf
        %%
        %% 6.2.1.3 Primary Account Number (PAN)
        %%
        %% The IIN appears in the first six (6) digits of the PAN and must be assigned
        %% by the ISO Registration Authority, and must be unique. This prefix will
        %% start with 50XXXX, 560000 through 589999, or 6XXXXX, but not 59XXXX.
        <<"50">>     => maestro,
        <<"56">>     => maestro,
        <<"57">>     => maestro,
        <<"58">>     => maestro,
        <<"6">>      => maestro,

        <<"600">>    => forbrugsforeningen,

        <<"5019">>   => dankort,

        <<"4">>      => visa,

        %% Mastercard Rules
        %% https://www.mastercard.us/content/dam/mccom/global/documents/mastercard-rules.pdf
        %%
        %% Any type of account (credit, debit, prepaid, commercial, etc.) identified
        %% as a Mastercard Account with a primary account number (PAN) that begins with
        %% a BIN in the range of 222100 to 272099 or 510000 to 559999.
        <<"51">>     => mastercard,
        <<"52">>     => mastercard,
        <<"53">>     => mastercard,
        <<"54">>     => mastercard,
        <<"55">>     => mastercard,
        <<"2221">>   => mastercard,
        <<"2222">>   => mastercard,
        <<"2223">>   => mastercard,
        <<"2224">>   => mastercard,
        <<"2225">>   => mastercard,
        <<"2226">>   => mastercard,
        <<"2227">>   => mastercard,
        <<"2228">>   => mastercard,
        <<"2229">>   => mastercard,
        <<"23">>     => mastercard,
        <<"24">>     => mastercard,
        <<"25">>     => mastercard,
        <<"26">>     => mastercard,
        <<"270">>    => mastercard,
        <<"271">>    => mastercard,
        <<"2720">>   => mastercard,
        <<"500000">> => mastercard, %% needed for tinkoff test card

        <<"34">>     => amex,
        <<"37">>     => amex,

        <<"30">>     => dinersclub,
        <<"36">>     => dinersclub,
        <<"38">>     => dinersclub,
        <<"39">>     => dinersclub,

        <<"60">>     => discover,
        <<"64">>     => discover,
        <<"65">>     => discover,
        <<"622">>    => discover,

        <<"62">>     => unionpay,
        <<"88">>     => unionpay,

        <<"35">>     => jcb,

        <<"2200">>   => nspkmir,
        <<"2201">>   => nspkmir,
        <<"2202">>   => nspkmir,
        <<"2203">>   => nspkmir,
        <<"2204">>   => nspkmir
    }.
