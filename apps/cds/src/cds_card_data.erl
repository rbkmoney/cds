%% TODO
%%  - Revise card network detection
%%  - Provide more links to the regulator issued documents

-module(cds_card_data).

-export([get_card_info/1]).

-export([marshal_cardholder_data/1]).
-export([marshal_session_data/1]).
-export([unmarshal_cardholder_data/1]).
-export([unmarshal_session_data/1]).

-type cardnumber() :: binary().
-type exp_date() :: {1..12, pos_integer()}.
-type cardholder() :: binary().

-type cardholder_data() :: #{
    cardnumber := cardnumber(),
    exp_date => exp_date(),
    cardholder => cardholder()
}.

-type payload_data() :: #{
    exp_date => exp_date(),
    cardholder => cardholder()
}.

-type cvv() :: #{
    type := cvv,
    value := binary()
}.

-type '3ds'() :: #{
    type := '3ds',
    cryptogram := binary(),
    eci => binary()
}.

-type session_data() :: #{
    auth_data := cvv() | '3ds'()
}.

-type card_info() :: #{
    payment_system := payment_system(),
    iin := binary(),
    last_digits := binary()
}.

-export_type([cardnumber/0]).
-export_type([cvv/0]).
-export_type([cardholder/0]).
-export_type([exp_date/0]).
-export_type([payment_system/0]).
-export_type([card_info/0]).
-export_type([cardholder_data/0]).
-export_type([reason/0]).
-export_type([payload_data/0]).

%%

-type reason() ::
    unrecognized
    | {invalid, cardnumber, check()}.

-spec get_card_info(cardholder_data()) -> {ok, card_info()} | {error, reason()}.
get_card_info(CardData = #{cardnumber := CardNumber}) ->
    case detect_payment_system(CardNumber) of
        {ok, PaymentSystem} ->
            #{PaymentSystem := Ruleset} = get_payment_system_map(),
            {ok, get_card_info(CardData, PaymentSystem, Ruleset)};
        {error, Reason} ->
            {error, Reason}
    end.

get_card_info(CardData, PaymentSystem, Ruleset) ->
    #{
        payment_system => PaymentSystem,
        iin => get_card_iin(CardData, Ruleset),
        last_digits => get_last_digits(CardData, Ruleset)
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

-type marshalled_metadata() :: #{
    binary() := binary()
}.

-type marshalled() :: binary() | {binary(), marshalled_metadata()}.

-spec marshal_cardholder_data(cardholder_data()) -> marshalled().
%% NOTE: New CDS store card number only, expiration date and cardholder name
%% will be stored elsewhere. This design allows to reduce amount of data
%% stored for the card with the same card number but different (or unknown)
%% expiration date and cardholder name.
%% Data format designed to be backward compatible with previous one.

marshal_cardholder_data(#{cardnumber := CN}) ->
    CNSize = byte_size(CN),
    <<CNSize, CN/binary>>.

-spec marshal_session_data(session_data()) -> marshalled().
marshal_session_data(SessionData) ->
    {
        msgpack:pack(marshal(session_data, SessionData)),
        marshal(metadata, #{content_type => <<"application/msgpack">>, vsn => 1})
    }.

marshal(session_data, #{auth_data := AuthData}) ->
    #{<<"auth_data">> => marshal(auth_data, AuthData)};
marshal(auth_data, #{type := cvv, value := Value}) ->
    #{<<"type">> => <<"cvv">>, <<"value">> => Value};
marshal(auth_data, #{type := '3ds', cryptogram := Cryptogram} = Data) ->
    ECI = genlib_map:get(eci, Data),
    genlib_map:compact(#{<<"type">> => <<"3ds">>, <<"cryptogram">> => Cryptogram, <<"eci">> => ECI});
marshal(metadata, #{content_type := ContentType, vsn := VSN}) ->
    #{<<"content_type">> => ContentType, <<"vsn">> => integer_to_binary(VSN)}.

-spec unmarshal_cardholder_data(marshalled()) -> cardholder_data().
unmarshal_cardholder_data(<<CNSize, CN:CNSize/binary, Payload/binary>>) ->
    PayloadData = unmarshal_payload(Payload),
    PayloadData#{
        cardnumber => CN
    }.

-spec unmarshal_payload(marshalled()) -> payload_data().
unmarshal_payload(<<Month:8, Year:16, Cardholder/binary>>) ->
    #{
        exp_date => {Month, Year},
        cardholder => unmarshal(cardholder, Cardholder)
    };
unmarshal_payload(<<>>) ->
    #{}.

-spec unmarshal_session_data(marshalled()) -> session_data().
unmarshal_session_data(CVV) when is_binary(CVV) ->
    #{auth_data => #{type => cvv, value => CVV}};
unmarshal_session_data({SessionData, Metadata}) ->
    {ok, UnpackedSessionData} = msgpack:unpack(SessionData),
    unmarshal_session_data(UnpackedSessionData, unmarshal(metadata, Metadata)).

unmarshal_session_data(SessionData, #{content_type := <<"application/msgpack">>, vsn := VSN}) ->
    unmarshal({session_data, VSN}, SessionData).

unmarshal(cardholder, V) when is_binary(V), V =/= <<>> ->
    V;
unmarshal(cardholder, <<>>) ->
    undefined;
unmarshal({session_data, 1}, #{<<"auth_data">> := AuthData}) ->
    #{auth_data => unmarshal(auth_data, AuthData)};
unmarshal(auth_data, #{<<"type">> := <<"cvv">>, <<"value">> := Value}) ->
    #{type => cvv, value => Value};
unmarshal(auth_data, #{<<"type">> := <<"3ds">>, <<"cryptogram">> := Cryptogram} = Data) ->
    ECI = genlib_map:get(<<"eci">>, Data),
    genlib_map:compact(#{type => '3ds', cryptogram => Cryptogram, eci => ECI});
unmarshal(metadata, #{<<"content_type">> := ContentType, <<"vsn">> := VSN}) ->
    #{content_type => ContentType, vsn => binary_to_integer(VSN)}.

% config

-type payment_system() ::
    visa
    | mastercard
    | visaelectron
    | nspkmir
    | amex
    | dinersclub
    | discover
    | unionpay
    | jcb
    | maestro
    | forbrugsforeningen
    | dankort
    | dummy
    | uzcard.

-type check() ::
    {length, [pos_integer() | {range, pos_integer(), pos_integer()}]}
    | luhn.

get_payment_system_map() ->
    #{
        dummy => #{
            iin_length => 6,
            exposed_length => 4
        },

        visa => #{
            iin_length => 6,
            exposed_length => 4
        },

        mastercard => #{
            iin_length => 6,
            exposed_length => 4
        },

        visaelectron => #{
            iin_length => 6,
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
            iin_length => 6,
            exposed_length => 4
        },

        nspkmir => #{
            iin_length => 8,
            exposed_length => 2
        },

        amex => #{
            iin_length => 6,
            exposed_length => 4
        },

        dinersclub => #{
            iin_length => 6,
            exposed_length => 4
        },

        discover => #{
            iin_length => 6,
            exposed_length => 4
        },

        unionpay => #{
            iin_length => 6,
            exposed_length => 4
        },

        jcb => #{
            iin_length => 6,
            exposed_length => 4
        },

        forbrugsforeningen => #{
            iin_length => 6,
            exposed_length => 4
        },

        dankort => #{
            iin_length => 6,
            exposed_length => 4
        },

        uzcard => #{
            iin_length => 6,
            exposed_length => 4
        }
    }.

get_inn_map() ->
    #{
        <<"4026">> => visaelectron,
        <<"417500">> => visaelectron,
        <<"4405">> => visaelectron,
        <<"4508">> => visaelectron,
        <<"4844">> => visaelectron,
        <<"4913">> => visaelectron,
        <<"4917">> => visaelectron,

        %% Maestro Global Rules
        %% https://www.mastercard.com/hr/merchants/_assets/Maestro_rules.pdf
        %%
        %% 6.2.1.3 Primary Account Number (PAN)
        %%
        %% The IIN appears in the first six (6) digits of the PAN and must be assigned
        %% by the ISO Registration Authority, and must be unique. This prefix will
        %% start with 50XXXX, 560000 through 589999, or 6XXXXX, but not 59XXXX.
        <<"50">> => maestro,
        <<"56">> => maestro,
        <<"57">> => maestro,
        <<"58">> => maestro,
        <<"6">> => maestro,

        <<"600">> => forbrugsforeningen,

        <<"5019">> => dankort,

        <<"4">> => visa,

        %% Mastercard Rules
        %% https://www.mastercard.us/content/dam/mccom/global/documents/mastercard-rules.pdf
        %%
        %% Any type of account (credit, debit, prepaid, commercial, etc.) identified
        %% as a Mastercard Account with a primary account number (PAN) that begins with
        %% a BIN in the range of 222100 to 272099 or 510000 to 559999.
        <<"51">> => mastercard,
        <<"52">> => mastercard,
        <<"53">> => mastercard,
        <<"54">> => mastercard,
        <<"55">> => mastercard,
        <<"2221">> => mastercard,
        <<"2222">> => mastercard,
        <<"2223">> => mastercard,
        <<"2224">> => mastercard,
        <<"2225">> => mastercard,
        <<"2226">> => mastercard,
        <<"2227">> => mastercard,
        <<"2228">> => mastercard,
        <<"2229">> => mastercard,
        <<"23">> => mastercard,
        <<"24">> => mastercard,
        <<"25">> => mastercard,
        <<"26">> => mastercard,
        <<"270">> => mastercard,
        <<"271">> => mastercard,
        <<"2720">> => mastercard,
        %% needed for tinkoff test card
        <<"500000">> => mastercard,

        <<"34">> => amex,
        <<"37">> => amex,

        <<"30">> => dinersclub,
        <<"36">> => dinersclub,
        <<"38">> => dinersclub,
        <<"39">> => dinersclub,

        <<"60">> => discover,
        <<"64">> => discover,
        <<"65">> => discover,
        <<"622">> => discover,

        <<"62">> => unionpay,
        <<"88">> => unionpay,

        <<"35">> => jcb,

        <<"2200">> => nspkmir,
        <<"2201">> => nspkmir,
        <<"2202">> => nspkmir,
        <<"2203">> => nspkmir,
        <<"2204">> => nspkmir,

        <<"8600">> => uzcard,

        %% non-existent payment system for non-prod or legally restricted evironments
        <<"1">> => dummy
    }.
