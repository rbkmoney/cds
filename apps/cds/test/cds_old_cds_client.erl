-module(cds_old_cds_client).

-include_lib("damsel/include/dmsl_cds_thrift.hrl").
-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API
-export([put_card/2]).

-type card_data() :: #{
    pan := binary(),
    exp_date := #{
        month := integer(),
        year := integer()
    },
    cardholder_name => binary() | undefined,
    cvv => binary() | undefined
}.

-type bank_card() :: #{
    token := dmsl_domain_thrift:'Token'(),
    payment_system := atom(),
    bin := binary(),
    masked_pan := binary(),
    token_provider => atom() | undefined,
    issuer_country => atom() | undefined,
    bank_name => binary() | undefined,
    metadata => #{binary() => dmsl_msgpack_thrift:'Value'()} | undefined,
    is_cvv_empty => boolean() | undefined
}.

-spec put_card(card_data(), woody:url()) -> #{bank_card := bank_card()} | {error, {invalid_card_data, binary()}}.
put_card(CardData, RootUrl) ->
    try call(card, 'PutCard', {encode_card_data(CardData)}, RootUrl) of
        #'PutCardResult'{bank_card = BankCard} ->
            #{
                bank_card => decode_bank_card(BankCard)
            }
    catch
        #'InvalidCardData'{reason = Reason} ->
            {error, {invalid_card_data, Reason}}
    end.

encode_card_data(
    #{
        pan := Pan,
        exp_date := #{
            month := ExpDateMonth,
            year := ExpDateYear
        }
    } = CardData
) ->
    #'CardData'{
        pan = Pan,
        exp_date = #'ExpDate'{
            month = ExpDateMonth,
            year = ExpDateYear
        },
        cardholder_name = maps:get(cardholder_name, CardData, undefined),
        cvv = maps:get(cvv, CardData, undefined)
    }.

decode_bank_card(
    #'domain_BankCard'{
        token = Token,
        payment_system = PaymentSystem,
        bin = Bin,
        last_digits = MaskedPan,
        token_provider = TokenProvider,
        issuer_country = IssuerCountry,
        bank_name = BankName,
        metadata = Metadata,
        is_cvv_empty = IsCVVEmpty
    }
) ->
    DecodedBankCard = #{
        token => Token,
        payment_system => PaymentSystem,
        bin => Bin,
        masked_pan => MaskedPan,
        token_provider => TokenProvider,
        issuer_country => IssuerCountry,
        bank_name => BankName,
        metadata => Metadata,
        is_cvv_empty => IsCVVEmpty
    },
    genlib_map:compact(DecodedBankCard).

call(Service, Method, Args, RootUrl) ->
    cds_ct_utils:call(Service, Method, Args, RootUrl).
