-module(cds_ct_utils).

-export([start_clear/1]).
-export([stop_clear/1]).

-export([set_riak_storage/1]).
-export([set_ets_storage/1]).

-export([store/2]).
-export([store/3]).
-export([lookup/2]).

-export([start_stash/1]).

%%
%% Types
%%

-type config() :: [{atom(), any()}] | atom().

-export_type([config/0]).

%%
%% API
%%

-spec start_clear(config()) -> config().
start_clear(Config) ->
    IP = "127.0.0.1",
    Port = 8022,
    RootUrl = "http://" ++ IP ++ ":" ++ integer_to_list(Port),
    StorageConfig = config(storage_config, Config, []),
    CleanConfig = config(session_cleaning_config, Config, []),
    Recrypting = config(recrypting_config, Config, []),
    ok = clean_storage(StorageConfig),
    EncPrivateKeys = #{
        <<"1">> =>  <<"{
                          \"protected\": \"eyJhbGciOiJQQkVTMi1IUzI1NitBMTI4S1ciLCJjdHkiOiJqd2sranNvbiIsImVuYyI6IkEyNTZHQ00iLCJwMmMiOjEwMDAwMCwicDJzIjoiWlhxWUtkN0VvNWwxR29TYU9uRERJdyJ9\",
                          \"encrypted_key\": \"uUC1-fWxYidjoM1vNsvkexPtrSj_QM-175HsORLjaRu4g5yrW8ZIsg\",
                          \"iv\": \"qq5xat9mMtcg_wRU\",
                          \"ciphertext\": \"_kzb1lXyDZMhWcrj8BNsfKAB_XkSHJ0oCpahcCvW2Lp7G_A6YUumsHW_-IwQ3k6TX3KwP5GvaNeLvFOH_LkJOF4RUYhDSKmR1dOSqMBF9Jg_GjNViETyOB1FIAV5gX-LajJ7EkNC5WrAtdjiidO7JrkjgCvEq3QWuUfRp39JYFRzMMrwbfmDys-Uub9mLmNvrv7COUqLr1DWzjEjNJueUyXxZFo2-UxuSYmLmyUvqQfz_KkWPtx8htvrY0d5ks52uYCUnm1iQ35S0xaGQ1mTsTx-6K30jUcOR5hdfUx__F4CRYI3b6ukJ71sAZjEziXWnTYWBuW4fYQ8g10lt95wYOutF1jp1cSAXdYwgSg4sfSCCApmnuhc8QSn902ZN85R6nOlLzmLr1RBpSaAzxisHb7kLwPsPdLsLFSM3zs1mcTxb9Ej-jYEp7rB3Uk2Z9dBmoZbFy6ZTVR_q2zgxAPLkMrOda1Kq2iW8o8lgAHlXRCDmMw1Q-iz3A7FL6f34iOAWuuoE2kim14v5p_2D5bOooNGi_-Ecg42-xryvUxHXZE9VMnX4OE2YLgqDT-T9scv6DXrZO4HEmnxKePvqCqIc-3N9tuCzscf0YAQsOauN0ETkV9uwZKguONVfCCDTPCiYFxFJ6-i1IcKZXUB8iZ59W0IRsJ871bqqm6wWJVVmQZcMouXazdIGz7UA0xaiiySGp8S6Jt90QN1YjQyTj8RHZMcvXdZ8QVcg3MjfbPeZr_hmzETGGbq-XrqJPWL8M9pq-skhZpIGOyUl-IcylbJD_Ayk5VLVUVFmuSamvqZgtOxlf4yiC2Tu6P4T7wkjDkcFS9F2MwQefj9SSoGJhkSaxHj3twQqrjdtDX6LF822lj_QM3PHHyTSrznha_h5fNZv9TUBDQdUAluY5vn4iVSx22AHDg_QcGAUJgUboEQAdtvQzEzow_0Ch0YbREtaDkJAgMjOPBFUQgqx2v70DE1bvuOfvvHi0stl9uj-pIfypYLRrpdQgEg_KiXTqHOXEOroaWcSG-9R4S7OkrZAey1NwbUDnXNdFHwGXXtWceSczFZs9ehThrcl32Y3ubINwHjxYR3fMMiCG2d91eIgym5tNNdxLi9HJu8tT5Rx8w7CdnXFV7ukG_OjgpMxt15M8op91SR9lCAE8RfQMnhkwYRyg23893GJ1fvW2frTG3asMjZhFtjPm8AU8QxTQdZCk62LYskqXACMR_LG_1s8bZqIr4byMyxc2mDiYRHO6LVJpKcJN2R26ht2OoPE9-wt_RCHNa7VB6R_PtCeZQAbrCRMhMau-zKWnKyT7BZrZjHXogqa7u0Q0uL1ttRrzhW9vwmAaSUU5ZCSRAhAXPUfgYG16i2u2FcVSYqhrE4cwxqlYZ0xAZ6pDuQBtG752J9lPS8Ae9W1Ab4RXJtVjG8Q8P9twZ_LDYt4Est9yzjkw6f_s5n5-LcHHMX8-nqC-j3eca_RFa_h_1rWef_Hyem4EDzKkd7kN8mlIt65hA3lzR9w5FegB3AcVKpkrOa2c6EH7z_1fVzFej14-d7Kp6SJIvaL1Pgs9YposYBn2mpEaepFFnRZ2n5UQ-3po8ePT-eJsOOeglm8zwzxcDWPYBY48Y91CVnZws29lNUoj_IhJpDrYj1hh0ksBea_NeG8xJxdTTaIOFwd7H74WgO_S24743_jKhF7Or9Gd-TCVZKJGu46m8Iv2nHEcim-99r8-Zp2NJriLtvUS8CqFxkbsrMOtMg1RiYYqwYJqkmhXVDCUMr_CAE_L3VO3owB0w8kGIxup5kDVg5MMLVuR4K9tDK2fy1sPgd05sqoc0Hz7iWLngr7SYUfFSfVEk8bl7Am6yVz7-dhER6RQ2zlDfUFkeleqnjYR7F-u6sFUf5BKuohzBWFritZWhi7pRYHJb09VZdz1v6zbNErYkskMMHu0bjrGJVcirXsYVzdv876YJXllNk9e3BV2C-pUBMcwEhabby0i5TWlTU-IV9Sf-WV5vVf8ToUo7EdtcrmGzfZE8x6y1aJ02vxej5aajnbCTWG3b-1AOBDwPJG-6M-zmWZvaY4k3f7jUeYR2xhUavYJVfyF5Wq5YhtFTlIFggoyN9XGXkRpETXRPnRVgggkFthjmQ2AcKffCZC-319RUc3uwZ_Cgs_cJjoSOHVCSBTEGDKJ61FwCuqciiZGSgq7KO6-KrDY8ift-yKnBglYRKGnJ0SWxdQ7UVeJ8W4cVKrt0gNVpX0o5GC32Q1aH5c4GKAea_RCTMXbvMhskSEK07A__qqKCAg2LOypPTOu7zNMSQckrRSemwcrXCexYnCVuf0lKPp8dyYWF5uQZUp3SLwLP_tVxZ-nBXFtmvFWNo9fuSuQLPr1DIRVWsYemqSfZBDZxWFq771JXvvD9CUbt9HAXNC-zlqYkLngDxwn02VThFv9t1HKWJUftANPu2EcLExwy9pKIv2Ti9IRXpvAefYHBN6BsY6_h4v05UMD1t46jqIHsm7B-1PuNDEta5KxUU5tc6rXeY2N9V_NObKRb7YzHzCFQ0Mb-Io40CGiOhc3Gy3cr86abENLpJNmYfEdKpYhb-8O52LJcJAroehgTPb_RwNBwvmI5QoXp5EicnbX2p2xwKuRiVsgGxiKx4d7oz4sDAnzZ_X2lCPjXqGefJIvGPeQcaHIj5SudeM-7rIxkE-A_xO1Isxj_EnUs6Oy4ipUFpflGGsu1ryHAfgnFCPbRntxQIgFijvJqxhm07lonK9pZITFXQRH26goypiECjsIpvDBVNzO8jenqym6DJS-Uhz_vz2j9kOF99JyDY3fJvx1FQvqrlkXn6-XDdnO8FQ6nAT70x4DMDrYm5P9iHX5RZBLbcTFbfIuasR-RVeL6r2wzu2votorW-Y-6HYOG830Py6RLMeTaR5rb9vxBc_eh9HeyM_l_QGTy8-xSPoJUk-hK7iMBSDvdIWxcWCitU6mYCM8GvFVVqmZo2UEeTfqLZ_-v7g_LzNdCQ6gvENSVQSPh2vHZ8RUA10LYLrBTIu47uSwwNheGzyL-MTrPrKQpj1sUDm91H4wj6VvxYCpfsx9Z-5h7zOM43v5z9_XSK_dPopWzzbN2Ez2rKdfgEGk7FwNZO4rNUPHI496pclLoCgkuhPJwl7YzqWWyrVYm4o1GtloAbgYpFe80qKj-jsUn9GGcsGu_pHs_LAKkyzu0BE3XlC3Y-v9cibU-97ujNEshZhS5i4mVKN_R5-W3WsVI7DIGudDxydtuGpM6Dd0V1usClB7FjT31NG5xbPHl3AZQEpmSFlf79nqC2hzjXPlX3arpkqj6mpisAdtWKgnNjo4yDdCHAej-bTa1CmU-18gOtX4-Q912eHvrSNAAfaUrPB4Kgnhk28X8-cwe4LNEhHr5GStRF_5mRueC_YWjqAMpCMMPoUyXpkCyPe0_zTc9LvFqPosmowhuzDp7MVcj-D9U6GKCBwke4o2x14mG-UZzACD56po2d6H5JqJdKRdn27f6ZyGDtmwGQO5x2sYgVMMcXGpyYzbvv3hH4orJDogorh0oxqVmCpkU6OLuoBqFlsV9nSA1vfctI7GnHug-OtdUSYg7jHnM5UlCgL5xomf88dHL7yn9980hjKiFrU4Gd43vmTEx3kwWJdZlYna55dN147NXPrngkQ4VeTht0xQQyEqSIUe9WEIS83do4SnmeWAWaKRr_4vQVqbBvUiJazbPIJuCipeCBNho3xl1NI5VV0NqWVeUnEyVr40HMfvlh9W42q6apOkTwF27a8a3jOcz8aFHyYXX1qyrSIkMCdKCtmZVMdmvtXVKdIwIBxZn9vsftJg77Fw0K7rnAQhRNuf6ZG78e9JicKADW65SRT-nNDekjYeTV-0-v1OFaJUpkxYn2VOdA-W9zqq84oTlIiG3Ul4wH-GyNplecEQkrTwGq3vmVND0EDlb0dLG8bE91uHgimTz3Z2SK1yYhUrG3GzYOIyxLDt6cjme6i5m0ThQCupxCK6_CH_2-u3BtfMaiM-i6y-e9x81M7wYBkzoEYZtJViqa_P6hDr5Eqc24ykkZ7kw53fS2iAcmhDFRk6dUkA_7fhon5kYcZkMPru1PBV_YTXlujf13kwgr2VIs-eYh0RfI452L4XUHknTOJ0syH1esUPgxDDy7u4OLeFo6S-k7z1kks6suS0A7S3ipgjSktwW2fxlZpXrucnj8Z_9XcclPsXdjLfn4y9t0_li3qdHODfYKbtZZo66WV1qZ4cgCOze5QQe0gjQghVmOt55Socw3m0ZuDsX7J9KUi55ejb-Iemqd65G_TnzWt9KRvvOeZkrRf5pZ97m4\",
                          \"tag\": \"sroFBm6AGteC4xKLTztiwQ\"
                        }">>,
        <<"2">> =>  <<"{
                          \"protected\": \"eyJhbGciOiJQQkVTMi1IUzI1NitBMTI4S1ciLCJjdHkiOiJqd2sranNvbiIsImVuYyI6IkEyNTZHQ00iLCJwMmMiOjEwMDAwMCwicDJzIjoiU2lFVFpQem4yaktaMVUxZTdBTWlUQSJ9\",
                          \"encrypted_key\": \"AlXqfhvUtFtXDX7ysEREl3RUO-71Qp1tgRAN7VO-cdAUTpyLuo032g\",
                          \"iv\": \"2GtXFBa6PbS7073S\",
                          \"ciphertext\": \"vbsaQUegZ6_fgaCmC73y5oNZHMLPMPqfqAdJHorOilI15fYz_a4JJcbVn7_SsPLO8sidDrZY0nbrLgM7j77eEHjcKYDLz1cnJAiryeqntkZrDIsaPSW3yKz6f2fOg4-9S7M6yWOoi9m949VWfV8jWMK6CZ3x6m2xUD7CBtwzZqssP-1UgVFER7poKG07SSTJUbAv5_iPvUbGvGR6d0f5tNLlioAI8Ru5eMoHcUawhOD51V2Uhmj8MQyaMKCNnXl3GNF6k3Axdd0RcHXyOeAxs9lufEd1eZ9ji6ae1HxGlOT4Nsa41zkiqM3enGl81syeDrmXttytJu2Tz4xSxWJstdxfP653U5Au9d4rmIdwrhIdPYSk3wJ2CeHVLq4DoLUMQ4YBqM5dqWZBq4VusI0TRAcuJHpPDUDtvtuUTECw2sytEFfr0wjxVZbdGtfElSKg-IutvAHOaGMEyNKgyy-GuHx8rb7TZVraKj-LVWivZUi1NqXys07ah7JjavCuITvAeoXJS6vl88L0N-4vxPvGZepuHqYv0rbbSpT7-Mz0MRkA2ydiNVROH4YaYTaealyiVgPmd1Bn3j0-nyULyg6hXnCQ5H2RMGEkqOopCuLrg2MLlA2jEvKRf-dQ8K7SamqhjfSm_xvGsp2qdFswKYCMSn6NUGqbupv27pIPi6rTMRUJGnD_9FTKusRdY9DC6OOpUznzGybp_EbEUzdz4kQ1PMzKnkOnF31J1Hqk-kCUs9b3OjtOa9CvaeD8FnBwyPQw_AWzHoQiuTSXvs_QmwyFZykivQDvDkXkomGqio_GdO-F0-sfJAqh1_nBvGBzd_qnjz5LiNer-dDac06KYmjv3kqKfvJHUawIeUsRSvqY9UeIawk8Kakaf5i7KrhYtmh7iA0rFMwDmXfkZxzJg27djUu6YtiaBbDHlJtfl9gm3uuqcJ0E5wR7NOaMYW7Fy_Iy5KD05bQiZNzYSFpRujUC4ycyXZCQbkgjOLnFvGwR-nAjzmBmmfNqOniPNpi9y7BrliNTKDIxa0QUnYLTWkRE_xs8LnytLoQbQaQPzKUGpGcXCN75818UksFZa2no148DBtzk2bZGCVZgH9I1FbVbVhO40qqleEl6eZi1qwJSXmMV1dedmByolERKVkjQtYcSJWiGbsaIZI2Aik8QFewpDnoOuAb2BxjZUNv5doDAcW0qMwU2M8v3grv-iwNyr1gLnzsYIyjqbs0e80oke-x1JjhU5wSnOLWojC9n_gIbZEzyNDMwYAKUugzJFeaCE1_Yd-786d3RokWWDgaOoANEFhsnDjli51TDpGo5o6TqPms-a9ygcZik-4_psPpiWt2Z4RB2JqxL-zkdCod1SZbZYHWBK02J7oCia_Gz0DCmhP4HY0wTIL1ZR2qtLvY1xDwsO66t3DXDyZxIfpyrD0bBBym4bAIuY5Tqwmh1rFgXRobofWvrWOyggpkuZ4OMJ6ziuwPgBFLUm8jcrnH5s18eIKLtO32dZ_ohh7LDiJWuKiRmteH3mta2WFsmOJ9895pRYRXwPur3sM8s29kNCaZqoQudPwn8HEgtj0e3dZdQBYDPc2idQPneS8Xjsec8PStfofx23Z7VVqsQI-e_EkWt68Gnv7jrLwAei9zZK68bapkOQSLaqfaQb0McYMqYIWnVFht7M5tiWt6re4NQx3nCEUwWlKqZYpkebvH8sYulNpzPPM_-2uO2xlbTAoiFK0Go6SDIBE1V1Yu7rnI-DLk2lp4wBc9_sy_88xeI_K7LmOKZkbKqJk49sVhyh9FqzoVUsvlNbTOVvOf2yMLdJ2qSeid2aG_VWC9kBvay4yOsQqT8Z7-niOpBjQIsrgEZB78J93ch2An4dcliWzw3K8PvSETsWOHKazwhSovRaMNZvBFDF-myJdc_yA4PUatg-FJ-hif1qhrwE-ydd_hCXQr4pMDF-jgRHNIr68OU3O635t7MQdY5I-dNKXmUITxNtPWhQYSAfaPyFhFizSKctN7bWdG2rHKY-IY0NWY8Y__14Dl42sCBaEFDtLCMtlVpOEaoz2iunlHBDKURtrVj2TpSyM5xyEPaB8yX7KaIdGcZfpLsYoS0tIRet_GlZSj5NwnApfDeT_q_PhIbM2NqorxttjRu0LLNIMn9lU0tDJADedujXR3HswF0hkaZTJjTGqf_o9b9Mzhzf2AhmbFlyxJxwM7oe-o20R73Lp2N_0MMTY4jlVIxAdw3kvn1ItuLI3gHeVcgg5IEYdv8niBKfRQVAT0je7XmU_msBU4SjJOvuG1gofVfywMw-baA7fdF2TE3-IV5GgOcAA-vZjjOTia4ISSmdF_zVvEXsp3LNCXLwtzXrXQCxNNseLZES9Q74qqu1XxgvUEUXvq-T6_9x-BC52CBhZDI_NEpCrQZButMSnK2WEyU1rRBlzuH49LEhO0vH1usygcuBrJ6nSaYVEBblPnD0i9zZ_1yolryWVma0gPGLHmnC-U5tV19ZkGUF6Hjp8yYAFeza650ZodacJm7WyuDNJGWOeug9udX3Xvsqw_PwMXOmRKnhwoaaFMk6TrSajpNZWTkD5uJNCktkBn9PC1NqNSXbK8w9wfgYBlpE28nhuymMIeQhmI1cTYGVKR4AMpL9M1ZT6kUNeVJ67nBGXyLJHb1BPCP-JxgtS9Bnhy5Im6E9I0_Yw0v-jJvEmFVH5ZPQCa2uGP90Fbir4mALPSp6H2jaX3fvMemDi48Rt5fO9rwjKFcW5pjKoOU_e3jEORUmMTWByL_y8lo3RJyk2K1l9VsvLva4lUELjHjZTahImDE-gAbKL4lWslrX-lQqlfx9o8XOeE5y0Nr0_KzsZYEIHF20QtArFGKkZMk31u34Pm54127Kn0s5PnvznMYNFXBdwmkpp5nRZO_l_9eY4b2JFCjceqNLQV7PmaKgkti1l472q5Cwen_iVbou5wIHQ7JOzJhNyYu_JFhUuu1g6r7aT6zMxtjfjjjMywRv92ciabU0flh54b44sCtRiNSKAYLLoWF9xlKAuhgqC25FJe0JV13AyjIYUTsC2SwbBit_irO5ksHpxdC7LctBJ75OF-ieg5AFMiB7Vh7IpAVyKnhpQCg-8NeYAGe3SPT5Z6rAxvfGD6i7bvh1p6EQpbbup4jyb2rkI62Uh7cs26vxMfHfjbRif2JIAsbr5EaY4avlMgUaXbq5ZULg4-bSB68mtViV21lZ7jhHchOgKFT-iq4j6Qwit0WvugKev5nuhm8D7D9JUYlbnb-P-VBGCONSn6pcS85OFStU-5yNj35foZeMT4mmN7QQglCYIZ2ef7fPN6fVEi_AGstdGqB9HO71IH5scBp3KSXU12EH_F61wGOfbxk2wUHd_y-M2hOvcBGOz-UDuK9EdnUg24Dt89UL9SPFxgOa3ihxdymvNWAsj-4dUyzQY2xg9brqUviF9Eju8DKgWm3EK61F_Bcjq-oT7ay8Hfeffr-1BhAJK286AQVV5E0CkTpMb-ZK3vG4cFwTgHiOEn-NdQnlFGyBwUtzUVG9owJdU0jGnzx1FGsS8pE2EI2ukFqwgvgQyAkA8bU9jFlOXZgUR6NurYGxv4l1qFHAq7H0zWEJmlH4szLKxf1VzGF8j8C4HxVcj_0mMU_3cvhTUceCQc_eWM0JvmcSKFEisQi2i9yG1q2R4EkcoD4MZrfsiGlCwTuLnOS_iMPpDE4Z5b-AuFsQBMYgFayuI8TBZReK9VbAlNOyE_rXL1u8cce2tHlQ3S1ap-DIx6AgQwAzMKvDth16B4rLpM07P4b7P0uHSskxjPDdTdL_4b6IN7t47k2xc6HEOqfW67cILuDI9sQ9ClP7sR4L_fhdTL6MRZ9C4zaXDK6SQuvKP4GstRlFD2FvTZXN6MnHy5j_JFi1AQ5aAR5-vvPwi9OEnZiSCmexfkLoutkwkZ7Y1GHxqDWG8k-8rdYTGRiR1WqRiJGlo2oUi4ZwgVStnqvAoRiilJDjl58cthTp3-e4SB4tg9Ts50vz2O1dtvS7fxJW3bi1tuTdjpQuLDpkHML9Iz5_aL2zuenyzAOtgcQ577wXTOhYrr66802goD4KAjr3W20MODYnK8OllAsSmflCdWegXaMVU8Zx2ooTxIhw6A1eXMGHWtzgpPN3_1T38AFuvPsL6DBvOrA_Uhk69qOqaY4BppH8r8fBmykSXffmHwV3VBQJ0glgQIF3ulLL3iLffeNs8pBQCRJTKcqjOmOoY2GPmQFVv_axyayNG12Tg4jvNQ6J8SLqSPyaRnJ1XsEKwsSPMzU2tzWLPhR2-8_COLMblypCqzCuXv3t0y2HOXh4G24\",
                          \"tag\": \"NdeKsEdAnMEFq0lcInh_AA\"
                        }">>,
        <<"3">> =>  <<"{
                          \"protected\": \"eyJhbGciOiJQQkVTMi1IUzI1NitBMTI4S1ciLCJjdHkiOiJqd2sranNvbiIsImVuYyI6IkEyNTZHQ00iLCJwMmMiOjEwMDAwMCwicDJzIjoiQktMZHhIa0syeFdBVmVWQm0wNWF0dyJ9\",
                          \"encrypted_key\": \"D9edWcyGCl5XK7pI3_Lq8lNoJ6JxPkb7gbBCxhyyWrEF0EE3zFhcuA\",
                          \"iv\": \"24FO5WBcW9Dy2vVf\",
                          \"ciphertext\": \"juxWPYE2Nvq4IAqfZWcxYR-YRVUKPXKs6dz4O050poPPLeR35EmLJ6jgddCsVSqgMlOehikg_W8iun2Z6fWYuNw-sbp49IQARhayj6S3cT0Vp4eECXVBQdT9wVqpoY10uyWNnymXc3lIKY4kVKtveyWq3r48zmh_4Nl_eXQl2Dp4R3qoNlBlmyn0KuSFvA7cpcNx7GmLe73JfXuugsJJ7rud56laFeddA5tTxqqCEMttw1RxPZ2ctNw7Lmcy55NgHAeXctF6-41rbX35w-6WLgb_B6V2XcAhYfkTsnbt4DDqWe1NQxJ2P1x7SHEAI52X-iAvB9mc3Nj7zlhjIg7Hi_WeVJf8DTpFNrhEHAjeQWGFL-xF4O5yQB-jOCSxaYrvpQvntZVJqtzbRkaiZFzqnIHIKelSqX-IomV3Sp9f2NohhqqBXqGSPiCyhONQNoqw9qV2tZheQfHz-O7ujH324joj2cN82EfOWevSooGLINbH0IsiwmBr2kZ6e80MMjCys3cafzy3nrWBd8HqHv-IHY4q26G1lqskg8iCcuRJAO5W93lFphlaEXWDarZGNofyKzpBg4_iEvkNw3vwUiZbjim3NitMa8OEGL1JyNqdbeLZvrx-KM_ZkxqI1NtOeseBAANr8dpVmnNHD8Mf_t6v4O1gS38rIqnCsY6j3DV-7ST75KZjcw3vtpEiyu29Y-jkwLi8mCrXl3PRAFrFh1sXlPgWsmnL5a34sFbjk7TKQS-gk7Nhz2YrM_Z8BmrsxBoWN6h9xlcVUf_y3hwjyEOyx-nOrLPt-n75sD05j0X5mRV2fYc18Rvfr0n188t8zGYmEVKBXnMciXZBpoDc7We86GGMaZFPMOFWf5FNsob75RgtxLpBK9R6rKTVQRpZX1R2zO0MpECNn9bRbubzE6RMSGcnrsvs8Gl1LDgqMhWbtisF16bLuFB9_r5jtU8x3VG9ZX0TU1FP5RW_02ca-WlXDYM5auHkvOWGDyaMkGTD4MC3YuCAuGMrMl-4WhtddndZPFg9M3ts-dj-Oh4Ii9fBcWdT-uq8FNF56idG14gW70K4zxTxbReQ4mx-lVqYwd-55Dw2Y5BPafbk0JIn5sfm-iDCSg9J4dDjCP9JK2Y0bON42rskgHshbTJ6EkZAcSAXKrEg2xx15_l84p_9gSzfeMDuZQGVzJPssHc35hX8-bbh7j4LAd0ttQhAQpWdoV4EEU8v8CwdwE9OxErcQTANcjZH09FbYnML1ROCbCFIe7lAeKKeEoEHOZWuAwdbSGsGlxLnhDc1Z1-z3Msv6DEPOj3Lb_iZ-dKDJxqa9bjThU6U8LtAbfesu3SVA4uiCUHuyM1lIF8qdTa5frBeZlsWrkStOf4itpDupwyOB0sz84dk8Twyo7hdEs4stdne-PD5LE9H60aNnLnKNYFFODeLNwRZudfQZLsmA5_K-59TSfcGQigvRn9EQNQR0-S5KzaDIA0K6yLCadNOg_dRYDCUy9iL4JFocoF_evCKVU72nSbOZ6f0a2m6-V-MZtaNQcLZ15wXhLo11cCgXGon2B6ZXi_EEt3LgidOV0LR5uQHtYWqvLTozrxjvo83FJmcP-UyvXBqdkmK_cT4tcSJkMAUhZVBxVilVkFs1ObIuZECq6CnQM1ZxKtweO_I3xSAq0oWKoHBUwGvzR2BHMAYqkFa5SYbImILSmYqxt9clOOp6ivF9U5b8dghGRF-83KM6k219ut6misIeJChsxqkeAH8KJ7IatJB5t8-EHqvTWHsoJSUvZFZlW95yVKUlhFXk_H8b3f-7n28pY8YPkHFrjBV-jrL_4wpIq-qAMbS4gnr2-Ivl9oXPcgov3-efpHXWwKVL4bofC8W_ZsO5a9fjZeLg3IWuz5pJmR2CLZuJllhtcHQQIyFAs4OuQVP6k5EZhe9JPmfpiCm4E-LRQR12B1ZJxaKi0VO1WDSordrD2SjAXwRcJ4yKG-_hMeuw36N8Xx37iNqYAgPPO3vHXJpkfgGm-PtXNlCMMioJ-QN0iNhaanjY207t9sPVAZqrfoaq4IByTRbKCB7cNnP-N4kW7NhqAAaeJ_eR30cJ-rVIpds3PL2C_oIaVE7jiPOka__zV3nZOEdNXmDSPq3DZwTOEzmgSZTq41cmDC95I1oDcI2E_PvCMjw4goSjdoKZUCJbGza0v6n0eWJ-f16UsN2-wcwNnAkUL8JtI65INlHZlp0icl8ZjKwemebNv8Bu9Fe9ssQykHIrhfunBHveYAw_cWHBdRiG0frzfCKFAq6L_PNpJI4wyD3qrDNGwyuEh_tGlMo3enN2vjEP1KgiRRvXX6xr7d4Y1gURv_DRVUHOWcBChYjg7xGejePpiDlq20Z59KvMU401uN_cvuXH2S2DKT_4cdPmXKfhu6ee5L9HF4jJT-JuYaYjiHNrBfBQ3WuwheyqfyjX9DLQfPVG_8JQ6Ew-j0BBN1mlLnfIdxVP59ovqvHrWKvhlg0rwMf--XUaSXcxTzByvFjKfZ83Uozr2RXc0hm3OM4l6GQQuKV6-v2ZvIlWDbK9Heyom2L5B2vsbaIRiv7kLB3Rf42mKxkGAU0RBkQL9oEUcgXMRrQHsDavPmlSNDMi9wU9HLFHR_KTqGSIOpi7wAPzQ4vl5ij7WKZsmD5w57tmmly8j9LXWOS_mFn7vpn-yaEDWI9lmOqW6f222oKAnj58m3Hd-4QlvUNIKQGa3KqH1HEVv6Masz920aFnISxWf1efc7nBddKq_iBqdw9Clo4vWvU0OKyDghbAwIIHfS4k1ujoTaKtFcc94MdpJFwpBmeE-h8I9ytrseK2UWvYYFogW6am3L1UKFZ574FCwn6dbD8RP1WpPqlvJJYeCPp1fEyMiLx57EZw0wlkhoiZqj9GWSidSXWp-nqFNGbQ8JiUaZLUZPngF9OzorhNobPNwY0k0eggsKQrVQAtLaN2VbLXhlD9O9Xl9dXz8qMSHHsspj6V_DrC612t0zYhB3s8caWZYqZ7-wcDB33Wz0nMmP-Bh-5yx_NePq0Czr2s_FX40WCLUG_a6H4s72mWstE3071U7vLgpVJdSdNp49lilCnMXLmLCVmDm9fFaLuDsAXTr8bSzkVCUmOjP7WrLjBXde8JlaXzFwVKhfphqnLeiOmj-4IcEbIjypJH_9fD4y5H0Tnx_7ISEto3wumefE_-CvAaa4lrcNMRDKxX5ZjmMvuu-vUFcHuhK4Joe3SEacQv6K35So8bc7EihTMDQYW-gRkiTd9M8VIHREkicEofxTn6BJ62Tx6vdsZP0WYIVqiE7OEiskJbk81zxZU8443cPYCz-g8LL-UMCuT33c5USRpBbLgHXndNqTha0l28Zd9u6jDh73aesv28GJXbmNKF-Vw5xGcIzifRtqMCHrBjDeA63fQTfLWr5Qm_ZAbEucVnuchjKFECzH0-OYk4bJKECf5j99XYTZDB7pckr34B2HdABUKyg343f5olWbsVLvpi9unUAzUOQiMYgCkOhDoKQw524-3SFCqXliTAxALrejbjIssLdOPEAh-kNTDHWy8TR7lF7_TiyERL3Xp7P79Xh4XnXXHZXQdbl3podzh4q2ibEswbGJ2uSHisT7EWDnhJQO6nWLdXIjnkj9yS2B4OPd6Vnca1UgnvStFhyC5nVQ0jLRRKUijPkopHqVALoRLdSUd9TTN7AEAn7J7pOkreXDKnYily1HD-j2sgXFoZAXkrwBu7e1SfqgcVfr_NYzAs5CWWn_kauSdv78lUaSVJnCWMJgVwj_-cAb595gPZMKLkMYdTwxCCd_RByOI06qsp6-MIg1S_tUr7XOZrJanJrMRs48wrmdKRJaXO66RRrV-0ncta58UvI1bQGWnOm7MANPdvqGsBAAyga9quBUeLnyOKZFhDEfeBB4YdTRSfnkhR9rs5CBTy88R9NizF0qbhiRkepsJxb8Bz2jJdYA9mUo8D6t9T1T5xPy-HDDuM_eH3fks8kFESbYjpK_YuQ4fAjABLtMZG1b38kGwB9y-P35hLBiPn5LCdXzJrBLArc7kEfRsUKVsi18bbNeT_qioloDgK8WcXqVCILwhFyKzy4UyJ7jMwIxTh04SAmsG7-c7UhvgfxCG140hCrB4iEbyX2nAMVnKQ5RRrmNc6bOzUjdn_igvqO_K-ZkIGNLO4c1dzWSmFPmcCbnM4YrtGefF2AhGYmo23heJUDO-nWk_nQWIdSOZe4ZST0El_FrtYdIHgd1y3pgucffGD75fbBzocVQt4UfoxKcc7Hryto3a6A_VafM2vR527Y2cMEk3QhwWzqRHbWXJyA1il27WsxJZ\",
                          \"tag\": \"lDK3h3bt6cC0dEm70lJdtg\"
                        }">>
    },
    SigPrivateKeys = #{
        <<"1">> =>  <<"{
                            \"crv\":\"Ed25519\",
                            \"d\":\"YwuzCqcMjGrIf23jJNJgtjFl13sZ2OpYGSpRR_HPzN0\",
                            \"kid\":\"K3ZpHNJw3IZYu4fefhImUtB47eSBD4nRmpjWIoGukyg\",
                            \"kty\":\"OKP\",
                            \"x\":\"hqoiLZvfBzgtFQop3mBzUACee1ycgaT3tJIcKQ2Ndjc\"
                        }">>,
        <<"2">> =>  <<"{
                            \"crv\":\"Ed25519\",
                            \"d\":\"E4TNaNQgvxs3hj699dIlxGSxlKDXdIFGbSlIWsXZhoI\",
                            \"kid\":\"Q_85NCYwrmJr1vcbPOzO8g31_ohqFLpVoaGysWPwCbc\",
                            \"kty\":\"OKP\",
                            \"x\":\"JhVaGPlRm67u0oGbgxAgqnfLfXeW0aGjhCrBf_C1Fiw\"
                        }">>,
        <<"3">> =>  <<"{
                            \"crv\":\"Ed25519\",
                            \"d\":\"DFUYl9S7iS9XoU7dGW6Voy9U4hz-mV_7uKQQjKo8VH8\",
                            \"kid\":\"nwy3plcwQj_b70JJ3maZkN-VFQpjGCVRyIFYNeC0vvs\",
                            \"kty\":\"OKP\",
                            \"x\":\"af4UVYqUB4g711yGxzKjWvd27c9WY1EQ1a1-fwk0A6w\"
                        }">>
    },
    Apps =
        genlib_app:start_application_with(lager, [
            {async_threshold, 1},
            {async_threshold_window, 0},
            {error_logger_hwm, 600},
            {suppress_application_start_stop, true},
            {crash_log, false},
            {handlers, [
                % {lager_common_test_backend, [debug, {lager_logstash_formatter, []}]}
                {lager_common_test_backend, warning}
            ]}
        ]) ++
        genlib_app:start_application_with(scoper, [
            {storage, scoper_storage_lager}
        ]) ++
        genlib_app:start_application_with(cds, [
            {ip, IP},
            {port, Port},
            {keyring_storage, cds_keyring_storage_env},
            {net_opts, #{
                % Bump keepalive timeout up to a minute
                timeout => 60000
            }},
            {keyring_rotation_lifetime, 1000},
            {keyring_unlock_lifetime, 1000},
            {keyring_rekeying_lifetime, 3000},
            {keyring_initialize_lifetime, 3000},
            {shareholders, #{
                <<"1">> => #{
                    owner => <<"ndiezel">>,
                    public_keys => #{
                        enc =>  <<"{
                                        \"use\": \"enc\",
                                        \"kty\": \"RSA\",
                                        \"kid\": \"KUb1fNMc5j9Ei_IV3DguhJh5UOH30uvO7qXq13uevnk\",
                                        \"alg\": \"RSA-OAEP-256\",
                                        \"n\": \"2bxkamUQjD4CN8rcq5BfNLJmRmosb-zY7ajPBJqtiLUTcqym23OkUIA1brBg34clmU2ZQmtd3LWi5kVJk_wr4WsMG_78jHK3wQA-HRhY4WZDZrULTsi4XWpNSwL4dCml4fs536RKy_TyrnpiXg0ug4JVVaEeo7VIZ593mVhCxC8Ev6FK8tZ2HGGOerUXLpgQdhcp9UwaI_l7jgoWNp1f7SuBqv1mfiw4ziC1yvwyXHTKy-37LjLmVB9EVyjqpkwZgzapaOvHc1ABqJpdOrUh-PyOgq-SduqSkMrvqZEdUeR_KbFVxqbxqWJMrqkl2HOJxOla9cHRowg5ObUBjeMoaTJfqie3t6uRUsFEFMzhIyvo6QMYHooxIdOdwpZ4tpzML6jv9o5DPtN375bKzy-UsjeshYbvad1mbrcxc8tYeiQkDZEIM0KeOdHm5C6neEyY6oF4s1vSYBNCnhE5O-R9dmp8Sk5KEseEkOH5u4G2RsIXBA9z1OTDoy6qF21EvRCGzsGfExfkmPAtzbnS-EHHxbMUiio0ZJoZshYo8dwJY6vSN7UsXBgW1v7GvIF9VsfzRmgkl_3rdemYy28DJKC0U2yufePcA3nUJEhtR3UO_tIlHxZvlDSX5eTx4vs5VkFfujNSiPsgH0PEeXABGBFbal7QxU1u0XHXIFwhW5cM8Fs\",
                                        \"e\": \"AQAB\"
                                    }">>,
                        sig =>  <<"{
                                        \"crv\":\"Ed25519\",
                                        \"kid\":\"K3ZpHNJw3IZYu4fefhImUtB47eSBD4nRmpjWIoGukyg\",
                                        \"kty\":\"OKP\",
                                        \"x\":\"hqoiLZvfBzgtFQop3mBzUACee1ycgaT3tJIcKQ2Ndjc\"
                                    }">>
                }},
                <<"2">> => #{
                    owner => <<"ndiezel2">>,
                    public_keys => #{
                        enc =>  <<"{
                                      \"use\": \"enc\",
                                      \"kty\": \"RSA\",
                                      \"kid\": \"JHKqPDhPO8ZnZsloKTHt44UbzYFnKnf_zowfL_zNFRE\",
                                      \"alg\": \"RSA-OAEP-256\",
                                      \"n\": \"5vIJr6yv-ipphJf8Saam2-bmB5lab7tzlGOoI6uU60x_yBfc58ttzoT__nz8UM0ZmW6k22YvMvnOvmNoPNM0rD_u7M8HGEjZyOlel64PVuv7eqU0-217JbjJ99iMbGagQkgGyyVRfS1sF9fqig79Pn7_4-bcY8-f1bZahgaDqimikfSWu00kvHwnQPNICC_xY7gtT1K40IlQcPG-XBGMrK3JXgEmTKYaNB6TS9MX20vEkcnhYzl6AeU_dj83IXuR_fw_qLqmY6rZjHWVrSvarsUIlVN3ti1Zs53eUwjv4r-wN4oK9NPNcTvAijeq85OH5DbN9ZyPTTJKcqq1Q-M2AaMTSIQCCs260CmL9Nn0M3b6eDglZumqMkCc5p_xPmNgtiFAu0_mLf3lk9MKwd2635Tz6tZO7Di77UrClTnneu1Du5VBt7v8-xIZWL11xXHaglpIwi7SLFOl_YRk6vKzjvt0pYe7N-y9T0MSTdDkB_it7Tt7rtltMYnTA8HZTlRC6EoFMj4e7bpM8iizxl6Hbg3lj0fb24kNbI4P7cV-Y6-81NLBu0Yi0H4J-b7Km_NU1tmK1SCLxzFqhCtQXg7JhJUY-gXdMgbdLyY5zrawkwsJhq_Lpsk6dHQsxV35imi7kNkOTnoiI-SpswrACIlThnT56xC5ROuFRxAlrpZef-c\",
                                      \"e\": \"AQAB\"
                                    }">>,
                        sig =>  <<"{
                                        \"crv\":\"Ed25519\",
                                        \"kid\":\"Q_85NCYwrmJr1vcbPOzO8g31_ohqFLpVoaGysWPwCbc\",
                                        \"kty\":\"OKP\",
                                        \"x\":\"JhVaGPlRm67u0oGbgxAgqnfLfXeW0aGjhCrBf_C1Fiw\"
                                    }">>
                }},
                <<"3">> => #{
                    owner => <<"ndiezel3">>,
                    public_keys => #{
                        enc =>  <<"{
                                      \"use\": \"enc\",
                                      \"kty\": \"RSA\",
                                      \"kid\": \"xBET5c4u0yT6pDb_Cok0exHe_wQVetVpkGKn_1mmn7A\",
                                      \"alg\": \"RSA-OAEP-256\",
                                      \"n\": \"qR8UeoACkdiKllzYR6KSqldMqeA_RkVePp1DKWXCRKDKrw3OieX81tmQmbBkcisnpSipTvezmr2-6t0sPELZeah3r1-qUwQeD2ugSicoqgQoqgLT7g9DHVF8NBvHbAgESJoq-1dJqepG8-jrwT5UGioE9SGowRVywrndUjdWrKyfDPiwzSALtV5mcpZi97M_ga5J1gNJFT0h1E2QbYkdEBeDsyatcJu_-LtEuCJN0DKUhvNeXVdIcnbwxFXtmx4dmxPUDG7a03bo2_Ni3-ZdvHmtkleHvWBn2LI_zArCIZdAMsA9HJiT8DrEuLXJ-pHhx2z6wJ9l8y7QSDTtKZE0GyNpCUHtzDfwfRS0GPdj2ntIHyBO8RZqDhWc3_FH9IxQYED5UnwP5Z-VodJ0ZIStNPNGtSs1hdnW3nyAFaP9T3X5UWhHsSGjq9pDm-Lroe4jJK4uKRa__ewIB8Szfp-NgG2SGeWhpETZSDwDYEYzMZncsp35GByj7YqmrpKqAHkTsTfkbCWHgN9wUqX1vjsPUtgHB4l_Ze1G_m__-URyu8qrDR11vzqMA-iY8aSQ7DpHoRp7fThVD7gJIQNyVyAzIvDyOVdSmUPPeGxnI1YWKX-5t5SnlnpWO1Rqqh6RBtxu_1JGfq77d2khskTaPXxc1E5iyCYLFI0UgreCXpBzSGU\",
                                      \"e\": \"AQAB\"
                                    }">>,
                        sig =>  <<"{
                                        \"crv\":\"Ed25519\",
                                        \"kid\":\"nwy3plcwQj_b70JJ3maZkN-VFQpjGCVRyIFYNeC0vvs\",
                                        \"kty\":\"OKP\",
                                        \"x\":\"af4UVYqUB4g711yGxzKjWvd27c9WY1EQ1a1-fwk0A6w\"
                                    }">>
                }}
            }}
        ] ++ StorageConfig ++ CleanConfig ++ Recrypting)
    ,
    [
        {apps, lists:reverse(Apps)},
        {root_url, genlib:to_binary(RootUrl)},
        {enc_private_keys, EncPrivateKeys},
        {sig_private_keys, SigPrivateKeys}
    ] ++ Config.

-spec stop_clear(config()) -> ok.
stop_clear(C) ->
    _ = (catch cds_keyring_storage_env:delete()),
    [ok = application:stop(App) || App <- config(apps, C)],
    stop_stash(C).

-spec set_riak_storage(config()) -> config().
set_riak_storage(C) ->
    StorageConfig = [
        {storage, cds_storage_riak},
        {cds_storage_riak, #{
            conn_params => #{
                host => "riakdb",
                port => 8087,
                options => #{
                    connect_timeout => 1000,
                    keepalive => true
                }
            },
            timeout => 5000
        }}
    ],
    [{storage_config, StorageConfig} | C].

-spec set_ets_storage(config()) -> config().
set_ets_storage(C) ->
    StorageConfig = [
        {storage, cds_storage_ets}
    ],
    [{storage_config, StorageConfig} | C].

-spec store([{any(), any()}], config()) -> ok.
store(KVs, C) when is_list(KVs) ->
    [store(Key, Value, C) || {Key, Value} <- KVs],
    ok.

-spec store(any(), any(), config()) -> ok.
store(Key, Value, C) ->
    cds_ct_stash:put(config(stash, C), Key, Value).

-spec lookup(any(), config()) -> any().
lookup(Key, C) ->
    cds_ct_stash:get(config(stash, C), Key).

-spec start_stash(config()) -> config().
start_stash(C) ->
    [
        {stash, cds_ct_stash:start()}
    ] ++ C.

%%
%% Internals
%%

config(Key, Config) ->
    config(Key, Config, undefined).

config(Key, Config, Default) ->
    case lists:keysearch(Key, 1, Config) of
        {value, {Key, Val}} ->
            Val;
        _ ->
            Default
    end.

clean_storage(CdsEnv) ->
    case genlib_opts:get(storage, CdsEnv) of
        cds_storage_riak ->
            clean_riak_storage(CdsEnv);
        cds_storage_ets ->
            ok
    end.

clean_riak_storage(CdsEnv) ->
    _ = application:start(riakc),
    _ = application:set_env(riakc, allow_listing, true),
    #{conn_params := #{
        host := Host,
        port := Port
    }} = genlib_opts:get(cds_storage_riak, CdsEnv),
    {ok, Client} = riakc_pb_socket:start_link(Host, Port),
    {ok, Buckets} = riakc_pb_socket:list_buckets(Client),
    lists:foreach(
        fun(B) ->
            {ok, Keys} = riakc_pb_socket:list_keys(Client, B),
            [
                ok = riakc_pb_socket:delete(Client, B, K)
                    || K <- Keys
            ]
        end,
        Buckets
    ),
    ok.

stop_stash(C) ->
    cds_ct_stash:stop(config(stash, C)).
