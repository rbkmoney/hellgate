-module(hg_errors_SUITE).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").

-export([all/0]).

-export([known_error_test       /1]).
-export([unknown_error_test     /1]).
-export([unknown_error_atom_test/1]).
-export([bad_static_type_test   /1]).

%%

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-spec all() -> [test_case_name()].

all() ->
    [
        known_error_test,
        unknown_error_test,
        unknown_error_atom_test,
        bad_static_type_test
    ].


-spec known_error_test(config()) ->
    ok.
known_error_test(_C) ->
    DE = #domain_Failure{
            code = <<"authorization_failed">>,
            sub = #domain_SubFailure{
                    code = <<"payment_tool_rejected">>,
                    sub = #domain_SubFailure{
                            code = <<"bank_card_rejected">>,
                            sub = #domain_SubFailure{
                                    code = <<"cvv_invalid">>
                                }
                        }
                }
        },
    SE = {authorization_failed,
            {payment_tool_rejected,
                {bank_card_rejected,
                    {cvv_invalid, #payprocerr_GeneralFailure{}}
                }
            }
        },
    DE = hg_errors:construct('PaymentFailure', SE),
    ok = hg_errors:match('PaymentFailure', DE, fun(SE_) when SE =:= SE_ -> ok end).

-spec unknown_error_atom_test(config()) ->
    ok.
unknown_error_atom_test(_C) ->
    UnknownCode = <<"unknown big fucking error">>,
    DE = #domain_Failure{
            code = UnknownCode
        },
    SE = {{unknown_error, UnknownCode}, #payprocerr_GeneralFailure{}},
    DE = hg_errors:construct('PaymentFailure', SE),
    ok = hg_errors:match('PaymentFailure', DE, fun(SE_) when SE =:= SE_  -> ok end).


-spec unknown_error_test(config()) ->
    ok.
unknown_error_test(_C) ->
    bad_error_code, % construct an atom
    UnknownCode = <<"ok">>,
    DE = #domain_Failure{
            code = UnknownCode
        },
    SE = {{unknown_error, UnknownCode}, #payprocerr_GeneralFailure{}},
    DE = hg_errors:construct('PaymentFailure', SE),
    ok = hg_errors:match('PaymentFailure', DE, fun(SE_) when SE =:= SE_  -> ok end).

-spec bad_static_type_test(config()) ->
    ok.
bad_static_type_test(_C) ->
    Bad = {qwe, #payprocerr_GeneralFailure{}},
    {'EXIT', {badarg, _}} =
        (catch hg_errors:construct('PaymentFailure', {authorization_failed, Bad})),
    {'EXIT', {badarg, _}} =
        (catch hg_errors:construct('PaymentFailure', {qwe, Bad})),
    {'EXIT', {badarg, _}} =
        (catch hg_errors:construct('PaymentFailure', Bad)),
    ok.
