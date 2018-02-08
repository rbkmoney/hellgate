-module(hg_errors_SUITE).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").

-export([all/0]).

-export([known_error_test  /1]).
-export([unknown_error_test/1]).

%%

-type config() :: hg_ct_helper:config().
-type test_case_name() :: hg_ct_helper:test_case_name().

-spec all() -> [test_case_name()].

all() ->
    [
        known_error_test,
        unknown_error_test
    ].


-spec known_error_test(config()) ->
    ok.
known_error_test(_C) ->
    DE = #domain_Failure{
            code = <<"authorization_failure">>,
            sub = #domain_SubFailure{
                    code = <<"payment_tool_reject">>,
                    sub = #domain_SubFailure{
                            code = <<"bank_card_reject">>,
                            sub = #domain_SubFailure{
                                    code = <<"invalid_cvv">>
                                }
                        }
                }
        },
    SE = {authorization_failure,
            {payment_tool_reject,
                {bank_card_reject,
                    {invalid_cvv, #payprocerr_GeneralFailure{}}
                }
            }
        },
    SE = hg_errors:error_to_static(DE),
    DE = hg_errors:error_to_dynamic(SE),
    ok.

-spec unknown_error_test(config()) ->
    ok.
unknown_error_test(_C) ->
    UnknownCode = <<"unknown big fucking error">>,
    DE = #domain_Failure{
            code = UnknownCode
        },
    SE = {{unknown_error, UnknownCode}, #payprocerr_GeneralFailure{}},
    SE = hg_errors:error_to_static(DE),
    DE = hg_errors:error_to_dynamic(SE),
    ok.
