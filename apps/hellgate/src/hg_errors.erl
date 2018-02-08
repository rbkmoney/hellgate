-module(hg_errors).

-export([error_to_static /1]).
-export([error_to_dynamic/1]).

-include_lib("dmsl/include/dmsl_domain_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_errors_thrift.hrl").

%%

-type static_code() :: atom() | {unknown_error, dynamic_code()}.
-type static_error() :: {static_code(), static_sub_error()}.
-type static_sub_error() ::
      {static_code(), static_sub_error()}
    | dmsl_payment_processing_errors_thrift:'GeneralFailure'()
.

-type dynamic_code() :: binary().
-type dynamic_error() :: dmsl_domain_thrift:'Failure'().
-type dynamic_sub_error() :: dmsl_domain_thrift:'SubFailure'() | undefined.

%%

-spec error_to_static(dynamic_error()) ->
    static_error().
error_to_static(#domain_Failure{code = Code, sub = SDE}) ->
    {code_to_static(Code), sub_error_to_static(SDE)}.


-spec sub_error_to_static(dynamic_sub_error()) ->
    static_sub_error().
sub_error_to_static(undefined) ->
    #payprocerr_GeneralFailure{};
sub_error_to_static(#domain_SubFailure{code = Code, sub = SDE}) ->
    {code_to_static(Code), sub_error_to_static(SDE)}.

-spec code_to_static(dynamic_code()) ->
    static_code().
code_to_static(Code) ->
    try
        erlang:binary_to_existing_atom(Code, utf8)
    catch error:badarg ->
        {unknown_error, Code}
    end.

%%

-spec error_to_dynamic(static_error()) ->
    dynamic_error().
error_to_dynamic({Code, SSE}) ->
    #domain_Failure{code = code_to_dynamic(Code), sub = sub_error_to_dynamic(SSE)}.

-spec sub_error_to_dynamic(static_sub_error()) ->
    dynamic_sub_error().
sub_error_to_dynamic(#payprocerr_GeneralFailure{}) ->
    undefined;
sub_error_to_dynamic({Code, SSE}) ->
    #domain_SubFailure{code = code_to_dynamic(Code), sub = sub_error_to_dynamic(SSE)}.

-spec code_to_dynamic(static_code()) ->
    dynamic_code().
code_to_dynamic({unknown_error, Code}) ->
    Code;
code_to_dynamic(Code) ->
    erlang:atom_to_binary(Code, utf8).

%%

