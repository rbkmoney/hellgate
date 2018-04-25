-module(hg_contract).

-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

%%

-export([create/4]).
-export([create_adjustment/4]).
-export([update_status/2]).

-export([get_categories/3]).
-export([get_adjustment/2]).
-export([get_payout_tool/2]).

-export([is_active/1]).
-export([is_live/2]).

%%

-type contract()              :: dmsl_domain_thrift:'Contract'().
-type contract_id()           :: dmsl_domain_thrift:'ContractID'().
-type contract_params()       :: dmsl_payment_processing_thrift:'ContractParams'().
-type contract_template()     :: dmsl_domain_thrift:'ContractTemplate'().
-type adjustment()            :: dmsl_domain_thrift:'ContractAdjustment'().
-type adjustment_id()         :: dmsl_domain_thrift:'ContractAdjustmentID'().
-type adjustment_params()     :: dmsl_payment_processing_thrift:'ContractAdjustmentParams'().
-type payout_tool()           :: dmsl_domain_thrift:'PayoutTool'().
-type payout_tool_id()        :: dmsl_domain_thrift:'PayoutToolID'().
-type category()              :: dmsl_domain_thrift:'CategoryRef'().
-type contract_template_ref() :: dmsl_domain_thrift:'ContractTemplateRef'().
-type payment_inst_ref()      :: dmsl_domain_thrift:'PaymentInstitutionRef'().

-type timestamp()             :: dmsl_base_thrift:'Timestamp'().
-type revision()              :: hg_domain:revision().

%%

-spec create(contract_id(), contract_params(), timestamp(), revision()) ->
    contract().

create(ID, Params, Timestamp, Revision) ->
    #payproc_ContractParams{
        contractor = Contractor,
        template = TemplateRef,
        payment_institution = PaymentInstitutionRef
    } = ensure_contract_creation_params(Params, Revision),
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, Revision),
    #domain_Contract{
        id = ID,
        contractor = Contractor,
        payment_institution = PaymentInstitutionRef,
        created_at = Timestamp,
        valid_since = instantiate_contract_lifetime_bound(ValidSince, Timestamp),
        valid_until = instantiate_contract_lifetime_bound(ValidUntil, Timestamp),
        status = {active, #domain_ContractActive{}},
        terms = TermSetHierarchyRef,
        adjustments = [],
        payout_tools = []
    }.

-spec update_status(contract(), timestamp()) ->
    contract().

update_status(
    #domain_Contract{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        status = {active, _}
    } = Contract,
    Timestamp
) ->
    case hg_datetime:between(Timestamp, ValidSince, ValidUntil) of
        true ->
            Contract;
        false ->
            Contract#domain_Contract{
                status = {expired, #domain_ContractExpired{}}
            }
    end;
update_status(Contract, _) ->
    Contract.

%% TODO should be in separate module
-spec create_adjustment(adjustment_id(), adjustment_params(), timestamp(), revision()) ->
    adjustment().

create_adjustment(ID, Params, Timestamp, Revision) ->
    #payproc_ContractAdjustmentParams{
        template = TemplateRef
    } = Params,
    #domain_ContractTemplate{
        valid_since = ValidSince,
        valid_until = ValidUntil,
        terms = TermSetHierarchyRef
    } = get_template(TemplateRef, Revision),
    #domain_ContractAdjustment{
        id = ID,
        created_at = Timestamp,
        valid_since = instantiate_contract_lifetime_bound(ValidSince, Timestamp),
        valid_until = instantiate_contract_lifetime_bound(ValidUntil, Timestamp),
        terms = TermSetHierarchyRef
    }.

-spec get_categories(contract() | contract_template(), timestamp(), revision()) ->
    ordsets:ordset(category()) | no_return().

get_categories(Contract, Timestamp, Revision) ->
    #domain_TermSet{
        payments = #domain_PaymentsServiceTerms{
            categories = CategorySelector
        }
    } = hg_party:get_terms(Contract, Timestamp, Revision),
    Value = hg_selector:reduce_to_value(CategorySelector, #{}, Revision),
    case ordsets:size(Value) > 0 of
        true ->
            Value;
        false ->
            error({misconfiguration, {'Empty set in category selector\'s value', CategorySelector, Revision}})
    end.

-spec get_adjustment(adjustment_id(), contract()) ->
    adjustment() | undefined.

get_adjustment(AdjustmentID, #domain_Contract{adjustments = Adjustments}) ->
    case lists:keysearch(AdjustmentID, #domain_ContractAdjustment.id, Adjustments) of
        {value, Adjustment} ->
            Adjustment;
        false ->
            undefined
    end.

-spec get_payout_tool(payout_tool_id(), contract()) ->
    payout_tool() | undefined.

get_payout_tool(PayoutToolID, #domain_Contract{payout_tools = PayoutTools}) ->
    case lists:keysearch(PayoutToolID, #domain_PayoutTool.id, PayoutTools) of
        {value, PayoutTool} ->
            PayoutTool;
        false ->
            undefined
    end.

-spec is_active(contract()) ->
    boolean().

is_active(#domain_Contract{status = {active, _}}) ->
    true;
is_active(_) ->
    false.

-spec is_live(contract(), revision()) ->
    boolean().

is_live(Contract, Revision) ->
    PaymentInstitutionRef = Contract#domain_Contract.payment_institution,
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    PaymentInstitution#domain_PaymentInstitution.realm =:= live.

%% Internals

-spec ensure_contract_creation_params(contract_params(), revision()) ->
    contract_params() | no_return().

ensure_contract_creation_params(
    #payproc_ContractParams{
        template = TemplateRef,
        payment_institution = PaymentInstitutionRef
    } = Params,
    Revision
) ->
    ValidRef = ensure_payment_institution(PaymentInstitutionRef),
    Params#payproc_ContractParams{
        template = ensure_contract_template(TemplateRef, ValidRef, Revision),
        payment_institution = ValidRef
    }.

-spec ensure_contract_template(contract_template_ref(), dmsl_domain_thrift:'PaymentInstitutionRef'(), revision()) ->
    contract_template_ref() | no_return().

ensure_contract_template(#domain_ContractTemplateRef{} = TemplateRef, _, _) ->
    TemplateRef;
ensure_contract_template(undefined, PaymentInstitutionRef, Revision) ->
    get_default_template_ref(PaymentInstitutionRef, Revision).

-spec ensure_payment_institution(payment_inst_ref()) ->
    payment_inst_ref() | no_return().

ensure_payment_institution(#domain_PaymentInstitutionRef{} = PaymentInstitutionRef) ->
    PaymentInstitutionRef;
ensure_payment_institution(undefined) ->
    throw({payment_institution_invalid, undefined}).

get_template(TemplateRef, Revision) ->
    try
        hg_domain:get(Revision, {contract_template, TemplateRef})
    catch
        error:{object_not_found, _} ->
            throw({template_invalid, TemplateRef})
    end.

get_payment_institution(PaymentInstitutionRef, Revision) ->
    try
        hg_domain:get(Revision, {payment_institution, PaymentInstitutionRef})
    catch
        error:{object_not_found, _} ->
            throw({payment_institution_invalid, PaymentInstitutionRef})
    end.

get_default_template_ref(PaymentInstitutionRef, Revision) ->
    PaymentInstitution = get_payment_institution(PaymentInstitutionRef, Revision),
    ContractTemplateSelector = PaymentInstitution#domain_PaymentInstitution.default_contract_template,
    % TODO fill varset properly
    hg_selector:reduce_to_value(ContractTemplateSelector, #{}, Revision).

instantiate_contract_lifetime_bound(undefined, _) ->
    undefined;
instantiate_contract_lifetime_bound({timestamp, Timestamp}, _) ->
    Timestamp;
instantiate_contract_lifetime_bound({interval, Interval}, Timestamp) ->
    add_interval(Timestamp, Interval).

add_interval(Timestamp, Interval) ->
    #domain_LifetimeInterval{
        years = YY,
        months = MM,
        days = DD
    } = Interval,
    hg_datetime:add_interval(Timestamp, {YY, MM, DD}).
