-module(hg_claim_committer_handler).
-include_lib("damsel/include/dmsl_payment_processing_thrift.hrl").
-include_lib("damsel/include/dmsl_claim_management_thrift.hrl").

-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term()| no_return().

handle_function(Func, Args, Opts) ->
    scoper:scope(partymgmt,
        fun() -> handle_function_(Func, Args, Opts) end
    ).

-spec handle_function_(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function_(Fun, [PartyID, _Claim] = Args, _Opts) when Fun == 'Accept'; Fun == 'Commit' ->
    call(PartyID, Fun, Args).

call(PartyID, FunctionName, Args) ->
    ok = scoper:add_meta(#{party_id => PartyID}),
    ok = assert_party_accessible(PartyID),
    try
        hg_party_machine:call(PartyID, claim_committer, {'ClaimCommitter', FunctionName}, Args)
    catch
        throw:#payproc_PartyNotFound{} ->
            erlang:throw(#claim_management_PartyNotFound{})
    end.

assert_party_accessible(PartyID) ->
    UserIdentity = hg_woody_handler_utils:get_user_identity(),
    case hg_access_control:check_user(UserIdentity, PartyID) of
        ok ->
            ok;
        invalid_user ->
            throw(#claim_management_InvalidChangeset{reason = <<"invalid_user">>, invalid_changeset = []})
    end.
