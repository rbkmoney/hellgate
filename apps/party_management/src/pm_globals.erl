-module(pm_globals).

-include_lib("damsel/include/dmsl_domain_thrift.hrl").

%% API
-export([reduce_globals/3]).

-type globals()         :: dmsl_domain_thrift:'Globals'().
-type varset()          :: pm_selector:varset().
-type domain_revision() :: pm_domain:revision().

-spec reduce_globals(globals(), varset(), domain_revision()) -> globals().

reduce_globals(Globals, VS, DomainRevision) ->
    Globals#domain_Globals{
        external_account_set = pm_selector:reduce(Globals#domain_Globals.external_account_set, VS, DomainRevision)
    }.
