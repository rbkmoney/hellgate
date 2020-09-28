-module(pm_machine_action).

-export([new/0]).

-include_lib("mg_proto/include/mg_proto_state_processing_thrift.hrl").

%%

-type t() :: mg_proto_state_processing_thrift:'ComplexAction'().

-export_type([t/0]).

%%

-spec new() -> t().
new() ->
    #mg_stateproc_ComplexAction{}.
