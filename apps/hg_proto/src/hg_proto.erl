-module(hg_proto).

-export([get_service_specs/0]).
-export([get_service_spec/1]).

-export_type([service_spec/0]).

%%

-type service_spec() :: {Name :: atom(), Path :: string(), Service :: {module(), atom()}}.

-spec get_service_specs() -> [service_spec()].

get_service_specs() ->
    VersionPrefix = "/v1",
    [
        {eventsink, VersionPrefix ++ "/processing/eventsink",
            {hg_payment_processing_thrift, 'EventSink'}},
        {invoicing, VersionPrefix ++ "/processing/invoicing",
            {hg_payment_processing_thrift, 'Invoicing'}},
        {processor, VersionPrefix ++ "/stateproc/processor",
            {hg_state_processing_thrift, 'Processor'}}
    ].

-spec get_service_spec(Name :: atom()) -> service_spec() | false.

get_service_spec(Name) ->
    lists:keyfind(Name, 1, get_service_specs()).
