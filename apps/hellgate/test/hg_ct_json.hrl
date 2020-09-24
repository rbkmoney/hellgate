-ifndef(__hellgate_ct_json__).
-define(__hellgate_ct_json__, 42).

-include_lib("damsel/include/dmsl_json_thrift.hrl").

-define(null(), {nl, #json_Null{}}).

-endif.
