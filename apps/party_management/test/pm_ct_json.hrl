-ifndef(__pm_ct_json__).
-define(__pm_ct_json__, 42).

-include_lib("damsel/include/dmsl_json_thrift.hrl").

-define(null(), {nl, #json_Null{}}).

-endif.
