-ifndef(__hg_duration__).
-define(__hg_duration__, 42).

-define(seconds(V) , (V)).
-define(minutes(V) , (?seconds(V) * 60)).
-define(hours(V)   , (?minutes(V) * 60)).
-define(days(V)    , (?hours(V) * 24)).

-define(MINUTE     , ?minutes(1)).
-define(HOUR       , ?hours(1)).
-define(DAY        , ?day(1)).

-endif.
