-module(hg_action).

-export([new/0]).
-export([instant/0]).
-export([set_timeout/1]).
-export([set_timeout/2]).
-export([set_deadline/1]).
-export([set_deadline/2]).
-export([set_timer/1]).
-export([set_timer/2]).
-export([set_tag/1]).
-export([set_tag/2]).

-include_lib("hg_proto/include/hg_state_processing_thrift.hrl").

%%

-type tag() :: binary().
-type seconds() :: non_neg_integer().
-type datetime_iso8601() :: binary().
-type datetime() :: calendar:datetime() | datetime_iso8601().

-type timer() :: hg_base_thrift:'Timer'().
-type t() :: hg_state_processing_thrift:'ComplexAction'().

-export_type([t/0]).

%%

-spec new() -> t().

new() ->
    #'ComplexAction'{}.

-spec instant() -> t().

instant() ->
    set_timeout(0, new()).

-spec set_timeout(seconds()) -> t().

set_timeout(Seconds) ->
    set_timeout(Seconds, new()).

-spec set_timeout(seconds(), t()) -> t().

set_timeout(Seconds, Action) when is_integer(Seconds) andalso Seconds >= 0 ->
    set_timer({timeout, Seconds}, Action).

-spec set_deadline(datetime()) -> t().

set_deadline(Deadline) ->
    set_deadline(Deadline, new()).

-spec set_deadline(datetime(), t()) -> t().

set_deadline(Deadline, Action) ->
    set_timer({deadline, try_format_dt(Deadline)}, Action).

-spec set_timer(timer()) -> t().

set_timer(Timer) ->
    set_timer(Timer, new()).

-spec set_timer(timer(), t()) -> t().

set_timer(Timer, Action = #'ComplexAction'{}) ->
    Action#'ComplexAction'{set_timer = #'SetTimerAction'{timer = Timer}}.

-spec set_tag(tag()) -> t().

set_tag(Tag) ->
    set_tag(Tag, new()).

-spec set_tag(tag(), t()) -> t().

set_tag(Tag, Action = #'ComplexAction'{}) when is_binary(Tag) andalso byte_size(Tag) > 0 ->
    Action#'ComplexAction'{tag = #'TagAction'{tag = Tag}}.

%%

try_format_dt(Datetime = {_, _}) ->
    genlib_format:format_datetime_iso8601(Datetime);
try_format_dt(Datetime) when is_binary(Datetime) ->
    Datetime.
