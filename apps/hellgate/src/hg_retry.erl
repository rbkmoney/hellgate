-module(hg_retry).

-export([
    next_step/1,
    skip_steps/2,
    new_strategy/1,
    call_with_retry/2
]).

-type retries_num() :: pos_integer() | infinity.
-type policy_spec() ::
    {linear, retries_num() | {max_total_timeout, pos_integer()}, pos_integer()}
    | {exponential, retries_num() | {max_total_timeout, pos_integer()}, number(), pos_integer()}
    | {exponential, retries_num() | {max_total_timeout, pos_integer()}, number(), pos_integer(), timeout()}
    | {intervals, [pos_integer(), ...]}
    | {timecap, timeout(), policy_spec()}
    | no_retry.

-type strategy() :: genlib_retry:strategy().

-export_type([
    strategy/0,
    policy_spec/0,
    retries_num/0
]).

%%% API

-spec new_strategy(policy_spec()) -> strategy().
new_strategy({linear, Retries, Timeout}) ->
    genlib_retry:linear(Retries, Timeout);
new_strategy({exponential, Retries, Factor, Timeout}) ->
    genlib_retry:exponential(Retries, Factor, Timeout);
new_strategy({exponential, Retries, Factor, Timeout, MaxTimeout}) ->
    genlib_retry:exponential(Retries, Factor, Timeout, MaxTimeout);
new_strategy({intervals, Array}) ->
    genlib_retry:intervals(Array);
new_strategy({timecap, Timeout, Policy}) ->
    genlib_retry:timecap(Timeout, new_strategy(Policy));
new_strategy(no_retry) ->
    finish;
new_strategy(BadPolicy) ->
    erlang:error(badarg, [BadPolicy]).

-spec next_step(strategy()) -> {wait, non_neg_integer(), strategy()} | finish.
next_step(Strategy) ->
    genlib_retry:next_step(Strategy).

-spec skip_steps(strategy(), non_neg_integer()) -> strategy().
skip_steps(Strategy, 0) ->
    Strategy;
skip_steps(Strategy, N) when N > 0 ->
    NewStrategy =
        case genlib_retry:next_step(Strategy) of
            {wait, _Timeout, NextStrategy} ->
                NextStrategy;
            finish = NextStrategy ->
                NextStrategy
        end,
    skip_steps(NewStrategy, N - 1).

-type retry_fun_return() :: term() | no_return().
-type retry_fun() :: fun(() -> retry_fun_return()).

-spec call_with_retry(retry_fun(), strategy()) -> retry_fun_return().
call_with_retry(Fun, Strategy) ->
    case Fun() of
        {return, Result} ->
            Result;
        retry ->
            case next_step(Strategy) of
                {wait, Timeout, NextStrategy} ->
                    _ = timer:sleep(Timeout),
                    call_with_retry(Fun, NextStrategy);
                finish ->
                    throw({error, no_more_retries})
            end
    end.

%%% Internal functions
