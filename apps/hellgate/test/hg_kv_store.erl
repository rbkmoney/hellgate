-module(hg_kv_store).

-export([start_link/1]).
-export([put/2]).
-export([get/1]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-spec start_link([]) -> pid().

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec put(term(), term()) -> ok.

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}, 5000).

-spec get(term()) -> term().

get(Key) ->
    gen_server:call(?MODULE, {get, Key}, 5000).

-spec init(term()) -> {ok, atom()}.

init(_) ->
    {ok, #{}}.

-spec handle_call(term(), pid(), atom()) -> {reply, atom(), atom()}.

handle_call({put, Key, Value}, _From, State) ->
    NewState = maps:update_with(Key, fun(_) -> Value end, undefined, State),
    {reply, ok, NewState};
handle_call({get, Key}, _From, State) ->
    Value = maps:get(Key, State, undefined),
    {reply, Value, State}.

-spec handle_cast(term(), atom()) ->  {noreply, atom()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), atom()) -> {noreply, atom()}.

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), atom()) -> atom().

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, atom()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.