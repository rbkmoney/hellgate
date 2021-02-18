-module(hg_pantry).

-behaviour(gen_server).

-define(SERVER,   ?MODULE).
-define(TABLE,    ?MODULE).
-define(TBL_OPTS, [named_table, set, private, {keypos, #object.id}]).
-define(CHECK,    600000). % 10min
-define(TTL,        3600). % 1h

%% API
-export([
    child_spec/0,
    start_link/0,
    get/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-type party() :: {party, dmsl_domain_thrift:'PartyID'()}.

-record(object, {
    id   :: term(),
    data :: term(),
    tm   :: pos_integer()
}).

-record (state, {
    timer :: reference()
}).

-type state() :: #state{}.


%%----------------- API Implemantation
-spec child_spec() -> supervisor:child_spec().
child_spec() ->
    #{
        id      => ?MODULE,
        start   => {?MODULE, start_link, []},
        restart => permanent,
        type    => worker
    }.

-spec start_link() -> {ok, pid()} | {error, {already_started, pid()}}.
start_link() ->
    gen_server:start_link( {local, ?SERVER}, ?MODULE, [], [] ).


-spec get(party()) -> term() | {error, term()}.
get( {party, _ID} = Req ) ->
    gen_server:call( ?SERVER, Req );

get( _ ) ->
    {error, unkown_type}.


%%----------------- gen_server Implementation

-spec init(_) -> {ok, state()}.
init( _ ) ->
    create_tables(),
    {ok, #state{
        timer = start_check_timer()
    }}.


-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call( {Qualifier, ID}, _From, State ) ->
    case get_object( ID, get_request_func(Qualifier) ) of
        {ok, Object} ->
            put_object( ID, Object ), %% store | update cache
            {reply, Object, State};
        
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call( _Msg, _From, State ) ->
    {noreply, State}.


-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast( _Msg, State ) ->
    {noreply, State}.


-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info( check, #state{timer = Timer} = State ) ->
    is_reference(Timer) andalso erlang:cancel_timer( Timer ),

    purge_cache(),

    {noreply, State#state{
        timer = start_check_timer()
    }};

handle_info(_Msg, State) ->
    {noreply, State}.


-spec terminate(term(), state()) -> ok.
terminate( _Reason, _State ) ->
    ok.


-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change( _OldVsn, State, _Extra ) ->
    {ok, State}.


%%----------------- Internal functions
start_check_timer() ->
    erlang:send_after( ?CHECK, self(), check ).


create_tables() ->
    ?TABLE = ets:new( ?TABLE, ?TBL_OPTS ).


get_object( _ID, unkown_type = Err ) ->
    {error, Err};

get_object( ID, RequestFunc ) when is_function(RequestFunc, 1) ->
    try ets:lookup( ?TABLE, ID ) of
        [] -> 
            {ok, RequestFunc(ID)};

        [#object{data = Obj}] ->
            {ok, Obj}

    catch
        _:_ -> {error, object_not_found}
    end.


put_object( ID, Obj ) ->
    ets:insert( ?TABLE, #object{id = ID, data = Obj, tm = timestamp()} ).


timestamp() ->
    erlang:monotonic_time( second ).


-spec get_request_func(atom()) -> fun((term()) -> term()) | unkown_type.
get_request_func( party ) -> fun ( ID ) -> hg_party:get_party( ID ) end;
get_request_func( _     ) -> unkown_type.


purge_cache() ->
    PofNR = timestamp() - ?TTL,
    MS    = ets:fun2ms( fun( #object{tm = Tm} ) -> Tm < PofNR end ),
    ets:select_delete( ?TABLE, MS ).

