-module(hg_dummy_cashreg_provider).
-behaviour(hg_woody_wrapper).

-export([handle_function/3]).

-behaviour(hg_test_proxy).

-export([get_service_spec/0]).
-export([get_http_cowboy_spec/0]).

-export([get_callback_url/0]).

%% cowboy http callbacks
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%

-define(COWBOY_PORT, 9977).

-define(sleep(To),
    {sleep, #'cashreg_adptprv_SleepIntent'{timer = {timeout, To}}}).
-define(suspend(Tag, To),
    {suspend, #'cashreg_adptprv_SuspendIntent'{tag = Tag, timeout = {timeout, To}}}).
-define(finish_success(ReceiptRegEntry),
    {finish, #'cashreg_adptprv_FinishIntent'{
        status = {success, #'cashreg_adptprv_Success'{receipt_reg_entry = ReceiptRegEntry}}
    }}).
-define(finish_failure(ErrorCode),
    {finish, #'cashreg_adptprv_FinishIntent'{status = {failure, #'cashreg_adptprv_Failure'{
        error = {receipt_registration_failed, #cashreg_main_ReceiptRegistrationFailed{
            reason = #cashreg_main_ExternalFailure{code = ErrorCode}
        }}
    }}}}).

-spec get_service_spec() ->
    hg_proto:service_spec().

get_service_spec() ->
    {"/test/proxy/cashreg_provider/dummy", {cashreg_proto_adapter_provider_thrift, 'ProviderAdapter'}}.

-spec get_http_cowboy_spec() -> #{}.

get_http_cowboy_spec() ->
    Dispatch = cowboy_router:compile([{'_', [{"/", ?MODULE, []}]}]),
    #{
        listener_ref => ?MODULE,
        acceptors_count => 10,
        transport_opts => [{port, ?COWBOY_PORT}],
        proto_opts => [{env, [{dispatch, Dispatch}]}]
    }.

%%

-include_lib("cashreg_proto/include/cashreg_proto_main_thrift.hrl").
-include_lib("cashreg_proto/include/cashreg_proto_adapter_provider_thrift.hrl").
-include_lib("hellgate/include/cashreg_events.hrl").

-spec handle_function(woody:func(), woody:args(), hg_woody_wrapper:handler_opts()) ->
    term() | no_return().

handle_function(
    'RegisterReceipt',
    [
        #cashreg_adptprv_ReceiptContext{
            receipt_params = _,
            options = Options
        },
        #cashreg_adptprv_Session{state = State}
    ],
    _
) ->
    register_receipt(State, Options);
handle_function(
    'HandleReceiptCallback',
    _,
    _
) ->
    Receipt = get_default_receipt_result(),
    handle_register_callback(Receipt).

%%

register_receipt(
    undefined,
    #{<<"adapter_state">> := <<"sleeping">>}
) ->
    receipt_sleep(1, {str, <<"sleeping">>});
register_receipt(
    undefined,
    #{<<"adapter_state">> := <<"suspending">>, <<"tag">> := Tag}
) ->
    receipt_suspend(Tag, 1, {str, <<"suspending">>});
register_receipt(
    undefined,
    #{<<"adapter_state">> := <<"finishing_failure">>}
) ->
    receipt_finish(?finish_failure(<<"400">>), undefined);
register_receipt(
    {str, <<"sleeping">>},
    _
) ->
    receipt_sleep(1, {str, <<"finishing_success">>});
register_receipt(
    {str, <<"finishing_success">>},
    _
) ->
    Receipt = get_default_receipt_result(),
    receipt_finish(?finish_success(Receipt), undefined).

receipt_finish(Intent, State) ->
    #cashreg_adptprv_ReceiptAdapterResult{
        intent = Intent,
        next_state = State
    }.

receipt_sleep(Timeout, State) ->
    #cashreg_adptprv_ReceiptAdapterResult{
        intent     = ?sleep(Timeout),
        next_state = State
    }.

receipt_suspend(Tag, Timeout, State) ->
    #cashreg_adptprv_ReceiptAdapterResult{
        intent     = ?suspend(Tag, Timeout),
        next_state = State
    }.

handle_register_callback(Receipt) ->
    #cashreg_adptprv_ReceiptCallbackResult{
        response = {str, <<"ok">>},
        result = #cashreg_adptprv_ReceiptCallbackAdapterResult{
            intent     = ?finish_success(Receipt),
            next_state = undefined
        }
    }.

%%

-spec init(atom(), cowboy_req:req(), list()) -> {ok, cowboy_req:req(), state}.

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

-spec handle(cowboy_req:req(), state) -> {ok, cowboy_req:req(), state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = handle_adapter_callback(Method, Req2),
    {ok, Req3, State}.

-spec terminate(term(), cowboy_req:req(), state) -> ok.

terminate(_Reason, _Req, _State) ->
    ok.

-spec get_callback_url() -> binary().

get_callback_url() ->
    genlib:to_binary("http://127.0.0.1:" ++ integer_to_list(?COWBOY_PORT)).

handle_adapter_callback(<<"POST">>, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Form = maps:from_list(cow_qs:parse_qs(Body)),
    Tag = maps:get(<<"tag">>, Form),
    Callback = maps:get(<<"callback">>, Form, {str, Tag}),
    RespCode = callback_to_hellgate(Tag, Callback),
    cowboy_req:reply(RespCode, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], <<>>, Req2);
handle_adapter_callback(_, Req) ->
    %% Method not allowed.
    cowboy_req:reply(405, Req).

callback_to_hellgate(Tag, Callback) ->
    case hg_client_api:call(
        cashreg_host_provider, 'ProcessReceiptCallback', [Tag, Callback],
        hg_client_api:new(hg_ct_helper:get_hellgate_url())
    ) of
        {{ok, _Response}, _} ->
            200;
        {{error, _}, _} ->
            500
    end.

get_default_receipt_result() ->
    #'cashreg_main_ReceiptRegistrationEntry'{id = <<"1">>, metadata = {nl, #cashreg_msgpack_Nil{}}}.
