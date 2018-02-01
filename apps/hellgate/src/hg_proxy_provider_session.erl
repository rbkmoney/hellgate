-module(hg_proxy_provider_session).
-include_lib("dmsl/include/dmsl_proxy_provider_thrift.hrl").
-include_lib("dmsl/include/dmsl_payment_processing_thrift.hrl").

-export([merge_change/2]).
-export([create/0, create/2]).
-export([get_status/1]).
-export([get_trx/1]).
-export([get_tags/1]).
-export([get_target/1]).
-export([get_result/1]).
-export([get_proxy_state/1]).

-export([construct_prxprv_session/1]).

-export([marshal/1]).
-export([unmarshal/1]).

-record(st, {
    target      :: undefined | target(),
    status      :: status(),
    trx         :: undefined | trx_info(),
    tags        :: undefined | [tag()],
    result      :: undefined | result(),
    proxy_state :: undefined | proxy_state()
}).

-type status()  :: active | suspended | finished.
-type st() :: #st{}.

-export_type([st/0]).

-type target()            :: dmsl_domain_thrift:'TargetInvoicePaymentStatus'().
-type trx_info()          :: dmsl_domain_thrift:'TransactionInfo'().
-type result()            :: dmsl_payment_processing_thrift:'SessionResult'().
-type proxy_state()       :: dmsl_proxy_thrift:'ProxyState'().
-type tag()               :: dmsl_proxy_thrift:'CallbackTag'().
-type prxprv_session()    :: dmsl_proxy_provider_thrift:'Session'().

-type change() ::
    dmsl_payment_processing_thrift:'SessionChangePayload'().

-include("payment_events.hrl").

-spec merge_change(change(), st()) -> st().

merge_change(?session_finished(Result), Session) ->
    Session#st{status = finished, result = Result};
merge_change(?session_activated(), Session) ->
    Session#st{status = active};
merge_change(?session_suspended(undefined), Session) ->
    Session#st{status = suspended};
merge_change(?session_suspended(Tag), Session) ->
    Session#st{status = suspended, tags = [Tag | get_tags(Session)]};
merge_change(?trx_bound(Trx), Session) ->
    Session#st{trx = Trx};
merge_change(?proxy_st_changed(ProxyState), Session) ->
    Session#st{proxy_state = ProxyState};
merge_change(?interaction_requested(_), Session) ->
    Session.

-spec create() -> st().

create() ->
    #st{status = active}.

-spec create(target(), trx_info() | undefined) -> st().

create(Target, Trx) ->
    #st{
        target = Target,
        status = active,
        trx    = Trx,
        tags   = []
    }.

-spec get_status(st()) -> status().

get_status(#st{status = Status}) ->
    Status.

-spec get_trx(st()) -> trx_info() | undefined.

get_trx(#st{trx = Trx}) ->
    Trx.

-spec get_tags(st()) -> [tag()] | undefined.

get_tags(#st{tags = Tags}) ->
    Tags.

-spec get_target(st()) -> target() | undefined.

get_target(#st{target = Target}) ->
    Target.

-spec get_result(st()) -> result() | undefined.

get_result(#st{result = Result}) ->
    Result.

-spec get_proxy_state(st()) -> proxy_state() | undefined.

get_proxy_state(#st{proxy_state = ProxyState}) ->
    ProxyState.

-spec construct_prxprv_session(st()) -> prxprv_session().

construct_prxprv_session(#st{target = Target, proxy_state = ProxyState}) ->
    #prxprv_Session{
        target = Target,
        state = ProxyState
    }.

%% Marshalling

-include("legacy_structures.hrl").

-spec marshal(change()) ->
    hg_msgpack_marshalling:value().

marshal(Change) ->
    marshal(change, Change).

marshal(change, ?session_started()) ->
    [3, <<"started">>];
marshal(change, ?session_finished(Result)) ->
    [3, [
        <<"finished">>,
        marshal(status, Result)
    ]];
marshal(change, ?session_suspended(Tag)) ->
    [3, [
        <<"suspended">>,
        marshal(str, Tag)
    ]];
marshal(change, ?session_activated()) ->
    [3, <<"activated">>];
marshal(change, ?trx_bound(Trx)) ->
    [3, [
        <<"transaction_bound">>,
        marshal(trx, Trx)
    ]];
marshal(change, ?proxy_st_changed(ProxySt)) ->
    [3, [
        <<"proxy_state_changed">>,
        marshal(bin, {bin, ProxySt})
    ]];
marshal(change, ?interaction_requested(UserInteraction)) ->
    [3, [
        <<"interaction_requested">>,
        marshal(interaction, UserInteraction)
    ]];

%% Session status

marshal(status, ?session_succeeded()) ->
    <<"succeeded">>;
marshal(status, ?session_failed(PayloadFailure)) ->
    [
        <<"failed">>,
        marshal(failure, PayloadFailure)
    ];

marshal(trx, #domain_TransactionInfo{} = TransactionInfo) ->
    genlib_map:compact(#{
        <<"id">>            => marshal(str, TransactionInfo#domain_TransactionInfo.id),
        <<"timestamp">>     => marshal(str, TransactionInfo#domain_TransactionInfo.timestamp),
        <<"extra">>         => marshal(map_str, TransactionInfo#domain_TransactionInfo.extra)
    });

marshal(interaction, {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}}) ->
    #{<<"redirect">> =>
        [
            <<"get_request">>,
            marshal(str, URI)
        ]
    };
marshal(interaction, {redirect, {post_request, #'BrowserPostRequest'{uri = URI, form = Form}}}) ->
    #{<<"redirect">> =>
        [
            <<"post_request">>,
            #{
                <<"uri">>   => marshal(str, URI),
                <<"form">>  => marshal(map_str, Form)
            }
        ]
    };
marshal(interaction, {payment_terminal_reciept, #'PaymentTerminalReceipt'{short_payment_id = SPID, due = DueDate}}) ->
    #{<<"payment_terminal_receipt">> =>
        #{
            <<"spid">>  => marshal(str, SPID),
            <<"due">>   => marshal(str, DueDate)
        }
    };

marshal(failure, {operation_timeout, _}) ->
    [2, <<"operation_timeout">>];
marshal(failure, {external_failure, #domain_ExternalFailure{} = ExternalFailure}) ->
    [2, [<<"external_failure">>, genlib_map:compact(#{
        <<"code">>          => marshal(str, ExternalFailure#domain_ExternalFailure.code),
        <<"description">>   => marshal(str, ExternalFailure#domain_ExternalFailure.description)
    })]];

marshal(_, Other) ->
    Other.

%% Unmarshalling

-spec unmarshal(hg_msgpack_marshalling:value()) ->
    change().

unmarshal(Change) ->
    unmarshal(change, Change).

unmarshal(change, [3, [<<"suspended">>, Tag]]) ->
    ?session_suspended(unmarshal(str, Tag));
unmarshal(change, [3, Change]) ->
    unmarshal(change, [2, Change]);

unmarshal(change, [2, <<"started">>]) ->
    ?session_started();
unmarshal(change, [2, [<<"finished">>, Result]]) ->
    ?session_finished(unmarshal(status, Result));
unmarshal(change, [2, <<"suspended">>]) ->
    ?session_suspended(undefined);
unmarshal(change, [2, <<"activated">>]) ->
    ?session_activated();
unmarshal(change, [2, [<<"transaction_bound">>, Trx]]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(change, [2, [<<"proxy_state_changed">>, {bin, ProxySt}]]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(change, [2, [<<"interaction_requested">>, UserInteraction]]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

unmarshal(change, [1, ?legacy_session_started()]) ->
    ?session_started();
unmarshal(change, [1, ?legacy_session_finished(Result)]) ->
    ?session_finished(unmarshal(status, Result));
unmarshal(change, [1, ?legacy_session_suspended()]) ->
    ?session_suspended(undefined);
unmarshal(change, [1, ?legacy_session_activated()]) ->
    ?session_activated();
unmarshal(change, [1, ?legacy_trx_bound(Trx)]) ->
    ?trx_bound(unmarshal(trx, Trx));
unmarshal(change, [1, ?legacy_proxy_st_changed(ProxySt)]) ->
    ?proxy_st_changed(unmarshal(bin, ProxySt));
unmarshal(change, [1, ?legacy_interaction_requested(UserInteraction)]) ->
    ?interaction_requested(unmarshal(interaction, UserInteraction));

%% Session status

unmarshal(status, <<"succeeded">>) ->
    ?session_succeeded();
unmarshal(status, [<<"failed">>, Failure]) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(status, ?legacy_session_succeeded()) ->
    ?session_succeeded();
unmarshal(status, ?legacy_session_failed(Failure)) ->
    ?session_failed(unmarshal(failure, Failure));

unmarshal(trx, #{
    <<"id">>    := ID,
    <<"extra">> := Extra
} = TRX) ->
    Timestamp = maps:get(<<"timestamp">>, TRX, undefined),
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(trx, ?legacy_trx(ID, Timestamp, Extra)) ->
    #domain_TransactionInfo{
        id          = unmarshal(str, ID),
        timestamp   = unmarshal(str, Timestamp),
        extra       = unmarshal(map_str, Extra)
    };

unmarshal(interaction, #{<<"redirect">> := [<<"get_request">>, URI]}) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, #{<<"redirect">> := [<<"post_request">>, #{
    <<"uri">>   := URI,
    <<"form">>  := Form
}]}) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};
unmarshal(interaction, #{<<"payment_terminal_receipt">> := #{
    <<"spid">>  := SPID,
    <<"due">>   := DueDate
}}) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(interaction, ?legacy_get_request(URI)) ->
    {redirect, {get_request, #'BrowserGetRequest'{uri = URI}}};
unmarshal(interaction, ?legacy_post_request(URI, Form)) ->
    {redirect, {post_request,
        #'BrowserPostRequest'{
            uri     = unmarshal(str, URI),
            form    = unmarshal(map_str, Form)
        }
    }};
unmarshal(interaction, ?legacy_payment_terminal_reciept(SPID, DueDate)) ->
    {payment_terminal_reciept, #'PaymentTerminalReceipt'{
        short_payment_id = unmarshal(str, SPID),
        due = unmarshal(str, DueDate)
    }};

unmarshal(failure, [2, <<"operation_timeout">>]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [2, [<<"external_failure">>, #{<<"code">> := Code} = ExternalFailure]]) ->
    Description = maps:get(<<"description">>, ExternalFailure, undefined),
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(failure, [1, ?legacy_operation_timeout()]) ->
    {operation_timeout, #domain_OperationTimeout{}};
unmarshal(failure, [1, ?legacy_external_failure(Code, Description)]) ->
    {external_failure, #domain_ExternalFailure{
        code        = unmarshal(str, Code),
        description = unmarshal(str, Description)
    }};

unmarshal(_, Other) ->
    Other.