-module(hg_cashreg_provider).
-include_lib("cashreg_proto/include/cashreg_proto_adapter_provider_thrift.hrl").

-export([register_receipt/3]).
-export([handle_receipt_callback/4]).

-type receipt_context() :: cashreg_proto_adapter_provider_thrift:'ReceiptContext'().
-type session()         :: cashreg_proto_adapter_provider_thrift:'Session'().
-type adapter()         :: cashreg_proto_adapter_provider_thrift:'Adapter'().
-type callback()        :: cashreg_proto_adapter_provider_thrift:'Callback'().

-spec register_receipt(receipt_context(), session(), adapter()) ->
    term().
register_receipt(ReceiptContext, Session, Adapter) ->
    issue_call('RegisterReceipt', [ReceiptContext, Session], Adapter).

-spec handle_receipt_callback(callback(), receipt_context(), session(), adapter()) ->
    term().
handle_receipt_callback(Callback, ReceiptContext, Session, Adapter) ->
    issue_call('HandleReceiptCallback', [Callback, ReceiptContext, Session], Adapter).

issue_call(Func, Args, Adapter) ->
    hg_woody_wrapper:call(cashreg_provider, Func, Args, construct_call_options(Adapter)).

construct_call_options(#cashreg_adptprv_Adapter{url = URL}) ->
    Opts = case genlib_app:env(hellgate, adapter_opts, #{}) of
        #{transport_opts := TransportOpts = #{}} ->
            #{transport_opts => maps:to_list(maps:with([connect_timeout, recv_timeout], TransportOpts))};
        #{} ->
            #{}
    end,
    Opts#{url => URL}.
