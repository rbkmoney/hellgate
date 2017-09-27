-ifndef(__hellgate_payment_processing__).
-define(__hellgate_payment_processing__, 42).

%%
%% Recurrent Payment Tool
%%

% Events

-define(recurrent_payment_tool_event(RecurrentPaymentToolChanges),
    {recurrent_payment_tool_changes, RecurrentPaymentToolChanges}).

-define(recurrent_payment_tool_has_created(RecurrentPaymentTool),
    {recurrent_payment_tool_has_created,
        #payproc_RecurrentPaymentToolHasCreated{rec_payment_tool = RecurrentPaymentTool}}).

-define(recurrent_payment_tool_has_acquired(Token),
    {recurrent_payment_tool_has_acquired,
        #payproc_RecurrentPaymentToolHasAcquired{token = Token}}).

-define(recurrent_payment_tool_has_abandoned(),
    {recurrent_payment_tool_has_abandoned,
        #payproc_RecurrentPaymentToolHasAbandoned{}}).

-define(recurrent_payment_tool_has_failed(Failure),
    {recurrent_payment_tool_has_failed,
        #payproc_RecurrentPaymentToolHasFailed{failure = Failure}}).

% Statuses

-define(recurrent_payment_tool_created(),
    {recurrent_payment_tool_created, #payproc_RecurrentPaymentToolCreated{}}).

-define(recurrent_payment_tool_acquired(),
    {recurrent_payment_tool_acquired, #payproc_RecurrentPaymentToolAcquired{}}).

-define(recurrent_payment_tool_abandoned(),
    {recurrent_payment_tool_abandoned, #payproc_RecurrentPaymentToolAbandoned{}}).

-define(recurrent_payment_tool_failed(Failure),
    {recurrent_payment_tool_failed, #payproc_RecurrentPaymentToolFailed{failure = Failure}}).

%% Sessions

-define(session_ev(Payload),
    {session_change, #payproc_RecurrentPaymentToolSessionChange{
        payload = Payload
    }}
).

-define(session_started(),
    {session_started,
        #payproc_SessionStarted{}
    }
).
-define(session_finished(Result),
    {session_finished,
        #payproc_SessionFinished{result = Result}
    }
).
-define(session_suspended(),
    {session_suspended,
        #payproc_SessionSuspended{}
    }
).
-define(session_activated(),
    {session_activated,
        #payproc_SessionActivated{}
    }
).
-define(trx_bound(Trx),
    {session_transaction_bound,
        #payproc_SessionTransactionBound{trx = Trx}
    }
).
-define(proxy_st_changed(ProxySt),
    {session_proxy_state_changed,
        #payproc_SessionProxyStateChanged{proxy_state = ProxySt}
    }
).
-define(interaction_requested(UserInteraction),
    {session_interaction_requested,
        #payproc_SessionInteractionRequested{interaction = UserInteraction}
    }
).

-define(session_succeeded(),
    {succeeded, #payproc_SessionSucceeded{}}
).
-define(session_failed(Failure),
    {failed, #payproc_SessionFailed{failure = Failure}}
).

-endif.
