include "damsel/proto/base.thrift"
include "damsel/proto/domain.thrift"
include "damsel/proto/payment_processing.thrift"

namespace erlang pm

/**
 * Party state.
 * Used primarily for snapshotting.
 */
struct State {
    1: optional domain.Party party
    2: optional base.Timestamp timestamp
    3: optional map<payment_processing.ClaimID, payment_processing.Claim> claims = {}
    4: optional domain.PartyMeta meta = {}
    5: optional base.EventID last_event = 0
}
