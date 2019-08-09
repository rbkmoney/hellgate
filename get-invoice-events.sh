#!/bin/bash

USAGE=$(cat <<EOF
Usage: ${SCRIPTNAME} id [ns]
  Fails a machine. Exploits the fact that most if not all already implemented
  machines do not tolerate arbitrary machinegun calls so it throws nils at them
  as a call args.
  id      Machine ID (string)
  ns      Machine namespace (string, default = invoice)

More information:
  https://github.com/rbkmoney/machinegun_proto
EOF
)

function usage {
    echo "${USAGE}"
    exit 127
}

[ -f woorlrc ] && source woorlrc

ID="${1}"
NS="${2:-invoice}"
[ -z "${ID}" ] && usage

MACHINEDESC=$(cat <<END
  {
    "ns": "${NS}",
    "ref": {"id": "${ID}"},
    "range": {"limit": 100, "direction": "forward"}
  }
END
)

${WOORL:-woorl} -s _build/default/lib/mg_proto/proto/state_processing.thrift \
    "http://${MACHINEGUN:-machinegun}:8022/v1/automaton" \
    Automaton GetMachine "${MACHINEDESC}"
