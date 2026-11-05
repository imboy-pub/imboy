#!/bin/sh
set -eu
set -- /opt/imboy/erts-*/bin/escript
escript=$1
EB_PROOF_CODE_PATHS=
for path in /opt/imboy/lib/*/ebin; do
    EB_PROOF_CODE_PATHS="${EB_PROOF_CODE_PATHS:+$EB_PROOF_CODE_PATHS:}$path"
done
export EB_PROOF_CODE_PATHS
set -- /opt/imboy/lib/imboy-*/priv/workers/enterprise_direct_proof.escript
exec "$escript" "$1" --poll
