#!/bin/sh
set -eu
: "${EDGE_AUTHORIZE_URL:?EDGE_AUTHORIZE_URL is required}"
: "${EDGE_INTERNAL_TOKEN:?EDGE_INTERNAL_TOKEN is required}"
set -- /opt/imboy/erts-*/bin/escript
escript=$1
set -- /opt/imboy/lib/imboy-*/ebin
export EDGE_EBIN=$1
EDGE_CODE_PATHS=
for path in /opt/imboy/lib/*/ebin; do
    EDGE_CODE_PATHS="${EDGE_CODE_PATHS:+$EDGE_CODE_PATHS:}$path"
done
export EDGE_CODE_PATHS
set -- /opt/imboy/lib/imboy-*/priv/workers/download_edge.escript
exec "$escript" "$1"
