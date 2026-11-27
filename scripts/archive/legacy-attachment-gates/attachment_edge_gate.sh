#!/usr/bin/env bash
# V3-08 下载边缘 gate：主 BEAM(app) + 边缘 OS 进程 + 真 Garage 端到端。
# 环境：A0_ENDPOINT/A0_AK/A0_SK（garage-v3run1.env）、本地 PG(sys.local)。
set -uo pipefail
B="$(cd "$(dirname "$0")/../.." && pwd)"
# code:priv_dir(imboy) 需要路径段 <...>/imboy/ebin —— appalias（软链，运行时 scratch）
ALIAS_DIR="$B/.ct/appalias/imboy"
mkdir -p "$ALIAS_DIR"
rm -rf "$ALIAS_DIR/ebin" "$ALIAS_DIR/priv"
ln -sfn "$B/ebin" "$ALIAS_DIR/ebin"
ln -sfn "$B/priv" "$ALIAS_DIR/priv"
PA=""
for d in "$B"/deps/*/ebin; do PA="$PA -pa $d"; done
exec erl -noinput -pa "$ALIAS_DIR/ebin" -pa "$B/ebin" $PA -pa "$B/test/lib" \
  -config "$B/config/sys.local" \
  -eval 'application:load(imboy), application:set_env(imboy, edge_internal_token, <<"v3run1-edge-internal-token">>), attachment_edge_gate:main()'
