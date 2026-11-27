#!/usr/bin/env bash
# V3-13 G13 gate：Admin 头像直传发布全链（direct 会话 + 服务端跨桶 Copy + 真 Garage）。
# 环境：A0_ENDPOINT/A0_AK/A0_SK（garage-v3run1.env）、本地 PG(sys.local)。
set -uo pipefail
B="$(cd "$(dirname "$0")/../.." && pwd)"
ALIAS_DIR="$B/.ct/appalias/imboy"
mkdir -p "$ALIAS_DIR"
rm -rf "$ALIAS_DIR/ebin" "$ALIAS_DIR/priv"
ln -sfn "$B/ebin" "$ALIAS_DIR/ebin"
ln -sfn "$B/priv" "$ALIAS_DIR/priv"
PA=""
for d in "$B"/deps/*/ebin; do PA="$PA -pa $d"; done
exec erl -noinput -pa "$ALIAS_DIR/ebin" -pa "$B/ebin" $PA -pa "$B/test" -pa "$B/test/lib" \
  -config "$B/config/sys.local" \
  -eval 'avatar_publish_gate:main()'
