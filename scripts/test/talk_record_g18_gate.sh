#!/usr/bin/env bash
# V3-18 G18 gate：talk-record 分片直传→assembly(Worker 拼接)→播放 Range→导出
# 真 Garage 全链。环境：A0_ENDPOINT/A0_AK/A0_SK（garage-v3run1.env）、本地
# PG(sys.local 4323)、ffmpeg（assembly 渲染）。独立 app VM（eunit 之外，
# fork 正常），承载 escript Worker 真链 + 边缘 302 承载。
# 退出：全链 PASS → exit 0；任一断言失败 → 非零。计数随日志落盘。
set -uo pipefail
B="$(cd "$(dirname "$0")/../.." && pwd)"
PORT="${G18_APP_PORT:-19881}"
EDGE_PORT="${G18_EDGE_PORT:-13992}"
export A0_ENDPOINT="${A0_ENDPOINT:?A0_ENDPOINT required}"
export A0_AK="${A0_AK:?A0_AK required}"
export A0_SK="${A0_SK:?A0_SK required}"
export G18_TALK_UPLOAD_ROOT="${G18_TALK_UPLOAD_ROOT:-/private/tmp/v3w5b-g18-talk-root}"
export T05_PG_HOST=127.0.0.1 T05_PG_PORT=4323 T05_PG_USER=imboy_user T05_PG_PASSWORD=abc54321 T05_PG_DB=talk_record_t05
mkdir -p "$G18_TALK_UPLOAD_ROOT/render"
# code:priv_dir(imboy) 需要路径段 <...>/imboy/ebin —— appalias（软链）
ALIAS_DIR="$B/.ct/appalias/imboy"
mkdir -p "$ALIAS_DIR"
rm -rf "$ALIAS_DIR/ebin" "$ALIAS_DIR/priv"
ln -sfn "$B/ebin" "$ALIAS_DIR/ebin"
ln -sfn "$B/priv" "$ALIAS_DIR/priv"
PA=""
for d in "$B"/deps/*/ebin; do PA="$PA -pa $d"; done
exec erl -noinput -pa "$ALIAS_DIR/ebin" -pa "$B/ebin" $PA -pa "$B/test/lib" \
  -config "$B/config/sys.local" \
  -eval '
application:load(imboy),
application:set_env(imboy, http_port, '"$PORT"'),
application:set_env(imboy, edge_internal_token, <<"v3run1-edge-internal-token">>),
application:set_env(imboy, edge_base_url, list_to_binary("http://127.0.0.1:'"$EDGE_PORT"'/edge")),
attachment_g18_gate:main('"$PORT"', '"$EDGE_PORT"')
'
