#!/bin/bash
# 附件三门打点（teaching scope）：presign 守卫 / confirm fail-closed / view_url 五场景。
# 前置：seed_smoke.sql + attach_scope_prep.sql + 教学核心链（产生 submitted/withdrawn 提交）。
set -e; DIR="$(cd "$(dirname "$0")" && pwd)"; source "$DIR/env.sh"
B=$SMOKE_BASE
hit() { local M_=$1 P=$2 TK=$3; shift 3
  echo "--- $M_ $P $*"; echo "HTTP=$(curl -s -m 8 -o /tmp/smoke_last.json -w "%{http_code}" -X "$M_" "$B$P" -H "Authorization: Bearer $TK" "$@") BODY=$(head -c 260 /tmp/smoke_last.json)"; }
hit GET "/attachment/presign?filename=v.mp4&mime_type=video%2Fmp4&scope=teaching" "$TK_GUARDIAN"
hit GET "/attachment/presign?filename=v.exe&mime_type=application%2Fx-msdownload&scope=teaching" "$TK_GUARDIAN"
hit GET "/attachment/presign?filename=v.mp4&mime_type=video%2Fmp4&scope=teaching" "$TK_ORGB"
curl -s -m 8 -X POST "$B/attachment/confirm" -H "Authorization: Bearer $TK_GUARDIAN" -H "Content-Type: application/json" -d '{"object_key":"u770002/fake.mp4","scope":"teaching","file_hash256":"h","mime_type":"video/mp4","size":100}'; echo " <- confirm 未上传对象（fail-closed 400）"
hit GET "/attachment/view_url?object_key=u770002/nope.mp4" "$TK_GUARDIAN"
hit GET "/attachment/view_url?object_key=p/778002" "$TK_GUARDIAN"
hit GET "/attachment/view_url?object_key=p/778002" "$TK_ORGB"
hit GET "/attachment/view_url?object_key=p/778003" "$TK_GUARDIAN"
hit GET "/attachment/view_url?object_key=p/778002" "$TK_TEACHER"
