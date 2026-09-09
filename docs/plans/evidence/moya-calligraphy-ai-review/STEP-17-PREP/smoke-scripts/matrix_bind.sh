#!/bin/bash
# 绑定/解绑打点（守卫 5429 / invalid target 422+5428 / 未绑解绑 409 / 本人 history 分支）。
set -e; DIR="$(cd "$(dirname "$0")" && pwd)"; source "$DIR/env.sh"
B=$SMOKE_BASE
hit() { local M_=$1 P=$2 TK=$3 BODY=$4
  echo "--- $M_ $P"; echo "HTTP=$(curl -s -m 8 -o /tmp/smoke_last.json -w "%{http_code}" -X "$M_" "$B$P" -H "Authorization: Bearer $TK" -H "Content-Type: application/json" ${BODY:+-d "$BODY"}) BODY=$(head -c 260 /tmp/smoke_last.json)"; }
hit POST /teaching/learners/774001/unbind "$TK_MANAGER"
hit POST /teaching/learners/774001/bind "$TK_GUARDIAN" '{"user_id":770006}'
hit POST /teaching/learners/774001/bind "$TK_MANAGER" '{"user_id":999999999}'
hit POST /teaching/learners/774001/bind "$TK_MANAGER" '{"user_id":"770006"}'
hit GET /teaching/learners/774001/history "$TK_TARGET"
hit POST /teaching/learners/774001/unbind "$TK_MANAGER"
hit GET /teaching/learners/774001/history "$TK_TARGET"
