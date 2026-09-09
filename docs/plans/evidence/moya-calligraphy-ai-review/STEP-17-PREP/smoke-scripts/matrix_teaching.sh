#!/bin/bash
# 教学核心链打点（契约矩阵：contexts/switch/assignments/submissions 幂等/撤回/review 全链/history）。
# 前置：seed_smoke.sql 已执行、冒烟节点已 boot（backend-boot-smoke.md 配方）、本目录 env.sh 可 source。
set -e; DIR="$(cd "$(dirname "$0")" && pwd)"; source "$DIR/env.sh"
B=$SMOKE_BASE
hit() { local M_=$1 P=$2 TK=$3 BODY=$4 HDR=$5
  local ARGS=(-s -m 8 -o /tmp/smoke_last.json -w "%{http_code}" -X "$M_" "$B$P" -H "Authorization: Bearer $TK" -H "Content-Type: application/json")
  [ -n "$HDR" ] && ARGS+=(-H "$HDR"); [ -n "$BODY" ] && ARGS+=(-d "$BODY")
  echo "--- $M_ $P"; echo "HTTP=$(curl "${ARGS[@]}") BODY=$(head -c 300 /tmp/smoke_last.json)"; }
hit GET /teaching/contexts "$TK_GUARDIAN"
hit POST /teaching/context/switch "$TK_GUARDIAN" '{"context_type":"guardian","learner_id":"774001"}'
hit GET "/teaching/assignments?learner_id=774001" "$TK_GUARDIAN"
hit GET /teaching/assignments/776001 "$TK_GUARDIAN"
hit GET /teaching/assignments/776001 "$TK_ORGB"
CB='{"learner_id":"774001","note":"smoke","assets":[{"attachment_id":"778001","kind":"practice_video"},{"attachment_id":"778002","kind":"final_photo"}]}'
hit POST /teaching/assignments/776001/submissions "$TK_GUARDIAN" "$CB" "Idempotency-Key: key77-r8-1"
hit POST /teaching/assignments/776001/submissions "$TK_GUARDIAN" '{"learner_id":"774001","note":"CONFLICT","assets":[{"attachment_id":"778001","kind":"practice_video"}]}' "Idempotency-Key: key77-r8-1"
SID=$(curl -s -m 8 -X POST "$B/teaching/assignments/776001/submissions" -H "Authorization: Bearer $TK_GUARDIAN" -H "Content-Type: application/json" -H "Idempotency-Key: key77-smoke-new" -d "$CB" | python3 -c 'import sys,json; print(json.load(sys.stdin)["payload"]["submission_id"])')
echo "SID=$SID"
hit GET "/teaching/submissions/$SID" "$TK_GUARDIAN"
hit GET /teaching/review-queue "$TK_TEACHER"
hit GET "/teaching/submissions/$SID/review-workbench" "$TK_TEACHER"
hit GET "/teaching/submissions/$SID/review-workbench" "$TK_GUARDIAN"
hit PUT "/teaching/submissions/$SID/review-draft" "$TK_ASSISTANT" '{"positive_point":"x"}'
hit PUT "/teaching/submissions/$SID/review-draft" "$TK_TEACHER" '{"positive_point":"p","focus_problem":"f","practice_action":"a"}'
hit POST "/teaching/submissions/$SID/reviews/publish" "$TK_TEACHER" '{"confirm_learner_id":"999999"}'
hit POST "/teaching/submissions/$SID/reviews/publish" "$TK_TEACHER" '{"confirm_learner_id":"774001"}'
hit POST "/teaching/submissions/$SID/reviews/publish" "$TK_TEACHER" '{"confirm_learner_id":"774001"}'
hit GET /teaching/learners/774001/history "$TK_GUARDIAN"
hit GET /teaching/learners/774001/history "$TK_ORGB"
hit POST "/teaching/submissions/$SID/withdraw" "$TK_GUARDIAN"
