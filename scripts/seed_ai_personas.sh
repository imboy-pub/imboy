#!/usr/bin/env bash
# AI 透明助手批量上架 / Batch-seed transparent AI companion personas
#
# 读取 scripts/seed_ai_personas.json，对每个人设经 RPC 调 ai_agent_ds:create/1
# （建 user + 标 account_type=1 + 绑 ai_agent 元数据三步编排）。上架后即进
# /api/v1/agent/list 助手广场（visibility=1）。中文字段经 base64 传输，
# 节点端 base64:decode 还原 UTF-8 binary，彻底绕开 shell/erl 转义。
#
# 幂等：按 account 经 PG 查重，已存在则跳过。NO production data.
#
# 前置 / Prerequisites:
#   后端已启动:  IMBOYENV=local make run
#   PG 容器在跑: docker compose --profile dev up -d imboy_pg18
#   本地 LLM provider: config 内置 qianfan（无需额外配置）
#   （非默认节点: export IMBOY_CTL_NODE=... IMBOY_CTL_COOKIE=...）
#
# 用法 / Usage:
#   bash scripts/seed_ai_personas.sh
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

CTL="escript scripts/imboy_ctl"
JSON="scripts/seed_ai_personas.json"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
PG_USER="${IMBOY_PG_USERNAME:-imboy_user}"
PG_DB="${IMBOY_PG_DATABASE:-imboy_v1}"
NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}"
COOKIE="${IMBOY_CTL_COOKIE:-imboy}"

step() { printf '\n==> %s\n' "$1"; }
die()  { printf 'x %s\n' "$1" >&2; exit 1; }
b64()  { printf '%s' "$1" | base64 | tr -d '\n'; }

# ============ 0. 前置检查 / Preflight ============
step "检查后端节点、PG 容器与人设包 / Checking node, PG, personas"
[ -f "$JSON" ] || die "人设包不存在: $JSON"
command -v jq >/dev/null 2>&1 || die "需要 jq（brew install jq / apt install jq）"
$CTL node status >/dev/null 2>&1 \
  || die "无法连接后端节点。请先启动：IMBOYENV=local make run"
docker exec "$PG_CONTAINER" pg_isready -U "$PG_USER" >/dev/null 2>&1 \
  || die "PG 容器 ${PG_CONTAINER} 不可用"
echo "OK 节点在线，PG 就绪，人设包有效（$(jq '.personas | length' "$JSON") 个）"

# account 已存在（任意 account_type）则跳过，避免 account 唯一约束冲突
account_exists() {
  local acct="$1"
  docker exec "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -tA \
    -c "SELECT 1 FROM public.\"user\" WHERE account = '$acct' LIMIT 1" 2>/dev/null \
    | tr -d '[:space:]'
}

# ============ 1. 逐个上架（幂等）/ Seed each persona ============
step "上架人设（已存在按 account 跳过）"
PROVIDER_DEFAULT="$(jq -r '.provider_default // "qianfan"' "$JSON")"
CALLS=""
CREATED=0
SKIPPED=0
COUNT="$(jq '.personas | length' "$JSON")"

for i in $(seq 0 $((COUNT - 1))); do
  acct="$(jq -r ".personas[$i].account" "$JSON")"
  nick="$(jq -r ".personas[$i].nickname" "$JSON")"
  desc="$(jq -r ".personas[$i].description" "$JSON")"
  prompt="$(jq -r ".personas[$i].system_prompt" "$JSON")"
  prov="$(jq -r ".personas[$i].provider // \"$PROVIDER_DEFAULT\"" "$JSON")"

  if [ -n "$(account_exists "$acct")" ]; then
    echo "  - ${acct}（${nick}）已存在，跳过"
    SKIPPED=$((SKIPPED + 1))
    continue
  fi

  # base64 传输中文，节点端 base64:decode 还原 UTF-8 binary
  nb="$(b64 "$nick")"; ab="$(b64 "$acct")"
  db="$(b64 "$desc")"; pb="$(b64 "$prompt")"; vb="$(b64 "$prov")"
  CALLS="${CALLS}
    _ = io:format(\"  create ~ts -> ~p~n\", [base64:decode(<<\"${nb}\">>),
        rpc:call(N, ai_agent_ds, create, [#{
            <<\"account\">>       => base64:decode(<<\"${ab}\">>),
            <<\"nickname\">>      => base64:decode(<<\"${nb}\">>),
            <<\"description\">>   => base64:decode(<<\"${db}\">>),
            <<\"system_prompt\">> => base64:decode(<<\"${pb}\">>),
            <<\"provider\">>      => base64:decode(<<\"${vb}\">>),
            <<\"visibility\">>    => 1,
            <<\"status\">>        => 1
        }])]),"
  CREATED=$((CREATED + 1))
done

if [ "$CREATED" -gt 0 ]; then
  erl -noshell -name "seedai_$$@127.0.0.1" -setcookie "$COOKIE" \
    -eval "N = '${NODE}', ${CALLS} halt()." 2>&1 \
    || die "RPC 上架失败（部分可能已创建，重跑幂等）"
fi

# ============ 2. 汇总输出 account -> uid（供 onboarding 配置引用）============
step "上架完成：新建 ${CREATED}，跳过 ${SKIPPED}"
echo "人设 account -> uid 映射（onboarding.welcome_agent_uid 可取 ai_welcome 的 uid）:"
ACCTS="$(jq -r '.personas[].account' "$JSON" | sed "s/.*/'&'/" | paste -sd, -)"
docker exec "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -tA -F'	' \
  -c "SELECT account, id, nickname FROM public.\"user\"
      WHERE account IN (${ACCTS}) AND account_type = 1 ORDER BY account" 2>/dev/null \
  | while IFS=$'\t' read -r a u n; do printf '  %-18s uid=%-20s %s\n' "$a" "$u" "$n"; done

cat <<'DONE'

下一步 / Next:
  1. 助手广场验证:  curl 'http://127.0.0.1:9800/api/v1/agent/list?page=1&size=20'（需 JWT）
  2. 配置新手引导:  bash scripts/onboarding_setup.sh
DONE
