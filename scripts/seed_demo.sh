#!/usr/bin/env bash
# 演示数据填充 / Demo data seeding
#
# 通过 imboy_ctl（RPC 连运行中的节点）创建合成演示用户并互发消息。
# 注意：user_repo:save/1 强制生成 TSID，uid 由服务端分配，脚本解析返回值。
# Creates synthetic demo users via imboy_ctl. UIDs are server-assigned TSIDs
# (user_repo:save/1 overrides any provided id); parsed from create output.
# NO production data involved. 幂等：按 account 经 PG 容器查重。
#
# 前置 / Prerequisites:
#   后端已启动 / Backend running:  IMBOYENV=local make run
#   PG 容器在跑 / PG container up: docker compose --profile dev up -d imboy_pg18
#   （非默认节点时 / non-default node: export IMBOY_CTL_NODE=... IMBOY_CTL_COOKIE=...）
#
# 用法 / Usage:
#   bash scripts/seed_demo.sh
#   DEMO_PASSWORD=xxx bash scripts/seed_demo.sh   # 自定义演示密码
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

CTL="escript scripts/imboy_ctl"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
PG_USER="${IMBOY_PG_USERNAME:-imboy_user}"
PG_DB="${IMBOY_PG_DATABASE:-imboy_v1}"
DEMO_ACCOUNTS=(demo_alice demo_bob demo_carol)
DEMO_NAMES=("演示用户Alice" "演示用户Bob" "演示用户Carol")
DEMO_PASSWORD="${DEMO_PASSWORD:-$(openssl rand -hex 8)}"

step() { printf '\n==> %s\n' "$1"; }
die()  { printf '✗ %s\n' "$1" >&2; exit 1; }

# 按 account 查已有演示用户 uid（幂等核心）/ Look up existing demo uid by account
uid_by_account() {
  docker exec "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -tA \
    -c "SELECT id FROM public.\"user\" WHERE account = '$1' AND status >= 0 LIMIT 1" 2>/dev/null \
    | tr -d '[:space:]'
}

# ============ 0. 前置检查 / Preflight ============
step "检查后端节点与 PG 容器 / Checking node & PG container"
$CTL node status >/dev/null 2>&1 \
  || die "无法连接后端节点。请先启动：IMBOYENV=local make run（非默认节点需 IMBOY_CTL_NODE/IMBOY_CTL_COOKIE）"
docker exec "$PG_CONTAINER" pg_isready -U "$PG_USER" >/dev/null 2>&1 \
  || die "PG 容器 ${PG_CONTAINER} 不可用"
echo "✓ 节点在线，PG 就绪"

# ============ 1. 创建演示用户 / Create demo users ============
step "创建 ${#DEMO_ACCOUNTS[@]} 个演示用户（密码: ${DEMO_PASSWORD}）"
UIDS=()
for i in "${!DEMO_ACCOUNTS[@]}"; do
  acct="${DEMO_ACCOUNTS[$i]}"
  name="${DEMO_NAMES[$i]}"
  uid="$(uid_by_account "$acct")"
  if [ -n "$uid" ]; then
    echo "  • ${acct} 已存在 uid=${uid}，跳过 / exists, skipped"
  else
    OUT="$($CTL user create 0 -a "$acct" -n "$name" -p "$DEMO_PASSWORD")" \
      || die "创建 ${acct} 失败: ${OUT}"
    uid="$(printf '%s\n' "$OUT" | sed -n 's/^UID=//p' | tr -d '[:space:]')"
    [ -n "$uid" ] || die "未能从输出解析 UID: ${OUT}"
    echo "  ✓ ${acct} (${name}) → uid=${uid}"
  fi
  UIDS+=("$uid")
done

# ============ 2. 建立好友关系 / Establish friendships ============
# C2C 消息要求双方为好友（否则 not_a_friend 拒发）；user_friend.id 无默认值，
# 用微秒时间戳生成；唯一键 (from_user_id,to_user_id) 保证幂等。
step "建立好友关系（全互为好友，双向）/ Making everyone mutual friends"
PAIRS=""
for a in "${UIDS[@]}"; do
  for b in "${UIDS[@]}"; do
    [ "$a" = "$b" ] && continue
    PAIRS="${PAIRS}INSERT INTO public.user_friend (id, from_user_id, to_user_id, status)
VALUES ((EXTRACT(EPOCH FROM clock_timestamp())*1000000)::bigint, ${a}, ${b}, 1)
ON CONFLICT (from_user_id, to_user_id) DO NOTHING;
"
  done
done
docker exec -i "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -q <<<"$PAIRS" \
  || die "好友关系写入失败"
echo "✓ 3 人两两互为好友（6 条关系，幂等）"

# 冲洗好友关系缓存（friend_ds 各查询均经 imboy_cache:memo 缓存 300s；
# 若此前有失败的发送尝试，false 结果会残留导致 not_a_friend）
# Invalidate friendship caches via friend_ds:invalidate_cache/2 (clears
# is_friend2 + check_relationship3 both directions, 300s TTL otherwise)
NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}"
COOKIE="${IMBOY_CTL_COOKIE:-imboy}"
FLUSH_CALLS=""
for a in "${UIDS[@]}"; do
  for b in "${UIDS[@]}"; do
    [ "$a" = "$b" ] && continue
    FLUSH_CALLS="${FLUSH_CALLS}rpc:call(N, friend_ds, invalidate_cache, [${a}, ${b}]), "
  done
done
erl -noshell -name "seed_$$@127.0.0.1" -setcookie "$COOKIE" -eval \
  "N = '${NODE}', ${FLUSH_CALLS} halt()." 2>/dev/null \
  && echo "✓ 好友关系缓存已冲洗" \
  || echo "△ 缓存冲洗失败（不致命：缓存最长 300s 后自然过期）"

# ============ 3. 互发演示消息 / Seed demo messages ============
step "发送演示消息 / Sending demo messages"
$CTL msg send "${UIDS[0]}" "${UIDS[1]}" -T "你好 Bob，这是一条演示消息 / Hi Bob, demo message" \
  && echo "  ✓ alice → bob"
$CTL msg send "${UIDS[1]}" "${UIDS[0]}" -T "收到 Alice！/ Got it, Alice!" \
  && echo "  ✓ bob → alice"
$CTL msg send "${UIDS[2]}" "${UIDS[0]}" -T "Carol 也来打个招呼 / Greetings from Carol" \
  && echo "  ✓ carol → alice"

# ============ 完成 / Done ============
step "演示数据就绪 / Demo data ready"
cat <<DONE
  账号 / Accounts : ${DEMO_ACCOUNTS[0]}(${UIDS[0]}) ${DEMO_ACCOUNTS[1]}(${UIDS[1]}) ${DEMO_ACCOUNTS[2]}(${UIDS[2]})
  密码 / Password : ${DEMO_PASSWORD}
  取 JWT token    : escript scripts/imboy_ctl user token ${UIDS[0]}
DONE
