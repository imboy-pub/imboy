#!/usr/bin/env bash
# 官方频道每日话题推送 / Official channel daily digest push
#
# 向官方频道 incoming webhook 推送一条当日话题，充实频道内容（冷启动）。
# 话题来源三选一（优先级从高到低）：
#   1. TOPIC 环境变量（运营手写，最可控）
#   2. LLM 生成（RPC 调 provider，需本地 qianfan 就绪）
#   3. 内置话题池随机兜底（零依赖，保证永远有内容）
#
# 消息作者是 webhook 绑定的 system_bot（account_type=2，客户端显示「官方」徽章，
# payload is_bot=true 防钓鱼）。
#
# 一次性 setup（建频道 + webhook，取 token）见文末注释。
#
# 前置: 后端已启动；已建官方频道 + webhook 并拿到 token
#
# 用法 / Usage:
#   WEBHOOK_TOKEN=xxx bash scripts/channel_daily_digest.sh              # LLM/兜底自动出话题
#   WEBHOOK_TOKEN=xxx TOPIC='今日话题：...' bash scripts/channel_daily_digest.sh
#
# crontab 样例（每天 9:00 推送）:
#   0 9 * * * cd /path/to/imboy && WEBHOOK_TOKEN=xxx bash scripts/channel_daily_digest.sh >> /var/log/imboy_digest.log 2>&1
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

HTTP_BASE="${HTTP_BASE:-http://127.0.0.1:9800}"
NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}"
COOKIE="${IMBOY_CTL_COOKIE:-imboy}"
PROVIDER="${LLM_PROVIDER:-qianfan}"
WEBHOOK_TOKEN="${WEBHOOK_TOKEN:-}"
TOPIC="${TOPIC:-}"

die() { printf 'x %s\n' "$1" >&2; exit 1; }

[ -n "$WEBHOOK_TOKEN" ] || die "缺 WEBHOOK_TOKEN（见文末 setup 注释建 webhook 取 token）"
command -v jq >/dev/null 2>&1 || die "需要 jq"

# 内置话题池（LLM 不可用时兜底，保证频道永远有内容）
POOL=(
  "今日话题｜你最近在追什么剧/看什么书？来评论区安利一部～"
  "今日话题｜工作日快过半，分享一件今天让你开心的小事吧🌤"
  "今日话题｜如果给三个月前的自己一句话，你会说什么？"
  "今日话题｜你手机里最舍不得删的一个 App 是什么，为什么？"
  "今日话题｜周末想怎么过？躺平党和特种兵都来报到🙋"
  "今日小贴士｜imboy 的加密聊天全程端到端，连服务器都读不到内容，放心说悄悄话。"
  "今日话题｜最近有没有被哪首歌单曲循环？评论区交换歌单🎧"
)

gen_by_llm() {
  # RPC 调 provider 生成一条话题；失败返回空串（由调用方兜底）。
  # Prompt 经 base64 传输，绕开中文/引号在 bash+erl 双层转义
  local pb
  pb="$(printf '%s' '请生成一条适合 IM 社区官方频道的当日互动话题，40 字以内，轻松有趣能引发评论，直接输出话题文本不要解释。' | base64 | tr -d '\n')"
  erl -noshell -name "digest_$$@127.0.0.1" -setcookie "$COOKIE" -eval "
    N = '${NODE}',
    Msgs = [#{<<\"role\">> => <<\"user\">>, <<\"content\">> => base64:decode(<<\"${pb}\">>)}],
    case rpc:call(N, imboy_llm_registry, lookup, [<<\"${PROVIDER}\">>]) of
      {ok, #{module := Mod, opts := Opts}} ->
        case rpc:call(N, Mod, chat, [0, Msgs, Opts]) of
          {ok, R} when is_map(R) ->
            io:format(\"~ts\", [maps:get(<<\"result\">>, R, <<>>)]);
          _ -> ok
        end;
      _ -> ok
    end,
    halt()." 2>/dev/null || true
}

# 决定话题文本
if [ -n "$TOPIC" ]; then
  TEXT="$TOPIC"
  SRC="manual"
else
  TEXT="$(gen_by_llm | tr -d '\r' | sed '/^$/d' | head -c 500)"
  if [ -n "$TEXT" ]; then
    SRC="llm"
  else
    # $RANDOM 兜底选池（一次性脚本，无需密码学随机）
    TEXT="${POOL[$((RANDOM % ${#POOL[@]}))]}"
    SRC="pool"
  fi
fi

[ -n "$TEXT" ] || die "话题为空，中止"

# 推送到 webhook（jq -n 安全构造 JSON，避免引号/换行注入）
BODY="$(jq -n --arg t "$TEXT" '{text: $t}')"
HTTP_CODE="$(curl -s -o /tmp/digest_resp.$$ -w '%{http_code}' \
  -X POST "${HTTP_BASE}/api/v1/webhook/channel/${WEBHOOK_TOKEN}" \
  -H 'Content-Type: application/json' \
  --data "$BODY")"
RESP="$(cat /tmp/digest_resp.$$ 2>/dev/null || true)"; rm -f /tmp/digest_resp.$$

if [ "$HTTP_CODE" = "200" ]; then
  printf 'OK [%s] 已推送: %s\n' "$SRC" "$TEXT"
else
  die "推送失败 HTTP ${HTTP_CODE}: ${RESP}"
fi

# ============================================================================
# 一次性 setup：建官方频道 + webhook，取 token（首次运行 digest 前执行）
# ----------------------------------------------------------------------------
# 1) 建频道（RPC，OwnerUid 用运营账号 uid，Name/其余按需）：
#    escript scripts/imboy_ctl 无 channel 子命令，用 erl RPC：
#    erl -noshell -name s@127.0.0.1 -setcookie imboy -eval \
#      "rpc:call('imboy@127.0.0.1', channel_logic, create_channel,
#         [OwnerUid, <<\"imboy 官方\"/utf8>>, 0, #{}, 1]), halt()."
# 2) 建 webhook（须频道管理员 role>=2），返回完整 token（仅此一次）：
#    rpc:call('imboy@127.0.0.1', channel_webhook_logic, create,
#      [OwnerUid, ChannelIdBin, <<\"daily_digest\"/utf8>>])
#    → {ok, #{<<"token">> := Token, ...}}
# 3) 把 Token 存好，作为本脚本 WEBHOOK_TOKEN 环境变量。
# ============================================================================
