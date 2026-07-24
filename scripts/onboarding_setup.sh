#!/usr/bin/env bash
# 新手引导配置初始化 / Initialize new-user onboarding config
#
# 经 RPC 调 user_onboarding_logic:put_config/1 写 config 表 5 个键，开启
# 注册后「默认好友 + 默认订阅 + 欢迎消息」三件套。welcome_agent_uid 默认
# 取 ai_welcome 人设的 uid（须先跑 seed_ai_personas.sh）。
#
# 幂等：put_config 半量覆盖写，可重复执行。
#
# 前置: 后端已启动 + 已跑 seed_ai_personas.sh
#
# 用法 / Usage:
#   bash scripts/onboarding_setup.sh
#   WELCOME_LLM=true bash scripts/onboarding_setup.sh          # 开 LLM 个性化欢迎
#   DEFAULT_CHANNELS='"ch_id_1","ch_id_2"' bash scripts/onboarding_setup.sh
#   WELCOME_AGENT_UID=123 bash scripts/onboarding_setup.sh     # 手动指定欢迎 agent
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

CTL="escript scripts/imboy_ctl"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
PG_USER="${IMBOY_PG_USERNAME:-imboy_user}"
PG_DB="${IMBOY_PG_DATABASE:-imboy_v1}"
NODE="${IMBOY_CTL_NODE:-imboy@127.0.0.1}"
COOKIE="${IMBOY_CTL_COOKIE:-imboy}"

WELCOME_LLM="${WELCOME_LLM:-false}"
DEFAULT_CHANNELS="${DEFAULT_CHANNELS:-}"          # 逗号分隔的带引号频道 id，默认空
WELCOME_AGENT_UID="${WELCOME_AGENT_UID:-}"

# 默认欢迎文案（管理后台可后续改，此为初始化值）。{{nickname}} 由后端渲染
WELCOME_TEMPLATE="${WELCOME_TEMPLATE:-嗨 {{nickname}}，欢迎来到 imboy🎉 我是官方 AI 小助手，有任何问题都可以找我。温馨提示：加密聊天里永远只有真人，AI 不会进入端到端加密会话～}"

step() { printf '\n==> %s\n' "$1"; }
die()  { printf 'x %s\n' "$1" >&2; exit 1; }
b64()  { printf '%s' "$1" | base64 | tr -d '\n'; }

step "前置检查 / Preflight"
$CTL node status >/dev/null 2>&1 || die "无法连接后端节点，请先 IMBOYENV=local make run"

# 未显式指定则取 ai_welcome 人设的 uid
if [ -z "$WELCOME_AGENT_UID" ]; then
  WELCOME_AGENT_UID="$(docker exec "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -tA \
    -c "SELECT id FROM public.\"user\" WHERE account = 'ai_welcome' AND account_type = 1 LIMIT 1" \
    2>/dev/null | tr -d '[:space:]')"
  [ -n "$WELCOME_AGENT_UID" ] \
    || die "未找到 ai_welcome 人设，请先跑 seed_ai_personas.sh 或用 WELCOME_AGENT_UID= 指定"
fi
echo "OK welcome_agent_uid=${WELCOME_AGENT_UID}  llm=${WELCOME_LLM}  channels=[${DEFAULT_CHANNELS}]"

step "写入 onboarding 配置 / Writing config"
TB="$(b64 "$WELCOME_TEMPLATE")"
# put_config 半量校验写入；default_channels 为 binary 列表
erl -noshell -name "onb_$$@127.0.0.1" -setcookie "$COOKIE" -eval "
  N = '${NODE}',
  Patch = #{
    <<\"enabled\">>             => true,
    <<\"welcome_agent_uid\">>   => ${WELCOME_AGENT_UID},
    <<\"default_channels\">>    => [${DEFAULT_CHANNELS}],
    <<\"welcome_template\">>    => base64:decode(<<\"${TB}\">>),
    <<\"welcome_llm_enabled\">> => ${WELCOME_LLM}
  },
  R = rpc:call(N, user_onboarding_logic, put_config, [Patch]),
  io:format(\"put_config -> ~p~n\", [R]),
  case R of {ok, _} -> ok; _ -> halt(1) end,
  halt()." 2>&1 || die "put_config 失败（检查参数类型：channels 须是带引号字符串）"

step "读回验证 / Verify"
erl -noshell -name "onbv_$$@127.0.0.1" -setcookie "$COOKIE" -eval "
  N = '${NODE}',
  io:format(\"~tp~n\", [rpc:call(N, user_onboarding_logic, get_config, [])]),
  halt()." 2>&1

cat <<'DONE'

OK onboarding 已开启。新注册用户将自动：
  - 添加欢迎 agent 为好友
  - 订阅默认频道（若已配 default_channels）
  - 收到欢迎消息（模板；WELCOME_LLM=true 则 LLM 个性化）

验证: 注册一个新用户，30s 内应收到欢迎私聊。
关闭: 管理后台 POST /api/adm/ai_agent/onboarding_config {"enabled": false}
DONE
