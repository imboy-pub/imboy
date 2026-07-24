#!/usr/bin/env bash
# 新手引导三件套集成验收 / New-user onboarding integration acceptance（AI 冷启动 M4）
#
# 对活栈做端到端断言：新用户注册后 user_onboarding_logic:after_signup/2 触发的
# 「默认好友 + 欢迎消息」是否真实落库，并验证幂等 + account_type 标记。
#
# 设计：HERMETIC 自provision——脚本自建临时欢迎 agent、快照/覆盖 onboarding 配置、
# 建测试新用户触发 after_signup，跑完 teardown 恢复原配置并删除所有测试行，零残留，
# 可在真实 dev 节点反复安全执行，无需 admin token / seed 前置。
#
# 触发路径：do_signup_by_email/3 未导出、do_signup/5 需真实验证码（活栈无验证码
# 通道），故 RPC user_ds:insert_and_get_id 建测试用户后直接 RPC after_signup/2 ——
# 即 passport 注册成功分支调用的**同一函数**。异步 hook 与验证码校验由 EUnit
# （signup_hook_不阻断注册_test 等）覆盖，本脚本专注断言三件套真实 DB 落库。
#
# ② 默认订阅未在此断言：channel_logic:subscribe 校验频道存在，hermetic 模式不建
# 测试频道；订阅编排由 EUnit after_signup_正常路径_test（meck channel_logic）覆盖。
#
# 前置：后端已启动（IMBOYENV=local make run）
#
# 用法 / Usage:
#   bash scripts/verify_onboarding.sh
#   IMBOY_CTL_NODE=imboy@127.0.0.1 IMBOY_CTL_COOKIE=imboy bash scripts/verify_onboarding.sh
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
PG_USER="${IMBOY_PG_USERNAME:-imboy_user}"
PG_DB="${IMBOY_PG_DATABASE:-imboy_v1}"
NODE="${IMBOY_CTL_NODE:-imboy_dev@127.0.0.1}"       # vm.args 真值默认
COOKIE="${IMBOY_CTL_COOKIE:-imboycookie}"
WELCOME_TIMEOUT="${WELCOME_TIMEOUT:-30}"            # 欢迎消息 send_next 异步落库轮询上限秒
DOCKER_BIN="${DOCKER_BIN:-$( command -v docker || echo /Applications/Docker.app/Contents/Resources/bin/docker )}"

PASS=0
TOTAL=0
step() { printf '\n==> %s\n' "$1"; }
die()  { printf 'x %s\n' "$1" >&2; exit 1; }
check() {
  TOTAL=$((TOTAL + 1))
  if [ "$2" = "$3" ]; then
    PASS=$((PASS + 1)); printf '  PASS %s (=%s)\n' "$1" "$3"
  else
    printf '  FAIL %s (期望=%s 实际=%s)\n' "$1" "$2" "$3" >&2
  fi
}

# RPC 到运行节点（复用 onboarding_setup.sh 的 erl -eval 范式），回显 io:format 输出
rpc() {
  erl -noshell -name "verifyonb_$$@127.0.0.1" -setcookie "$COOKIE" -eval "
    N = '${NODE}',
    $1,
    halt()." 2>&1 | tail -1
}
# psql 单值查询
q() {
  "$DOCKER_BIN" exec "$PG_CONTAINER" psql -U "$PG_USER" -d "$PG_DB" -tA -c "$1" 2>/dev/null | tr -d '[:space:]'
}

step "前置检查 / Preflight"
[ "$(rpc "case net_adm:ping(N) of pong -> io:format(\"UP\"); _ -> io:format(\"DOWN\") end")" = "UP" ] \
  || die "无法连接节点 ${NODE}（cookie ${COOKIE}），请先 IMBOYENV=local make run"
echo "  node ${NODE} UP"

# 快照原 onboarding 配置（teardown 恢复），base64 传 template 避免转义
step "快照原配置 / Snapshot config"
ORIG_ENABLED="$(rpc "M=rpc:call(N,user_onboarding_logic,get_config,[]), io:format(\"~p\",[maps:get(<<\"enabled\">>,M,false)])")"
ORIG_UID="$(rpc "M=rpc:call(N,user_onboarding_logic,get_config,[]), io:format(\"~p\",[maps:get(<<\"welcome_agent_uid\">>,M,0)])")"
ORIG_LLM="$(rpc "M=rpc:call(N,user_onboarding_logic,get_config,[]), io:format(\"~p\",[maps:get(<<\"welcome_llm_enabled\">>,M,false)])")"
ORIG_TPL_B64="$(rpc "M=rpc:call(N,user_onboarding_logic,get_config,[]), T=maps:get(<<\"welcome_template\">>,M,<<>>), io:format(\"~s\",[base64:encode(T)])")"
echo "  orig enabled=${ORIG_ENABLED} uid=${ORIG_UID} llm=${ORIG_LLM}"

step "provision 临时欢迎 agent + 测试新用户"
# 经 passport_logic:pick_data_for_insert/2 补齐 reg_ip/reg_cosv/status 等 NOT NULL 列（与真实 signup 一致）
AGENT_UID="$(rpc "
  B=#{<<\"nickname\">>=><<\"验收助手\"/utf8>>, <<\"password\">>=>rpc:call(N,elib_password,generate,[<<\"Test1234\">>])},
  D=rpc:call(N,passport_logic,pick_data_for_insert,[B,#{}]),
  case rpc:call(N,user_ds,insert_and_get_id,[D]) of {ok,U}->io:format(\"~p\",[U]); O->io:format(\"ERR:~p\",[O]) end")"
case "$AGENT_UID" in ERR:*|*badrpc*|"") die "建欢迎 agent 失败: $AGENT_UID";; esac
q "UPDATE public.\"user\" SET account_type=1 WHERE id=${AGENT_UID};" >/dev/null
echo "  agent_uid=${AGENT_UID}"

EMAIL="onbv_$$_$(od -An -N3 -tu1 /dev/urandom | tr -d ' ')@example.test"
NICK="验收新人$$"
NEWUID="$(rpc "
  B=#{<<\"email\">>=><<\"${EMAIL}\">>, <<\"nickname\">>=><<\"${NICK}\"/utf8>>,
      <<\"password\">>=>rpc:call(N,elib_password,generate,[<<\"Test1234\">>])},
  D=rpc:call(N,passport_logic,pick_data_for_insert,[B,#{<<\"nickname\">>=><<\"${NICK}\"/utf8>>}]),
  case rpc:call(N,user_ds,insert_and_get_id,[D]) of {ok,U}->io:format(\"~p\",[U]); O->io:format(\"ERR:~p\",[O]) end")"
case "$NEWUID" in ERR:*|*badrpc*|"") die "建测试用户失败: $NEWUID";; esac
echo "  new_uid=${NEWUID}"

# teardown：恢复原配置 + 删所有测试行（trap 保证异常也执行）
cleanup() {
  # put_config 拒空 welcome_template（校验须非空）；原模板为空时省略该键，否则整体拒→配置不恢复
  rpc "T=base64:decode(<<\"${ORIG_TPL_B64}\">>),
    Base=#{<<\"enabled\">>=>${ORIG_ENABLED}, <<\"welcome_agent_uid\">>=>${ORIG_UID}, <<\"welcome_llm_enabled\">>=>${ORIG_LLM}},
    Patch=case T of <<>> -> Base; _ -> Base#{<<\"welcome_template\">>=>T} end,
    rpc:call(N,user_onboarding_logic,put_config,[Patch])" >/dev/null 2>&1 || true
  q "DELETE FROM public.user_friend WHERE from_user_id IN (${NEWUID},${AGENT_UID}) OR to_user_id IN (${NEWUID},${AGENT_UID});" >/dev/null || true
  q "DELETE FROM public.msg_c2c WHERE to_id IN (${NEWUID},${AGENT_UID}) OR from_id IN (${NEWUID},${AGENT_UID});" >/dev/null || true
  q "DELETE FROM public.\"user\" WHERE id IN (${NEWUID},${AGENT_UID});" >/dev/null || true
}
trap cleanup EXIT

step "覆盖 onboarding 配置（enabled + welcome_agent_uid）"
PUT="$(rpc "io:format(\"~p\",[rpc:call(N,user_onboarding_logic,put_config,[#{
  <<\"enabled\">>=>true, <<\"welcome_agent_uid\">>=>${AGENT_UID},
  <<\"welcome_llm_enabled\">>=>false,
  <<\"welcome_template\">>=><<\"嗨 {{nickname}}，欢迎（验收）\"/utf8>>}])])")"
echo "  put_config -> ${PUT}"

step "触发 after_signup"
rpc "io:format(\"~p\",[rpc:call(N,user_onboarding_logic,after_signup,[${NEWUID}, <<\"${NICK}\"/utf8>>])])" >/dev/null
echo "  after_signup done"

step "断言 account_type"
check "welcome_agent.account_type=1" "1" "$(q "SELECT account_type FROM public.\"user\" WHERE id=${AGENT_UID}")"

step "断言① 默认好友（双边 + source 标记）"
check "user_friend 双边行=2" "2" \
  "$(q "SELECT count(*) FROM public.user_friend WHERE (from_user_id=${NEWUID} AND to_user_id=${AGENT_UID}) OR (from_user_id=${AGENT_UID} AND to_user_id=${NEWUID})")"
check "source=system_onboarding" "1" \
  "$(q "SELECT count(*) FROM public.user_friend WHERE from_user_id=${NEWUID} AND to_user_id=${AGENT_UID} AND setting::text LIKE '%system_onboarding%'")"

step "断言③ 欢迎消息（轮询 msg_c2c，上限 ${WELCOME_TIMEOUT}s）"
WMSG=0
for _ in $(seq 1 "$WELCOME_TIMEOUT"); do
  WMSG="$(q "SELECT count(*) FROM public.msg_c2c WHERE from_id=${AGENT_UID} AND to_id=${NEWUID} AND msg_type='text'")"
  [ "${WMSG:-0}" = "0" ] || break
  sleep 1
done
check "欢迎消息落库≥1" "1" "$( [ "${WMSG:-0}" -ge 1 ] 2>/dev/null && echo 1 || echo 0 )"

step "断言④ 幂等（重复触发不新增好友）"
rpc "io:format(\"~p\",[rpc:call(N,user_onboarding_logic,after_signup,[${NEWUID}, <<\"${NICK}\"/utf8>>])])" >/dev/null
check "重复触发后好友行仍=2" "2" \
  "$(q "SELECT count(*) FROM public.user_friend WHERE (from_user_id=${NEWUID} AND to_user_id=${AGENT_UID}) OR (from_user_id=${AGENT_UID} AND to_user_id=${NEWUID})")"

step "结果 / Result"
printf 'PASS %d/%d\n' "$PASS" "$TOTAL"
[ "$PASS" -eq "$TOTAL" ] || die "存在失败断言"
echo "OK 新手引导三件套集成验收通过"
