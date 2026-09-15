#!/usr/bin/env bash
# ============================================================
# 墨芽 AI 回课：启用前置条件检查 / Moya AI review readiness
# ------------------------------------------------------------
# 为什么需要它：
#   AI 回课有 **三处使能条件只存在于模板、真实环境里没有**，且都不会编译失败、
#   不会抛异常，只表现为「provider_unavailable」或「worker 从未被调度」，
#   排查成本极高（同一个模式在本仓已发生三次）：
#     ③ teaching_ai_llm_provider 未配置，或名字与该环境 llm_providers 条目不匹配
#     ② teaching_ai_attach_video_url 未开 → worker 内容盲（模型看不到视频）
#     ① 该环境配置里没有 ecron 的 teaching_ai_worker 作业 → worker 从不被调度
#   （①的系统性成因见 scripts/check_cron_config.sh 头部说明。）
#
# 本脚本对每个存在的配置文件回答两件事：
#   A. **指定/候选 provider 能不能真的工作**（硬门，四种失败都可就地证明
#      「就算打开也一定不工作」）
#   B. **当前配置是否自洽**（软告警：视频开关配对、ecron worker、env 形态的 key）
#
# 只读：只打印条目名与模块名；**不读取、不打印任何 api_key 值**。
#
# 用法 / Usage：
#   bash scripts/check_moya_ai_config.sh              # 扫全部存在的配置
#   bash scripts/check_moya_ai_config.sh <config>     # 只查指定文件
#   bash scripts/check_moya_ai_config.sh --self-test
# 退出码：0=无硬失败；1=存在硬失败
# ============================================================
set -uo pipefail

TAI_ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

TAI_PASS=0
TAI_FAIL=0

tai_ok() {
  TAI_PASS=$((TAI_PASS + 1))
  echo "  ✓ $1"
}

tai_bad() {
  TAI_FAIL=$((TAI_FAIL + 1))
  echo "  ✗ $1"
}

tai_warn() {
  echo "  ⚠ $1"
}

# ------------------------------------------------------------
# llm_providers 逐条目切分：每条一行（整块文本）。
# 先用 `tr '#' '\n'` 在 `#{name =>` 处断行，再取以 `{name =>` 开头的行；
# 整行注释先剔除（配置里大量注释会提到这些键名）。
# ------------------------------------------------------------
tai_entries() {
  sed -n '/{llm_providers,/,/^        \]}/p' "$1" 2>/dev/null \
    | grep -vE '^[[:space:]]*%' \
    | tr '\n' ' ' \
    | tr '#' '\n' \
    | grep -E '^\{[[:space:]]*name =>'
}

tai_entry_name() {
  printf '%s' "$1" | sed -nE 's/^\{[[:space:]]*name =>[[:space:]]*<<"([^"]*)">>.*/\1/p'
}

tai_entry_module() {
  printf '%s' "$1" | sed -nE 's/.*module[[:space:]]*=>[[:space:]]*([a-z0-9_]+).*/\1/p'
}

tai_entry_has_vision() {
  printf '%s' "$1" | grep -qE 'vision[[:space:]]*=>[[:space:]]*true'
}

# key 只判形态，不取值：inline 非空 / inline 空 / 由环境变量提供 / 缺失
tai_entry_key_form() {
  local blk="$1"
  if printf '%s' "$blk" | grep -qE 'api_key[[:space:]]*=>[[:space:]]*\{env,'; then
    echo "env"
  elif printf '%s' "$blk" | grep -qE 'api_key[[:space:]]*=>[[:space:]]*<<"">>'; then
    echo "empty"
  elif printf '%s' "$blk" | grep -qE 'api_key[[:space:]]*=>[[:space:]]*<<"[^"]'; then
    echo "inline"
  else
    echo "missing"
  fi
}

# ------------------------------------------------------------
# 单次请求超时 × 重试次数  vs  客户端轮询预算（跨仓配对检查）
#
# 背景（2026-09-15 事故）：服务端最坏路径与前端预算**恰好相等**
#（30s × 2 次尝试 = 60s vs 3s × 20 次 = 60s）→ 两边同时到点，轮询永远看
# 不见终态。它不编译失败、不抛异常、不落任何 error_code —— 老师看到的就是
# 一屏不变的「AI 正在看这份作业」，而这正是本脚本存在的理由。
#
# 难点是这两个常量**分处两个仓库**（imboy 的 config + moya_ai_worker，
# 与 moya 的 workbench.ts），没有任何编译期或运行期依赖能把它们绑住，
# 唯一能把它们放在一起比较的地方就是这个脚本。改任一侧请务必跑一次。
# ------------------------------------------------------------

# 取 provider 条目的 timeout（毫秒）；未配返回空
tai_entry_timeout() {
  printf '%s' "${1:-}" | sed -nE 's/.*timeout[[:space:]]*=>[[:space:]]*([0-9]+).*/\1/p'
}

# 最大尝试次数：moya_ai_worker 的 ?DEFAULT_MAX_RETRIES；读不到时按 2（与代码一致）
tai_default_max_retries() {
  local f="${1}/src/logic/moya_ai_worker.erl"
  local n=""
  if [ -f "$f" ]; then
    n="$(sed -nE 's/^-define\(DEFAULT_MAX_RETRIES,[[:space:]]*([0-9]+)\).*/\1/p' "$f" | head -1)"
  fi
  echo "${n:-2}"
}

# 客户端轮询预算："<间隔毫秒> <次数>"；moya 不在约定位置时返回空（跳过）
tai_client_poll_budget() {
  local f="${1}/../moya/src/packages/teacher/workbench/workbench.ts"
  [ -f "$f" ] || return 0
  local iv att
  iv="$(sed -nE 's/^const AI_POLL_INTERVAL_MS = ([0-9]+);.*/\1/p' "$f" | head -1)"
  att="$(sed -nE 's/^const AI_POLL_MAX_ATTEMPTS = ([0-9]+);.*/\1/p' "$f" | head -1)"
  [ -n "$iv" ] && [ -n "$att" ] && echo "$iv $att"
  return 0
}

tai_check_poll_budget() {
  local root="$1" name="$2" blk="$3"
  local timeout retries budget interval attempts worst budget_ms need

  timeout="$(tai_entry_timeout "$blk")"
  retries="$(tai_default_max_retries "$root")"
  budget="$(tai_client_poll_budget "$root")"

  if [ -z "$budget" ]; then
    tai_warn "读不到 moya 侧轮询常量（workbench.ts 的 AI_POLL_*）—— 跳过预算配对检查"
    return 0
  fi
  set -- $budget
  interval="$1"
  attempts="$2"

  if [ -z "$timeout" ]; then
    # 未配 → 运行时用 imboy_llm_openai 的 ?DEFAULT_TIMEOUT_MS
    timeout=30000
    tai_warn "条目 <<\"${name}\">> 未配 timeout → 运行时取缺省 30s（纯文本够用；视频理解的长尾会偶发 error_code=timeout）"
  else
    tai_ok "条目 <<\"${name}\">> 单次请求超时 timeout = $((timeout / 1000))s"
  fi

  worst=$((timeout * retries))
  budget_ms=$((interval * attempts))

  if [ "$budget_ms" -lt "$worst" ]; then
    need=$((worst / interval + 1))
    tai_bad "轮询预算 ${budget_ms}ms < 服务端最坏 ${worst}ms（timeout $((timeout / 1000))s × ${retries} 次尝试）→ 前端必然先放弃而后端还在跑，页面就是一屏不变的「正在整理」"
    echo "        修：把 moya 侧 AI_POLL_MAX_ATTEMPTS 从 ${attempts} 提到 ≥ ${need}，或调小 timeout"
    return 1
  elif [ "$budget_ms" -lt $((worst * 3 / 2)) ]; then
    tai_warn "轮询预算 ${budget_ms}ms 仅比服务端最坏 ${worst}ms 多 $((budget_ms - worst))ms（不足 1.5 倍）→ 边界抖动仍能重现「前端先放弃」"
  else
    tai_ok "轮询预算 ${budget_ms}ms ≥ 1.5 × 服务端最坏 ${worst}ms（timeout $((timeout / 1000))s × ${retries} 次尝试）"
  fi
  return 0
}

# ------------------------------------------------------------
# 对象存储 public_endpoint 是否「模型侧可抓」
# ------------------------------------------------------------
# presign 出来的 URL 是交给**第三方多模态模型**抓的，不是给浏览器。
# 内网端点（127 / 192.168 / 10 / 172.16-31）在模型侧必然抓不到，表现为
# provider_error 或两次各烧满 timeout，**日志里没有任何线索** —— 这个坑
# 实测踩过两次，第二次还误判成「素材不合格」。故用静态判据钉死。
#
# 不在这里做 curl 实测：本机 DNS 被本地代理的 fake-ip 劫持（s3.imboy.pub
# 解析成 198.18.x.x），实测会给出与真实情况相反的结论。
tai_public_endpoint_of() {
  grep -vE '^[[:space:]]*%' "$1" |
    sed -nE 's/.*public_endpoint[[:space:]]*=>[[:space:]]*<<"([^"]+)".*/\1/p' | head -1
}

tai_is_private_host() {
  case "$1" in
  127.* | localhost | 10.* | 192.168.* | 172.1[6-9].* | 172.2[0-9].* | 172.3[01].*)
    return 0
    ;;
  *.local | *.internal)
    return 0
    ;;
  *)
    return 1
    ;;
  esac
}

tai_check_public_endpoint() {
  local f="$1" video="$2"
  local ep host

  ep="$(tai_public_endpoint_of "$f")"

  if [ -z "$ep" ]; then
    if [ "$video" = "true" ]; then
      tai_warn "未配 garage.public_endpoint → 运行时回落到 endpoint()（本地即 127.0.0.1:3900），presign 出去的是内网地址，模型抓不到"
    else
      tai_ok "未配 garage.public_endpoint（attach_video_url=false，当前不影响）"
    fi
    return 0
  fi

  host="${ep#*://}"
  host="${host%%/*}"
  host="${host%%:*}"

  # 内网先判：本地 garage 本来就没有 https，内网属**开发态**而非配置错误，
  # 给 warn + 明确动作，不硬失败（否则本地常态红，门禁就失去信号了）。
  if tai_is_private_host "$host"; then
    tai_warn "garage.public_endpoint = ${ep} 是内网地址 → 第三方模型抓不到视频，回课必然 provider_error / 超时（日志无痕）"
    echo "        修：bash scripts/dev_garage_tunnel.sh start（临时公网 https 隧道），或把本地指向公网真桶（永久）"
    return 0
  fi

  case "$ep" in
  https://*)
    tai_ok "garage.public_endpoint = ${ep}（公网 https，模型侧可抓）"
    return 0
    ;;
  http://*)
    tai_bad "garage.public_endpoint = ${ep} 是明文 http → 多数模型网关拒抓（且即便肯抓也是裸流传未成年人媒体）；必须用 https"
    return 1
    ;;
  *)
    tai_bad "garage.public_endpoint = ${ep} 不是合法 URL"
    return 1
    ;;
  esac
}

# ------------------------------------------------------------
# 单文件检查
# ------------------------------------------------------------
tai_check_file() {
  local root="$1"
  local f="$2"
  local label="${f#"${root}/"}"

  if [ ! -f "$f" ]; then
    return 0
  fi

  echo "== ${label} =="

  local name
  name="$(grep -vE '^[[:space:]]*%' "$f" | sed -nE 's/.*\{teaching_ai_llm_provider,[[:space:]]*<<"([^"]*)">>\}.*/\1/p' | head -1)"
  local explicitly_off
  explicitly_off="$(grep -vE '^[[:space:]]*%' "$f" | grep -cE '\{teaching_ai_llm_provider,[[:space:]]*undefined\}')"

  # 视频开关（只认非注释行；配置注释里会提到 true）
  local video="false"
  if grep -vE '^[[:space:]]*%' "$f" | grep -qE '\{teaching_ai_attach_video_url,[[:space:]]*true\}'; then
    video="true"
  fi

  # ecron 里有没有 teaching_ai_worker 作业
  local has_job=0
  if sed -n '/^    {ecron, \[/,/^    \]}/p' "$f" 2>/dev/null \
    | grep -vE '^[[:space:]]*%' | grep -qE '\{teaching_ai_worker,[[:space:]]*"'; then
    has_job=1
  fi

  local entries
  entries="$(tai_entries "$f")"

  if [ -z "$name" ]; then
    # 未启用态：报告「本文件若要启用，有没有可用的 vision provider」
    local candidates="" blk n
    while IFS= read -r blk; do
      [ -n "$blk" ] || continue
      n="$(tai_entry_name "$blk")"
      if [ -n "$n" ] && tai_entry_has_vision "$blk"; then
        candidates="${candidates} ${n}"
      fi
    done <<EOF
${entries:-}
EOF
    if [ "$explicitly_off" -gt 0 ]; then
      tai_ok "teaching_ai_llm_provider=undefined（未启用态，符合默认）"
    else
      tai_warn "本文件未声明 teaching_ai_llm_provider（键缺失，运行时取默认 —— 与显式 undefined 等价）"
    fi
    if [ -n "$candidates" ]; then
      tai_ok "可用于启用的 vision provider 候选：${candidates}"
    else
      tai_warn "本文件**没有任何 vision provider 候选** → 即使把 teaching_ai_llm_provider 设为某个名字也无法启用（必落 provider_unavailable）"
    fi
    [ "$video" = "true" ] && tai_warn "teaching_ai_attach_video_url=true 但 provider 未定 —— 开了也不会签 URL（无害，但属半配置状态）"
    [ "$has_job" = "1" ] || tai_warn "本文件 ecron 无 teaching_ai_worker 作业 → worker 不会被调度（见 check_cron_config.sh）"
    echo ""
    return 0
  fi

  # 已启用态：四种可证明的失败，全部硬门
  echo "  · teaching_ai_llm_provider = <<\"${name}\">>, attach_video_url = ${video}"
  [ "$video" = "true" ] && tai_check_public_endpoint "$f" "$video"

  local target="" blk
  while IFS= read -r blk; do
    [ -n "$blk" ] || continue
    if [ "$(tai_entry_name "$blk")" = "$name" ]; then
      target="$blk"
      break
    fi
  done <<EOF
${entries:-}
EOF

  if [ -z "$target" ]; then
    local known="" n
    while IFS= read -r blk; do
      [ -n "$blk" ] || continue
      n="$(tai_entry_name "$blk")"
      known="${known} ${n}"
    done <<EOF
${entries:-}
EOF
    tai_bad "provider <<\"${name}\">> 在本文件的 llm_providers 里不存在（现有：${known:- 无}）—— 运行时 lookup 失败，恒 provider_unavailable"
    echo ""
    return 1
  fi
  tai_ok "provider <<\"${name}\">> 条目存在"

  local mod
  mod="$(tai_entry_module "$target")"
  if [ -z "$mod" ]; then
    tai_bad "条目 <<\"${name}\">> 没写 module"
  elif [ ! -f "${root}/src/lib/${mod}.erl" ]; then
    tai_bad "条目模块 ${mod} 在 src/lib/ 不存在（改名或删除的连带缺口）"
  else
    tai_ok "条目模块 ${mod} 存在"
  fi

  # vision：条目级声明，或模块 capabilities/0 声明
  if tai_entry_has_vision "$target"; then
    tai_ok "条目声明 vision => true"
  elif [ -n "$mod" ] && [ -f "${root}/src/lib/${mod}.erl" ] && grep -qE 'vision[[:space:]]*=>[[:space:]]*true' "${root}/src/lib/${mod}.erl"; then
    tai_ok "模块 ${mod} 的能力声明含 vision"
  else
    tai_bad "条目 <<\"${name}\">> 不满足 vision（条目无 vision => true，模块 ${mod:-?} 的能力声明也无 vision）—— 运行时 HasVision=false，恒 provider_unavailable"
  fi

  # key 形态
  case "$(tai_entry_key_form "$target")" in
    inline)
      tai_ok "条目 api_key 为内联非空值（值不打印）"
      ;;
    env)
      tai_warn "条目 api_key 由环境变量提供 —— 需确认运行时该变量存在，否则 provider_unavailable"
      ;;
    empty)
      tai_bad "条目 api_key 为空串（占位）—— 运行时 HasKey=false，恒 provider_unavailable"
      ;;
    *)
      tai_bad "条目没有 api_key —— 运行时 HasKey=false，恒 provider_unavailable"
      ;;
  esac

  # 配对与调度
  if [ "$video" = "true" ]; then
    tai_ok "teaching_ai_attach_video_url=true（模型能看到视频）"
  else
    tai_warn "teaching_ai_attach_video_url=false → 模型只拿到 object_key，产出的点评**内容盲**（诚实失败优于假绿，但这不是可用的回课）"
  fi
  if [ "$has_job" = "1" ]; then
    tai_ok "ecron 含 teaching_ai_worker 作业"
  else
    tai_warn "ecron 无 teaching_ai_worker 作业 → 即使 provider 就绪，worker 也不会跑"
  fi

  # 超时 vs 客户端轮询预算（跨仓配对，见 tai_check_poll_budget 上方说明）
  tai_check_poll_budget "$root" "$name" "$target"

  echo ""
  return 0
}

tai_main() {
  local arg root="$TAI_ROOT" only=""
  for arg in "$@"; do
    case "$arg" in
      --self-test)
        tai_self_test
        return $?
        ;;
      --*) : ;;
      *)
        # 位置参数是目录 → 当作根（扫描其 config/）；是文件 → 只查该文件。
        # src/ 里模块存在性用的是 IMBOY_ROOT（默认仓库根），故查临时副本时
        # 仍能正确解析模块。
        if [ -d "$arg" ]; then root="$arg"; else only="$arg"; fi
        ;;
    esac
  done

  local f scanned=0
  if [ -n "$only" ]; then
    if [ ! -f "$only" ]; then
      echo "配置文件不存在：$only"
      return 1
    fi
    tai_check_file "$root" "$only"
    scanned=1
  else
    for f in config/sys.config.example config/sys.runtime.config config/sys.local.config config/sys.pro.config config/sys.dev.config; do
      [ -f "${root}/${f}" ] || continue
      scanned=$((scanned + 1))
      tai_check_file "$root" "${root}/${f}"
    done
  fi

  if [ "$scanned" -eq 0 ]; then
    echo "未找到任何配置文件（在 ${root}/config 下）"
    return 1
  fi

  echo "通过 ${TAI_PASS} 项，失败 ${TAI_FAIL} 项（硬失败仅「可证明打开也一定不工作」的四类）"
  [ "$TAI_FAIL" -eq 0 ]
}

# ------------------------------------------------------------
# fixture 自检
# ------------------------------------------------------------
tai_self_test() {
  local tmp rc=0 msg
  tmp="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmp'" EXIT
  mkdir -p "${tmp}/config" "${tmp}/src/lib"
  printf -- '-module(demo_llm).\n-export([capabilities/0]).\ncapabilities() -> #{vision => false}.\n' \
    >"${tmp}/src/lib/demo_llm.erl"

  tai_fixture() {
    cat >"${tmp}/config/sys.config.example" <<CFG
[{imboy, [
        {teaching_ai_llm_provider, $1},
        {teaching_ai_attach_video_url, $2}
    ]},
    {llm_providers, [
        #{name => <<"demo">>,
          module => demo_llm,
          api_key => <<"k">>,
          vision => $3}
    ]},
    {ecron, [
        {local_jobs, [
            {teaching_ai_worker, "*/1 * * * *", {demo_llm, cap, []}}
        ]}
    ]},
    {kernel, []}].
CFG
  }

  # 正例：名字匹配 + vision + key + 视频开 + 有作业
  tai_fixture '<<"demo">>' true true
  if ! IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then msg="正例应通过"; rc=1; else msg=""; fi
  [ -z "$msg" ] && tai_ok "self-test 正例通过" || { echo "  ✗ self-test ${msg}"; }

  # 反例1：名字不在 llm_providers 里
  tai_fixture '<<"nope">>' true true
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then echo "  ✗ self-test 名字不存在却通过"; rc=1; else tai_ok "self-test 反例1（provider 名不存在）判红"; fi

  # 反例2：条目无 vision
  tai_fixture '<<"demo">>' true false
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then echo "  ✗ self-test 无 vision 却通过"; rc=1; else tai_ok "self-test 反例2（条目无 vision）判红"; fi

  # 反例3：未启用态但无任何 vision 候选 → 只告警不失败（默认配置必须放行）
  cat >"${tmp}/config/sys.config.example" <<'CFG'
[{imboy, [
        {teaching_ai_llm_provider, undefined},
        {teaching_ai_attach_video_url, false}
    ]},
    {llm_providers, [
        #{name => <<"demo">>, module => demo_llm, api_key => <<"k">>, vision => false}
    ]},
    {kernel, []}].
CFG
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then tai_ok "self-test 反例3（未启用态无 vision 候选 = 告警不失败）放行"; else echo "  ✗ self-test 反例3 未启用态被误判失败"; rc=1; fi

  if [ "$rc" -eq 0 ]; then
    echo "MOYA_AI_CONFIG_SELF_TEST=PASS"
  else
    echo "MOYA_AI_CONFIG_SELF_TEST=FAIL"
  fi
  return $rc
}

# 被 source 时只加载函数
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
  tai_main "$@"
  exit $?
fi
