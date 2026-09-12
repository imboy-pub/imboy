#!/usr/bin/env bash
# ============================================================
# 定时作业（ecron）配置一致性门禁 / Cron jobs config gate
# ------------------------------------------------------------
# 为什么需要这个门禁：
#   全仓唯一的 ecron 作业真源是 config/sys.config.example 顶层的
#   {ecron,[{local_jobs,[...]}]}。其余运行配置
#   （config/sys.local.config / sys.pro.config / sys.dev.config）
#   全被 .gitignore 忽略（*local*.config / *pro*.config / *.dev.config），
#   是每台机器各写各的，**不会**从模板同步。
#
#   历史事故：alpha.42（f5420d70）把周期作业从 config/cron.config 迁进
#   模板时，逐机配置没有跟着迁——它们至今残留着指向已删加载器的死键
#   {config_files,[{cron_config,"config/cron.config"}]}，而
#   config/cron.config 里的 crontab_jobs 已无任何代码读取。于是凡以
#   `IMBOYENV=<env>` 构建/启动的实例，ecron 作业数为 0，且**不报错**：
#   附件孤儿清理 / 支付对账 / 红包过期退款 / 教学 Worker 全部静默不跑。
#   本机 dev 节点即为此形态（release sys.config -> config/sys.local.config），
#   55 条 calligraphy_review_draft 恒卡 queued 就是它的可观察后果。
#
# 检查项：
#   [硬门] 1. 模板存在顶层 {ecron,[...]} 段，且 local_jobs 解析出作业 > 0
#   [硬门] 2. 未回退到 ecron v1.1.0 不认的 {jobs,...} 键（R7 事故：该键下
#            全部作业从未被调度）
#   [硬门] 3. 每个作业的 crontab 规格合法（5 段，或 @yearly/@monthly/
#            @weekly/@daily/@midnight/@hourly/@minutely/@every 宏）
#   [硬门] 4. 每个作业的 {Mod,Fun,_} —— 模块在 src/ 真实存在，且入口函数
#            在模块内出现（**改名的连带检查**：重命名模块/入口后这里立刻红）
#   [硬门] 5. job 名不重复
#   [软告警] 6. 逐机运行配置若存在：无 ecron 段 / 作业集与模板不一致
#            → 默认只告警（这些文件是每台机器各写各的，且修它可能要动
#              本机 dev 行为，不宜由门禁代拍）；--strict 或
#              IMBOY_CRON_STRICT=1 时判失败，用于发布前自查
#   [软告警] 7. 残留死键 config_files / cron_config
#
# 只读：不改任何文件；只打印作业名与模块名，不读取也不打印任何密钥。
#
# 用法 / Usage：
#   bash scripts/check_cron_config.sh              # 硬门 + 逐机告警
#   bash scripts/check_cron_config.sh --strict     # 逐机漂移也判失败
#   bash scripts/check_cron_config.sh --self-test  # fixture 自检
#   IMBOY_ROOT=/path bash scripts/check_cron_config.sh
# 退出码：0=通过；1=存在失败项
# ============================================================
set -uo pipefail

CCC_ROOT="${IMBOY_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"

CCC_PASS=0
CCC_FAIL=0

ccc_ok() {
  CCC_PASS=$((CCC_PASS + 1))
  echo "  ✓ $1"
}

ccc_bad() {
  CCC_FAIL=$((CCC_FAIL + 1))
  echo "  ✗ $1"
}

ccc_warn() {
  echo "  ⚠ $1"
}

# ------------------------------------------------------------
# 取顶层 {ecron, [ ... ]} 段（顶层元组缩进固定 4 空格），剔除整行注释。
# ------------------------------------------------------------
ccc_ecron_block() {
  sed -n '/^    {ecron, \[/,/^    \]}/p' "$1" 2>/dev/null | grep -vE '^[[:space:]]*%'
}

# ------------------------------------------------------------
# 从配置抽出作业，每行一条 "job|spec|mod|fun"。
# 作业元组可能跨行（{job, "spec",\n {mod, fun, []}}），故先合并成一行再匹配。
# 名字一律用 [a-z0-9_]+：job/模块/函数名都可以带数字（如 foo_v2），
# 用 [a-z_]+ 会把这类条目**静默跳过**——那是最危险的失败模式，
# 故另有 ccc_spec_count/1 做解析缺口 fail-closed 守卫。
# ------------------------------------------------------------
ccc_jobs() {
  ccc_ecron_block "$1" \
    | tr '\n' ' ' \
    | grep -oE '\{[a-z0-9_]+,[[:space:]]*"[^"]*",[[:space:]]*\{[a-z0-9_]+,[[:space:]]*[a-z0-9_]+,[[:space:]]*\[' \
    | sed -E 's/^\{(.*),[[:space:]]*"(.*)",[[:space:]]*\{([a-z0-9_]+),[[:space:]]*([a-z0-9_]+),[[:space:]]*\[$/\1|\2|\3|\4/'
}

# 配置合并死键的**非注释**出现（行号+内容）。注释过滤不可省：
# 模板里恰好用注释记录了这些死键的历史，不加过滤会把说明文字当成真键判红。
ccc_dead_keys() {
  grep -nE 'auto_load_configs|cron_config|config_files' "$1" 2>/dev/null \
    | grep -vE '^[0-9]+:[[:space:]]*%'
}

# 配置里"作业名 + crontab 规格"前缀的出现次数。它与 ccc_jobs 的行数必须相等：
# 少一个就说明有作业没被解析出来（名字带数字、元组写法变化等），
# 此时门禁必须红，绝不能静默放过。
ccc_spec_count() {
  ccc_ecron_block "$1" \
    | tr '\n' ' ' \
    | grep -oE '\{[a-z0-9_]+,[[:space:]]*"' \
    | grep -c .
}

# ------------------------------------------------------------
# 1~5：模板侧硬门。这些项的失败意味着一份**语法上合法但行为上是死的**
# 配置，正是最容易被忽略的一类腐坏。
# ------------------------------------------------------------
ccc_check_template() {
  local root="$1"
  local tpl="${root}/config/sys.config.example"
  local rc=0

  if [ ! -f "$tpl" ]; then
    ccc_bad "模板缺失：config/sys.config.example"
    return 1
  fi

  if [ -z "$(ccc_ecron_block "$tpl")" ]; then
    ccc_bad "模板缺顶层 {ecron, [...]} 段 —— 全仓定时作业真源不在，任何环境都不会有作业"
    return 1
  fi
  ccc_ok "模板含顶层 {ecron, [...]} 段"

  # 2. R7 回归：ecron v1.1.0 只认 local_jobs / global_jobs
  if ccc_ecron_block "$tpl" | grep -qE '\{[[:space:]]*jobs[[:space:]]*,'; then
    ccc_bad "模板 ecron 段出现 {jobs,...} 键 —— ecron v1.1.0 不读取它（R7 事故），该键下作业永不调度"
    rc=1
  else
    ccc_ok "ecron 段未使用不被识别的 {jobs,...} 键"
  fi

  # 死键：旧「启动时合并额外配置文件」加载器的键。加载器已于 T13 (fc687c44)
  # 移除，`git log -S auto_load_configs -- src/` 为空（imboy 代码从未读过）。
  # 留着这些键的危险不是"多一行配置"，而是**让人以为周期作业由
  # config/cron.config 提供**——真源其实是下面的 {ecron,[...]} 段。
  local dead
  dead="$(ccc_dead_keys "$tpl" | head -5)"
  if [ -n "$dead" ]; then
    ccc_bad "模板残留配置合并死键（加载器已移除、无任何代码读取）：$(printf '%s' "$dead" | tr '\n' ' ')"
    rc=1
  else
    ccc_ok "模板无配置合并死键（auto_load_configs / config_files / cron_config）"
  fi

  local jobs
  jobs="$(ccc_jobs "$tpl")"
  if [ -z "$jobs" ]; then
    ccc_bad "模板 ecron 段解析出 0 个作业（local_jobs 为空或格式已变）"
    return 1
  fi

  # 解析缺口 fail-closed：抽出的条目数必须等于配置里的「名+规格」条目数。
  # 不等就意味着有作业没被门禁看到——宁可判红，也不要静默放过。
  local got expect
  got="$(printf '%s\n' "$jobs" | grep -c .)"
  expect="$(ccc_spec_count "$tpl")"
  if [ "$got" -ne "$expect" ]; then
    ccc_bad "作业解析缺口：配置含 ${expect} 个「作业名+规格」条目，只解析出 ${got} 个 —— 有作业未被门禁覆盖（fail-closed，先修解析规则再放行）"
    rc=1
  else
    ccc_ok "作业解析无缺口（${got}/${expect}），共 ${got} 个作业"
  fi

  # 5. 重名
  local dup
  dup="$(printf '%s\n' "$jobs" | cut -d'|' -f1 | sort | uniq -d)"
  if [ -n "$dup" ]; then
    ccc_bad "job 名重复：$(printf '%s' "$dup" | tr '\n' ' ')"
    rc=1
  else
    ccc_ok "job 名无重复"
  fi

  # 3+4. 逐作业：规格合法 + MFA 目标真实存在
  local line name spec mod fun f bad_spec="" bad_mod="" bad_fun=""
  while IFS= read -r line; do
    [ -n "$line" ] || continue
    name="$(printf '%s' "$line" | cut -d'|' -f1)"
    spec="$(printf '%s' "$line" | cut -d'|' -f2)"
    mod="$(printf '%s' "$line" | cut -d'|' -f3)"
    fun="$(printf '%s' "$line" | cut -d'|' -f4)"

    case "$spec" in
      @yearly | @annually | @monthly | @weekly | @daily | @midnight | @hourly | @minutely) : ;;
      "@every "*) : ;;
      *)
        # 五段：分 时 日 月 周。用 wc -w 数词，不要用 `set -- $spec`——
        # 未加引号的 $spec 里 '*' 会被 shell 当通配符展开，字段数恒不等于 5。
        if [ "$(printf '%s' "$spec" | wc -w | tr -d ' ')" -ne 5 ]; then
          bad_spec="${bad_spec} ${name}('${spec}')"
        fi
        ;;
    esac

    f="$(find "${root}/src" -name "${mod}.erl" 2>/dev/null | head -1)"
    if [ -z "$f" ]; then
      bad_mod="${bad_mod} ${name}->${mod}"
    elif ! grep -q "\b${fun}\b" "$f" 2>/dev/null; then
      bad_fun="${bad_fun} ${name}->${mod}:${fun}"
    fi
  done <<EOF
$jobs
EOF

  if [ -n "$bad_spec" ]; then
    ccc_bad "crontab 规格非法（须 5 段或 @macro）：${bad_spec}"
    rc=1
  else
    ccc_ok "全部作业 crontab 规格合法"
  fi
  if [ -n "$bad_mod" ]; then
    ccc_bad "作业指向的模块在 src/ 不存在（改名/删除的连带缺口）：${bad_mod}"
    rc=1
  else
    ccc_ok "全部作业目标模块存在"
  fi
  if [ -n "$bad_fun" ]; then
    ccc_bad "作业入口函数在目标模块内找不到（改名/删除的连带缺口）：${bad_fun}"
    rc=1
  else
    ccc_ok "全部作业入口函数存在"
  fi

  return $rc
}

# ------------------------------------------------------------
# 6~7：逐机运行配置漂移。默认告警，--strict 判失败。
# 这些文件被 .gitignore 忽略、每台机器各写各的，CI 里通常不存在
# （CI 的 sys.runtime.config 由 Makefile 从模板生成，故天然一致）。
# ------------------------------------------------------------
ccc_check_runtime_configs() {
  local root="$1"
  local strict="$2"
  local tpl="${root}/config/sys.config.example"
  local canon
  canon="$(ccc_jobs "$tpl" | cut -d'|' -f1 | sort)"

  local missing_total=""
  local f
  for f in sys.runtime.config sys.local.config sys.pro.config sys.dev.config; do
    [ -f "${root}/config/${f}" ] || continue

    if [ -z "$(ccc_ecron_block "${root}/config/${f}")" ]; then
      ccc_warn "config/${f} 无 ecron 段 → 该环境定时作业数为 0（附件清理/支付对账/红包退款/教学 Worker 都不会跑，且不报错）"
      missing_total="${missing_total} ${f}"
    else
      local actual
      actual="$(ccc_jobs "${root}/config/${f}" | cut -d'|' -f1 | sort)"
      if [ "$actual" = "$canon" ]; then
        ccc_ok "config/${f} 作业集与模板一致（$(printf '%s\n' "$actual" | grep -c .) 个）"
      else
        ccc_warn "config/${f} 作业集与模板不一致：缺=[$(printf '%s\n' "$canon" | grep -vxF "$actual" | tr '\n' ' ')] 多=[$(printf '%s\n' "$actual" | grep -vxF "$canon" | tr '\n' ' ')]"
        missing_total="${missing_total} ${f}"
      fi
    fi

    if [ -n "$(ccc_dead_keys "${root}/config/${f}")" ]; then
      ccc_warn "config/${f} 残留配置合并死键（auto_load_configs / config_files / cron_config）—— 加载器已移除，其指向的 config/cron.config 无任何代码读取，不是作业真源"
    fi
  done

  [ -z "${missing_total}" ] && ccc_ok "逐机运行配置与模板一致（或无逐机配置）"

  # 修法提示：把模板 {ecron, [...]} 段整体复制进目标文件即可。
  if [ -n "${missing_total}" ] && [ "$strict" = "1" ]; then
    ccc_bad "逐机漂移在 --strict 下判失败：${missing_total}（修法：将 config/sys.config.example 的 {ecron,[...]} 段整体复制进该文件；只想显式关闭则改为空 local_jobs 并在此门禁登记豁免）"
    return 1
  fi
  return 0
}

ccc_main() {
  local strict=0
  local arg
  for arg in "$@"; do
    case "$arg" in
      --strict) strict=1 ;;
      --self-test)
        ccc_self_test
        return $?
        ;;
      *) : ;;
    esac
  done
  [ "${IMBOY_CRON_STRICT:-0}" = "1" ] && strict=1

  echo "== 1. 模板定时作业真源（硬门）=="
  ccc_check_template "$CCC_ROOT"
  echo "== 2. 逐机运行配置一致性（$([ "$strict" = "1" ] && echo '严格模式' || echo '告警模式')）=="
  ccc_check_runtime_configs "$CCC_ROOT" "$strict"

  echo ""
  echo "通过 ${CCC_PASS} 项，失败 ${CCC_FAIL} 项"
  [ "$CCC_FAIL" -eq 0 ]
}

# ------------------------------------------------------------
# fixture 自检：不依赖本机 config/ 现状，构造最小配置验证门禁真的会红。
# ------------------------------------------------------------
ccc_self_test() {
  local tmp
  tmp="$(mktemp -d)"
  # shellcheck disable=SC2064
  trap "rm -rf '$tmp'" EXIT
  mkdir -p "${tmp}/src/logic" "${tmp}/config"

  cat >"${tmp}/src/logic/demo_logic.erl" <<'ERL'
-module(demo_logic).
-export([run/0]).
run() -> ok.
ERL

  ccc_make_tpl() {
    cat >"${tmp}/config/sys.config.example" <<'CFG'
[{imboy, [{http_port, 9800}]},
    {ecron, [
        {time_zone, local},
        {local_jobs, [
            {demo_job, "*/5 * * * *", {demo_logic, run, []}}
        ]},
        {global_jobs, []}
    ]},
    {kernel, []}].
CFG
  }

  local rc=0 label

  # 正例：模板完好、无逐机配置 → 通过
  ccc_make_tpl
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then :; else label="正例应通过"; fi
  [ -z "${label:-}" ] && ccc_ok_out "self-test 正例通过" || { echo "  ✗ self-test ${label}"; rc=1; }

  # 反例 1：模板无 ecron 段 → 必须红
  cat >"${tmp}/config/sys.config.example" <<'CFG'
[{imboy, [{http_port, 9800}]},
    {kernel, []}].
CFG
  label=''
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then label="缺 ecron 段却判通过"; fi
  [ -z "${label}" ] && ccc_ok_out "self-test 反例1（模板缺 ecron 段）正确判红" || { echo "  ✗ self-test ${label}"; rc=1; }

  # 反例 2：作业指向不存在的模块 → 必须红
  ccc_make_tpl
  sed -i.bak 's/demo_logic/nope_logic/g' "${tmp}/config/sys.config.example"
  label=''
  if IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1; then label="幽灵模块却判通过"; fi
  [ -z "${label}" ] && ccc_ok_out "self-test 反例2（作业指向不存在模块）正确判红" || { echo "  ✗ self-test ${label}"; rc=1; }

  # 反例 3：逐机配置无 ecron 段 → 默认放行、--strict 必须红
  ccc_make_tpl
  cat >"${tmp}/config/sys.local.config" <<'CFG'
[{imboy, [{http_port, 9800}]},
    {kernel, []}].
CFG
  label=''
  IMBOY_ROOT="$tmp" bash "$0" >/dev/null 2>&1 || label="告警模式下逐机漂移误判失败"
  [ -n "${label}" ] && { echo "  ✗ self-test ${label}"; rc=1; }
  label=''
  IMBOY_ROOT="$tmp" bash "$0" --strict >/dev/null 2>&1 && label="--strict 下逐机漂移却判通过"
  [ -n "${label}" ] && { echo "  ✗ self-test ${label}"; rc=1; }
  [ -z "${label}" ] && ccc_ok_out "self-test 反例3（逐机漂移：默认放行/严格判红）双态正确"

  if [ "$rc" -eq 0 ]; then
    echo "CRON_CONFIG_SELF_TEST=PASS"
  else
    echo "CRON_CONFIG_SELF_TEST=FAIL"
  fi
  return $rc
}

ccc_ok_out() {
  echo "  ✓ $1"
}

# 被 source 时只加载函数，便于测试注入 fixture 根目录
if [ "${BASH_SOURCE[0]}" = "${0}" ]; then
  ccc_main "$@"
  exit $?
fi
