#!/usr/bin/env bash
# ============================================================
# check_l4_sni_listen_test.sh — Task 1 行为测试（自包含，离线）
# ------------------------------------------------------------
# 覆盖计划 Task 1 的最小反例集：健康 IPv4/IPv6、缺 proxy_protocol、
# 各类直连 443、空/缺/不可读文件、混合 server、注释、多行/未闭合、
# include、参数缺值/未知选项、--pre-switch、--metrics、--push。
# 每个反例同时断言退出码与 stderr 诊断内容（不只查非零）。
# 不触网：推送失败路径用本地不可达地址 http://127.0.0.1:1。
# 兼容 bash 3.2；不可读文件用例在 root 下跳过（chmod 000 对 root 无效）。
# ============================================================
set -uo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
SCRIPT="$ROOT/scripts/check_l4_sni_listen.sh"
TMP="$(mktemp -d /tmp/imboy_l4_sni_check.XXXXXX)"

PASS=0
FAIL=0
SKIP=0
RC=0

cleanup() {
  chmod -R u+rwx "$TMP" 2>/dev/null || true
  rm -rf -- "$TMP"
}
trap cleanup EXIT

ok()   { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad()  { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }
skip() { SKIP=$((SKIP + 1)); echo "  SKIP $1"; }

assert_rc() {
  local want="$1" desc="$2" detail
  if [ "$RC" = "$want" ]; then
    ok "$desc"
  else
    detail="$(LC_ALL=C tail -3 "$TMP/err" 2>/dev/null | LC_ALL=C tr '\n' '_')"
    bad "$desc" "exit=$RC want=$want stderr_tail=$detail"
  fi
}
assert_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then
    ok "$desc"
  else
    bad "$desc" "pattern=$pattern 未命中 $(basename "$file")=$(LC_ALL=C tail -2 "$file" 2>/dev/null | LC_ALL=C tr '\n' '_')"
  fi
}
assert_not_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then
    bad "$desc" "不应命中 pattern=$pattern $(basename "$file")=$(LC_ALL=C tail -2 "$file" 2>/dev/null | LC_ALL=C tr '\n' '_')"
  else
    ok "$desc"
  fi
}

run_check() {
  bash "$SCRIPT" "$@" >"$TMP/out" 2>"$TMP/err"
  RC=$?
}

# ── fixtures：所有反例文件（listen 行号固定，供 file:line 断言）─────────

cat >"$TMP/c01.conf" <<'NGINX'
server {
    listen 127.0.0.1:10443 ssl http2 proxy_protocol;
    server_name api.example.com;
}
NGINX

cat >"$TMP/c02.conf" <<'NGINX'
server {
    listen [::1]:10443 ssl proxy_protocol;
    server_name v6.example.com;
}
NGINX

cat >"$TMP/c03.conf" <<'NGINX'
server {
    listen 127.0.0.1:10443 ssl proxy_protocol;
    server_name a.example.com;
}
server {
    listen [::1]:10443 ssl proxy_protocol;
    server_name b.example.com;
}
NGINX

cat >"$TMP/c04.conf" <<'NGINX'
server {
    listen 80 default_server;
    listen 127.0.0.1:10443 ssl proxy_protocol;
    server_name mix.example.com;
}
NGINX

cat >"$TMP/c05.conf" <<'NGINX'
server {
    listen 80 default_server;
    server_name plain-http.example.com;
}
NGINX

: >"$TMP/c06.conf"

cat >"$TMP/c07.conf" <<'NGINX'
server {
    listen 443 ssl;
    server_name bare443.example.com;
}
NGINX

cat >"$TMP/c08.conf" <<'NGINX'
server {
    listen 192.0.2.7:443 ssl;
    server_name addr443.example.com;
}
NGINX

cat >"$TMP/c09.conf" <<'NGINX'
server {
    listen 0.0.0.0:443 ssl;
    server_name any443.example.com;
}
NGINX

cat >"$TMP/c10.conf" <<'NGINX'
server {
    listen *:443 ssl;
    server_name star443.example.com;
}
NGINX

cat >"$TMP/c11.conf" <<'NGINX'
server {
    listen [::]:443 ssl;
    server_name v6any443.example.com;
}
NGINX

cat >"$TMP/c12.conf" <<'NGINX'
server {
    listen [::1]:443 ssl;
    server_name v6loop443.example.com;
}
NGINX

cat >"$TMP/c13.conf" <<'NGINX'
server {
    listen 0.0.0.0:10443 ssl proxy_protocol;
    server_name open10443.example.com;
}
NGINX

cat >"$TMP/c14.conf" <<'NGINX'
server {
    listen 127.0.0.1:10443 ssl;
    server_name nopp.example.com;
}
NGINX

cat >"$TMP/c15.conf" <<'NGINX'
server {
    listen 8443 ssl;
    server_name oddport.example.com;
}
NGINX

cat >"$TMP/c16.conf" <<'NGINX'
server {
    listen 443 ssl;
    server_name unreadable.example.com;
}
NGINX

cat >"$TMP/c17.conf" <<'NGINX'
server {
    listen
    443 ssl;
}
NGINX

cat >"$TMP/c18.conf" <<'NGINX'
server {
    listen 443 ssl;
NGINX

cat >"$TMP/c19.conf" <<'NGINX'
server {
    include snippets/ssl.conf;
}
NGINX

cat >"$TMP/c20.conf" <<'NGINX'
# server {
#     listen 443 ssl;
# }
NGINX

cat >"$TMP/c21.conf" <<'NGINX'
# listen 443 ssl;
server {
    listen 127.0.0.1:10443 ssl proxy_protocol; # old: listen 443 ssl
    server_name commented.example.com;
}
NGINX

cat >"$TMP/c22.conf" <<'NGINX'
server {
    listen 443 ssl;
    server_name good.example.com;
}
server {
    listen 443 ssl;
    server_name bad.example.com;
}
NGINX

cat >"$TMP/c23.conf" <<'NGINX'
server {
    listen 127.0.0.1:10443 ssl proxy_protocol;
    listen 443 ssl;
    server_name both.example.com;
}
NGINX

cat >"$TMP/c24.conf" <<'NGINX'
server {
    listen 127.0.0.1:10443 ssl proxy_protocol banana=1;
    server_name badparam.example.com;
}
NGINX

cat >"$TMP/c25.conf" <<'NGINX'
listen 443 ssl;
NGINX

cat >"$TMP/c27.conf" <<'NGINX'
server {
    listen 80;
    ssl_certificate /etc/letsencrypt/live/x/fullchain.pem;
    server_name certonly.example.com;
}
NGINX

cat >"$TMP/c28.conf" <<'NGINX'
server { listen 127.0.0.1:10443 ssl proxy_protocol; server_name inline.example.com; }
NGINX

printf 'server {\r\n    listen 127.0.0.1:10443 ssl proxy_protocol;\r\n}\r\n' >"$TMP/c29.conf"

# c26（多打一个 }）单独生成，避免 heredoc 误导
cat >"$TMP/c26.conf" <<'NGINX'
server {
    listen 443 ssl;
}
}
NGINX

echo "== 严格模式：健康用例 =="
run_check "$TMP/c01.conf"
assert_rc 0 "健康 IPv4 loopback:10443 + proxy_protocol → 通过"
assert_not_grep "健康 IPv4 无违规诊断" ':[0-9]+: .*drift|:[0-9]+: .*missing|:[0-9]+: .*direct' "$TMP/err"

run_check "$TMP/c02.conf"
assert_rc 0 "健康 IPv6 loopback [::1]:10443 + proxy_protocol → 通过"

run_check "$TMP/c01.conf" "$TMP/c02.conf" "$TMP/c03.conf"
assert_rc 0 "多文件/多 server 全健康 → 通过"

run_check "$TMP/c04.conf"
assert_rc 0 "同 server 混 HTTP(80)+健康 HTTPS(10443) → HTTP listener 不产生违规"

run_check "$TMP/c05.conf"
assert_rc 0 "纯 HTTP 文件（只有 listen 80）→ 不产生违规"

run_check "$TMP/c06.conf"
assert_rc 0 "空文件 → 不产生违规"
assert_grep "空文件给出说明性诊断" 'c06\.conf:0: note: .*empty' "$TMP/err"

run_check "$TMP/c21.conf"
assert_rc 0 "注释行/行尾注释含 443 → 注释不算有效指令，被忽略"

run_check "$TMP/c28.conf"
assert_rc 0 "单行 server 块（parser 稳健性）→ 通过"

run_check "$TMP/c29.conf"
assert_rc 0 "CRLF 行尾 → 通过（\\r 按空白处理）"

echo "== 严格模式：直连 443 反例（退出 1 + file:line 诊断）=="

run_check "$TMP/c07.conf"
assert_rc 1 "裸 443 → 漂移（退出 1）"
assert_grep "裸 443 诊断含 file:line 与 direct :443" 'c07\.conf:2: .*direct :443' "$TMP/err"
assert_grep "诊断格式为 file:line: message" '^[^:]+\.conf:[0-9]+: ' "$TMP/err"

run_check "$TMP/c08.conf"
assert_rc 1 "IPv4 指定地址 192.0.2.7:443 → 漂移"
assert_grep "IPv4:443 诊断定位到行 2" 'c08\.conf:2: .*direct :443' "$TMP/err"

run_check "$TMP/c09.conf"
assert_rc 1 "0.0.0.0:443 → 漂移"
assert_grep "0.0.0.0:443 诊断" 'c09\.conf:2: .*direct :443' "$TMP/err"

run_check "$TMP/c10.conf"
assert_rc 1 "*:443 → 漂移"
assert_grep "\*:443 诊断" 'c10\.conf:2: .*direct :443' "$TMP/err"

run_check "$TMP/c11.conf"
assert_rc 1 "\[::\]:443 → 漂移"
assert_grep "\[::\]:443 诊断" 'c11\.conf:2: .*direct :443' "$TMP/err"

run_check "$TMP/c12.conf"
assert_rc 1 "\[::1\]:443（IPv6 loopback 但端口 443）→ 漂移"
assert_grep "\[::1\]:443 诊断" 'c12\.conf:2: .*direct :443' "$TMP/err"

run_check "$TMP/c13.conf"
assert_rc 1 "非回环 10443（0.0.0.0:10443）→ 漂移"
assert_grep "非回环 10443 诊断" 'c13\.conf:2: .*must bind 127\.0\.0\.1 or \[::1\]' "$TMP/err"

run_check "$TMP/c14.conf"
assert_rc 1 "loopback:10443 缺独立 proxy_protocol token → 漂移"
assert_grep "缺 proxy_protocol 诊断" 'c14\.conf:2: .*missing a standalone proxy_protocol' "$TMP/err"

run_check "$TMP/c15.conf"
assert_rc 1 "HTTPS(ssl) listener 在异常端口 8443 → 漂移"
assert_grep "异常端口诊断" 'c15\.conf:2: .*unexpected' "$TMP/err"

run_check "$TMP/c27.conf"
assert_rc 1 "ssl_certificate 存在但无 10443 listener → 漂移（受管 HTTPS server 缺 listener）"
assert_grep "缺 10443 listener 诊断" 'c27\.conf:1: .*no loopback:10443 listener' "$TMP/err"

echo "== 混合 server / listener（整体失败）=="

run_check "$TMP/c22.conf"
assert_rc 1 "同文件健康+443 server 混合 → 整体失败（退出 1）"
assert_grep "混合文件两个 server 均被判定" 'c22\.conf:2: .*direct :443' "$TMP/err"
assert_grep "混合文件第二个 server 诊断在行 6" 'c22\.conf:6: .*direct :443' "$TMP/err"

run_check "$TMP/c23.conf"
assert_rc 1 "同 server 内 10443 健康 + 443 直连混合 → 失败"
assert_grep "同 server 混合 listener 诊断" 'c23\.conf:3: .*direct :443' "$TMP/err"

echo "== 输入/解析错误（退出 2 + file:line 诊断）=="

run_check "$TMP/missing_file.conf"
assert_rc 2 "缺文件 → 退出 2"
assert_grep "缺文件诊断含路径" "missing_file\.conf:0: .*not found" "$TMP/err"

if [ "$(id -u)" = 0 ]; then
  skip "不可读文件用例（root 下 chmod 000 仍可读）"
else
  chmod 000 "$TMP/c16.conf"
  run_check "$TMP/c16.conf"
  assert_rc 2 "不可读文件（chmod 000）→ 退出 2"
  assert_grep "不可读诊断" 'c16\.conf:0: .*not readable|c16\.conf:0: .*cannot open' "$TMP/err"
  chmod 644 "$TMP/c16.conf"
fi

run_check "$TMP/c17.conf"
assert_rc 2 "多行 listen → 退出 2（不猜通过）"
assert_grep "多行指令诊断" 'c17\.conf:2: .*spans multiple lines' "$TMP/err"

run_check "$TMP/c18.conf"
assert_rc 2 "未闭合 server 块 → 退出 2"
assert_grep "未闭合块诊断" 'c18\.conf:2: .*unclosed block' "$TMP/err"

run_check "$TMP/c26.conf"
assert_rc 2 "多余 \} → 退出 2"
assert_grep "unbalanced 诊断" 'c26\.conf:4: .*unbalanced' "$TMP/err"

run_check "$TMP/c19.conf"
assert_rc 2 "include 指令 → 退出 2（不能可靠解析，不许猜通过）"
assert_grep "include 诊断" 'c19\.conf:2: .*include directive is not supported' "$TMP/err"

run_check "$TMP/c20.conf"
assert_rc 2 "只有注释的文件 → 退出 2（不能当作有效通过受检）"
assert_grep "只有注释诊断" 'c20\.conf:3: .*only comments' "$TMP/err"

run_check "$TMP/c25.conf"
assert_rc 2 "server 块外 listen → 退出 2"
assert_grep "块外 listen 诊断" 'c25\.conf:1: .*outside any server block' "$TMP/err"

run_check "$TMP/c24.conf"
assert_rc 2 "未知 listen 参数 → 退出 2（不猜测语义）"
assert_grep "未知参数诊断" 'c24\.conf:2: .*unknown listen parameter' "$TMP/err"

echo "== CLI 参数错误（退出 2）=="

run_check --pushgateway-url
assert_rc 2 "--pushgateway-url 缺值 → 退出 2"
assert_grep "缺值诊断" 'requires a URL' "$TMP/err"

run_check --bogus "$TMP/c01.conf"
assert_rc 2 "未知选项 → 退出 2"
assert_grep "未知选项诊断" 'unknown option: --bogus' "$TMP/err"

run_check --strict --pre-switch "$TMP/c01.conf"
assert_rc 2 "模式互斥冲突 → 退出 2"
assert_grep "模式冲突诊断" 'exactly one mode' "$TMP/err"

L4_SNI_ENV_FILE="$TMP/nonexistent.env" run_check
assert_rc 2 "无显式参数且发现 env 缺失 → 退出 2（受检列表必须非空）"
assert_grep "发现 env 缺失诊断" 'discovery env file is missing' "$TMP/err"

echo "== --pre-switch 模式 =="

run_check --pre-switch "$TMP/c07.conf"
assert_rc 0 "pre-switch：裸 443 直连放行（切换前状态）"

cat >"$TMP/p01.conf" <<'NGINX'
server {
    listen 443 ssl http2;
    listen [::]:443 ssl;
    server_name pre.example.com;
}
NGINX
run_check --pre-switch "$TMP/p01.conf"
assert_rc 0 "pre-switch：IPv4+IPv6 双栈 443 原始形态放行"

run_check --pre-switch "$TMP/c01.conf"
assert_rc 0 "pre-switch：已切换的健康形态同样放行"

run_check --pre-switch "$TMP/c18.conf"
assert_rc 2 "pre-switch：不可解析（未闭合）仍失败"
assert_grep "pre-switch 解析失败诊断" 'unclosed block' "$TMP/err"

run_check --pre-switch "$TMP/c14.conf"
assert_rc 1 "pre-switch：10443 缺 proxy_protocol 仍报漂移"
assert_grep "pre-switch 10443 规范诊断" 'c14\.conf:2: .*proxy_protocol' "$TMP/err"

echo "== --metrics 输出（接口合同冻结指标名）=="

run_check --metrics "$TMP/c01.conf"
assert_rc 0 "--metrics 健康 → 退出 0"
assert_grep "输出 drift 指标 TYPE 行" '^# TYPE imboy_l4_sni_listen_drift gauge$' "$TMP/out"
assert_grep "drift 序列带 config_file/server 标签且值 0" 'imboy_l4_sni_listen_drift\{config_file="[^"]+",server="api\.example\.com"\} 0' "$TMP/out"
assert_grep "check_success=1（检测完整成功）" '^imboy_l4_sni_check_success 1$' "$TMP/out"
assert_grep "last_check_timestamp_seconds 为 unix epoch" '^imboy_l4_sni_last_check_timestamp_seconds [0-9]+$' "$TMP/out"
assert_not_grep "--metrics 模式 stdout 不含人类摘要" 'CHECK_OK' "$TMP/out"

run_check --metrics "$TMP/c07.conf"
assert_rc 1 "--metrics 漂移 → 退出 1 且 drift=1、check_success=1"
assert_grep "漂移序列值 1" 'imboy_l4_sni_listen_drift\{config_file="[^"]+",server="[^"]+"\} 1' "$TMP/out"
assert_grep "漂移时 check_success 仍为 1" '^imboy_l4_sni_check_success 1$' "$TMP/out"

run_check --metrics "$TMP/c18.conf"
assert_rc 2 "--metrics 解析失败 → 退出 2 且 check_success=0"
assert_grep "解析失败 check_success=0" '^imboy_l4_sni_check_success 0$' "$TMP/out"

echo "== --push（缺 URL / 推送失败 → 退出 3；不触网）=="

PUSHGATEWAY_URL= run_check --push "$TMP/c01.conf"
assert_rc 3 "--push 缺 Pushgateway URL → 退出 3"
assert_grep "缺 URL 明确原因" 'no Pushgateway URL' "$TMP/err"

run_check --push --pushgateway-url http://127.0.0.1:1 "$TMP/c01.conf"
assert_rc 3 "--push 推送到本地不可达地址失败 → 退出 3"
assert_grep "推送失败诊断含 URL" 'push to Pushgateway failed.*127\.0\.0\.1:1' "$TMP/err"

PUSHGATEWAY_URL=http://127.0.0.1:1 run_check --push "$TMP/c01.conf"
assert_rc 3 "--push 经 PUSHGATEWAY_URL 环境变量取 URL 失败 → 退出 3"

PUSHGATEWAY_URL=http://127.0.0.1:1 run_check "$TMP/c07.conf"
assert_rc 1 "未启用 --push 时只检查不推送（退出码不受推送影响）"
assert_not_grep "未推送时无推送诊断" 'push to Pushgateway failed' "$TMP/err"

echo "== 内置发现与显式参数优先级 =="

mkdir -p "$TMP/dis"
cp "$TMP/c01.conf" "$TMP/dis/d1.conf"
cp "$TMP/c02.conf" "$TMP/dis/d2.conf"
cat >"$TMP/l4.env" <<ENV
NGINX_VHOST_DIR=$TMP/dis
HTTPS_VHOST_FILES="d1.conf d2.conf"
ENV
L4_SNI_ENV_FILE="$TMP/l4.env" run_check
assert_rc 0 "发现模式：从 env 文件发现 2 个健康文件 → 通过"
assert_grep "发现日志说明数量与来源" 'discovered 2 config file\(s\)' "$TMP/err"

cp "$TMP/c07.conf" "$TMP/dis/d2.conf"
L4_SNI_ENV_FILE="$TMP/l4.env" run_check
assert_rc 1 "发现模式：发现文件漂移 → 退出 1"
assert_grep "发现文件漂移诊断" 'd2\.conf:2: .*direct :443' "$TMP/err"

L4_SNI_ENV_FILE="$TMP/l4.env" run_check "$TMP/c01.conf"
assert_rc 0 "显式参数优先：显式传健康文件时跳过发现（漂移文件不被检查）"
assert_not_grep "显式模式不做发现" 'discovered' "$TMP/err"
assert_not_grep "显式模式不产生发现文件的漂移诊断" 'direct :443' "$TMP/err"

cat >"$TMP/l4-empty.env" <<ENV
NGINX_VHOST_DIR=$TMP/dis
ENV
L4_SNI_ENV_FILE="$TMP/l4-empty.env" run_check
assert_rc 2 "发现 env 缺 HTTPS_VHOST_FILES → 退出 2"
assert_grep "发现字段缺失诊断" 'needs both NGINX_VHOST_DIR and HTTPS_VHOST_FILES' "$TMP/err"

echo "== 退出码优先级 2 > 1 =="

cat >"$TMP/p02.conf" <<'NGINX'
server {
    listen 443 ssl;
NGINX
run_check "$TMP/c07.conf" "$TMP/p02.conf"
assert_rc 2 "同批文件既有漂移又有不可解析 → 退出 2 优先"
assert_grep "优先级用例仍输出漂移诊断" 'c07\.conf:2: .*direct :443' "$TMP/err"
assert_grep "优先级用例仍输出解析诊断" 'p02\.conf:2: .*unclosed' "$TMP/err"

echo "== 其他 =="

run_check -h
assert_rc 0 "-h 输出用法并退出 0"
assert_grep "usage 说明优先级" 'explicit' "$TMP/out"

printf 'server {\n    listen 443 ssl;\n}\n' >"$TMP/p03.conf"
run_check --metrics --push --pushgateway-url http://127.0.0.1:1 "$TMP/p03.conf"
assert_rc 3 "组合：漂移 + metrics + 推送失败 → 退出 3（推送失败优先报出）"
assert_grep "组合用例 drift 序列仍输出" 'imboy_l4_sni_listen_drift\{[^}]*\} 1' "$TMP/out"

echo "== 结果：PASS=$PASS FAIL=$FAIL SKIP=$SKIP =="
if [ "$FAIL" -ne 0 ]; then
  exit 1
fi
exit 0
