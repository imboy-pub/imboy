#!/usr/bin/env bash
# ============================================================
# metrics_push.sh 严格模式与结果观测行为测试（L4 SNI 计划 Task 2 / A2）
# ------------------------------------------------------------
# 自包含：不触外网。用 127.0.0.1 随机端口的 python3 HTTP 服务伪造
# Pushgateway，捕获请求（方法/路径/请求体）到本地文件；结束自动清理。
#
# 覆盖：
#   1. 默认宽松语义回归（URL 未设置 / 推送失败均返回 0，默认 stderr 不变）
#   2. 严格模式 METRICS_PUSH_STRICT=1（未设置 URL / 推送失败返回非零，
#      stderr 有明确原因；成功返回 0）
#   3. 伪造 Pushgateway 收到的请求路径 /metrics/job/<job>/component/<c>
#      与 payload 内容
#   4. 结果观测变量 METRICS_PUSH_LAST_STATUS(ok/fail/skipped) 与时间戳
#   5. 与 A1 check_l4_sni_listen.sh --metrics 输出的兼容性：三个冻结
#      指标名 + Pushgateway 文本格式（python 解析校验语法；
#      --metrics 只输出不推送，不需要网络）
#
# 运行 / Run:  bash scripts/test/metrics_push_l4_test.sh
# ============================================================
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PASS=0
FAIL=0

ok()   { PASS=$((PASS+1)); echo "  ✓ $1"; }
bad()  { FAIL=$((FAIL+1)); echo "  ✗ $1"; }
check(){ if [ "$2" = "$3" ]; then ok "$1"; else bad "$1 (期望 '$3'，实际 '$2')"; fi; }
contains() {
  if printf '%s\n' "$2" | grep -q -- "$3"; then ok "$1"; else bad "$1 (未包含 '$3')"; fi
}
file_contains() {
  if grep -qF -- "$3" "$2" 2>/dev/null; then ok "$1"; else bad "$1 ($2 未包含 '$3')"; fi
}

# ── 伪造 Pushgateway（127.0.0.1 随机端口，请求记录到 requests.log） ──
FAKE_DIR="$(mktemp -d /tmp/metrics_push_l4_test.XXXXXX)"
FAKE_PID=""

cleanup() {
  if [ -n "$FAKE_PID" ]; then
    kill "$FAKE_PID" 2>/dev/null
    wait "$FAKE_PID" 2>/dev/null
  fi
  rm -rf "$FAKE_DIR"
}
trap cleanup EXIT

if ! command -v python3 >/dev/null 2>&1; then
  echo "BLOCKED_ENV: python3 不可用，无法伪造 Pushgateway" >&2
  exit 9
fi

python3 - "$FAKE_DIR" >"$FAKE_DIR/server.out" 2>"$FAKE_DIR/server.err" <<'PY' &
import http.server, json, os, sys

outdir = sys.argv[1]

class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _handle(self):
        n = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(n).decode("utf-8", "replace") if n else ""
        with open(os.path.join(outdir, "requests.log"), "a") as f:
            f.write(json.dumps({"method": self.command,
                                "path": self.path,
                                "body": body}) + "\n")
        self.send_response(200)
        self.send_header("Content-Length", "2")
        self.end_headers()
        self.wfile.write(b"ok")

    do_POST = _handle
    do_PUT = _handle

    def log_message(self, *args):
        pass

srv = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
with open(os.path.join(outdir, "port"), "w") as f:
    f.write(str(srv.server_port))
srv.serve_forever()
PY
FAKE_PID=$!

PORT=""
i=0
while [ "$i" -lt 100 ]; do
  if [ -f "$FAKE_DIR/port" ]; then PORT="$(cat "$FAKE_DIR/port")"; break; fi
  if ! kill -0 "$FAKE_PID" 2>/dev/null; then break; fi
  sleep 0.1
  i=$((i+1))
done
if [ -z "$PORT" ]; then
  echo "BLOCKED_ENV: 伪造 Pushgateway 启动失败" >&2
  cat "$FAKE_DIR/server.err" >&2
  exit 9
fi
ok "伪造 Pushgateway 已监听 127.0.0.1:${PORT}"

# shellcheck source=scripts/lib/metrics_push.sh
. "${ROOT}/scripts/lib/metrics_push.sh"

NOW="$(date -u +%s)"
TEST_START_TS="$NOW"
save_url="$PUSHGATEWAY_URL"
save_strict="${METRICS_PUSH_STRICT:-}"
save_timeout="$PUSH_TIMEOUT_SEC"

ts_is_valid() {
  printf '%s' "${METRICS_PUSH_LAST_TS:-}" | grep -Eq '^[0-9]+$' \
    && [ "${METRICS_PUSH_LAST_TS:-0}" -ge "$TEST_START_TS" ]
}

# ============================================================
echo "== 默认宽松语义回归（现有调用方不受影响） =="

# 1) URL 未设置 → rc 0
PUSHGATEWAY_URL=""
push_backup_result pg 1 "$NOW" >"$FAKE_DIR/out1" 2>"$FAKE_DIR/err1"
check "URL 未设置返回 0" "$?" "0"
file_contains "URL 未设置 stderr 说明跳过" "$FAKE_DIR/err1" "跳过指标推送"
if grep -q "严格模式" "$FAKE_DIR/err1"; then bad "默认模式 stderr 不应出现严格模式字样"; else ok "默认模式 stderr 无严格模式字样"; fi
check "结果观测 skipped" "${METRICS_PUSH_LAST_STATUS:-}" "skipped"
if ts_is_valid; then ok "skipped 态时间戳有效"; else bad "skipped 态时间戳无效 '${METRICS_PUSH_LAST_TS:-}'"; fi

# 2) 推送失败（不可达回环端口）→ rc 0
PUSHGATEWAY_URL="http://127.0.0.1:1"
PUSH_TIMEOUT_SEC=2
push_backup_result pg 1 "$NOW" >"$FAKE_DIR/out2" 2>"$FAKE_DIR/err2"
check "推送失败仍返回 0（宽松语义不因监控故障判失败）" "$?" "0"
file_contains "推送失败 stderr 告警" "$FAKE_DIR/err2" "推送失败"
if grep -q "严格模式" "$FAKE_DIR/err2"; then bad "默认模式 stderr 不应出现严格模式字样"; else ok "默认模式 stderr 无严格模式字样"; fi
check "结果观测 fail" "${METRICS_PUSH_LAST_STATUS:-}" "fail"
if ts_is_valid; then ok "fail 态时间戳有效"; else bad "fail 态时间戳无效 '${METRICS_PUSH_LAST_TS:-}'"; fi

# 3) 默认推送成功 → rc 0、无 stdout 输出、stderr 保持原有单行
PUSHGATEWAY_URL="http://127.0.0.1:${PORT}"
push_backup_result pg 1 "$NOW" >"$FAKE_DIR/out3" 2>"$FAKE_DIR/err3"
check "默认推送成功返回 0" "$?" "0"
check "默认成功不产生 stdout（输出约定零改变）" "$(wc -c < "$FAKE_DIR/out3" | tr -d ' ')" "0"
file_contains "默认成功 stderr 仍为已推送行" "$FAKE_DIR/err3" "已推送 job=imboy_backup component=pg"
check "默认成功 stderr 单行（无追加信息）" "$(wc -l < "$FAKE_DIR/err3" | tr -d ' ')" "1"
check "结果观测 ok" "${METRICS_PUSH_LAST_STATUS:-}" "ok"
if ts_is_valid; then ok "ok 态时间戳有效"; else bad "ok 态时间戳无效 '${METRICS_PUSH_LAST_TS:-}'"; fi

# ============================================================
echo "== 严格模式（METRICS_PUSH_STRICT=1，显式 opt-in） =="

# 4) 严格 + URL 未设置 → rc 非零，stderr 有明确原因
PUSHGATEWAY_URL=""
METRICS_PUSH_STRICT=1
push_backup_result pg 1 "$NOW" >"$FAKE_DIR/out4" 2>"$FAKE_DIR/err4"
rc4=$?
METRICS_PUSH_STRICT="$save_strict"
if [ "$rc4" -ne 0 ]; then ok "严格+URL 未设置返回非零 (rc=$rc4)"; else bad "严格+URL 未设置应返回非零"; fi
file_contains "严格+URL 未设置 stderr 指明原因" "$FAKE_DIR/err4" "PUSHGATEWAY_URL 未设置"
file_contains "严格+URL 未设置 stderr 标明严格模式" "$FAKE_DIR/err4" "严格模式"
check "结果观测 skipped" "${METRICS_PUSH_LAST_STATUS:-}" "skipped"

# 5) 严格 + 推送失败 → rc 非零
PUSHGATEWAY_URL="http://127.0.0.1:1"
METRICS_PUSH_STRICT=1
push_backup_result pg 1 "$NOW" >"$FAKE_DIR/out5" 2>"$FAKE_DIR/err5"
rc5=$?
METRICS_PUSH_STRICT="$save_strict"
if [ "$rc5" -ne 0 ]; then ok "严格+推送失败返回非零 (rc=$rc5)"; else bad "严格+推送失败应返回非零"; fi
file_contains "严格+推送失败 stderr 告警" "$FAKE_DIR/err5" "推送失败"
file_contains "严格+推送失败 stderr 标明严格模式" "$FAKE_DIR/err5" "严格模式"
check "结果观测 fail" "${METRICS_PUSH_LAST_STATUS:-}" "fail"

# 6) 严格 + 推送成功 → rc 0（换 push_tls_expiry 覆盖另一推送函数）
PUSHGATEWAY_URL="http://127.0.0.1:${PORT}"
METRICS_PUSH_STRICT=1
push_tls_expiry "imboy.example.com" 1800000000 >"$FAKE_DIR/out6" 2>"$FAKE_DIR/err6"
rc6=$?
METRICS_PUSH_STRICT="$save_strict"
check "严格+推送成功返回 0" "$rc6" "0"
file_contains "严格成功 stderr 已推送" "$FAKE_DIR/err6" "已推送 job=imboy_tls"
check "结果观测 ok" "${METRICS_PUSH_LAST_STATUS:-}" "ok"

PUSHGATEWAY_URL="$save_url"
PUSH_TIMEOUT_SEC="$save_timeout"

# ============================================================
echo "== 伪造 Pushgateway 收到的请求（路径与 payload） =="

REQS="$FAKE_DIR/requests.log"
file_contains "请求路径 /metrics/job/imboy_backup/component/pg" "$REQS" '"path": "/metrics/job/imboy_backup/component/pg"'
file_contains "请求路径 /metrics/job/imboy_tls/component/imboy.example.com" "$REQS" '"path": "/metrics/job/imboy_tls/component/imboy.example.com"'

if python3 - "$REQS" <<'PY'
import json, sys

reqs = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
assert reqs, "no requests captured"
for r in reqs:
    assert r["method"] in ("POST", "PUT"), r
    assert r["path"].startswith("/metrics/job/"), r["path"]

jobs = [r["path"] for r in reqs]
assert "/metrics/job/imboy_backup/component/pg" in jobs, jobs
assert "/metrics/job/imboy_tls/component/imboy.example.com" in jobs, jobs

body = next(r["body"] for r in reqs
            if r["path"] == "/metrics/job/imboy_backup/component/pg")
assert "# TYPE imboy_backup_last_status gauge" in body, body
assert "imboy_backup_last_status 1" in body, body
assert "imboy_backup_last_success_timestamp" in body, body

tls = next(r["body"] for r in reqs
           if r["path"] == "/metrics/job/imboy_tls/component/imboy.example.com")
assert "imboy_tls_cert_expiry_timestamp 1800000000" in tls, tls
print("request-shape OK")
PY
then ok "全部请求为 POST/PUT、路径 /metrics/job/... 且 payload 内容正确"
else bad "请求形状或 payload 校验失败"; fi

# ============================================================
echo "== 与 A1 check_l4_sni_listen.sh --metrics 输出兼容性（冻结契约） =="

CONF="$FAKE_DIR/healthy.conf"
cat > "$CONF" <<'NGINX'
# healthy fixture: loopback:10443 + ssl + standalone proxy_protocol
server {
    listen 127.0.0.1:10443 ssl proxy_protocol;
    server_name imboy.example.com;
    ssl_certificate     /etc/nginx/certs/imboy.example.com.pem;
    ssl_certificate_key /etc/nginx/certs/imboy.example.com.key;
    location / {
        proxy_pass http://127.0.0.1:8080;
    }
}
NGINX

run_a1_metrics() {
  bash "${ROOT}/scripts/check_l4_sni_listen.sh" --metrics "$CONF" \
    >"$FAKE_DIR/a1.out" 2>"$FAKE_DIR/a1.err"
}

# A1b 可能正并行微调该脚本：首次异常先重试一次再定性
if ! run_a1_metrics; then
  echo "  （A1 --metrics 首次运行 rc 非零，等 2s 重试一次）"
  sleep 2
  run_a1_metrics
fi
A1_RC=$?

if [ "$A1_RC" -eq 0 ]; then
  ok "A1 --metrics 健康配置 rc=0"
else
  bad "A1 --metrics 健康配置 rc=$A1_RC（并行微调或契约破坏，stderr 如下）"
  sed 's/^/    /' "$FAKE_DIR/a1.err"
fi

A1_OUT="$(cat "$FAKE_DIR/a1.out")"
contains "含冻结指标 imboy_l4_sni_listen_drift" "$A1_OUT" "imboy_l4_sni_listen_drift"
contains "含冻结指标 imboy_l4_sni_check_success" "$A1_OUT" "imboy_l4_sni_check_success"
contains "含冻结指标 imboy_l4_sni_last_check_timestamp_seconds" "$A1_OUT" "imboy_l4_sni_last_check_timestamp_seconds"

if printf '%s\n' "$A1_OUT" | grep -Eq 'imboy_l4_sni_listen_drift\{[^}]*\} 0$'; then
  ok "健康配置 drift 样本值为 0"
else
  bad "健康配置 drift 样本应为 0"
fi
if printf '%s\n' "$A1_OUT" | grep -q '^imboy_l4_sni_check_success 1$'; then
  ok "健康配置 check_success=1"
else
  bad "健康配置 check_success 应为 1"
fi

# Pushgateway/Prometheus 文本格式语法校验：注释行仅允许 # TYPE/# HELP，
# 样本行必须形如 name{labels} value；# EOF 可选（Pushgateway 不要求）
if printf '%s\n' "$A1_OUT" | python3 -c '
import re, sys

text = sys.stdin.read()
sample = re.compile(
    r"^([a-zA-Z_:][a-zA-Z0-9_:]*)(\{.*\})?"
    r" (NaN|[+-]?Inf|-?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?)$")
types, names = {}, set()
for line in text.strip().splitlines():
    if not line.strip():
        continue
    if line.startswith("#"):
        assert re.match(r"^# (TYPE|HELP) ", line), "unknown comment: " + line
        if line.startswith("# TYPE "):
            m = re.match(r"^# TYPE ([a-zA-Z_:][a-zA-Z0-9_:]*) ([a-zA-Z_]+)$", line)
            assert m, "bad TYPE line: " + line
            types[m.group(1)] = m.group(2)
        continue
    m = sample.match(line)
    assert m, "bad sample line: " + line
    names.add(m.group(1))
for n in types:
    assert n in names, "TYPE without sample: " + n
required = {"imboy_l4_sni_listen_drift",
            "imboy_l4_sni_check_success",
            "imboy_l4_sni_last_check_timestamp_seconds"}
missing = required - names
assert not missing, "missing frozen metrics: %s" % sorted(missing)
print("prometheus-text OK")
'; then
  ok "A1 --metrics 输出符合 Pushgateway 文本格式（python 解析通过）"
else
  bad "A1 --metrics 输出未通过文本格式校验"
fi

# ============================================================
echo
echo "通过 ${PASS}，失败 ${FAIL}"
[ "$FAIL" -eq 0 ]
