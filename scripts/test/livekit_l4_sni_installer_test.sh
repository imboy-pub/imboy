#!/usr/bin/env bash
# install-livekit-l4-sni.sh 端到端沙箱测试（Task 3：宝塔 nginx 实例兼容 + 回滚行为）。
#
# 完全离线自包含：fixture（scripts/test/fixtures/l4_sni_installer_sandbox/）通过
# PATH 注入 fake nginx×2 / systemctl / docker / ss / ps / curl / openssl / install /
# stat / find / sha256sum / haproxy / id，绝不触碰真实 nginx、docker、systemd 或网络。
# "运行中的 nginx master" 是一个忽略 SIGHUP 的真实 sleep 进程，fake ps 按状态表
# 报告它，安装器的存活检查与 kill -HUP 走真实系统调用。
#
# 用例覆盖（对应计划 Task 3 验收）：
#   T1  --check 只读通过（实例 pinning 打印 + pre-switch 检测，无任何落盘副作用）
#   T2  显式 NGINX_BIN 不可执行 → 立即失败，绝不静默回落
#   T3  自动探测选中错误实例（PATH nginx=发行版，master=宝塔）→ BLOCKED_ENV 失败
#   T4  候选配置 nginx -t 失败（安装阶段）→ 失败且自动回滚（首次切换失败状态）
#   T5  reload 失败（-s reload 非零）→ 不进入"已切换"，自动回滚恢复完整 Compose 组合
#   T6  完整 apply 成功：同一实例贯穿 preflight→backup→install→verify（含 -c 参数
#       一致、无 systemctl reload nginx、strict 检测在 reload 前后各一次）、备份格式
#       （manifest+每文件 sha256+服务状态快照）、staged 渲染断言（10443/proxy_protocol
#       /32 信任/无 HAProxy 裸 TCP check）
#   T8  显式 --rollback 成功：恢复实际 Compose 组合（base+overlay，无 generated）、
#       有效配置哈希与切换前一致、eturnal 服务状态恢复
#   T8b 备份被篡改 → 校验和失败拒绝恢复
#   T9  安装器外已启用的 SNI 再 --apply → 拒绝重复接管（保护保留）
#   T11 Task-1 strict 检测失败 → reload 前阻断并回滚
#   T12 Task-1 检测脚本缺失 → BLOCKED_ENV fail-closed
#   T13 自动探测选中正确实例（PATH=宝塔）
#   T14 pidfile 与 master 不符 → 直接 SIGHUP 定向 reload（同一实例兜底）
#   T15 -V 缺 stream/ssl_preread 模块 → 拒绝
set -Eeuo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIXTURE="$ROOT/scripts/test/fixtures/l4_sni_installer_sandbox"
INSTALLER="$ROOT/deploy/install-livekit-l4-sni.sh"

command -v python3 >/dev/null 2>&1 || { echo "BLOCKED_ENV: python3 不可用（安装器与 stub 均依赖）"; exit 4; }
[ -f "$INSTALLER" ] || { echo "BLOCKED_ENV: 缺 $INSTALLER"; exit 4; }
[ -d "$FIXTURE" ] || { echo "BLOCKED_ENV: 缺 fixture 目录 $FIXTURE"; exit 4; }

WORK="$(mktemp -d /tmp/imboy_l4sni.XXXXXX)"
MASTER_PIDS=()
cleanup() {
  local pid
  for pid in ${MASTER_PIDS[@]+"${MASTER_PIDS[@]}"}; do
    kill "$pid" 2>/dev/null || true
    pkill -TERM -P "$pid" 2>/dev/null || true
  done
  if [ "${L4SNI_TEST_KEEP:-0}" = 1 ] && [ "${FAIL:-0}" -gt 0 ]; then
    echo "KEEP: 失败现场保留在 $WORK（L4SNI_TEST_KEEP=1）"
    return
  fi
  rm -rf "$WORK"
}
trap cleanup EXIT

PASS=0
FAIL=0
ok()  { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }
assert_rc() {
  local want="$1" got="$2" desc="$3"
  if [ "$want" = "$got" ]; then ok "$desc (rc=$got)"; else bad "$desc" "exit=$got want=$want tail=$(tail -2 "$STATE/out.log" 2>/dev/null | tr '\n' '_')"; fi
}
assert_grep() {
  local desc="$1" pat="$2" file="$3"
  if grep -qE "$pat" "$file" 2>/dev/null; then ok "$desc"; else bad "$desc" "pattern=$pat 未命中 file=$file"; fi
}
assert_grep_f() {
  local desc="$1" str="$2" file="$3"
  if grep -qF "$str" "$file" 2>/dev/null; then ok "$desc"; else bad "$desc" "string=$str 未命中 file=$file"; fi
}
assert_ngrep() {
  local desc="$1" pat="$2" file="$3"
  if grep -qE "$pat" "$file" 2>/dev/null; then bad "$desc" "不应命中 pattern=$pat"; else ok "$desc"; fi
}
assert_file() { local desc="$1" path="$2"; if [ -e "$path" ]; then ok "$desc"; else bad "$desc" "缺失 $path"; fi; }
assert_no_file() { local desc="$1" path="$2"; if [ ! -e "$path" ]; then ok "$desc"; else bad "$desc" "不应存在 $path"; fi; }
assert_eq() {
  local desc="$1" got="$2" want="$3"
  if [ "$got" = "$want" ]; then ok "$desc"; else bad "$desc" "got='$got' want='$want'"; fi
}
line_of() { grep -nE "$1" "$2" 2>/dev/null | head -1 | cut -d: -f1; }

# ── 沙箱搭建 ────────────────────────────────────────────────────────────────
# 产物：SB（fixture 副本，含两个 nginx 实例/compose/证书）、STATE（stub 状态与
# calls.log/out.log）、CFG（安装器配置）、MASTER_PID（忽略 SIGHUP 的 sleep）。
new_sandbox() {
  local name="$1" inst mpid wpid wpid2
  mkdir -p "$WORK/$name"
  SB="$WORK/$name/sb"
  cp -R "$FIXTURE" "$SB"
  STATE="$WORK/$name/state"
  CTL="$STATE/ctl"
  mkdir -p "$CTL"
  for inst in nginx-bt nginx-distro; do
    sed "s|__HOME__|$SB/$inst|g" "$SB/$inst/conf/nginx.conf.template" >"$SB/$inst/conf/nginx.conf"
    mkdir -p "$SB/$inst/run" "$SB/$inst/logs"
  done
  chmod +x "$SB/bin/"* "$SB/nginx-bt/sbin/nginx" "$SB/nginx-distro/sbin/nginx"
  mkdir -p "$SB/compose" "$SB/haproxy" "$SB/hooks" "$SB/state-lib" "$SB/backups" "$SB/turn-certs"
  # 预置"haproxy 包已安装"的世界（默认配置不含 15442 bind，restart 不会点亮它）
  printf 'global\n    daemon\n\ndefaults\n    mode tcp\n' >"$SB/haproxy/haproxy.cfg"
  printf 'services:\n  imboy_livekit:\n    image: sandbox/livekit\n' >"$SB/compose/base.yml"
  printf '# operator overlay (sandbox)\nservices:\n  imboy_livekit:\n    cpus: 1\n' >"$SB/compose/operator-overlay.yml"
  printf 'SANDBOX_ONLY=1\n' >"$SB/compose/livekit.env"
  printf 'sandbox fake cert\n' >"$SB/turn-certs/fullchain.pem"
  printf 'sandbox fake key\n'  >"$SB/turn-certs/privkey.pem"
  # "运行中的 nginx master"：真实进程。收到 SIGHUP（安装器兜底 reload 路径）时
  # 立即登记切换后世界的 10443 监听行（worker 4102），模拟 master 重载生效后存活。
  # 用 python 而非 bash trap：bash 会把 HUP trap 推迟到前台 sleep 结束，python 即刻处理。
  L4_SB_STATE="$STATE" python3 -c '
import os, signal, time
state = os.environ["L4_SB_STATE"]
def on_hup(signum, frame):
    with open(state + "/ctl/ss-listeners", "a") as fh:
        fh.write("t 10443 4102 nginx\n")
signal.signal(signal.SIGHUP, on_hup)
while True:
    time.sleep(600)
' &
  mpid=$!
  MASTER_PIDS+=("$mpid")
  wpid=$((mpid + 1000))
  {
    echo "$mpid 1 nginx: master process $SB/nginx-bt/sbin/nginx"
    echo "$wpid $mpid nginx: worker process"
    echo "4102 $mpid nginx: worker process"
  } >"$CTL/ps-table"
  echo "$mpid" >"$SB/nginx-bt/run/nginx.pid"
  echo "t 443 $wpid nginx" >"$CTL/ss-listeners"
  echo 4102 >"$CTL/nginx-worker-pid"
  printf 'eturnal=active\nhaproxy=active\n' >"$CTL/systemctl-active"
  printf 'eturnal=enabled\nhaproxy=enabled\n' >"$CTL/systemctl-enabled"
  printf 'turn.example.com\n' >"$CTL/turn-domain"
  printf 'http://127.0.0.1:7880/ OK\nhttp://127.0.0.1:9800/healthz {"status":"ok"}\n' >"$CTL/curl-ok-urls"
  printf 'api.example.com 200\ndual.example.com 404\n' >"$CTL/curl-codes"
  : >"$STATE/calls.log"
  MASTER_PID="$mpid"
  CFG="$WORK/$name/config.env"
  PATH_EXTRA=""
  CFG_NGINX_MODE="explicit"
  CFG_CHECK_SCRIPT=""
  write_config
}

write_config() {
  {
    echo 'TURN_DOMAIN=turn.example.com'
    case "${CFG_NGINX_MODE:-explicit}" in
      explicit) echo "NGINX_BIN=$SB/nginx-bt/sbin/nginx" ;;
      omit)     : ;;
      bogus)    echo "NGINX_BIN=$SB/no-such-nginx" ;;
    esac
    echo "NGINX_VHOST_DIR=$SB/nginx-bt/conf/vhosts"
    echo "NGINX_STREAM_CONF=$SB/nginx-bt/conf/tcp/livekit-turn-l4-sni.conf"
    echo "NGINX_REALIP_CONF=$SB/nginx-bt/conf/0.realip.conf"
    echo 'HTTPS_VHOST_FILES="api.conf dual.conf"'
    echo 'HTTPS_HEALTH_CONTRACTS="api.example.com:200 dual.example.com:404"'
    echo "COMPOSE_DIR=$SB/compose"
    echo "COMPOSE_FILE=$SB/compose/base.yml"
    echo 'COMPOSE_OVERLAY_FILES=operator-overlay.yml'
    echo "COMPOSE_ENV_FILE=$SB/compose/livekit.env"
    echo 'LIVEKIT_SERVICE=imboy_livekit'
    echo 'LIVEKIT_CONTAINER=imboy_livekit'
    echo 'LIVEKIT_HEALTH_URL=http://127.0.0.1:7880/'
    echo 'BACKEND_HEALTH_URL=http://127.0.0.1:9800/healthz'
    echo "LIVEKIT_TURN_CERT_DIR=$SB/turn-certs"
    echo "CERTBOT_HOOK=$SB/hooks/livekit-turn-cert.sh"
    echo 'ETURNAL_SERVICE=eturnal'
    echo "L4_STATE_DIR=$SB/state-lib"
    echo "L4_BACKUP_ROOT=$SB/backups"
    echo "HAPROXY_CONF=$SB/haproxy/haproxy.cfg"
    echo "L4_SNI_CHECK_SCRIPT=${CFG_CHECK_SCRIPT:-$SB/bin/check-stub}"
  } >"$CFG"
  chmod 600 "$CFG"
}

run_installer() {
  local rc=0 path_extra="${PATH_EXTRA:-}"
  env PATH="$SB/bin${path_extra:+:$path_extra}:$PATH" \
      L4_SB_STATE="$STATE" \
      L4_SB_HAPROXY_CONF="$SB/haproxy/haproxy.cfg" \
      bash "$INSTALLER" "$@" >"$STATE/out.log" 2>&1 || rc=$?
  RC="$rc"
}

latest_backup() { ls -1 "$SB/backups" 2>/dev/null | head -1; }

echo "== T1 --check 只读通过 =="
new_sandbox t1
run_installer --check --config "$CFG"
assert_rc 0 "$RC" "T1 --check 成功"
assert_grep "T1 实例 pinning 身份打印（bin/master_pid/conf）" 'nginx instance pinned: bin=[^ ]+nginx-bt/sbin/nginx real=[^ ]+ master_pid=[0-9]+ .+conf=' "$STATE/out.log"
assert_grep "T1 CHECK_PASS" 'CHECK_PASS' "$STATE/out.log"
assert_grep "T1 pre-switch 检测被调用（只读）" 'check mode=--pre-switch' "$STATE/calls.log"
assert_eq "T1 --check 无备份产生" "$(ls -A "$SB/backups" 2>/dev/null | wc -l | tr -d ' ')" "0"
assert_no_file "T1 未产生 Compose 重建" "$CTL/compose-last-up"
assert_grep_f "T1 vhost 未被改写" "listen 443 ssl http2" "$SB/nginx-bt/conf/vhosts/api.conf"

echo "== T2 显式 NGINX_BIN 不可执行必须失败 =="
new_sandbox t2
CFG_NGINX_MODE="bogus"; write_config
run_installer --check --config "$CFG"
assert_rc 1 "$RC" "T2 显式不可执行 → 拒绝"
assert_grep "T2 明确诊断（不静默回落）" 'NGINX_BIN is not executable' "$STATE/out.log"
assert_eq "T2 未执行任何 nginx 操作" "$(grep -c '^nginx:' "$STATE/calls.log" 2>/dev/null || true)" "0"

echo "== T3 自动探测选中错误实例 → BLOCKED_ENV =="
new_sandbox t3
CFG_NGINX_MODE="omit"; write_config
PATH_EXTRA="$SB/nginx-distro/sbin"
run_installer --check --config "$CFG"
assert_rc 1 "$RC" "T3 PATH nginx=发行版而 master=宝塔 → 拒绝"
assert_grep "T3 BLOCKED_ENV 语义" 'BLOCKED_ENV: no candidate nginx binary matches a running master' "$STATE/out.log"
assert_grep_f "T3 报告正在运行的 master（含宝塔实例路径）" "$SB/nginx-bt/sbin/nginx" "$STATE/out.log"
assert_eq "T3 未执行任何 nginx 操作" "$(grep -c '^nginx:' "$STATE/calls.log" 2>/dev/null || true)" "0"

echo "== T4 候选配置 nginx -t 失败 → 失败并自动回滚 =="
new_sandbox t4
echo 2 >"$CTL/nginx-fail-t-on"
run_installer --apply --config "$CFG"
assert_rc 1 "$RC" "T4 安装阶段 -t 失败 → apply 失败"
assert_grep "T4 候选配置 -t 失败诊断" 'candidate nginx config failed -t' "$STATE/out.log"
assert_grep "T4 自动回滚完成" 'ROLLBACK_COMPLETE' "$STATE/out.log"
assert_ngrep "T4 未进入已切换状态" 'VERIFY_PASS|APPLY_PASS' "$STATE/out.log"
assert_no_file "T4 未写 active-backup" "$SB/state-lib/active-backup"
assert_no_file "T4 generated overlay 已移除" "$SB/compose/livekit-turn-l4-sni.generated.yml"
assert_grep_f "T4 vhost 恢复原 443 直连" "listen 443 ssl http2" "$SB/nginx-bt/conf/vhosts/api.conf"
assert_eq "T4 回滚重建使用完整组合（base+overlay，无 generated）" "$(cat "$CTL/compose-last-up" 2>/dev/null)" "$SB/compose/base.yml $SB/compose/operator-overlay.yml"

echo "== T5 reload 失败 → 不进入已切换并回滚 =="
new_sandbox t5
echo 1 >"$CTL/nginx-fail-reload-on"
run_installer --apply --config "$CFG"
assert_rc 1 "$RC" "T5 -s reload 非零 → apply 失败"
assert_grep "T5 reload 失败诊断（同一实例）" 'nginx -s reload failed on pinned instance' "$STATE/out.log"
assert_grep "T5 自动回滚完成" 'ROLLBACK_COMPLETE' "$STATE/out.log"
assert_ngrep "T5 未进入已切换状态" 'VERIFY_PASS|APPLY_PASS' "$STATE/out.log"
assert_no_file "T5 未写 active-backup" "$SB/state-lib/active-backup"
assert_eq "T5 回滚重建使用完整组合（base+overlay）" "$(cat "$CTL/compose-last-up" 2>/dev/null)" "$SB/compose/base.yml $SB/compose/operator-overlay.yml"
assert_no_file "T5 generated overlay 已移除" "$SB/compose/livekit-turn-l4-sni.generated.yml"

echo "== T6 完整 apply 成功：同一实例贯穿 + 备份格式 + 渲染断言 =="
new_sandbox t6
run_installer --apply --config "$CFG"
assert_rc 0 "$RC" "T6 apply 成功"
assert_grep "T6 APPLY_PASS" 'APPLY_PASS' "$STATE/out.log"
assert_grep "T6 VERIFY_PASS" 'VERIFY_PASS' "$STATE/out.log"
BK="$SB/backups/$(latest_backup)"
assert_file "T6 备份 manifest.txt 存在" "$BK/manifest.txt"
assert_file "T6 备份 service-state.txt 存在" "$BK/service-state.txt"
assert_file "T6 备份文件库 files/ 存在" "$BK/files"
assert_file "T6 nginx -T 导出快照存在" "$BK/nginx-T.txt"
assert_file "T6 livekit inspect 快照存在" "$BK/livekit-inspect.json"
assert_file "T6 Compose 有效配置切换前快照存在" "$BK/compose-effective-pre.txt"
assert_grep "T6 manifest 含每文件 sha256 行" '^file [0-9a-f]{64} [0-7]{3,4} /' "$BK/manifest.txt"
assert_grep "T6 服务状态含 eturnal.enabled" 'eturnal.enabled=enabled' "$BK/service-state.txt"
assert_grep "T6 服务状态含 eturnal.active" 'eturnal.active=active' "$BK/service-state.txt"
assert_grep "T6 服务状态含 nginx 实例身份" "nginx.bin=.*nginx-bt/sbin/nginx" "$BK/service-state.txt"
assert_grep "T6 服务状态含 compose.base" "compose.base=$SB/compose/base.yml" "$BK/service-state.txt"
assert_grep "T6 服务状态含 overlay 组合记录" "compose.overlay=$SB/compose/operator-overlay.yml" "$BK/service-state.txt"
assert_grep "T6 generated overlay 记录为切换前不存在" 'compose.generated_pre_existing=0' "$BK/service-state.txt"
assert_grep_f "T6 staged stream.conf 路由 TURN 域" "turn.example.com 127.0.0.1:15442" "$BK/staged/stream.conf"
assert_grep_f "T6 staged compose 信任唯一 /32" "172.29.0.1/32" "$BK/staged/compose.yml"
assert_grep_f "T6 staged compose 发布 127.0.0.1:15443:443" "127.0.0.1:15443:443" "$BK/staged/compose.yml"
assert_grep_f "T6 vhost 改写为 10443+proxy_protocol" "listen 127.0.0.1:10443 ssl http2 proxy_protocol;" "$BK/staged/vhosts/api.conf"
assert_grep_f "T6 IPv6 vhost 同步改写" "listen [::1]:10443 ssl proxy_protocol;" "$BK/staged/vhosts/dual.conf"
assert_ngrep "T6 HAProxy 模板无裸 TCP check" '^\s*server livekit .* check\s*$' "$BK/staged/haproxy.cfg"
assert_grep_f "T6 磁盘 vhost 已改写" "listen 127.0.0.1:10443 ssl http2 proxy_protocol;" "$SB/nginx-bt/conf/vhosts/api.conf"
assert_file "T6 certbot hook 已安装" "$SB/hooks/livekit-turn-cert.sh"
assert_grep_f "T6 active-backup 指向备份目录" "$BK" "$SB/state-lib/active-backup"
assert_eq "T6 切换重建使用 base+overlay+generated 完整组合" "$(cat "$CTL/compose-last-up" 2>/dev/null)" "$SB/compose/base.yml $SB/compose/operator-overlay.yml $SB/compose/livekit-turn-l4-sni.generated.yml"
total_nginx="$(grep -c '^nginx:' "$STATE/calls.log" || echo 0)"
bt_nginx="$(grep -c '^nginx:nginx-bt ' "$STATE/calls.log" || echo 0)"
assert_eq "T6 全部 nginx 操作走选定宝塔实例（无发行版实例混入）" "$bt_nginx" "$total_nginx"
other_conf="$(grep '^nginx:nginx-bt ' "$STATE/calls.log" | grep -- ' -c ' | grep -cvF -- "-c $SB/nginx-bt/conf/nginx.conf" || true)"
assert_eq "T6 全部带 -c 的 nginx 操作使用同一 pinned 配置路径" "${other_conf:-0}" "0"
assert_ngrep "T6 不经 systemd reload nginx" '^systemctl reload nginx' "$STATE/calls.log"
strict_nums="$(grep -n 'check mode=--strict' "$STATE/calls.log" | cut -d: -f1 | tr '\n' ' ' || true)"
reload_line="$(line_of 'nginx:nginx-bt .* -s reload' "$STATE/calls.log")"
first_strict="$(printf '%s\n' $strict_nums | head -1)"
second_strict="$(printf '%s\n' $strict_nums | sed -n 2p)"
if [ -n "$first_strict" ] && [ -n "$second_strict" ] && [ -n "$reload_line" ] \
   && [ "$first_strict" -lt "$reload_line" ] && [ "$reload_line" -lt "$second_strict" ]; then
  ok "T6 strict 检测闭环：reload 前(strict) < reload < reload 后(strict)"
else
  bad "T6 strict 检测闭环" "strict=[$strict_nums] reload=$reload_line"
fi
assert_grep "T6 reload 经 pidfile 校验走 -s reload" 'reloaded via pinned binary -s reload' "$STATE/out.log"

echo "== T8 显式 --rollback 成功：恢复实际 Compose 组合 =="
new_sandbox t8
run_installer --apply --config "$CFG"
assert_rc 0 "$RC" "T8 前置 apply 成功"
BK="$SB/backups/$(latest_backup)"
run_installer --rollback --config "$CFG"
assert_rc 0 "$RC" "T8 --rollback 成功"
assert_grep "T8 ROLLBACK_COMPLETE" 'ROLLBACK_COMPLETE' "$STATE/out.log"
assert_eq "T8 回滚重建组合= base+overlay（绝不止 base）" "$(cat "$CTL/compose-last-up" 2>/dev/null)" "$SB/compose/base.yml $SB/compose/operator-overlay.yml"
assert_grep "T8 有效配置与切换前一致（哈希闭环）" 'matches the pre-switch effective configuration' "$STATE/out.log"
assert_grep_f "T8 vhost 恢复原 443 直连" "listen 443 ssl http2" "$SB/nginx-bt/conf/vhosts/api.conf"
assert_no_file "T8 generated overlay 已移除" "$SB/compose/livekit-turn-l4-sni.generated.yml"
assert_no_file "T8 stream conf 已移除" "$SB/nginx-bt/conf/tcp/livekit-turn-l4-sni.conf"
assert_no_file "T8 active-backup 指针清除" "$SB/state-lib/active-backup"
assert_grep_f "T8 eturnal active 状态恢复" "eturnal=active" "$CTL/systemctl-active"
assert_grep_f "T8 eturnal enabled 状态恢复" "eturnal=enabled" "$CTL/systemctl-enabled"
assert_grep "T8 回滚后 pre-switch 检测门（restore 阶段）" 'check mode=--pre-switch' "$STATE/calls.log"

echo "== T8b 备份被篡改 → 拒绝恢复 =="
new_sandbox t8b
run_installer --apply --config "$CFG"
assert_rc 0 "$RC" "T8b 前置 apply 成功"
BK="$SB/backups/$(latest_backup)"
first_path="$(awk '$1 == "file" { print $4; exit }' "$BK/manifest.txt")"
printf 'tamper\n' >>"$BK/files/${first_path#/}"
run_installer --rollback --config "$CFG" --backup "$BK"
assert_rc 1 "$RC" "T8b 校验和不符 → 拒绝恢复"
assert_grep "T8b 每文件校验和诊断" 'backup per-file checksum mismatch' "$STATE/out.log"
assert_eq "T8b 拒绝恢复后未新增重建（仅 apply 阶段一次）" "$(grep -ac 'docker compose up' "$STATE/calls.log" 2>/dev/null || true)" "1"

echo "== T9 拒绝对安装器外已启用的 SNI 重复 apply =="
new_sandbox t9
printf 'eturnal=inactive\nhaproxy=active\n' >"$CTL/systemctl-active"
{ cat "$CTL/ss-listeners"; printf 't 15442 4301 haproxy\nt 15443 4401 docker-proxy\n'; } >"$CTL/ss-listeners.tmp"
mv "$CTL/ss-listeners.tmp" "$CTL/ss-listeners"
run_installer --apply --config "$CFG"
assert_rc 1 "$RC" "T9 重复接管被拒（保护保留）"
assert_grep "T9 明确诊断" 'refusing a duplicate apply' "$STATE/out.log"
assert_eq "T9 未发生任何 Compose 重建" "$(grep -c 'compose up' "$STATE/calls.log" 2>/dev/null || true)" "0"
assert_eq "T9 未创建备份" "$(ls -A "$SB/backups" 2>/dev/null | wc -l | tr -d ' ')" "0"
assert_eq "T9 未 reload nginx" "$(grep -c ' -s reload' "$STATE/calls.log" 2>/dev/null || true)" "0"

echo "== T11 Task-1 strict 检测失败 → reload 前阻断并回滚 =="
new_sandbox t11
echo 1 >"$CTL/check-strict-rc"
run_installer --apply --config "$CFG"
assert_rc 1 "$RC" "T11 strict 检测失败 → apply 失败"
assert_grep "T11 检测失败诊断" 'listen-drift check failed \(mode=--strict rc=1\)' "$STATE/out.log"
assert_grep "T11 自动回滚完成" 'ROLLBACK_COMPLETE' "$STATE/out.log"
first_reload="$(line_of ' -s reload' "$STATE/calls.log")"
first_strict="$(line_of 'check mode=--strict' "$STATE/calls.log")"
if [ -n "$first_reload" ] && [ -n "$first_strict" ] && [ "$first_strict" -lt "$first_reload" ]; then
  ok "T11 失败的 strict 检测发生在任何 reload 之前"
else
  bad "T11 失败的 strict 检测发生在任何 reload 之前" "strict=$first_strict reload=$first_reload"
fi
assert_ngrep "T11 未进入已切换状态" 'VERIFY_PASS|APPLY_PASS' "$STATE/out.log"

echo "== T12 Task-1 检测脚本缺失 → BLOCKED_ENV fail-closed =="
new_sandbox t12
CFG_CHECK_SCRIPT="$SB/missing-check.sh"; write_config
run_installer --check --config "$CFG"
assert_rc 1 "$RC" "T12 检测脚本缺失 → 拒绝"
assert_grep "T12 fail-closed 诊断" 'BLOCKED_ENV: listen-drift checker not found' "$STATE/out.log"

echo "== T13 自动探测选中正确实例（PATH=宝塔）=="
new_sandbox t13
CFG_NGINX_MODE="omit"; write_config
PATH_EXTRA="$SB/nginx-bt/sbin"
run_installer --check --config "$CFG"
assert_rc 0 "$RC" "T13 自动探测成功"
assert_grep "T13 pin 到宝塔实例" 'nginx instance pinned: bin=[^ ]+nginx-bt/sbin/nginx' "$STATE/out.log"

echo "== T14 pidfile 不匹配 → 直接 SIGHUP 定向 reload =="
new_sandbox t14
echo 999999 >"$SB/nginx-bt/run/nginx.pid"
run_installer --apply --config "$CFG"
assert_rc 0 "$RC" "T14 pidfile 失配仍 apply 成功"
assert_grep "T14 兜底为对 pinned master 的 SIGHUP" 'direct SIGHUP to pinned master' "$STATE/out.log"
assert_grep "T14 VERIFY_PASS" 'VERIFY_PASS' "$STATE/out.log"

echo "== T15 -V 缺 stream/ssl_preread 模块 → 拒绝 =="
new_sandbox t15
: >"$CTL/nginx-V-no-modules"
run_installer --check --config "$CFG"
assert_rc 1 "$RC" "T15 模块缺失 → 拒绝"
assert_grep "T15 模块诊断" 'lacks stream module' "$STATE/out.log"

echo
echo "== 结果：PASS=${PASS} FAIL=${FAIL} =="
[ "$FAIL" -eq 0 ]
