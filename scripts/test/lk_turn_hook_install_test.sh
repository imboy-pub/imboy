#!/usr/bin/env bash
# LK-DEP-01 TURN 证书 hook 单测 + 两域 vhost nginx -t + 安装器故障注入（离线测试）。
#
# 覆盖验收 A03（hook 只动目标、失败不留半配置）、A04（证书签发失败 / LiveKit
# start 失败 / hook 各故障路径的退出码与清理逻辑）、A05 的 nginx -t 部分。
# docker / certbot 全部本地桩，不触网不占端口；openssl 与 nginx 用真二进制。
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

REPO="$(pwd)"
TMP_ROOT="$(mktemp -d /tmp/imboy_lk_hook.XXXXXX)"
cleanup() { rm -rf -- "$TMP_ROOT"; }
trap cleanup EXIT

PASS=0
FAIL=0
ok()  { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }
assert_rc() {
  local want="$1" got="$2" desc="$3" log="${4:-$TMP_ROOT/install.log}"
  if [ "$got" = "$want" ]; then ok "$desc"; else bad "$desc" "exit=$got want=$want tail=$(tail -3 "$log" 2>/dev/null | tr '\n' '_')"; fi
}
assert_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then ok "$desc"; else bad "$desc" "pattern=$pattern 未命中"; fi
}
assert_not_grep() {
  local desc="$1" pattern="$2" file="$3"
  if grep -qE "$pattern" "$file" 2>/dev/null; then bad "$desc" "不应命中 pattern=$pattern"; else ok "$desc"; fi
}
assert_file_absent() {
  if [ -e "$2" ]; then bad "$1" "$2 仍存在"; else ok "$1"; fi
}
assert_file_present() {
  if [ -s "$2" ]; then ok "$1"; else bad "$1" "$2 缺失或为空"; fi
}

HOOK="deploy/nginx/livekit-turn-cert-deploy-hook.sh"
TURN_DOM="turn.test.invalid"

# ── fixture：真自签证书（SAN 含/不含 TURN 域两套）+ mock docker ───────────────
mkcert() { # mkcert <dir> <cn> [keydir2]：生成 fullchain.pem/privkey.pem
  local dir="$1" cn="$2"
  mkdir -p "$dir"
  openssl req -x509 -nodes -newkey rsa:2048 -days 2 \
    -subj "/CN=$cn" -addext "subjectAltName=DNS:$cn" \
    -keyout "$dir/privkey.pem" -out "$dir/fullchain.pem" 2>/dev/null
}

LINEAGE_OK="$TMP_ROOT/letsencrypt/live/$TURN_DOM"
mkcert "$LINEAGE_OK" "$TURN_DOM"
LINEAGE_OTHER="$TMP_ROOT/letsencrypt/live/pro.test.invalid"
mkcert "$LINEAGE_OTHER" "pro.test.invalid"
LINEAGE_MISMATCH="$TMP_ROOT/letsencrypt-mm/live/$TURN_DOM"
mkcert "$LINEAGE_MISMATCH" "$TURN_DOM"
# 用另一把私钥制造配对失败（证书 A + 私钥 B）
openssl req -x509 -nodes -newkey rsa:2048 -days 2 -subj "/CN=x" \
  -keyout "$LINEAGE_MISMATCH/privkey.pem" -out "$TMP_ROOT/throwaway.crt" 2>/dev/null

MOCK_BIN="$TMP_ROOT/bin"
mkdir -p "$MOCK_BIN"
MOCK_LOG="$TMP_ROOT/docker-events.log"
cat > "$MOCK_BIN/docker" <<'MOCK'
#!/bin/sh
set -u
printf 'docker %s\n' "$*" >>"${MOCK_LOG:-/dev/null}"
case " $* " in
  *" inspect "*)
    printf '%s\n' "${MOCK_INSPECT_STATE-running}"   # 空串显式传入=容器不存在
    exit 0 ;;
  *" restart "*)
    [ "${MOCK_RESTART_RC:-0}" -eq 0 ] || exit 1
    exit 0 ;;
esac
exit 0
MOCK
chmod +x "$MOCK_BIN/docker"

run_hook() { # run_hook <lineage>；MOCK_RESTART_RC/MOCK_INSPECT_STATE 由调用方设置
  env PATH="$MOCK_BIN:$PATH" TURN_DOMAIN="$TURN_DOM" \
    LIVEKIT_TURN_CERT_TARGET="$TARGET_DIR" MOCK_LOG="$MOCK_LOG" \
    ${MOCK_RESTART_RC:+MOCK_RESTART_RC="$MOCK_RESTART_RC"} \
    ${MOCK_INSPECT_STATE+MOCK_INSPECT_STATE="$MOCK_INSPECT_STATE"} \
    bash "$HOOK" "$1" \
    >"$TMP_ROOT/hook.log" 2>&1
  echo $?
}

echo "== A03/A04 TURN 证书 hook 单测 =="

# 1) 非 TURN 域：跳过且零副作用（不动 nginx、不碰 docker）
TARGET_DIR="$TMP_ROOT/certs-skip"; : >"$MOCK_LOG"
RC="$(run_hook "$LINEAGE_OTHER")"
assert_rc 0 "$RC" "非 TURN 域 lineage → exit 0 跳过" "$TMP_ROOT/hook.log"
assert_file_absent "非 TURN 域不产生分发目录" "$TARGET_DIR/fullchain.pem"
if [ -s "$MOCK_LOG" ]; then bad "非 TURN 域不触发 docker" "$(cat "$MOCK_LOG")"; else ok "非 TURN 域不触发 docker"; fi

# 2) 坏证书内容：拒绝分发，目标目录零改动（失败不留半配置）
TARGET_DIR="$TMP_ROOT/certs-garbage"; mkdir -p "$TARGET_DIR"
printf 'old-cert-keep\n' > "$TARGET_DIR/fullchain.pem"
printf 'old-key-keep\n'  > "$TARGET_DIR/privkey.pem"
BAD_LINEAGE="$TMP_ROOT/letsencrypt-bad/live/$TURN_DOM"
mkdir -p "$BAD_LINEAGE"; printf 'not a cert\n' > "$BAD_LINEAGE/fullchain.pem"; printf 'k\n' > "$BAD_LINEAGE/privkey.pem"
RC="$(run_hook "$BAD_LINEAGE")"
[ "$RC" != 0 ] && ok "坏证书内容 → 非零退出" || bad "坏证书内容 → 非零退出" "exit=0"
assert_grep "坏证书报 X.509 解析失败" '无法解析为 X' "$TMP_ROOT/hook.log"
assert_grep "旧证书原样保留" 'old-cert-keep' "$TARGET_DIR/fullchain.pem"
assert_file_absent "无暂存残留" "$TARGET_DIR/.fullchain.pem.staged"

# 3) 证书与私钥不配对：拒绝分发
TARGET_DIR="$TMP_ROOT/certs-mismatch"
RC="$(run_hook "$LINEAGE_MISMATCH")"
[ "$RC" != 0 ] && ok "证书/私钥不配对 → 非零退出" || bad "证书/私钥不配对 → 非零退出" "exit=0"
assert_file_absent "不配对时不分发" "$TARGET_DIR/fullchain.pem"

# 4) 正常分发 + 容器在跑：restart 恰好一次
TARGET_DIR="$TMP_ROOT/certs-ok"; : >"$MOCK_LOG"
RC="$(run_hook "$LINEAGE_OK")"
assert_rc 0 "$RC" "正常续期 → exit 0" "$TMP_ROOT/hook.log"
assert_file_present "fullchain 已分发" "$TARGET_DIR/fullchain.pem"
assert_file_present "privkey 已分发" "$TARGET_DIR/privkey.pem"
assert_file_absent "分发后无暂存残留" "$TARGET_DIR/.fullchain.pem.staged"
assert_file_absent "分发后无私钥暂存残留" "$TARGET_DIR/.privkey.pem.staged"
RESTART_COUNT="$(grep -c 'docker restart imboy_livekit' "$MOCK_LOG" || true)"
[ "$RESTART_COUNT" = 1 ] && ok "容器重启恰好一次（且先 inspect）" \
  || bad "容器重启恰好一次（且先 inspect）" "count=$RESTART_COUNT log=$(cat "$MOCK_LOG" | tr '\n' ';')"
grep -q 'docker inspect' "$MOCK_LOG" && ok "restart 前有 docker inspect 状态确认" || bad "restart 前有 docker inspect 状态确认" "log=$(cat "$MOCK_LOG")"
# 分发的就是新证书（与 lineage 逐字节一致）
cmp -s "$LINEAGE_OK/fullchain.pem" "$TARGET_DIR/fullchain.pem" \
  && ok "分发内容与 lineage 一致" || bad "分发内容与 lineage 一致" "内容不匹配"

# 5) restart 失败：证书保留、非零退出（可人工重试恢复）
TARGET_DIR="$TMP_ROOT/certs-restart-fail"; : >"$MOCK_LOG"
RC="$(MOCK_RESTART_RC=1 run_hook "$LINEAGE_OK")"
[ "$RC" != 0 ] && ok "restart 失败 → 非零退出（certbot 日志留痕）" || bad "restart 失败 → 非零退出" "exit=0"
assert_file_present "restart 失败但证书已就位（重试 restart 即恢复）" "$TARGET_DIR/fullchain.pem"

# 6) 容器不存在：分发但跳过重启，exit 0
TARGET_DIR="$TMP_ROOT/certs-noc"; : >"$MOCK_LOG"
RC="$(MOCK_INSPECT_STATE= run_hook "$LINEAGE_OK")"
assert_rc 0 "$RC" "容器不存在 → exit 0（不动 certbot 状态）" "$TMP_ROOT/hook.log"
assert_file_present "容器不存在时证书仍分发" "$TARGET_DIR/fullchain.pem"
assert_not_grep "容器不存在时不 restart" 'docker restart' "$MOCK_LOG"

# 7) 目标就是 Certbot lineage：不得用 mv 覆盖 Certbot 管理的符号链接
ARCHIVE_SAME="$TMP_ROOT/letsencrypt/archive/$TURN_DOM"
LINEAGE_SAME="$TMP_ROOT/letsencrypt/live-same/$TURN_DOM"
mkcert "$ARCHIVE_SAME" "$TURN_DOM"
mkdir -p "$LINEAGE_SAME"
ln -s "$ARCHIVE_SAME/fullchain.pem" "$LINEAGE_SAME/fullchain.pem"
ln -s "$ARCHIVE_SAME/privkey.pem" "$LINEAGE_SAME/privkey.pem"
TARGET_DIR="$LINEAGE_SAME"; : >"$MOCK_LOG"
RC="$(run_hook "$LINEAGE_SAME")"
assert_rc 0 "$RC" "目标等于 lineage → exit 0" "$TMP_ROOT/hook.log"
if [ -L "$LINEAGE_SAME/fullchain.pem" ]; then
  ok "lineage fullchain 符号链接保持不变"
else
  bad "lineage fullchain 符号链接保持不变" "链接被覆盖"
fi
if [ -L "$LINEAGE_SAME/privkey.pem" ]; then
  ok "lineage privkey 符号链接保持不变"
else
  bad "lineage privkey 符号链接保持不变" "链接被覆盖"
fi
assert_grep "同目录模式明确跳过证书复制" '保留 Certbot 符号链接' "$TMP_ROOT/hook.log"
RESTART_COUNT="$(grep -c 'docker restart imboy_livekit' "$MOCK_LOG" || true)"
if [ "$RESTART_COUNT" = 1 ]; then
  ok "同目录模式仍重启容器一次"
else
  bad "同目录模式仍重启容器一次" "count=$RESTART_COUNT"
fi

echo "== A04/A05 两域 vhost nginx -t（真 nginx，BT include/日志路径桩替换） =="

NGX_TMP="$TMP_ROOT/nginx"; NGX_LOGS="$NGX_TMP/logs"; NGX_CONF="$NGX_TMP/conf.d"
mkdir -p "$NGX_LOGS" "$NGX_CONF" "$NGX_TMP/letsencrypt/live/rtc.imboy.pub"
mkcert "$NGX_TMP/letsencrypt/live/rtc.imboy.pub" "rtc.imboy.pub"
: > "$NGX_TMP/enable-php-00.conf"   # turn vhost 的宝塔相对 include 桩

prepare_vhost() { # prepare_vhost <src> <dst>：注释宝塔绝对 include、改写日志与证书路径
  sed -E -e 's|^([[:space:]]*include /www/server)|# \1|' \
          -e "s|/www/wwwlogs|$NGX_LOGS|g" \
          -e "s|/etc/letsencrypt|$NGX_TMP/letsencrypt|g" "$1" >"$2"
}
prepare_vhost deploy/nginx/prod-vhosts/rtc.imboy.pub.conf "$NGX_CONF/rtc.conf"
prepare_vhost deploy/nginx/prod-vhosts/turn.imboy.pub.conf "$NGX_CONF/turn.conf"

cat > "$NGX_TMP/wrapper.conf" <<WRAP
worker_processes 1;
error_log $NGX_LOGS/wrapper-error.log;
pid $NGX_TMP/wrapper.pid;
events { worker_connections 16; }
http {
  access_log off;
  map \$http_upgrade \$connection_upgrade { default upgrade; '' close; }
  include $NGX_CONF/*.conf;
}
WRAP

NGINX_BIN="$(command -v nginx || true)"
if [ -n "$NGINX_BIN" ]; then
  if "$NGINX_BIN" -t -c "$NGX_TMP/wrapper.conf" -p "$NGX_TMP/" 2>"$TMP_ROOT/nginx-t.log"; then
    ok "rtc + turn 两域 vhost 通过 nginx -t"
  else
    bad "rtc + turn 两域 vhost 通过 nginx -t" "$(tail -8 "$TMP_ROOT/nginx-t.log")"
  fi
else
  echo "  SKIP 本机无 nginx，跳过 nginx -t（CI 或有 nginx 的机器上运行）"
fi
# 合同断言：turn 域无 443（TURN_443=BLOCKED）；rtc 域有 WSS 反代特征
TURN_SRC="deploy/nginx/prod-vhosts/turn.imboy.pub.conf"
RTC_SRC="deploy/nginx/prod-vhosts/rtc.imboy.pub.conf"
assert_not_grep "turn vhost 不含 443 监听（TURN_443=BLOCKED）" 'listen 443' "$TURN_SRC"
assert_grep     "turn vhost 保留 ACME webroot location" 'location \^~ /\.well-known/acme-challenge/' "$TURN_SRC"
assert_grep     "turn vhost ACME 指向 certbot webroot" 'root /var/www/certbot' "$TURN_SRC"
assert_grep     "rtc vhost 反代 7880" 'proxy_pass http://127\.0\.0\.1:7880' "$RTC_SRC"
assert_grep     "rtc vhost 带 Upgrade 头" 'proxy_set_header Upgrade \$http_upgrade' "$RTC_SRC"
assert_grep     "rtc vhost 3600s 长超时" 'proxy_read_timeout 3600s' "$RTC_SRC"
assert_grep     "rtc vhost 关闭缓冲" 'proxy_buffering off' "$RTC_SRC"
assert_grep     "rtc vhost 有 ACME webroot" 'acme-challenge' "$RTC_SRC"

echo "== A04 安装器故障注入（证书失败/LiveKit start 失败/TURN 装配） =="

FIXTURE="$TMP_ROOT/install"; MOCK_BIN2="$TMP_ROOT/bin2"
mkdir -p "$FIXTURE/nginx" "$MOCK_BIN2" "$TMP_ROOT/scripts"
cp deploy/install.sh deploy/.env.example deploy/docker-compose.community.yml \
   deploy/docker-compose.livekit-turn.yml "$FIXTURE/"
cp deploy/nginx/init-letsencrypt.sh "$FIXTURE/nginx/"
touch "$TMP_ROOT/scripts/sanity_check.sh"
MOCK_LOG2="$TMP_ROOT/install-events.log"

cat > "$MOCK_BIN2/docker" <<'MOCK'
#!/bin/sh
set -u
printf 'docker %s\n' "$*" >>"${MOCK_LOG2:-/dev/null}"
case " $* " in
  *" compose version "*) exit 0 ;;
  *" info "*) exit 0 ;;
  *" network create "*) exit 0 ;;
  *" compose "*" pull --policy missing "*)
    [ "${MOCK_PULL_FAIL:-0}" != 1 ]; exit ;;
  *" compose "*" up -d"*)
    [ "${MOCK_UP_FAIL:-0}" != 1 ]; exit ;;
  *" compose "*" run "*" imboy_certbot "*)
    [ "${MOCK_CERT_FAIL:-0}" = 1 ] && exit 1
    previous=""; domain=""
    for arg in "$@"; do
      [ "$previous" = -d ] && domain="$arg"
      previous="$arg"
    done
    [ -n "$domain" ] || exit 2
    cert_dir="${DATA_DIR:-./data}/certbot/conf/live/$domain"
    mkdir -p "$cert_dir"
    printf 'issued fullchain\n' >"$cert_dir/fullchain.pem"
    printf 'issued private key\n' >"$cert_dir/privkey.pem"
    exit 0 ;;
  *" compose "*" images -q imboy_backend "*) echo test-backend-image-id; exit 0 ;;
  *" inspect --format "*) echo 'ghcr.io/imboy-pub/imboy-backend@sha256:aaaa'; exit 0 ;;
esac
exit 0
MOCK
# bash 桩：install.sh 本体真执行；preflight/sanity 记录即通过；
# init-letsencrypt 真执行（测证书故障路径）
cat > "$MOCK_BIN2/bash" <<'MOCK'
#!/bin/sh
set -u
printf 'bash COMPOSE_FILES=%s args=%s\n' "${COMPOSE_FILES:-}" "$*" >>"${MOCK_LOG2:-/dev/null}"
case "${1:-}" in
  *install.sh|*init-letsencrypt.sh) exec /bin/bash "$@" ;;
  *) exit 0 ;;
esac
MOCK
cat > "$MOCK_BIN2/curl" <<'MOCK'
#!/bin/sh
printf '{"status":"ok"}'
MOCK
chmod +x "$MOCK_BIN2/docker" "$MOCK_BIN2/bash" "$MOCK_BIN2/curl"

set_env2() {
  local key="$1" val="$2" tmp="$FIXTURE/.env.tmp"
  awk -v k="$key" -v v="$val" 'BEGIN{FS=OFS="="} $1==k{print k"="v; found=1; next} {print} END{if(!found) print k"="v}' \
    "$FIXTURE/.env" >"$tmp" && mv "$tmp" "$FIXTURE/.env"
}
reset_env() {
  cp deploy/.env.example "$FIXTURE/.env"
  for kv in \
    "API_DOMAIN=api.test.invalid" "ADMIN_DOMAIN=admin.test.invalid" \
    "CS_WIDGET_DOMAIN=cs.test.invalid" "RTC_DOMAIN=rtc.test.invalid" \
    "TURN_DOMAIN=turn.test.invalid" "CERTBOT_EMAIL=certbot@test.invalid" \
    "IMBOY_PAYMENT_GATEWAY_ENABLED=false"; do
    set_env2 "${kv%%=*}" "${kv#*=}"
  done
}
run_install() {
  (cd "$FIXTURE" && env PATH="$MOCK_BIN2:$PATH" MOCK_LOG2="$MOCK_LOG2" \
    MOCK_UP_FAIL="${MOCK_UP_FAIL:-0}" MOCK_CERT_FAIL="${MOCK_CERT_FAIL:-0}" \
    bash ./install.sh --edition community --yes "$@" >"$TMP_ROOT/install.log" 2>&1)
  echo $?
}

# C1) LiveKit start 失败（compose up 失败）：安装中止，绝不进入 TLS/完成阶段
reset_env; : >"$MOCK_LOG2"
RC="$(MOCK_UP_FAIL=1 run_install)"
[ "$RC" != 0 ] && ok "compose up 失败 → 安装非零退出" || bad "compose up 失败 → 安装非零退出" "exit=0"
assert_not_grep "启动失败不进入 TLS 阶段" 'init-letsencrypt' "$MOCK_LOG2"
assert_not_grep "启动失败不打印部署完成" '部署完成' "$TMP_ROOT/install.log"

# C2) 证书签发失败（certbot 失败）：中止并可恢复（重跑成功）
reset_env; : >"$MOCK_LOG2"
RC="$(MOCK_CERT_FAIL=1 run_install)"
[ "$RC" != 0 ] && ok "certbot 失败 → 安装非零退出" || bad "certbot 失败 → 安装非零退出" "exit=0"
assert_grep "证书失败给出 TLS 修复指引" 'TLS 证书签发失败' "$TMP_ROOT/install.log"
assert_file_absent "失败后不留半签发证书（api 域）" "$FIXTURE/data/certbot/conf/live/api.test.invalid/fullchain.pem"
# 恢复：同 fixture 重跑（幂等），certbot 桩恢复签发
: >"$MOCK_LOG2"
RC="$(run_install)"
assert_rc 0 "$RC" "故障恢复：重跑一次安装成功（幂等）" "$TMP_ROOT/install.log"
assert_file_present "恢复后 TURN 域证书已签发" "$FIXTURE/data/certbot/conf/live/turn.test.invalid/fullchain.pem"
assert_file_present "恢复后 RTC 域证书已签发" "$FIXTURE/data/certbot/conf/live/rtc.test.invalid/fullchain.pem"
assert_grep "TURN 域参与证书签发" 'docker compose.*run.*certbot.* -d turn\.test\.invalid' "$MOCK_LOG2"
# 恢复质量：turn 证书必须是 certbot 正式产物（而非残留的 1 天期临时自签证书）
assert_grep "恢复后 TURN 域为正式证书（非临时自签）" '^issued fullchain$' \
  "$FIXTURE/data/certbot/conf/live/turn.test.invalid/fullchain.pem"
if find "$FIXTURE/data/certbot/conf/live" -name '.imboy-selfsigned' | grep -q .; then
  bad "恢复后无临时自签标记残留" "$(find "$FIXTURE/data/certbot/conf/live" -name '.imboy-selfsigned')"
else
  ok "恢复后无临时自签标记残留"
fi
assert_grep "完成态打印 rtc 域 WSS" 'wss://rtc\.test\.invalid' "$TMP_ROOT/install.log"

# C3) TURN overlay 装配（LIVEKIT_TURN_ENABLED=true）
reset_env; set_env2 LIVEKIT_TURN_ENABLED true
: >"$MOCK_LOG2"
RC="$(run_install)"
assert_rc 0 "$RC" "TURN 开启时安装成功（证书桩已就绪）" "$TMP_ROOT/install.log"
assert_grep "up -d 装配 TURN overlay" 'compose -f docker-compose\.community\.yml -f docker-compose\.livekit-turn\.yml up -d' "$MOCK_LOG2"
assert_grep "证书目录默认值展开写入 .env" '^LIVEKIT_TURN_CERT_DIR=\./data/certbot/conf/live/turn\.test\.invalid$' "$FIXTURE/.env"
assert_grep "完成态打印 TURN 端点" 'turn\(s\):turn\.test\.invalid:3478\|5349' "$TMP_ROOT/install.log"

echo "== 结果：PASS=${PASS} FAIL=${FAIL} =="
[ "$FAIL" -eq 0 ]
