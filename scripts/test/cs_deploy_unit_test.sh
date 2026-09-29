#!/usr/bin/env bash
# =============================================================================
# cs_deploy 单元/事务测试（CSD-CLI-01 focused tests）
# -----------------------------------------------------------------------------
# 全离线：ssh/scp/rsync/bun/blue-green 全部为本地 fake，不连接任何服务器。
# 覆盖（对应验收 A01-A06）：
#   A01 usage/参数等价（cs 合法、非法组件拒绝、长短选项与 --env-file 顺序无关）
#   A02 allowlist+marker 在任何远端调用前拒绝（注入计数器证明远端零调用）
#   A03 步骤序列精确等于合同 S7 且各恰一次；Backend 复用 deploy_api 零第二套蓝绿
#   A04 五个故障点回滚 oracle + 首次安装失败无残余（fake 文件系统断言 symlink 指向）
#   A05 verbose 输出脱敏断言（cookie/证书私钥零出现，仅脱敏占位）
#   A06 all -l backend 恰一次且顺序 api→admin→cs；既有组件行为不回归
#   A07 cs-widget 网关模板合同（Seat 嵌入；seat SSE 正则 ↔ imboy_router
#       真实路由表逐段对照，REVIEW-2 P1 防再漂移闭环）
#   A08 preflight base_url ↔ CS_WIDGET_DOMAIN 比对检查（SC-EMBED §5；正例/
#       缺失口径/三类漂移负例 + 变异自证：阉割或弱化检查器后负例必不再报错）
# =============================================================================
set -uo pipefail

cd "$(dirname "$0")/../.." || exit 1

TMP_ROOT="$(mktemp -d /tmp/imboy_cs_deploy_test.XXXXXX)"
MOCK_BIN="$TMP_ROOT/bin"
MOCK_LOG="$TMP_ROOT/ops.events"        # fake 远端操作事件（一行一事件）
EVENTS="$TMP_ROOT/steps.events"        # deploy_cs 步骤事件（CS_EVENT_LOG）
OUT="$TMP_ROOT/output.log"
SSH_CALLS="$TMP_ROOT/ssh.count"
SCP_CALLS="$TMP_ROOT/scp.count"
RSYNC_CALLS="$TMP_ROOT/rsync.count"
BUN_CALLS="$TMP_ROOT/bun.count"
DISCOVER_STATE="$TMP_ROOT/discover.count"
BLUE_GREEN_LOG="$TMP_ROOT/bluegreen.log"
ENV_FILE="$TMP_ROOT/customer.env"
FAKE_ROOT="$TMP_ROOT/fakeroot"
TEST_SCRIPTS="$TMP_ROOT/scripts"
TEST_SOURCE_HEAD=0123456789abcdef0123456789abcdef01234567

mkdir -p "$MOCK_BIN" "$TEST_SCRIPTS/lib"

cleanup() { [ "${KEEP_TMP:-0}" = 1 ] || rm -rf -- "$TMP_ROOT"; }
trap cleanup EXIT

# ---------- 复制被测脚本 + 蓝绿桩 ----------
cp scripts/imboy-deploy.sh "$TEST_SCRIPTS/imboy-deploy.sh"
cp scripts/lib/cs_deploy.sh "$TEST_SCRIPTS/lib/cs_deploy.sh"
# 1b2ca499 起 imboy-deploy.sh 对 api/cs/all 惰性 source 生产配置键预检库，
# harness 必须同步携带（否则 source 失败，事务在构建前即死）。
cp scripts/lib/check_prod_config.sh "$TEST_SCRIPTS/lib/check_prod_config.sh"
cat >"$TEST_SCRIPTS/lib/blue_green_deploy.sh" <<'STUB'
#!/usr/bin/env bash
# CSD-CLI-01 测试桩：只记录被调用与参数，可注入失败。
printf 'BLUEGREEN %s\n' "$*" >>"$MOCK_LOG"
[ "${MOCK_FAIL_AT:-}" != "backend" ] || exit 1
exit 0
STUB
chmod +x "$TEST_SCRIPTS/lib/blue_green_deploy.sh"

# ---------- 仓库根桩文件（api/all 的版本同步/CHANGELOG 门禁用） ----------
printf '%s\n' 0.0.0 >"$TMP_ROOT/VERSION"
printf '%s\n' '{release, {imboy, "0.0.0"}, [' \
              '    imboy' \
              ']}.' >"$TMP_ROOT/relx.config"
cp "$TMP_ROOT/relx.config" "$TMP_ROOT/relxpro.config"
printf '%s\n' '## [1.0.0] - 2026-09-20' >"$TMP_ROOT/CHANGELOG.md"

# ---------- fake 远端文件系统 ----------
setup_fake_fs() {
  local mode="${1:-upgrade}"
  rm -rf "$FAKE_ROOT"
  mkdir -p "$FAKE_ROOT/www/wwwroot/cs.imboy.test/releases/old-release" \
           "$FAKE_ROOT/www/wwwroot/admin.imboy.test" \
           "$FAKE_ROOT/admin-src" \
           "$FAKE_ROOT/etc/nginx" "$FAKE_ROOT/etc/ssl" "$FAKE_ROOT/tmp"
  printf 'marker' >"$FAKE_ROOT/www/wwwroot/cs.imboy.test/.imboy-cs-root"
  printf 'old-loader' >"$FAKE_ROOT/www/wwwroot/cs.imboy.test/releases/old-release/loader.js"
  printf '{"scripts":{"build":"bun run build"}}' >"$FAKE_ROOT/www/wwwroot/admin.imboy.test/package.json"
  printf 'admin-marker' >"$FAKE_ROOT/www/wwwroot/admin.imboy.test/.imboy-admin-root"
  printf '%s\n' '{"scripts":{"build:widget":"vite build --mode widget"}}' >"$FAKE_ROOT/admin-src/package.json"
  printf 'api-upstream-9800\nserver 127.0.0.1:9800;\n' >"$FAKE_ROOT/etc/nginx/api.imboy.test.conf"
  printf 'prodadm-upstream-9800\n' >"$FAKE_ROOT/etc/nginx/prodadm.imboy.test.conf"
  printf -- '-----BEGIN CERTIFICATE-----\nfake-fullchain-body\n' >"$FAKE_ROOT/etc/ssl/cs.imboy.test.fullchain.pem"
  printf -- '-----BEGIN PRIVATE KEY-----\nsupersecret-key-material-DO-NOT-PRINT\n' >"$FAKE_ROOT/etc/ssl/cs.imboy.test.key.pem"
  if [ "$mode" = "upgrade" ]; then
    printf 'old-cs-vhost-content\n' >"$FAKE_ROOT/etc/nginx/cs.imboy.test.conf"
    ln -sfn releases/old-release "$FAKE_ROOT/www/wwwroot/cs.imboy.test/current"
  fi
}

write_env() {
  cat >"$ENV_FILE" <<'EOF'
SERVER_HOST=deploy.test.invalid
SERVER_PORT=2222
SERVER_USER=deployer
DEPLOY_VSN=1.0.0
DEPLOY_BRANCH=main
DEPLOY_PROJECT_DIR=/srv/imboy
DEPLOY_BLUE_PORT=9800
DEPLOY_GREEN_PORT=9801
DEPLOY_COOKIE=supersecret-cookie-value
DEPLOY_SALES_RELEASE=false
DEPLOY_STOP_OLD=true
NGINX_CONF=/etc/nginx/api.imboy.test.conf
PRODADM_CONF=/etc/nginx/prodadm.imboy.test.conf
ADMIN_BUILD_DIR=../fakeroot/admin-src
ADMIN_REMOTE_DIR=/www/wwwroot/admin.imboy.test
DB_CONTAINER=pg.test
DB_NAME=imboy_test
DB_USER=imboy_user
CS_WIDGET_DOMAIN=cs.imboy.test
CS_BUILD_DIR=../fakeroot/admin-src/dist-widget
CS_REMOTE_ROOT=/www/wwwroot/cs.imboy.test
CS_NGINX_CONF=/etc/nginx/cs.imboy.test.conf
CS_CERT_FULLCHAIN=/etc/ssl/cs.imboy.test.fullchain.pem
CS_CERT_KEY=/etc/ssl/cs.imboy.test.key.pem
CS_SMOKE_SHOP_ORIGIN=https://shop.imboy.test
EOF
}

# 覆盖/追加一个配置项：值一律单引号包裹，防止 source 时执行 $( )/反引号
override_env_kv() {
  printf "%s='%s'\n" "$1" "$2" >>"$ENV_FILE"
}

# ---------- fake binaries ----------
cat >"$MOCK_BIN/ssh" <<'MOCK'
#!/usr/bin/env bash
# fake ssh：记录调用数；按 `: imboy-cs-*` 标签在 fake 文件系统上模拟远端操作。
inc_counter() { local n; n="$(cat "$1" 2>/dev/null || printf 0)"; printf '%s\n' "$((n+1))" >"$1"; }
log_event() { printf '%s\n' "$1" >>"$MOCK_LOG"; }
inc_counter "$SSH_CALLS"

case " $* " in
  *" -O exit "*|*" -fNM "*) exit 0 ;;
esac

last=""
for a in "$@"; do last="$a"; done
cmd="$last"
sedget() { printf '%s' "$cmd" | sed -n "$1" | head -1; }

case "$cmd" in
  ": imboy-cs-check-precheck"*)
    [ "${MOCK_PRECHECK_ESCAPE:-0}" = 1 ] && exit 3
    ROOT="$(sedget "s#.*realpath -e '\([^']*\)'.*#\1#p")"
    CONF="$(sedget "s#.*if \[ -f '\([^']*\)' \].*#\1#p")"
    L="$FAKE_ROOT$ROOT"
    [ -d "$L" ] || exit 2
    [ -f "$L/.imboy-cs-root" ] || exit 4
    printf 'ROOT=%s\n' "$ROOT"
    C="$(readlink "$L/current" 2>/dev/null || true)"
    printf 'CURRENT=%s\n' "$C"
    if [ -f "$FAKE_ROOT$CONF" ]; then printf 'VHOST_EXISTS=1\n'; else printf 'VHOST_EXISTS=0\n'; fi
    log_event CHECK_PRECHECK
    exit 0 ;;
  ": imboy-cs-op-prepare-release"*)
    [ "${MOCK_FAIL_AT:-}" = "prepare_release" ] && exit 1
    D="$(sedget "s#.*mkdir -p '\([^']*\)'.*#\1#p")"
    mkdir -p "$FAKE_ROOT$D"
    log_event PREPARE_RELEASE
    exit 0 ;;
  ": imboy-cs-op-verify-release"*)
    D="$(sedget "s#.*cd '\([^']*\)'.*#\1#p")"
    L="$FAKE_ROOT$D"
    if [ "${MOCK_FAIL_AT:-}" = "verify" ] || [ ! -s "$L/manifest.json" ] \
       || [ ! -s "$L/manifest.sha256" ] || [ ! -s "$L/loader.js" ] \
       || [ ! -s "$L/widget/index.html" ] || [ ! -d "$L/assets" ]; then
      exit 8
    fi
    log_event VERIFY_RELEASE
    exit 0 ;;
  ": imboy-cs-check-tls-vhost"*)
    [ "${MOCK_FAIL_AT:-}" = "nginx_t" ] && exit 15
    CERT="$(sedget "s#.*\[ -s '\([^']*\)' \].*#\1#p")"
    STAGED="$(sedget "s#.* '\([^']*\)' > .*#\1#p")"
    if [ ! -s "$FAKE_ROOT$CERT" ] || [ ! -s "$FAKE_ROOT$STAGED" ]; then exit 10; fi
    log_event TLS_VALIDATE
    exit 0 ;;
  ": imboy-cs-op-discover-upstream"*)
    n="$(cat "$DISCOVER_STATE" 2>/dev/null || printf 0)"; n=$((n+1))
    printf '%s\n' "$n" >"$DISCOVER_STATE"
    if [ "$n" -eq 1 ]; then V="${MOCK_UPSTREAM_PRE-9800}"; else V="${MOCK_UPSTREAM_POST-9801}"; fi
    printf 'UPSTREAM=%s\n' "$V"
    log_event "DISCOVER_UPSTREAM#$n"
    exit 0 ;;
  ": imboy-cs-op-backup-vhost"*)
    CONF="$(sedget "s#.*CONF='\([^']*\)'.*#\1#p")"
    STAMP="$(sedget "s#.*cs-bak-\([0-9]*\).*#\1#p")"
    LCONF="$FAKE_ROOT$CONF"
    if [ -f "$LCONF" ]; then
      cp -p "$LCONF" "$LCONF.cs-bak-$STAMP" || exit 12
      shasum -a 256 "$LCONF" >"$LCONF.cs-bak-$STAMP.sha256" || exit 13
      printf 'BAK=%s.cs-bak-%s\n' "$CONF" "$STAMP"
      log_event BACKUP_VHOST
    else
      printf 'BAK=\n'
    fi
    exit 0 ;;
  ": imboy-cs-op-record-prev"*)
    TGT="$(sedget "s#.*printf '%s.n' '\([^']*\)'.*#\1#p")"
    DST="$(sedget "s#.*> '\([^']*\)'.*#\1#p")"
    mkdir -p "$FAKE_ROOT$(dirname "$DST")"
    printf '%s\n' "$TGT" >"$FAKE_ROOT$DST"
    log_event RECORD_PREV
    exit 0 ;;
  ": imboy-cs-op-swap-symlink"*)
    [ "${MOCK_FAIL_AT:-}" = "swap_symlink" ] && exit 16
    TGT="$(sedget "s#.*ln -sfn '\([^']*\)'.*#\1#p")"
    LNK="$(sedget "s#.*ln -sfn '[^']*' '\([^']*\)\.cs-new'.*#\1#p")"
    L="$FAKE_ROOT$LNK"
    ln -sfn "$TGT" "$L.cs-new" || exit 16
    mv -f "$L.cs-new" "$L" || exit 17
    log_event SWAP_SYMLINK
    exit 0 ;;
  ": imboy-cs-op-swap-vhost"*)
    [ "${MOCK_FAIL_AT:-}" = "swap_vhost" ] && exit 18
    SRC="$(sedget "s#.*mv -T '\([^']*\)'.*#\1#p")"
    DST="$(sedget "s#.*mv -T '[^']*' '\([^']*\)'.*#\1#p")"
    [ -f "$FAKE_ROOT$SRC" ] || exit 18
    mv -f "$FAKE_ROOT$SRC" "$FAKE_ROOT$DST" || exit 18
    log_event SWAP_VHOST
    exit 0 ;;
  ": imboy-cs-op-nginx-reload"*)
    [ "${MOCK_FAIL_AT:-}" = "nginx_t2" ] && exit 6
    [ "${MOCK_FAIL_AT:-}" = "nginx_reload" ] && exit 19
    log_event NGINX_RELOAD
    exit 0 ;;
  ": imboy-cs-check-smoke"*)
    log_event SMOKE
    [ "${MOCK_SMOKE_OK:-1}" = 1 ] || exit 32
    printf 'SMOKE=OK\n'
    exit 0 ;;
  ": imboy-cs-op-finalize"*)
    R="$(sedget "s#.*>> '\([^']*\)/activations.log'.*#\1#p")"
    CLN="$(sedget "s#.*rm -f '\([^']*\)'\.cs-staged-.*#\1#p")"
    mkdir -p "$FAKE_ROOT$R"
    printf '%s\n' "$cmd" | sed -n '2p' >>"$FAKE_ROOT$R/activations.log"
    if [ -n "$CLN" ]; then
      rm -f "$FAKE_ROOT$CLN".cs-staged-* "$FAKE_ROOT$CLN".cs-final-* 2>/dev/null
    fi
    log_event FINALIZE
    exit 0 ;;
  ": imboy-cs-op-restore-symlink"*)
    OLD="$(sedget "s#.*if \[ -n '\([^']*\)' \].*#\1#p")"
    LNK="$(sedget "s#.*mv -T '[^']*\.cs-restore' '\([^']*\)'.*#\1#p")"
    if [ -z "$LNK" ]; then
      LNK="$(sedget "s#.*rm -f '\([^']*\)'.*#\1#p")"
    fi
    L="$FAKE_ROOT$LNK"
    if [ -n "$OLD" ]; then
      ln -sfn "$OLD" "$L.cs-restore" || exit 20
      mv -f "$L.cs-restore" "$L" || exit 20
    else
      rm -f "$L" || exit 20
    fi
    log_event RESTORE_SYMLINK
    exit 0 ;;
  ": imboy-cs-op-restore-vhost"*)
    BAK="$(sedget "s#.*if \[ -n '\([^']*\)' \].*#\1#p")"
    CONF="$(sedget "s#.*mv -T '[^']*\.cs-restore' '\([^']*\)'.*#\1#p")"
    if [ -z "$CONF" ]; then
      CONF="$(sedget "s#.*rm -f '\([^']*\)'.*#\1#p")"
    fi
    if [ -n "$BAK" ]; then
      cp -p "$FAKE_ROOT$BAK" "$FAKE_ROOT$CONF.cs-restore" || exit 21
      mv -f "$FAKE_ROOT$CONF.cs-restore" "$FAKE_ROOT$CONF" || exit 21
    else
      rm -f "$FAKE_ROOT$CONF" || exit 21
    fi
    log_event RESTORE_VHOST
    exit 0 ;;
  ": imboy-cs-op-clean-staged"*)
    CLN="$(sedget "s#.*rm -f '\([^']*\)'\.cs-staged-.*#\1#p")"
    if [ -n "$CLN" ]; then
      rm -f "$FAKE_ROOT$CLN".cs-staged-* "$FAKE_ROOT$CLN".cs-final-* 2>/dev/null
    fi
    CUR="$(sedget "s#.*rm -f '[^']*\.cs-staged-[^']* '[^']*/current\.cs-new'.*#\1#p")"
    if [ -n "$CUR" ]; then
      rm -f "$FAKE_ROOT$CUR" 2>/dev/null
    fi
    log_event CLEAN_STAGED
    exit 0 ;;
  *".imboy-admin-root"*)
    A="$(sedget "s#.*realpath -e '\([^']*\)'.*#\1#p")"
    L="$FAKE_ROOT$A"
    if [ -d "$L" ] && [ -f "$L/.imboy-admin-root" ]; then
      printf '%s\n' "$A"
      log_event ADMIN_REALPATH
      exit 0
    fi
    exit 4 ;;
  *"BLUE_API="*"统一 readiness"*|*"BLUE_API="*"ADMIN_META="*)
    log_event READINESS
    exit 0 ;;
  *"curl -sS -o /dev/null -w '%{http_code}'"*"/api/adm/admin/config/sidebar"*)
    printf '401\n'
    exit 0 ;;
  *"curl -sS -o /dev/null -w '%{http_code}'"*"https://admin.imboy.test/"*)
    printf '200\n'
    exit 0 ;;
  *"deploy-meta.json"*)
    printf '%s\n' "$TEST_SOURCE_HEAD"
    exit 0 ;;
  *"command -v escript"*)
    # check_prod_config 远端守卫执行命令（1b2ca499 引入）：离线桩直接回 OK。
    # 守卫自身逻辑属生产路径，本 harness 只保证其不改变 cs/api/all 事务时序。
    log_event CONFIG_GUARD
    printf 'CONFIG_KEYS_OK\n'
    exit 0 ;;
  *"imboy-config-key-guard.escript"*)
    # 守卫清理远端临时 escript 的 ssh 调用：静默成功。
    exit 0 ;;
  *)
    log_event "SSH_OTHER:$(printf '%s' "$cmd" | cut -c1-40)"
    exit 0 ;;
esac
MOCK
chmod +x "$MOCK_BIN/ssh"

cat >"$MOCK_BIN/scp" <<'MOCK'
#!/usr/bin/env bash
inc_counter() { local n; n="$(cat "$1" 2>/dev/null || printf 0)"; printf '%s\n' "$((n+1))" >"$1"; }
log_event() { printf '%s\n' "$1" >>"$MOCK_LOG"; }
inc_counter "$SCP_CALLS"
[ "${MOCK_FAIL_AT:-}" = "scp" ] && exit 1
src="" ; dst=""
prev_opt=0
for a in "$@"; do
  if [ "$prev_opt" = 1 ]; then prev_opt=0; continue; fi
  case "$a" in
    -P|-o|-i) prev_opt=1 ;;
    -*) : ;;
    *) if [ -z "$src" ]; then src="$a"; else dst="$a"; fi ;;
  esac
done
remote_path="${dst#*:}"
mkdir -p "$FAKE_ROOT$(dirname "$remote_path")"
cp "$src" "$FAKE_ROOT$remote_path"
log_event "SCP:$remote_path"
exit 0
MOCK
chmod +x "$MOCK_BIN/scp"

cat >"$MOCK_BIN/rsync" <<'MOCK'
#!/usr/bin/env bash
inc_counter() { local n; n="$(cat "$1" 2>/dev/null || printf 0)"; printf '%s\n' "$((n+1))" >"$1"; }
log_event() { printf '%s\n' "$1" >>"$MOCK_LOG"; }
inc_counter "$RSYNC_CALLS"
[ "${MOCK_FAIL_AT:-}" = "rsync" ] && exit 1
positional=""
skip_next=0
for a in "$@"; do
  if [ "$skip_next" = 1 ]; then skip_next=0; continue; fi
  case "$a" in
    -e|--rsh) skip_next=1 ;;
    -*) : ;;
    *) positional="$positional
$a" ;;
  esac
done
items=""
while IFS= read -r line; do
  [ -n "$line" ] || continue
  if [ -z "$items" ]; then items="$line"; else items="$items
$line"; fi
done <<EOF
$positional
EOF
n="$(printf '%s\n' "$items" | grep -c . || true)"
if [ "${n:-0}" -ge 2 ]; then
  src="$(printf '%s\n' "$items" | sed -n "$((n-1))p")"
  dst="$(printf '%s\n' "$items" | sed -n "${n}p")"
  remote_path="${dst#*:}"
  remote_path="${remote_path%/}"
  mkdir -p "$FAKE_ROOT$remote_path"
  cp -R "${src%/}/." "$FAKE_ROOT$remote_path/"
  log_event "RSYNC:$remote_path"
fi
exit 0
MOCK
chmod +x "$MOCK_BIN/rsync"

cat >"$MOCK_BIN/git" <<'MOCK'
#!/usr/bin/env bash
case " $* " in
  *" rev-parse HEAD "*) printf '%s\n' "$TEST_SOURCE_HEAD" ;;
  *" status --porcelain "*) [ "${MOCK_GIT_DIRTY:-0}" = 1 ] && printf ' M src/widget-demo.ts\n' ;;
  *) exit 0 ;;
esac
MOCK
chmod +x "$MOCK_BIN/git"

cat >"$MOCK_BIN/bun" <<'MOCK'
#!/usr/bin/env bash
inc_counter() { local n; n="$(cat "$1" 2>/dev/null || printf 0)"; printf '%s\n' "$((n+1))" >"$1"; }
log_event() { printf '%s\n' "$1" >>"$MOCK_LOG"; }
inc_counter "$BUN_CALLS"
case "$*" in
  install*)
    exit 0 ;;
  *"build:widget"*)
    [ "${MOCK_FAIL_AT:-}" = "build" ] && exit 1
    D="$PWD/dist-widget"
    mkdir -p "$D/widget" "$D/assets" "$D/widget-assets"
    printf 'loader-js\n' >"$D/loader.js"
    printf '{"source_head":"%s","files":[]}\n' "$TEST_SOURCE_HEAD" >"$D/manifest.json"
    shasum -a 256 "$D/manifest.json" | awk '{print $1}' >"$D/manifest.sha256"
    printf 'ok\n' >"$D/health.txt"
    printf '<html>widget</html>\n' >"$D/widget/index.html"
    printf 'x' >"$D/assets/cs-widget-deadbeef.js"
    printf 'frame-v2\n' >"$D/widget-assets/cs-widget.v2.js"
    log_event BUN_BUILD_WIDGET
    exit 0 ;;
  *"build"*)
    mkdir -p "$PWD/dist"
    printf '<html>admin-ok</html>\n' >"$PWD/dist/index.html"
    log_event BUN_BUILD_ADMIN
    exit 0 ;;
esac
exit 0
MOCK
chmod +x "$MOCK_BIN/bun"

# ---------- 断言助手 ----------
PASS=0
FAIL=0
ok()  { PASS=$((PASS + 1)); echo "  PASS $1"; }
bad() { FAIL=$((FAIL + 1)); echo "  FAIL $1: ${2:-<无详情>}"; }

assert_eq() {
  local desc="$1" want="$2" got="$3"
  if [ "$want" = "$got" ]; then ok "$desc"; else bad "$desc" "want=[$want] got=[$got]"; fi
}

reset_run_state() {
  : >"$MOCK_LOG"; : >"$EVENTS"; : >"$BLUE_GREEN_LOG"
  rm -f "$SSH_CALLS" "$SCP_CALLS" "$RSYNC_CALLS" "$BUN_CALLS" "$DISCOVER_STATE"
}

# 运行主脚本（args 透传）；输出在 ${OUT}，退出码通过 $? 取
run_component() {
  reset_run_state
  ( cd "$TMP_ROOT" && env \
      PATH="$MOCK_BIN:$PATH" \
      CS_EVENT_LOG="$EVENTS" \
      MOCK_LOG="$MOCK_LOG" \
      FAKE_ROOT="$FAKE_ROOT" \
      SSH_CALLS="$SSH_CALLS" SCP_CALLS="$SCP_CALLS" RSYNC_CALLS="$RSYNC_CALLS" \
      BUN_CALLS="$BUN_CALLS" DISCOVER_STATE="$DISCOVER_STATE" \
      BLUE_GREEN_LOG="$BLUE_GREEN_LOG" \
      MOCK_FAIL_AT="${MOCK_FAIL_AT-}" \
      MOCK_SMOKE_OK="${MOCK_SMOKE_OK-1}" \
      MOCK_UPSTREAM_PRE="${MOCK_UPSTREAM_PRE-9800}" \
      MOCK_UPSTREAM_POST="${MOCK_UPSTREAM_POST-9801}" \
      MOCK_PRECHECK_ESCAPE="${MOCK_PRECHECK_ESCAPE-0}" \
      MOCK_GIT_DIRTY="${MOCK_GIT_DIRTY-0}" \
      TEST_SOURCE_HEAD="$TEST_SOURCE_HEAD" \
      bash "$TEST_SCRIPTS/imboy-deploy.sh" "$@" ) >"$OUT" 2>&1
}

no_remote_calls() {
  local desc="$1"
  if [ ! -s "$SSH_CALLS" ] && [ ! -s "$SCP_CALLS" ] && [ ! -s "$RSYNC_CALLS" ]; then
    ok "${desc}（ssh/scp/rsync 计数全零）"
  else
    bad "$desc" "ssh=$(cat "$SSH_CALLS" 2>/dev/null || echo 0) scp=$(cat "$SCP_CALLS" 2>/dev/null || echo 0) rsync=$(cat "$RSYNC_CALLS" 2>/dev/null || echo 0)"
  fi
}

steps_list() { grep -a '^STEP ' "$EVENTS" 2>/dev/null | sed 's/^STEP //' | tr '\n' ' ' | sed 's/ $//'; }

assert_steps_equal_contract() {
  local desc="$1"
  local expected done_steps
  expected="PRECHECK BUILD_AND_VERIFY_WIDGET STAGE_WIDGET_RELEASE VALIDATE_CS_VHOST_AND_TLS DEPLOY_BACKEND_BLUE_GREEN ATOMIC_ACTIVATE_WIDGET_AND_VHOST REAL_SMOKE FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY"
  done_steps="$(steps_list)"
  assert_eq "$desc" "$expected" "$done_steps"
}

event_count() { grep -ac "^$1" "$MOCK_LOG" 2>/dev/null || true; }

assert_event_count() {
  local desc="$1" event="$2" want="$3" got
  got="$(event_count "$event")"
  assert_eq "$desc" "$want" "$got"
}

current_symlink_target() { readlink "$FAKE_ROOT/www/wwwroot/cs.imboy.test/current" 2>/dev/null || printf '<absent>'; }
cs_vhost_content() { cat "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf" 2>/dev/null || printf '<absent>'; }

assert_no_staged_residue() {
  local desc="$1" n
  n="$(find "$FAKE_ROOT/etc/nginx" -name 'cs.imboy.test.conf.cs-*' 2>/dev/null | grep -vc 'cs-bak' || true)"
  assert_eq "$desc" "0" "${n:-0}"
}

# =============================================================================
echo "== A00. 纯函数库（直接 source lib/cs_deploy.sh） =="
LIB_RESULT="$(mktemp "$TMP_ROOT/libok.XXXXXX")"
(
  # shellcheck source=../../lib/cs_deploy.sh
  source "$PWD/scripts/lib/cs_deploy.sh"
  p=0; f=0
  chk() { # desc expect_ok(0=应通过,1=应拒绝) 实际调用结果
    local got
    if [ "$3" = "0" ]; then got=0; else got=1; fi
    if [ "$got" = "$2" ]; then p=$((p+1)); else f=$((f+1)); echo "    subFAIL $1 (got=$got want=$2)"; fi
  }
  nl=$'\n'
  chk "domain 合法" 0 "$(cs_valid_domain cs.imboy.test; echo $?)"
  chk "domain ../evil 拒绝" 1 "$(cs_valid_domain '../evil'; echo $?)"
  chk "domain 空格拒绝" 1 "$(cs_valid_domain 'has space'; echo $?)"
  chk "domain \$(cmd) 拒绝" 1 "$(cs_valid_domain 'a$(touch /tmp/pwn1)b'; echo $?)"
  chk "domain 反引号拒绝" 1 "$(cs_valid_domain 'a`id`b'; echo $?)"
  chk "domain 分号拒绝" 1 "$(cs_valid_domain 'a;b'; echo $?)"
  chk "domain 换行拒绝" 1 "$(cs_valid_domain "a${nl}b.c"; echo $?)"
  chk "domain 单段拒绝" 1 "$(cs_valid_domain 'localhost'; echo $?)"
  chk "domain 尾连字符拒绝" 1 "$(cs_valid_domain 'a.b-'; echo $?)"
  chk "origin https 合法" 0 "$(cs_valid_origin 'https://shop.imboy.test'; echo $?)"
  chk "origin 带端口合法" 0 "$(cs_valid_origin 'https://shop.imboy.test:8443'; echo $?)"
  chk "origin ftp 拒绝" 1 "$(cs_valid_origin 'ftp://shop.imboy.test'; echo $?)"
  chk "origin 带路径拒绝" 1 "$(cs_valid_origin 'https://shop.imboy.test/p'; echo $?)"
  chk "origin userinfo 拒绝" 1 "$(cs_valid_origin 'https://u@shop.imboy.test'; echo $?)"
  chk "abs path 合法" 0 "$(cs_safe_abs_path '/www/wwwroot/x/conf'; echo $?)"
  chk "abs path 根拒绝" 1 "$(cs_safe_abs_path '/'; echo $?)"
  chk "abs path .. 拒绝" 1 "$(cs_safe_abs_path '/a/../b'; echo $?)"
  chk "abs path 空格拒绝" 1 "$(cs_safe_abs_path '/a b'; echo $?)"
  chk "abs path 相对路径拒绝" 1 "$(cs_safe_abs_path 'etc/nginx'; echo $?)"
  chk "approved root 成员合法" 0 "$(cs_remote_root_in_approved_scope '/www/wwwroot/cs.x'; echo $?)"
  chk "approved root 裸根拒绝" 1 "$(cs_remote_root_in_approved_scope '/www/wwwroot'; echo $?)"
  chk "approved root 尾斜杠裸根拒绝" 1 "$(cs_remote_root_in_approved_scope '/www/wwwroot/'; echo $?)"
  chk "approved root 越根拒绝" 1 "$(cs_remote_root_in_approved_scope '/etc/cs'; echo $?)"
  chk "version 合法" 0 "$(cs_valid_version '1.0.0-rc.1'; echo $?)"
  chk "version 非法拒绝" 1 "$(cs_valid_version '1.0.0 bad'; echo $?)"
  r="$(cs_redact '')";            [ "$r" = "(unset)" ] && p=$((p+1)) || { f=$((f+1)); echo "    subFAIL redact empty: $r"; }
  r="$(cs_redact 'ab')";          [ "$r" = "***" ] && p=$((p+1)) || { f=$((f+1)); echo "    subFAIL redact short: $r"; }
  r="$(cs_redact 'supersecret-cookie-value')"
  case "$r" in su***) p=$((p+1));; *) f=$((f+1)); echo "    subFAIL redact mask: $r";; esac
  case "$r" in *supersecret*) f=$((f+1)); echo "    subFAIL redact 泄漏: $r";; *) p=$((p+1));; esac
  want_steps="PRECHECK BUILD_AND_VERIFY_WIDGET STAGE_WIDGET_RELEASE VALIDATE_CS_VHOST_AND_TLS DEPLOY_BACKEND_BLUE_GREEN ATOMIC_ACTIVATE_WIDGET_AND_VHOST REAL_SMOKE FINALIZE_OR_ROLLBACK_WIDGET_GATEWAY"
  got_steps="$(cs_expected_steps | tr '\n' ' ' | sed 's/ $//')"
  [ "$got_steps" = "$want_steps" ] && p=$((p+1)) || { f=$((f+1)); echo "    subFAIL expected_steps: $got_steps"; }
  printf '%s %s\n' "$p" "$f" >"$LIB_RESULT"
)
LIB_P="$(cut -d' ' -f1 "$LIB_RESULT")"
LIB_F="$(cut -d' ' -f2 "$LIB_RESULT")"
if [ "${LIB_F:-1}" = "0" ]; then ok "纯函数库断言全绿（$LIB_P 项：allowlist/redact/S7 步骤表）"; else bad "纯函数库断言失败" "$LIB_P pass / $LIB_F fail"; fi

# =============================================================================
echo "== A01. usage / 参数等价 =="
setup_fake_fs upgrade
write_env

run_component totally-bogus
rc=$?
if [ "$rc" -ne 0 ] && grep -q "用法" "$OUT" && [ ! -s "$SSH_CALLS" ]; then
  ok "非法组件在 SSH 前拒绝并输出 usage"
else
  bad "非法组件拒绝" "rc=$rc ssh=$(cat "$SSH_CALLS" 2>/dev/null || echo 0)"
fi

run_component cs -x
rc=$?
if [ "$rc" -ne 0 ] && grep -q "用法" "$OUT" && [ ! -s "$SSH_CALLS" ]; then
  ok "非法 flag 拒绝并输出 usage"
else
  bad "非法 flag 拒绝" "rc=$rc"
fi

# cs 非 -l 同样从源码构建，不复用缺失或陈旧产物。
run_component cs -v --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -eq 0 ] && [ "$(event_count BUN_BUILD_WIDGET)" = 1 ]; then
  ok "cs 非 -l 在产物缺失时仍从当前源码构建"
else
  bad "cs 非 -l 源码构建" "rc=$rc out=$(tail -3 "$OUT")"
fi

# 长短选项 / --env-file 顺序等价：全部成功且步骤序列/蓝绿参数一致
first_seq=""
first_bg=""
equiv_fail=0
run_equiv() {
  setup_fake_fs upgrade
  write_env
  run_component "$@"
  local rc=$?
  if [ "$rc" -ne 0 ]; then
    bad "等价变体失败: $*" "$(tail -3 "$OUT")"
    equiv_fail=1
    return 1
  fi
  local seq bg
  seq="$(steps_list)"
  bg="$(cat "$BLUE_GREEN_LOG")"
  if [ -z "$first_seq" ]; then
    first_seq="$seq"; first_bg="$bg"
  else
    [ "$seq" = "$first_seq" ] || { bad "变体步骤序列不一致: $*" "$seq"; equiv_fail=1; }
    [ "$bg" = "$first_bg" ] || { bad "变体蓝绿参数不一致: $*" "$bg"; equiv_fail=1; }
  fi
}
run_equiv cs -v -l --env-file "$ENV_FILE"
run_equiv cs --verbose --local --env-file "$ENV_FILE"
run_equiv cs -l -v --env-file "$ENV_FILE"
run_equiv cs --env-file "$ENV_FILE" -v -l
run_equiv cs -l --env-file="$ENV_FILE" -v
if [ "$equiv_fail" = 0 ]; then
  ok "长短选项与 --env-file 位置全部等价（5 变体步骤序列与蓝绿参数一致）"
fi

# =============================================================================
echo "== A02. allowlist / marker：任何远端调用前拒绝 =="
poison_case() { # $1=描述 $2=key $3=value（单引号包裹写入 env）
  setup_fake_fs upgrade
  write_env
  override_env_kv "$2" "$3"
  run_component cs -v -l --env-file "$ENV_FILE"
  local rc=$?
  if [ "$rc" -eq 0 ]; then
    bad "allowlist 负例被接受: $2=$3" ""
  elif [ -s "$SSH_CALLS" ] || [ -s "$RSYNC_CALLS" ] || [ -s "$SCP_CALLS" ]; then
    bad "allowlist 负例($2=$3)拒绝前已产生远端调用" "ssh=$(cat "$SSH_CALLS" 2>/dev/null || echo 0) rsync=$(cat "$RSYNC_CALLS" 2>/dev/null || echo 0)"
  else
    ok "allowlist 负例 SSH 前拒绝: $1"
  fi
}
poison_case "domain 路径穿越"   CS_WIDGET_DOMAIN "../evil"
poison_case "domain 空格"       CS_WIDGET_DOMAIN "has space"
poison_case "domain 命令替换"   CS_WIDGET_DOMAIN 'a$(touch /tmp/pwn1)b'
poison_case "domain 反引号"     CS_WIDGET_DOMAIN 'a`id`b'
poison_case "domain 带端口"     CS_WIDGET_DOMAIN "cs.imboy.test:8080"
poison_case "domain 单段"       CS_WIDGET_DOMAIN "localhost"
poison_case "root 越批准根"     CS_REMOTE_ROOT "/etc/nginx"
poison_case "root 穿越批准根"   CS_REMOTE_ROOT "/www/wwwroot/../evil"
poison_case "vhost 路径空格"    CS_NGINX_CONF "/etc/nginx/x y.conf"
poison_case "cert key 相对路径" CS_CERT_KEY "relative/key.pem"
poison_case "smoke origin ftp"  CS_SMOKE_SHOP_ORIGIN "ftp://shop.imboy.test"
poison_case "smoke origin 带路径" CS_SMOKE_SHOP_ORIGIN "https://shop.imboy.test/with/path"
poison_case "smoke origin 空格" CS_SMOKE_SHOP_ORIGIN "https://shop.imboy.test evil.com"

# fullchain == key 拒绝
setup_fake_fs upgrade; write_env
override_env_kv CS_CERT_KEY "/etc/ssl/cs.imboy.test.fullchain.pem"
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && grep -q "不得相同" "$OUT" && [ ! -s "$SSH_CALLS" ]; then
  ok "fullchain/key 相同在 SSH 前拒绝"
else
  bad "fullchain/key 相同拒绝" "rc=$rc"
fi

# 缺失必填键
setup_fake_fs upgrade; write_env
grep -v '^CS_CERT_FULLCHAIN=' "$ENV_FILE" >"$ENV_FILE.tmp" && mv "$ENV_FILE.tmp" "$ENV_FILE"
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && grep -q "缺少 CS 必填项: CS_CERT_FULLCHAIN" "$OUT" && [ ! -s "$SSH_CALLS" ]; then
  ok "缺失 CS_CERT_FULLCHAIN 在 SSH 前拒绝"
else
  bad "缺失必填键拒绝" "rc=$rc"
fi

# marker 缺失 → PRECHECK 拒绝且零上传
setup_fake_fs upgrade; write_env
rm -f "$FAKE_ROOT/www/wwwroot/cs.imboy.test/.imboy-cs-root"
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && grep -q ".imboy-cs-root" "$OUT" && [ ! -s "$RSYNC_CALLS" ] && [ "$(event_count CHECK_PRECHECK)" = 0 ]; then
  ok "marker 缺失在 PRECHECK 拒绝（零上传、零远端写）"
else
  bad "marker 缺失拒绝" "rc=$rc rsync=$(cat "$RSYNC_CALLS" 2>/dev/null || echo 0)"
fi

# realpath 越根 → PRECHECK 拒绝
setup_fake_fs upgrade; write_env
MOCK_PRECHECK_ESCAPE=1 run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && grep -q "越出批准根" "$OUT" && [ ! -s "$RSYNC_CALLS" ]; then
  ok "realpath 越出批准根在 PRECHECK 拒绝"
else
  bad "realpath 越根拒绝" "rc=$rc"
fi

# 根目录不存在 → PRECHECK 拒绝
setup_fake_fs upgrade; write_env
rm -rf "$FAKE_ROOT/www/wwwroot/cs.imboy.test"
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && grep -q "远端根不存在" "$OUT" && [ ! -s "$RSYNC_CALLS" ]; then
  ok "CS 远端根目录缺失在 PRECHECK 拒绝"
else
  bad "远端根缺失拒绝" "rc=$rc"
fi

# Widget 本地源码允许未提交改动：应告警并至少进入构建步骤，而不是预检拒绝。
setup_fake_fs upgrade; write_env
MOCK_GIT_DIRTY=1 MOCK_FAIL_AT=build run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] \
   && grep -q 'Widget 源码存在未提交或未跟踪改动；按当前工作树继续发布' "$OUT" \
   && grep -q '^STEP BUILD_AND_VERIFY_WIDGET$' "$EVENTS"; then
  ok "Widget 本地工作树有改动时告警并继续到构建"
else
  bad "Widget 本地工作树改动仍在构建前阻断" "rc=$rc output=$(tail -5 "$OUT" | tr '\n' ',')"
fi

# =============================================================================
echo "== A03. 成功路径：顺序精确等于合同 S7 且各恰一次 =="
setup_fake_fs upgrade; write_env
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ]; then
  bad "成功路径应退出 0" "$(tail -5 "$OUT")"
fi
assert_steps_equal_contract "S7 步骤序列逐字匹配"
assert_event_count "PRECHECK 恰一次" CHECK_PRECHECK 1
assert_event_count "build:widget 恰一次" BUN_BUILD_WIDGET 1
assert_event_count "prepare-release 恰一次" PREPARE_RELEASE 1
assert_event_count "rsync 上传恰一次" "RSYNC:" 1
assert_event_count "release 校验恰一次" VERIFY_RELEASE 1
assert_event_count "TLS/nginx -t 恰两次（staged+final）" TLS_VALIDATE 2
assert_event_count "upstream 发现恰两次（激活前后）" DISCOVER_UPSTREAM 2
assert_event_count "vhost 备份恰一次" BACKUP_VHOST 1
assert_event_count "symlink 切换恰一次" SWAP_SYMLINK 1
assert_event_count "vhost 替换恰一次" SWAP_VHOST 1
assert_event_count "nginx reload 恰一次" NGINX_RELOAD 1
assert_event_count "smoke 恰一次" SMOKE 1
assert_event_count "finalize 恒恰一次" FINALIZE 1
assert_event_count "蓝绿(deploy_api)恰一次" BLUEGREEN 1

# 事件全局顺序（各事件的首次出现行号严格单调）
order_ok=1
prev_line=0
for ev in CHECK_PRECHECK BUN_BUILD_WIDGET PREPARE_RELEASE "RSYNC:" VERIFY_RELEASE TLS_VALIDATE BLUEGREEN BACKUP_VHOST SWAP_SYMLINK SWAP_VHOST NGINX_RELOAD SMOKE FINALIZE; do
  line="$(grep -an "^$ev" "$MOCK_LOG" | head -1 | cut -d: -f1)"
  if [ -z "$line" ]; then bad "成功路径缺事件 $ev" ""; order_ok=0; break; fi
  if [ "$line" -lt "$prev_line" ]; then bad "成功路径事件顺序错位: $ev" "line=$line prev=$prev_line"; order_ok=0; break; fi
  prev_line="$line"
done
[ "$order_ok" = 1 ] && ok "成功路径事件顺序 = S7（build→stage→validate→backend→activate→smoke→finalize）"

# 激活结果断言（fake 文件系统）
case "$(current_symlink_target)" in
  releases/*) ok "成功后 current → 新 release 目录 ($(current_symlink_target))" ;;
  *) bad "成功后 current 指向异常" "$(current_symlink_target)" ;;
esac
if grep -q "proxy_pass http://127.0.0.1:9801;" "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf" \
   && grep -q "server_name cs.imboy.test;" "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf" \
   && grep -q "max-age=31536000, immutable" "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf" \
   && grep -q "proxy_buffering off;" "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf"; then
  ok "vhost 已原子替换为指向新 upstream 9801 的 final 内容"
else
  bad "vhost final 内容不正确" "$(head -3 "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf")"
fi
bak_n="$(find "$FAKE_ROOT/etc/nginx" -name 'cs.imboy.test.conf.cs-bak-*' ! -name '*.sha256' | wc -l | tr -d ' ')"
assert_eq "旧 vhost 时间戳备份存在（I5）" "1" "$bak_n"
baksum_n="$(find "$FAKE_ROOT/etc/nginx" -name '*.cs-bak-*.sha256' | wc -l | tr -d ' ')"
assert_eq "备份 checksum 记录存在（I5）" "1" "$baksum_n"
prev_n="$(find "$FAKE_ROOT/www/wwwroot/cs.imboy.test" -maxdepth 1 -name '.imboy-cs-prev-*' | wc -l | tr -d ' ')"
assert_eq "旧 symlink 指向记录存在（I5）" "1" "$prev_n"
assert_no_staged_residue "成功后无暂存 vhost 残留"
if grep -q "vsn=1.0.0" "$FAKE_ROOT/www/wwwroot/cs.imboy.test/activations.log" 2>/dev/null; then
  ok "activations.log 记录版本（无敏感值）"
else
  bad "activations.log 未记录" "$(cat "$FAKE_ROOT/www/wwwroot/cs.imboy.test/activations.log" 2>/dev/null)"
fi

# =============================================================================
echo "== A04. 故障点回滚矩阵 =="
# E1 构建失败
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=build run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count BLUEGREEN)" = 0 ] && [ "$(event_count SWAP_SYMLINK)" = 0 ]; then
  ok "E1 build 失败：不触蓝绿、不触远端激活"
else
  bad "E1 build 失败传播" "rc=$rc bg=$(event_count BLUEGREEN)"
fi
assert_eq "E1 symlink 保持旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E1 vhost 保持旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"

# E2 rsync 上传失败
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=rsync run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count BLUEGREEN)" = 0 ] && [ "$(event_count VERIFY_RELEASE)" = 0 ]; then
  ok "E2 rsync 失败：不校验、不进蓝绿"
else
  bad "E2 rsync 失败传播" "rc=$rc"
fi
assert_eq "E2 symlink 保持旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E2 vhost 保持旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"

# E3 Backend 蓝绿失败（I1/I2：backend 失败绝不激活 Widget）
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=backend run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count BLUEGREEN)" = 1 ] && [ "$(event_count SWAP_SYMLINK)" = 0 ] && [ "$(event_count SWAP_VHOST)" = 0 ]; then
  ok "E3 backend 失败：蓝绿被真实调用恰一次且零激活"
else
  bad "E3 backend 失败传播" "rc=$rc swap=$(event_count SWAP_SYMLINK)"
fi
assert_eq "E3 symlink 保持旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E3 vhost 保持旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"
if [ ! -f "$FAKE_ROOT/etc/nginx/cs.imboy.test.conf.cs-bak-00000000000000" ]; then
  ok "E3 backend 失败发生在激活前（无 vhost 备份副作用）"
fi

# E4 staged vhost 校验（nginx -t/证书）失败 → 激活前失败
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=nginx_t run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count BLUEGREEN)" = 0 ] && [ "$(event_count SWAP_SYMLINK)" = 0 ]; then
  ok "E4 staged nginx -t 失败：不进蓝绿、零激活"
else
  bad "E4 staged nginx -t 失败传播" "rc=$rc"
fi
assert_eq "E4 symlink 保持旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E4 vhost 保持旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"
assert_no_staged_residue "E4 暂存 vhost 已清理（I3）"

# E5 激活阶段最终 nginx -t 失败（reload 门内 -t）→ 双恢复 + backend 保留（I2）
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=nginx_t2 run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count BLUEGREEN)" = 1 ] \
   && [ "$(event_count SWAP_SYMLINK)" = 1 ] && [ "$(event_count SWAP_VHOST)" = 1 ] \
   && [ "$(event_count RESTORE_SYMLINK)" = 1 ] && [ "$(event_count RESTORE_VHOST)" = 1 ]; then
  ok "E5 激活内 nginx -t 失败：已切换面全部恢复、backend 成功保留"
else
  bad "E5 激活内 nginx -t 失败回滚" "rc=$rc swap=$(event_count SWAP_SYMLINK) rest=$(event_count RESTORE_SYMLINK)"
fi
assert_eq "E5 symlink 恢复为旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E5 vhost 恢复为旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"
assert_no_staged_residue "E5 暂存 vhost 已清理（I3）"

# E6 激活后 nginx reload 失败 → 双恢复
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=nginx_reload run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count RESTORE_SYMLINK)" = 1 ] && [ "$(event_count RESTORE_VHOST)" = 1 ]; then
  ok "E6 reload 失败：旧 symlink/vhost 双恢复"
else
  bad "E6 reload 失败回滚" "rc=$rc rest_symlink=$(event_count RESTORE_SYMLINK) rest_vhost=$(event_count RESTORE_VHOST)"
fi
assert_eq "E6 symlink 恢复为旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E6 vhost 恢复为旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"
if [ -d "$FAKE_ROOT/www/wwwroot/cs.imboy.test/releases" ]; then
  ok "E6 新 release 目录保留（I4）"
else
  bad "E6 release 目录被误删" ""
fi

# E7 smoke 失败 → 双恢复 + release 保留 + 不写激活台账
setup_fake_fs upgrade; write_env
MOCK_SMOKE_OK=0 run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count SMOKE)" = 1 ] && [ "$(event_count RESTORE_SYMLINK)" = 1 ] && [ "$(event_count RESTORE_VHOST)" = 1 ]; then
  ok "E7 smoke 失败：恢复旧 symlink/vhost"
else
  bad "E7 smoke 失败回滚" "rc=$rc"
fi
assert_eq "E7 symlink 恢复为旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E7 vhost 恢复为旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"
new_release_n="$(find "$FAKE_ROOT/www/wwwroot/cs.imboy.test/releases" -mindepth 1 -maxdepth 1 -type d ! -name old-release | wc -l | tr -d ' ')"
assert_eq "E7 smoke 失败后新 release 保留（I4）" "1" "$new_release_n"
if [ ! -f "$FAKE_ROOT/www/wwwroot/cs.imboy.test/activations.log" ]; then
  ok "E7 smoke 失败不写激活台账"
else
  bad "E7 激活台账被误写" ""
fi

# E8 首次安装失败 → 无残余 active vhost / symlink（I3）
setup_fake_fs first-install; write_env
# shellcheck disable=SC1007  # 空值 env 前缀 = 首次安装无活动 upstream
MOCK_UPSTREAM_PRE= MOCK_SMOKE_OK=0 run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ]; then
  ok "E8 首次安装失败退出非零"
else
  bad "E8 首次安装失败应退出非零" ""
fi
if [ "$(current_symlink_target)" = "<absent>" ]; then
  ok "E8 首次安装失败无 current symlink 残留"
else
  bad "E8 current symlink 残留" "$(current_symlink_target)"
fi
if [ "$(cs_vhost_content)" = "<absent>" ]; then
  ok "E8 首次安装失败无 active vhost 残留"
else
  bad "E8 vhost 残留" "$(cs_vhost_content)"
fi
assert_no_staged_residue "E8 暂存文件零残留"

# E9 symlink 切换失败 → 不替换 vhost
setup_fake_fs upgrade; write_env
MOCK_FAIL_AT=swap_symlink run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ] && [ "$(event_count SWAP_VHOST)" = 0 ]; then
  ok "E9 symlink 切换失败：vhost 不被替换"
else
  bad "E9 symlink 失败传播" "rc=$rc swap_vhost=$(event_count SWAP_VHOST)"
fi
assert_eq "E9 symlink 保持旧指向" "releases/old-release" "$(current_symlink_target)"
assert_eq "E9 vhost 保持旧内容" "old-cs-vhost-content" "$(cs_vhost_content)"

# E10 非 -l：陈旧产物必须被当前源码构建覆盖。
setup_fake_fs upgrade; write_env
mkdir -p "$FAKE_ROOT/admin-src/dist-widget/widget" "$FAKE_ROOT/admin-src/dist-widget/assets"
printf 'loader-js\n' >"$FAKE_ROOT/admin-src/dist-widget/loader.js"
printf '{"files":[]}\n' >"$FAKE_ROOT/admin-src/dist-widget/manifest.json"
shasum -a 256 "$FAKE_ROOT/admin-src/dist-widget/manifest.json" | awk '{print $1}' >"$FAKE_ROOT/admin-src/dist-widget/manifest.sha256"
printf 'ok\n' >"$FAKE_ROOT/admin-src/dist-widget/health.txt"
printf '<html>w</html>\n' >"$FAKE_ROOT/admin-src/dist-widget/widget/index.html"
printf 'x' >"$FAKE_ROOT/admin-src/dist-widget/assets/a.js"
run_component cs -v --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -eq 0 ] && [ "$(event_count BUN_BUILD_WIDGET)" = 1 ] \
   && grep -q "\"source_head\":\"$TEST_SOURCE_HEAD\"" "$FAKE_ROOT/admin-src/dist-widget/manifest.json" \
   && [ "$(event_count SMOKE)" = 1 ]; then
  ok "E10 非 -l 会重建陈旧产物并绑定当前源码 HEAD"
else
  bad "E10 非 -l 源码重建" "rc=$rc bun=$(event_count BUN_BUILD_WIDGET)"
fi

# =============================================================================
echo "== A05. verbose 脱敏（I7） =="
setup_fake_fs upgrade; write_env
run_component cs -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ]; then bad "A05 前置：成功路径" "$(tail -3 "$OUT")"; fi
leak=0
grep -q "supersecret-cookie-value" "$OUT" && { bad "verbose 泄漏 cookie 明文" ""; leak=1; }
grep -q "supersecret-key-material" "$OUT" && { bad "verbose 泄漏证书私钥内容" ""; leak=1; }
grep -q "BEGIN PRIVATE KEY" "$OUT" && { bad "verbose 泄漏私钥 PEM 头" ""; leak=1; }
[ "$leak" = 0 ] && ok "verbose 输出零 cookie/私钥/PEM 明文"
if grep -q "cookie=su\*\*\*" "$OUT"; then
  ok "cookie 仅以脱敏占位出现"
else
  bad "cookie 脱敏占位缺失" "$(grep -o 'cookie=[^|]*' "$OUT" | head -1)"
fi

# =============================================================================
echo "== A06. all -l：backend 恰一次、顺序 api→admin→cs；既有组件不回归 =="
setup_fake_fs upgrade; write_env
run_component all -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -ne 0 ]; then bad "all -l 应成功" "$(tail -5 "$OUT")"; fi
assert_event_count "all -l 蓝绿恰一次（backend 不重复部署）" BLUEGREEN 1
assert_event_count "all -l admin 构建恰一次" BUN_BUILD_ADMIN 1
assert_event_count "all -l widget 构建恰一次" BUN_BUILD_WIDGET 1
assert_event_count "all -l 统一 readiness 恰一次" READINESS 1
assert_steps_equal_contract "all -l 中 cs 段步骤序列完整"
bg_line="$(grep -an '^BLUEGREEN' "$MOCK_LOG" | head -1 | cut -d: -f1)"
admin_line="$(grep -an '^BUN_BUILD_ADMIN' "$MOCK_LOG" | head -1 | cut -d: -f1)"
widget_line="$(grep -an '^BUN_BUILD_WIDGET' "$MOCK_LOG" | head -1 | cut -d: -f1)"
swap_line="$(grep -an '^SWAP_SYMLINK' "$MOCK_LOG" | head -1 | cut -d: -f1)"
if [ -n "$bg_line" ] && [ -n "$admin_line" ] && [ -n "$widget_line" ] && [ -n "$swap_line" ] \
   && [ "$bg_line" -lt "$admin_line" ] && [ "$admin_line" -lt "$widget_line" ] && [ "$widget_line" -lt "$swap_line" ]; then
  ok "all -l 总顺序 = api → admin → cs artifact/gateway"
else
  bad "all -l 总顺序错误" "bg=$bg_line admin=$admin_line widget=$widget_line swap=$swap_line"
fi

# 既有组件回归：api -v -l / admin 单独运行
setup_fake_fs upgrade; write_env
run_component api -v -l --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -eq 0 ] && [ "$(event_count BLUEGREEN)" = 1 ] && [ "$(event_count READINESS)" = 1 ] \
   && [ "$(event_count SWAP_SYMLINK)" = 0 ] && [ "$(event_count BUN_BUILD_WIDGET)" = 0 ]; then
  ok "回归: api -v -l 蓝绿恰一次、统一 readiness 恰一次、零 CS 副作用"
else
  bad "回归: api -v -l" "rc=$rc bg=$(event_count BLUEGREEN) readiness=$(event_count READINESS) cs=$(event_count BUN_BUILD_WIDGET)"
fi
setup_fake_fs upgrade; write_env
run_component admin --env-file "$ENV_FILE"
rc=$?
if [ "$rc" -eq 0 ] && [ "$(event_count ADMIN_REALPATH)" = 1 ] && [ "$(event_count BLUEGREEN)" = 0 ] && [ "$(event_count SMOKE)" = 0 ]; then
  ok "回归: admin 行为不变（无蓝绿、无 CS smoke）"
else
  bad "回归: admin" "rc=$rc realpath=$(event_count ADMIN_REALPATH)"
fi

# =============================================================================
echo "== A07. cs-widget 网关模板合同（Seat 嵌入路由，SC-OPS-A01..A04） =="
# 直接对 deploy/nginx/templates/cs-widget.conf.template 做 location 级断言：
# 既有 Widget 面不变量 + Seat 增量（/seat/ frame、/seat-assets/ 静态、四组
# API 精确放行、Seat SSE）。负例通过变异副本证明检查器能捕获回归。
CS_TEMPLATE="$PWD/deploy/nginx/templates/cs-widget.conf.template"

# 取含固定子串的第 nth 个 location 块（从 location 行到首个独立收尾大括号）；
# nth 缺省 1。location / 在模板出现两次（:80 301 跳转 + :443 兜底），需取第 2 个。
tpl_block_at() { # $1=file $2=fixed substring $3=occurrence(从1起)
  awk -v pat="$2" -v want="${3:-1}" 'index($0, pat) { hit++; if (hit == want) inf=1 } inf { print; if ($0 ~ /^[[:space:]]*}[[:space:]]*$/) exit }' "$1"
}
tpl_block() { # $1=file $2=fixed substring
  tpl_block_at "$1" "$2" 1
}

# 聚合检查器：输出 TPLERR 行；供正例（应零输出）与负例（应非零）共用
cs_template_contract_errors() { # $1=template file
  local f="$1" blk
  # Seat 动态 frame：backend、300s 超时对齐 /w/、零 add_header、零 buffering 开关
  blk="$(tpl_block "$f" 'location ^~ /seat/ {')"
  [ -n "$blk" ] || echo "TPLERR: 缺 location ^~ /seat/（Seat 动态 frame）"
  if [ -n "$blk" ]; then
    printf '%s\n' "$blk" | grep -qF 'proxy_pass http://imboy_backend:9800;' || echo "TPLERR: /seat/ 未指向 imboy_backend:9800"
    printf '%s\n' "$blk" | grep -qF 'proxy_read_timeout 300s;'  || echo "TPLERR: /seat/ 缺 300s 读超时"
    printf '%s\n' "$blk" | grep -qF 'proxy_send_timeout 300s;'  || echo "TPLERR: /seat/ 缺 300s 写超时"
    printf '%s\n' "$blk" | grep -qF 'proxy_set_header Host              $http_host;' || echo "TPLERR: /seat/ Host 头须为 \$http_host"
    printf '%s\n' "$blk" | grep -q  'add_header'      && echo "TPLERR: /seat/ 出现 add_header（网关零注入）"
    printf '%s\n' "$blk" | grep -q  'proxy_buffering' && echo "TPLERR: /seat/ 出现 proxy_buffering（HTML 无需，镜像 /w/）"
  fi
  # Seat 静态别名：widget 容器、零缓存头注入（容器按 S6 下发）
  blk="$(tpl_block "$f" 'location ^~ /seat-assets/ {')"
  [ -n "$blk" ] || echo "TPLERR: 缺 location ^~ /seat-assets/（Seat 稳定别名静态面）"
  if [ -n "$blk" ]; then
    printf '%s\n' "$blk" | grep -qF 'proxy_pass http://imboy_widget:8080;' || echo "TPLERR: /seat-assets/ 未指向 imboy_widget:8080"
    printf '%s\n' "$blk" | grep -q  'add_header'     && echo "TPLERR: /seat-assets/ 出现 add_header（缓存头必须由容器下发）"
    printf '%s\n' "$blk" | grep -q  'Cache-Control'  && echo "TPLERR: /seat-assets/ 出现 Cache-Control（网关不得覆写）"
  fi
  # Seat SSE：关缓冲/关缓存 + 3600s；正则须与 imboy_router 真实端点合同
  # （REVIEW-2 P1 机制闭环：此前静态合同只锁 harness 渲染产物，模板正则指向
  # 不存在的 /api/v1/cs/sessions/:id/events 时无任何断言拦截）。
  blk="$(tpl_block "$f" 'location ~ ^/api/v1/cs/organizations/')"
  [ -n "$blk" ] || echo "TPLERR: 缺 Seat SSE 正则 location"
  if [ -n "$blk" ]; then
    printf '%s\n' "$blk" | grep -qF 'proxy_buffering off;'          || echo "TPLERR: Seat SSE 缺 proxy_buffering off"
    printf '%s\n' "$blk" | grep -qF 'proxy_cache off;'              || echo "TPLERR: Seat SSE 缺 proxy_cache off"
    printf '%s\n' "$blk" | grep -qF 'chunked_transfer_encoding on;' || echo "TPLERR: Seat SSE 缺 chunked_transfer_encoding on"
    printf '%s\n' "$blk" | grep -qF 'proxy_read_timeout 3600s;'     || echo "TPLERR: Seat SSE 读超时须 3600s"
    printf '%s\n' "$blk" | grep -qF 'proxy_send_timeout 3600s;'     || echo "TPLERR: Seat SSE 写超时须 3600s"
  fi
  # 模板 ↔ 真实路由表对照（对照粒度）：把路由表 seat SSE 端点的每个 :param
  # 槽机械替换为模板既定单段字符类 [0-9A-Za-z_-]+ 得到骨架；模板 SSE
  # location 行去掉 ^/$ 锚与反斜杠转义后须与骨架逐字相等 —— 字面段
  # organizations/seats/me/events（含 api/v1/cs 前缀）逐段对应，参数槽允许
  # 转义写法差异（如 \-），段序列/段数不允许任何差异。
  ROUTE_SEAT_SSE="$(grep -oE '"/api/v1/cs/organizations/:org_id/seats/me/events"' "$PWD/src/imboy_router.erl" 2>/dev/null | head -1 | tr -d '"')"
  [ -n "$ROUTE_SEAT_SSE" ] || echo "TPLERR: 路由表缺 seat SSE 端点（src/imboy_router.erl: organizations/:org_id/seats/me/events）"
  if [ -n "$ROUTE_SEAT_SSE" ] && [ -n "$blk" ]; then
    want_skel="$(printf '%s\n' "$ROUTE_SEAT_SSE" | sed -E 's/:[A-Za-z_][A-Za-z0-9_]*/[0-9A-Za-z_-]+/g')"
    got_pat="$(printf '%s\n' "$blk" | head -1 | sed -E 's/.*location[[:space:]]+~[[:space:]]+\^//; s/\$[[:space:]]*\{.*//' | tr -d '\\')"
    [ "$got_pat" = "$want_skel" ] || echo "TPLERR: Seat SSE 正则与路由表漂移 (want=$want_skel got=$got_pat)"
  fi
  # 历史漂移形态负向禁止（P1 原 bug 形态）：location 行出现
  # /api/v1/cs/sessions/…/events 即 FAIL —— 路由表无此端点；Widget 块
  # /cs/widget/sessions/ 不含该子串不受影响；模板头部注释中的历史字样
  # 非 nginx 指令，不计。
  grep -qE '^[[:space:]]*location .*/api/v1/cs/sessions/[^[:space:]]*/events' "$f" \
    && echo "TPLERR: 出现历史漂移形态 cs/sessions/…/events（路由表无此端点）"
  # 四组 API 精确放行（含既有 widget 组）
  local loc
  for loc in 'location /api/v1/cs/widget/ {' 'location /api/v1/cs/ {' \
             'location /api/v1/passport/qr_login/ {' \
             'location /api/v1/enterprise/conversations/ {' \
             'location /api/v1/enterprise/organizations/ {'; do
    blk="$(tpl_block "$f" "$loc")"
    if [ -z "$blk" ]; then echo "TPLERR: 缺 API 精确前缀 location: $loc"; continue; fi
    printf '%s\n' "$blk" | grep -qF 'proxy_pass http://imboy_backend:9800;' || echo "TPLERR: $loc 未指向 imboy_backend:9800"
  done
  # 禁止项：全 /api/v1/ 通配（两种书写形态）
  grep -qE '^[[:space:]]*location /api/v1/ \{' "$f" && echo "TPLERR: 出现全 /api/v1/ 通配代理（禁止项）"
  grep -qE '^[[:space:]]*location /api/v1/[[:space:]]*$' "$f" && echo "TPLERR: 出现全 /api/v1/ 通配代理（禁止项，无大括号形态）"
  # 既有 Widget 面不变量（spot-check）
  blk="$(tpl_block "$f" 'location ~ ^/api/v1/cs/widget/sessions/')"
  printf '%s\n' "$blk" | grep -qF 'proxy_buffering off;' && printf '%s\n' "$blk" | grep -qF 'proxy_read_timeout 3600s;' \
    || echo "TPLERR: Widget SSE 块不变量漂移（buffering off/3600s）"
  blk="$(tpl_block "$f" 'location ^~ /w/ {')"
  printf '%s\n' "$blk" | grep -qF 'proxy_pass http://imboy_backend:9800;' && printf '%s\n' "$blk" | grep -qF 'proxy_read_timeout 300s;' \
    || echo "TPLERR: /w/ 动态 frame 块不变量漂移"
  blk="$(tpl_block_at "$f" 'location / {' 2)"
  printf '%s\n' "$blk" | grep -qF 'proxy_pass http://imboy_widget:8080;' || echo "TPLERR: location / 兜底不再指向静态容器（fail-closed 漂移）"
  grep -qF 'location = /v1/loader.js' "$f"  || echo "TPLERR: 缺 loader 静态面"
  grep -qF 'location ^~ /assets/' "$f"      || echo "TPLERR: 缺 assets 静态面"
  grep -qF 'location ^~ /widget/ {' "$f"    || echo "TPLERR: 缺 widget 壳静态面"
  # 安全头边界：server 级最小集恰一次；XFO/CSP/Cache-Control 零注入
  [ "$(grep -cF 'add_header Strict-Transport-Security' "$f")" = "1" ] || echo "TPLERR: HSTS 须 server 级唯一来源"
  grep -qF 'add_header X-Content-Type-Options nosniff' "$f" || echo "TPLERR: 缺 nosniff"
  grep -qF 'add_header Referrer-Policy' "$f"                || echo "TPLERR: 缺 Referrer-Policy"
  grep -q  'add_header X-Frame-Options' "$f"                && echo "TPLERR: 网关注入 XFO（禁止）"
  grep -q  'add_header Content-Security-Policy' "$f"        && echo "TPLERR: 网关注入 CSP（禁止）"
  grep -q  'add_header Cache-Control' "$f"                  && echo "TPLERR: 网关注入 Cache-Control（缓存头归容器）"
  # upstream 计数：backend 9 处（2 SSE + /w/ + /seat/ + 5 组 API）；widget 5 处
  [ "$(grep -cF 'proxy_pass http://imboy_backend:9800;' "$f")" = "9" ] || echo "TPLERR: backend upstream 数量漂移"
  [ "$(grep -cF 'proxy_pass http://imboy_widget:8080;' "$f")" = "5" ]  || echo "TPLERR: widget upstream 数量漂移"
}

if [ -f "$CS_TEMPLATE" ]; then
  TPL_ERRS="$(cs_template_contract_errors "$CS_TEMPLATE" | grep -c 'TPLERR' || true)"
  if [ "${TPL_ERRS:-1}" = "0" ]; then
    ok "模板合同检查器全绿（Seat frame/静态/SSE↔路由表对照/四组 API/负向禁止项/既有 Widget 面不变量）"
  else
    bad "模板合同检查器报错" "$(cs_template_contract_errors "$CS_TEMPLATE" | head -5 | tr '\n' ';')"
  fi

  # 逐项正例（可读性：失败时能直接定位漂移面）
  grep -qF 'location ^~ /seat/ {' "$CS_TEMPLATE" \
    && ok "A01 /seat/ 动态 frame 块存在" || bad "A01 /seat/ 块缺失" ""
  grep -qF 'location ^~ /seat-assets/ {' "$CS_TEMPLATE" \
    && ok "A01 /seat-assets/ 静态块存在" || bad "A01 /seat-assets/ 块缺失" ""
  for loc in '/api/v1/cs/widget/' '/api/v1/cs/' '/api/v1/passport/qr_login/' \
             '/api/v1/enterprise/conversations/' '/api/v1/enterprise/organizations/'; do
    grep -qF "location $loc {" "$CS_TEMPLATE" \
      && ok "A02 API 精确前缀存在: $loc" || bad "A02 API 前缀缺失: $loc" ""
  done
  if grep -qE '^[[:space:]]*location /api/v1/ \{' "$CS_TEMPLATE"; then
    bad "A02 全 /api/v1/ 通配出现（禁止项）" ""
  else
    ok "A02 无全 /api/v1/ 通配（四组之外不达 backend）"
  fi

  # A03 正例对照：模板 seat SSE 正则 ↔ imboy_router 真实路由表（REVIEW-2 P1
  # 防再漂移）。粒度同检查器：路由表端点 :param 槽机械替换为模板既定单段
  # 字符类 [0-9A-Za-z_-]+ 得骨架；模板 SSE location 行去 ^/$ 锚与反斜杠转义
  # 后逐字相等 —— 字面段逐段对应，参数槽允许转义差异。
  ROUTE_SEAT_SSE="$(grep -oE '"/api/v1/cs/organizations/:org_id/seats/me/events"' src/imboy_router.erl 2>/dev/null | head -1 | tr -d '"')"
  TPL_SEAT_SSE_PAT="$(grep -E '^[[:space:]]*location ~ \^.*seats/me/events' "$CS_TEMPLATE" 2>/dev/null | head -1 \
    | sed -E 's/.*location[[:space:]]+~[[:space:]]+\^//; s/\$[[:space:]]*\{.*//' | tr -d '\\')"
  ROUTE_SEAT_SSE_SKEL="$(printf '%s' "$ROUTE_SEAT_SSE" | sed -E 's/:[A-Za-z_][A-Za-z0-9_]*/[0-9A-Za-z_-]+/g')"
  if [ -n "$ROUTE_SEAT_SSE" ] && [ "$TPL_SEAT_SSE_PAT" = "$ROUTE_SEAT_SSE_SKEL" ]; then
    ok "A03 模板↔路由表对照: seat SSE 正则骨架 = 真实端点：$ROUTE_SEAT_SSE_SKEL"
  else
    bad "A03 模板↔路由表对照: seat SSE 正则与真实端点漂移" "route=[$ROUTE_SEAT_SSE] skel=[$ROUTE_SEAT_SSE_SKEL] tpl=[$TPL_SEAT_SSE_PAT]"
  fi

  # 负例：变异副本必须被检查器捕获（证明回归可测）
  tpl_neg() { # $1=描述 $2=sed 表达式（BSD sed 兼容）
    local m="$TMP_ROOT/tpl-mutant.$$" n
    sed -E "$2" "$CS_TEMPLATE" >"$m"
    n="$(cs_template_contract_errors "$m" | grep -c 'TPLERR' || true)"
    rm -f "$m"
    if [ "${n:-0}" -ge 1 ]; then ok "模板负例被捕获: $1"; else bad "模板负例未被捕获: $1" "检查器误放行"; fi
  }
  tpl_neg "删除 /seat/ 动态块"           '/location \^~ \/seat\/ \{/,/^[[:space:]]*}[[:space:]]*$/d'
  tpl_neg "删除 /seat-assets/ 静态块"    '/location \^~ \/seat-assets\/ \{/,/^[[:space:]]*}[[:space:]]*$/d'
  tpl_neg "Seat SSE 关缓冲被移除"        '/proxy_buffering off;/d'
  tpl_neg "qr_login 组被改名（组缺失）"  's/location \/api\/v1\/passport\/qr_login\/ \{/location \/api\/v1\/passport\/qr_loginX \{/'
  tpl_neg "cs/ 前缀被放大为全通配"       's/location \/api\/v1\/cs\/ \{/location \/api\/v1\/ \{/'
  # seat-assets 被注入缓存头：awk 注入（BSD sed 替换串不支持 \n）
  m_inject="$TMP_ROOT/tpl-mutant-inject.$$"
  awk '{print} /location \^~ \/seat-assets\/ \{/ {print "            add_header Cache-Control \"public, max-age=31536000, immutable\" always;"}' \
    "$CS_TEMPLATE" >"$m_inject"
  n_inject="$(cs_template_contract_errors "$m_inject" | grep -c 'TPLERR' || true)"
  rm -f "$m_inject"
  if [ "${n_inject:-0}" -ge 1 ]; then
    ok "模板负例被捕获: seat-assets 被注入缓存头"
  else
    bad "模板负例未被捕获: seat-assets 被注入缓存头" "检查器误放行"
  fi
  # 历史漂移形态负例（P1 原 bug）：向真实块之后注入一条指向不存在端点的
  # cs/sessions/…/events location 行，其余面不动 —— 检查器必须 FAIL，且由
  # 路由表对照断言（而非缺块/计数旁路）捕获。
  m_drift="$TMP_ROOT/tpl-mutant-drift.$$"
  awk '{print} /seats\/me\/events\$ \{/ {print "    location ~ ^/api/v1/cs/sessions/[0-9A-Za-z_-]+/events$ {"}' \
    "$CS_TEMPLATE" >"$m_drift"
  n_drift="$(cs_template_contract_errors "$m_drift" | grep -c 'TPLERR' || true)"
  rm -f "$m_drift"
  if [ "${n_drift:-0}" -ge 1 ]; then
    ok "模板负例被捕获: 注入历史漂移形态 cs/sessions/…/events"
  else
    bad "模板负例未被捕获: 注入历史漂移形态 cs/sessions/…/events" "检查器误放行"
  fi
else
  bad "cs-widget 模板缺失" "$CS_TEMPLATE"
fi

# =============================================================================
echo "== A08. preflight base_url ↔ CS_WIDGET_DOMAIN 比对检查（SC-EMBED §5） =="
# deploy/preflight.sh 不可整体 source（主流程 source 时即执行并 exit）；沿用 A00
# 「提取纯函数 + 测试桩」方式：awk 抠出 check_base_url_matches_cs_widget_domain
# 函数定义，配 ok/err 桩独立执行。断言面：正例过检、缺失口径（交 check_var 不
# 重复计）、三类漂移负例必抓、错误文案含 §5 修复指引；变异自证（删 err 分支 /
# 弱化为仅 scheme 比对后负例不再报错）证明本组断言真实依赖检查逻辑 —— 检查被
# 删除或弱化时本测试组必然 FAIL，非恒真。
PREFLIGHT_SH="$PWD/deploy/preflight.sh"

pf_fn() { # $1=file $2=函数名 → 打印函数定义（函数头行到首个顶格 }）；同 A07 tpl_block_at 手法
  awk -v pat="$2() {" 'index($0, pat) { inf=1 } inf { print; if ($0 ~ /^\}[[:space:]]*$/) exit }' "$1"
}

PF_STUB="$TMP_ROOT/pf_check_stub.$$"
cat >"$PF_STUB" <<'PFSTUB'
ERRORS=0
ok() { :; }
warn() { :; }
info() { :; }
err() { ERRORS=$((ERRORS+1)); printf 'PFERR: %s\n' "$1"; }
PFSTUB

pf_base_url_errors() { # $1=preflight文件 $2=base_url $3=domain → PFERR 行数
  (
    # shellcheck disable=SC1090
    source "$PF_STUB"
    eval "$(pf_fn "$1" check_base_url_matches_cs_widget_domain)"
    check_base_url_matches_cs_widget_domain "IMBOY_BASE_URL" "$2" "CS_WIDGET_DOMAIN" "$3"
  ) | grep -c 'PFERR' || true
}

if [ -f "$PREFLIGHT_SH" ] && [ -n "$(pf_fn "$PREFLIGHT_SH" check_base_url_matches_cs_widget_domain)" ]; then
  # 正例：恰为 https://<CS_WIDGET_DOMAIN>（合法形态）必须过检
  assert_eq "preflight 正例: https://<CS_WIDGET_DOMAIN> 过检" "0" \
    "$(pf_base_url_errors "$PREFLIGHT_SH" "https://cs.imboy.test" "cs.imboy.test")"
  # 缺失口径：任一值缺失不在此计错（交由 check_var 报告，preflight 现有口径）
  assert_eq "preflight 缺失口径: base_url 空值不重复计错" "0" \
    "$(pf_base_url_errors "$PREFLIGHT_SH" "" "cs.imboy.test")"
  assert_eq "preflight 缺失口径: 域空值不重复计错" "0" \
    "$(pf_base_url_errors "$PREFLIGHT_SH" "https://cs.imboy.test" "")"
  # 负例：http:// / 尾斜杠 / 带路径 / 域不符，检查器必须报错
  for pf_drift in "http://cs.imboy.test" "https://cs.imboy.test/" "https://cs.imboy.test/api" "https://api.imboy.test"; do
    n="$(pf_base_url_errors "$PREFLIGHT_SH" "$pf_drift" "cs.imboy.test")"
    if [ "${n:-0}" -ge 1 ]; then
      ok "preflight 负例被捕获: base_url=$pf_drift"
    else
      bad "preflight 负例未被捕获: base_url=$pf_drift" "检查器误放行"
    fi
  done
  # 可读错误与修复指引：负例文案必须指向部署文档 §5
  PF_ERR_OUT="$(
    # shellcheck disable=SC1090
    source "$PF_STUB"
    eval "$(pf_fn "$PREFLIGHT_SH" check_base_url_matches_cs_widget_domain)"
    check_base_url_matches_cs_widget_domain "IMBOY_BASE_URL" "https://api.imboy.test" "CS_WIDGET_DOMAIN" "cs.imboy.test"
  )"
  case "$PF_ERR_OUT" in
    *'deploy/cs-seat-console-embed.md §5'*) ok "错误文案含修复指引（deploy/cs-seat-console-embed.md §5）" ;;
    *) bad "错误文案缺 §5 修复指引" "$(printf '%s' "$PF_ERR_OUT" | head -1)" ;;
  esac

  # 变异自证 1（err 分支变 no-op）：同一负例必须不再报错 —— 证明负例断言真实
  # 依赖检查器的 err 调用（检查被删除时本测试组必然 FAIL）。手术必须保留合法
  # 语法（把 err 调用替换为冒号 no-op 而非删行——删行会留下空 then 分支，bash
  # 3.2 eval 直接语法错 → 函数未定义 → 「不再报错」空洞成立，证明变装饰）；
  # 变异体必须仍可定义才计证明（手术破坏语法时本组 FAIL，不假绿）。
  PF_MUTANT1="$TMP_ROOT/preflight-mutant1.$$"
  sed 's/^\( *\)err "/\1: "mutated-err-removed/' "$PREFLIGHT_SH" >"$PF_MUTANT1"
  PF_M1_DEFINED="$(
    # shellcheck disable=SC1090
    source "$PF_STUB"
    eval "$(pf_fn "$PF_MUTANT1" check_base_url_matches_cs_widget_domain)" 2>/dev/null
    type check_base_url_matches_cs_widget_domain >/dev/null 2>&1 && echo yes || echo no
  )"
  if [ "$PF_M1_DEFINED" = "yes" ]; then
    assert_eq "变异自证1: err 分支变 no-op 后负例不再报错（断言非恒真）" "0" \
      "$(pf_base_url_errors "$PF_MUTANT1" "https://api.imboy.test" "cs.imboy.test")"
  else
    bad "变异自证1 的变异体无法定义（手术破坏语法，证明失效）" "function undefined"
  fi
  # 变异自证 2（严格相等弱化为仅 scheme 检查）：域不符形态必须放行 —— 证明
  # 负例断言真实依赖「恰为 https://<域>」的严格比对（弱化时本测试组必然 FAIL）。
  PF_MUTANT2="$TMP_ROOT/preflight-mutant2.$$"
  sed 's|"$url" != "https://${dom}"|"$url" != https://*|' "$PREFLIGHT_SH" >"$PF_MUTANT2"
  assert_eq "变异自证2: 比对弱化为仅 scheme 后域不符不再报错" "0" \
    "$(pf_base_url_errors "$PF_MUTANT2" "https://api.imboy.test" "cs.imboy.test")"
  rm -f "$PF_MUTANT1" "$PF_MUTANT2"
else
  bad "preflight.sh 缺 check_base_url_matches_cs_widget_domain" "$PREFLIGHT_SH"
fi

# =============================================================================
echo
echo "总计: PASS=$PASS FAIL=$FAIL"
[ "$FAIL" -eq 0 ]
