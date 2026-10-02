#!/usr/bin/env bash
# =============================================================================
# customer_service_deploy_test.sh — CS 托管 Widget 部署端到端离线 harness
# （CSD-TEST-01 / run 20260920t150315）
# -----------------------------------------------------------------------------
# 与 A5 的 cs_deploy_unit_test.sh（函数级、远端操作按标签逐条模拟）互补：
# 本 harness 是**进程级 + 文件系统级**验证——从真实仓库入口
#   bash scripts/imboy-deploy.sh cs -v -l --env-file <fixture env>
# 驱动完整事务。fake ssh 不做标签模拟，而是把远端命令中的批准路径前缀
# （/www/wwwroot/ /etc/nginx/ /etc/letsencrypt/）sed 翻译到隔离 fake 远端树后
# **真实执行**（bash -c），stdout 再反向翻译——因此 ln/mv -T/readlink/sha256sum
# 等副作用全部落在真实文件系统语义上，oracle = fake 调用序（ops.events）+
# fake 远端文件系统终态 + 退出码，而非 exit 0 本身。
#
# fake binaries（PATH 前置注入）：ssh scp rsync nginx curl certbot bun
#   + 兼容 shim：sha256sum(shasum) mv(-T) realpath(-e)。certbot 恒 fake 且断言
#   零调用（证明部署流程永不触碰真实证书签发）。全部离线，零真实网络。
#
# 验收覆盖（冻结合同 hosted-widget-contract-v1 §S7 + 计划卡 CSD-TEST-01）：
#   A01 happy path：build→stage→backend→activate→smoke 顺序精确且各恰一次；
#       backend=deploy_api（经 CS_DEPLOY_API_FN 注入点，A5 预留）恰一次。
#   A02 恶意输入（domain/path/symlink 目标/命令替换/反引号/换行）：SSH 前拒绝
#       ——connect/cmd/scp/rsync 计数器全零 + 非零退出。
#   A03 失败点恢复（build/rsync/backend/nginx-t/reload/smoke）：旧 symlink 指向
#       不变或恢复、旧 vhost 内容不变、新 release 保留、无半激活、非零退出。
#   A04 首次安装 vs 升级：首次安装失败零残余（无 current/无 active vhost）；
#       升级失败回滚旧指向；foreign release 在一切场景后仍存在。
#   A05 卫生：全部日志与 fake remote 终态 secret scan 为零（含诱饵
#       CS_TEST_CANARY_9f2b 只以 CS*** 出现）；migrate/restore 回归全绿；
#       deploy_sequence_test 记录结果（既有漂移对比由证据侧两份失败名单完成）。
#
# 可重复性：断言全部为确定性谓词（计数/顺序/文件终态），连跑两次输出一致；
# harness 自检 worktree 无残留（widget-dist 清理）。
# =============================================================================
set -uo pipefail

RUN_ID="20260920t150315"
cd "$(dirname "$0")/../.." || exit 1
REPO="$(pwd -P)"
ENTRY="scripts/imboy-deploy.sh"
LIB="scripts/lib/cs_deploy.sh"
FIX_BASE="$REPO/scripts/test/fixtures/cs_deploy/fake-remote"
FIX_OVERLAY="$REPO/scripts/test/fixtures/cs_deploy/fake-remote-upgrade-overlay"
CANARY="CS_TEST_CANARY_9f2b"

CS_ROOT_REL="www/wwwroot/cs.test.local"
OLD_RELEASE_NAME="20260101000000-old"
FOREIGN_RELEASE_NAME="20991231000000-foreign-unknown"
API_CONF_REL="etc/nginx/confs/imboy.api.conf"
CS_CONF_REL="etc/nginx/confs/imboy.cs.conf"
OLD_SYMLINK_TARGET="/www/wwwroot/cs.test.local/releases/$OLD_RELEASE_NAME"

PASS=0
FAIL=0
SCN="-"

pass() { PASS=$((PASS + 1)); printf 'PASS [%s] %s\n' "$SCN" "$*"; }
nope() { FAIL=$((FAIL + 1)); printf 'FAIL [%s] %s\n' "$SCN" "$*"; }

# ---------- 断言原语（全部落 PASS/FAIL 计数） ----------
ck_eq()   { local d="$1" a="$2" e="$3"; if [ "$a" = "$e" ]; then pass "$d (=$e)"; else nope "$d (got=$a want=$e)"; fi; }
ck_nenum() { local d="$1" a="$2" e="$3"; if [ "$a" -ne "$e" ] 2>/dev/null; then pass "$d (=$a!=$e)"; else nope "$d (got=$a, 要求≠$e)"; fi; }
ck_gt0()  { local d="$1" a="$2"; if [ "${a:-0}" -ge 1 ] 2>/dev/null; then pass "$d (=$a)"; else nope "$d (got=$a, 要求≥1)"; fi; }
ck_contains()     { local d="$1" f="$2" p="$3"; if grep -qF -- "$p" "$f" 2>/dev/null; then pass "$d"; else nope "$d (文件 $f 缺少: $p)"; fi; }
ck_not_contains() { local d="$1" f="$2" p="$3"; if grep -qF -- "$p" "$f" 2>/dev/null; then nope "$d (文件 $f 不应出现: $p)"; else pass "$d"; fi; }
ck_file_eq() { local d="$1" a="$2" b="$3"; if cmp -s "$a" "$b"; then pass "$d"; else nope "$d ($a 与 $b 内容不一致)"; fi; }
ck_present() { local d="$1" p="$2"; if [ -e "$p" ]; then pass "$d"; else nope "$d (缺失: $p)"; fi; }
ck_absent()  { local d="$1" p="$2"; if [ -e "$p" ]; then nope "$d (不应存在: $p)"; else pass "$d"; fi; }
ck_link()    { local d="$1" l="$2" want="$3" got
               got="$(readlink "$l" 2>/dev/null || printf '<none>')"
               # 统一剥去 fake 根前缀：翻译往返只影响绝对路径形式，语义以
               # 批准根内相对形式为准（原值/回滚重建值必须一致）
               want="${want#"$FROOT"}"; got="${got#"$FROOT"}"
               if [ "$got" = "$want" ]; then pass "$d"; else nope "$d (link=$l got=$got want=$want)"; fi; }
ck_no_residue() { # $1=fake-root；无 .cs-staged/.cs-final/.cs-new/.cs-restore 半配置残留
  local root="$1" hits
  hits="$(find "$root" \( -name '*.cs-staged-*' -o -name '*.cs-final-*' \
        -o -name 'current.cs-new' -o -name '*.cs-restore' \) 2>/dev/null | head -5)"
  if [ -z "$hits" ]; then pass "无半配置残留（staged/final/cs-new/restore 均不存在）"
  else nope "发现半配置残留: $hits"; fi
}

TMP_ROOT="$(mktemp -d "/tmp/imboy_csd_test_${RUN_ID}.XXXXXX")" || exit 1
# macOS /tmp → /private/tmp symlink：fake ssh 的 realpath/cd -P 会解析 symlink，
# 路径翻译往返必须基于规范化路径，否则批准根校验失配。
TMP_ROOT="$( cd "$TMP_ROOT" && pwd -P )" || exit 1
RUN_REPO="$TMP_ROOT/repo"
git clone -q --no-hardlinks "$REPO" "$RUN_REPO" || exit 1
# 发布入口会正确拒绝脏源码；harness 必须在隔离的 clean clone 中运行，不能让
# 调用者工作树里的无关 WIP 改变离线事务测试结果。
cp "$REPO/scripts/imboy-deploy.sh" "$RUN_REPO/scripts/imboy-deploy.sh" || exit 1
cp "$REPO/scripts/lib/blue_green_deploy.sh" "$RUN_REPO/scripts/lib/blue_green_deploy.sh" || exit 1
cp "$REPO/scripts/lib/cs_deploy.sh" "$RUN_REPO/scripts/lib/cs_deploy.sh" || exit 1
git -C "$RUN_REPO" add scripts/imboy-deploy.sh scripts/lib/blue_green_deploy.sh scripts/lib/cs_deploy.sh
GIT_AUTHOR_NAME=leeyi GIT_AUTHOR_EMAIL=leeyisoft@qq.com \
GIT_COMMITTER_NAME=leeyi GIT_COMMITTER_EMAIL=leeyisoft@qq.com \
  git -C "$RUN_REPO" -c commit.gpgsign=false commit --allow-empty -qm 'test: isolated deployment source' || exit 1
BUILD_SRC="$RUN_REPO/scripts/test/fixtures/cs_deploy/build-src"
ART_DIR="$BUILD_SRC/widget-dist"
ORIG_PATH="$PATH"
cleanup() {
  [ "${KEEP_TMP:-0}" = 1 ] || rm -rf -- "$TMP_ROOT"
  rm -rf -- "$ART_DIR" 2>/dev/null || true
}
trap cleanup EXIT INT TERM

rm -rf -- "$ART_DIR" 2>/dev/null || true   # 自愈：清掉上次中断可能的残留

# ---------- 运行期自签测试证书（零私钥入库；openssl 全本地） ----------
CERTS="$TMP_ROOT/certs"
mkdir -p "$CERTS"
if ! openssl req -x509 -newkey rsa:2048 -keyout "$CERTS/privkey.pem" \
       -out "$CERTS/fullchain.pem" -days 2 -nodes -subj "/CN=cs.test.local" \
       >/dev/null 2>&1; then
  echo "ABORT: openssl 自签证书生成失败（harness 无法继续）" >&2
  exit 1
fi

# =============================================================================
# fake binaries（每场景独立 bin 目录；共享 ops.events 单一日志，SEQ=行号单调）
# =============================================================================
gen_fake_bins() { # $1 = bindir
  local bd="$1"
  mkdir -p "$bd"

  # ---- ssh：翻译远端命令路径 → fake 树真实执行 → stdout 反向翻译 ----
  cat >"$bd/ssh" <<'FB'
#!/usr/bin/env bash
# fake ssh（CSD-TEST-01 run 20260920t150315，进程级远端模拟器；零网络）
FB_LOG="${FAKE_LOG:?FAKE_LOG required}"
FB_ROOT="${FAKE_ROOT:?FAKE_ROOT required}"
FB_SED="${FAKE_SED:?FAKE_SED required}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
prev=0; dest=""; cmd=""
for a in "$@"; do
  if [ "$prev" = 1 ]; then prev=0; continue; fi
  case "$a" in
    -p|-o|-i|-l|-F|-W|-L|-R|-D|-b|-c|-E|-e|-O) prev=1 ;;
    -*) : ;;
    *) if [ -z "$dest" ] && [ -z "$cmd" ]; then dest="$a"; else cmd="$cmd $a"; fi ;;
  esac
done
cmd="${cmd# }"
if [ -z "$cmd" ]; then
  case " $* " in
    *" -O "*) printf '%s\tctl\t%s\n' "$FB_N" "$dest" >>"$FB_LOG" ;;
    *)        printf '%s\tconnect\t%s\n' "$FB_N" "$dest" >>"$FB_LOG" ;;
  esac
  exit 0
fi
tag="$(printf '%s' "$cmd" | sed -n '1p')"
case "$tag" in
  ": "*) : ;;
  *) tag="BARE:$(printf '%s' "$cmd" | cut -c1-32)" ;;
esac
printf '%s\tcmd\t%s\n' "$FB_N" "$tag" >>"$FB_LOG"
[ -f "$FB_SED" ] || exit 90
tcmd="$(printf '%s\n' "$cmd" | sed -f "$FB_SED")"
tmpout="$(mktemp "${TMPDIR:-/tmp}/fakessh-out-20260920t150315.XXXXXX")" || exit 91
rc=0
bash -c "$tcmd" >"$tmpout" || rc=$?
sed "s|$FB_ROOT/|/|g" "$tmpout"
rm -f "$tmpout"
exit $rc
FB

  # ---- scp：上传单个文件（翻译远端路径） ----
  cat >"$bd/scp" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}" FB_SED="${FAKE_SED:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\tscp\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
[ "${FAIL_AT:-}" = "scp" ] && exit 1
prev=0; src=""; dst=""
for a in "$@"; do
  if [ "$prev" = 1 ]; then prev=0; continue; fi
  case "$a" in -P|-o|-i|-F) prev=1 ;; -*) : ;; *) if [ -z "$src" ]; then src="$a"; else dst="$a"; fi ;; esac
done
case "$dst" in *:*) remote="${dst#*:}" ;; *) exit 2 ;; esac
rpath="$(printf '%s' "$remote" | sed -f "$FB_SED")"
# check_prod_config 守卫（1b2ca499）把临时 escript 传到远端 /tmp：fake 环境
# 本地与"远端"同路径，真实 cp 会拒绝 identical 拷贝——语义上文件已在远端
# 就位，直接成功。
[ "$src" = "$rpath" ] && exit 0
mkdir -p "$(dirname "$rpath")" || exit 3
cp "$src" "$rpath" || exit 4
exit 0
FB

  # ---- escript：check_prod_config 远端守卫桩。fake ssh 会真实执行守卫命令，
  #      `command -v escript` 命中本桩即回 OK（守卫逻辑属生产路径，离线桩只
  #      需保证预检不改变部署事务时序）。
  cat >"$bd/escript" <<'FB'
#!/usr/bin/env bash
printf 'CONFIG_KEYS_OK\n'
exit 0
FB

  # ---- rsync：上传 release 目录（翻译远端路径；tar 管道复制内容） ----
  cat >"$bd/rsync" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}" FB_SED="${FAKE_SED:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\trsync\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
[ "${FAIL_AT:-}" = "rsync" ] && exit 1
skip=0; src=""; dst=""
for a in "$@"; do
  if [ "$skip" = 1 ]; then skip=0; continue; fi
  case "$a" in
    -e|--rsh|--exclude) skip=1 ;;
    -*) : ;;
    *) if [ -z "$src" ]; then src="$a"; else dst="$a"; fi ;;
  esac
done
case "$dst" in *:*) remote="${dst#*:}" ;; *) exit 2 ;; esac
rpath="$(printf '%s' "$remote" | sed -f "$FB_SED")"
mkdir -p "$rpath" || exit 3
( cd "$src" && tar cf - . ) | ( cd "$rpath" && tar xf - ) || exit 4
exit 0
FB

  # ---- nginx：-t 校验 include 目标存在；-s reload 可注入失败 ----
  cat >"$bd/nginx" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\tnginx\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
prev=0; conf=""; has_t=0; has_reload=0; want_reload=0
for a in "$@"; do
  if [ "$prev" = 1 ]; then conf="$a"; prev=0; continue; fi
  case "$a" in
    -c) prev=1 ;;
    -t) has_t=1 ;;
    -s) want_reload=1 ;;
    reload) [ "$want_reload" = 1 ] && has_reload=1 ;;
  esac
done
[ "${FAIL_AT:-}" = "nginx-t" ] && [ "$has_t" = 1 ] && [ "$has_reload" = 0 ] && exit 15
[ "${FAIL_AT:-}" = "reload" ] && [ "$has_reload" = 1 ] && exit 19
if [ "$has_t" = 1 ] && [ -n "$conf" ]; then
  inc="$(sed -n 's/^[[:space:]]*include \(.*\);$/\1/p' "$conf" | head -1)"
  [ -n "$inc" ] && [ -s "$inc" ] || exit 15
fi
exit 0
FB

  # ---- curl：全 fake 零网络；smoke 语义（未知 /w/ → 404）；可注入失败 ----
  cat >"$bd/curl" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\tcurl\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
url=""; has_w=0; prev=0
for a in "$@"; do
  if [ "$prev" = 1 ]; then prev=0; continue; fi
  case "$a" in
    -w) has_w=1; prev=1 ;;
    -o|--max-time|-H|-m|-A) prev=1 ;;
    http*) url="$a" ;;
  esac
done
[ "${FAIL_AT:-}" = "smoke" ] && exit 1
code=200
case "$url" in */w/*) code=404 ;; esac
[ "$has_w" = 1 ] && printf '%s\n' "$code"
exit 0
FB

  # ---- certbot：恒 fake；部署流程永不调用（harness 断言计数为 0） ----
  cat >"$bd/certbot" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\tcertbot\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
exit 0
FB

  # ---- bun：fake 本地构建；产物符合 cs_verify_artifact_dir 布局 ----
  cat >"$bd/bun" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
printf '%s\tbun\t%s\n' "$FB_N" "$*" >>"$FB_LOG"
[ "${FAIL_AT:-}" = "build" ] && exit 1
[ "$1" = "run" ] && [ "$2" = "build:widget" ] || exit 91
art="${CS_FAKE_ART_DIR:?CS_FAKE_ART_DIR required}"
rm -rf "$art"
mkdir -p "$art/assets" "$art/widget" "$art/widget-assets"
printf '// widget loader fixture run 20260920t150315\n' >"$art/loader.js"
printf '{"fixture":"widget-dist","run":"20260920t150315","source_head":"%s"}\n' \
  "$(git rev-parse HEAD)" >"$art/manifest.json"
h="$(shasum -a 256 "$art/manifest.json" | cut -d' ' -f1)"
printf '%s\n' "$h" >"$art/manifest.sha256"
printf 'ok\n' >"$art/health.txt"
printf '<!doctype html><div id="cs-widget-root"></div>\n' >"$art/widget/index.html"
printf 'console.log("asset");\n' >"$art/assets/app.20260920t150315.js"
printf 'console.log("frame v2");\n' >"$art/widget-assets/cs-widget.v2.js"
exit 0
FB

  # ---- backend：deploy_api 注入点（A5 预留 CS_DEPLOY_API_FN）----
  cat >"$bd/imboy-cs-fake-backend-deploy" <<'FB'
#!/usr/bin/env bash
FB_LOG="${FAKE_LOG:?}"
FB_N=$(($(wc -l <"$FB_LOG" 2>/dev/null) + 1))
bd_msg="deploy_api"
[ $# -gt 0 ] && bd_msg="deploy_api $*"
printf '%s\tbackend\t%s\n' "$FB_N" "$bd_msg" >>"$FB_LOG"
[ "${FAIL_AT:-}" = "backend" ] && exit 1
exit 0
FB

  # ---- 兼容 shim：macOS 缺失的远端语义（sha256sum / mv -T / realpath -e）----
  cat >"$bd/sha256sum" <<'FB'
#!/usr/bin/env bash
exec /usr/bin/shasum -a 256 "$@"
FB
  cat >"$bd/mv" <<'FB'
#!/usr/bin/env bash
# GNU mv -T 语义 shim：BSD mv 默认会跟随指向目录的 dst symlink（把源移进目录），
# 必须用 -h「dst 为指向目录的 symlink 时不跟随，替换链接本身」，与 GNU -T 在
# 本 harness 的全部用例（dst 恒为 symlink/regular file）上语义一致。
if [ "$1" = "-T" ]; then shift; exec /bin/mv -fh "$@"; fi
exec /bin/mv "$@"
FB
  cat >"$bd/realpath" <<'FB'
#!/usr/bin/env bash
# realpath -e shim（macOS 无 /usr/bin/realpath）：存在性要求 + cd -P 解析
if [ "$1" = "-e" ]; then shift; fi
rc=0
for p in "$@"; do
  if [ ! -e "$p" ]; then rc=1; continue; fi
  rd="$( cd "$p" 2>/dev/null && pwd -P )" || { rc=1; continue; }
  printf '%s\n' "$rd"
done
exit $rc
FB

  chmod +x "$bd"/* || return 1
}

# =============================================================================
# 场景生命周期
# =============================================================================
SD=""; FROOT=""; ENVF=""; CUR_FAIL_AT="none"; RUN_RC=0

new_scenario() { # $1 = 场景名（fresh-install 基线）
  SCN="$1"
  SD="$TMP_ROOT/$SCN"
  rm -rf -- "$ART_DIR" 2>/dev/null || true   # 场景间产物隔离（build 失败用例依赖空产物态）
  mkdir -p "$SD/bin"
  FROOT="$SD/fakeroot"
  mkdir -p "$FROOT"
  cp -R "$FIX_BASE/." "$FROOT/" || return 1
  mkdir -p "$FROOT/etc/letsencrypt/live/cs.test.local"
  cp "$CERTS/fullchain.pem" "$CERTS/privkey.pem" \
     "$FROOT/etc/letsencrypt/live/cs.test.local/" || return 1
  {
    printf 's|/www/wwwroot/|%s/www/wwwroot/|g\n' "$FROOT"
    printf 's|/etc/nginx/|%s/etc/nginx/|g\n' "$FROOT"
    printf 's|/etc/letsencrypt/|%s/etc/letsencrypt/|g\n' "$FROOT"
  } >"$SD/fake-map.sed"
  gen_fake_bins "$SD/bin" || return 1
  : >"$SD/ops.events"
  ENVF="$SD/customer.env"
  cat >"$ENVF" <<EOF
SERVER_HOST=deploy-fake-${RUN_ID}.test.invalid
SERVER_PORT=2222
SERVER_USER=fakeruser
DEPLOY_VSN=9.9.9-test
DEPLOY_BRANCH=cs-fake-${RUN_ID}
DEPLOY_PROJECT_DIR=/srv/imboy-fake-${RUN_ID}
DEPLOY_BLUE_PORT=9800
DEPLOY_GREEN_PORT=9801
DEPLOY_COOKIE=${CANARY}
DEPLOY_SALES_RELEASE=false
NGINX_CONF=/${API_CONF_REL}
PRODADM_CONF=/etc/nginx/confs/imboy.prodadm.conf
ADMIN_BUILD_DIR=/tmp/fake-admin-${RUN_ID}
ADMIN_REMOTE_DIR=/www/wwwroot/admin-fake-${RUN_ID}
DB_CONTAINER=pg-fake-${RUN_ID}
DB_NAME=imboy_fake_${RUN_ID}
DB_USER=imboy_fake_user
CS_WIDGET_DOMAIN=cs.test.local
CS_BUILD_DIR=test/fixtures/cs_deploy/build-src/widget-dist
CS_REMOTE_ROOT=/www/wwwroot/cs.test.local
CS_NGINX_CONF=/${CS_CONF_REL}
CS_CERT_FULLCHAIN=/etc/letsencrypt/live/cs.test.local/fullchain.pem
CS_CERT_KEY=/etc/letsencrypt/live/cs.test.local/privkey.pem
CS_SMOKE_SHOP_ORIGIN=https://shop.test.local
EOF
}

append_kv() { # 追加覆盖项；值一律单引号包裹（source 时不执行 $() / 反引号）
  printf "%s='%s'\n" "$1" "$2" >>"$ENVF"
}

make_upgrade_state() { # 叠加升级态：旧 release + current symlink（原始形式绝对路径）+ 旧 vhost
  cp -R "$FIX_OVERLAY/." "$FROOT/" || return 1
  ln -sfn "$OLD_SYMLINK_TARGET" "$FROOT/$CS_ROOT_REL/current" || return 1
  cp "$FROOT/$CS_CONF_REL" "$SD/vhost.pristine" || return 1
}

run_deploy() { # 驱动真实入口；EXIT_CODE → RUN_RC
  RUN_RC=0
  ( cd "$RUN_REPO" && exec env \
      -u CS_SSH_EXEC_FN -u CS_SSH_CAP_FN -u CS_SCP_FN -u CS_RSYNC_FN \
      -u CS_BUN_FN -u CS_DEPLOY_API_FN -u FAIL_AT -u FAKE_LOG -u FAKE_ROOT \
      -u FAKE_SED -u CS_EVENT_LOG -u CS_FAKE_ART_DIR \
      PATH="$SD/bin:$ORIG_PATH" \
      FAKE_LOG="$SD/ops.events" FAKE_ROOT="$FROOT" FAKE_SED="$SD/fake-map.sed" \
      CS_EVENT_LOG="$SD/steps.events" \
      CS_DEPLOY_API_FN=imboy-cs-fake-backend-deploy \
      CS_FAKE_ART_DIR="$ART_DIR" \
      FAIL_AT="$CUR_FAIL_AT" \
      bash "$ENTRY" cs -v -l --env-file "$ENVF" \
  ) >"$SD/stdout.log" 2>"$SD/stderr.log" || RUN_RC=$?
}

# ---------- ops.events 计数/顺序读取 ----------
opcount()     { awk -F'\t' -v k="$1" '$2==k{n++} END{print n+0}' "$SD/ops.events"; }
opcount_tag() { awk -F'\t' -v k="$1" -v t="$2" '$2==k && $3==t{n++} END{print n+0}' "$SD/ops.events"; }
# check_prod_config 守卫（1b2ca499）会先 scp 临时 escript 到远端 /tmp；
# vhost 上传断言只统计 release/vhost 面（排除 guard 自身的传输）。
opcount_scp_vhost() { awk -F'\t' '$2=="scp" && $3 !~ /imboy-config-key-guard\.escript/{n++} END{print n+0}' "$SD/ops.events"; }
opseq()       { awk -F'\t' -v k="$1" -v t="$2" '$2==k && $3==t{print $1; exit}' "$SD/ops.events"; }
opseq_nth()   { awk -F'\t' -v k="$1" -v t="$2" -v n="$3" '$2==k && $3==t{c++; if(c==n){print $1; exit}}' "$SD/ops.events"; }
ck_order() { # $1..= 有序 "kind:tag[@n]" 对；tag 为空 = 仅按 kind 匹配；@n = 第 n 次出现；SEQ 必须严格递增
  local d="调用序严格递增: $*" prev=0 cur kv k t n
  for kv in "$@"; do
    k="${kv%%:*}"; t="${kv#*:}"
    n=""
    case "$t" in
      *@*) n="${t##*@}"; t="${t%@*}" ;;
    esac
    if [ -n "$t" ] && [ -n "$n" ]; then
      cur="$(opseq_nth "$k" "$t" "$n")"
    elif [ -n "$t" ]; then
      cur="$(opseq_nth "$k" "$t" 1)"
    else
      cur="$(awk -F'\t' -v k="$k" '$2==k{print $1; exit}' "$SD/ops.events")"
    fi
    if [ -z "$cur" ] || [ "$cur" -le "$prev" ]; then
      nope "$d [$t seq=${cur:-缺失} prev=$prev]"
      return
    fi
    prev="$cur"
  done
  pass "$d"
}

new_release_dir() { # fake 树中唯一 ^14位数字 命名的 release 目录（本 run 创建）
  local d="$FROOT/$CS_ROOT_REL/releases" names
  names="$(ls "$d" 2>/dev/null | grep -E '^[0-9]{14}$' || true)"
  [ "$(printf '%s\n' "$names" | grep -c .)" = "1" ] || { printf ''; return; }
  printf '%s/%s' "$d" "$names"
}

cs_foreign_ok() {
  ck_present "foreign release 目录仍存在（I4/I5）" \
    "$FROOT/$CS_ROOT_REL/releases/$FOREIGN_RELEASE_NAME/FOREIGN_SENTINEL.txt"
  ck_present "批准根 .imboy-cs-root marker 仍存在" "$FROOT/$CS_ROOT_REL/.imboy-cs-root"
}

# =============================================================================
# A01 — happy path（升级 + 首次安装）
# =============================================================================
suite_a01() {
  # ---------- A01a 升级 happy path ----------
  new_scenario "a01-happy-upgrade" || { nope "场景初始化失败"; return; }
  make_upgrade_state || { nope "升级态构造失败"; return; }
  run_deploy
  ck_eq "部署进程退出码为 0" "$RUN_RC" "0"

  # 步骤事件 == 合同 S7 固定序列（cs_expected_steps 为唯一真源）
  sed 's/^STEP //' "$SD/steps.events" >"$SD/steps.norm" 2>/dev/null
  bash -c ". '$REPO/$LIB'; cs_expected_steps" >"$SD/steps.expected" 2>/dev/null
  if cmp -s "$SD/steps.norm" "$SD/steps.expected"; then
    pass "步骤事件序列逐字等于合同 S7（8 步，经 cs_expected_steps 比对）"
  else
    nope "步骤事件偏离 S7: got=[$(tr '\n' ' ' <"$SD/steps.norm")]"
  fi

  # 恰一次计数
  ck_eq "SSH ControlMaster 连接恰一次"  "$(opcount connect)" "1"
  ck_eq "PRECHECK 恰一次"    "$(opcount_tag cmd ': imboy-cs-check-precheck')"        "1"
  ck_eq "prepare-release 恰一次" "$(opcount_tag cmd ': imboy-cs-op-prepare-release')" "1"
  ck_eq "rsync 上传恰一次"   "$(opcount rsync)" "1"
  ck_eq "verify-release 恰一次" "$(opcount_tag cmd ': imboy-cs-op-verify-release')" "1"
  ck_eq "staged+final vhost 上传恰两次" "$(opcount_scp_vhost)" "2"
  ck_eq "TLS/nginx -t 校验恰两次" "$(opcount_tag cmd ': imboy-cs-check-tls-vhost')" "2"
  ck_eq "upstream 发现恰两次" "$(opcount_tag cmd ': imboy-cs-op-discover-upstream')" "2"
  ck_eq "backend=deploy_api 恰一次（CS_DEPLOY_API_FN 注入点）" "$(opcount backend)" "1"
  ck_eq "backup-vhost 恰一次" "$(opcount_tag cmd ': imboy-cs-op-backup-vhost')" "1"
  ck_eq "record-prev 恰一次"  "$(opcount_tag cmd ': imboy-cs-op-record-prev')" "1"
  ck_eq "swap-symlink 恰一次" "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "1"
  ck_eq "swap-vhost 恰一次"   "$(opcount_tag cmd ': imboy-cs-op-swap-vhost')" "1"
  ck_eq "nginx reload 恰一次" "$(opcount_tag cmd ': imboy-cs-op-nginx-reload')" "1"
  ck_eq "smoke 恰一次"        "$(opcount_tag cmd ': imboy-cs-check-smoke')" "1"
  ck_eq "finalize 恰一次"     "$(opcount_tag cmd ': imboy-cs-op-finalize')" "1"
  ck_eq "回滚动作零次(clean-staged)" "$(opcount_tag cmd ': imboy-cs-op-clean-staged')" "0"
  ck_eq "回滚动作零次(restore-symlink)" "$(opcount_tag cmd ': imboy-cs-op-restore-symlink')" "0"
  ck_eq "回滚动作零次(restore-vhost)"   "$(opcount_tag cmd ': imboy-cs-op-restore-vhost')" "0"
  ck_eq "本地构建（bun）恰一次" "$(opcount bun)" "1"
  ck_eq "smoke curl 探针恰五次" "$(opcount curl)" "5"
  ck_eq "certbot 零调用（永不触碰真实签发）" "$(opcount certbot)" "0"

  # 顺序精确：PRECHECK → build → stage → backend → activate → smoke（S7；SEQ 单调）
  ck_order \
    "cmd:: imboy-cs-check-precheck" \
    "bun:run build:widget" \
    "cmd:: imboy-cs-op-prepare-release" \
    "rsync:" \
    "cmd:: imboy-cs-op-verify-release" \
    "cmd:: imboy-cs-check-tls-vhost" \
    "backend:deploy_api" \
    "cmd:: imboy-cs-op-discover-upstream@2" \
    "cmd:: imboy-cs-op-backup-vhost" \
    "cmd:: imboy-cs-op-swap-symlink" \
    "cmd:: imboy-cs-op-swap-vhost" \
    "cmd:: imboy-cs-op-nginx-reload" \
    "cmd:: imboy-cs-check-smoke" \
    "cmd:: imboy-cs-op-finalize"

  # 文件系统终态：current → 新 release，内容与本地构建一致
  NEWREL="$(new_release_dir)"
  if [ -n "$NEWREL" ] && [ -d "$NEWREL" ]; then
    pass "releases/ 中恰新增一个 14 位时间戳 release 目录"
  else
    nope "新 release 目录缺失或出现多个: $NEWREL"
  fi
  ck_link "current symlink 指向新 release（原子激活终态）" \
    "$FROOT/$CS_ROOT_REL/current" "$NEWREL"
  ck_file_eq "新 release loader.js 与本地构建产物逐字节一致" \
    "$NEWREL/loader.js" "$ART_DIR/loader.js"
  ck_present "新 release manifest.json 存在" "$NEWREL/manifest.json"
  ck_present "新 release assets/ 非空" "$NEWREL/assets/app.20260920t150315.js"
  ck_present "新 release widget/index.html 存在" "$NEWREL/widget/index.html"

  # vhost 终态：final 渲染 + 无占位符 + 上游端口 + 无旧内容
  ck_contains "vhost server_name 为 cs.test.local" "$FROOT/$CS_CONF_REL" "server_name cs.test.local;"
  ck_contains "vhost upstream 为蓝 9800" "$FROOT/$CS_CONF_REL" "proxy_pass http://127.0.0.1:9800;"
  ck_contains "vhost 证书路径为 fullchain" "$FROOT/$CS_CONF_REL" \
    "ssl_certificate     /etc/letsencrypt/live/cs.test.local/fullchain.pem;"
  ck_not_contains "vhost 零 @占位符@ 残留" "$FROOT/$CS_CONF_REL" "@"
  ck_not_contains "vhost 不再含旧内容(return 503)" "$FROOT/$CS_CONF_REL" "return 503;"
  # CSD-CLI-01R（SEC-2，对齐 DEP-01R）：proxy 面 Host 头统一 $http_host
  # （保留端口，非默认端口网关部署同源判定不失配）——渲染产物禁
  # `proxy_set_header Host $host` 残留；80→443 的 301 跳转行豁免。
  # 计数=9：Widget SSE + Seat SSE + /w/ + /seat/ + widget API + 坐席 API
  # 收敛正则 + qr_login + enterprise conversations + organizations 正则。
  ck_not_contains "vhost proxy_set_header Host 禁 \$host 残留（301 跳转行豁免）" \
    "$FROOT/$CS_CONF_REL" 'proxy_set_header Host $host'
  ck_eq "vhost proxy 面九处 Host 头均为 \$http_host" \
    "$(grep -cF 'proxy_set_header Host $http_host' "$FROOT/$CS_CONF_REL" 2>/dev/null)" "9"

  # I5：时间戳备份 + 恢复记录；无半配置
  BAKF="$(ls "$FROOT/$CS_CONF_REL".cs-bak-* 2>/dev/null | head -1)"
  if [ -n "$BAKF" ] && [ -f "$BAKF" ]; then
    pass "旧 vhost 时间戳备份存在（I5）"
  else
    nope "旧 vhost 时间戳备份缺失"
  fi
  [ -n "$BAKF" ] && ck_file_eq "备份内容 == 部署前旧 vhost" "$BAKF" "$SD/vhost.pristine"
  ls "$FROOT/$CS_ROOT_REL"/.imboy-cs-prev-* >/dev/null 2>&1 \
    && pass "旧 symlink 指向恢复记录存在（I5）" \
    || nope "旧 symlink 恢复记录缺失"
  ck_no_residue "$FROOT"

  # 台账与 foreign/marker
  ck_eq "activations.log 恰一行台账" "$(wc -l <"$FROOT/$CS_ROOT_REL/activations.log" | tr -d ' ')" "1"
  ck_contains "台账记录版本 vsn=9.9.9-test" "$FROOT/$CS_ROOT_REL/activations.log" "vsn=9.9.9-test"
  ck_present "旧 release 仍存在（不删历史）" "$FROOT/$CS_ROOT_REL/releases/$OLD_RELEASE_NAME/loader.js"
  cs_foreign_ok

  # 出网面：fake curl 的 URL 全部限定在测试域名（只检查 curl 行，避免误匹配
  # rsync/scp 参数中的本地 worktree 路径）
  if awk -F'\t' '$2=="curl"' "$SD/ops.events" | grep -E 'imboy\.pub|106\.53' >/dev/null 2>&1; then
    nope "fake curl 出现测试域名之外的 URL"
  else
    pass "全部 curl URL 限定在 shop.test.local / cs.test.local（零真实出网面）"
  fi
  A05_SCAN_LOGS="$SD/stdout.log
$SD/stderr.log"
  A05_FROOT="$FROOT"

  # ---------- A01b 首次安装 happy path ----------
  new_scenario "a01-happy-fresh" || { nope "fresh 场景初始化失败"; return; }
  run_deploy
  ck_eq "首次安装退出码 0" "$RUN_RC" "0"
  NEWREL="$(new_release_dir)"
  ck_link "首次安装后 current → 新 release" "$FROOT/$CS_ROOT_REL/current" "$NEWREL"
  ck_contains "首次安装 vhost 已创建且含 server_name" "$FROOT/$CS_CONF_REL" "server_name cs.test.local;"
  if ls "$FROOT/$CS_CONF_REL".cs-bak-* >/dev/null 2>&1; then
    nope "首次安装不应产生 .cs-bak 备份（部署前无 vhost）"
  else
    pass "首次安装无 .cs-bak 备份（BAK 为空语义正确）"
  fi
  ck_eq "首次安装回滚动作零次(restore-symlink)" "$(opcount_tag cmd ': imboy-cs-op-restore-symlink')" "0"
  ck_no_residue "$FROOT"
  cs_foreign_ok
}

# =============================================================================
# A02 — 恶意输入在 SSH 前拒绝（计数器全零 + 非零退出）
# =============================================================================
suite_a02() {
  a02_case() { # $1=用例名 $2=变量 $3=恶意值（单引号安全：无单引号字符）
    local name="$1" var="$2" val="$3" pwn="${4:-}"
    new_scenario "a02-$name" || { nope "$name 场景初始化失败"; return; }
    append_kv "$var" "$val"
    [ -n "$pwn" ] && rm -f "$pwn"
    run_deploy
    ck_nenum "[$var 注入] 进程非零退出" "$RUN_RC" "0"
    ck_eq "[$var 注入] SSH 连接零次（SSH 前拒绝）" "$(opcount connect)" "0"
    ck_eq "[$var 注入] 远端命令零次" "$(opcount cmd)" "0"
    ck_eq "[$var 注入] scp 零次" "$(opcount scp)" "0"
    ck_eq "[$var 注入] rsync 零次" "$(opcount rsync)" "0"
    ck_eq "[$var 注入] 本地构建零次" "$(opcount bun)" "0"
    if [ -n "$pwn" ] && [ -e "$pwn" ]; then
      nope "[$var 注入] 命令替换似乎被执行（$pwn 出现）"
    elif [ -n "$pwn" ]; then
      pass "[$var 注入] 命令替换未执行（$pwn 不存在）"
    fi
  }

  a02_case domain-cmdsub   CS_WIDGET_DOMAIN      'cs.evil$(touch /tmp/cs-pwn-a02-1).local' /tmp/cs-pwn-a02-1
  a02_case domain-backtick CS_WIDGET_DOMAIN      'cs.evil`touch /tmp/cs-pwn-a02-2`.local'  /tmp/cs-pwn-a02-2
  a02_case domain-newline  CS_WIDGET_DOMAIN      'cs.test.local
rm -rf /tmp/cs-pwn-a02-3'
  a02_case root-traversal  CS_REMOTE_ROOT        '/www/wwwroot/../etc'
  a02_case root-semicolon  CS_REMOTE_ROOT        '/www/wwwroot/cs.test.local;touch /tmp/cs-pwn-a02-5' /tmp/cs-pwn-a02-5
  a02_case root-cmdsub     CS_REMOTE_ROOT        '/www/wwwroot/$(touch /tmp/cs-pwn-a02-6)' /tmp/cs-pwn-a02-6
  a02_case root-backtick   CS_REMOTE_ROOT        '/www/wwwroot/cs.`id`.local'
  a02_case root-newline    CS_REMOTE_ROOT        '/www/wwwroot/cs.test.local
victim-link-target'   # symlink 目标注入面：换行拆包
  a02_case conf-newline    CS_NGINX_CONF         '/etc/nginx/confs/imboy.cs.conf
include /tmp/evil'
  a02_case cert-same       CS_CERT_KEY           '/etc/letsencrypt/live/cs.test.local/fullchain.pem'
  a02_case origin-cmdsub   CS_SMOKE_SHOP_ORIGIN  'http://shop.test$(touch /tmp/cs-pwn-a02-10)' /tmp/cs-pwn-a02-10
  a02_case vsn-traversal   DEPLOY_VSN            '9.9.9/../../evil'
  a02_case root-empty      CS_REMOTE_ROOT        ''
}

# =============================================================================
# A03 — 六个失败点的恢复 oracle（升级态）
# =============================================================================
suite_a03() {
  a03_case() { # $1=FAIL_AT 值
    local f="$1"
    new_scenario "a03-$f" || { nope "$f 场景初始化失败"; return; }
    make_upgrade_state || { nope "$f 升级态构造失败"; return; }
    CUR_FAIL_AT="$f"
    run_deploy
    CUR_FAIL_AT="none"
    ck_nenum "[FAIL_AT=$f] 进程非零退出" "$RUN_RC" "0"
    ck_link "[FAIL_AT=$f] current symlink 指向不变/已恢复" \
      "$FROOT/$CS_ROOT_REL/current" "$OLD_SYMLINK_TARGET"
    ck_file_eq "[FAIL_AT=$f] 旧 vhost 内容不变/已恢复" \
      "$FROOT/$CS_CONF_REL" "$SD/vhost.pristine"
    ck_eq "[FAIL_AT=$f] 回滚 clean-staged 恰一次" \
      "$(opcount_tag cmd ': imboy-cs-op-clean-staged')" "1"
    ck_no_residue "$FROOT"
    ck_present "[FAIL_AT=$f] 旧 release 保留" \
      "$FROOT/$CS_ROOT_REL/releases/$OLD_RELEASE_NAME/loader.js"
    cs_foreign_ok

    case "$f" in
      build)
        ck_eq "[build] 本地构建被尝试一次且失败" "$(opcount bun)" "1"
        ck_eq "[build] 未创建远端 release（prepare=0）" "$(opcount_tag cmd ': imboy-cs-op-prepare-release')" "0"
        ck_eq "[build] 未上传（rsync=0）" "$(opcount rsync)" "0"
        ck_eq "[build] backend 未触达" "$(opcount backend)" "0"
        ck_eq "[build] 零原子激活(swap)" "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "0"
        ck_absent "[build] 构建失败未产出半成品" "$ART_DIR/manifest.json"
        ;;
      rsync)
        ck_eq "[rsync] prepare 已执行" "$(opcount_tag cmd ': imboy-cs-op-prepare-release')" "1"
        ck_eq "[rsync] 上传被尝试且失败" "$(opcount rsync)" "1"
        ck_eq "[rsync] verify 未触达" "$(opcount_tag cmd ': imboy-cs-op-verify-release')" "0"
        ck_eq "[rsync] backend 未触达" "$(opcount backend)" "0"
        ck_gt0  "[rsync] 新 release 目录保留待排查（I4）" \
          "$(ls "$FROOT/$CS_ROOT_REL/releases" 2>/dev/null | grep -cE '^[0-9]{14}$')"
        ;;
      backend)
        ck_eq "[backend] staging 全部完成（verify=1）" "$(opcount_tag cmd ': imboy-cs-op-verify-release')" "1"
        ck_eq "[backend] deploy_api 被调用一次且失败" "$(opcount backend)" "1"
        ck_eq "[backend] 零原子激活（I1/I2：backend 未成不得激活）" \
          "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "0"
        ck_eq "[backend] 零 swap-vhost" "$(opcount_tag cmd ': imboy-cs-op-swap-vhost')" "0"
        ck_gt0 "[backend] 新 release 目录保留（I4）" \
          "$(ls "$FROOT/$CS_ROOT_REL/releases" 2>/dev/null | grep -cE '^[0-9]{14}$')"
        ;;
      nginx-t)
        ck_eq "[nginx-t] staged vhost 校验被尝试一次（nginx -t 失败）" "$(opcount nginx)" "1"
        ck_eq "[nginx-t] backend 未触达（S7 顺序：VALIDATE 在 backend 之前）" "$(opcount backend)" "0"
        ck_eq "[nginx-t] 零原子激活" "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "0"
        ck_absent "[nginx-t] activations.log 未写（未成功）" "$FROOT/$CS_ROOT_REL/activations.log"
        ck_gt0 "[nginx-t] 新 release 目录保留（I4）" \
          "$(ls "$FROOT/$CS_ROOT_REL/releases" 2>/dev/null | grep -cE '^[0-9]{14}$')"
        ;;
      reload)
        ck_eq "[reload] 原子激活曾发生（swap-symlink=1）" "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "1"
        ck_eq "[reload] 原子激活曾发生（swap-vhost=1）" "$(opcount_tag cmd ': imboy-cs-op-swap-vhost')" "1"
        ck_eq "[reload] 回滚恢复 symlink 恰一次" "$(opcount_tag cmd ': imboy-cs-op-restore-symlink')" "1"
        ck_eq "[reload] 回滚恢复 vhost 恰一次" "$(opcount_tag cmd ': imboy-cs-op-restore-vhost')" "1"
        ck_eq "[reload] finalize 未执行（部署未成功）" "$(opcount_tag cmd ': imboy-cs-op-finalize')" "0"
        ck_gt0 "[reload] 新 release 目录保留（I4）" \
          "$(ls "$FROOT/$CS_ROOT_REL/releases" 2>/dev/null | grep -cE '^[0-9]{14}$')"
        ;;
      smoke)
        ck_eq "[smoke] smoke 被尝试一次且失败" "$(opcount_tag cmd ': imboy-cs-check-smoke')" "1"
        ck_eq "[smoke] 回滚恢复 symlink 恰一次" "$(opcount_tag cmd ': imboy-cs-op-restore-symlink')" "1"
        ck_eq "[smoke] 回滚恢复 vhost 恰一次" "$(opcount_tag cmd ': imboy-cs-op-restore-vhost')" "1"
        ck_eq "[smoke] finalize 未执行" "$(opcount_tag cmd ': imboy-cs-op-finalize')" "0"
        ck_gt0 "[smoke] 新 release 目录保留（I4）" \
          "$(ls "$FROOT/$CS_ROOT_REL/releases" 2>/dev/null | grep -cE '^[0-9]{14}$')"
        ;;
    esac
  }

  a03_case build
  a03_case rsync
  a03_case backend
  a03_case nginx-t
  a03_case reload
  a03_case smoke
}

# =============================================================================
# A04 — 首次安装失败零残余 + foreign 不删（升级侧回滚已由 A03 覆盖）
# =============================================================================
suite_a04() {
  a04_case() { # $1=FAIL_AT
    local f="$1"
    new_scenario "a04-fresh-$f" || { nope "a04-$f 场景初始化失败"; return; }
    CUR_FAIL_AT="$f"
    run_deploy
    CUR_FAIL_AT="none"
    ck_nenum "[fresh/$f] 进程非零退出" "$RUN_RC" "0"
    ck_absent "[fresh/$f] 无 current symlink（零残余）" "$FROOT/$CS_ROOT_REL/current"
    ck_absent "[fresh/$f] 无 active CS vhost（零残余）" "$FROOT/$CS_CONF_REL"
    ck_no_residue "$FROOT"
    ck_absent "[fresh/$f] activations.log 未写" "$FROOT/$CS_ROOT_REL/activations.log"
    cs_foreign_ok
  }
  a04_case backend
  a04_case smoke

  # fresh+smoke 额外断言：激活发生过且被完全恢复为"无"
  SCN="a04-fresh-smoke"
  SD="$TMP_ROOT/a04-fresh-smoke"; FROOT="$SD/fakeroot"
  ck_eq "[fresh/smoke] 激活曾发生(swap-symlink=1)" "$(opcount_tag cmd ': imboy-cs-op-swap-symlink')" "1"
  ck_eq "[fresh/smoke] 恢复为无（restore-symlink=1）" "$(opcount_tag cmd ': imboy-cs-op-restore-symlink')" "1"
  ck_eq "[fresh/smoke] 恢复为无（restore-vhost=1）" "$(opcount_tag cmd ': imboy-cs-op-restore-vhost')" "1"
}

# =============================================================================
# A05 — secret 卫生 + 既有回归
# =============================================================================
suite_a05() {
  SCN="a05-hygiene"
  # 1) 全场景 stdout/stderr secret scan（诱饵 canary 只允许以 CS*** 形式出现）
  local scan_pat='BEGIN.*PRIVATE KEY|sk-[A-Za-z0-9]{16}|token=[A-Za-z0-9]'
  local hits
  hits="$(grep -rE "$scan_pat" "$TMP_ROOT"/*/stdout.log "$TMP_ROOT"/*/stderr.log 2>/dev/null | head -3)"
  if [ -z "$hits" ]; then
    pass "全部场景日志零 secret（PRIVATE KEY / sk- / token= 均未出现）"
  else
    nope "日志发现 secret 模式: $hits"
  fi
  hits="$(grep -rF "$CANARY" "$TMP_ROOT"/*/stdout.log "$TMP_ROOT"/*/stderr.log 2>/dev/null | head -3)"
  if [ -z "$hits" ]; then
    pass "诱饵 canary 全文未出现在任何 verbose 日志"
  else
    nope "canary 泄漏: $hits"
  fi
  if grep -qF 'cookie=CS***' "$TMP_ROOT/a01-happy-upgrade/stdout.log" 2>/dev/null; then
    pass "cookie 以脱敏占位 CS*** 出现（I7 脱敏路径生效）"
  else
    nope "未观察到 cookie 脱敏占位（cookie=CS***）"
  fi

  # 2) fake remote 终态 scan（happy 场景）：除证书私钥文件本身外零私钥/canary
  local fr="${A05_FROOT:-$TMP_ROOT/a01-happy-upgrade/fakeroot}"
  hits="$(grep -rE 'BEGIN.*PRIVATE KEY' --exclude=privkey.pem "$fr" 2>/dev/null | head -3)"
  [ -z "$hits" ] && pass "fake remote 终态（除证书私钥文件）零私钥内容" \
                 || nope "fake remote 泄漏私钥: $hits"
  hits="$(grep -rF "$CANARY" "$fr" 2>/dev/null | head -3)"
  [ -z "$hits" ] && pass "fake remote 终态零 canary" \
                 || nope "fake remote 出现 canary: $hits"
  local nkey
  nkey="$(find "$fr" -name 'privkey.pem' 2>/dev/null | wc -l | tr -d ' ')"
  ck_eq "私钥文件只存在于授权路径（恰一份 privkey.pem）" "$nkey" "1"

  # 3) 既有回归：migrate / restore 必须全绿
  if bash "$REPO/scripts/test/migrate_gate_test.sh" >/dev/null 2>&1; then
    pass "既有回归 migrate_gate_test.sh 全绿"
  else
    nope "既有回归 migrate_gate_test.sh 失败"
  fi
  if bash "$REPO/scripts/test/restore_guard_test.sh" >/dev/null 2>&1; then
    pass "既有回归 restore_guard_test.sh 全绿"
  else
    nope "既有回归 restore_guard_test.sh 失败"
  fi

  # 4) deploy_sequence_test 仅记录（已知既有漂移；失败名单对比在证据侧完成）
  DS_RC=0
  bash "$REPO/scripts/test/deploy_sequence_test.sh" >"$TMP_ROOT/deploy_sequence.post.log" 2>&1 || DS_RC=$?
  DS_TOTAL="$(grep -E '^总计' "$TMP_ROOT/deploy_sequence.post.log" | tail -1)"
  printf 'INFO [a05] deploy_sequence_test 记录: exit=%s %s（既有漂移零新增由证据侧两份失败名单 diff 证明）\n' \
    "$DS_RC" "$DS_TOTAL"

  # 5) worktree 卫生自检：清理运行期产物后 worktree 必须无新增脏文件
  rm -rf -- "$ART_DIR" 2>/dev/null || true
  local dirt
  dirt="$(cd "$REPO" && git status --porcelain -- scripts/test/fixtures/cs_deploy/build-src/widget-dist 2>/dev/null | head -3)"
  if [ -z "$dirt" ] && [ ! -e "$ART_DIR" ]; then
    pass "harness 无 worktree 残留（widget-dist 已清理）"
  else
    nope "harness 残留: $dirt"
  fi
}

# =============================================================================
# 主流程
# =============================================================================
printf '=== CSD-TEST-01 customer_service_deploy_test (run %s) ===\n' "$RUN_ID"
[ -f "$ENTRY" ] || { echo "ABORT: 找不到 $ENTRY" >&2; exit 1; }

suite_a01
suite_a02
suite_a03
suite_a04
suite_a05

printf '=== TOTAL PASS=%d FAIL=%d ===\n' "$PASS" "$FAIL"
if [ "$FAIL" -eq 0 ]; then
  printf 'RESULT: PASS\n'
  exit 0
fi
printf 'RESULT: FAIL\n'
exit 1
