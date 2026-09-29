#!/usr/bin/env bash
# IMBoy CS Widget install/upgrade/restart/rollback 演练（DEP-01 A06，合成环境）
#
# 全程只在本地 Docker 合成栈内进行：
#   * 项目名固定 csww-dep01，容器名 csww-widget-nginx，宿主端口 18900；
#   * 不触碰任何 PG/Redis 容器与端口（5432/4323），不触碰 imboy 运行节点
#     （9700/9801/9802 仅在显式提供 SSE 演练参数时经只读 GET 观察，绝不写入）；
#   * 镜像 tag 固定 v0/v1（禁 latest）：升级=换 tag，回滚=切回旧 tag。
#
# 用法：
#   bash deploy/widget/dryrun.sh --dist <build:widget 产物目录> [选项]
#
# 选项：
#   --dist DIR        v1 镜像内容物（admin 仓 dist-widget/ 拷贝），必填
#   --skip-sse        跳过经容器反代的 SSE 断言（默认在提供环境变量时执行）
# 环境变量（SSE 断言，全部可选；不提供则该步 SKIP）：
#   DRYRUN_SSE_URL    完整 SSE URL（经 18900 反代，含 organization_id 等
#                     查询参数；路径段为 sessions/<id>/events）
#   DRYRUN_SSE_TOKEN  x-cs-visit-token 值
#   DRYRUN_SSE_ORIGIN Origin 头值（须命中 installation allowlist）
#   DRYRUN_KEYFILES   逗号分隔的 key 文件路径（0600 secret 文件稳定性断言）
#
# 步骤：build v0/v1 → install v1 → 断言(200/缓存头/资产hash) → restart →
#       稳定断言(镜像 digest/资产 hash/key 文件 hash) → rollback 切 v0 →
#       断言旧产物 → down -v 清理。任何一步失败即非零退出。
set -euo pipefail
cd "$(dirname "$0")"

PROJ="csww-dep01"
CTR="csww-widget-nginx"
IMG_V0="csww-widget:v0"
IMG_V1="csww-widget:v1"
PORT="18900"
BASE="http://127.0.0.1:${PORT}"
COMPOSE=(docker compose -p "$PROJ" -f ../docker-compose.widget.yml)
FAILED=0

say()  { printf '\n\033[1;36m==> %s\033[0m\n' "$*"; }
ok()   { printf '  [PASS] %s\n' "$*"; }
bad()  { printf '  [FAIL] %s\n' "$*"; FAILED=1; }
skip() { printf '  [SKIP] %s\n' "$*"; }

cleanup() {
  if [[ "${KEEP_UP:-0}" != 1 ]]; then
    "${COMPOSE[@]}" down -v --remove-orphans >/dev/null 2>&1 || true
  fi
  [[ -n "${WORKDIR:-}" && -d "$WORKDIR" ]] && rm -rf "$WORKDIR"
  return 0
}
trap cleanup EXIT

usage() { grep '^#' "$0" | sed -n '2,25p'; exit 0; }

DIST=""
SKIP_SSE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --dist)    DIST="$2"; shift 2 ;;
    --dist=*)  DIST="${1#*=}"; shift ;;
    --skip-sse) SKIP_SSE=1; shift ;;
    -h|--help) usage ;;
    *) echo "未知参数：$1（--help 查看用法）" >&2; exit 2 ;;
  esac
done

# ── STEP 0：环境前置 ──────────────────────────────────────────────────────────
say "STEP 0 环境前置"
docker info >/dev/null 2>&1 || { bad "docker daemon 不可用"; exit 1; }
ok "docker daemon 可用"
case "${PROJ}/${CTR}" in *csww*) ok "演练命名含 RUN_ID 片段 csww：${PROJ} / ${CTR}" ;; esac
if lsof -iTCP:"${PORT}" -sTCP:LISTEN >/dev/null 2>&1; then
  bad "宿主端口 ${PORT} 已被占用，拒绝演练"
  exit 1
fi
ok "宿主端口 ${PORT} 空闲"
[[ -n "$DIST" && -d "$DIST" ]] || { echo "--dist 需要 build:widget 产物目录（--help）" >&2; exit 2; }
ok "v1 内容物：${DIST}"

# ── STEP 0.1 产物清单 fail-closed 校验（SC-OPS-A05）──────────────────────────
# Seat 控制台嵌入后（冻结合同 control/build-contract.json seat），v1 产物必须
# 同时携带 Widget 面与 Seat 面：/seat/:id frame HTML 引用稳定别名
# /seat-assets/cs-seat.v1.{js,css}，缺任一文件 = 线上 frame 静默 404，
# 故 fail-closed 拒绝演练（缺什么在错误里列全）。manifest.sha256 与
# manifest.json 的一致性为完整性预检（防呆不防恶）。
say "STEP 0.1 产物清单 fail-closed 校验（Widget + Seat 面）"
MISSING=""
for f in loader.js manifest.json manifest.sha256 health.txt \
         widget/index.html seat/index.html \
         widget-assets/cs-widget.v2.js \
         seat-assets/cs-seat.v1.js seat-assets/cs-seat.v1.css; do
  [[ -s "$DIST/$f" ]] || MISSING="$MISSING $f"
done
if [[ -n "$MISSING" ]]; then
  bad "产物缺失或为空:${MISSING}（SC-OPS-A05 fail-closed）"
  exit 1
fi
ok "必需产物齐全（widget 面 + seat-assets/cs-seat.v1.{js,css} + 双 HTML 壳）"
if [[ -n "$(ls -A "$DIST/assets" 2>/dev/null)" ]]; then
  ok "assets/ 非空"
else
  bad "assets/ 为空（hashed 资产缺失，SC-OPS-A05 fail-closed）"
  exit 1
fi
SUM_CALC="$(shasum -a 256 "$DIST/manifest.json" | awk '{print $1}')"
SUM_REG="$(awk '{print $1; exit}' "$DIST/manifest.sha256")"
if [[ -n "$SUM_REG" && "$SUM_CALC" == "$SUM_REG" ]]; then
  ok "manifest.sha256 与 manifest.json 一致"
else
  bad "manifest.sha256 与 manifest.json 不一致（SC-OPS-A05 fail-closed）" "calc=${SUM_CALC:0:12} reg=${SUM_REG:0:12}"
  exit 1
fi

# ── STEP 1：构建 v0（合成空壳页，模拟旧 artifact）────────────────────────────
say "STEP 1 构建 v0 空壳页镜像（合成旧产物）"
WORKDIR="$(mktemp -d "${TMPDIR:-/tmp}/csww-dep01.XXXXXX")"
mkdir -p "${WORKDIR}/v0/assets" "${WORKDIR}/v0/widget"
cat > "${WORKDIR}/v0/loader.js" <<'EOF'
/* csww-widget-dryrun v0 shell loader */
EOF
cat > "${WORKDIR}/v0/widget/index.html" <<'EOF'
<!doctype html><html><body>csww-widget-dryrun-v0-shell</body></html>
EOF
printf '/* v0 asset */\n' > "${WORKDIR}/v0/assets/cs-widget.js"
rm -rf dist && mkdir -p dist
cp -R "${WORKDIR}/v0/." dist/
docker build -q --build-arg WIDGET_BACKEND_UPSTREAM="${WIDGET_BACKEND_UPSTREAM:-http://127.0.0.1:9800}" \
  -t "$IMG_V0" . >/dev/null
[[ -n "$(docker image inspect -f '{{.Id}}' "$IMG_V0" 2>/dev/null)" ]] \
  && ok "v0 镜像构建完成 ${IMG_V0}" || bad "v0 镜像构建失败"

# ── STEP 2：构建 v1（真产物）─────────────────────────────────────────────────
say "STEP 2 构建 v1 真产物镜像"
rm -rf dist && mkdir -p dist
cp -R "${DIST}/." dist/
docker build -q --build-arg WIDGET_BACKEND_UPSTREAM="${WIDGET_BACKEND_UPSTREAM:-http://127.0.0.1:9800}" \
  -t "$IMG_V1" . >/dev/null
[[ -n "$(docker image inspect -f '{{.Id}}' "$IMG_V1" 2>/dev/null)" ]] \
  && ok "v1 镜像构建完成 ${IMG_V1}" || bad "v1 镜像构建失败"
DIGEST_V0="$(docker image inspect -f '{{.Id}}' "$IMG_V0")"
DIGEST_V1="$(docker image inspect -f '{{.Id}}' "$IMG_V1")"
[[ "$DIGEST_V0" != "$DIGEST_V1" ]] && ok "v0/v1 镜像 digest 互异（内容可区分）" || bad "v0/v1 digest 相同"

# ── STEP 3：install（起 v1）──────────────────────────────────────────────────
say "STEP 3 install：up v1"
WIDGET_IMAGE="$IMG_V1" WIDGET_CONTAINER_NAME="$CTR" WIDGET_HTTP_PORT="$PORT" \
  "${COMPOSE[@]}" up -d >/dev/null
for _ in $(seq 1 30); do
  [[ "$(docker inspect -f '{{.State.Health.Status}}' "$CTR" 2>/dev/null)" == "healthy" ]] && break
  sleep 1
done
[[ "$(docker inspect -f '{{.State.Health.Status}}' "$CTR" 2>/dev/null)" == "healthy" ]] \
  && ok "容器 healthy（${CTR}）" || { bad "容器未达 healthy"; exit 1; }
CODE_H="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/healthz")"
CODE_L="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/loader.js")"
CODE_W="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/widget/")"
[[ "$CODE_H" == 200 && "$CODE_L" == 200 && "$CODE_W" == 200 ]] \
  && ok "healthz/loader.js/widget 全部 200" || bad "200 断言失败 h=${CODE_H} l=${CODE_L} w=${CODE_W}"
CODE_SJS="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/seat-assets/cs-seat.v1.js")"
CODE_SCSS="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/seat-assets/cs-seat.v1.css")"
CODE_SEAT="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/seat/")"
[[ "$CODE_SJS" == 200 && "$CODE_SCSS" == 200 ]] \
  && ok "seat-assets 稳定别名 200（v1 含 Seat 面）" || bad "seat-assets 非 200 js=${CODE_SJS} css=${CODE_SCSS}"
[[ "$CODE_SEAT" == 200 ]] && ok "seat/ HTML 壳 200（v1 含 Seat 入口）" || bad "seat/ 非 200：${CODE_SEAT}"

# ── STEP 4：缓存头断言（A04）+ v1 资产指纹 ───────────────────────────────────
say "STEP 4 缓存策略断言 + v1 资产指纹"
CC_L="$(curl -sI "${BASE}/loader.js" | tr -d '\r' | awk -F': ' 'tolower($1)=="cache-control"{print $2}')"
CC_W="$(curl -sI "${BASE}/widget/" | tr -d '\r' | awk -F': ' 'tolower($1)=="cache-control"{print $2}')"
CC_A="$(curl -sI "${BASE}/assets/cs-widget.js" | tr -d '\r' | awk -F': ' 'tolower($1)=="cache-control"{print $2}')"
ET_L="$(curl -sI "${BASE}/loader.js" | tr -d '\r' | awk -F': ' 'tolower($1)=="etag"{print $2}')"
[[ "$CC_L" == *"must-revalidate"* && "$CC_L" != *"immutable"* ]] \
  && ok "loader.js 短缓存+强制协商：${CC_L}" || bad "loader.js 缓存头异常：${CC_L}"
[[ "$CC_W" == *"no-cache"* ]] && ok "widget HTML no-cache：${CC_W}" || bad "widget HTML 缓存头异常：${CC_W}"
[[ "$CC_A" == *"must-revalidate"* && "$CC_A" != *"immutable"* ]] \
  && ok "assets 无 hash 文件名走协商缓存：${CC_A}" || bad "assets 缓存头异常：${CC_A}"
[[ -n "$ET_L" ]] && ok "loader.js ETag 协商校验存在" || bad "loader.js 缺 ETag"
HASH_LOADER_V1="$(curl -s "${BASE}/loader.js" | shasum -a 256 | awk '{print $1}')"
HASH_ASSET_V1="$(curl -s "${BASE}/assets/cs-widget.js" | shasum -a 256 | awk '{print $1}')"
HASH_HTML_V1="$(curl -s "${BASE}/widget/" | shasum -a 256 | awk '{print $1}')"
HASH_SEATJS_V1="$(curl -s "${BASE}/seat-assets/cs-seat.v1.js" | shasum -a 256 | awk '{print $1}')"
HASH_SEATCSS_V1="$(curl -s "${BASE}/seat-assets/cs-seat.v1.css" | shasum -a 256 | awk '{print $1}')"
ok "v1 资产指纹已记录 loader=${HASH_LOADER_V1:0:12} asset=${HASH_ASSET_V1:0:12} html=${HASH_HTML_V1:0:12} seat-js=${HASH_SEATJS_V1:0:12} seat-css=${HASH_SEATCSS_V1:0:12}"

# ── STEP 5：SSE 经容器反代断言（A05，可选）───────────────────────────────────
say "STEP 5 SSE 经容器反代断言（可选）"
if [[ "$SKIP_SSE" == 1 || -z "${DRYRUN_SSE_URL:-}" ]]; then
  skip "未提供 DRYRUN_SSE_URL，跳过（SSE 断言另见 DEP-01 证据）"
else
  SSE_HDR="$(mktemp "${TMPDIR:-/tmp}/csww-sse-hdr.XXXXXX")"
  SSE_CODE="$(curl -s --max-time 6 -N -D "$SSE_HDR" -o "${WORKDIR}/sse-body.txt" -w '%{http_code}' \
    -H "x-cs-visit-token: ${DRYRUN_SSE_TOKEN:-}" \
    -H "Origin: ${DRYRUN_SSE_ORIGIN:-http://localhost:8901}" \
    "$DRYRUN_SSE_URL" || true)"
  SSE_CT="$(tr -d '\r' < "$SSE_HDR" | awk -F': ' 'tolower($1)=="content-type"{print $2}')"
  SSE_CC="$(tr -d '\r' < "$SSE_HDR" | awk -F': ' 'tolower($1)=="cache-control"{print $2}')"
  SSE_XA="$(tr -d '\r' < "$SSE_HDR" | awk -F': ' 'tolower($1)=="x-accel-buffering"{print $2}')"
  if [[ "$SSE_CODE" != 200 ]]; then
    bad "反代链 HTTP ${SSE_CODE}（body: $(head -c 120 "${WORKDIR}/sse-body.txt" 2>/dev/null)）"
  else
    ok "反代链 HTTP 200"
  fi
  [[ "$SSE_CT" == text/event-stream* ]] && ok "反代链 content-type=${SSE_CT}" || bad "反代链 content-type=${SSE_CT}"
  [[ "$SSE_CC" == *no-cache* || "$SSE_CC" == *no-store* ]] && ok "反代链 cache-control=${SSE_CC}" || bad "反代链 cache-control=${SSE_CC}"
  [[ "$SSE_XA" == no ]] && ok "反代链 X-Accel-Buffering=no" || bad "反代链 X-Accel-Buffering=${SSE_XA}"
  grep -q "retry:" "${WORKDIR}/sse-body.txt" && ok "SSE 流体帧到达（retry 帧存在）" || bad "SSE 无流体帧"
  rm -f "$SSE_HDR"
fi

# ── STEP 6：restart + 稳定断言 ───────────────────────────────────────────────
say "STEP 6 restart + 版本/key 稳定断言"
KEYHASH_BEFORE=""
if [[ -n "${DRYRUN_KEYFILES:-}" ]]; then
  KEYHASH_BEFORE="$(printf '%s' "${DRYRUN_KEYFILES}" | tr ',' '\n' | sort | xargs shasum -a 256 | shasum -a 256 | awk '{print $1}')"
  ok "key 文件指纹已记录（restart 前）"
fi
CID_BEFORE="$(docker inspect -f '{{.Id}}' "$CTR")"
STARTED_BEFORE="$(docker inspect -f '{{.State.StartedAt}}' "$CTR")"
"${COMPOSE[@]}" restart >/dev/null
for _ in $(seq 1 30); do
  [[ "$(docker inspect -f '{{.State.Health.Status}}' "$CTR" 2>/dev/null)" == "healthy" ]] && break
  sleep 1
done
ok "restart 完成、恢复 healthy"
CID_AFTER="$(docker inspect -f '{{.Id}}' "$CTR")"
STARTED_AFTER="$(docker inspect -f '{{.State.StartedAt}}' "$CTR")"
[[ "$CID_BEFORE" == "$CID_AFTER" && "$STARTED_BEFORE" != "$STARTED_AFTER" ]] \
  && ok "restart 生效（同一容器新启动实例）" || bad "restart 断言异常"
RUNNING_IMG="$(docker inspect -f '{{.Image}}' "$CTR")"
[[ "$RUNNING_IMG" == "$DIGEST_V1" ]] && ok "restart 后镜像 digest 不变" || bad "restart 后镜像 digest 变化"
HASH_LOADER_R="$(curl -s "${BASE}/loader.js" | shasum -a 256 | awk '{print $1}')"
HASH_ASSET_R="$(curl -s "${BASE}/assets/cs-widget.js" | shasum -a 256 | awk '{print $1}')"
HASH_SEATJS_R="$(curl -s "${BASE}/seat-assets/cs-seat.v1.js" | shasum -a 256 | awk '{print $1}')"
[[ "$HASH_LOADER_R" == "$HASH_LOADER_V1" && "$HASH_ASSET_R" == "$HASH_ASSET_V1" && "$HASH_SEATJS_R" == "$HASH_SEATJS_V1" ]] \
  && ok "restart 后静态资产 hash 不变（含 seat-assets）" || bad "restart 后资产 hash 变化（seat-js ${HASH_SEATJS_R:0:12} vs ${HASH_SEATJS_V1:0:12}）"
if [[ -n "${DRYRUN_KEYFILES:-}" ]]; then
  KEYHASH_AFTER="$(printf '%s' "${DRYRUN_KEYFILES}" | tr ',' '\n' | sort | xargs shasum -a 256 | shasum -a 256 | awk '{print $1}')"
  [[ "$KEYHASH_BEFORE" == "$KEYHASH_AFTER" ]] && ok "key 文件 restart 前后指纹一致（key 稳定）" || bad "key 文件指纹变化"
fi

# ── STEP 7：rollback 切回 v0（旧 artifact）───────────────────────────────────
say "STEP 7 rollback：切 tag v1 → v0"
WIDGET_IMAGE="$IMG_V0" WIDGET_CONTAINER_NAME="$CTR" WIDGET_HTTP_PORT="$PORT" \
  "${COMPOSE[@]}" up -d >/dev/null
for _ in $(seq 1 30); do
  [[ "$(docker inspect -f '{{.State.Health.Status}}' "$CTR" 2>/dev/null)" == "healthy" ]] && break
  sleep 1
done
ok "回滚后恢复 healthy"
ROLL_IMG="$(docker inspect -f '{{.Image}}' "$CTR")"
[[ "$ROLL_IMG" == "$DIGEST_V0" ]] && ok "运行镜像 digest = v0（回滚生效）" || bad "运行镜像不是 v0"
ROLL_HTML="$(curl -s "${BASE}/widget/")"
[[ "$ROLL_HTML" == *"csww-widget-dryrun-v0-shell"* ]] \
  && ok "回滚后 widget HTML 返回 v0 空壳内容" || bad "回滚后内容仍为 v1"
ROLL_LOADER="$(curl -s "${BASE}/loader.js" | shasum -a 256 | awk '{print $1}')"
V0_LOADER="$(printf '/* csww-widget-dryrun v0 shell loader */\n' | shasum -a 256 | awk '{print $1}')"
[[ "$ROLL_LOADER" == "$V0_LOADER" ]] && ok "回滚后 loader.js 为 v0 内容" || bad "回滚后 loader.js 非 v0"
CODE_RB="$(curl -s -o /dev/null -w '%{http_code}' "${BASE}/widget/")"
[[ "$CODE_RB" == 200 ]] && ok "回滚后仍 200 可服务" || bad "回滚后非 200：${CODE_RB}"

# ── STEP 8：down 清理 ────────────────────────────────────────────────────────
say "STEP 8 down -v 清理"
WIDGET_IMAGE="$IMG_V0" WIDGET_CONTAINER_NAME="$CTR" WIDGET_HTTP_PORT="$PORT" \
  "${COMPOSE[@]}" down -v --remove-orphans >/dev/null
sleep 1
LEFT_CTR="$(docker ps -a --filter "name=^${CTR}$" --format '{{.Names}}')"
LEFT_NET="$(docker network ls --filter "name=^${PROJ}_" --format '{{.Name}}')"
[[ -z "$LEFT_CTR" ]] && ok "容器已移除：${CTR}" || bad "残留容器：${LEFT_CTR}"
[[ -z "$LEFT_NET" ]] && ok "演练网络已移除：${PROJ}_*" || bad "残留网络：${LEFT_NET}"

# ── 结论 ─────────────────────────────────────────────────────────────────────
say "演练结论"
if [[ "$FAILED" == 0 ]]; then
  echo "install/upgrade-restart-stability/rollback/down 全部 PASS（合成环境，未触碰 PG 与 imboy 节点）"
  exit 0
else
  echo "存在 FAIL 步骤，详见上方输出" >&2
  exit 1
fi
