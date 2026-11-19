#!/usr/bin/env bash
# ============================================================
# PG18 镜像蓝绿升级 / Blue-green image upgrade for imboy_pg18
# ------------------------------------------------------------
# 面向 deploy/docker-compose.prod.yml 的 imboy_pg18 服务：
# 物理同步（预拷贝+冻结窗口 rsync delta）+ 立即 ALTER EXTENSION
# + 硬验证门禁 + 失败自动回滚。数据零丢失：切换前全量备份
# （backup_pg.sh + restore_smoke）+ 冷快照 + 旧数据目录原样保留。
#
# 用法 / Usage:
#   bash scripts/pg18_bluegreen_upgrade.sh status     # 状态盘点
#   bash scripts/pg18_bluegreen_upgrade.sh prepare    # 阶段一：零停机（备份+在线预拷贝）
#   bash scripts/pg18_bluegreen_upgrade.sh cutover    # 阶段二：停机窗口（冻结→同步→换→验→起）
#   bash scripts/pg18_bluegreen_upgrade.sh verify     # 切后深度验证（可随时重跑）
#   bash scripts/pg18_bluegreen_upgrade.sh rollback   # 手动回滚（切后发现问题）
#   bash scripts/pg18_bluegreen_upgrade.sh cleanup    # 清理回滚锚（需确认+最小保留天数）
#   bash scripts/pg18_bluegreen_upgrade.sh all        # prepare + cutover（含确认门）
#
# 常用开关 / Flags（也可用同名大写环境变量）:
#   --yes                     跳过人工确认门（默认需交互确认）
#   --skip-backup             跳过 backup_pg.sh（强烈不建议；rehearsal 用）
#   --skip-cold-snapshot      跳过冷快照（省磁盘；不建议）
#   --skip-online-precopy     跳过在线预拷贝（窗口变为全量拷贝）
#   --new-image <tag>         新镜像（默认 imboy/pg18:3.7.0-1）
#
# 环境覆盖（rehearsal/异构部署）:
#   COMPOSE_FILE / PG_SERVICE / PG_CONTAINER / BACKEND_SERVICE /
#   BACKEND_HEALTH_URL / DATA_DIR / PG_OLD_IMAGE / PG_NEW_IMAGE /
#   POSTGRES_USER / POSTGRES_DB / SMOKE_TABLES / YES
#
# 安全模型 / Safety model（数据三重锚）:
#   1. backup_pg.sh --full + restore_smoke.sh   可证明可恢复的异地备份
#   2. pg18_cold_<ts>.tar                       停机窗口内冷快照（秒级回滚点）
#   3. pg18_old_<ts>/                           旧数据目录原样保留（切后回滚锚）
#
# 停机预算（prod 量级实测后更新）: ~5-12 分钟
# ============================================================
set -Eeuo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# ---------- 可配置项 ----------
COMPOSE_FILE="${COMPOSE_FILE:-deploy/docker-compose.prod.yml}"
PG_SERVICE="${PG_SERVICE:-imboy_pg18}"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"
BACKEND_SERVICE="${BACKEND_SERVICE:-backend}"
BACKEND_HEALTH_URL="${BACKEND_HEALTH_URL-http://127.0.0.1:9800/healthz}"   # 空串=显式禁用（注意用 - 不用 :-）
DATA_DIR="${DATA_DIR:-}"
PG_OLD_IMAGE="${PG_OLD_IMAGE:-}"            # 空=自动从运行容器探测
PG_NEW_IMAGE="${PG_NEW_IMAGE:-imboy/pg18:3.7.0-1}"
POSTGRES_USER_DB="${POSTGRES_USER:-imboy_user}"
POSTGRES_DB_DB="${POSTGRES_DB:-imboy_pro}"
# 行数抽检表（prepare 期过滤：只测新旧两边都存在的表）
SMOKE_TABLES="${SMOKE_TABLES:-msg_c2c msg_c2s msg_c2g msg_read msg_store user_log}"
# 升级目标扩展清单（在新容器上动态过滤实际安装的）
ALTERABLE_EXTS="${ALTERABLE_EXTS:-timescaledb postgis postgis_topology pgrouting roaringbitmap}"
CLEANUP_MIN_AGE_DAYS="${CLEANUP_MIN_AGE_DAYS:-7}"
YES="${YES:-0}"
SKIP_BACKUP="${SKIP_BACKUP:-0}"
SKIP_COLD_SNAPSHOT="${SKIP_COLD_SNAPSHOT:-0}"
SKIP_ONLINE_PRECOPY="${SKIP_ONLINE_PRECOPY:-0}"

OLD_DIR=""      # canonical 数据目录（compose 挂载点），resolve_dirs 后填充
NEW_DIR=""
STATE_FILE=""

TS="$(date +%Y%m%dT%H%M%S)"
LOG_FILE="${LOG_FILE:-/tmp/pg18_bluegreen_$TS.log}"
CUTOVER_ACTIVE=0
SWAPPED=0
HYPER_OLD=""
COUNTS_OLD=""

# ---------- 输出助手 ----------
RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; CYAN='\033[0;36m'; NC='\033[0m'
say()  { printf "${CYAN}[bg  %s]${NC} %s\n" "$(date +%H:%M:%S)" "$*" | tee -a "$LOG_FILE" >&2; }
ok()   { printf "${GREEN}[ok  %s]${NC} %s\n" "$(date +%H:%M:%S)" "$*" | tee -a "$LOG_FILE" >&2; }
warn() { printf "${YELLOW}[warn%s]${NC} %s\n" " $(date +%H:%M:%S)" "$*" | tee -a "$LOG_FILE" >&2; }
die()  { printf "${RED}[FAIL%s]${NC} %s\n" " $(date +%H:%M:%S)" "$*" | tee -a "$LOG_FILE" >&2; exit 1; }
step() { say "── $* ──"; }

confirm() {
  [ "$YES" = "1" ] && { say "自动模式：跳过确认（$*）"; return 0; }
  printf "${YELLOW}[确认 %s]${NC} %s\n是否继续？输入 yes 继续: " "$(date +%H:%M:%S)" "$*" | tee -a "$LOG_FILE" >&2
  local a; read -r a
  [ "$a" = "yes" ] || die "用户取消"
}

# ---------- 通用助手 ----------
compose() { docker compose -f "$COMPOSE_FILE" "$@"; }
pg_exec() { docker exec "$PG_CONTAINER" psql -X -U "$POSTGRES_USER_DB" -d "$POSTGRES_DB_DB" -v ON_ERROR_STOP=1 "$@"; }

resolve_dirs() {
  # DATA_DIR 与 compose 的 ${DATA_DIR:-./data} 同源：显式 env > deploy/.env > ./data
  if [ -z "$DATA_DIR" ]; then
    if [ -f deploy/.env ]; then
      DATA_DIR="$(grep -E '^DATA_DIR=' deploy/.env | head -1 | cut -d= -f2- | tr -d "\"'" || true)"
    fi
    DATA_DIR="${DATA_DIR:-./data}"
  fi
  OLD_DIR="$DATA_DIR/pg18"
  NEW_DIR="$DATA_DIR/pg18_new"
  STATE_FILE="$DATA_DIR/pg18_upgrade.state"
  mkdir -p "$DATA_DIR"
}

load_state() {
  [ -f "$STATE_FILE" ] || return 1
  # shellcheck disable=SC1090
  . "$STATE_FILE"
  return 0
}

save_state() {
  cat > "$STATE_FILE" <<EOF
TS=$TS
OLD_IMAGE=$PG_OLD_IMAGE
NEW_IMAGE=$PG_NEW_IMAGE
SWAPPED=$SWAPPED
EOF
}

detect_old_image() {
  if [ -n "$PG_OLD_IMAGE" ]; then return 0; fi
  PG_OLD_IMAGE="$(docker inspect "$PG_CONTAINER" --format '{{.Config.Image}}' 2>/dev/null || true)"
  [ -n "$PG_OLD_IMAGE" ] || die "无法从运行容器 $PG_CONTAINER 探测旧镜像；请显式传 PG_OLD_IMAGE=<tag>"
}

disk_free_kb() { df -Pk "$1" | awk 'NR==2{print $4}'; }
dir_size_kb()  { du -sk "$1" 2>/dev/null | awk '{print $1}'; }

check_disk() { # $1=需要的额外 KB
  local free; free="$(disk_free_kb "$DATA_DIR")"
  if [ "${free:-0}" -lt "$1" ]; then
    die "磁盘不足：$DATA_DIR 可用 ${free}KB < 需要 ${1}KB；清理空间或 --skip-cold-snapshot 后重试"
  fi
}

container_running() {
  [ "$(docker inspect -f '{{.State.Running}}' "$1" 2>/dev/null)" = "true" ]
}

# rsync 所有权保留：root（prod）保留 uid/gid；非 root（macOS rehearsal）跳过，
# Docker Desktop 侧 uid 映射不受影响
if [ "$(id -u)" -eq 0 ]; then
  RSYNC_OWNER_FLAGS=""
else
  RSYNC_OWNER_FLAGS="--no-o --no-g"
fi

wait_pg_ready() { # $1=容器名 $2=超时秒
  local i=0
  while [ "$i" -lt "$2" ]; do
    docker exec "$1" pg_isready -U "$POSTGRES_USER_DB" -d "$POSTGRES_DB_DB" >/dev/null 2>&1 && return 0
    container_running "$1" || die "容器 $1 已退出，取证: docker logs $1"
    sleep 1; i=$((i+1))
  done
  die "等待 $1 就绪超时（${2}s）"
}

wait_healthz() {
  [ -z "$BACKEND_HEALTH_URL" ] && { say "未配置 BACKEND_HEALTH_URL，跳过健康检查"; return 0; }
  local i=0
  while [ "$i" -lt 24 ]; do
    curl -fsS --max-time 5 "$BACKEND_HEALTH_URL" >/dev/null 2>&1 && return 0
    sleep 5; i=$((i+1))
  done
  die "backend 健康检查超时：$BACKEND_HEALTH_URL"
}

capture_old_counts() { # 旧行数基线 → /tmp/pgbg_counts_old.$TS（表=行数）
  local f="/tmp/pgbg_counts_old.$TS" t n rows
  : > "$f"
  for t in $SMOKE_TABLES; do
    n="$(pg_exec -At -c "SELECT count(*) FROM pg_tables WHERE schemaname='public' AND tablename='$t'" 2>/dev/null || echo 0)"
    [ "$n" = "1" ] || continue
    rows="$(pg_exec -At -c "SELECT count(*) FROM \"$t\"")"
    printf '%s=%s\n' "$t" "$rows" >> "$f"
  done
  printf '%s' "$f"
}

# ============================================================
# 子命令：status
# ============================================================
status() {
  resolve_dirs
  say "COMPOSE_FILE=$COMPOSE_FILE  DATA_DIR=$DATA_DIR"
  if container_running "$PG_CONTAINER"; then
    say "canonical 容器 $PG_CONTAINER: 运行中，镜像=$(docker inspect -f '{{.Config.Image}}' "$PG_CONTAINER")"
  else
    say "canonical 容器 $PG_CONTAINER: 不存在/停止"
  fi
  say "canonical 目录 $OLD_DIR: $([ -d "$OLD_DIR" ] && du -sh "$OLD_DIR" | cut -f1 || echo 不存在)"
  say "新镜像 $PG_NEW_IMAGE: $(docker image inspect "$PG_NEW_IMAGE" >/dev/null 2>&1 && echo 本地已有 || echo 未拉取)"
  say "NEW_DIR $NEW_DIR: $([ -d "$NEW_DIR" ] && echo "存在残留 $(du -sh "$NEW_DIR" 2>/dev/null | cut -f1)" || echo 干净)"
  if load_state; then
    say "升级状态文件: TS=$TS SWAPPED=$SWAPPED OLD_IMAGE=$OLD_IMAGE NEW_IMAGE=$NEW_IMAGE"
  else
    say "无升级状态文件（未发起过升级）"
  fi
  ls -d "$DATA_DIR"/pg18_old_* "$DATA_DIR"/pg18_cold_* "$DATA_DIR"/pg18_failed_* 2>/dev/null || true
}

# ============================================================
# 阶段一：preflight + prepare（零停机）
# ============================================================
preflight() { # $1=mode: strict(prepare，禁残留) | cutover(允许 pg18_new=预拷贝产物)
  local mode="${1:-strict}"
  [ -f "$COMPOSE_FILE" ] || die "compose 文件不存在: $COMPOSE_FILE"
  [ -d "$OLD_DIR" ] || die "旧数据目录不存在: $OLD_DIR"
  container_running "$PG_CONTAINER" || die "旧容器未运行: $PG_CONTAINER"
  docker image inspect "$PG_NEW_IMAGE" >/dev/null 2>&1 \
    || die "新镜像不在本地: $PG_NEW_IMAGE（先 docker load / docker pull）"
  if [ "$mode" = "strict" ] && [ -e "$NEW_DIR" ]; then
    die "发现残留 $NEW_DIR；先跑 status 盘点，确认无用后手动删除再重试"
  fi
  # 磁盘：新目录(1x) + 冷快照(1x) + 余量(0.5x)；跳过快照则 1x+0.5x
  local need; need="$(dir_size_kb "$OLD_DIR")"
  if [ "$SKIP_COLD_SNAPSHOT" = "1" ]; then need=$((need * 15 / 10)); else need=$((need * 25 / 10)); fi
  check_disk "$need"
  detect_old_image
  ok "preflight 通过：旧镜像=$PG_OLD_IMAGE 数据=$(du -sh "$OLD_DIR" | cut -f1) 需额外磁盘≈$((need/1024))MB"
}

prepare() {
  resolve_dirs; step "阶段一 prepare（零停机）开始"
  preflight

  # 1) 全量备份 + 可恢复性证明（回滚锚 1）
  if [ "$SKIP_BACKUP" = "1" ]; then
    warn "--skip-backup：跳过全量备份（生产禁用此开关）"
  else
    step "全量备份 backup_pg.sh --full（加密+异地副本）"
    BACKUP_DIR="${BACKUP_DIR:-$DATA_DIR/backups/pg}" \
      bash "$REPO_ROOT/scripts/backup_pg.sh" --full
    step "备份可恢复性冒烟 restore_smoke.sh"
    BACKUP_DIR="${BACKUP_DIR:-$DATA_DIR/backups/pg}" \
      bash "$REPO_ROOT/scripts/restore_smoke.sh"
  fi

  # 2) 在线预拷贝（压小停机窗口；预拷贝撕裂文件由冻结后 delta rsync 修正：
  #    干净关库后文件不再变化，mtime 未变=内容一致，变过的 mtime 必更新）
  if [ "$SKIP_ONLINE_PRECOPY" = "1" ]; then
    warn "跳过在线预拷贝；停机窗口将执行全量拷贝"
  else
    step "在线预拷贝 rsync $OLD_DIR → $NEW_DIR"
    local t0=$SECONDS
    rsync -aH $RSYNC_OWNER_FLAGS --numeric-ids "$OLD_DIR/" "$NEW_DIR/"
    ok "预拷贝完成 $((SECONDS-t0))s（$(du -sh "$NEW_DIR" | cut -f1)）"
  fi

  ok "prepare 完成。可择窗口执行 cutover（预计 5-12 分钟停机）"
}

# ============================================================
# 阶段二：cutover（停机窗口）
# ============================================================
auto_rollback() {
  # 防重入：陷阱会在命令替换子 shell 与父 shell 各触发一次，文件守卫兜底
  local guard="/tmp/pg18_bg_rb_${TS}.flag"
  [ -e "$guard" ] && exit 1
  touch "$guard"
  CUTOVER_ACTIVE=0
  set +e
  printf "${RED}[自动回滚]${NC} cutover 失败(rc=$rc)，开始恢复旧栈...\n" | tee -a "$LOG_FILE" >&2
  [ -n "$BACKEND_SERVICE" ] && compose start "$BACKEND_SERVICE" >/dev/null 2>&1
  compose stop "$PG_SERVICE" >/dev/null 2>&1
  docker rm -f "$PG_CONTAINER" >/dev/null 2>&1
  if [ "$SWAPPED" = "1" ] && [ -d "$OLD_DIR.$TS" ]; then
    mv "$OLD_DIR" "$OLD_DIR.failed_$TS"
    mv "$OLD_DIR.$TS" "$OLD_DIR"
    if PG_IMAGE="$PG_OLD_IMAGE" compose up -d "$PG_SERVICE" >/dev/null 2>&1; then
      wait_pg_ready "$PG_CONTAINER" 60
    fi
    [ -n "$BACKEND_SERVICE" ] && compose start "$BACKEND_SERVICE" >/dev/null 2>&1
    ok "已回滚到旧镜像 $PG_OLD_IMAGE + 原数据；失败现场: $OLD_DIR.failed_$TS"
  else
    compose start "$PG_SERVICE" >/dev/null 2>&1
    [ -n "$BACKEND_SERVICE" ] && compose start "$BACKEND_SERVICE" >/dev/null 2>&1
    ok "尚未换目录，旧容器+backend 已恢复；残留检查: $NEW_DIR"
  fi
  die "cutover 失败已自动回滚。取证日志: $LOG_FILE"
}

on_exit() {
  # 覆盖一切退出路径（含 die 的显式 exit——EXIT 不触发 ERR 陷阱）；
  # die 在 auto_rollback 内部退出时 CUTOVER_ACTIVE 已复位，不会递归
  local rc=$?
  if [ "$CUTOVER_ACTIVE" = "1" ]; then
    auto_rollback "$rc"
  fi
}

cutover() {
  resolve_dirs; step "阶段二 cutover（停机窗口）开始"
  preflight cutover
  confirm "即将停止 backend 并切换数据库镜像（预计 5-12 分钟停机）"
  CUTOVER_ACTIVE=1
  trap on_exit EXIT

  # 1) 停写：backend 先停（此后 PG 无新写入）
  if [ -n "$BACKEND_SERVICE" ]; then
    step "停止 backend ($BACKEND_SERVICE)"
    compose stop "$BACKEND_SERVICE"
  fi

  # 2) 冻结前基线（趁旧容器还在运行采集）
  step "采集旧库基线（行数抽检/hypertable 数）"
  COUNTS_OLD="$(capture_old_counts)"
  HYPER_OLD="$(pg_exec -At -c "SELECT count(*) FROM timescaledb_information.hypertables" 2>/dev/null || echo 0)"

  # 3) 优雅停旧库（干净关库=checkpoint，是 delta rsync 正确性前提）
  step "停止旧 PG（优雅关库 checkpoint）"
  compose stop "$PG_SERVICE"

  # 4) 冷快照（回滚锚 2；无压缩速度优先）
  if [ "$SKIP_COLD_SNAPSHOT" = "1" ]; then
    warn "跳过冷快照"
  else
    step "冷快照 pg18_cold_$TS.tar"
    local t0=$SECONDS
    tar -C "$DATA_DIR" -cf "$DATA_DIR/pg18_cold_$TS.tar" pg18
    ok "冷快照完成 $((SECONDS-t0))s（$(du -sh "$DATA_DIR/pg18_cold_$TS.tar" | cut -f1)）"
  fi

  # 5) delta 同步（--delete 镜像化；只传变化文件）
  step "delta 同步 rsync（只传变化文件）"
  local t1=$SECONDS
  rsync -aH $RSYNC_OWNER_FLAGS --numeric-ids --delete --stats "$OLD_DIR/" "$NEW_DIR/" 2>&1 \
    | grep -E 'Number of files|transferred|total size' | tee -a "$LOG_FILE" >&2 || true
  ok "delta 同步完成 $((SECONDS-t1))s"

  # 6) 换目录（同文件系统原子改名）+ 移除旧容器（给 compose 让出名字）
  step "换数据目录: $OLD_DIR → $OLD_DIR.$TS ; $NEW_DIR → $OLD_DIR"
  SWAPPED=1
  docker rm -f "$PG_CONTAINER" >/dev/null 2>&1 || true
  mv "$OLD_DIR" "$OLD_DIR.$TS"
  mv "$NEW_DIR" "$OLD_DIR"
  save_state

  # 7) 新镜像起 canonical（compose 原配置，仅注入 PG_IMAGE）
  step "新镜像启动 $PG_CONTAINER（$PG_NEW_IMAGE）"
  PG_IMAGE="$PG_NEW_IMAGE" compose up -d "$PG_SERVICE"
  wait_pg_ready "$PG_CONTAINER" 90

  # 8) 立即 ALTER EXTENSION（实证：不做这步，库内所有查询报错；
  #    catalog 错位模式下普通 SELECT 也报错——禁止预查询清单，
  #    按固定顺序逐个容错尝试，timescaledb 必须第一个跑通以解锁后续查询）
  step "升级库内扩展（timescaledb 优先，逐个容错尝试）"
  local e t2 out
  for e in $ALTERABLE_EXTS; do
    t2=$SECONDS
    out="$(pg_exec -v ON_ERROR_STOP=0 -c "ALTER EXTENSION \"$e\" UPDATE" 2>&1 || true)"
    if printf '%s' "$out" | grep -qi 'does not exist'; then
      say "  $e 未安装，跳过"
    elif printf '%s' "$out" | grep -qi 'ERROR'; then
      warn "  $e ALTER 报错（verify 门禁兜底）: $(printf '%s' "$out" | grep -i error | head -1)"
    else
      ok "ALTER EXTENSION $e 完成 $((SECONDS-t2))s"
    fi
  done

  # 9) 硬验证门禁
  verify_impl

  # 10) 起 backend + 健康检查
  if [ -n "$BACKEND_SERVICE" ]; then
    step "启动 backend"
    compose start "$BACKEND_SERVICE"
    step "等待 backend 健康: $BACKEND_HEALTH_URL"
    wait_healthz
  fi

  CUTOVER_ACTIVE=0
  trap - EXIT
  ok "✅ cutover 完成，停机窗口闭合"
  say "回滚锚: $OLD_DIR.$TS（旧数据目录原样保留）"
  say "稳定观察 ${CLEANUP_MIN_AGE_DAYS} 天后执行: $0 cleanup"
}

# ============================================================
# 验证门禁（cutover 内强制；verify 子命令复跑）
# ============================================================
verify_impl() {
  step "验证门禁开始"
  # 1) 扩展版本 = 镜像内置默认版本（ALTER 生效的充分条件）
  local e cur def
  for e in $ALTERABLE_EXTS; do
    cur="$(pg_exec -At -c "SELECT extversion FROM pg_extension WHERE extname='$e'" || true)"
    [ -z "$cur" ] && continue    # 未安装的不校验
    def="$(pg_exec -At -c "SELECT default_version FROM pg_available_extensions WHERE name='$e'")"
    [ "$cur" = "$def" ] || die "验证失败: $e extversion=$cur ≠ 镜像默认 $def"
    ok "扩展 $e=$cur ✓"
  done
  # 2) hypertable 数不变
  local hy; hy="$(pg_exec -At -c "SELECT count(*) FROM timescaledb_information.hypertables" 2>/dev/null || echo 0)"
  [ -z "$HYPER_OLD" ] || [ "$hy" = "$HYPER_OLD" ] || die "验证失败: hypertable 数 $hy ≠ 旧库 $HYPER_OLD"
  ok "hypertable 数 $hy ✓"
  # 3) 行数抽检逐表相等（COUNTS_OLD 非空文件时启用）
  if [ -n "$COUNTS_OLD" ] && [ -s "$COUNTS_OLD" ]; then
    local t old_n new_n
    for t in $SMOKE_TABLES; do
      grep -q "^$t=" "$COUNTS_OLD" || continue
      old_n="$(grep "^$t=" "$COUNTS_OLD" | cut -d= -f2)"
      new_n="$(pg_exec -At -c "SELECT count(*) FROM \"$t\"")"
      [ "$old_n" = "$new_n" ] || die "验证失败: $t 行数 $new_n ≠ 旧库 $old_n"
      ok "表 $t rows=$new_n ✓"
    done
  else
    say "（无旧库基线，跳过行数门）"
  fi
  # 4) timescale launcher 已接入（先取全量再 grep，避免 grep -q 早退致 SIGPIPE 竞态）
  local dlog; dlog="$(docker logs "$PG_CONTAINER" 2>&1 || true)"
  printf '%s' "$dlog" | grep -q "background worker launcher connected" \
    || die "验证失败: TimescaleDB launcher 未接入（docker logs $PG_CONTAINER）"
  ok "TimescaleDB launcher ✓"
  ok "验证门禁全绿"
}

verify() {
  resolve_dirs
  container_running "$PG_CONTAINER" || die "$PG_CONTAINER 未运行"
  HYPER_OLD=""          # 复跑无对照
  COUNTS_OLD=""         # 复跑跳过行数门
  verify_impl
  ok "verify（复跑模式）完成"
}

# ============================================================
# 手动回滚
# ============================================================
rollback() {
  resolve_dirs
  load_state || die "无升级状态文件，无法回滚（$STATE_FILE）"
  [ -d "$OLD_DIR.$TS" ] || die "回滚锚目录不存在: $OLD_DIR.$TS"
  confirm "回滚：数据库切回 $OLD_IMAGE，当前(新)数据保留到 $OLD_DIR.failed_$TS"
  step "停止 backend + PG"
  if [ -n "$BACKEND_SERVICE" ]; then compose stop "$BACKEND_SERVICE" || true; fi
  compose stop "$PG_SERVICE" || true
  docker rm -f "$PG_CONTAINER" >/dev/null 2>&1 || true
  step "目录还原"
  mv "$OLD_DIR" "$OLD_DIR.failed_$TS"
  mv "$OLD_DIR.$TS" "$OLD_DIR"
  step "旧镜像 $OLD_IMAGE 起 canonical"
  PG_IMAGE="$OLD_IMAGE" compose up -d "$PG_SERVICE"
  wait_pg_ready "$PG_CONTAINER" 90
  if [ -n "$BACKEND_SERVICE" ]; then
    compose start "$BACKEND_SERVICE"
    wait_healthz
  fi
  ok "✅ 已回滚。失败现场: $OLD_DIR.failed_$TS（排查后可删）"
}

# ============================================================
# 清理回滚锚（人工确认）
# ============================================================
cleanup() {
  resolve_dirs
  local now candidates c age mt
  now="$(date +%s)"
  candidates="$(ls -d "$DATA_DIR"/pg18_old_* "$DATA_DIR"/pg18_cold_*.tar "$DATA_DIR"/pg18_failed_* 2>/dev/null || true)"
  [ -n "$candidates" ] || { ok "无待清理回滚锚"; return 0; }
  say "回滚锚清单："
  for c in $candidates; do
    mt="$(stat -f %m "$c" 2>/dev/null || stat -c %Y "$c" 2>/dev/null || echo "$now")"
    age=$(( (now - mt) / 86400 ))
    say "  $c（${age} 天前，$(du -sh "$c" | cut -f1)）"
  done
  confirm "删除上列回滚锚（不可逆）？建议确认新栈稳定 ≥${CLEANUP_MIN_AGE_DAYS} 天"
  # shellcheck disable=SC2086
  rm -rf $candidates
  ok "清理完成"
}

# ============================================================
# 参数解析与入口
# ============================================================
CMD=""
while [ $# -gt 0 ]; do
  case "$1" in
    status|prepare|cutover|verify|rollback|cleanup|all) CMD="$1"; shift ;;
    --yes) YES=1; shift ;;
    --skip-backup) SKIP_BACKUP=1; shift ;;
    --skip-cold-snapshot) SKIP_COLD_SNAPSHOT=1; shift ;;
    --skip-online-precopy) SKIP_ONLINE_PRECOPY=1; shift ;;
    --new-image) PG_NEW_IMAGE="$2"; shift 2 ;;
    *) die "未知参数: $1" ;;
  esac
done
if [ -z "$CMD" ]; then
  sed -n '/^# 用法/,/^# ============================================================$/p' "$0" | sed 's/^# \{0,1\}//'
  exit 1
fi

case "$CMD" in
  status)   status ;;
  prepare)  prepare ;;
  cutover)  cutover ;;
  verify)   verify ;;
  rollback) rollback ;;
  cleanup)  cleanup ;;
  all)      prepare && cutover ;;
esac
