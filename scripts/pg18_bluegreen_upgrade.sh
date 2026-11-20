#!/usr/bin/env bash
# ============================================================
# PG18 镜像蓝绿升级 / Blue-green image upgrade for imboy_pg18
# ------------------------------------------------------------
# 面向 deploy/docker-compose.prod.yml 的 imboy_pg18 服务：
# 物理同步（预拷贝+冻结窗口 rsync delta）+ 立即 ALTER EXTENSION
# + 硬验证门禁。放行写入前失败自动回滚；放行后失败保留现场、停写待人工恢复。
# backup_pg.sh + restore_smoke 是单库逻辑备份/同实例恢复冒烟；异地恢复须另行验收。
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
#   1. backup_pg.sh --full + restore_smoke.sh   单库逻辑备份及恢复冒烟
#   2. pg18_cold_<ts>.tar                       停机窗口内冷快照（秒级回滚点）
#   3. pg18.<ts>/                               旧数据目录原样保留（切后回滚锚）
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
ALLOW_DATA_LOSS="${ALLOW_DATA_LOSS:-0}"       # 手动回滚已放行写入的升级，须显式接受丢弃新写入

OLD_DIR=""      # canonical 数据目录（compose 挂载点），resolve_dirs 后填充
NEW_DIR=""
STATE_FILE=""

TS="$(date +%Y%m%dT%H%M%S)_$$"
LOG_FILE="${LOG_FILE:-/tmp/pg18_bluegreen_$TS.log}"
CUTOVER_ACTIVE=0
SWAPPED=0
PHASE=idle
WRITES_RELEASED=0
HYPER_OLD=""
COUNTS_OLD=""
INSTALLED_EXTS=""

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
  local key value seen="|"
  TS=""; OLD_IMAGE=""; NEW_IMAGE=""; SWAPPED=""
  PHASE=legacy; WRITES_RELEASED=1  # 旧格式不能证明从未恢复写入
  while IFS='=' read -r key value || [ -n "$key$value" ]; do
    case "$key" in
      TS|OLD_IMAGE|NEW_IMAGE|SWAPPED|PHASE|WRITES_RELEASED) ;;
      *) die "状态文件含未知字段: $STATE_FILE" ;;
    esac
    case "$seen" in *"|$key|"*) die "状态文件含重复字段: $key" ;; esac
    seen="$seen$key|"
    printf -v "$key" '%s' "$value"
  done < "$STATE_FILE"
  [[ "$TS" =~ ^[0-9]{8}T[0-9]{6}(_[0-9]+)?$ ]] || die "状态文件 TS 无效"
  [[ "$OLD_IMAGE" =~ ^[a-zA-Z0-9][a-zA-Z0-9._/:@-]*$ && "$NEW_IMAGE" =~ ^[a-zA-Z0-9][a-zA-Z0-9._/:@-]*$ ]] || die "状态文件镜像无效"
  [[ "$SWAPPED" =~ ^[01]$ && "$WRITES_RELEASED" =~ ^[01]$ ]] || die "状态文件标志无效"
  case "$PHASE" in
    legacy|cutover|swapping|swapped|writes_released|complete|rolling_back|rolled_back|manual_required) ;;
    *) die "状态文件阶段无效: $PHASE" ;;
  esac
  return 0
}

save_state() {
  [ ! -d "$STATE_FILE" ] && [ ! -L "$STATE_FILE" ] || return 1
  local tmp; tmp="$(mktemp "$STATE_FILE.tmp.XXXXXXXX")" || return 1
  (umask 077; printf 'TS=%s\nOLD_IMAGE=%s\nNEW_IMAGE=%s\nSWAPPED=%s\nPHASE=%s\nWRITES_RELEASED=%s\n' \
    "$TS" "$PG_OLD_IMAGE" "$PG_NEW_IMAGE" "$SWAPPED" "$PHASE" "$WRITES_RELEASED" > "$tmp") || return 1
  mv -f "$tmp" "$STATE_FILE"
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
    container_running "$1" || { printf '容器 %s 已退出\n' "$1" >&2; return 1; }
    sleep 1; i=$((i+1))
  done
  printf '等待 %s 就绪超时（%ss）\n' "$1" "$2" >&2
  return 1
}

wait_healthz() {
  [ -z "$BACKEND_HEALTH_URL" ] && { printf '未配置 BACKEND_HEALTH_URL，跳过健康检查\n' >&2; return 0; }
  local i=0
  while [ "$i" -lt 24 ]; do
    curl -fsS --max-time 5 "$BACKEND_HEALTH_URL" >/dev/null 2>&1 && return 0
    sleep 5; i=$((i+1))
  done
  printf 'backend 健康检查超时：%s\n' "$BACKEND_HEALTH_URL" >&2
  return 1
}

capture_old_counts() { # 旧行数基线（表=行数）；调用方不得把失败转换为空基线
  local t n rows
  : > "$COUNTS_OLD" || return 1
  for t in $SMOKE_TABLES; do
    [[ "$t" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ ]] || return 1
    n="$(pg_exec -At -c "SELECT count(*) FROM pg_tables WHERE schemaname='public' AND tablename='$t'")" || return 1
    case "$n" in 0) continue ;; 1) ;; *) return 1 ;; esac
    rows="$(pg_exec -At -c "SELECT count(*) FROM \"$t\"")" || return 1
    [[ "$rows" =~ ^[0-9]+$ ]] || return 1
    printf '%s=%s\n' "$t" "$rows" >> "$COUNTS_OLD" || return 1
  done
  [ -s "$COUNTS_OLD" ]  # 不能把零张受检表报告为行数门通过
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
    say "升级状态文件: TS=$TS PHASE=$PHASE SWAPPED=$SWAPPED WRITES_RELEASED=$WRITES_RELEASED OLD_IMAGE=$OLD_IMAGE NEW_IMAGE=$NEW_IMAGE"
  else
    say "无升级状态文件（未发起过升级）"
  fi
  ls -d "$OLD_DIR".[0-9]* "$DATA_DIR"/pg18_cold_* "$OLD_DIR".failed_* 2>/dev/null || true
}

# ============================================================
# 阶段一：preflight + prepare（零停机）
# ============================================================
preflight() { # $1=mode: strict(prepare，禁残留) | cutover(允许 pg18_new=预拷贝产物)
  local mode="${1:-strict}"
  [ -f "$COMPOSE_FILE" ] || die "compose 文件不存在: $COMPOSE_FILE"
  [ -d "$OLD_DIR" ] || die "旧数据目录不存在: $OLD_DIR"
  [ ! -L "$OLD_DIR" ] && [ ! -L "$NEW_DIR" ] || die "数据根目录不可为符号链接"
  [ ! -e "$NEW_DIR" ] || [ -d "$NEW_DIR" ] || die "预拷贝目标不是目录: $NEW_DIR"
  # 不覆盖尚未处置的恢复记录；子 shell 避免改变本次 TS/镜像。
  if [ -e "$STATE_FILE" ]; then
    (load_state && [ "$PHASE" = "rolled_back" ]) || die "已有升级记录；先核对 state/目录并归档，禁止盲目重跑"
  fi
  [ ! -e "$OLD_DIR.$TS" ] && [ ! -e "$OLD_DIR.failed_$TS" ] || die "本次回滚锚路径已存在"
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

  # 2) 在线预拷贝；冻结后的 --checksum 同步才可纠正相同大小/mtime 的内容变化。
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
move_dir() {
  # mv 对已存在的目录会做嵌套移动；这里必须是不存在的目标。
  [ -d "$1" ] && [ ! -L "$1" ] && [ ! -e "$2" ] && [ ! -L "$2" ] \
    || { printf '拒绝不明确的目录改名: %s -> %s\n' "$1" "$2" >&2; return 1; }
  mv "$1" "$2"
}

stop_stack() {
  # 回滚安全动作不能依赖可写日志盘；每个操作显式检查。
  if [ -n "$BACKEND_SERVICE" ]; then compose stop "$BACKEND_SERVICE" || return 1; fi
  compose stop "$PG_SERVICE" || return 1
}

restore_old_stack() {
  stop_stack || return 1
  PHASE=rolling_back
  save_state || return 1
  if [ -d "$OLD_DIR.$TS" ]; then
    if [ -e "$OLD_DIR" ]; then
      move_dir "$OLD_DIR" "$OLD_DIR.failed_$TS" || return 1
    fi
    move_dir "$OLD_DIR.$TS" "$OLD_DIR" || return 1
  else
    [ "$SWAPPED" = "0" ] && [ -d "$OLD_DIR" ] || return 1
  fi
  SWAPPED=0
  WRITES_RELEASED=0
  save_state || return 1
  # compose 负责重建容器及绑定目录，不能 rm 后再 start 一个已不存在的容器。
  PG_IMAGE="$PG_OLD_IMAGE" compose up -d --no-deps --force-recreate "$PG_SERVICE" || return 1
  wait_pg_ready "$PG_CONTAINER" 90 || return 1
  pg_exec -At -c 'SELECT 1' >/dev/null || return 1
  PHASE=rolled_back
  save_state || return 1  # 先确认旧库及恢复记录可用，再允许 backend 写入
  if [ -n "$BACKEND_SERVICE" ]; then
    if compose start "$BACKEND_SERVICE" && wait_healthz; then
      :
    else
      compose stop "$BACKEND_SERVICE" || printf '恢复失败且无法确认 backend 已停，需立即人工隔离写入\n' >&2
      return 1
    fi
  fi
  return 0
}

auto_rollback() {
  local rc="$1"
  CUTOVER_ACTIVE=0
  printf '[自动回滚] cutover 失败(rc=%s)；先停止 backend 和 PG\n' "$rc" >&2
  if [ "$WRITES_RELEASED" = "1" ]; then
    # backend 启动尝试后可能已经产生写入；禁止自动丢弃当前数据。
    stop_stack || { printf '停写失败；需立即人工隔离，禁止目录切换\n' >&2; return 1; }
    PHASE=manual_required
    save_state || printf '无法保存状态；保留全部目录，需人工核对\n' >&2
    printf '已放行过写入：保留新旧数据，禁止自动回退，需人工恢复。日志: %s\n' "$LOG_FILE" >&2
  elif restore_old_stack; then
    printf '已回滚到旧镜像 %s + 原数据；升级仍以失败退出。日志: %s\n' "$PG_OLD_IMAGE" "$LOG_FILE" >&2
  else
    PHASE=manual_required
    save_state || printf '无法保存恢复状态；需人工核对目录\n' >&2
    printf '自动回滚未完成；禁止继续切换或清理，需人工恢复。日志: %s\n' "$LOG_FILE" >&2
  fi
  return 1
}

on_exit() {
  local rc=$?
  [ "$BASH_SUBSHELL" -eq 0 ] || return "$rc"
  trap - EXIT
  if [ "$CUTOVER_ACTIVE" = "1" ]; then
    auto_rollback "$rc"
  fi
  return "$rc"
}

cutover() {
  resolve_dirs; step "阶段二 cutover（停机窗口）开始"
  preflight cutover
  confirm "即将停止 backend 并切换数据库镜像（预计 5-12 分钟停机）"
  PHASE=cutover
  save_state || die "无法保存升级状态；尚未停写"
  CUTOVER_ACTIVE=1
  trap on_exit EXIT
  trap 'exit 130' INT
  trap 'exit 143' TERM

  # 1) 停写：backend 先停（此后 PG 无新写入）
  if [ -n "$BACKEND_SERVICE" ]; then
    step "停止 backend ($BACKEND_SERVICE)"
    compose stop "$BACKEND_SERVICE"
  fi

  # 2) 冻结前基线（趁旧容器还在运行采集）
  step "采集旧库基线（行数抽检/hypertable 数）"
  COUNTS_OLD="$DATA_DIR/pg18_counts_$TS"
  capture_old_counts || die "采集旧库行数基线失败"
  HYPER_OLD="$(pg_exec -At -c "SELECT count(*) FROM timescaledb_information.hypertables")"
  [[ "$HYPER_OLD" =~ ^[0-9]+$ ]] || die "旧库 hypertable 基线无效"
  # 旧库仍可查询时确认安装清单；新镜像启动后的第一个 SQL 仍是 timescaledb ALTER。
  local e cur
  for e in timescaledb $ALTERABLE_EXTS; do
    case " $INSTALLED_EXTS " in *" $e "*) continue ;; esac
    [[ "$e" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ ]] || die "扩展名无效: $e"
    cur="$(pg_exec -At -c "SELECT extversion FROM pg_extension WHERE extname='$e'")"
    if [ -n "$cur" ]; then INSTALLED_EXTS="$INSTALLED_EXTS $e"; fi
  done
  case " $INSTALLED_EXTS " in *' timescaledb '*) ;; *) die "旧库缺少 timescaledb" ;; esac

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

  # 5) 冻结后按内容校验；rsync/日志管道任一失败均禁止切换。
  step "delta 同步 rsync（--checksum 校验内容）"
  local t1=$SECONDS
  rsync -aH $RSYNC_OWNER_FLAGS --numeric-ids --checksum --delete --stats "$OLD_DIR/" "$NEW_DIR/" 2>&1 \
    | tee -a "$LOG_FILE" >&2
  ok "delta 同步完成 $((SECONDS-t1))s"

  # 6) 两次独立改名并非一个事务；先记录意图，恢复时兼容只完成第一次改名。
  step "换数据目录: $OLD_DIR → $OLD_DIR.$TS ; $NEW_DIR → $OLD_DIR"
  PHASE=swapping
  save_state
  move_dir "$OLD_DIR" "$OLD_DIR.$TS"
  SWAPPED=1
  move_dir "$NEW_DIR" "$OLD_DIR"
  PHASE=swapped
  save_state

  # 7) 新镜像起 canonical（compose 原配置，仅注入 PG_IMAGE）
  step "新镜像启动 $PG_CONTAINER（$PG_NEW_IMAGE）"
  PG_IMAGE="$PG_NEW_IMAGE" compose up -d --no-deps --force-recreate "$PG_SERVICE"
  wait_pg_ready "$PG_CONTAINER" 90

  # 8) 立即 ALTER EXTENSION（实证：不做这步，库内所有查询报错；
  #    catalog 错位模式下普通 SELECT 也报错——禁止预查询清单，
  #    使用旧库清单，timescaledb 必须第一个跑通；任一 ALTER 失败立即中止）
  step "升级库内扩展（timescaledb 优先）"
  local t2
  for e in $INSTALLED_EXTS; do
    t2=$SECONDS
    pg_exec -c "ALTER EXTENSION \"$e\" UPDATE"
    ok "ALTER EXTENSION $e 完成 $((SECONDS-t2))s"
  done

  # 9) 硬验证门禁
  verify_impl

  # 10) 起 backend + 健康检查
  if [ -n "$BACKEND_SERVICE" ]; then
    step "启动 backend"
    WRITES_RELEASED=1
    PHASE=writes_released
    save_state  # 在启动尝试之前持久化；启动返回失败也可能已发生写入
    compose start "$BACKEND_SERVICE"
    step "等待 backend 健康: $BACKEND_HEALTH_URL"
    wait_healthz
  fi

  PHASE=complete
  save_state
  CUTOVER_ACTIVE=0
  trap - EXIT INT TERM
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
  for e in ${INSTALLED_EXTS:-$ALTERABLE_EXTS}; do
    cur="$(pg_exec -At -c "SELECT extversion FROM pg_extension WHERE extname='$e'")"
    if [ -z "$cur" ]; then
      [ -z "$INSTALLED_EXTS" ] && [ "$e" != "timescaledb" ] && continue
      die "验证失败: 已安装扩展 $e 消失"
    fi
    def="$(pg_exec -At -c "SELECT default_version FROM pg_available_extensions WHERE name='$e'")"
    [ "$cur" = "$def" ] || die "验证失败: $e extversion=$cur ≠ 镜像默认 $def"
    ok "扩展 $e=$cur ✓"
  done
  # 2) hypertable 数不变
  local hy; hy="$(pg_exec -At -c "SELECT count(*) FROM timescaledb_information.hypertables")"
  [[ "$hy" =~ ^[0-9]+$ ]] || die "验证失败: hypertable 计数无效"
  [ -z "$HYPER_OLD" ] || [ "$hy" = "$HYPER_OLD" ] || die "验证失败: hypertable 数 $hy ≠ 旧库 $HYPER_OLD"
  ok "hypertable 数 $hy ✓"
  # 3) cutover 的基线必须可读、非空；只有独立 verify 显式不带对照。
  if [ -n "$COUNTS_OLD" ]; then
    local t old_n new_n counts
    counts="$(cat "$COUNTS_OLD")" || die "验证失败: 旧库基线不可读"
    [ -n "$counts" ] || die "验证失败: 旧库基线为空"
    while IFS='=' read -r t old_n; do
      [[ "$t" =~ ^[a-zA-Z_][a-zA-Z0-9_]*$ && "$old_n" =~ ^[0-9]+$ ]] || die "验证失败: 旧库基线格式错误"
      new_n="$(pg_exec -At -c "SELECT count(*) FROM \"$t\"")"
      [ "$old_n" = "$new_n" ] || die "验证失败: $t 行数 $new_n ≠ 旧库 $old_n"
      ok "表 $t rows=$new_n ✓"
    done <<< "$counts"
  else
    say "（无旧库基线，跳过行数门）"
  fi
  # 4) timescale launcher 已接入（先取全量再 grep，避免 grep -q 早退致 SIGPIPE 竞态）
  local dlog; dlog="$(docker logs "$PG_CONTAINER" 2>&1)"
  grep -q "background worker launcher connected" <<< "$dlog" \
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
  [ "$PHASE" != "rolled_back" ] || die "此升级已回滚，拒绝重复移动目录"
  if [ "$WRITES_RELEASED" = "1" ] && [ "$ALLOW_DATA_LOSS" != "1" ]; then
    die "已放行或无法排除新写入；先保全/对账，明确接受放弃新写入后设 ALLOW_DATA_LOSS=1"
  fi
  [ -d "$OLD_DIR.$TS" ] || { [ "$SWAPPED" = "0" ] && [ -d "$OLD_DIR" ]; } \
    || die "回滚锚目录不存在: $OLD_DIR.$TS"
  confirm "回滚：数据库切回 $OLD_IMAGE；将放弃切换后的新写入，当前数据保留到 $OLD_DIR.failed_$TS"
  PG_OLD_IMAGE="$OLD_IMAGE"; PG_NEW_IMAGE="$NEW_IMAGE"
  if restore_old_stack; then
    ok "✅ 已回滚。保留现场: $OLD_DIR.failed_$TS"
  else
    PHASE=manual_required
    save_state || printf '无法保存恢复状态；需人工核对目录\n' >&2
    die "回滚未完成；禁止继续切换或清理，需人工恢复"
  fi
}

# ============================================================
# 清理回滚锚（人工确认）
# ============================================================
cleanup() {
  status
  # 原目录 mtime 不能证明升级完成时间；本补丁不扩展成保留期管理系统。
  die "cleanup 仅列清单：自动删除已禁用。须另行核对恢复状态、备份和观察期后人工审批"
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
  all)      prepare; cutover ;;
esac
