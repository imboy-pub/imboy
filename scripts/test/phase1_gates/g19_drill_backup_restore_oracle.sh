#!/usr/bin/env bash
# ============================================================================
# G19 / 设计门 F3 —— 演练·PG/Garage 联合备份恢复一致性 oracle（AC29 核心）
# ----------------------------------------------------------------------------
# 依据: Q12-final-gate-runners.md §3.3（L153-163）+ §1 资产表（L135-138）
#   五步骨架（设计 L154-162）:
#     step1 造数据  —— 账号 A/B + C2C N 条 + C2G M 条 + 附件 round-trip
#                      （复用 golden_upgrade 数据生成段 API，L155）
#     step2 备份    —— backup_pg.sh（--full）+ backup_garage.sh → 产物各记 sha256
#     step3 破坏+恢复 —— DROP 演练库 → restore_pg.sh <dump> --target <独立名，
#                      绝不触生产守卫红线（L157）→ rclone copy 回新 bucket
#     step4 oracle 三集合核验（L158-162）:
#        a) 消息 ID 集合: 恢复前后 msg_store(conversation_id,id) 导出
#           sort|sha256 比对，集合差必须为空（不允许只比 count，L159）
#        b) 附件哈希: attachment 全量行 × 恢复桶对象字节 md5 逐一比对
#           （attachment.md5 == 对象 md5）不匹配数=0；object_key 集合
#           DB↔桶双向 diff 为空（无缺失/无孤儿，L160）
#        c) 迁移水位: 恢复后 schema_migrations 版本集合 == 备份时 == 仓内
#           priv/migrations head（L161）
#     step5 快速线  —— restore_pg 自带行数抽样 + restore_smoke.sh MIN_TABLES（L162）
# TODO 说明: 设计文档只给步骤与 oracle 语义，未提供单条可执行命令——
#   各步骤写为 TODO 占位 + 资产入口；Q12 执行时按本骨架落命令与对比脚本。
# 互斥: 由总控 phase1_gate_all.sh 持有 /tmp/phase1-node.lock（连库 + 起演练栈）。
# 预计: 单门 ~0.5h 量级；Phase F 三件合计 ~1.5–2.5h（设计 §4 L181）
# 环境变量（实跑时；默认值仅示意隔离前缀，绝不默认指向生产名）:
#   DRILL_TARGET_DB    造数据目标库（默认 p1drill；§3.2 方案 B L145 隔离口径）
#   DRILL_RESTORE_DB   恢复目标库（默认 p1drill_restore；独立名，L157）
#   DRILL_BUCKET       Garage 演练 bucket（默认 p1drill）
#   DRILL_BACKUP_DIR   备份产物目录（默认 ./data-p1drill/backups）
#   PG_CONTAINER       backup_pg.sh 用（默认 imboy_pg18，资产表 L135）
# 用法:
#   bash g19_drill_backup_restore_oracle.sh [--dry-run]
# ============================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
IMBOY_ROOT="${IMBOY_ROOT:-$(cd "$SCRIPT_DIR/../../.." && pwd)}"

usage() {
  cat <<'EOF'
用法: bash g19_drill_backup_restore_oracle.sh [选项]
  --dry-run    只打印将执行的步骤与命令占位，不执行
  -h, --help   显示本帮助
门: G19（设计 F3）演练·PG/Garage 联合备份恢复 oracle（三集合核验）
env: DRILL_TARGET_DB / DRILL_RESTORE_DB / DRILL_BUCKET / DRILL_BACKUP_DIR / PG_CONTAINER
EOF
}

DRY_RUN=0
for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "ERROR: 未知参数 $arg" >&2; usage >&2; exit 64 ;;
  esac
done

DRILL_TARGET_DB="${DRILL_TARGET_DB:-p1drill}"
DRILL_RESTORE_DB="${DRILL_RESTORE_DB:-p1drill_restore}"
DRILL_BUCKET="${DRILL_BUCKET:-p1drill}"
DRILL_BACKUP_DIR="${DRILL_BACKUP_DIR:-$IMBOY_ROOT/data-p1drill/backups}"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"

cd "$IMBOY_ROOT"

# ---- step1 造数据（TODO，设计 L155）------------------------------------
step1_seed_data() {
  # TODO(Q12, 设计 L155): 复用 golden_upgrade 的数据生成段 API 造数据：
  #   账号 A/B signup/login、C2C N 条、C2G M 条（ws_c2g_send.py）、
  #   附件 presign/confirm + round-trip；指向演练栈/隔离后端（9831）。
  # TODO 参考入口: scripts/golden_upgrade.sh 数据生成段（资产表 L134）
  echo "TODO(Q12): step1 造数据（设计 §3.3 L155——复用 golden_upgrade 数据生成段 API）"
  return 1
}

# ---- step2 备份（真实资产入口 + TODO 落参，设计 L156-157 / 资产表 L135/L138）----
step2_backup() {
  # TODO(Q12): 按演练栈实际连接参数落 env；两个产物各记 sha256（设计 L157）
  # PG_CONTAINER="$PG_CONTAINER" POSTGRES_DB="$DRILL_TARGET_DB" \
  #   BACKUP_DIR="$DRILL_BACKUP_DIR" bash scripts/backup_pg.sh --full
  # GARAGE_REMOTE=<演练 remote> BUCKET="$DRILL_BUCKET" \
  #   DEST="$DRILL_BACKUP_DIR/garage" bash scripts/backup_garage.sh
  # shasum -a 256 "$DRILL_BACKUP_DIR"/*.dump "$DRILL_BACKUP_DIR"/garage/**/*
  echo "TODO(Q12): step2 备份 backup_pg.sh + backup_garage.sh + 产物 sha256（设计 §3.3 L156-157）"
  return 1
}

# ---- step3 破坏+恢复（真实资产入口 + TODO 落参，设计 L157 / 资产表 L136）----
step3_drop_and_restore() {
  # TODO(Q12): DROP 演练库（仅 DRILL_TARGET_DB，绝不触生产守卫红线，设计 L157）；
  #   恢复到独立名 DRILL_RESTORE_DB；rclone copy 备份目录回新 bucket。
  # bash scripts/restore_pg.sh "$DRILL_BACKUP_DIR/<dump>" --target "$DRILL_RESTORE_DB"
  # rclone copy "$DRILL_BACKUP_DIR/garage" "<演练remote>:$DRILL_BUCKET-restore"
  echo "TODO(Q12): step3 DROP 演练库 + restore_pg.sh --target $DRILL_RESTORE_DB + rclone 回桶（设计 §3.3 L157）"
  return 1
}

# ---- step4 oracle 三集合核验（TODO 对比脚本，设计 L158-162）----------------
step4_oracle() {
  # TODO(Q12, 设计 L158-162): SQL+对象存储对比脚本，产出 diff 文件与布尔判定：
  #  a) 消息 ID 集合:
  #     恢复前后分别: SELECT conversation_id, id FROM msg_store ORDER BY 1,2
  #     → 导出 → sort | shasum -a 256 比对；集合差必须为空（不允许只比 count）
  #  b) 附件哈希:
  #     SELECT id, object_key, md5 FROM attachment 全量行 ×
  #     恢复桶逐 object_key 取字节算 md5（rclone cat | md5）；
  #     不匹配数=0；object_key 集合 DB↔桶双向 diff 为空
  #  c) 迁移水位:
  #     恢复后 schema_migrations 版本集合 == 备份时 == 仓内 priv/migrations head
  echo "TODO(Q12): step4 oracle 三集合核验（消息 ID 集合/附件 md5/迁移水位，设计 §3.3 L158-162）"
  return 1
}

# ---- step5 快速线（真实资产入口 + TODO 落参，设计 L162 / 资产表 L137）-------
step5_quick_line() {
  # TODO(Q12): restore_pg 自带行数抽样 + restore_smoke.sh MIN_TABLES 快速线
  # bash scripts/restore_smoke.sh
  echo "TODO(Q12): step5 快速线 restore_smoke.sh MIN_TABLES（设计 §3.3 L162）"
  return 1
}

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd ${IMBOY_ROOT}（演练对象: DB=${DRILL_TARGET_DB}→${DRILL_RESTORE_DB} bucket=${DRILL_BUCKET} container=${PG_CONTAINER}）"
  echo "[DRY-RUN] step1 造数据          : TODO（设计 L155：复用 golden_upgrade 数据生成段 API）"
  echo "[DRY-RUN] step2 备份            : PG_CONTAINER=$PG_CONTAINER POSTGRES_DB=$DRILL_TARGET_DB BACKUP_DIR=$DRILL_BACKUP_DIR bash scripts/backup_pg.sh --full"
  echo "[DRY-RUN]                          + GARAGE_REMOTE=<tbd> BUCKET=$DRILL_BUCKET bash scripts/backup_garage.sh + 产物 sha256"
  echo "[DRY-RUN] step3 破坏+恢复       : bash scripts/restore_pg.sh <dump> --target $DRILL_RESTORE_DB + rclone copy 回新 bucket"
  echo "[DRY-RUN] step4 oracle 三集合核验: TODO 对比脚本（设计 L158-162：消息 ID 集合 sha256 / 附件 md5 双向 diff / 迁移水位）"
  echo "[DRY-RUN] step5 快速线          : bash scripts/restore_smoke.sh（MIN_TABLES）"
  exit 0
fi

echo "==> G19 / F3 演练·备份恢复 oracle start=$(date -u +%Y-%m-%dT%H:%M:%SZ)"
echo "    演练对象: 目标库=$DRILL_TARGET_DB 恢复库=$DRILL_RESTORE_DB bucket=$DRILL_BUCKET"
step1_seed_data
step2_backup
step3_drop_and_restore
step4_oracle
step5_quick_line
echo "==> G19 / F3 演练·备份恢复 oracle PASS rc=0（diff 文件与布尔判定留档入 manifest.drills.restore）"
