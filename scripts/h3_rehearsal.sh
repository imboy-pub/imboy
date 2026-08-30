#!/usr/bin/env bash
# ============================================================
# H3 迁移/回滚演练编排（单命令）/ H3 drill orchestrator
# ------------------------------------------------------------
# 前提：scripts/sanitized_snapshot.sh 已产出 <SNAPSHOT_DIR>/sanitized.dump
#       （手册 §五 流水线第 1 步）；本脚本执行其后四步：
#   1) 恢复演练库（DROP+CREATE+pg_restore）
#   2) 迁移基线确认（version）→ 回滚一步（down，如 81→80，计时）
#   3) 重新应用（up，计时）→ 版本回到基线
#   4) 行数对账（对 snapshot 的 manifest.txt 逐表核对）+ 假号段抽检
# 应用冒烟（起节点+healthz/登录）为手册 §五 的独立一步，见
# w2-zc12-manual-execution-handbook.md §五（需 IMBOYENV 与配置切换，人工执行）。
#
# 用法 / Usage:
#   PGHOST=... PGPORT=... PGUSER=... PGPASSWORD=... PGDATABASE=<prod 库名> \
#     bash scripts/h3_rehearsal.sh <SNAPSHOT_DIR> [DRILL_DB]
#   DRILL_DB 默认 <PGDATABASE>_drill；演练库存在即先删除重建。
# ============================================================
set -euo pipefail

SNAP_DIR="${1:?用法: h3_rehearsal.sh <SNAPSHOT_DIR> [DRILL_DB]}"
DRILL_DB="${2:-${PGDATABASE:-imboy_v1}_drill}"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
export PGPASSWORD="${PGPASSWORD:-}"
MANIFEST="$SNAP_DIR/manifest.txt"
[[ -f "$MANIFEST" ]] || { echo "缺少 $MANIFEST（先跑 sanitized_snapshot.sh）"; exit 2; }
[[ -f "$SNAP_DIR/sanitized.dump" ]] || { echo "缺少 $SNAP_DIR/sanitized.dump"; exit 2; }

RED='\033[0;31m'; GREEN='\033[0;32m'; YEL='\033[1;33m'; NC='\033[0m'
info(){ echo -e "${GREEN}[h3_rehearsal]${NC} $*"; }
fail(){ echo -e "${RED}[h3_rehearsal] ERROR:${NC} $*" >&2; exit 1; }
REPO_DIR="$(cd "$(dirname "$0")/.." && pwd)"

psql_db(){ psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$DRILL_DB" -v ON_ERROR_STOP=1 "$@"; }
T0=$(date +%s)

info "1) 恢复演练库 $DRILL_DB"
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres -q \
     -c "DROP DATABASE IF EXISTS \"$DRILL_DB\"" -c "CREATE DATABASE \"$DRILL_DB\""
if ! pg_restore -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$DRILL_DB" --no-owner \
        "$SNAP_DIR/sanitized.dump" 2> >(grep -E "ERROR" >&2); then
    fail "pg_restore 失败"
fi

info "2) 迁移基线与回滚一步"
V0=$(PGDATABASE="$DRILL_DB" "$REPO_DIR/scripts/drill_migrate.escript" version | tail -1)
info "基线版本: $V0"
T1=$(date +%s)
PGDATABASE="$DRILL_DB" "$REPO_DIR/scripts/drill_migrate.escript" down
T2=$(date +%s)
info "down 完成（$((T2 - T1))s），版本: $(PGDATABASE="$DRILL_DB" "$REPO_DIR/scripts/drill_migrate.escript" version | tail -1)"

info "3) 重新应用 up"
PGDATABASE="$DRILL_DB" "$REPO_DIR/scripts/drill_migrate.escript" up
T3=$(date +%s)
info "up 完成（$((T3 - T2))s），版本: $(PGDATABASE="$DRILL_DB" "$REPO_DIR/scripts/drill_migrate.escript" version | tail -1)"

info "4) 行数对账（对 snapshot manifest）"
DIFFS=0
while IFS=$'\t' read -r t s _d; do
    # 只认「表名\t数字\t数字」数据行；跳过头部说明与尾部 sha256=/size=/scan= 摘要行
    [[ "$t" =~ ^[a-z0-9_]+$ && "$s" =~ ^[0-9]+$ ]] || continue
    c=$(psql_db -tAc "SELECT count(*) FROM \"$t\"" 2>/dev/null || echo "NA")
    if [[ "$c" != "$s" ]]; then
        warn_org="表 $t: manifest=$s 实际=$c"
        echo -e "${YEL}[h3_rehearsal] WARN:${NC} $warn_org"
        DIFFS=$((DIFFS + 1))
    fi
done < "$MANIFEST"
[[ $DIFFS -eq 0 ]] || fail "行数对账差异 $DIFFS 处"
info "行数对账通过 ✓"

# 假号段抽检（user.mobile 全落 '12' 前缀；无 user 表的部署自动跳过）
HAS_USER=$(psql_db -tAc "SELECT 1 FROM information_schema.tables WHERE table_schema='public' AND table_name='user'" || true)
if [[ -n "$HAS_USER" ]]; then
    BAD=$(psql_db -tAc "SELECT count(*) FROM \"user\" WHERE mobile IS NOT NULL AND mobile !~ '^12'" || echo "?")
    [[ "$BAD" == "0" ]] || fail "假号段抽检失败：$BAD 条非 12 前缀手机号"
    info "假号段抽检通过 ✓"
fi

DUR=$(( $(date +%s) - T0 ))
info "演练完成：down/up 与对账全通过（总 ${DUR}s）。"
info "后续：应用冒烟（手册 §五）→ 人工确认 → 清理演练库："
echo "  psql -h $PGHOST -p $PGPORT -U $PGUSER -d postgres -c 'DROP DATABASE IF EXISTS \"$DRILL_DB\"'"
