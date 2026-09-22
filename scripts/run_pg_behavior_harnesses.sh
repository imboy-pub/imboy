#!/usr/bin/env bash
# =============================================================================
# 真 PostgreSQL 行为 harness 门（GZAPP：默认工作区鉴权 / 归档强交接 / 建企模板 /
# 成员有权 Workspace / 频道与群的双授权源，以及建企-加入-生命周期全链）。
#
# 为什么单独成门：这两支 harness 是本 run 唯一"真库行为矩阵"证据面，但长期只
# 以「手工可复跑」形式存在（未进门禁）。本脚本把「跑它们」变成一条命令，CI 与
# 本地共用同一实现，避免两边各写一份。
#
# 前置（脚本会自检，不满足即失败）：
#   * 目标库已应用本仓迁移链到 schema_migrations 当前 head；
#   * 该库所在集群已铺齐迁移所需的 Postgres 扩展
#     （本仓迁移不含 CREATE EXTENSION，全新集群必须预铺；
#      扩展清单与自建镜像见 docker/pg18_Dockerfile + docker/pg-initdb-imboy.sh）。
#
# 用法（本地）：
#   PGHOST=127.0.0.1 PGPORT=4323 PGUSER=imboy_user PGPASSWORD=*** \
#     PGDATABASE=scratch_gzapp_fix_verify scripts/run_pg_behavior_harnesses.sh
# 用法（CI）：由 .github/workflows/nightly.yml 的 pg-behavior-harnesses job 调用，
#   该 job 先用 docker/pg18_Dockerfile 构建镜像再起 service 容器。
# =============================================================================
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

: "${PGHOST:=127.0.0.1}"
: "${PGPORT:=5432}"
: "${PGUSER:=imboy_user}"
: "${PGDATABASE:=imboy_drill}"
: "${PGPASSWORD:=}"

export PGHOST PGPORT PGUSER PGDATABASE PGPASSWORD
export IMBOY_DIR="$REPO_ROOT"

HARNESSES=(
  "test/lib/organization/gzapp_enterprise_behavior_harness.escript"
  "test/lib/organization/gzapp_fix_behavior_harness.escript"
  "test/lib/organization/group_org_authority_behavior_harness.escript"
)

echo "=== 前置自检：目标库迁移状态 ==="
HEAD="$(
  PGPASSWORD="$PGPASSWORD" psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" -tAc \
    "SELECT version FROM schema_migrations" 2>/dev/null || true
)"
if [ -z "$HEAD" ]; then
  echo "FAIL: 连不上 $PGUSER@$PGHOST:$PGPORT/$PGDATABASE 或该库未迁移（schema_migrations 无行）" >&2
  echo "      先跑：PGDATABASE=$PGDATABASE scripts/drill_migrate.escript up" >&2
  exit 1
fi
EXPECTED_HEAD="$(
  ls priv/migrations/*.up.sql | sed 's/.*\///; s/_.*//' | sort -n | tail -1 | sed 's/^0*//'
)"
if [ "$HEAD" != "$EXPECTED_HEAD" ]; then
  echo "FAIL: 目标库 head=$HEAD 与仓库迁移 head=$EXPECTED_HEAD 不一致" >&2
  echo "      测试库的 schema 必须来自本仓迁移，否则 harness 的结论不成立" >&2
  exit 1
fi
echo "OK: head=$HEAD"

FAILED=0
for h in "${HARNESSES[@]}"; do
  echo
  echo "=== $h ==="
  LOG="$(mktemp -t "$(basename "$h").XXXXXX.log")"
  if ! escript "$h" 2>&1 | tee "$LOG"; then
    echo "FAIL: $h 非零退出（明细见 $LOG）" >&2
    FAILED=1
    continue
  fi
  SUMMARY="$(grep -E '^PROBE-SUMMARY' "$LOG" | tail -1 || true)"
  if [ -z "$SUMMARY" ]; then
    echo "FAIL: $h 未产出 PROBE-SUMMARY（明细见 $LOG）" >&2
    FAILED=1
    continue
  fi
  # fail 计数只在部分 harness 出现；出现即必须为 0
  if echo "$SUMMARY" | grep -qE 'fail=[1-9]'; then
    echo "FAIL: $h $SUMMARY" >&2
    FAILED=1
    continue
  fi
  echo "OK: $h $SUMMARY"
done

echo
if [ "$FAILED" -ne 0 ]; then
  echo "真库行为 harness 门：FAIL" >&2
  exit 1
fi
echo "真库行为 harness 门：PASS（3 支 harness 全绿）"
