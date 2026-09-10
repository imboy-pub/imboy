#!/usr/bin/env bash
# 迁移文件门禁 — ADR-0002: priv/migrations/ 按顺序版本号、成对提供 up/down
#   命名规范：NNNNNNNN_<name>.up.sql  /  NNNNNNNN_<name>.down.sql
#
# 校验项：
#   1) 命名格式符合 NNNNNNNN_<小写 snake_case>.(up|down).sql
#   2) up / down 成对存在（缺失会让 erlang_migrate:apply_down/6 返回
#      {error,{no_down_migration,V}}，卡死该版本之后的整条回滚链）
#   3) 版本号唯一（重复会让 erlang_migrate_source:scan/1 直接 duplicate_versions）
#   4) up 文件非空
#   5) 文件内注释头自报的版本号与文件名一致
#   6) 编号连续性（仅告警：erlang_migrate 只要求递增，不要求连续）
#
# 注意：本脚本需兼容 macOS 自带 bash 3.2——该版本的解析器无法处理
# "命令替换 + 管道 + case" 的组合，故全程避免在 $( ) 内使用 case。
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT_DIR"

MIG_DIR="priv/migrations"
NAME_PATTERN='^[0-9]{8}_[a-z0-9_]+\.(up|down)\.sql$'
CITE_PATTERN='^--[[:space:]]*(合并迁移回滚|合并迁移|迁移)[[:space:]]+0*[0-9]+'
# 历史遗留断档：2026-06-08 基线压缩(70→9) + 2026-06-11 重编号(2953c7d2) 所致。
# 新增断档会在此告警——若确属有意为之，把编号加进白名单并注明原因。
LEGACY_GAPS="41"

errors=0
warns=0

fail() { echo "  ❌ $*" >&2; errors=$((errors + 1)); }
note() { echo "  ⚠️  $*"; warns=$((warns + 1)); }

if [ ! -d "$MIG_DIR" ]; then
  echo "error: $MIG_DIR 不存在（请在仓库根目录运行）" >&2
  exit 1
fi

tmp_all="$(mktemp)"
tmp_stems="$(mktemp)"
trap 'rm -f "$tmp_all" "$tmp_stems"' EXIT

find "$MIG_DIR" -maxdepth 1 -type f ! -name '.*' | sort > "$tmp_all"

if [ ! -s "$tmp_all" ]; then
  echo "error: $MIG_DIR 下没有文件" >&2
  exit 1
fi

# ---- 1) 命名格式 ----------------------------------------------------------
while IFS= read -r path; do
  base="${path##*/}"
  if [ "${base##*.}" != "sql" ]; then
    fail "非 .sql 文件混入迁移目录: $base"
    continue
  fi
  if ! [[ "$base" =~ $NAME_PATTERN ]]; then
    fail "命名不符合 NNNNNNNN_<name>.(up|down).sql: $base"
  fi
done < "$tmp_all"

# ---- 2) 收集 stem（去掉 .up.sql / .down.sql）------------------------------
while IFS= read -r path; do
  base="${path##*/}"
  if [ "$base" != "${base%.up.sql}" ]; then
    printf '%s\n' "${path%.up.sql}" >> "$tmp_stems"
  elif [ "$base" != "${base%.down.sql}" ]; then
    printf '%s\n' "${path%.down.sql}" >> "$tmp_stems"
  fi
done < "$tmp_all"
sort -u "$tmp_stems" -o "$tmp_stems"

# ---- 3) up / down 成对 ----------------------------------------------------
pairs=0
while IFS= read -r stem; do
  pairs=$((pairs + 1))
  [ -f "${stem}.up.sql" ] || fail "缺 up 文件: ${stem##*/}.up.sql"
  if [ ! -f "${stem}.down.sql" ]; then
    fail "缺 down 文件: ${stem##*/}.down.sql（ADR-0002 要求成对；缺失会阻断该版本之后的回滚）"
  fi
done < "$tmp_stems"

# ---- 4) 版本号唯一 --------------------------------------------------------
dup_versions=$(awk -F/ '{
    name = $NF
    ver = name
    sub(/_.*/, "", ver)
    count[ver]++
    names[ver] = names[ver] " " name
  }
  END {
    for (ver in count) {
      if (count[ver] > 1) printf "%s ->%s\n", ver, names[ver]
    }
  }' "$tmp_stems" | sort)

if [ -n "$dup_versions" ]; then
  while IFS= read -r line; do
    [ -n "$line" ] && fail "版本号重复（erlang_migrate 会拒绝启动）: $line"
  done <<< "$dup_versions"
fi

# ---- 5) up 非空 + 注释头版本号自洽 ---------------------------------------
while IFS= read -r stem; do
  up="${stem}.up.sql"
  [ -f "$up" ] || continue

  base="${up##*/}"
  ver_num=$((10#${base%%_*}))

  if [ ! -s "$up" ]; then
    fail "up 文件为空: $base"
    continue
  fi

  if grep -qE "$CITE_PATTERN" "$up"; then
    cited=$(grep -E "$CITE_PATTERN" "$up" |
      head -1 |
      sed -E 's/^[^0-9]*0*([0-9]+).*/\1/')
    if [ "$((10#$cited))" != "$ver_num" ]; then
      fail "注释头自报版本 $((10#$cited)) 与文件名版本 $ver_num 不一致: $base"
    fi
  fi
done < "$tmp_stems"

# ---- 6) 编号连续性（仅告警）----------------------------------------------
gap_versions=$(awk -F/ '{
    ver = $NF
    sub(/_.*/, "", ver)
    print ver
  }' "$tmp_stems" | sort -u)

gaps=$(printf '%s\n' "$gap_versions" | awk -v legacy="$LEGACY_GAPS" '
  NF == 0 { next }
  { v = $1 + 0 }
  NR == 1 { prev = v; next }
  {
    if (v != prev + 1) {
      for (g = prev + 1; g < v; g++) {
        if (index(" " legacy " ", " " g " ") == 0) printf "%d ", g
      }
    }
    prev = v
  }
  END { printf "\n" }
')

if [ -n "$(printf '%s' "$gaps" | tr -d '[:space:]')" ]; then
  note "编号断档（不在历史白名单内）: 缺编号${gaps}"
fi

# ---- 汇总 -----------------------------------------------------------------
total=$(grep -c . "$tmp_all" || true)
echo "迁移文件门禁: ${total} 个文件 / ${pairs} 组 up-down"

if [ "$warns" -gt 0 ]; then
  echo "  ⚠️  ${warns} 条告警（不阻断）"
fi

if [ "$errors" -gt 0 ]; then
  echo "❌ 迁移文件门禁未通过：${errors} 处违规" >&2
  exit 1
fi

echo "✅ 迁移文件门禁通过"
