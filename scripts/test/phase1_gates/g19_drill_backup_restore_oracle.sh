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
# 实现: 五步已落可执行命令（2026-10-07 主线补实现，R4 清偿）。列名与 API
#   已对真库/src 核实：msg_store 无 conversation_id → (chat_type,conv_key,
#   msg_id)；attachment 无 md5 → (path,file_hash256) 且哈希为 sha256；
#   附件 API 按 attach_handler.erl:7/103-107（presign→PUT→confirm）。
#   剩余执行期绑定：GARAGE_REMOTE（rclone remote 名）+ 演练栈（DRILL_BASE_URL/
#   DRILL_WS_URL，默认 9831，由总控起栈）+ G19_GID（C2G 需演练库已有群）。
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
PG_USER="${PG_USER:-imboy_user}"
# 演练栈（step1 造数据指向的隔离后端；由总控起栈，见 g18/g20）
DRILL_BASE_URL="${DRILL_BASE_URL:-http://127.0.0.1:9831}"
DRILL_WS_URL="${DRILL_WS_URL:-ws://127.0.0.1:9831/api/v1/ws}"
# 造数据规模（设计 L155 N/M 条；默认小规模——oracle 是集合一致性而非规模）
G19_C2C_N="${G19_C2C_N:-3}"
G19_C2G_M="${G19_C2G_M:-3}"
G19_ATT_N="${G19_ATT_N:-1}"
# Garage 备份 remote（rclone 配置名；Q12 执行时绑定真实演练 remote）
GARAGE_REMOTE="${GARAGE_REMOTE:-}"
RCLONE_RESTORE_PREFIX="p1drill-restore"

cd "$IMBOY_ROOT"

psql_exec() { # <db> <sql> —— 演练容器内执行（对齐 backup_pg.sh:81-100 的 exec 形态）
  docker exec -i "$PG_CONTAINER" psql -U "$PG_USER" -d "$1" -tAc "$2"
}

# ---- step1 造数据（设计 L155；API 从 src/ 逐一核实，禁止臆造）------------
step1_seed_data() {
  command -v jq >/dev/null || { echo "ERROR: 需要 jq" >&2; return 1; }
  local stamp base_dir state
  stamp="$(date +%s)"
  base_dir="$DRILL_BACKUP_DIR"; mkdir -p "$base_dir"
  state="$base_dir/state.json"
  # 账号 A/B（golden_upgrade gu_register_login 同款 HTTP 契约：
  # signup type=email + code=abc12345 + rsa_encrypt=0 → login 取 token/uid）
  local creds=() label email pwd resp token uid
  for label in A B; do
    email="g19-${label}-${stamp}@smoke.local"
    pwd="G19drill-$(openssl rand -hex 4)"
    resp="$(curl -sS -X POST "$DRILL_BASE_URL/api/v1/passport/signup" \
      -H 'Content-Type: application/x-www-form-urlencoded' \
      --data-urlencode 'type=email' --data-urlencode "account=$email" \
      --data-urlencode "pwd=$pwd" --data-urlencode 'code=abc12345' \
      --data-urlencode 'rsa_encrypt=0' --data-urlencode "nickname=g19-$label")"
    [ "$(printf '%s' "$resp" | jq -r '.code')" = "0" ] \
      || { echo "ERROR: signup $label: $(printf '%s' "$resp" | head -c 200)" >&2; return 1; }
    resp="$(curl -sS -X POST "$DRILL_BASE_URL/api/v1/passport/login" \
      -H 'Content-Type: application/x-www-form-urlencoded' \
      --data-urlencode 'type=email' --data-urlencode "account=$email" \
      --data-urlencode "pwd=$pwd" --data-urlencode 'rsa_encrypt=0')"
    token="$(printf '%s' "$resp" | jq -r '.payload.token // empty')"
    uid="$(printf '%s' "$resp" | jq -r '.payload.uid // empty')"
    { [ -n "$token" ] && [ -n "$uid" ]; } \
      || { echo "ERROR: login $label: $(printf '%s' "$resp" | head -c 200)" >&2; return 1; }
    eval "G19_${label}_TOKEN=\$token; G19_${label}_UID=\$uid"
    creds+=("$(jq -nc --arg l "$label" --arg e "$email" --arg p "$pwd" \
      --arg u "$uid" '{label:$l,email:$e,pwd:$p,uid:$u}')")
  done
  jq -nc --argjson arr "$(printf '%s\n' "${creds[@]}" | jq -sc '.')" \
    '{users: $arr}' > "$state"
  # C2C×N（imboy_ctl msg send RPC——golden_upgrade gu_send_c2c 同款）
  local i mid c2c_ids=""
  for i in $(seq 1 "$G19_C2C_N"); do
    mid="$("$IMBOY_ROOT/scripts/imboy_ctl" msg send "$G19_A_UID" "$G19_B_UID" \
      -T "g19-c2c-$i" 2>&1 | awk -F= '/^MSG_ID=/ {print $2}' | tr -d '[:space:]')"
    [ -n "$mid" ] || { echo "ERROR: C2C #$i 发送失败（栈未起/E2EE 强制档）" >&2; return 1; }
    c2c_ids="$c2c_ids $mid"
  done
  # 附件 round-trip ×N（attach_handler.erl:7/103-107 契约：
  # presign{filename,mime_type} → PUT put_url → confirm{object_key,file_hash256,mime_type,size}）
  local att_keys="" pre put_url okey fhash fsize
  for i in $(seq 1 "$G19_ATT_N"); do
    f="$base_dir/g19-att-$i.bin"; head -c 65536 /dev/urandom > "$f"
    fhash="$(shasum -a 256 "$f" | awk '{print $1}')"; fsize="$(wc -c < "$f" | tr -d ' ')"
    pre="$(curl -sS "$DRILL_BASE_URL/api/v1/attachment/presign?filename=g19-$i.bin&mime_type=application/octet-stream" \
      -H "Authorization: Bearer $G19_A_TOKEN")"
    put_url="$(printf '%s' "$pre" | jq -r '.payload.put_url // empty')"
    okey="$(printf '%s' "$pre" | jq -r '.payload.object_key // empty')"
    [ -n "$put_url" ] && [ -n "$okey" ] \
      || { echo "ERROR: presign #$i: $(printf '%s' "$pre" | head -c 200)" >&2; return 1; }
    curl -sS -X PUT --data-binary "@$f" -H 'Content-Type: application/octet-stream' "$put_url" \
      || { echo "ERROR: PUT #$i 直传失败（garage 不可达则检查 DRILL 栈）" >&2; return 1; }
    resp="$(curl -sS -X POST "$DRILL_BASE_URL/api/v1/attachment/confirm" \
      -H "Authorization: Bearer $G19_A_TOKEN" \
      --data-urlencode "object_key=$okey" --data-urlencode "file_hash256=$fhash" \
      --data-urlencode "mime_type=application/octet-stream" --data-urlencode "size=$fsize")"
    [ "$(printf '%s' "$resp" | jq -r '.code')" = "0" ] \
      || { echo "ERROR: confirm #$i: $(printf '%s' "$resp" | head -c 200)" >&2; return 1; }
    att_keys="$att_keys $okey"
  done
  # C2G×M（ws_c2g_send.py WS 帧——golden_upgrade 同工具）
  for i in $(seq 1 "$G19_C2G_M"); do
    python3 "$IMBOY_ROOT/scripts/smoke/ws_c2g_send.py" \
      --url "$DRILL_WS_URL" --token "$G19_A_TOKEN" --from "$G19_A_UID" \
      --group "${G19_GID:-}" --text "g19-c2g-$i" \
      || { echo "ERROR: C2G #$i（G19_GID 需指向演练库已有群）" >&2; return 1; }
  done
  jq --arg c2c "$c2c_ids" --arg att "$att_keys" \
    '. + {c2c_msg_ids: ($c2c|split(" ")), att_object_keys: ($att|split(" "))}' \
    "$state" > "$state.tmp" && mv "$state.tmp" "$state"
  echo "[g19] step1 造数据完成：users=2 c2c=${G19_C2C_N} att=${G19_ATT_N} c2g=${G19_C2G_M} → $state"
}

# Preserve relative/../ backup paths, but reject symlink ancestors before any backup.
g19_normalize_backup_dir() {
  local normalized
  normalized="$(python3 - "$DRILL_BACKUP_DIR" <<'PYTHON'
import os, pathlib, sys
path = pathlib.Path(sys.argv[1])
if not path.is_absolute():
    path = pathlib.Path.cwd() / path
# Check the supplied traversal before collapsing '..' (a/../ may cross a link).
prefix = pathlib.Path(path.anchor)
for part in path.parts[1:]:
    prefix = prefix / part
    if prefix.is_symlink():
        raise SystemExit('Garage backup path symlink ancestor refused')
normalized = pathlib.Path(os.path.abspath(str(path)))
for item in [normalized, *normalized.parents]:
    if item.is_symlink():
        raise SystemExit('Garage backup path symlink ancestor refused')
print(normalized)
PYTHON
)" || return 1
  DRILL_BACKUP_DIR="$normalized"
}

# backup_garage.sh writes exactly one timestamped bucket snapshot below DEST.
# Pin that directory, not its parent, so restoration preserves every object key.
g19_snapshot_dir() (
  shopt -s nullglob dotglob
  local parent="$DRILL_BACKUP_DIR/garage" path actual
  local -a entries
  [[ -d "$parent" && ! -L "$parent" ]] || return 1
  actual="$(cd "$parent" && pwd -P)" || return 1
  [[ "$actual" == "$parent" ]] || return 1
  entries=("$parent"/*)
  [[ "${#entries[@]}" -eq 1 ]] || return 1
  path="${entries[0]}"
  [[ -d "$path" && ! -L "$path" && "${path##*/}" == "$DRILL_BUCKET"_* ]] || return 1
  [[ "$path" != *$'\n'* && "$(cd "$path" && pwd -P)" == "$path" ]] || return 1
  printf '%s\n' "$path"
)

restore_garage_snapshot() {
  local snapshot current
  IFS= read -r snapshot < "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path" || return 1
  current="$(g19_snapshot_dir)" || { echo "ERROR: Garage snapshot layout invalid" >&2; return 1; }
  [[ "$snapshot" == "$current" ]] || { echo "ERROR: Garage snapshot changed" >&2; return 1; }
  rclone copy "$snapshot" "${GARAGE_REMOTE}:${RCLONE_RESTORE_PREFIX}" \
    || { echo "ERROR: Garage snapshot restore failed" >&2; return 1; }
}

# ---- step2 备份（资产表 L135/L138；并导出备份时 oracle 基线集合）----------
step2_backup() {
  [ -n "$GARAGE_REMOTE" ] || { echo "ERROR: GARAGE_REMOTE 未绑定（Q12 执行时注入 rclone remote 名，R4）" >&2; return 1; }
  local dump_out snapshot
  g19_normalize_backup_dir || return 1
  [[ ! -e "$DRILL_BACKUP_DIR/garage" && ! -L "$DRILL_BACKUP_DIR/garage" ]] \
    || { echo "ERROR: Garage backup parent must be fresh" >&2; return 1; }
  PG_CONTAINER="$PG_CONTAINER" POSTGRES_USER="$PG_USER" POSTGRES_DB="$DRILL_TARGET_DB" \
    BACKUP_DIR="$DRILL_BACKUP_DIR" bash "$IMBOY_ROOT/scripts/backup_pg.sh" --full \
    || return 1
  dump_out="$(ls -t "$DRILL_BACKUP_DIR"/${DRILL_TARGET_DB}_*.dump | head -1)"
  GARAGE_REMOTE="$GARAGE_REMOTE" BUCKET="$DRILL_BUCKET" \
    DEST="$DRILL_BACKUP_DIR/garage" bash "$IMBOY_ROOT/scripts/backup_garage.sh" \
    || return 1
  snapshot="$(g19_snapshot_dir)" || { echo "ERROR: Garage snapshot layout invalid" >&2; return 1; }
  mkdir -p "$DRILL_BACKUP_DIR/oracle_before"
  [[ ! -e "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path" ]] || return 1
  printf '%s\n' "$snapshot" > "$DRILL_BACKUP_DIR/oracle_before/garage_snapshot.path"
  shasum -a 256 "$dump_out" > "$DRILL_BACKUP_DIR/oracle_before/dump.sha256"
  find "$snapshot" -type f -exec shasum -a 256 {} + \
    > "$DRILL_BACKUP_DIR/oracle_before/garage_files.sha256"
  # 备份时基线集合（step4 与恢复后比对；真实列名已对库核实：
  # msg_store 无 conversation_id → 用 (chat_type, conv_key, msg_id)；
  # attachment 无 md5 → 用 (path, file_hash256)，哈希为 sha256）
  psql_exec "$DRILL_TARGET_DB" \
    "SELECT chat_type||'|'||conv_key||'|'||msg_id FROM msg_store ORDER BY 1" \
    > "$DRILL_BACKUP_DIR/oracle_before/msg_set.txt"
  psql_exec "$DRILL_TARGET_DB" \
    "SELECT path||'|'||file_hash256 FROM attachment ORDER BY 1" \
    > "$DRILL_BACKUP_DIR/oracle_before/att_set.txt"
  psql_exec "$DRILL_TARGET_DB" "SELECT version FROM schema_migrations ORDER BY 1" \
    > "$DRILL_BACKUP_DIR/oracle_before/migrations.txt"
  echo "[g19] step2 备份完成：dump=$(basename "$dump_out") + garage → ${DRILL_BACKUP_DIR}（oracle 基线三集合已导出）"
}

# ---- step3 破坏+恢复（资产表 L136；防御性 DROP 守卫）----------------------
step3_drop_and_restore() {
  # 防御：只允许 DROP 演练前缀库，绝不触生产/共享库（设计 L157 红线）
  case "$DRILL_TARGET_DB" in
    p1drill*) ;;
    *) echo "ERROR: 拒绝 DROP 非演练库名 '$DRILL_TARGET_DB'（守卫红线）" >&2; return 1 ;;
  esac
  local dump_out; dump_out="$(ls -t "$DRILL_BACKUP_DIR"/${DRILL_TARGET_DB}_*.dump | head -1)"
  psql_exec postgres "SELECT pg_terminate_backend(pid) FROM pg_stat_activity \
    WHERE datname='$DRILL_TARGET_DB' AND pid<>pg_backend_pid()" >/dev/null || true
  psql_exec postgres "DROP DATABASE IF EXISTS $DRILL_TARGET_DB" >/dev/null
  bash "$IMBOY_ROOT/scripts/restore_pg.sh" "$dump_out" --target "$DRILL_RESTORE_DB" \
    || return 1
  # Garage 回桶（新 bucket，不动原桶——保留对照面）
  restore_garage_snapshot || return 1
  echo "[g19] step3 恢复完成：DB→$DRILL_RESTORE_DB 桶→${GARAGE_REMOTE}:${RCLONE_RESTORE_PREFIX}"
}

# ---- step4 oracle 三集合核验（设计 L158-162；diff 落档 + 布尔判定）---------
step4_oracle() {
  local od="$DRILL_BACKUP_DIR/oracle_after" rc=0
  mkdir -p "$od"
  psql_exec "$DRILL_RESTORE_DB" \
    "SELECT chat_type||'|'||conv_key||'|'||msg_id FROM msg_store ORDER BY 1" \
    > "$od/msg_set.txt"
  psql_exec "$DRILL_RESTORE_DB" \
    "SELECT path||'|'||file_hash256 FROM attachment ORDER BY 1" \
    > "$od/att_set.txt"
  psql_exec "$DRILL_RESTORE_DB" "SELECT version FROM schema_migrations ORDER BY 1" \
    > "$od/migrations.txt"
  # a) 消息 ID 集合：双向 diff 为空（设计 L159：不允许只比 count）
  if ! diff -u "$DRILL_BACKUP_DIR/oracle_before/msg_set.txt" "$od/msg_set.txt" \
      > "$od/msg_set.diff"; then
    echo "FAIL: a) 消息 ID 集合不一致（见 $od/msg_set.diff）"; rc=1
  else
    echo "[g19] a) 消息 ID 集合一致：$(wc -l < "$od/msg_set.txt" | tr -d ' ') 行 sha=$(shasum -a 256 "$od/msg_set.txt" | awk '{print substr($1,1,12)}')"
  fi
  # b) 附件哈希：DB(file_hash256) × 恢复桶对象字节 sha256 逐一比对 + 双向集合 diff
  local mismatch=0 key dbhash objhash
  while IFS='|' read -r key dbhash; do
    [ -n "$key" ] || continue
    objhash="$(rclone cat "${GARAGE_REMOTE}:${RCLONE_RESTORE_PREFIX}/${key}" 2>/dev/null | shasum -a 256 | awk '{print $1}')"
    if [ "$objhash" != "$dbhash" ]; then
      echo "  [att-mismatch] $key db=$dbhash obj=${objhash:-MISSING}"; mismatch=$((mismatch+1))
    fi
  done < "$od/att_set.txt"
  rclone lsf "${GARAGE_REMOTE}:${RCLONE_RESTORE_PREFIX}" -R --files-only 2>/dev/null | sort > "$od/bucket_keys.txt"
  cut -d'|' -f1 "$od/att_set.txt" | sort > "$od/db_keys.txt"
  if ! diff -u "$od/db_keys.txt" "$od/bucket_keys.txt" > "$od/att_keys.diff"; then
    echo "FAIL: b) object_key 集合 DB↔桶双向 diff 非空（见 $od/att_keys.diff）"; rc=1
  elif [ "$mismatch" -ne 0 ]; then
    echo "FAIL: b) 附件哈希不匹配数=$mismatch"; rc=1
  else
    echo "[g19] b) 附件哈希全部一致（$(wc -l < "$od/att_set.txt" | tr -d ' ') 行）+ 集合双向 diff 为空"
  fi
  # c) 迁移水位：恢复后 == 备份时 == 仓内 priv/migrations head
  local repo_head; repo_head="$(ls "$IMBOY_ROOT/priv/migrations/"*.up.sql \
    | sed -E 's/.*\/0*([0-9]+)_.*/\1/' | sort -n | tail -1)"
  if ! diff -u "$DRILL_BACKUP_DIR/oracle_before/migrations.txt" "$od/migrations.txt" \
      > "$od/migrations.diff"; then
    echo "FAIL: c) 迁移水位 备份≠恢复（见 $od/migrations.diff）"; rc=1
  elif [ "$(tail -1 "$od/migrations.txt")" != "$repo_head" ]; then
    echo "FAIL: c) 迁移水位恢复后 head=$(tail -1 "$od/migrations.txt") ≠ 仓内 head=$repo_head"; rc=1
  else
    echo "[g19] c) 迁移水位一致：head=${repo_head}（备份==恢复==仓内）"
  fi
  [ "$rc" -eq 0 ] && echo "ORACLE-PASS" > "$od/verdict.txt" \
    || echo "ORACLE-FAIL" > "$od/verdict.txt"
  return "$rc"
}

# ---- step5 快速线（资产表 L137）------------------------------------------
step5_quick_line() {
  bash "$IMBOY_ROOT/scripts/restore_smoke.sh" \
    || { echo "ERROR: restore_smoke.sh MIN_TABLES 快速线失败" >&2; return 1; }
  echo "[g19] step5 快速线通过"
}

# Sourcing exposes functions only; executing this file preserves the normal main.
if [[ "${BASH_SOURCE[0]}" != "$0" ]]; then return 0; fi

if [ "$DRY_RUN" -eq 1 ]; then
  echo "[DRY-RUN] cd ${IMBOY_ROOT}（演练对象: DB=${DRILL_TARGET_DB}→${DRILL_RESTORE_DB} bucket=${DRILL_BUCKET} container=${PG_CONTAINER}）"
  echo "[DRY-RUN] 演练栈: BASE_URL=$DRILL_BASE_URL WS=$DRILL_WS_URL garage_remote=${GARAGE_REMOTE:-<未绑定,Q12 注入>}"
  echo "[DRY-RUN] step1 造数据          : signup/login A/B + C2C×${G19_C2C_N} + 附件×${G19_ATT_N} + C2G×${G19_C2G_M}（G19_GID=${G19_GID:-<需演练库已有群>}）"
  echo "[DRY-RUN] step2 备份            : backup_pg.sh --full + backup_garage.sh + 产物 sha256 + oracle 基线三集合导出"
  echo "[DRY-RUN] step3 破坏+恢复       : DROP（守卫 p1drill* 前缀）+ restore_pg.sh --target + rclone copy 回 ${RCLONE_RESTORE_PREFIX}"
  echo "[DRY-RUN] step4 oracle 三集合核验: 消息集合双向 diff / 附件 sha256×桶字节+集合 diff / 迁移水位三相等 → verdict.txt"
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
