#!/usr/bin/env bash
# ============================================================
# 脱敏快照生成（H3 生产等价演练用）/ Sanitized snapshot for H3 drill
# ------------------------------------------------------------
# 目标（见 docs/planning/w2-zc12-manual-execution-handbook.md §五）：
#   产出「生产等价规模、PII 严禁外泄」的快照，供演练库做
#   迁移（→00000081）→ 冒烟 → 回滚 → 行数对账。
#
# 架构：源库 schema+全量数据灌入同实例临时库 → 原地 UPDATE 脱敏 →
#   残留 PII 扫描 → 行数对账 → pg_dump -Fc 导出。
#
# 脱敏策略（default-deny，列级分类）：
#   1) ID/数值/布尔/时间列原样保留（外键完整 + 规模真实性）；
#   2) jsonb/json 一律占位（半结构化内容不做语义判别）；
#   3) varchar/text 列数据驱动分类：取样 DISTINCT 全部匹配
#      ^[a-z0-9_\-\./+=:]{1,128}$（无空白/CJK）且基数 ≤ 阈值 → 系统
#      枚举/代码，保留；否则自由文本 → 可空置空、NOT NULL 用
#      'sd_'||md5(主键) 占位（保唯一约束）；
#   4) 列名强制规则（优先于数据分类）：
#      password→保留哈希（不可逆，演练登录）；mobile/phone→假号
#      （'12'+9位序号，主键有序确定；「12」开头非真实号段）；nickname→
#      drill用户N；email→drillN@drill.invalid；reg_ip/last_login_ip/ip、
#      birthday/avatar/logo/cover/sign→置空或占位；token/secret/api_key/
#      aes_key/webhook_url/verify_token/access_token/stream_key/sign_key→
#      占位；
#   5) 数据形态追加规则：account 类列若取样过半匹配 CN 手机号 → 假号。
# 安全网（扫描不过即整体失败；--raw 模式专用于实证扫描会爆红）：
#   user/adm_user 的 mobile/account 在假值段外命中 CN 手机号 → 失败；
#   源库抽样 500 真手机号在演练库 0 命中要求。
#
# 用法 / Usage:
#   PGPASSWORD=... PGHOST=... PGPORT=... PGUSER=... PGDATABASE=... \
#     bash scripts/sanitized_snapshot.sh -o /path/outdir [--raw]
# 产出:
#   OUT_DIR/sanitized.dump    pg_dump -Fc 全量（可外发）
#   OUT_DIR/manifest.txt      参数/时长/行数对账/校验和/扫描结论
#   OUT_DIR/mobile_map.csv    假手机号→真手机号（留在授权环境，严禁外发！）
#   OUT_DIR/class.tsv         列分类清单（审计用）
#
# 限制（首次对生产使用前必读）:
#   - 分类器基于列名+数据形态，非语义理解；误判列请反馈调整规则后重跑
#     （残留扫描是最后防线）；
#   - E2EE payload 为密文仍按自由文本占位（演练不需要消息内容）；
#   - 假手机号唯一性依赖 row_number（>999,999,999 用户才可能耗尽）；
#   - PostGIS/pg_stat_statements 等扩展内部表不搬数据。
# ============================================================
set -euo pipefail

PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
PGDATABASE="${PGDATABASE:-imboy_v1}"
export PGPASSWORD="${PGPASSWORD:-}"
OUT_DIR=""; RAW=0
FAKE_PREFIX="12"               # 假号段 '12' 开头——CN 真实号段第二位必为 [3-9]，永不冲突
SAMPLE_DISTINCT=64
SKIP_TABLES='_qa_tmp|spatial_ref_sys|geography_columns|geometry_columns|pg_stat_statements|app_ddl'

while [[ $# -gt 0 ]]; do case "$1" in
  -o) OUT_DIR="$2"; shift 2;;
  --raw) RAW=1; shift;;
  *) echo "未知参数 $1"; exit 2;;
esac; done
[[ -n "$OUT_DIR" ]] || { echo "用法: -o OUT_DIR [--raw]（PG* 环境变量定连接）"; exit 2; }
mkdir -p "$OUT_DIR"

RED='\033[0;31m'; GREEN='\033[0;32m'; YEL='\033[1;33m'; NC='\033[0m'
info(){ echo -e "${GREEN}[sanitized_snapshot]${NC} $*"; }
warn(){ echo -e "${YEL}[sanitized_snapshot]${NC} $*"; }
fail(){ echo -e "${RED}[sanitized_snapshot] ERROR:${NC} $*" >&2; exit 1; }
T0=$(date +%s); TS=$(date -u +%Y%m%dT%H%M%SZ)
TMP_DB="${PGDATABASE}_sd_tmp"

psql_src(){ psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" -v ON_ERROR_STOP=1 "$@"; }
psql_tmp(){ psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$TMP_DB" -v ON_ERROR_STOP=1 "$@"; }
cleanup(){ psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres \
            -tc "DROP DATABASE IF EXISTS \"$TMP_DB\"" >/dev/null 2>&1 || true; }
trap cleanup EXIT

info "源库 ${PGDATABASE} @ ${PGHOST}:${PGPORT}（raw=${RAW}）"
psql_src -tc "SELECT 1" >/dev/null || fail "源库不可达"

# ---------- 1) 列分类 → class.tsv: table|col|type|nullable|decision ----------
info "列分类（数据驱动取样，文本列较多时约需 1-2 分钟）…"
CLASS_TSV="$OUT_DIR/class.tsv"; : > "$CLASS_TSV"
while IFS='|' read -r t c ty nbl ml; do
  dec=""
  case "$ty" in
    'character varying'|text) : ;;
    jsonb|json|tsvector) dec="PLACEHOLDER";;   # tsvector=分词向量，含昵称/手机号切片，等同 PII
    *) dec="KEEP";;
  esac
  if [[ -z "$dec" ]]; then
    if [[ "$c" =~ password ]]; then dec="KEEP"
    elif [[ "$c" =~ (^|_)(mobile|phone)($|_) ]]; then dec="FAKE_MOBILE"
    elif [[ "$c" =~ (^|_)(nickname|nick_name)($|_) ]]; then dec="FAKE_NICK"
    elif [[ "$c" =~ (^|_)email($|_) ]]; then dec="FAKE_EMAIL"
    elif [[ "$c" =~ (^|_)(reg_ip|last_login_ip|login_ip|ip)($|_) ]]; then dec="NULLIFY"
    elif [[ "$c" =~ (^|_)(birthday|avatar|logo|cover|photo_url|thumbnail_url|sign)($|_) ]]; then dec="NULLIFY"
    elif [[ "$c" =~ (token|secret|api_key|aes_key|webhook_url|verify_token|access_token|stream_key|sign_key) ]]; then dec="PLACEHOLDER"
    else
      stats=$(psql_src -tAc "SELECT count(DISTINCT s.v)||'|'||coalesce(bool_and(s.v ~ '^[a-z0-9_\\-\\./+=:]{1,128}\$'), true)::text||'|'||coalesce(avg(CASE WHEN s.v ~ '1[3-9][0-9]{9}' THEN 1.0 ELSE 0 END), 0)::text
                              FROM (SELECT DISTINCT \"$c\" AS v FROM \"$t\" WHERE \"$c\" IS NOT NULL LIMIT ${SAMPLE_DISTINCT}) s" 2>/dev/null || echo "ERR")
      if [[ "$stats" == "ERR" ]]; then dec="PLACEHOLDER"
      else
        dn="$(cut -d'|' -f1 <<<"$stats")"; sys="$(cut -d'|' -f2 <<<"$stats")"; mob="$(cut -d'|' -f3 <<<"$stats")"
        if awk -v m="$mob" 'BEGIN{exit !(m+0 >= 0.5)}'; then dec="FAKE_MOBILE"     # 账号列实为手机号
        elif [[ "$sys" == "true" && "${dn:-0}" -le "$SAMPLE_DISTINCT" ]]; then dec="KEEP"
        elif [[ "$nbl" == "NO" ]]; then dec="PLACEHOLDER"
        else dec="NULLIFY"; fi
      fi
    fi
  fi
  [[ "$dec" == "NULLIFY" && "$nbl" == "NO" ]] && dec="PLACEHOLDER"   # NOT NULL 列不可置空 → 占位
  echo "$t|$c|$ty|$nbl|$dec|${ml:-}" >> "$CLASS_TSV"
done < <(psql_src -tA -F'|' -c "SELECT table_name, column_name, data_type, is_nullable, character_maximum_length
          FROM information_schema.columns
          WHERE table_schema='public'
            AND table_name IN (SELECT table_name FROM information_schema.tables
                                WHERE table_schema='public' AND table_type='BASE TABLE'
                                  AND table_name !~ '^(${SKIP_TABLES})$')
          ORDER BY table_name, ordinal_position")
info "分类完成 $(wc -l < "$CLASS_TSV" | tr -d ' ') 列：KEEP=$(awk -F'|' '$5=="KEEP"' "$CLASS_TSV" | wc -l | tr -d ' ') 假值=$(awk -F'|' '$5 ~ /^FAKE_/' "$CLASS_TSV" | wc -l | tr -d ' ') 占位=$(awk -F'|' '$5=="PLACEHOLDER"' "$CLASS_TSV" | wc -l | tr -d ' ') 置空=$(awk -F'|' '$5=="NULLIFY"' "$CLASS_TSV" | wc -l | tr -d ' ')"

# ---------- 2) 临时库：schema + 全量数据 ----------
psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d postgres -q \
     -c "DROP DATABASE IF EXISTS \"$TMP_DB\"" -c "CREATE DATABASE \"$TMP_DB\""
EXCLUDES=""; for t in ${SKIP_TABLES//|/ }; do EXCLUDES+=" --exclude-table=$t"; done
# timescaledb 超表：父表 COPY 是空壳、数据在内部 chunk（其表由扩展运行期管理，
# 恢复端不存在）→ 数据 dump 排除超表，改用 \copy 管道直灌（临时库中按普通表承载，
# 迁移/回滚演练不受影响；快照内超表即普通表，此差异记录于 manifest）
HTS=$(psql_src -tAc "SELECT hypertable_name FROM timescaledb_information.hypertables WHERE hypertable_schema='public'" 2>/dev/null || true)
HT_EXCL=""; for h in $HTS; do HT_EXCL+=" --exclude-table=$h"; done
pg_dump -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" --schema-only $EXCLUDES | psql_tmp -q
pg_dump -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$PGDATABASE" --data-only --schema=public $EXCLUDES $HT_EXCL --disable-triggers | psql_tmp -q
for h in $HTS; do
  psql_src -c "\copy (SELECT * FROM \"$h\") TO STDOUT" | psql_tmp -c "\copy \"$h\" FROM STDIN"
done
info "临时库 $TMP_DB 灌入完成（超表 \copy 直灌 $(echo "$HTS" | wc -l | tr -d ' ') 张）"

# ---------- 3) 原地脱敏（--raw 跳过） ----------
if [[ $RAW -eq 0 ]]; then
  SD_SQL="$OUT_DIR/.sanitize.sql"; : > "$SD_SQL"
  # replica 角色：脱敏 UPDATE 不触发业务触发器（临时库无 jiebacfg 等扩展配置）
  echo 'SET session_replication_role = replica;' >> "$SD_SQL"
  echo 'CREATE TABLE sd_mobile_map AS SELECT id AS uid, mobile AS real FROM "user" WHERE mobile IS NOT NULL;' >> "$SD_SQL"
  CUR_T=""; PK=""; ML=""
  while IFS='|' read -r t c ty nbl dec ml; do
    if [[ "$t" != "$CUR_T" ]]; then CUR_T="$t"
      # 完整主键列（复合键全取）；无主键表回退 ctid（运行期内物理唯一）
      PK=$(psql_tmp -tAc "SELECT string_agg(quote_ident(a.attname), ',' ORDER BY x.ord)
            FROM pg_index i
            CROSS JOIN LATERAL unnest(i.indkey) WITH ORDINALITY AS x(attnum, ord)
            JOIN pg_attribute a ON a.attrelid=i.indrelid AND a.attnum=x.attnum
            WHERE i.indrelid='$t'::regclass AND i.indisprimary" 2>/dev/null || echo "")
      if [[ -z "$PK" ]]; then PK="ctid"; BASIS="ctid::text"; PK_SEL="ctid"; PK_T="t.ctid"; PK_S="s.ctid"
      else BASIS="ROW($PK)::text"
        IFS=',' read -ra _pkp <<< "$PK"
        PK_SEL=$(IFS=,; echo "${_pkp[*]}")
        PK_T=$(printf 't.%s,' "${_pkp[@]}"); PK_T=${PK_T%,}
        PK_S=$(printf 's.%s,' "${_pkp[@]}"); PK_S=${PK_S%,}
      fi
    fi
    case "$dec" in
      KEEP) continue;;
      FAKE_MOBILE)
        echo "WITH s AS (SELECT ${PK_SEL}, row_number() OVER (ORDER BY ${PK}) AS rn FROM \"$t\")
UPDATE \"$t\" t SET \"$c\" = '${FAKE_PREFIX}'||lpad((s.rn % 1000000000)::text, 9, '0') FROM s WHERE (${PK_T}) = (${PK_S});" >> "$SD_SQL";;
      FAKE_NICK)
        echo "WITH s AS (SELECT ${PK_SEL}, row_number() OVER (ORDER BY ${PK}) AS rn FROM \"$t\")
UPDATE \"$t\" t SET \"$c\" = 'drill用户'||s.rn FROM s WHERE (${PK_T}) = (${PK_S});" >> "$SD_SQL";;
      FAKE_EMAIL)
        echo "WITH s AS (SELECT ${PK_SEL}, row_number() OVER (ORDER BY ${PK}) AS rn FROM \"$t\")
UPDATE \"$t\" t SET \"$c\" = 'drill'||s.rn||'@drill.invalid' FROM s WHERE (${PK_T}) = (${PK_S});" >> "$SD_SQL";;
      NULLIFY)     echo "UPDATE \"$t\" SET \"$c\" = NULL;" >> "$SD_SQL";;
      PLACEHOLDER)
        case "$ty" in
          jsonb|json) echo "UPDATE \"$t\" SET \"$c\" = to_jsonb('sd_'||md5(${BASIS}));" >> "$SD_SQL";;
          tsvector)   echo "UPDATE \"$t\" SET \"$c\" = to_tsvector('simple', 'sd_'||md5(${BASIS}));" >> "$SD_SQL";;
          *)  # 窄 varchar 按列宽截断：'sd_' 前缀 + md5 截段（宽<=3 时仅 md5 截段）
            if [[ -n "$ml" && "$ml" != "NULL" && "$ml" -gt 0 ]] 2>/dev/null; then
              if [[ "$ml" -le 3 ]]; then
                echo "UPDATE \"$t\" SET \"$c\" = left(md5(${BASIS}), $ml);" >> "$SD_SQL"
              else
                echo "UPDATE \"$t\" SET \"$c\" = 'sd_'||left(md5(${BASIS}), $((ml - 3)));" >> "$SD_SQL"
              fi
            else
              echo "UPDATE \"$t\" SET \"$c\" = 'sd_'||md5(${BASIS});" >> "$SD_SQL"
            fi;;
        esac;;
    esac
  done < "$CLASS_TSV"
  # 映射表在假值已应用后导出（保证与快照严格一致），导完即删、绝不入 dump
  echo "\copy (SELECT t.mobile AS fake, m.real AS real FROM \"user\" t JOIN sd_mobile_map m ON m.uid = t.id) TO '$OUT_DIR/mobile_map.csv' WITH (FORMAT csv, HEADER true)" >> "$SD_SQL"
  echo 'DROP TABLE sd_mobile_map;' >> "$SD_SQL"
  psql_tmp -q -f "$SD_SQL" || fail "脱敏 UPDATE 失败，逐条排查 ${SD_SQL}"
  info "原地脱敏完成"
else
  warn "--raw 模式：跳过脱敏（扫描应爆红）"
fi

# ---------- 4) 残留 PII 扫描（安全网） ----------
info "残留 PII 扫描…"
VIOL=""
for TBL in user adm_user; do
  for COL in mobile account; do
    HAS=$(psql_tmp -tAc "SELECT 1 FROM information_schema.columns WHERE table_name='$TBL' AND column_name='$COL'" || true)
    [[ -n "$HAS" ]] || continue
    N=$(psql_tmp -tAc "SELECT count(*) FROM \"$TBL\" WHERE \"$COL\" ~ '1[3-9][0-9]{9}' AND \"$COL\" !~ '^${FAKE_PREFIX}'" || echo "?")
    [[ "$N" == "0" ]] || VIOL+="$TBL.$COL 残留真号 $N 条; "
  done
done
SAMPLE_REAL=$(psql_src -tAc "SELECT coalesce(string_agg(mobile, ','), '') FROM (SELECT DISTINCT mobile FROM \"user\" WHERE mobile ~ '1[3-9][0-9]{9}' LIMIT 500) s")
if [[ -n "$SAMPLE_REAL" ]]; then
  LEAKED=$(psql_tmp -tAc "SELECT count(*) FROM \"user\" WHERE mobile = ANY(string_to_array(\$s\$${SAMPLE_REAL}\$s\$, ','))" || echo "?")
  [[ "$LEAKED" == "0" ]] || VIOL+="抽样真手机号命中 $LEAKED 条; "
fi
if [[ -n "$VIOL" ]]; then
  if [[ $RAW -eq 1 ]]; then
    echo -e "${RED}[sanitized_snapshot] 扫描爆红（--raw 预期，检测能力实证）：${VIOL}${NC}"
    exit 10
  else
    fail "残留扫描未通过 → $VIOL"
  fi
fi
info "残留扫描通过 ✓"

# ---------- 5) 行数对账 + 导出 ----------
MANIFEST="$OUT_DIR/manifest.txt"
echo "sanitized_snapshot @ $TS | 源 ${PGHOST}:${PGPORT}/${PGDATABASE} | raw=$RAW | fake_prefix=${FAKE_PREFIX}" > "$MANIFEST"
PARITY="PASS"
while read -r t; do
  [[ -z "$t" ]] && continue
  s=$(psql_src -tAc "SELECT count(*) FROM \"$t\""); d=$(psql_tmp -tAc "SELECT count(*) FROM \"$t\"")
  if [[ "$s" != "$d" ]]; then PARITY="FAIL($t)"; warn "行数不一致 $t: src=$s tmp=$d"; fi
  printf '%s\t%s\t%s\n' "$t" "$s" "$d" >> "$MANIFEST"
done < <(psql_tmp -tAc "SELECT table_name FROM information_schema.tables
                        WHERE table_schema='public' AND table_type='BASE TABLE'
                          AND table_name !~ '^(${SKIP_TABLES})$' ORDER BY table_name")
[[ "$PARITY" == "PASS" ]] || fail "行数对账失败"
info "行数对账通过 ✓"

DUMP="$OUT_DIR/sanitized.dump"
pg_dump -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" -d "$TMP_DB" -Fc -f "$DUMP"
SHA=$(shasum -a 256 "$DUMP" | cut -d' ' -f1)
DUR=$(( $(date +%s) - T0 ))
{
  echo "sha256=$SHA"
  echo "size=$(du -h "$DUMP" | cut -f1 | tr -d ' ') duration=${DUR}s"
  echo "scan=PASS parity=PASS"
} >> "$MANIFEST"
cleanup
[[ $RAW -eq 1 ]] || warn "mobile_map.csv 为假↔真映射：留在授权环境，严禁随快照外发"
info "完成：${DUMP}（${DUR}s）——manifest / mobile_map.csv / class.tsv 同目录"
