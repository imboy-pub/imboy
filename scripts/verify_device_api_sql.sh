#!/usr/bin/env bash
# B.3.2 — Device API JOIN/claim SQL 真实 PostgreSQL 集成验证（ADR 03 §8）。
#
# 在临时 PG 实例上应用 olm 基础迁移（42 olm 表 / 43 user_device 扩展列 /
# 44 trust_audit / 45 OTK 审计列），插入 fixture，跑 olm_identity_repo 中的
# **真实 SQL**（list_devices_with_identity 的 JOIN + claim_one_time_key 的 CTE），
# 断言：
#   - 设备列表 INNER JOIN 只返回 status=1 且有 olm_identity 的设备（排除非活跃 / 无身份）；
#   - capabilities/trust_state 默认列可读（migration 43）；
#   - claim 原子 UPDATE 审计：选中行 status='claimed' + consumed_at + claimed_by，不删除。
#
# 临时实例跑完即删，零残留、不碰任何真实库。需本机 postgresql@18（或 PATH 内 initdb/pg_ctl）。
# 用法：bash scripts/verify_device_api_sql.sh
set -euo pipefail
cd "$(dirname "$0")/.."
MIG=priv/migrations

# 定位 PG 可执行文件：bin 目录须同时含 initdb + postgres 服务器（排除 libpq 客户端桩）
PGBIN=""
for c in /opt/homebrew/opt/postgresql@*/bin/initdb \
         /usr/lib/postgresql/*/bin/initdb \
         "$(command -v initdb 2>/dev/null || true)"; do
  d="$(dirname "$c" 2>/dev/null || true)"
  if [ -n "$d" ] && [ -x "$d/initdb" ] && [ -x "$d/postgres" ]; then PGBIN="$d"; break; fi
done
if [ -z "$PGBIN" ]; then echo "[SKIP] 未找到 PostgreSQL 服务器（initdb+postgres），跳过集成验证"; exit 0; fi

PGDATA="$(mktemp -d "${TMPDIR:-/tmp}/devapi_verify.XXXXXX")"
PGPORT="${PGPORT:-54331}"
cleanup() { "$PGBIN/pg_ctl" -D "$PGDATA" -m immediate stop >/dev/null 2>&1 || true; rm -rf "$PGDATA"; }
trap cleanup EXIT

"$PGBIN/initdb" -D "$PGDATA" -U postgres --auth=trust >/dev/null 2>&1
"$PGBIN/pg_ctl" -D "$PGDATA" -o "-p $PGPORT -k $PGDATA -c listen_addresses=''" -w start >/dev/null 2>&1
PSQL=("$PGBIN/psql" -h "$PGDATA" -p "$PGPORT" -U postgres -d migtest -v ON_ERROR_STOP=1 -qtA)
"$PGBIN/createdb" -h "$PGDATA" -p "$PGPORT" -U postgres migtest

# 基础 user_device 表（迁移 43 前的代表性结构：device_id varchar(40)）
"${PSQL[@]}" -c "CREATE TABLE public.user_device (
  id bigserial PRIMARY KEY, user_id bigint NOT NULL,
  device_id varchar(40) NOT NULL, device_type varchar(20) NOT NULL DEFAULT '',
  status integer NOT NULL DEFAULT 1);" >/dev/null

# 应用 olm 基础迁移 + device-identity/trust/otk-audit 扩展
for m in 00000042_olm_prekeys 00000043_device_identity 00000044_device_trust 00000045_olm_otk_audit; do
  "${PSQL[@]}" -f "$MIG/${m}.up.sql" >/dev/null
done

# ---- fixture ----
# user 200: 2 活跃有身份设备 + 1 非活跃 + 1 活跃但无 olm_identity（应被 JOIN 排除）
"${PSQL[@]}" -c "INSERT INTO public.user_device (user_id, device_id, device_type, status, capabilities, trust_state) VALUES
  (200,'phone-a','phone',1, '{olm,megolm}','unverified'),
  (200,'ipad-b','ipad',1,  '{olm}','verified'),
  (200,'dead-c','phone',0, '{olm}','revoked'),
  (200,'noident-d','web',1,'{}','unverified');" >/dev/null
"${PSQL[@]}" -c "INSERT INTO public.olm_identity (id,user_id,device_id,ed25519_key,curve25519_key,signature) VALUES
  (1,200,'phone-a','ed-a','cv-a','sig-a'),
  (2,200,'ipad-b','ed-b','cv-b','sig-b');" >/dev/null
"${PSQL[@]}" -c "INSERT INTO public.olm_one_time_key (id,user_id,device_id,key_id,key_base64) VALUES
  (10,200,'phone-a','otk1','k1'),(11,200,'phone-a','otk2','k2');" >/dev/null

fail=0
check() { # desc expected actual
  if [ "$2" = "$3" ]; then echo "[OK] $1"; else echo "[FAIL] $1: expected=$2 got=$3"; fail=1; fi
}

# ---- 断言 1：list_devices_with_identity JOIN 只返回活跃+有身份设备 ----
LIST_SQL="SELECT ud.device_id, ud.device_type, ud.capabilities, ud.trust_state,
       ud.identity_blob, ud.identity_signature,
       oi.ed25519_key, oi.curve25519_key, oi.signature
 FROM public.user_device ud
 JOIN public.olm_identity oi ON ud.user_id = oi.user_id AND ud.device_id = oi.device_id
 WHERE ud.user_id = 200 AND ud.status = 1
 ORDER BY ud.device_id"
CNT=$("${PSQL[@]}" -c "SELECT count(*) FROM ($LIST_SQL) t;")
check "JOIN 返回 2 设备（排除非活跃 dead-c + 无身份 noident-d）" "2" "$CNT"
IDS=$("${PSQL[@]}" -c "SELECT string_agg(device_id,',') FROM ($LIST_SQL) t;")
check "JOIN 设备为 ipad-b,phone-a（按 device_id 排序）" "ipad-b,phone-a" "$IDS"
CAPS=$("${PSQL[@]}" -c "SELECT array_to_string(capabilities,'|') FROM ($LIST_SQL) t WHERE device_id='phone-a';")
check "capabilities 列可读（migration 43）" "olm|megolm" "$CAPS"
TS=$("${PSQL[@]}" -c "SELECT trust_state FROM ($LIST_SQL) t WHERE device_id='ipad-b';")
check "trust_state 列可读" "verified" "$TS"

# ---- 断言 2：claim_one_time_key 原子 CTE 保留审计（status='claimed' 不删）----
CLAIM_SQL="WITH picked AS (
  SELECT id, key_id, key_base64 FROM public.olm_one_time_key
  WHERE user_id = 200 AND device_id = 'phone-a' AND status = 'available'
  ORDER BY id ASC LIMIT 1 FOR UPDATE SKIP LOCKED
),
claimed AS (
  UPDATE public.olm_one_time_key
  SET status = 'claimed', consumed_at = CURRENT_TIMESTAMP, claimed_by = 100
  WHERE id IN (SELECT id FROM picked) RETURNING id
)
SELECT p.key_id FROM picked p JOIN claimed c ON p.id = c.id"
CLAIMED_KEY=$("${PSQL[@]}" -c "$CLAIM_SQL;")
check "claim 返回最早 OTK（otk1）" "otk1" "$CLAIMED_KEY"
STILL=$("${PSQL[@]}" -c "SELECT count(*) FROM public.olm_one_time_key WHERE id=10 AND status='claimed' AND claimed_by=100 AND consumed_at IS NOT NULL;")
check "claim 保留审计行（UPDATE 非 DELETE，claimed_by/consumed_at 填充）" "1" "$STILL"
AVAIL=$("${PSQL[@]}" -c "SELECT count(*) FROM public.olm_one_time_key WHERE user_id=200 AND device_id='phone-a' AND status='available';")
check "剩余 available OTK = 1（低水位口径一致）" "1" "$AVAIL"

# ---- 断言 3：trust_audit 表存在且 append-only 结构（migration 44 foundation）----
TA=$("${PSQL[@]}" -c "SELECT count(*) FROM information_schema.tables WHERE table_name='trust_audit';")
check "trust_audit 表已建（migration 44 foundation）" "1" "$TA"

if [ "$fail" -eq 0 ]; then echo "[PASS] Device API SQL 集成验证全部通过"; else echo "[FAIL] Device API SQL 集成验证有失败项"; fi
exit "$fail"
