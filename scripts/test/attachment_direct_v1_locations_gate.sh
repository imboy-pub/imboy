#!/usr/bin/env bash
# direct-v1 位置映射与 attempt 账本门禁（step-4 / T26/T41 相关 SQL 行为断言）
# 用法: bash scripts/test/attachment_direct_v1_locations_gate.sh
# 依赖: 本地 PG（config/sys.local.config 的 imboy_db 配置；默认 127.0.0.1:4323 imboy_test_v1）
# 全部断言在单事务内执行并 ROLLBACK，不留任何数据。
set -euo pipefail
cd "$(dirname "$0")/../../"

DB_HOST="${IMBOY_PGHOST:-127.0.0.1}"
DB_PORT="${IMBOY_PGPORT:-4323}"
DB_USER="${IMBOY_PGUSER:-imboy_user}"
DB_NAME="${IMBOY_PGDATABASE:-imboy_test_v1}"
export PGPASSWORD="${IMBOY_PGPASSWORD:-abc54321}"

OUT="$(psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -tA <<'SQL' 2>&1)"
BEGIN;
INSERT INTO attach_pending (object_key, bucket, scope, creator_user_id, session_state, direct_state, upload_registration, upload_content,
  storage_driver, storage_namespace, storage_container, storage_locator_sha256)
VALUES ('u999/dv1-gate', 'b', 'private', 999, 'registered', 'GRANTED',
  '{"protocol":"direct-v1","subject":{"kind":"user","principal":"999"},"declaration":{"scope":"private"}}'::jsonb,
  '{"version":"1","mime":"image/png","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","size_bytes":"10"}'::jsonb,
  'garage','g','b', repeat('a',64));
SELECT 'A1:' || fn_dv1_attempt_create('u999/dv1-gate','u999/dv1-gate','direct-c/g/1/a','garage','g','b','gate');
SELECT 'A2:' || fn_dv1_attempt_transition('u999/dv1-gate',1,'CREATED','DISPATCHED',NULL);
SELECT 'A3:' || fn_dv1_attempt_transition('u999/dv1-gate',1,'DISPATCHED','UNKNOWN_QUARANTINED','copy_timeout');
DO $$ BEGIN
  BEGIN
    PERFORM fn_dv1_attempt_transition('u999/dv1-gate',1,'UNKNOWN_QUARANTINED','PROOF_VERIFIED',NULL);
    RAISE EXCEPTION 'GATE_FAIL no-promotion edge accepted';
  EXCEPTION WHEN SQLSTATE '23510' THEN NULL; -- 预期：unknown 无晋升边被拒绝
  END;
END $$;
SELECT 'A4:no-promotion-rejected';
SELECT 'A5:' || fn_dv1_attempt_create('u999/dv1-gate','u999/dv1-gate','direct-c/g/2/b','garage','g','b','gate');
SELECT 'A6:' || fn_dv1_attempt_create('u999/dv1-gate','u999/dv1-gate','direct-c/g/3/c','garage','g','b','gate');
DO $$ BEGIN
  BEGIN
    PERFORM fn_dv1_attempt_create('u999/dv1-gate','u999/dv1-gate','direct-c/g/4/d','garage','g','b','gate');
    RAISE EXCEPTION 'GATE_FAIL budget not enforced';
  EXCEPTION WHEN SQLSTATE '23514' THEN NULL; -- 预期：预算耗尽（fn 内 RAISE 23514）
  END;
END $$;
SELECT 'A7:budget-enforced';
UPDATE attach_pending SET direct_state='FINALIZING' WHERE object_key='u999/dv1-gate';
SELECT fn_dv1_attempt_transition('u999/dv1-gate',2,'CREATED','DISPATCHED',NULL);
SELECT fn_dv1_attempt_transition('u999/dv1-gate',2,'DISPATCHED','COPY_COMPLETED',NULL);
UPDATE attach_direct_attempt SET attempt_state='PROOF_VERIFIED', proof_sha256=repeat('a',64), proof_size=10, proof_received_at=now()
 WHERE object_key='u999/dv1-gate' AND attempt_no=2 AND attempt_state='COPY_COMPLETED';
INSERT INTO attachment (id, file_hash256, mime_type, ext, name, path, url, size, info, referer_time, creator_user_id, scope, scope_ref, cipher, status, storage_driver, storage_namespace, storage_container, storage_locator_sha256)
VALUES (999002, repeat('a',64), 'image/png', '.png', 'a.png', 'u999/dv1-gate', '', 10, '{}', 1, 999, 'private', '', null, 1, 'garage','g','b', repeat('a',64));
SELECT fn_dv1_winner_pin('u999/dv1-gate',2,'direct-c/g/2/b',repeat('a',64));
SELECT 'A8:' || CASE WHEN storage_physical_key='direct-c/g/2/b' THEN 'mapped' ELSE 'bad:'||coalesce(storage_physical_key,'NULL') END FROM attachment WHERE path='u999/dv1-gate';
SELECT 'A9:' || direct_state || '/' || session_state || '/' || confirmed_attachment_id FROM attach_pending WHERE object_key='u999/dv1-gate';
SELECT 'A10:' || attempt_state FROM attach_direct_attempt WHERE object_key='u999/dv1-gate' AND attempt_no=2;
ROLLBACK;
SQL

echo "$OUT" | grep -q '^A1:1$'        || { echo "FAIL A1 attempt create"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A2:true$'        || { echo "FAIL A2 dispatch"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A3:true$'        || { echo "FAIL A3 quarantine"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A4:'          || { echo "FAIL A4 no-promotion"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A5:2$'        || { echo "FAIL A5 second attempt"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A6:3$'        || { echo "FAIL A6 third attempt"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A7:'          || { echo "FAIL A7 budget"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A8:mapped$'   || { echo "FAIL A8 physical mapping"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A9:CONFIRMED/confirmed/999002$' || { echo "FAIL A9 pending confirm"; echo "$OUT"; exit 1; }
echo "$OUT" | grep -q '^A10:WINNER$'  || { echo "FAIL A10 winner"; echo "$OUT"; exit 1; }
echo "PASS attachment_direct_v1_locations_gate"
