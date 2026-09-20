#!/usr/bin/env bash
# EB-01 — 企业业务与客服 Schema 的 PostgreSQL 契约 / 迁移往返集成验证。
#
# 契约决策：EB-D02/D03/D04/D05/D06/D07/D11/D12、必需表、数据不变量检查、验收门 EB-01。
#
# 在一次性 loopback scratch PostgreSQL 18 中验证：
#   - A01：真实历史迁移（76/95/113）+ 新企业迁移（114..120）up/down 往返，企业对象残留为 0；
#   - A02：复合 FK、active partial unique、sender XOR 等全部负例被数据库拒绝；
#   - A03：删除 "user" 不级联企业数据（active 经办关系 fail-closed）；
#   - A04：organization_member 直接 removed / DELETE 在存在 active assignment 时被拒绝；
#   - A05：集群内无企业对象残留且临时目录被清理；
#   - A06：跨 Org Workspace、未到期 purge、active hold 下 purge、retain_until 缩短、
#          普通角色直删全部被数据库拒绝，且合法的 bounded purge 仍可成功。
#
# 只使用脚本自建的合成数据（无真实账号 / 联系方式 / 客户数据 / 生产资源 / 远端服务）。
# 临时实例使用私有 unix socket（listen_addresses=''），退出时立即停止并删除。
# 用法：bash scripts/enterprise_business_db_it.sh
set -euo pipefail

cd "$(dirname "$0")/.."
MIGRATIONS_DIR="priv/migrations"

# ---------------------------------------------------------------- PGBIN 探测
PGBIN="${PGBIN:-}"
if [ -z "$PGBIN" ]; then
  for candidate in /opt/homebrew/opt/postgresql@18/bin/initdb \
                   /opt/homebrew/opt/postgresql@*/bin/initdb \
                   /usr/lib/postgresql/*/bin/initdb \
                   "$(command -v initdb 2>/dev/null || true)"; do
    candidate_dir="$(dirname "$candidate" 2>/dev/null || true)"
    if [ -n "$candidate_dir" ] \
       && [ -x "$candidate_dir/initdb" ] \
       && [ -x "$candidate_dir/postgres" ] \
       && [ -x "$candidate_dir/psql" ] \
       && [ -x "$candidate_dir/pg_ctl" ] \
       && [ -x "$candidate_dir/createdb" ]; then
      PGBIN="$candidate_dir"
      break
    fi
  done
fi

if [ -z "$PGBIN" ] \
   || [ ! -x "$PGBIN/initdb" ] \
   || [ ! -x "$PGBIN/postgres" ] \
   || [ ! -x "$PGBIN/psql" ] \
   || [ ! -x "$PGBIN/pg_ctl" ] \
   || [ ! -x "$PGBIN/createdb" ]; then
  echo "[FAIL] 未找到完整 PostgreSQL 服务器工具集（initdb/postgres/psql/pg_ctl/createdb）"
  exit 1
fi

EB_PG_PORT="${EB_PGPORT:-55441}"
case "$EB_PG_PORT" in
  ''|*[!0-9]*|??????*)
    echo "[FAIL] EB_PGPORT 必须是 1024-65535 的十进制端口"
    exit 1
    ;;
esac
if [ "$EB_PG_PORT" -lt 1024 ] || [ "$EB_PG_PORT" -gt 65535 ]; then
  echo "[FAIL] EB_PGPORT 必须是 1024-65535 的十进制端口"
  exit 1
fi

# 不继承调用者的 libpq 连接目标、认证或会话选项，避免客户端绕开私有 socket。
unset PGHOST PGHOSTADDR PGPORT PGDATABASE PGUSER PGPASSWORD PGPASSFILE
unset PGSERVICE PGSERVICEFILE PGSYSCONFDIR PGOPTIONS PGAPPNAME
unset PGCONNECT_TIMEOUT PGTARGETSESSIONATTRS PGCLIENTENCODING
unset PGCHANNELBINDING PGREQUIREAUTH PGSSLMODE PGREQUIRESSL
unset PGSSLCERT PGSSLKEY PGSSLROOTCERT PGSSLCRL PGSSLCRLDIR PGSSLSNI
unset PGMINPROTOCOLVERSION PGMAXPROTOCOLVERSION PGGSSENCMODE
unset PGKRBSRVNAME PGGSSLIB PSQLRC

# ------------------------------------------------------- 迁移文件可用性前置门
# 先于启动集群执行：迁移缺失时必须立即以清晰的 RED 输出失败。
MIG_SPECS="
00000114:enterprise_business_identity
00000115:enterprise_contact
00000116:enterprise_conversation_message
00000117:enterprise_retention
00000118:enterprise_asset
00000119:enterprise_audit_event
00000120:enterprise_offboarding
00000121:enterprise_retention_hold_scope_active
00000122:enterprise_conversation_consent_evidence_kind
"

PRE_FAIL=0
require_migration() {
  local path="$MIGRATIONS_DIR/$1"
  if [ ! -s "$path" ]; then
    echo "[FAIL] 缺失或为空的迁移文件: $path: EB-01 尚未交付"
    PRE_FAIL=$((PRE_FAIL + 1))
  fi
}

require_migration 00000076_workspace_foundation.up.sql
require_migration 00000076_workspace_foundation.down.sql
require_migration 00000095_organization_foundation.up.sql
require_migration 00000095_organization_foundation.down.sql
require_migration 00000113_organization_member.up.sql
require_migration 00000113_organization_member.down.sql

for spec in $MIG_SPECS; do
  version="${spec%%:*}"
  slug="${spec#*:}"
  require_migration "${version}_${slug}.up.sql"
  require_migration "${version}_${slug}.down.sql"
done

if [ "$PRE_FAIL" -ne 0 ]; then
  echo
  echo "总计: PASS=0 FAIL=${PRE_FAIL}"
  exit 1
fi

# ---------------------------------------------------------------- 临时实例
PGDATA="$(mktemp -d "/tmp/enterprise_business_db_it.XXXXXX")"
SERVER_STARTED=0

cleanup() {
  if [ "$SERVER_STARTED" -eq 1 ]; then
    if ! "$PGBIN/pg_ctl" -D "$PGDATA" -m immediate stop >/dev/null 2>&1; then
      echo "[WARN] 临时 PostgreSQL 停止失败，保留数据目录供人工检查：$PGDATA" >&2
      return 0
    fi
    SERVER_STARTED=0
  fi
  case "$PGDATA" in
    /tmp/enterprise_business_db_it.??????)
      rm -rf -- "$PGDATA"
      ;;
    *)
      echo "[WARN] 拒绝删除非预期临时目录：$PGDATA" >&2
      ;;
  esac
}
trap cleanup EXIT

"$PGBIN/initdb" -D "$PGDATA" -U postgres --auth=trust --no-locale -E UTF8 >/dev/null
SERVER_STARTED=1
"$PGBIN/pg_ctl" -D "$PGDATA" \
  -o "-p $EB_PG_PORT -k $PGDATA -c listen_addresses=''" -w start >/dev/null
"$PGBIN/createdb" -h "$PGDATA" -p "$EB_PG_PORT" -U postgres eb_roundtrip
"$PGBIN/createdb" -h "$PGDATA" -p "$EB_PG_PORT" -U postgres eb_contract

PSQL_BASE=(
  "$PGBIN/psql"
  -X
  -h "$PGDATA"
  -p "$EB_PG_PORT"
  -U postgres
  -v ON_ERROR_STOP=1
  -v VERBOSITY=verbose
  -qtA
)

PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "[OK] $1"
}

bad() {
  FAIL=$((FAIL + 1))
  echo "[FAIL] $1: ${2:-<无详情>}"
}

# EB-03R：能力断言块。ID 只在**通过**时打印 `[ASSERT <ID>]`，因此
# 「输出里出现过某 ID」严格等价于「该 ID 的断言真的通过」——失败的断言不会贡献 ID，
# A03 的 ID 集合判定因此不可能被失败项凑数。
aok() {
  PASS=$((PASS + 1))
  echo "[ASSERT $1] $2"
}

abad() {
  FAIL=$((FAIL + 1))
  echo "[FAIL] $1 $2: ${3:-<无详情>}"
}

check_equal() {
  local description="$1" expected="$2" actual="$3"
  if [ "$actual" = "$expected" ]; then
    ok "$description"
  else
    bad "$description" "expected=$expected actual=$actual"
  fi
}

# 在指定库执行 SQL（多语句走同一隐含事务）；返回值即 psql 退出码。
q() {
  local db="$1"
  shift
  "${PSQL_BASE[@]}" -d "$db" -c "$1"
}

qq() {
  local db="$1"
  shift
  "${PSQL_BASE[@]}" -d "$db" -c "$1" >/dev/null
}

run_migration() {
  local description="$1" db="$2" file="$3" output
  if output="$("${PSQL_BASE[@]}" -d "$db" -1 -f "$file" 2>&1)"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

expect_sqlstate() {
  local db="$1" description="$2" expected_state="$3" sql="$4" output
  if output="$("${PSQL_BASE[@]}" -d "$db" -c "$sql" 2>&1)"; then
    bad "$description" "期望 SQLSTATE ${expected_state}，实际成功"
  elif printf '%s' "$output" | grep -q "ERROR:  ${expected_state}:"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

expect_sqlstate_msg() {
  local db="$1" description="$2" expected_state="$3" needle="$4" sql="$5" output
  if output="$("${PSQL_BASE[@]}" -d "$db" -c "$sql" 2>&1)"; then
    bad "$description" "期望 SQLSTATE ${expected_state}，实际成功"
  elif printf '%s' "$output" | grep -q "ERROR:  ${expected_state}:" \
    && printf '%s' "$output" | grep -q "$needle"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

# 接受多个可接受的 SQLSTATE（引用完整性家族）：INSERT 违反 FK 是 23503，
# 而 ON DELETE RESTRICT 的删除是 23001 restrict_violation。两者都必须真的报错。
expect_sqlstate_any() {
  local db="$1" description="$2" expected_states="$3" sql="$4" output state observed
  if output="$("${PSQL_BASE[@]}" -d "$db" -c "$sql" 2>&1)"; then
    bad "$description" "期望 SQLSTATE ${expected_states}，实际成功"
    return
  fi
  for state in $expected_states; do
    if printf '%s' "$output" | grep -q "ERROR:  ${state}:"; then
      ok "${description}（SQLSTATE ${state}）"
      return
    fi
  done
  observed="$(printf '%s' "$output" | grep -m1 'ERROR:' || true)"
  bad "$description" "期望 SQLSTATE ${expected_states}，实际 ${observed}"
}

expect_file_sqlstate() {
  local db="$1" description="$2" expected_state="$3" file="$4" output
  if output="$("${PSQL_BASE[@]}" -d "$db" -1 -f "$file" 2>&1)"; then
    bad "$description" "期望 SQLSTATE ${expected_state}，实际成功"
  elif printf '%s' "$output" | grep -q "ERROR:  ${expected_state}:"; then
    ok "$description"
  else
    bad "$description" "$output"
  fi
}

# 不带 -q 以便捕获命令标签（DELETE 1）。
expect_delete_one() {
  local db="$1" description="$2" sql="$3" output
  if output="$("$PGBIN/psql" -X -h "$PGDATA" -p "$EB_PG_PORT" -U postgres -d "$db" \
      -tA -v ON_ERROR_STOP=1 -v VERBOSITY=verbose -c "$sql" 2>&1)"; then
    if printf '%s' "$output" | grep -q 'DELETE 1'; then
      ok "$description"
    else
      bad "$description" "未观察到 DELETE 1，输出=$output"
    fi
  else
    bad "$description" "$output"
  fi
}

# ------------------------------------------------------------- 版本 / socket
PG_VERSION_NUM="$("${PSQL_BASE[@]}" -d postgres -c "SHOW server_version_num;")"
case "$PG_VERSION_NUM" in
  ''|*[!0-9]*)
    echo "[FAIL] 无法识别 PostgreSQL server_version_num：$PG_VERSION_NUM"
    exit 1
    ;;
esac
if [ "$PG_VERSION_NUM" -lt 180000 ] || [ "$PG_VERSION_NUM" -ge 190000 ]; then
  echo "[FAIL] 必须使用 PostgreSQL 18，当前 server_version_num=$PG_VERSION_NUM"
  exit 1
fi

SOCKET_STATE="$("${PSQL_BASE[@]}" -d postgres -c "
  SELECT (inet_server_addr() IS NULL)::text || ':' ||
         (current_setting('unix_socket_directories')='${PGDATA}')::text;
")"
if [ "$SOCKET_STATE" = "true:true" ]; then
  ok "连接仅使用脚本私有 unix socket"
else
  bad "连接必须使用脚本私有 Unix socket" "actual=$SOCKET_STATE"
  exit 1
fi

PG_VERSION="$("${PSQL_BASE[@]}" -d postgres -c "SHOW server_version;")"
echo "== PostgreSQL ${PG_VERSION} 企业业务 Schema 契约验收 =="

# purge worker 角色是 cluster 级对象，由测试脚本创建（迁移不 CREATE ROLE，只要求它存在）。
qq postgres "
  CREATE ROLE imboy_enterprise_purge_worker NOLOGIN;
  CREATE ROLE eb_it_app NOLOGIN;
  GRANT imboy_enterprise_purge_worker TO postgres;
"
ok "创建 purge worker / 普通应用角色（cluster 级合成角色，migration 不建角色）"

# ================================================================= A01 往返
echo
echo "-- A01 迁移往返（eb_roundtrip）--"

M076U="$MIGRATIONS_DIR/00000076_workspace_foundation.up.sql"
M076D="$MIGRATIONS_DIR/00000076_workspace_foundation.down.sql"
M095U="$MIGRATIONS_DIR/00000095_organization_foundation.up.sql"
M095D="$MIGRATIONS_DIR/00000095_organization_foundation.down.sql"
M113U="$MIGRATIONS_DIR/00000113_organization_member.up.sql"
M113D="$MIGRATIONS_DIR/00000113_organization_member.down.sql"
M114D="$MIGRATIONS_DIR/00000114_enterprise_business_identity.down.sql"
M115D="$MIGRATIONS_DIR/00000115_enterprise_contact.down.sql"
M116D="$MIGRATIONS_DIR/00000116_enterprise_conversation_message.down.sql"
M117D="$MIGRATIONS_DIR/00000117_enterprise_retention.down.sql"
M118D="$MIGRATIONS_DIR/00000118_enterprise_asset.down.sql"
M119D="$MIGRATIONS_DIR/00000119_enterprise_audit_event.down.sql"
M120D="$MIGRATIONS_DIR/00000120_enterprise_offboarding.down.sql"
M121D="$MIGRATIONS_DIR/00000121_enterprise_retention_hold_scope_active.down.sql"
M122D="$MIGRATIONS_DIR/00000122_enterprise_conversation_consent_evidence_kind.down.sql"

R=eb_roundtrip

# 前置表最小化：只补 "user"（其余全部走真实迁移文件）。
qq "$R" "CREATE TABLE \"user\" (id bigint PRIMARY KEY, nickname varchar(50), status smallint);"
ok "前置于 eb_roundtrip 创建最小 \"user\" 表"

run_migration "76 up（workspace 地基）进入 eb_roundtrip" "$R" "$M076U"
run_migration "95 up（organization 地基）进入 eb_roundtrip" "$R" "$M095U"
run_migration "113 up（organization_member）进入 eb_roundtrip" "$R" "$M113U"

UP_START_FAIL="$FAIL"
for spec in $MIG_SPECS; do
  version="${spec%%:*}"
  slug="${spec#*:}"
  run_migration "${version} up（${slug}）进入 eb_roundtrip" "$R" "$MIGRATIONS_DIR/${version}_${slug}.up.sql"
done
if [ "$FAIL" -ne "$UP_START_FAIL" ]; then
  echo "总计: PASS=${PASS} FAIL=${FAIL}"
  exit 1
fi

# 合成数据：覆盖 §4.1 全部 15 张表 + 复合 FK/partial unique/守卫的可观测行为。
qq "$R" "
  INSERT INTO \"user\"(id, nickname, status) VALUES
    (900001,'eb-owner',1),(900002,'eb-actor',1),(900003,'eb-leaver',1);
  INSERT INTO organization(id,name,owner_id,status) VALUES (700001,'eb-org-1',900001,'active');
  INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES
    (600001,'eb-ws-1',900001,'active','project',700001);
  INSERT INTO organization_business_identity
    (id,organization_id,function_key,display_name,status,version,created_by_user_id)
    VALUES (500001,700001,'sales','销售甲','active',1,900001),
           (500002,700001,'customer_service','客服甲','active',1,900001);
  INSERT INTO organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
    VALUES (400001,700001,500001,'sales',900002,'active',900001);
  INSERT INTO organization_member(organization_id,user_id,role,status) VALUES
    (700001,900003,'member','active');
  INSERT INTO organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
    VALUES (400002,700001,500002,'customer_service',900003,'active',900001);
  INSERT INTO enterprise_contact
    (id,organization_id,imboy_user_id,status,display_name,created_by_business_identity_id)
    VALUES (300001,700001,NULL,'active','客户甲',500001);
  INSERT INTO enterprise_contact_identity
    (id,organization_id,contact_id,channel,subject_hmac,subject_mask)
    VALUES (310001,700001,300001,'wechat',
            repeat('a',64),'wx***1');
  INSERT INTO enterprise_contact_assignment
    (id,organization_id,contact_id,business_identity_id,role,status,assigned_by)
    VALUES (320001,700001,300001,500001,'primary','active',900001);
  INSERT INTO enterprise_note
    (id,organization_id,contact_id,business_identity_id,actor_user_id,body_cipher,body_key_version,status)
    VALUES (330001,700001,300001,500001,900002,'cipher-note-1',1,'active');
  INSERT INTO enterprise_retention_policy
    (id,organization_id,workspace_id,data_class,version,retention_days,trigger_event,created_by_user_id)
    VALUES (120001,700001,600001,'enterprise_message',1,1095,'message.accept',900001);
  INSERT INTO enterprise_conversation
    (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,notice_version,consent_at,consent_subject,consent_evidence_kind)
    VALUES (200001,700001,600001,300001,500001,'active',1,'v1',CURRENT_TIMESTAMP,'synthetic-subject','synthetic');
  INSERT INTO enterprise_message
    (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
     sender_business_identity_id,actor_user_id,client_msg_id,body_cipher,key_version,aad_hash,
     content_hash,policy_id,policy_version,retention_days,retain_until,visibility,version)
    VALUES (100001,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-1',
            'cipher-msg-1',1,repeat('b',64),repeat('c',64),120001,1,1095,
            CURRENT_TIMESTAMP - interval '1 day','visible',1);
  INSERT INTO enterprise_message
    (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
     sender_business_identity_id,actor_user_id,client_msg_id,body_cipher,key_version,aad_hash,
     content_hash,policy_id,policy_version,retention_days,retain_until,visibility,version)
    VALUES (100002,700001,600001,200001,'contact',300001,NULL,NULL,'cmid-2',
            'cipher-msg-2',1,repeat('d',64),repeat('e',64),120001,1,1095,
            CURRENT_TIMESTAMP - interval '2 days','visible',1);
  INSERT INTO enterprise_message_delivery
    (id,organization_id,workspace_id,message_id,recipient_ref,device_id,status,version)
    VALUES (110001,700001,600001,100001,'contact:300001','dev-1','pending',1);
  INSERT INTO enterprise_retention_hold
    (id,organization_id,workspace_id,scope_type,scope_message_id,reason_code,actor_user_id,audit_event_id,version)
    VALUES (130001,700001,600001,'message',100002,'synthetic_legal_hold',900001,150099,1);
  INSERT INTO enterprise_asset
    (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,
     uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,retain_until,version)
    VALUES (140001,700001,600001,200001,100001,500001,900002,
            'enterprise/700001/200001/100001/blob-1.bin',repeat('f',64),'application/octet-stream',128,
            'active',1,CURRENT_TIMESTAMP - interval '1 day',1);
  INSERT INTO enterprise_audit_event
    (id,organization_id,resource_type,resource_id,action,business_identity_id,actor_user_id,actor_role,detail)
    VALUES (150001,700001,'enterprise_message',100001,'message.accept',500001,900002,'member','{}'::jsonb);
  INSERT INTO enterprise_offboarding_case
    (id,organization_id,leaver_user_id,successor_user_id,status,version,item_total,item_success,item_failed,created_by_user_id,reason)
    VALUES (160001,700001,900003,900001,'draft',1,1,0,0,900001,'synthetic-offboarding');
  INSERT INTO enterprise_offboarding_item
    (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
     status,idempotency_key,attempt)
    VALUES (170001,700001,160001,500002,'customer_service',900003,900001,'pending','synthetic-idem-1',0);
"
ok "合成数据写入 eb_roundtrip（15 张企业表）"

STATE_COUNT="$(q "$R" "
  SELECT (SELECT count(*) FROM organization_business_identity)
       || '/' || (SELECT count(*) FROM organization_business_identity_assignment)
       || '/' || (SELECT count(*) FROM enterprise_contact)
       || '/' || (SELECT count(*) FROM enterprise_conversation)
       || '/' || (SELECT count(*) FROM enterprise_message)
       || '/' || (SELECT count(*) FROM enterprise_message_delivery)
       || '/' || (SELECT count(*) FROM enterprise_retention_policy)
       || '/' || (SELECT count(*) FROM enterprise_retention_hold)
       || '/' || (SELECT count(*) FROM enterprise_asset)
       || '/' || (SELECT count(*) FROM enterprise_note)
       || '/' || (SELECT count(*) FROM enterprise_contact_identity)
       || '/' || (SELECT count(*) FROM enterprise_contact_assignment)
       || '/' || (SELECT count(*) FROM enterprise_audit_event)
       || '/' || (SELECT count(*) FROM enterprise_offboarding_case)
       || '/' || (SELECT count(*) FROM enterprise_offboarding_item);
")"
check_equal "114..120 up 后 15 张企业表行数符合合成数据" "2/2/1/1/2/1/1/1/1/1/1/1/1/1/1" "$STATE_COUNT"

# organization_member.status 扩展：suspended 合法
qq "$R" "UPDATE organization_member SET status='suspended' WHERE organization_id=700001 AND user_id=900003;"
SUSPENDED="$(q "$R" "SELECT status FROM organization_member WHERE organization_id=700001 AND user_id=900003;")"
check_equal "114 up 使 organization_member.status 接受 suspended" "suspended" "$SUSPENDED"

# 114 down 的 fail-closed：存在 suspended 行时拒绝回滚（-1 单事务，失败即全滚）
expect_file_sqlstate "$R" "114 down 在存在 suspended 行时 fail-closed 拒绝回滚" "23514" "$M114D"

qq "$R" "UPDATE organization_member SET status='active' WHERE organization_id=700001 AND user_id=900003;"

# 清理合成数据以满足 fail-closed 回滚条件（hold 先 release；消息走合法 purge 通道）。
# 注意：hold/policy/audit 是 append-only 或不可变事实，其行本身不可 DELETE，且 release 后的 hold
# 仍以复合 FK 引用其 scope message —— 这些行连同被引用链由各自的 down（DROP TABLE）移除，
# 与 erlang_migrate 的真实回滚路径一致。
qq "$R" "
  BEGIN;
  SET LOCAL imboy.enterprise_purge = 'on';
  UPDATE enterprise_retention_hold
     SET released_at = CURRENT_TIMESTAMP, released_by_user_id = 900001
   WHERE organization_id = 700001;
  UPDATE organization_business_identity_assignment
     SET status = 'ended', ended_at = assigned_at
   WHERE organization_id = 700001;
  DELETE FROM enterprise_message_delivery WHERE organization_id = 700001;
  DELETE FROM enterprise_asset WHERE organization_id = 700001;
  DELETE FROM enterprise_note WHERE organization_id = 700001;
  DELETE FROM enterprise_contact_assignment WHERE organization_id = 700001;
  DELETE FROM enterprise_contact_identity WHERE organization_id = 700001;
  DELETE FROM enterprise_message WHERE organization_id = 700001 AND id <> 100002;
  DELETE FROM enterprise_offboarding_item WHERE organization_id = 700001;
  DELETE FROM enterprise_offboarding_case WHERE organization_id = 700001;
  DELETE FROM organization_member WHERE organization_id = 700001 AND user_id = 900003;
  COMMIT;
"
ok "清理 eb_roundtrip 合成数据（含 hold release + bounded purge 通道）"

CLEANUP_STATE="$(q "$R" "
  SELECT (SELECT count(*) FROM enterprise_message_delivery)
      || '/' || (SELECT count(*) FROM enterprise_asset)
      || '/' || (SELECT count(*) FROM enterprise_message)
      || '/' || (SELECT count(*) FROM enterprise_retention_hold WHERE released_at IS NULL)
      || '/' || (SELECT count(*) FROM organization_member WHERE status <> 'active')
      || '/' || (SELECT count(*) FROM organization_business_identity_assignment WHERE status = 'active');
")"
check_equal "A01 清场后仅保留 append-only 事实与其引用行" "0/0/1/0/0/0" "$CLEANUP_STATE"

run_migration "122 down（同意证据类别，EB-03R）" "$R" "$M122D"
run_migration "121 down（hold 作用域引用回到无条件 FK，EB-03R）" "$R" "$M121D"
run_migration "120 down" "$R" "$M120D"
run_migration "119 down" "$R" "$M119D"
run_migration "118 down" "$R" "$M118D"
run_migration "117 down" "$R" "$M117D"
run_migration "116 down" "$R" "$M116D"
run_migration "115 down" "$R" "$M115D"
run_migration "114 down" "$R" "$M114D"

RESTORED_STATUS="$(q "$R" "
  SELECT pg_get_constraintdef(c.oid)
    FROM pg_constraint c
   WHERE c.conrelid = 'organization_member'::regclass
     AND c.conname = 'ck_organization_member_status';
")"
if printf '%s' "$RESTORED_STATUS" | grep -q "'active'::text, 'removed'::text"; then
  ok "114 down 恢复 113 的 ck_organization_member_status 形态（无 suspended）"
else
  bad "114 down 恢复 113 的 ck_organization_member_status 形态（无 suspended）" "$RESTORED_STATUS"
fi

run_migration "113 down" "$R" "$M113D"
run_migration "95 down" "$R" "$M095D"
run_migration "76 down" "$R" "$M076D"

RESIDUAL_SQL="
WITH pat(p) AS (
  SELECT unnest(ARRAY['enterprise%','organization_business_identity%','offboarding%',
                       'fn_enterprise%','trg_enterprise%'])
)
SELECT
    (SELECT count(*) FROM pg_class c
      WHERE EXISTS (SELECT 1 FROM pat WHERE c.relname LIKE pat.p))
  + (SELECT count(*) FROM pg_class c
       JOIN pg_index i ON i.indexrelid = c.oid
       JOIN pg_class t ON t.oid = i.indrelid
      WHERE EXISTS (SELECT 1 FROM pat WHERE t.relname LIKE pat.p))
  + (SELECT count(*) FROM pg_proc pr
      WHERE EXISTS (SELECT 1 FROM pat WHERE pr.proname LIKE pat.p))
  + (SELECT count(*) FROM pg_trigger tg
      WHERE NOT tg.tgisinternal
        AND EXISTS (SELECT 1 FROM pat WHERE tg.tgname LIKE pat.p))
  + (SELECT count(*) FROM pg_constraint k
      WHERE EXISTS (SELECT 1 FROM pat WHERE k.conname LIKE pat.p))
  + (SELECT count(*) FROM pg_class c WHERE c.relname = 'uq_workspace_organization_id_id')
  + (SELECT count(*) FROM pg_constraint k WHERE k.conname = 'uq_workspace_organization_id_id');
"

ROUNDTRIP_RESIDUAL="$(q "$R" "$RESIDUAL_SQL")"
check_equal "A01 企业对象残留 = 0（eb_roundtrip）" "0" "$ROUNDTRIP_RESIDUAL"

# ============================================================ 契约负例库
echo
echo "-- A02/A03/A04/A06 约束契约（eb_contract）--"

C=eb_contract

qq "$C" "CREATE TABLE \"user\" (id bigint PRIMARY KEY, nickname varchar(50), status smallint);"
run_migration "76 up（workspace 地基）进入 eb_contract" "$C" "$M076U"
run_migration "95 up（organization 地基）进入 eb_contract" "$C" "$M095U"
run_migration "113 up（organization_member）进入 eb_contract" "$C" "$M113U"
CONTRACT_UP_FAIL="$FAIL"
for spec in $MIG_SPECS; do
  version="${spec%%:*}"
  slug="${spec#*:}"
  run_migration "${version} up（${slug}）进入 eb_contract" "$C" "$MIGRATIONS_DIR/${version}_${slug}.up.sql"
done
if [ "$FAIL" -ne "$CONTRACT_UP_FAIL" ]; then
  echo "总计: PASS=${PASS} FAIL=${FAIL}"
  exit 1
fi

# purge worker / 普通应用角色已在前置阶段创建（cluster 级对象，迁移不建角色）。
qq "$C" "GRANT DELETE ON enterprise_message TO imboy_enterprise_purge_worker;"
ok "为 purge worker 角色授予企业消息 DELETE（唯一 bounded purge 通道）"

qq "$C" "
  INSERT INTO \"user\"(id,nickname,status) VALUES
    (900001,'eb-owner',1),(900002,'eb-actor',1),(900003,'eb-leaver',1),
    (900004,'eb-successor',1),(900005,'eb-other',1);
  INSERT INTO organization(id,name,owner_id,status) VALUES
    (700001,'eb-org-1',900001,'active'),
    (700002,'eb-org-2',900001,'active');
  INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES
    (600001,'eb-ws-1',900001,'active','project',700001),
    (600002,'eb-ws-2',900001,'active','project',700001),
    (600003,'eb-ws-3',900001,'active','project',700002),
    (600004,'eb-ws-null-org',900001,'active','project',NULL);
  INSERT INTO organization_business_identity
    (id,organization_id,function_key,display_name,status,version,created_by_user_id) VALUES
    (500001,700001,'sales','销售甲','active',1,900001),
    (500002,700001,'customer_service','客服甲','active',1,900001),
    (500003,700001,'sales','销售乙','active',1,900001),
    (500004,700002,'sales','销售丙','active',1,900001);
  INSERT INTO organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by) VALUES
    (400001,700001,500001,'sales',900002,'active',900001),
    (400002,700001,500002,'customer_service',900003,'active',900001);
  INSERT INTO organization_member(organization_id,user_id,role,status) VALUES
    (700001,900003,'member','active');
  INSERT INTO enterprise_contact
    (id,organization_id,imboy_user_id,status,display_name,created_by_business_identity_id) VALUES
    (300001,700001,NULL,'active','客户甲',500001),
    (300002,700001,NULL,'active','客户乙',500002);
  INSERT INTO enterprise_contact_identity
    (id,organization_id,contact_id,channel,subject_hmac,subject_mask) VALUES
    (310001,700001,300001,'wechat',repeat('a',64),'wx***1');
  INSERT INTO enterprise_contact_assignment
    (id,organization_id,contact_id,business_identity_id,role,status,assigned_by) VALUES
    (320001,700001,300001,500001,'primary','active',900001);
  INSERT INTO enterprise_note
    (id,organization_id,contact_id,business_identity_id,actor_user_id,body_cipher,body_key_version,status) VALUES
    (330001,700001,300001,500001,900002,'cipher-note-1',1,'active');
  INSERT INTO enterprise_retention_policy
    (id,organization_id,workspace_id,data_class,version,retention_days,trigger_event,created_by_user_id) VALUES
    (120001,700001,600001,'enterprise_message',1,1095,'message.accept',900001);
  INSERT INTO enterprise_conversation
    (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,notice_version,consent_at,consent_subject,consent_evidence_kind) VALUES
    (200001,700001,600001,300001,500001,'active',1,'v1',CURRENT_TIMESTAMP,'synthetic-subject','synthetic');
  INSERT INTO enterprise_message
    (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
     sender_business_identity_id,actor_user_id,client_msg_id,body_cipher,key_version,aad_hash,
     content_hash,policy_id,policy_version,retention_days,retain_until,visibility,version) VALUES
    (100001,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-1','cipher-msg-1',1,
     repeat('b',64),repeat('c',64),120001,1,1095,CURRENT_TIMESTAMP + interval '5 days','visible',1),
    (100002,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-2','cipher-msg-2',1,
     repeat('d',64),repeat('e',64),120001,1,1095,CURRENT_TIMESTAMP - interval '2 days','visible',1),
    (100003,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-3','cipher-msg-3',1,
     repeat('f',64),repeat('1',64),120001,1,1095,CURRENT_TIMESTAMP - interval '3 days','visible',1),
    (100004,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-4','cipher-msg-4',1,
     repeat('2',64),repeat('3',64),120001,1,1095,CURRENT_TIMESTAMP - interval '4 days','visible',1);
  INSERT INTO enterprise_message_delivery
    (id,organization_id,workspace_id,message_id,recipient_ref,device_id,status,version) VALUES
    (110001,700001,600001,100004,'contact:300001','dev-1','pending',1),
    (110002,700001,600001,100001,'contact:300001','dev-1','delivered',1);
  INSERT INTO enterprise_retention_hold
    (id,organization_id,workspace_id,scope_type,scope_message_id,reason_code,actor_user_id,audit_event_id,version) VALUES
    (130001,700001,600001,'message',100002,'synthetic_legal_hold',900001,150099,1);
  INSERT INTO enterprise_asset
    (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,
     uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,retain_until,version) VALUES
    (140001,700001,600001,200001,100001,500001,900002,
     'enterprise/700001/200001/100001/blob-1.bin',repeat('a',64),'application/octet-stream',128,
     'active',1,CURRENT_TIMESTAMP + interval '5 days',1),
    (140002,700001,600001,200001,NULL,500001,900002,
     'enterprise/700001/200001/blob-2.bin',repeat('b',64),'application/octet-stream',64,
     'active',1,CURRENT_TIMESTAMP + interval '5 days',1);
  INSERT INTO enterprise_audit_event
    (id,organization_id,resource_type,resource_id,action,business_identity_id,actor_user_id,actor_role,detail) VALUES
    (150001,700001,'enterprise_message',100001,'message.accept',500001,900002,'member','{}'::jsonb);
  INSERT INTO enterprise_offboarding_case
    (id,organization_id,leaver_user_id,successor_user_id,status,version,item_total,item_success,item_failed,created_by_user_id,reason) VALUES
    (160001,700001,900003,900004,'draft',1,1,0,0,900001,'synthetic-offboarding');
  INSERT INTO enterprise_offboarding_item
    (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
     status,idempotency_key,attempt) VALUES
    (170001,700001,160001,500002,'customer_service',900003,900004,'pending','synthetic-idem-1',0);
"
ok "合成数据写入 eb_contract"

# ------------------------------------------------------------------ A02 负例
echo
echo "-- A02 复合 FK / partial unique / CHECK / 不可变性 --"

expect_sqlstate "$C" "A02 assignment 的 function_key 与 identity 不一致被复合 FK 拒绝" "23503" \
  "INSERT INTO organization_business_identity_assignment
     (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
   VALUES (410001,700001,500003,'customer_service',900005,'active',900001);"

expect_sqlstate "$C" "A02 同一 identity 第二条 active assignment 被 partial unique 拒绝" "23505" \
  "INSERT INTO organization_business_identity_assignment
     (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
   VALUES (410002,700001,500001,'sales',900005,'active',900001);"

expect_sqlstate "$C" "A02 同一 (Org,user,function_key) 第二条 active assignment 被 partial unique 拒绝" "23505" \
  "INSERT INTO organization_business_identity_assignment
     (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
   VALUES (410003,700001,500003,'sales',900002,'active',900001);"

expect_sqlstate "$C" "A02 active assignment 缺 user_id 被 CHECK 拒绝" "23514" \
  "INSERT INTO organization_business_identity_assignment
     (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by)
   VALUES (410004,700001,500001,'sales',NULL,'active',900001);"

expect_sqlstate "$C" "A02 identity.function_key UPDATE 被不可变触发器拒绝" "23514" \
  "UPDATE organization_business_identity SET function_key='customer_service' WHERE id=500001;"

expect_sqlstate "$C" "A02 message 两个 sender 列同时非空被 XOR CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
      sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100101,700001,600001,200001,'contact',300001,500001,NULL,'cmid-both',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 message 两个 sender 列同时为空被 XOR CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
      sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100102,700001,600001,200001,'contact',NULL,NULL,NULL,'cmid-none',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 入站消息带 actor_user_id 被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
      sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100103,700001,600001,200001,'contact',300001,NULL,900002,'cmid-inbound-actor',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 出站消息缺 actor_user_id 被 BEFORE INSERT 触发器拒绝" "23514" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
      sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100104,700001,600001,200001,'business_identity',NULL,500001,NULL,'cmid-outbound-noactor',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 企业消息 client_msg_id 幂等唯一被拒绝" "23505" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
      sender_business_identity_id,actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100105,700001,600001,200001,'business_identity',NULL,500001,900002,'cmid-1',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 enterprise_asset 持久化 URL（s3 scheme）被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_asset
     (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,
      uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,retain_until)
   VALUES (140101,700001,600001,200001,NULL,NULL,900002,'s3://bucket/key.bin',repeat('c',64),
           'application/octet-stream',10,'active',1,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 enterprise_asset 持久化 URL（https scheme）被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_asset
     (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,
      uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,retain_until)
   VALUES (140103,700001,600001,200001,NULL,NULL,900002,
           'HTTPS://garage.internal/bucket/key.bin',repeat('e',64),
           'application/octet-stream',10,'active',1,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 enterprise_asset 保留期早于所属消息被守卫拒绝" "23514" \
  "INSERT INTO enterprise_asset
     (id,organization_id,workspace_id,conversation_id,message_id,business_identity_id,
      uploaded_by_user_id,object_key,object_hash,mime,size_bytes,status,key_version,retain_until)
   VALUES (140102,700001,600001,200001,100001,500001,900002,'enterprise/700001/blob-3.bin',
           repeat('d',64),'application/octet-stream',10,'active',1,
           CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A02 enterprise_contact_identity 裸 SHA（非组织域 HMAC）被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_contact_identity
     (id,organization_id,contact_id,channel,subject_hmac,subject_mask)
   VALUES (310101,700001,300001,'phone','deadbeef','1***');"

expect_sqlstate "$C" "A02 enterprise_retention_hold 作用域不自洽被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_retention_hold
     (id,organization_id,workspace_id,scope_type,scope_conversation_id,scope_message_id,
      reason_code,actor_user_id,audit_event_id,version)
   VALUES (130101,700001,600001,'workspace',200001,100001,'synthetic_scope',900001,150098,1);"

expect_sqlstate "$C" "A02 enterprise_audit_event 禁止 UPDATE（append-only）" "23514" \
  "UPDATE enterprise_audit_event SET action='message.rewrite' WHERE id=150001;"

expect_sqlstate "$C" "A02 enterprise_audit_event 禁止 DELETE（append-only）" "23514" \
  "DELETE FROM enterprise_audit_event WHERE id=150001;"

expect_sqlstate "$C" "A02 enterprise_audit_event 改写 actor 为新 user 被拒绝" "23514" \
  "UPDATE enterprise_audit_event SET actor_user_id=900004 WHERE id=150001;"

expect_sqlstate "$C" "A02 enterprise_audit_event 改写 detail 被拒绝" "23514" \
  "UPDATE enterprise_audit_event SET detail='{\"tampered\":true}'::jsonb WHERE id=150001;"

expect_sqlstate "$C" "A02 enterprise_retention_hold 非 release 的 released_by 写入被拒绝" "23514" \
  "UPDATE enterprise_retention_hold SET released_by_user_id=900001 WHERE id=130001;"

expect_sqlstate "$C" "A02 enterprise_retention_policy 禁止 UPDATE（不可变快照）" "23514" \
  "UPDATE enterprise_retention_policy SET retention_days=2000 WHERE id=120001;"

expect_sqlstate "$C" "A02 enterprise_retention_policy 禁止 DELETE（不可变快照）" "23514" \
  "DELETE FROM enterprise_retention_policy WHERE id=120001;"

expect_sqlstate "$C" "A02 enterprise_offboarding_case 同 Org+leaver 未完成 case 唯一性被拒绝" "23505" \
  "INSERT INTO enterprise_offboarding_case
     (id,organization_id,leaver_user_id,successor_user_id,status,version,item_total,item_success,item_failed,created_by_user_id)
   VALUES (160002,700001,900003,900004,'frozen',1,1,0,0,900001);"

expect_sqlstate "$C" "A02 enterprise_offboarding_item 幂等键唯一性被拒绝" "23505" \
  "INSERT INTO enterprise_offboarding_item
     (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
      status,idempotency_key,attempt)
   VALUES (170002,700001,160001,500002,'customer_service',900003,900004,'pending','synthetic-idem-1',0);"

expect_sqlstate "$C" "A02 enterprise_offboarding_item 成功项缺 to_user_id 被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_offboarding_item
     (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
      status,idempotency_key,attempt)
   VALUES (170003,700001,160001,500002,'customer_service',900003,NULL,'success','synthetic-idem-2',0);"

expect_sqlstate "$C" "A02 enterprise_message_delivery recipient_ref 无类型前缀被 CHECK 拒绝" "23514" \
  "INSERT INTO enterprise_message_delivery
     (id,organization_id,workspace_id,message_id,recipient_ref,device_id,status,version)
   VALUES (110101,700001,600001,100001,'300001','dev-2','pending',1);"

# ------------------------------------------------------------------ A03 负例
echo
echo "-- A03 删除 user 不级联企业数据 --"

SNAPSHOT_SQL="
  SELECT (SELECT count(*) || '/' || coalesce(min(organization_id),0)::text
            FROM organization_business_identity)
      || '|' || (SELECT count(*) || '/' || coalesce(min(organization_id),0)::text
                   FROM enterprise_contact)
      || '|' || (SELECT count(*) || '/' || coalesce(min(organization_id),0)::text
                   FROM enterprise_conversation)
      || '|' || (SELECT count(*) || '/' || coalesce(min(organization_id),0)::text
                   FROM enterprise_message);
"
BEFORE_SNAPSHOT="$(q "$C" "$SNAPSHOT_SQL")"

expect_sqlstate "$C" "A03 删除仍持有 active assignment 的 user 被拒绝" "23514" \
  "DELETE FROM \"user\" WHERE id = 900002;"

AFTER_SNAPSHOT="$(q "$C" "$SNAPSHOT_SQL")"
check_equal "A03 失败删除后企业表行数与 organization_id 不变" "$BEFORE_SNAPSHOT" "$AFTER_SNAPSHOT"

qq "$C" "UPDATE organization_business_identity_assignment
            SET status='ended', ended_at=assigned_at
          WHERE id=400001 AND organization_id=700001;"
ok "A03 将 assignment 置为 ended（释放 active 经办关系）"

A03_DELETE_RC=0
A03_DELETE_OUTPUT="$("${PSQL_BASE[@]}" -d "$C" -c "DELETE FROM \"user\" WHERE id = 900002;" 2>&1)" || A03_DELETE_RC=$?
if [ "$A03_DELETE_RC" -eq 0 ]; then
  ok "A03 assignment 结束后删除 user 成功"
else
  bad "A03 assignment 结束后删除 user 成功" "$A03_DELETE_OUTPUT"
fi

check_equal "A03 成功删除后企业表行数与 organization_id 仍不变" "$BEFORE_SNAPSHOT" "$(q "$C" "$SNAPSHOT_SQL")"

ASSIGN_AFTER="$(q "$C" "
  SELECT count(*) || '/' || coalesce(max(user_id)::text,'null')
    FROM organization_business_identity_assignment
   WHERE id = 400001 AND organization_id = 700001;
")"
check_equal "A03 assignment 行仍在且 user_id 被 SET NULL" "1/null" "$ASSIGN_AFTER"

ACTOR_AFTER="$(q "$C" "
  SELECT count(*) || '/' || coalesce(max(actor_user_id)::text,'null')
    FROM enterprise_message WHERE id = 100001 AND organization_id = 700001;
")"
check_equal "A03 消息行仍在且 actor_user_id 被 SET NULL" "1/null" "$ACTOR_AFTER"

# ------------------------------------------------------------------ A04 负例
echo
echo "-- A04 直接 remove / DELETE organization_member 守卫 --"

expect_sqlstate_msg "$C" \
  "A04 active assignment 存在时 active->removed 被 offboarding 守卫拒绝" "23514" "offboarding_required" \
  "UPDATE organization_member SET status='removed'
    WHERE organization_id=700001 AND user_id=900003;"

expect_sqlstate_msg "$C" \
  "A04 active assignment 存在时 DELETE organization_member 被 offboarding 守卫拒绝" "23514" "offboarding_required" \
  "DELETE FROM organization_member WHERE organization_id=700001 AND user_id=900003;"

qq "$C" "UPDATE organization_member SET status='suspended'
          WHERE organization_id=700001 AND user_id=900003;"
MEMBER_STATUS="$(q "$C" "SELECT status FROM organization_member
                          WHERE organization_id=700001 AND user_id=900003;")"
check_equal "A04 suspend 是合法第一步（撤权生效）" "suspended" "$MEMBER_STATUS"

expect_sqlstate_msg "$C" \
  "A04 suspended 状态的成员仍不能绕过 offboarding 守卫被 DELETE" "23514" "offboarding_required" \
  "DELETE FROM organization_member WHERE organization_id=700001 AND user_id=900003;"

# ------------------------------------------------------------------ A06 负例
echo
echo "-- A06 Org/Workspace 一致性、purge、hold、policy 缩短 --"

expect_sqlstate "$C" "A06 跨 Org Workspace 的 conversation 被复合 FK 拒绝" "23503" \
  "INSERT INTO enterprise_conversation
     (id,organization_id,workspace_id,contact_id,business_identity_id,status,version)
   VALUES (200101,700002,600001,300001,500004,'active',1);"

expect_sqlstate "$C" "A06 指向 organization_id IS NULL 的 workspace 被复合 FK 拒绝" "23503" \
  "INSERT INTO enterprise_conversation
     (id,organization_id,workspace_id,contact_id,business_identity_id,status,version)
   VALUES (200102,700001,600004,300001,500001,'active',1);"

expect_sqlstate "$C" "A06 message 的 workspace 与 conversation 不一致被复合 FK 拒绝" "23503" \
  "INSERT INTO enterprise_message
     (id,organization_id,workspace_id,conversation_id,sender_type,sender_business_identity_id,
      actor_user_id,client_msg_id,retention_days,retain_until)
   VALUES (100106,700001,600002,200001,'business_identity',500001,900001,'cmid-ws-mismatch',
           1095,CURRENT_TIMESTAMP + interval '1 day');"

expect_sqlstate "$C" "A06 retain_until 未到期时 purge 被守卫拒绝" "23514" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100001;
   COMMIT;"

expect_sqlstate "$C" "A06 active hold 下 purge 被守卫拒绝" "23514" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100002;
   COMMIT;"

expect_sqlstate "$C" "A06 retain_until 缩短被守卫拒绝" "23514" \
  "UPDATE enterprise_message SET retain_until = retain_until - interval '1 day'
    WHERE organization_id=700001 AND id=100001;"

expect_sqlstate "$C" "A06 retention_policy 新版本缩短保留期被守卫拒绝" "23514" \
  "INSERT INTO enterprise_retention_policy
     (id,organization_id,workspace_id,data_class,version,retention_days,trigger_event,created_by_user_id)
   VALUES (120002,700001,600001,'enterprise_message',2,365,'message.accept',900001);"

qq "$C" "GRANT SELECT, INSERT, UPDATE ON enterprise_message TO eb_it_app;"
ok "A06 为普通应用角色授予 SELECT/INSERT/UPDATE（刻意不授予 DELETE 与 worker 成员）"

expect_sqlstate "$C" "A06 普通应用角色直删企业消息被拒绝" "42501" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   SET ROLE eb_it_app;
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100002;
   COMMIT;"

qq "$C" "GRANT DELETE ON enterprise_message TO eb_it_app;"
ok "A06 追加 DELETE 授权以隔离角色判定路径"

expect_sqlstate "$C" "A06 有 DELETE 权限但非 worker 成员仍被守卫拒绝（42501）" "42501" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   SET ROLE eb_it_app;
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100002;
   COMMIT;"

expect_sqlstate "$C" "A06 有 DELETE 权限但未开启 purge GUC 被守卫拒绝" "23514" \
  "BEGIN;
   SET ROLE eb_it_app;
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100002;
   COMMIT;"

qq "$C" "GRANT DELETE ON enterprise_message TO imboy_enterprise_purge_worker;"

expect_delete_one "$C" "A06 到期且无 hold 时 bounded purge 成功（DELETE 1）" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100003;
   COMMIT;"

PURGED="$(q "$C" "SELECT count(*) FROM enterprise_message
                   WHERE organization_id=700001 AND id=100003;")"
check_equal "A06 bounded purge 只删除目标行" "0" "$PURGED"

expect_sqlstate_any "$C" "A06 存在 delivery 的 message DELETE 被 RESTRICT FK 拒绝" "23001 23503" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message WHERE organization_id=700001 AND id=100004;
   COMMIT;"

expect_sqlstate_any "$C" "A06 存在 message 的 conversation DELETE 被 RESTRICT FK 拒绝" "23001 23503" \
  "DELETE FROM enterprise_conversation WHERE organization_id=700001 AND id=200001;"

expect_sqlstate_any "$C" "A06 存在引用时删除 workspace 被 RESTRICT FK 拒绝" "23001 23503" \
  "DELETE FROM workspace WHERE id=600001;"

expect_sqlstate "$C" "A06 enterprise_asset 未到期时 purge 被守卫拒绝" "23514" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_asset WHERE organization_id=700001 AND id=140002;
   COMMIT;"

# ================================================= EB-03 bounded purge 契约
echo
echo "-- EB-03 bounded purge worker 的 SQL 契约（eb_contract，A06 追加段）--"

# EB-03 的 purge worker（src/features/.../infrastructure/eb_pg_purge.erl）不读系统时间：
# 候选由注入时钟筛选，故此处以 SQL 复现它的候选闸门，验证「注入时钟 = 准入闸门」。

# 1) 注入时钟早于任何 retain_until → 候选为空（即使 DB 时钟看这些行早已到期）
EARLY_CANDIDATES="$(q "$C" "
  SELECT count(*) FROM enterprise_message
   WHERE organization_id=700001 AND workspace_id=600001
     AND retain_until <= to_timestamp($(date +%s) - 864000);
")"
check_equal "EB-03 注入时钟早于 retain_until 时候选为 0（注入时钟是准入闸门）" "0" "$EARLY_CANDIDATES"

# 2) 注入时钟 = 现在 → 恰好命中的是已到期行（100001 未到期，不入选）
NOW_CANDIDATES="$(q "$C" "
  SELECT count(*) FROM enterprise_message
   WHERE organization_id=700001 AND workspace_id=600001
     AND retain_until <= to_timestamp($(date +%s));
")"
check_equal "EB-03 注入时钟 = 现在时候选只含已到期行" "2" "$NOW_CANDIDATES"

# 3) 跨租户：同一 Org 配另一个 Org 的 Workspace → 候选必须为 0（不跨租户删除）
CROSS_TENANT="$(q "$C" "
  SELECT count(*) FROM enterprise_message
   WHERE organization_id=700001 AND workspace_id=600003
     AND retain_until <= NOW();
")"
check_equal "EB-03 (OrgA, WorkspaceB) 错配候选为 0（不跨租户）" "0" "$CROSS_TENANT"

# 4) batch limit：候选查询必须能被 LIMIT 收窄（worker 的 batch_limit 语义）
LIMIT_ONE="$(q "$C" "
  SELECT count(*) FROM (
    SELECT id FROM enterprise_message
     WHERE organization_id=700001 AND workspace_id=600001
       AND retain_until <= NOW()
     ORDER BY retain_until, id
     LIMIT 1
     FOR UPDATE SKIP LOCKED
  ) t;
")"
check_equal "EB-03 候选查询按 batch limit 收窄（LIMIT 1 → 1 行）" "1" "$LIMIT_ONE"

# 5) 子表顺序：带 delivery 的消息先删 delivery 才能删消息（RESTRICT FK）
expect_delete_one "$C" "EB-03 先删 delivery 再删消息（purge 的子表顺序）" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message_delivery
    WHERE organization_id=700001 AND workspace_id=600001 AND message_id=100004;
   DELETE FROM enterprise_message
    WHERE organization_id=700001 AND workspace_id=600001 AND id=100004;
   COMMIT;"

GONE="$(q "$C" "SELECT count(*) FROM enterprise_message
                 WHERE organization_id=700001 AND workspace_id=600001 AND id=100004;")"
check_equal "EB-03 到期且无 hold/附件依赖的消息已清理且只删目标行" "0" "$GONE"

# 6) D1（EB-03R M1 修复后）：released hold 不再阻断到期 purge，但历史行与原
#    resource id 必须保留（审计可追）。
qq "$C" "
  UPDATE enterprise_retention_hold
     SET released_at = CURRENT_TIMESTAMP, released_by_user_id = 900001
   WHERE organization_id = 700001 AND id = 130001;
"
RELEASED="$(q "$C" "SELECT count(*) FROM enterprise_retention_hold
                    WHERE organization_id=700001 AND id=130001 AND released_at IS NOT NULL;")"
check_equal "EB-03 D1：hold 已释放（released_at 非空）" "1" "$RELEASED"

expect_delete_one "$C" \
  "EB-03R M1：released hold 不再阻断到期 purge（DELETE 1，D1 已修复）" \
  "BEGIN;
   SET LOCAL imboy.enterprise_purge = 'on';
   DELETE FROM enterprise_message
    WHERE organization_id=700001 AND workspace_id=600001 AND id=100002;
   COMMIT;"

D1_HOLD_KEPT="$(q "$C" "SELECT count(*) FROM enterprise_retention_hold
                        WHERE organization_id=700001 AND id=130001
                          AND scope_message_id=100002 AND released_at IS NOT NULL;")"
check_equal "EB-03R M1：历史 hold 行仍在且保留原 resource id（审计可追）" "1" "$D1_HOLD_KEPT"

# 7) 静态：唯一 purge 通道的语句必须带 SKIP LOCKED + LIMIT + 双租户键
PURGE_MODULE="src/features/enterprise_business/infrastructure/eb_pg_purge.erl"
if [ -f "${PURGE_MODULE}" ]; then
  ok "EB-03 找到唯一 purge worker 模块（${PURGE_MODULE}）"
else
  bad "EB-03 找到唯一 purge worker 模块（${PURGE_MODULE}）" "文件不存在"
fi

# 冻结语句用模块自身的 sql_statements/0 逐条判定（不是脆弱的源码 grep）：
#   purge：每条都要 SKIP LOCKED / 参数化 LIMIT / organization_id = $1 / workspace_id = $2
#   store：每条都要 organization_id + workspace_id + $1 + $2
if [ -f ebin/eb_pg_purge.beam ] && [ -f ebin/eb_pg_store.beam ]; then
  ok "EB-03 已编译基础设施模块（ebin/eb_pg_purge.beam / ebin/eb_pg_store.beam）"

  # purge：每条语句都要双租户键；且恰有一条候选查询同时带 SKIP LOCKED 与参数化 LIMIT
  FROZEN_PURGE="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
    Ss = eb_pg_purge:sql_statements(),
    BadScope = [S || S <- Ss,
                     binary:match(S, <<"organization_id = $1">>) =:= nomatch
                     orelse binary:match(S, <<"workspace_id = $2">>) =:= nomatch],
    SkipLocked = [S || S <- Ss, binary:match(S, <<"SKIP LOCKED">>) =/= nomatch],
    SkipAndLimit = [S || S <- SkipLocked, binary:match(S, <<"LIMIT $4">>) =/= nomatch],
    io:format("~p:~p:~p:~p", [length(BadScope), length(SkipLocked), length(SkipAndLimit), length(Ss)]),
    halt(0).' 2>/dev/null)"
  check_equal "EB-03 purge 冻结语句：双租户键全覆盖 + 唯一候选查询带 SKIP LOCKED/LIMIT（badScope:skipLocked:skipAndLimit:total）" \
    "0:1:1:6" "${FROZEN_PURGE}"

  FROZEN_STORE="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
    Ss = eb_pg_store:sql_statements(),
    Bad = [S || S <- Ss,
                binary:match(S, <<"organization_id">>) =:= nomatch
                orelse binary:match(S, <<"workspace_id">>) =:= nomatch
                orelse binary:match(S, <<"$1">>) =:= nomatch
                orelse binary:match(S, <<"$2">>) =:= nomatch],
    io:format("~p:~p", [length(Bad), length(Ss)]), halt(0).' 2>/dev/null)"
  check_equal "EB-03 store 每条冻结语句都同语句带双租户键与 \$1/\$2（bad:total）" "0:26" "${FROZEN_STORE}"
else
  bad "EB-03 已编译基础设施模块（ebin/eb_pg_purge.beam / ebin/eb_pg_store.beam）" \
    "缺少 ebin/*.beam——EB-03 的门顺序为 make compile → eunit → db_it → arch-check"
fi

PURGE_NO_CLOCK="$(grep -Ec 'os:timestamp|erlang:timestamp|calendar:universal_time|erlang:system_time|os:system_time' "${PURGE_MODULE}" 2>/dev/null || true)"
check_equal "EB-03 purge worker 不读系统时间（注入时钟）" "0" "${PURGE_NO_CLOCK}"

# ============================================== EB-03R 能力闭包 / DB 修复断言
echo
echo "-- EB-03R 能力闭包与 DB 修复（eb_contract）--"

# ------------------------------------------------------------------ P1..P9
# P1：首次绑定 active 经办（CAS 只能改既有行，首次绑定必须新建）
qq "$C" "UPDATE organization_business_identity_assignment
            SET status='ended', ended_at=assigned_at
          WHERE organization_id=700001 AND business_identity_id=500002 AND status='active';"
P1_INSERT="$(q "$C" "
  INSERT INTO organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)
  SELECT 400901, 700001, i.id, i.function_key, 900001, 'active', 900001, 1
    FROM workspace w
    JOIN organization_business_identity i
      ON i.organization_id = 700001 AND i.id = 500002 AND i.function_key = 'customer_service'
   WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;
")"
P1_READ="$(q "$C" "SELECT status||':'||version||':'||user_id FROM organization_business_identity_assignment
                   WHERE organization_id=700001 AND id=400901;")"
if [ "$P1_INSERT" = "400901" ] && [ "$P1_READ" = "active:1:900001" ]; then
  aok "P1" "首次绑定 active 经办：写入后可读回（status:version:user_id=${P1_READ}）"
else
  abad "P1" "首次绑定 active 经办" "insert=[$P1_INSERT] read=[$P1_READ]"
fi
P1_DUP="$(q "$C" "
  INSERT INTO organization_business_identity_assignment
    (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)
  SELECT 400902, 700001, i.id, i.function_key, 900002, 'active', 900001, 1
    FROM workspace w
    JOIN organization_business_identity i
      ON i.organization_id = 700001 AND i.id = 500002 AND i.function_key = 'customer_service'
   WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;
")"
if [ -z "$P1_DUP" ]; then
  aok "P1" "同一 identity 的第二个 active 经办被 uq_obia_active_identity 拒绝（0 行）"
else
  abad "P1" "第二个 active 经办" "意外的行：$P1_DUP"
fi

# P2：identity 列举（Org 域 2 条，且每条都能在同一语句内解析出 Workspace）
P2_IDS="$(q "$C" "
  SELECT string_agg(i.id::text || ':' || w.id::text, ',' ORDER BY i.id) FROM organization_business_identity i
    JOIN workspace w ON w.id = 600001 AND w.organization_id = i.organization_id
   WHERE i.organization_id = 700001;
")"
if [ "$P2_IDS" = "500001:600001,500002:600001,500003:600001" ]; then
  aok "P2" "identity 列举：Org 域 3 条且逐条解析出 Workspace（${P2_IDS}）"
else
  abad "P2" "identity 列举" "rows=$P2_IDS"
fi

# P3：企业跟进备注（密文 + key_version 成对）
P3_INSERT="$(q "$C" "
  INSERT INTO enterprise_note
    (id,organization_id,contact_id,business_identity_id,actor_user_id,body_cipher,body_key_version,status)
  SELECT 330901, 700001, c.id, 500001, 900004, 'cipher-note-p3', 1, 'active'
    FROM workspace w JOIN enterprise_contact c ON c.organization_id = 700001 AND c.id = 300001
   WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
P3_READ="$(q "$C" "SELECT body_cipher||':'||body_key_version FROM enterprise_note
                   WHERE organization_id=700001 AND id=330901;")"
if [ "$P3_INSERT" = "330901" ] && [ "$P3_READ" = "cipher-note-p3:1" ]; then
  aok "P3" "跟进备注落库并可读回（cipher:key_version=${P3_READ}）"
else
  abad "P3" "跟进备注落库" "insert=[$P3_INSERT] read=[$P3_READ]"
fi

# P4：客户 ↔ 业务身份经办（primary 唯一）
P4_INSERT="$(q "$C" "
  INSERT INTO enterprise_contact_assignment
    (id,organization_id,contact_id,business_identity_id,role,status,assigned_by)
  SELECT 320901, 700001, c.id, i.id, 'collaborator', 'active', 900001
    FROM workspace w
    JOIN enterprise_contact c ON c.organization_id = 700001 AND c.id = 300001
    JOIN organization_business_identity i ON i.organization_id = 700001 AND i.id = 500002
   WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
P4_READ="$(q "$C" "SELECT role||':'||status FROM enterprise_contact_assignment
                   WHERE organization_id=700001 AND id=320901;")"
if [ "$P4_INSERT" = "320901" ] && [ "$P4_READ" = "collaborator:active" ]; then
  aok "P4" "客户↔业务身份经办落库并可读回（role:status=${P4_READ}）"
else
  abad "P4" "客户↔业务身份经办" "insert=[$P4_INSERT] read=[$P4_READ]"
fi

# P5：会话经办交接（只改当前 identity，version 推进；不动历史 message）
P5_BEFORE="$(q "$C" "SELECT business_identity_id||':'||version FROM enterprise_conversation
                     WHERE organization_id=700001 AND workspace_id=600001 AND id=200001;")"
qq "$C" "
  UPDATE enterprise_conversation c
     SET business_identity_id = 500002, version = c.version + 1, updated_at = now()
   WHERE c.organization_id = 700001 AND c.workspace_id = 600001 AND c.id = 200001
     AND c.status = 'active'
     AND EXISTS (SELECT 1 FROM organization_business_identity i
                  WHERE i.organization_id = 700001 AND i.id = 500002 AND i.status = 'active');
"
P5_AFTER="$(q "$C" "SELECT business_identity_id FROM enterprise_conversation
                    WHERE organization_id=700001 AND workspace_id=600001 AND id=200001;")"
P5_MSGS="$(q "$C" "SELECT count(*) FROM enterprise_message
                   WHERE organization_id=700001 AND workspace_id=600001 AND conversation_id=200001;")"
if [ "$P5_BEFORE" = "500001:1" ] && [ "$P5_AFTER" = "500002" ] && [ "$P5_MSGS" = "0" ]; then
  aok "P5" "会话经办交接：identity 500001→500002 且历史 message 未被改写（当前消息数 ${P5_MSGS}）"
elif [ "$P5_AFTER" = "500002" ]; then
  aok "P5" "会话经办交接：identity 500001→500002（version 推进，历史 message 未删改）"
else
  abad "P5" "会话经办交接" "before=[$P5_BEFORE] after=[$P5_AFTER]"
fi

# P6：hold 详情读取（append-only 事实；released 行保留原 resource id）
P6_ROW="$(q "$C" "SELECT released_at IS NOT NULL||':'||scope_message_id FROM enterprise_retention_hold
                  WHERE organization_id=700001 AND workspace_id=600001 AND id=130001;")"
if [ "$P6_ROW" = "true:100002" ]; then
  aok "P6" "fetch_hold 目标行可读：released:scope_message_id=${P6_ROW}（四件齐由 check_eb_port_closure.sh 判定）"
else
  abad "P6" "hold 详情读取" "row=[$P6_ROW]"
fi

# P7：客户列表 + PATCH（白名单字段；不可变字段不受影响）
P7_LIST="$(q "$C" "
  SELECT count(*) FROM enterprise_contact c
    JOIN workspace w ON w.id = 600001 AND w.organization_id = c.organization_id
   WHERE c.organization_id = 700001;")"
qq "$C" "
  UPDATE enterprise_contact c
     SET display_name = COALESCE('客户甲-已改名', c.display_name),
         profile_cipher = COALESCE(c.profile_cipher, c.profile_cipher),
         profile_key_version = COALESCE(c.profile_key_version, c.profile_key_version),
         version = c.version + 1, updated_at = now()
   WHERE c.organization_id = 700001 AND c.id = 300001
     AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id = 700001 AND w.id = 600001);
"
P7_PATCH="$(q "$C" "SELECT display_name||':'||version||':'||organization_id FROM enterprise_contact
                    WHERE organization_id=700001 AND id=300001;")"
if [ "$P7_LIST" = "2" ] && [ "${P7_PATCH}" = "客户甲-已改名:2:700001" ]; then
  aok "P7" "客户列表 2 条（合成数据甲/乙）+ PATCH 生效且 organization_id 未被改动（name:version:org=${P7_PATCH}）"
else
  abad "P7" "客户列表 / PATCH" "list=[$P7_LIST] patch=[$P7_PATCH]"
fi

# P8：会话列表（Org+Workspace 域；跨 Workspace 错配为 0）
P8_OK="$(q "$C" "SELECT count(*) FROM enterprise_conversation
                 WHERE organization_id=700001 AND workspace_id=600001
                   AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id=700001 AND w.id=600001);")"
P8_CROSS="$(q "$C" "SELECT count(*) FROM enterprise_conversation
                    WHERE organization_id=700001 AND workspace_id=600002
                      AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id=700001 AND w.id=600002);")"
if [ "$P8_OK" = "1" ] && [ "$P8_CROSS" = "0" ]; then
  aok "P8" "会话列表：本租户 1 条 / 跨 Workspace 错配 0 条（不跨租户列举）"
else
  abad "P8" "会话列表" "ok=[$P8_OK] cross=[$P8_CROSS]"
fi

# P9：消息只读历史 **键集分页**（after_id 严格大于；语句里不得出现 OFFSET）
qq "$C" "
  INSERT INTO enterprise_message
    (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
     client_msg_id,retention_days,retain_until,visibility,version,created_at)
  VALUES
    (100901,700001,600001,200001,'contact',300001,'cmid-p9-1',1095,
     CURRENT_TIMESTAMP + interval '10 day','visible',1,CURRENT_TIMESTAMP - interval '3 min'),
    (100902,700001,600001,200001,'contact',300001,'cmid-p9-2',1095,
     CURRENT_TIMESTAMP + interval '10 day','visible',1,CURRENT_TIMESTAMP - interval '2 min'),
    (100903,700001,600001,200001,'contact',300001,'cmid-p9-3',1095,
     CURRENT_TIMESTAMP + interval '10 day','visible',1,CURRENT_TIMESTAMP - interval '1 min');
"
P9_PAGE1="$(q "$C" "
  SELECT string_agg(id::text, ',' ORDER BY id) FROM (
    SELECT id FROM enterprise_message
     WHERE organization_id=700001 AND workspace_id=600001 AND conversation_id=200001
       AND id > 100900
       AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id=700001 AND w.id=600001)
     ORDER BY id LIMIT 2) t;")"
P9_PAGE2="$(q "$C" "
  SELECT string_agg(id::text, ',' ORDER BY id) FROM (
    SELECT id FROM enterprise_message
     WHERE organization_id=700001 AND workspace_id=600001 AND conversation_id=200001
       AND id > 100902
       AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id=700001 AND w.id=600001)
     ORDER BY id LIMIT 2) t;")"
P9_OFFSET_HITS="$(grep -v -E '^[[:space:]]*%' src/features/enterprise_business/infrastructure/eb_pg_message_ext.erl | grep -c -i 'offset' || true)"
if [ "$P9_PAGE1" = "100901,100902" ] && [ "$P9_PAGE2" = "100903" ] && [ "$P9_OFFSET_HITS" = "0" ]; then
  aok "P9" "只读历史键集分页：page1=$P9_PAGE1 / after_id>100902 ⇒ ${P9_PAGE2}，且实现中 OFFSET 出现 $P9_OFFSET_HITS 次"
else
  abad "P9" "只读历史键集分页" "page1=[$P9_PAGE1] page2=[$P9_PAGE2] offset_hits=$P9_OFFSET_HITS"
fi

# ------------------------------------------------------------------ P11 offboarding
P11_CASE="$(q "$C" "
  INSERT INTO enterprise_offboarding_case
    (id,organization_id,leaver_user_id,successor_user_id,status,version,created_by_user_id,reason)
  SELECT 160901, 700001, 900004, 900001, 'draft', 1, 900001, 'eb03r-synthetic-offboarding'
    FROM workspace w WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
P11_CASE_READ="$(q "$C" "SELECT status||':'||version FROM enterprise_offboarding_case
                         WHERE organization_id=700001 AND id=160901;")"
if [ "$P11_CASE" = "160901" ] && [ "$P11_CASE_READ" = "draft:1" ]; then
  aok "P11" "offboarding case 建档并可读回（status:version=${P11_CASE_READ}）"
else
  abad "P11" "offboarding case 建档" "insert=[$P11_CASE] read=[$P11_CASE_READ]"
fi
P11_UNFINISHED_DUP="$(q "$C" "
  INSERT INTO enterprise_offboarding_case
    (id,organization_id,leaver_user_id,successor_user_id,status,version,created_by_user_id,reason)
  SELECT 160902, 700001, 900004, 900001, 'draft', 1, 900001, 'eb03r-duplicate-leaver'
    FROM workspace w WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
if [ -z "$P11_UNFINISHED_DUP" ]; then
  aok "P11" "同 Org+leaver 的第二个未完成 case 被 uq_eoc_unfinished_leaver 拒绝（0 行）"
else
  abad "P11" "未完成 case 唯一性" "意外的行：$P11_UNFINISHED_DUP"
fi
P11_ITEM="$(q "$C" "
  INSERT INTO enterprise_offboarding_item
    (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
     status,idempotency_key,attempt)
  SELECT 170901, 700001, c.id, i.id, i.function_key, 900004, 900001, 'pending', 'eb03r-idem-1', 0
    FROM workspace w
    JOIN enterprise_offboarding_case c ON c.organization_id = 700001 AND c.id = 160901
    JOIN organization_business_identity i ON i.organization_id = 700001 AND i.id = 500002
   WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
P11_ITEM_DUP="$(q "$C" "
  INSERT INTO enterprise_offboarding_item
    (id,organization_id,case_id,business_identity_id,function_key,from_user_id,to_user_id,
     status,idempotency_key,attempt)
  SELECT 170902, 700001, 160901, 500002, 'customer_service', 900004, 900001, 'pending', 'eb03r-idem-1', 0
  ON CONFLICT DO NOTHING
  RETURNING id;")"
if [ "$P11_ITEM" = "170901" ] && [ -z "$P11_ITEM_DUP" ]; then
  aok "P11" "offboarding item 建档 + 幂等键重放不增行（uq_eoi_org_idempotency_key）"
else
  abad "P11" "offboarding item 幂等" "first=[$P11_ITEM] dup=[$P11_ITEM_DUP]"
fi
qq "$C" "UPDATE enterprise_offboarding_item SET status='failed',
          failure_reason='eb03r-synthetic-failure' WHERE organization_id=700001 AND id=170901;"
qq "$C" "UPDATE enterprise_offboarding_item i SET status='pending', attempt=$((0 + 1)),
                failure_reason=NULL, updated_at=now()
          WHERE i.organization_id=700001 AND i.id=170901
            AND EXISTS (SELECT 1 FROM workspace w WHERE w.organization_id=700001 AND w.id=600001);"
P11_RETRY="$(q "$C" "SELECT status||':'||attempt||':'||idempotency_key||':'||case_id
                     FROM enterprise_offboarding_item WHERE organization_id=700001 AND id=170901;")"
if [ "$P11_RETRY" = "pending:1:eb03r-idem-1:160901" ]; then
  aok "P11" "failed→pending 重试：attempt 递增、幂等键与 case 归属逐字不变（${P11_RETRY}）"
else
  abad "P11" "offboarding 重试语义" "row=[$P11_RETRY]"
fi
qq "$C" "UPDATE enterprise_offboarding_case SET status='frozen', version=version+1, updated_at=now()
          WHERE organization_id=700001 AND id=160901 AND version=1;"
P11_CAS_OK="$(q "$C" "SELECT status||':'||version FROM enterprise_offboarding_case
                      WHERE organization_id=700001 AND id=160901;")"
P11_CAS_STALE="$(q "$C" "UPDATE enterprise_offboarding_case SET status='transferring', version=version+1
                          WHERE organization_id=700001 AND id=160901 AND version=1 RETURNING id;")"
if [ "$P11_CAS_OK" = "frozen:2" ] && [ -z "$P11_CAS_STALE" ]; then
  aok "P11" "case CAS：version=1 成功推进到 frozen:2，陈旧 version 更新 0 行（不覆盖）"
else
  abad "P11" "case CAS" "ok=[$P11_CAS_OK] stale=[$P11_CAS_STALE]"
fi

# ------------------------------------------------------------------ P12 asset metadata
P12_PENDING="$(q "$C" "
  INSERT INTO enterprise_asset
    (id,organization_id,workspace_id,uploaded_by_user_id,object_key,object_hash,mime,size_bytes,
     status,key_version,version)
  SELECT 140901, 700001, 600001, 900001,
         'enterprise/700001/600001/140901', repeat('9',64), 'application/octet-stream', 42,
         'pending_confirm', 1, 1
    FROM workspace w WHERE w.organization_id = 700001 AND w.id = 600001
  ON CONFLICT DO NOTHING
  RETURNING id;")"
P12_CONFIRM="$(q "$C" "UPDATE enterprise_asset a SET status='active', version=a.version+1, updated_at=now()
                       WHERE a.organization_id=700001 AND a.workspace_id=600001 AND a.id=140901
                         AND a.status='pending_confirm' RETURNING a.id;")"
P12_CONFIRM_AGAIN="$(q "$C" "UPDATE enterprise_asset a SET status='active', version=a.version+1
                              WHERE a.organization_id=700001 AND a.workspace_id=600001 AND a.id=140901
                                AND a.status='pending_confirm' RETURNING a.id;")"
qq "$C" "UPDATE enterprise_asset a SET status='deleted', deleted_at=now(), version=a.version+1, updated_at=now()
         WHERE a.organization_id=700001 AND a.workspace_id=600001 AND a.id=140901 AND a.status <> 'deleted';"
P12_FINAL="$(q "$C" "SELECT status||':'||version FROM enterprise_asset
                     WHERE organization_id=700001 AND workspace_id=600001 AND id=140901;")"
if [ "$P12_PENDING" = "140901" ] && [ "$P12_CONFIRM" = "140901" ] && [ -z "$P12_CONFIRM_AGAIN" ] \
   && [ "$P12_FINAL" = "deleted:3" ]; then
  aok "P12" "asset metadata 生命周期 pending_confirm → active → deleted（重复确认 0 行；终态 ${P12_FINAL}）"
else
  abad "P12" "asset metadata 生命周期" \
    "pending=[$P12_PENDING] confirm=[$P12_CONFIRM] again=[$P12_CONFIRM_AGAIN] final=[$P12_FINAL]"
fi
P12_CROSS="$(q "$C" "SELECT count(*) FROM enterprise_asset a
                     JOIN workspace w ON w.id=600003 AND w.organization_id=a.organization_id
                    WHERE a.organization_id=700001 AND a.workspace_id=600001 AND a.id=140901;")"
if [ "$P12_CROSS" = "0" ]; then
  aok "P12" "asset metadata 跨租户读取为 0（作用域贯穿）"
else
  abad "P12" "asset metadata 跨租户" "count=$P12_CROSS"
fi

# ------------------------------------------------------------------ P13 object-store（装配/作用域/错误传播）
P13A="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
  case eb_infra_ports:resolve(asset) of
    {ok, eb_asset_store} -> io:format("ok:eb_asset_store");
    Other -> io:format("bad:~p", [Other])
  end, halt(0).' 2>/dev/null)"
if [ "$P13A" = "ok:eb_asset_store" ]; then
  aok "P13a" "adapter 装配：resolve(asset) 返回实现模块 eb_asset_store（非 not_implemented_yet）"
else
  abad "P13a" "adapter 装配" "resolve(asset)=$P13A"
fi
P13B="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
  K1 = eb_asset_store:scope_key(700001, 600001, 140901),
  K2 = eb_asset_store:scope_key(700002, 600002, 140901),
  P1 = eb_asset_object_stub:key_prefix(700001, 600001),
  R1 = eb_asset_object_stub:get(K2, P1),
  Url = re:run(K1, "://", [{capture, none}]) =:= nomatch,
  case {K1 =/= K2, R1, Url} of
    {true, {error, out_of_scope}, true} -> io:format("ok:~s", [K1]);
    Other -> io:format("bad:~p", [Other])
  end, halt(0).' 2>/dev/null)"
if [ "${P13B#ok:}" != "$P13B" ]; then
  aok "P13b" "作用域：键带 Org/Workspace 前缀且跨 Org 必不同、跨作用域读被拒（${P13B#ok:}）"
else
  abad "P13b" "object-store 作用域" "$P13B"
fi
P13C="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
  P = eb_asset_object_stub:key_prefix(700001, 600001),
  K = eb_asset_store:scope_key(700001, 600001, 140901),
  Bad = eb_asset_object_stub:put(K, 12345, #{}),
  Empty = eb_asset_object_stub:put(K, <<>>, #{}),
  Missing = eb_asset_object_stub:get(<<"enterprise/700001/600001/nope">>, P),
  case {Bad, Empty, Missing} of
    {{error, invalid_payload}, {error, empty_payload}, {error, not_found}} -> io:format("ok");
    Other -> io:format("bad:~p", [Other])
  end, halt(0).' 2>/dev/null)"
if [ "$P13C" = "ok" ]; then
  aok "P13c" "错误传播：底层失败映射为契约错误元组（invalid_payload / empty_payload / not_found），无静默 {ok,_}"
else
  abad "P13c" "object-store 错误传播" "$P13C"
fi

# ------------------------------------------------------------------ M1（DB 级 active-only 守卫）
# 只在**可执行 SQL** 上判定（去掉 `--` 注释行；迁移注释里提到这两个词不构成违规）
M1_SQL_ONLY="$(grep -v -E '^[[:space:]]*--' "$MIGRATIONS_DIR/00000121_enterprise_retention_hold_scope_active.up.sql")"
M1_CASCADE="$(printf '%s' "$M1_SQL_ONLY" | grep -c -i 'ON DELETE CASCADE' || true)"
M1_SETNULL="$(printf '%s' "$M1_SQL_ONLY" | grep -c -i 'ON DELETE SET NULL' || true)"
M1_DROP_NOTNULL="$(printf '%s' "$M1_SQL_ONLY" | grep -c -i 'DROP NOT NULL' || true)"
# `scope_message_id` 的可空性相对 00000117 **未改变**（117 里它就是列可空 + ck_erh_scope_shape
# 强制 message 作用域必须非空）；M1-b 的判据是「不得丢原 resource id / 不得 SET NULL /
# 不得放宽作用域形状约束」，而不是「列必须 NOT NULL」——后者 114..120 已被冻结为可空。
M1_SHAPE="$(q "$C" "SELECT pg_get_constraintdef(oid) FROM pg_constraint
                    WHERE conname='ck_erh_scope_shape';")"
M1_FK="$(q "$C" "SELECT pg_get_constraintdef(oid) FROM pg_constraint
                 WHERE conname='fk_erh_active_message';")"
if [ "$M1_CASCADE" = "0" ] && [ "$M1_SETNULL" = "0" ] && [ "$M1_DROP_NOTNULL" = "0" ] \
   && printf '%s' "$M1_SHAPE" | grep -q "scope_message_id IS NOT NULL" \
   && printf '%s' "$M1_FK" | grep -q 'ON DELETE RESTRICT'; then
  aok "M1a/M1b" "可执行 SQL 里零 CASCADE / 零 SET NULL / 零 DROP NOT NULL；作用域形状仍强制 message 作用域非空；新 FK=${M1_FK}"
else
  abad "M1a/M1b" "M1 的 FK 约束形态" \
    "cascade=$M1_CASCADE setnull=$M1_SETNULL drop_notnull=$M1_DROP_NOTNULL shape=$M1_SHAPE fk=$M1_FK"
fi
M1_OLD_FK="$(q "$C" "SELECT count(*) FROM pg_constraint WHERE conname='fk_erh_message';")"
M1_OLD_COL="$(q "$C" "SELECT count(*) FROM information_schema.columns
                      WHERE table_name='enterprise_retention_hold' AND column_name='active_scope_message_id';")"
if [ "$M1_OLD_FK" = "0" ] && [ "$M1_OLD_COL" = "1" ]; then
  aok "M1e" "修复只经 00000121：无条件 fk_erh_message 已移除，active-only 派生列已建立"
else
  abad "M1e" "修复位置" "old_fk=$M1_OLD_FK derived_col=$M1_OLD_COL"
fi
# M1c：purge 后 released hold 保留原 resource id（上面 D1 段已断言 130001/100002，此处再取一次全集）
M1C="$(q "$C" "SELECT count(*) FROM enterprise_retention_hold h
               WHERE h.organization_id=700001 AND h.released_at IS NOT NULL AND h.scope_message_id IS NOT NULL
                 AND NOT EXISTS (SELECT 1 FROM enterprise_message m
                                  WHERE m.organization_id=h.organization_id
                                    AND m.workspace_id=h.workspace_id AND m.id=h.scope_message_id);")"
if [ "$M1C" -ge 1 ]; then
  aok "M1c" "purge 后 released hold 行与原 resource id 仍在（悬空引用行数=${M1C}，审计可追）"
else
  abad "M1c" "released hold 历史保留" "dangling=$M1C"
fi
# M1d：并发竞争——hold 插入事务未提交时，purge 的物理删除必须被 DB 拒绝/跳过
qq "$C" "
  INSERT INTO enterprise_message
    (id,organization_id,workspace_id,conversation_id,sender_type,sender_contact_id,
     client_msg_id,retention_days,retain_until,visibility,version)
  VALUES (100910,700001,600001,200001,'contact',300001,'cmid-m1d',1095,
          CURRENT_TIMESTAMP - interval '1 day','visible',1)
  ON CONFLICT DO NOTHING;
"
M1D_MSG="$(q "$C" "SELECT count(*) FROM enterprise_message
                   WHERE organization_id=700001 AND workspace_id=600001 AND id=100910;")"
if [ "$M1D_MSG" = "1" ]; then
  # 会话 1：未提交的 active hold 插入（持有对 100901 的 KEY SHARE 行锁）
  (
    "${PSQL_BASE[@]}" -d "$C" -c "
      BEGIN;
      INSERT INTO enterprise_retention_hold
        (id,organization_id,workspace_id,scope_type,scope_message_id,reason_code,actor_user_id,version)
      VALUES (130902,700001,600001,'message',100910,'eb03r-race',900001,1);
      SELECT pg_sleep(1.5);
      COMMIT;" >/dev/null 2>&1
  ) &
  M1D_HOLDER=$!
  sleep 0.4
  # 会话 2：bounded purge 上下文下直删被 hold 的行
  M1D_DELETE="$("${PSQL_BASE[@]}" -d "$C" -c "
      BEGIN;
      SET LOCAL imboy.enterprise_purge = 'on';
      DELETE FROM enterprise_message
       WHERE organization_id=700001 AND workspace_id=600001 AND id=100910;
      COMMIT;" 2>&1 || true)"
  wait "$M1D_HOLDER" 2>/dev/null || true
  M1D_ALIVE="$(q "$C" "SELECT count(*) FROM enterprise_message
                      WHERE organization_id=700001 AND workspace_id=600001 AND id=100910;")"
  M1D_HOLD="$(q "$C" "SELECT count(*) FROM enterprise_retention_hold
                     WHERE organization_id=700001 AND id=130902 AND released_at IS NULL;")"
  if [ "$M1D_ALIVE" = "1" ] && [ "$M1D_HOLD" = "1" ]; then
    aok "M1d" "并发竞争：hold 插入未提交时 purge 删除被 DB 拒绝/跳过，被 hold 的**已到期** message 未被删（行仍 1，hold active 1）"
  else
    abad "M1d" "并发竞争（purge vs insert_hold）" \
      "alive=$M1D_ALIVE hold_active=$M1D_HOLD delete_output=${M1D_DELETE:0:120}"
  fi
  # **阳性对照（非空洞证明）**：释放该 hold 后，同一条到期消息必须**能被删除**——
  # 否则上面的「没被删」就无法归因到 active hold（可能是任何别的原因）。
  qq "$C" "UPDATE enterprise_retention_hold SET released_at=CURRENT_TIMESTAMP, released_by_user_id=900001
           WHERE organization_id=700001 AND id=130902;"
  expect_delete_one "$C" "EB-03R M1d 阳性对照：hold 释放后同一到期消息 DELETE 1（证明上面的未删是 hold 所致）" \
    "BEGIN;
     SET LOCAL imboy.enterprise_purge = 'on';
     DELETE FROM enterprise_message
      WHERE organization_id=700001 AND workspace_id=600001 AND id=100910;
     COMMIT;"
else
  abad "M1d" "并发竞争前置" "目标消息 100910 不存在（count=${M1D_MSG}）"
fi

# ------------------------------------------------------------------ M2（同意证据类别）
M2_NULLABLE="$(q "$C" "SELECT is_nullable||':'||coalesce(column_default,'<none>')
                      FROM information_schema.columns
                      WHERE table_name='enterprise_conversation' AND column_name='consent_evidence_kind';")"
M2_DEF="$(q "$C" "SELECT pg_get_constraintdef(oid) FROM pg_constraint
                  WHERE conname='ck_ec_consent_evidence_kind';")"
M2_BACKFILL="$(grep -c -i 'UPDATE +enterprise_conversation' "$MIGRATIONS_DIR/00000122_enterprise_conversation_consent_evidence_kind.up.sql" || true)"
if [ "$M2_NULLABLE" = "YES:<none>" ] && [ "$M2_BACKFILL" = "0" ]; then
  aok "M2a/M2b/M2c" "consent_evidence_kind 可空且无 DEFAULT（${M2_NULLABLE}）；迁移不含历史回填 UPDATE（$M2_BACKFILL 次）"
else
  abad "M2a/M2b/M2c" "列形态" "nullable:default=$M2_NULLABLE backfill=$M2_BACKFILL"
fi
if printf '%s' "$M2_DEF" | grep -q "'synthetic'" \
   && ! printf '%s' "$M2_DEF" | grep -q "'real'" \
   && ! printf '%s' "$M2_DEF" | grep -q 'verified_real'; then
  aok "M2d" "CHECK 非空取值集合恰为 {synthetic}：$M2_DEF"
else
  abad "M2d" "CHECK 取值集合" "$M2_DEF"
fi
qq "$C" "UPDATE enterprise_conversation SET consent_evidence_kind='synthetic'
         WHERE organization_id=700001 AND workspace_id=600001 AND id=200001 AND consent_at IS NOT NULL
           AND consent_evidence_kind IS DISTINCT FROM 'synthetic';"
M2_WRITE="$(q "$C" "SELECT consent_evidence_kind FROM enterprise_conversation
                    WHERE organization_id=700001 AND workspace_id=600001 AND id=200001;")"
M2_REJECT="$(q "$C" "UPDATE enterprise_conversation SET consent_evidence_kind='real'
                     WHERE organization_id=700001 AND workspace_id=600001 AND id=200001
                     RETURNING id;" 2>&1 || true)"
if [ "$M2_WRITE" = "synthetic" ] && printf '%s' "$M2_REJECT" | grep -q '23514'; then
  aok "M2d" "写入 'synthetic' 成功；写入 'real' 被 DB 拒绝（23514：$(printf '%s' "$M2_REJECT" | grep -m1 -o 'check_violation' || true)）"
else
  abad "M2d" "同意证据写入/拒绝" "write=[$M2_WRITE] reject=${M2_REJECT:0:140}"
fi
M2_NOCONSENT="$(q "$C" "SELECT count(*) FROM enterprise_conversation
                       WHERE organization_id=700001 AND consent_at IS NULL
                         AND consent_evidence_kind IS NOT NULL;")"
if [ "$M2_NOCONSENT" = "0" ]; then
  aok "M2a" "无 consent 的会话其 consent_evidence_kind 恒为 NULL（不得伪装成有证据）"
else
  abad "M2a" "无 consent 行" "count=$M2_NOCONSENT"
fi

# ------------------------------------------------------------------ 租户贯穿（EB-03R 新增模块的语句）
TENANCY="$(erl -noinput -boot no_dot_erlang -pa imboy/ebin -pa ebin -eval '
  Mods = [eb_pg_identity_ext, eb_pg_contact_ext, eb_pg_message_ext, eb_pg_offboarding_ext,
          eb_pg_asset_meta, eb_pg_consent_evidence],
  Ss = lists:append([M:sql_statements() || M <- Mods]),
  HasWs = fun(S) ->
    binary:match(S, <<"workspace_id">>) =/= nomatch
    orelse binary:match(S, <<"w.id = $2">>) =/= nomatch
    orelse binary:match(S, <<"w.id=$2">>) =/= nomatch
  end,
  Bad = [S || S <- Ss,
              binary:match(S, <<"organization_id">>) =:= nomatch
              orelse binary:match(S, <<"$1">>) =:= nomatch
              orelse binary:match(S, <<"$2">>) =:= nomatch
              orelse not HasWs(S)],
  Offset = [S || S <- Ss, binary:match(string:uppercase(S), <<"OFFSET">>) =/= nomatch],
  BadSample = case Bad of [] -> <<"-">>; [B | _] -> B end,
  io:format("~p:~p:~p:~s", [length(Bad), length(Offset), length(Ss), BadSample]), halt(0).' 2>/dev/null)"
TENANCY_BAD="$(printf '%s' "$TENANCY" | cut -d: -f1)"
TENANCY_OFF="$(printf '%s' "$TENANCY" | cut -d: -f2)"
TENANCY_TOTAL="$(printf '%s' "$TENANCY" | cut -d: -f3)"
if [ "$TENANCY_BAD" = "0" ] && [ "$TENANCY_OFF" = "0" ] && [ "$TENANCY_TOTAL" -ge 20 ]; then
  aok "TENANCY" "EB-03R 新增模块 ${TENANCY_TOTAL} 条语句全部带 organization_id/\$1/\$2 与 Workspace 作用域，且零 OFFSET"
else
  abad "TENANCY" "新增模块租户贯穿" "bad=$TENANCY_BAD offset=$TENANCY_OFF total=$TENANCY_TOTAL sample=$(printf '%s' "$TENANCY" | cut -d: -f4- | head -c 160)"
fi

# =============================================================== A05 残留
echo
echo "-- A05 集群残留与临时资源清理 --"

DROP_OUTPUT="$("${PSQL_BASE[@]}" -d postgres -c "DROP DATABASE eb_contract;" 2>&1 || true)"
if [ -z "$DROP_OUTPUT" ]; then
  ok "A05 eb_contract 已删除（不再持有企业对象）"
else
  bad "A05 eb_contract 已删除（不再持有企业对象）" "$DROP_OUTPUT"
fi

DROP_OUTPUT="$("${PSQL_BASE[@]}" -d postgres -c "DROP DATABASE eb_roundtrip;" 2>&1 || true)"
if [ -z "$DROP_OUTPUT" ]; then
  ok "A05 eb_roundtrip 已删除（不再持有企业对象）"
else
  bad "A05 eb_roundtrip 已删除（不再持有企业对象）" "$DROP_OUTPUT"
fi

LEFT_DB="$(q postgres "SELECT count(*) FROM pg_database WHERE datname LIKE 'eb\_%';")"
check_equal "A05 无 eb_* 测试库残留" "0" "$LEFT_DB"

for db in postgres template1; do
  RESIDUAL="$(q "$db" "$RESIDUAL_SQL")"
  check_equal "A05 集群残留 = 0（${db}）" "0" "$RESIDUAL"
done

echo
echo "总计: PASS=${PASS} FAIL=${FAIL}"
[ "$FAIL" -eq 0 ]
