#!/usr/bin/env bash
# csww_e2e_seed.sh — CSWW Playwright real/p2 种子重建（幂等；数据面 imboy_test_v1）
#
# 结构性缺口修复：csww_20260920T051447Z 种子库与 csww_e2e_chain.sh 配方原先不在
# 任何仓，Playwright real/p2 全阻塞。本脚本把「种子事实清单」收进仓内，可重复执行。
#
# 种子事实清单来源（固定值逐字对齐，spec 侧硬编码不可 env 覆盖）：
#   admin/tests/e2e/customer-service-real/helpers/env.ts
#     ORG_ID=1603940848519155 / WORKSPACE_ID=1776025844701027
#     INSTALLATION_ID=5837897154422619 / PUBLIC_WIDGET_ID=csww-e2e-widget-77729338
#     OTHER_ORG_ID=1593173991415380 / OTHER_INSTALLATION_ID=9460771712205608
#     OTHER_PUBLIC_WIDGET_ID=csww-chain-1760383326（跨安装负例）
#     SEAT_A.identityId=1812620870393267 / SEAT_B.identityId=8304657945488544
#     SEAT_A(19900000001) / SEAT_B(19900000002)（合成坐席，DB 直种，A=org owner）
#     seatLogin()：POST /api/v1/passport/login（rsa_encrypt:"0" 明文沙盒口径）
#   customer-service-p2/helpers/env.ts 与 real 共用同一套 ORG/INSTALLATION/SEAT
#   固定值，PG 库名经 CSWW_P2_PG_DB 注入（本脚本导出，见输出段）。
#   helpers/agent-api.ts：queue/claim/detail/transfer/close/reply/ack 全链依赖
#     坐席事实链 = organization_member(active) + organization_business_identity
#     (customer_service,active) + _assignment(active) + customer_service_seat(enabled)
#
# 实现口径：
#   * 基础事实（org/workspace/user/member）用 psql 幂等 INSERT（imboyapp/scripts/
#     setup_test_data.sh 同款惯例）。
#   * id 全部固定为 env.ts 硬编码值（跨 run 稳定 → CSWW_* 注入段在重复执行时
#     逐字节不变）。user id / assignment id 不被 env.ts 引用，沿用 v3run 稳定段
#     （17000000000001xx，16 位 TSID 形态）。
#   * 坐席 identity/assignment 用 psql 幂等 upsert 固定 id 预插（与生产列口径
#     一致，provision 无法指定 id）；同 org 同 user 的旧 active assignment 先行
#     退役（partial unique 让位 + 保证 provision_seat 只命中固定 identity 行）。
#     随后仍走生产代码路径 customer_service_facade:provision_seat/2 复用该
#     identity 补建 seat（幂等：重复调用返回既有事实）。
#   * widget installation 用 psql 幂等 upsert 固定 id（生产 admin API 无法指定
#     id/public_widget_id；列口径对齐 cs_pg_widget 插入语句），allowed_origins
#     同时覆盖 P1 HOST_ORIGIN 与 P2 shop origin。
#   * 排队配置：CSW 会话默认 visitor 发起即 queued，容量即坐席 max_concurrent
#     （provision_seat 参数），无独立排队配置表种子事实（如后续合同新增，
#     在本脚本 FACTS 段补行）。
#
# 前置：后端 9802 已运行（种子经其生产代码路径写 seat）、docker 容器 imboy_pg18
#       （PG 4323；db-proof.ts 经 docker exec 读同一实例）、psql 客户端。
# 输出：实体 id 清单 + CSWW_E2E_* / CSWW_P2_* export 注入段（stdout；"export 行"
#       以 ----CSWW_E2E_ENV---- 分隔便于上游 eval $(...) 拼接）。
# 退出码：0=就绪；非 0=前置缺失或种子失败（失败处如实报错）。
#
# 用法：bash scripts/test/csww_e2e_seed.sh
set -euo pipefail

STATE_ROOT="${CSWW_STATE_ROOT:-/Users/leeyi/project/imboy.pub/.task-state/imboy-garage-direct-v1}"
BACKEND_WT="${CSWW_BACKEND_WT:-/Users/leeyi/project/imboy.pub/.worktrees/imboy-garage-direct-v1/backend}"
REL_BIN="${BACKEND_WT}/_rel/imboy/bin/imboy"
BACKEND_BASE="${CSWW_E2E_BACKEND:-http://127.0.0.1:9802}"
HOST_ORIGIN="${CSWW_E2E_HOST_ORIGIN:-http://localhost:8901}"
P2_SHOP_ORIGIN="${CSWW_P2_SHOP_ORIGIN:-https://shop.test:8943}"
P2_ADMIN_ORIGIN="${CSWW_P2_ADMIN_ORIGIN:-https://admin.test:8943}"
PG_HOST="${PGHOST:-127.0.0.1}"
PG_PORT="${PGPORT:-4323}"
PG_USER="${PGUSER:-imboy_user}"
PG_PASSWORD="${PGPASSWORD:-abc54321}"
PG_DATABASE="${PGDATABASE:-imboy_test_v1}"
PG_CONTAINER="${PG_CONTAINER:-imboy_pg18}"

# ── 固定 id（与 env.ts 逐字一致 = 幂等输出稳定）──────────────────────────
ORG_ID=1603940848519155              # env.ts ORG_ID
WS_ID=1776025844701027               # env.ts WORKSPACE_ID
ORG_OTHER_ID=1593173991415380        # env.ts OTHER_ORG_ID
WS_OTHER_ID=1700000000000202         # env.ts 不引用；v3run 稳定段
INST_ID=5837897154422619             # env.ts INSTALLATION_ID
PUB_WIDGET='csww-e2e-widget-77729338'          # env.ts PUBLIC_WIDGET_ID
INST_OTHER_ID=9460771712205608       # env.ts OTHER_INSTALLATION_ID
PUB_WIDGET_OTHER='csww-chain-1760383326'       # env.ts OTHER_PUBLIC_WIDGET_ID
SEAT_A_IDENTITY_ID=1812620870393267  # env.ts SEAT_A.identityId
SEAT_B_IDENTITY_ID=8304657945488544  # env.ts SEAT_B.identityId
SEAT_A_UID=1700000000000111          # env.ts 不引用 user id；账号固定 19900000001
SEAT_B_UID=1700000000000112          # env.ts 不引用 user id；账号固定 19900000002
ASSIGN_A_ID=1700000000000131         # assignment id env.ts 不引用；v3run 稳定段
ASSIGN_B_ID=1700000000000132
SEAT_A_ACCOUNT=19900000001
SEAT_B_ACCOUNT=19900000002
SEAT_PASSWORD='CswwE2e2026'   # 仅合成测试账号；与 env.ts SEAT_PASSWORD 同值
ADMIN_ACTOR_ID=121439036125743104   # provision 审计 actor 传参（provision 内部
                                    # 对 created_by 类列 nullify，不落 user FK）

log() { printf '[csww-seed] %s\n' "$*"; }
die() { printf '[csww-seed] FAIL: %s\n' "$*" >&2; exit 1; }

# ── 前置 ────────────────────────────────────────────────────────────────
[ -x "${REL_BIN}" ] || die "release 未构建: ${REL_BIN}"
"${REL_BIN}" ping >/dev/null 2>&1 || die "后端未运行（需 9802 在跑，种子经生产代码路径写入）"
command -v psql >/dev/null 2>&1 || die "缺 psql 客户端（brew install libpq）"

psql_run() {
  PGPASSWORD="${PG_PASSWORD}" psql -h "${PG_HOST}" -p "${PG_PORT}" -U "${PG_USER}" \
    -d "${PG_DATABASE}" -v ON_ERROR_STOP=1 -q -t "$@"
}

psql_run -c "SELECT 1" >/dev/null 2>&1 || die "PG 不可达: ${PG_HOST}:${PG_PORT}/${PG_DATABASE}"

# ── 1) 密码哈希（生产模块生成；与 login verify 同链；ctl eval 输出经文件桥）──
"${REL_BIN}" eval "ok = file:write_file(\"/tmp/csww-seed-pwd.txt\", elib_password:generate(<<\"${SEAT_PASSWORD}\">>)), ok." >/dev/null 2>&1 \
  || die "elib_password:generate eval 执行失败"
PWD_HASH="$(cat /tmp/csww-seed-pwd.txt 2>/dev/null | tr -d '\r\n')"
[ -n "${PWD_HASH}" ] && [ ${#PWD_HASH} -gt 20 ] || die "elib_password:generate 失败"

# ── 2) 基础事实幂等 INSERT（org / workspace / user / member）─────────────
# user 表必填无默认列：id/password/account/reg_ip/reg_cosv（setup_test_data 同款）；
# organization/workspace 必填 owner_id；member 主键 (organization_id,user_id) 无 id 列；
# owner 唯一约束（partial unique）：A=owner，B=member。
psql_run <<SQL
INSERT INTO public."user" (id, password, account, mobile, reg_ip, reg_cosv, nickname, status)
VALUES (${SEAT_A_UID}, '${PWD_HASH}', '${SEAT_A_ACCOUNT}', '${SEAT_A_ACCOUNT}', '127.0.0.1', 'v3run', 'csww-seat-a', 1),
       (${SEAT_B_UID}, '${PWD_HASH}', '${SEAT_B_ACCOUNT}', '${SEAT_B_ACCOUNT}', '127.0.0.1', 'v3run', 'csww-seat-b', 1)
ON CONFLICT (id) DO UPDATE SET password = EXCLUDED.password, mobile = EXCLUDED.mobile;
INSERT INTO public.organization (id, name, owner_id, status)
VALUES (${ORG_ID}, 'csww-e2e-org', ${SEAT_A_UID}, 'active'),
       (${ORG_OTHER_ID}, 'csww-e2e-org-other', ${SEAT_A_UID}, 'active')
ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name;
INSERT INTO public.workspace (id, organization_id, name, owner_id, status)
VALUES (${WS_ID}, ${ORG_ID}, 'csww-e2e-ws', ${SEAT_A_UID}, 'active'),
       (${WS_OTHER_ID}, ${ORG_OTHER_ID}, 'csww-e2e-ws-other', ${SEAT_A_UID}, 'active')
ON CONFLICT (id) DO UPDATE SET name = EXCLUDED.name;
INSERT INTO public.organization_member (organization_id, user_id, role, status)
VALUES (${ORG_ID}, ${SEAT_A_UID}, 'owner', 'active'),
       (${ORG_ID}, ${SEAT_B_UID}, 'member', 'active')
ON CONFLICT (organization_id, user_id) DO UPDATE SET status = 'active', role = EXCLUDED.role;
INSERT INTO public.user_device (id, user_id, device_id, login_count, last_login_ip, status)
VALUES (${SEAT_A_UID}, ${SEAT_A_UID}, 'csww-device-a', 1, '127.0.0.1', 1),
       (${SEAT_B_UID}, ${SEAT_B_UID}, 'csww-device-b', 1, '127.0.0.1', 1)
ON CONFLICT (id) DO UPDATE SET status = 1;
SQL
log "基础事实就绪（org=${ORG_ID}/${ORG_OTHER_ID} ws=${WS_ID}/${WS_OTHER_ID} users=${SEAT_A_UID}/${SEAT_B_UID}）"

# ── 3) 坐席身份链固定 id 预插（psql 幂等；provision 无法指定 id）──────────
# identity/assignment 列口径对齐 cs_pg_seat:provision_tx 生产插入语句；
# 旧行退役两个目的：(a) partial unique uq_obia_active_user_function 让位；
# (b) 保证 provision_seat 的 FIND_IDENTITY 只命中固定 identity 行。
psql_run <<SQL
UPDATE organization_business_identity_assignment
   SET status = 'ended', ended_at = now() at time zone 'utc',
       end_reason = 'csww-e2e-superseded', version = version + 1
 WHERE organization_id = ${ORG_ID}
   AND user_id IN (${SEAT_A_UID}, ${SEAT_B_UID})
   AND function_key = 'customer_service'
   AND status = 'active'
   AND business_identity_id NOT IN (${SEAT_A_IDENTITY_ID}, ${SEAT_B_IDENTITY_ID});
INSERT INTO organization_business_identity
  (id, organization_id, function_key, display_name, status, version, created_by_user_id)
VALUES (${SEAT_A_IDENTITY_ID}, ${ORG_ID}, 'customer_service', 'csww-e2e-seat-a', 'active', 1, NULL),
       (${SEAT_B_IDENTITY_ID}, ${ORG_ID}, 'customer_service', 'csww-e2e-seat-b', 'active', 1, NULL)
ON CONFLICT (id) DO UPDATE SET display_name = EXCLUDED.display_name, status = 'active';
INSERT INTO organization_business_identity_assignment
  (id, organization_id, business_identity_id, function_key, user_id, status, assigned_by, version)
VALUES (${ASSIGN_A_ID}, ${ORG_ID}, ${SEAT_A_IDENTITY_ID}, 'customer_service', ${SEAT_A_UID}, 'active', NULL, 1),
       (${ASSIGN_B_ID}, ${ORG_ID}, ${SEAT_B_IDENTITY_ID}, 'customer_service', ${SEAT_B_UID}, 'active', NULL, 1)
ON CONFLICT (id) DO UPDATE SET status = 'active', ended_at = NULL, end_reason = NULL;
SQL
log "固定身份链就绪：A identity=${SEAT_A_IDENTITY_ID} B identity=${SEAT_B_IDENTITY_ID}"

# ── 4) 坐席开通（生产代码路径 provision_seat 复用固定 identity 补 seat；幂等）──
provision() {
  local org="$1" ws="$2" uid="$3" name="$4"
  "${REL_BIN}" eval "R = customer_service_facade:provision_seat(${org}, #{workspace_id => ${ws}, user_id => ${uid}, display_name => <<\"${name}\">>, adm_user_id => ${ADMIN_ACTOR_ID}}), file:write_file(\"/tmp/csww-seed-provision.txt\", io_lib:format(\"~p~n\", [R])), ok." >/dev/null 2>&1
  cat /tmp/csww-seed-provision.txt
}

SEAT_A_OUT="$(provision "${ORG_ID}" "${WS_ID}" "${SEAT_A_UID}" "csww-e2e-seat-a")"
SEAT_B_OUT="$(provision "${ORG_ID}" "${WS_ID}" "${SEAT_B_UID}" "csww-e2e-seat-b")"
echo "${SEAT_A_OUT}" | grep -q "^{ok" || die "seat A provision 失败: ${SEAT_A_OUT}"
echo "${SEAT_B_OUT}" | grep -q "^{ok" || die "seat B provision 失败: ${SEAT_B_OUT}"

# 身份 id 从事实链权威行（assignment）回读，并断言与 env.ts 固定值一致（防漂移）
SEAT_A_IDENTITY="$(psql_run -c "SELECT business_identity_id FROM organization_business_identity_assignment WHERE organization_id=${ORG_ID} AND user_id=${SEAT_A_UID} AND function_key='customer_service' AND status='active' ORDER BY business_identity_id LIMIT 1" | head -1 | tr -d '[:space:]')"
SEAT_B_IDENTITY="$(psql_run -c "SELECT business_identity_id FROM organization_business_identity_assignment WHERE organization_id=${ORG_ID} AND user_id=${SEAT_B_UID} AND function_key='customer_service' AND status='active' ORDER BY business_identity_id LIMIT 1" | head -1 | tr -d '[:space:]')"
[ -n "${SEAT_A_IDENTITY}" ] || die "seat A identity 缺失（provision 未落事实链）"
[ -n "${SEAT_B_IDENTITY}" ] || die "seat B identity 缺失（provision 未落事实链）"
[ "${SEAT_A_IDENTITY}" = "${SEAT_A_IDENTITY_ID}" ] || die "seat A identity 漂移: 得 ${SEAT_A_IDENTITY} 预期 ${SEAT_A_IDENTITY_ID}"
[ "${SEAT_B_IDENTITY}" = "${SEAT_B_IDENTITY_ID}" ] || die "seat B identity 漂移: 得 ${SEAT_B_IDENTITY} 预期 ${SEAT_B_IDENTITY_ID}"
log "坐席事实链就绪：A identity=${SEAT_A_IDENTITY} B identity=${SEAT_B_IDENTITY}"

# ── 5) widget installation（固定 id SQL upsert；列口径对齐 cs_pg_widget）──
# allowed_origins 覆盖 P1（localhost:8901 宿主）与 P2（shop.test:8943 宿主）；
# 探针端点 GET /api/v1/cs/widget/frame/:public_widget_id（零凭证面）。
psql_run <<SQL
INSERT INTO customer_service_widget_installation
  (id, organization_id, public_widget_id, display_name, allowed_origins,
   branding, consent_version, status, created_by_user_id)
VALUES (${INST_ID}, ${ORG_ID}, '${PUB_WIDGET}', 'csww-e2e-widget',
         '["${HOST_ORIGIN}", "${P2_SHOP_ORIGIN}"]'::jsonb,
         '{}'::jsonb, 'v3run-1', 'active', NULL),
       (${INST_OTHER_ID}, ${ORG_OTHER_ID}, '${PUB_WIDGET_OTHER}', 'csww-chain-widget-other',
         '["${HOST_ORIGIN}", "${P2_SHOP_ORIGIN}"]'::jsonb,
         '{}'::jsonb, 'v3run-1', 'active', NULL)
ON CONFLICT (id) DO UPDATE SET
  public_widget_id = EXCLUDED.public_widget_id,
  allowed_origins = EXCLUDED.allowed_origins,
  status = 'active', revoked_at = NULL;
SQL

FRAME_CODE="$(curl -s -o /dev/null -w '%{http_code}' "${BACKEND_BASE}/w/${PUB_WIDGET}")"
[ "${FRAME_CODE}" = "200" ] || die "widget frame 探针失败: HTTP ${FRAME_CODE} (${BACKEND_BASE}/w/${PUB_WIDGET})"
log "安装就绪：${INST_ID}/${PUB_WIDGET}（负例 ${INST_OTHER_ID}/${PUB_WIDGET_OTHER}；frame 探针 200）"

# ── 6) 输出实体清单 + CSWW_E2E_* / CSWW_P2_* 注入段 ──────────────────────
cat <<ENVELOPE

==== CSWW 种子实体清单（v3run / ${PG_DATABASE}）====
organization            ${ORG_ID}   (csww-e2e-org)
workspace               ${WS_ID}   (csww-e2e-ws)
other_organization      ${ORG_OTHER_ID}   (csww-e2e-org-other)
other_workspace         ${WS_OTHER_ID}   (csww-e2e-ws-other)
seat_a user/account      ${SEAT_A_UID} / ${SEAT_A_ACCOUNT}   (identity ${SEAT_A_IDENTITY})
seat_b user/account      ${SEAT_B_UID} / ${SEAT_B_ACCOUNT}   (identity ${SEAT_B_IDENTITY})
installation            ${INST_ID}   (public ${PUB_WIDGET})
other_installation      ${INST_OTHER_ID}   (public ${PUB_WIDGET_OTHER})
admin_actor(admin id)   ${ADMIN_ACTOR_ID}

----CSWW_E2E_ENV----
export CSWW_E2E_BACKEND=${BACKEND_BASE}
export CSWW_E2E_HOST_ORIGIN=${HOST_ORIGIN}
export CSWW_E2E_PG_DB=${PG_DATABASE}
export CSWW_E2E_ORG_ID=${ORG_ID}
export CSWW_E2E_WORKSPACE_ID=${WS_ID}
export CSWW_E2E_INSTALLATION_ID=${INST_ID}
export CSWW_E2E_PUBLIC_WIDGET_ID=${PUB_WIDGET}
export CSWW_E2E_OTHER_ORG_ID=${ORG_OTHER_ID}
export CSWW_E2E_OTHER_INSTALLATION_ID=${INST_OTHER_ID}
export CSWW_E2E_OTHER_PUBLIC_WIDGET_ID=${PUB_WIDGET_OTHER}
export CSWW_E2E_SEAT_A_ACCOUNT=${SEAT_A_ACCOUNT}
export CSWW_E2E_SEAT_A_IDENTITY_ID=${SEAT_A_IDENTITY}
export CSWW_E2E_SEAT_B_ACCOUNT=${SEAT_B_ACCOUNT}
export CSWW_E2E_SEAT_B_IDENTITY_ID=${SEAT_B_IDENTITY}
export CSWW_P2_BACKEND=${BACKEND_BASE}
export CSWW_P2_SHOP_ORIGIN=${P2_SHOP_ORIGIN}
export CSWW_P2_ADMIN_ORIGIN=${P2_ADMIN_ORIGIN}
export CSWW_P2_PG_DB=${PG_DATABASE}
ENVELOPE

log "种子就绪（幂等；可重复执行）"
