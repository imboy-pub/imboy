#!/usr/bin/env bash
# paid_channel_fixture.sh — 本地付费频道闭环 fixture
#
# 目的：为 integration_test/demo_flow/paid_channel_flow_test.dart 准备一个
# 可回收的 type=2 频道、价格和一条内容消息。
#
# 默认只读。create/cleanup 必须同时满足：
#   PAID_FIXTURE_ALLOW_WRITES=true
#   PAID_FIXTURE_CONFIRM=CREATE_LOCAL_PAID_FIXTURE 或 CLEANUP_LOCAL_PAID_FIXTURE
#   PGHOST 为 localhost/回环/私网地址
#
# 不创建用户、不修改已有频道、不接触真实支付；买家和作者 UID 必须由调用者
# 明确提供，且必须已存在于本地测试库。
#
# 示例（不会自动执行）：
#   PAID_FIXTURE_OWNER_UID=... \
#   PAID_FIXTURE_BUYER_UID=... \
#   PAID_FIXTURE_ALLOW_WRITES=true \
#   PAID_FIXTURE_CONFIRM=CREATE_LOCAL_PAID_FIXTURE \
#   bash scripts/paid_channel_fixture.sh create
#
# 清理：
#   PAID_FIXTURE_MARKER=... \
#   PAID_FIXTURE_ALLOW_WRITES=true \
#   PAID_FIXTURE_CONFIRM=CLEANUP_LOCAL_PAID_FIXTURE \
#   bash scripts/paid_channel_fixture.sh cleanup

set -euo pipefail

PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-5432}"
PGUSER="${PGUSER:-}"
PGDATABASE="${PGDATABASE:-}"
PGPASSWORD="${PGPASSWORD:-}"
export PGPASSWORD

PAID_FIXTURE_MARKER="${PAID_FIXTURE_MARKER:-}"
PAID_FIXTURE_OWNER_UID="${PAID_FIXTURE_OWNER_UID:-}"
PAID_FIXTURE_BUYER_UID="${PAID_FIXTURE_BUYER_UID:-}"
PAID_FIXTURE_ALLOW_WRITES="${PAID_FIXTURE_ALLOW_WRITES:-false}"
PAID_FIXTURE_CONFIRM="${PAID_FIXTURE_CONFIRM:-}"

PSQL=(psql -X -v ON_ERROR_STOP=1 -h "${PGHOST}" -p "${PGPORT}" -U "${PGUSER}" -d "${PGDATABASE}")

die() {
    echo "FAIL: $*" >&2
    exit 1
}

usage() {
    cat >&2 <<'EOF'
用法：
  paid_channel_fixture.sh inspect
  paid_channel_fixture.sh create
  paid_channel_fixture.sh cleanup

create/cleanup 需要显式设置 PAID_FIXTURE_ALLOW_WRITES=true 和对应确认词。
EOF
    exit 2
}

require_connection_config() {
    [[ -n "${PGUSER}" ]] || die "缺少 PGUSER"
    [[ -n "${PGDATABASE}" ]] || die "缺少 PGDATABASE"
    "${PSQL[@]}" -At -c 'SELECT 1' >/dev/null || die "PostgreSQL 连接失败"
}

is_local_host() {
    case "${PGHOST}" in
        localhost|localhost.localdomain|::1|127.*|10.*|192.168.*|172.1[6-9].*|172.2[0-9].*|172.3[0-1].*|*.local)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

require_write_guard() {
    [[ "${PAID_FIXTURE_ALLOW_WRITES}" == "true" ]] ||
        die "写操作默认关闭：请显式设置 PAID_FIXTURE_ALLOW_WRITES=true"
    is_local_host || die "拒绝非本地/私网 PGHOST：${PGHOST}"
}

require_positive_uid() {
    local label="$1"
    local value="$2"
    [[ "${value}" =~ ^[1-9][0-9]*$ ]] || die "${label} 必须是正整数 UID"
}

create_fixture() {
    require_write_guard
    [[ "${PAID_FIXTURE_CONFIRM}" == "CREATE_LOCAL_PAID_FIXTURE" ]] ||
        die "create 需要 PAID_FIXTURE_CONFIRM=CREATE_LOCAL_PAID_FIXTURE"
    require_positive_uid PAID_FIXTURE_OWNER_UID "${PAID_FIXTURE_OWNER_UID}"
    require_positive_uid PAID_FIXTURE_BUYER_UID "${PAID_FIXTURE_BUYER_UID}"
    [[ "${PAID_FIXTURE_OWNER_UID}" != "${PAID_FIXTURE_BUYER_UID}" ]] ||
        die "owner 和 buyer 必须是不同用户，才能验证 paywall"

    local marker
    marker="${PAID_FIXTURE_MARKER:-imboy-paid-fixture-$(date +%s)}"
    [[ "${marker}" =~ ^[A-Za-z0-9._-]{1,90}$ ]] || die "PAID_FIXTURE_MARKER 格式无效"

    local owner_count buyer_count existing_count channel_id admin_id price_id message_id
    owner_count="$("${PSQL[@]}" -At \
        -c "SELECT COUNT(*) FROM public.\"user\" WHERE id = ${PAID_FIXTURE_OWNER_UID} AND status = 1;")"
    buyer_count="$("${PSQL[@]}" -At \
        -c "SELECT COUNT(*) FROM public.\"user\" WHERE id = ${PAID_FIXTURE_BUYER_UID} AND status = 1;")"
   [[ "${owner_count}" == "1" ]] || die "owner 用户不存在或未启用"
   [[ "${buyer_count}" == "1" ]] || die "buyer 用户不存在或未启用"

    existing_count="$("${PSQL[@]}" -At \
        -c "SELECT COUNT(*) FROM public.channel WHERE custom_id = '${marker}';")"
    [[ "${existing_count}" == "0" ]] || die "marker 已存在，先 cleanup 或换 marker"

    channel_id="$("${PSQL[@]}" -At -c 'SELECT (EXTRACT(EPOCH FROM clock_timestamp()) * 1000000)::bigint;')"
    admin_id="$((channel_id + 1))"
    price_id="$((channel_id + 2))"
    message_id="$((channel_id + 3))"

    "${PSQL[@]}" -v channel_id="${channel_id}" -v admin_id="${admin_id}" \
        -v price_id="${price_id}" -v message_id="${message_id}" \
        -v owner_uid="${PAID_FIXTURE_OWNER_UID}" -v marker="${marker}" <<'SQL'
BEGIN;
INSERT INTO public.channel
    (id, name, description, avatar, type, custom_id, creator_uid, status)
VALUES
    (:channel_id, 'IMBoy local paid fixture', '自动化测试专用付费频道', '', 2,
     :'marker', :owner_uid, 1);
INSERT INTO public.channel_admin (id, channel_id, user_id, role)
VALUES (:admin_id, :channel_id, :owner_uid, 3);
INSERT INTO public.channel_price
    (id, channel_id, price, currency, subscription_type, original_price, description, status)
VALUES
    (:price_id, :channel_id, 9.90, 'CNY', 1, 19.90, '自动化测试 fixture', 1);
INSERT INTO public.channel_message
    (id, channel_id, author_id, author_name, content, msg_type, status)
VALUES
    (:message_id, :channel_id, :owner_uid, 'IMBoy fixture',
     'paid-channel-fixture-content', 'text', 1);
COMMIT;
SQL

    echo "PAID_FIXTURE_CREATED=true"
    echo "TEST_PAID_CHANNEL_ID=${channel_id}"
    echo "PAID_FIXTURE_MARKER=${marker}"
}

inspect_fixture() {
    require_connection_config
    local marker_clause=""
    if [[ -n "${PAID_FIXTURE_MARKER}" ]]; then
        [[ "${PAID_FIXTURE_MARKER}" =~ ^[A-Za-z0-9._-]{1,90}$ ]] || die "PAID_FIXTURE_MARKER 格式无效"
        marker_clause=" AND c.custom_id = '${PAID_FIXTURE_MARKER}'"
    fi
    "${PSQL[@]}" -P pager=off -F '|' -At -c \
        "SELECT c.id, c.custom_id, c.type, c.status, cp.price, cp.currency,
                COUNT(DISTINCT cm.id) AS message_count,
                COUNT(DISTINCT co.id) AS order_count
           FROM public.channel c
           LEFT JOIN public.channel_price cp ON cp.channel_id = c.id AND cp.status = 1
           LEFT JOIN public.channel_message cm ON cm.channel_id = c.id AND cm.status = 1
           LEFT JOIN public.channel_order co ON co.channel_id = c.id
          WHERE c.custom_id LIKE 'imboy-paid-fixture-%'${marker_clause}
          GROUP BY c.id, c.custom_id, c.type, c.status, cp.price, cp.currency
          ORDER BY c.id DESC;"
}

cleanup_fixture() {
    require_write_guard
    [[ "${PAID_FIXTURE_CONFIRM}" == "CLEANUP_LOCAL_PAID_FIXTURE" ]] ||
        die "cleanup 需要 PAID_FIXTURE_CONFIRM=CLEANUP_LOCAL_PAID_FIXTURE"
    [[ -n "${PAID_FIXTURE_MARKER}" ]] || die "cleanup 必须提供 PAID_FIXTURE_MARKER"
    [[ "${PAID_FIXTURE_MARKER}" =~ ^[A-Za-z0-9._-]{1,90}$ ]] || die "PAID_FIXTURE_MARKER 格式无效"

    "${PSQL[@]}" -v marker="${PAID_FIXTURE_MARKER}" <<'SQL'
BEGIN;
DELETE FROM public.channel
 WHERE custom_id = :'marker'
   AND custom_id LIKE 'imboy-paid-fixture-%';
COMMIT;
SQL
    echo "PAID_FIXTURE_CLEANED=true"
    echo "PAID_FIXTURE_MARKER=${PAID_FIXTURE_MARKER}"
}

command="${1:-}"
case "${command}" in
    inspect)
        require_connection_config
        inspect_fixture
        ;;
    create)
        require_connection_config
        create_fixture
        ;;
    cleanup)
        require_connection_config
        cleanup_fixture
        ;;
    *)
        usage
        ;;
esac
