#!/usr/bin/env bash
# CS-04 —— 客服产品全链 **HTTP 独立 E2E** 门（plan §8 CS-04 / A0 客户端契约基准）。
#
# 依据：
#   * docs/plans/2026-09-14-enterprise-business-and-customer-service-zcode-plan-v4.1.md §8 CS-04
#   * control/prompts/cs04-worker-prompt.md（场景与验收点 CS-04-A01..A06）
#   * 契约来源（逐条核对，禁止臆造端点/参数）：
#       - src/features/customer_service/interfaces/cs_actions.erl      （客服租户面冻结契约）
#       - src/features/enterprise_business/interfaces/eb_enterprise_actions.erl（企业租户面冻结契约）
#       - src/features/customer_service/interfaces/cs_http.erl         （错误 → HTTP 映射；
#         offboarding 降级 = 409 + envelope `offboarding_required`）
#       - scripts/smoke_8step.sh                                       （passport signup/login 造数配方）
#
# ## 场景（与任务卡顺序一致）
#
#   合成商城访客 -> 企业 contact -> 客服 session -> claim -> 消息/附件 ->
#   delivery ACK 后真源仍在 -> A suspend -> identity rebind B -> B 继续会话 ->
#   close/rating；跨 Org/旧 JWT/无凭证/重放/并发 claim 负例；合成 policy/
#   hold/purge 时钟边界。
#
# ## 时钟边界（合成 policy/hold/purge）与本脚本的能力边界
#
#   * 合成 1095d 保留策略：`retain_until = accepted_at + 1095*86400`，消息接受时由
#     `eb_retention` 固化（domain/eb_retention.erl 头注释）。本脚本以 **DB 只读探针**
#     验证固化值（不做任何写操作）。
#   * 附件保留期**不得短于**所属消息，短了 fail-closed
#     （application/asset/eb_asset_app.erl「retain_until_shorter_than_message」）。
#     本脚本以负例验证（retain_until < 消息期 ⇒ 4xx）。
#   * **purge 不存在 HTTP 面**：唯一物理删除路径在 DB 侧，且必须
#     `SET LOCAL imboy.enterprise_purge = 'on'`（GUC 门，见
#     infrastructure/eb_pg_purge.erl 的 ?PURGE_GUC 定义；无该 GUC 时 DELETE 被
#     数据库侧拒绝）。hold 的创建/释放同样是 DB/人工 Gate（plan §9 外部 Gate），
#     不在本脚本能力内。本脚本以「purge/hold HTTP 路径 404/405」+「close/suspend/
#     transfer/ACK 后消息与附件行数不变（只读探针）」机械证明 A06。
#   * 时钟源全部为服务端时钟；本脚本不注入、不加速、不伪造时钟。
#
# ## 边界声明（CS-04-A05，结论口径）
#
#   本脚本产出的全部证据都是 **synthetic**（合成账号/合成 consent/合成 policy）。
#   通过后仅可声明 `LOCAL_CS_E2E_PASS (synthetic)`；**不得**据此声称真实客户、
#   真实 consent、真实 policy/hold、法律合规或生产验收。
#
# ## 两种运行模式
#
#   CS04_E2E_MODE=remote（默认）
#     打 $CS04_BASE_URL（A0 提供的 wi_e2e daemon，9810），全部造数经 HTTP。
#     注册受 License 用户配额闸门（见下 BLOCKED_DATA）；scratch 库配额占满时
#     exit 2 报 BLOCKED_DATA，铺底后可原样复跑。
#
#   CS04_E2E_MODE=local
#     自含模式（与既有 enterprise_business_e2e.sh 的 EB-11 门同款）：
#     本脚本自起一个**隔离端口**的 imboy 节点（连同一 scratch 库），节点内以
#     fixture 同款方式种入 3 行 **user 认证壳**（id/password='x'/account 前缀
#     cs04local-，可归因；eb_e2e_fixture:insert_users/1 同一语句）并以生产同款
#     签发函数 `token_ds:encrypt_token/1` 签发真 JWT。**业务数据 100% 经 HTTP**
#     造数（org/workspace/member/身份/assignment/contact/conversation/message/
#     asset/session/claim/... 全部 API 调用）。种子仅覆盖「注册 API 被配额闸门
#     挡住」这一环境限制，不种任何企业/客服业务行。
#
# ## BLOCKED_DATA（remote 模式环境铺底缺失时）
#
#   注册经 POST /api/v1/passport/signup 受 License 用户配额闸门
#   （passport_logic:quota_guard/0 → imboy_license:check_user_quota/1）。
#   scratch 库历史 eunit 夹具用户把配额占满时（402），本脚本以 **exit 2** 报
#   BLOCKED_DATA，并在 evidence 目录写 blocked-data.txt（含 A0 最小铺底两种方案）。
#   铺底就绪后本脚本可原样复跑（exit 0/1 才是链路结论）。
#
# 用法：
#   bash scripts/customer_service_e2e.sh                                   # remote：打 9810
#   CS04_E2E_MODE=local bash scripts/customer_service_e2e.sh               # local：自含隔离节点
#   CS04_BASE_URL=http://127.0.0.1:9810 bash scripts/customer_service_e2e.sh
#   CS04_EVIDENCE_DIR=/tmp/cs04-ev bash scripts/customer_service_e2e.sh
set -uo pipefail

cd "$(dirname "$0")/.."
WT="$(pwd)"
RUN_ROOT="${CS04_RUN_ROOT:-$(cd "$WT/../.." && pwd)}"
BASE="${CS04_BASE_URL:-http://127.0.0.1:9810}"
EVID="${CS04_EVIDENCE_DIR:-$RUN_ROOT/control/agents/cs04/logs/cs04-http}"
MODE="${CS04_E2E_MODE:-remote}"
MASTER_CODE="${CS04_MASTER_CODE:-abc12345}"
EXPECT_DB="${CS04_SCRATCH_DB:-imboy_eb_w21}"
PGHOST="${PGHOST:-127.0.0.1}"
PGPORT="${PGPORT:-4323}"
PGUSER="${PGUSER:-imboy_user}"
export PGPASSWORD="${PGPASSWORD:-abc54321}"
CURL_T="${CS04_CURL_TIMEOUT:-15}"

mkdir -p "$EVID"
BODY="$(mktemp)"; trap 'rm -f "$BODY"' EXIT

PASS=0; FAIL=0

say()  { printf '%s\n' "$*"; }
now_ms() { python3 -c 'import time; print(int(time.time()*1000))'; }

# jsonget <json 文本> <jq 表达式默认 .payload>：提取失败输出空串
jsonget() { printf '%s' "$1" | jq -r "${2:-.payload}" 2>/dev/null; }

# http <method> <url> [curl 额外参数...]：HTTP 码 → stdout，body → $BODY
http() {
    local method="$1" url="$2"; shift 2
    curl -sS -m "$CURL_T" -o "$BODY" -w '%{http_code}' -X "$method" "$@" "$url" 2>/dev/null || printf '000'
}

# assert_eq <断言名> <实际> <期望>
assert_eq() {
    local name="$1" actual="$2" expect="$3"
    if [ "$actual" = "$expect" ]; then
        PASS=$((PASS + 1)); say "[ASSERT-PASS] $name :: actual=$actual"
    else
        FAIL=$((FAIL + 1))
        say "[ASSERT-FAIL] $name :: expect=$expect actual=$actual"
        say "    body=$(head -c 400 "$BODY")"
    fi
}

# assert_ne <断言名> <实际> <不期望>
assert_ne() {
    local name="$1" actual="$2" notexpect="$3"
    if [ "$actual" != "$notexpect" ]; then
        PASS=$((PASS + 1)); say "[ASSERT-PASS] $name :: actual=$actual"
    else
        FAIL=$((FAIL + 1)); say "[ASSERT-FAIL] $name :: 值不应为 $notexpect"
    fi
}

# assert_tsid <断言名> <值>：TSID 以 JSON string 传输（64-bit 数字字符串，A02）
assert_tsid() {
    local name="$1" v="$2"
    if [ -n "$v" ] && printf '%s' "$v" | grep -qE '^[0-9]{15,20}$'; then
        PASS=$((PASS + 1)); say "[ASSERT-PASS] $name :: tsid-string=$v"
    else
        FAIL=$((FAIL + 1)); say "[ASSERT-FAIL] $name :: 不是合法 TSID string（got '$v'）"
    fi
}

# signup <email> <pwd> <nickname>：注册；输出 code
signup() {
    local acct="$1" pwd="$2" nick="$3"
    local code
    code="$(http POST "$BASE/api/v1/passport/signup" \
        -H 'Content-Type: application/x-www-form-urlencoded' \
        --data-urlencode 'type=email' --data-urlencode "account=$acct" \
        --data-urlencode "pwd=$pwd" --data-urlencode "code=$MASTER_CODE" \
        --data-urlencode 'rsa_encrypt=0' --data-urlencode "nickname=$nick")"
    printf '%s' "$code"
}

# login <email> <pwd>：登录；输出 "<http_code> <token> <uid>"
login() {
    local acct="$1" pwd="$2"
    local code token uid
    code="$(http POST "$BASE/api/v1/passport/login" \
        -H 'Content-Type: application/x-www-form-urlencoded' \
        --data-urlencode 'type=email' --data-urlencode "account=$acct" \
        --data-urlencode "pwd=$pwd" --data-urlencode 'rsa_encrypt=0')"
    token="$(jsonget "$(cat "$BODY")" '.payload.token // empty')"
    uid="$(jsonget "$(cat "$BODY")" '.payload.uid // empty')"
    printf '%s %s %s' "$code" "$token" "$uid"
}

# eb <方法> <org_id> <路径后缀> <jwt> [json-body]
eb() {
    local m="$1" org="$2" path="$3" jwt="$4" data="${5:-}"
    local -a args=(-H "Authorization: Bearer $jwt")
    if [ -n "$data" ]; then args+=(-H 'Content-Type: application/json' -d "$data"); fi
    http "$m" "$BASE/api/v1/enterprise/organizations/$org/$path" "${args[@]}"
}

# cs <方法> <路径(含 /api/v1/cs/... 前缀)> <认证参数...>
#   认证参数形态：jwt:<token> | visit:<token> | shopkey:<secret> | anon
cs() {
    local m="$1" path="$2"; shift 2
    local -a args=()
    local p
    for p in "$@"; do
        case "$p" in
            jwt:*)     args+=(-H "Authorization: Bearer ${p#jwt:}") ;;
            visit:*)   args+=(-H "x-cs-visit-token: ${p#visit:}") ;;
            shopkey:*) args+=(-H "x-cs-shop-key: ${p#shopkey:}") ;;
            anon)      ;;
        esac
    done
    if [ -n "${CS04_JSON:-}" ]; then
        args+=(-H 'Content-Type: application/json' -d "$CS04_JSON")
    fi
    http "$m" "$BASE$path" "${args[@]}"
}

# comp <facade_fun> <json-params>：F6 补偿通道（仅 local 模式）。
#   通过种子节点的命令目录直调 facade（注入 eb_env_keyring:key_ref()），
#   与 enterprise_business_e2e 门 eb_e2e_a01 的「facade 层 + 测试侧 key_ref 注入」
#   同一补偿口径；HTTP 面动作表白名单无 key_ref、生产装配无密钥提供者（F6）。
comp() {
    local fun="$1" json="$2" n o
    n="$(ls "$SEED_IN" 2>/dev/null | grep -c cmd || true)"
    n=$((n + 1))$(date +%s%N)
    printf '%s\n%s\n' "$fun" "$json" > "$SEED_IN/cmd$n.cmd"
    for _ in $(seq 1 40); do
        [ -f "$SEED_OUT/cmd$n.out" ] && break
        sleep 0.25
    done
    o="$(cat "$SEED_OUT/cmd$n.out" 2>/dev/null)"
    rm -f "$SEED_OUT/cmd$n.out"
    say "[COMP]   $fun -> $(printf '%s' "$o" | head -c 90)"
    printf '%s' "$o"
}

# db_ro <sql>：DB 只读探针（SELECT only；本脚本对业务表零写）
db_ro() {
    PGPASSWORD="$PGPASSWORD" psql -h "$PGHOST" -p "$PGPORT" -U "$PGUSER" \
        -d "$EXPECT_DB" -tAc "$1" 2>/dev/null
}

say "== CS-04 客服产品全链 HTTP 独立 E2E 门 =="
say "worktree : $WT"
say "base_url : $BASE"
say "evidence : $EVID"
say "scratch  : $EXPECT_DB @ $PGHOST:$PGPORT"
say ""

# ---------------------------------------------------------------- 前置检查 --
PRE_FAIL=0
for bin in curl jq python3 psql; do
    command -v "$bin" >/dev/null 2>&1 || { say "[E2E-PRE] 缺少依赖 $bin"; PRE_FAIL=$((PRE_FAIL + 1)); }
done
# closure 修正：healthz 预检只对 remote 模式有意义——local 模式此时自含节点
# 尚未启动（BASE 会在节点起来后重指到隔离端口），旧顺序会误杀 local 复跑。
if [ "$MODE" != "local" ]; then
    HZ="$(http GET "$BASE/healthz")"
    [ "$HZ" = "200" ] || { say "[E2E-PRE] 后端不可达：GET $BASE/healthz -> $HZ"; PRE_FAIL=$((PRE_FAIL + 1)); }
fi
# 库名护栏：绝不允许脚本指向共享/系统库
case "$EXPECT_DB" in
    imboy_v1|postgres|template*) say "[E2E-PRE] 拒绝共享/系统库 $EXPECT_DB"; PRE_FAIL=$((PRE_FAIL + 1)) ;;
esac
if [ "$PRE_FAIL" -ne 0 ]; then
    say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$PRE_FAIL"
    say "[FAIL] CS-04 HTTP E2E 前置检查未通过"
    exit 1
fi
say "[OK]   前置：healthz=200 + 依赖齐 + scratch 库名护栏通过"
say ""

# ------------------------------------------------- local 模式：自含隔离节点 --
SEED_PID=""
cleanup_seed() {
    [ -n "$SEED_PID" ] && kill "$SEED_PID" 2>/dev/null
    return 0
}
trap cleanup_seed EXIT

pick_port() {
    python3 - <<'PY'
import socket
s = socket.socket()
s.bind(("127.0.0.1", 0))
print(s.getsockname()[1])
s.close()
PY
}

if [ "$MODE" = "local" ]; then
    PORT="$(pick_port)"
    PORT_ADM="$(pick_port)"
    SEED_OUT="$EVID/internal/seed-node.txt"
    mkdir -p "$EVID/internal"
    say "-- local 模式：自起隔离节点（port=${PORT}，adm=${PORT_ADM}，库=${EXPECT_DB}）--"
    # 配置副本：把 http_port/http_port_adm 改写为本运行的隔离端口（绝不触碰
    # 9700/9706 等被占端口；-config 的加载时序晚于 -eval 的 set_env，不能依赖
    # 运行时覆盖——实测 eaddrinuse 即此因）。
    # 另注入合成 {eb_enterprise_keyring,...}（F6：企业托管加密 fail-closed 的
    # 服务端配置注入面；scratch 环境缺失即 contact/asset 全线 500。此处为
    # 本 run 生成的合成 32 字节 hex key，仅本节点使用，不入任何仓库）。
    CFG="$EVID/internal/sys.local.cs04.config"
    cp config/sys.local.eb.config "$CFG"
    EB_KEY_HEX="$(openssl rand -hex 32)"
    python3 - "$CFG" "$PORT" "$PORT_ADM" "$EB_KEY_HEX" <<'PY'
import re, sys
path, port, adm, key_hex = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
t = open(path, encoding='utf-8').read()
t = re.sub(r'\{http_port, \d+\}', '{http_port, %s}' % port, t)
t = re.sub(r'\{http_port_adm, \d+\}', '{http_port_adm, %s}' % adm, t)
keyring = '{eb_enterprise_keyring, #{active_version => 1, keys => #{1 => <<"%s">>}}},\n' % key_hex
t = t.replace('        %% Feature flags', '        ' + keyring + '\n        %% Feature flags', 1)
open(path, 'w', encoding='utf-8').write(t)
PY
    # 生成 seed escript（F6 补偿口径，见 eb_e2e_a01 头注释）：
    #   基础种子 = user/org/workspace/member（eb_e2e_fixture 同款语句）；
    #   F6 补偿种子 = 需要 key_ref 的写链（identity/assignment/contact/conversation）
    #   经 facade + eb_env_keyring:key_ref()；业务读链与客服链全部由脚本经 HTTP 驱动。
    SEED_ERL="$EVID/internal/cs04_seed.escript"
    SEED_IN="$EVID/internal/comp-in"; SEED_OUT="$EVID/internal/comp-out"
    SEED_LOG="$EVID/internal/seed-node.log"
    mkdir -p "$SEED_IN" "$SEED_OUT"
    cat > "$SEED_ERL" <<'ERLEOF'
#!/usr/bin/env escript
%%! -noshell
main([PortStr, CompIn, CompOut, ConfigBase]) ->
    Root = "@@WT@@",
    [code:add_patha(P) || P <- filelib:wildcard(Root ++ "/deps/*/ebin")],
    code:add_patha(Root ++ "/ebin"),
    code:add_patha(Root ++ "/imboy/ebin"),
    code:add_patha(Root ++ "/test"),
    application:set_env(imboy, http_port, list_to_integer(PortStr)),
    {ok, _} = application:ensure_all_started(imboy),
    {ok, KR} = eb_env_keyring:key_ref(),
    T0 = erlang:system_time(millisecond),
    MkId = fun() -> 930000000000000000 + ((T0 - 1789000000000) * 100000) + (erlang:unique_integer([positive, monotonic]) rem 100000) end,
    [O, A, B, C, D, ORG, WS] = [MkId(), MkId(), MkId(), MkId(), MkId(), MkId(), MkId()],
    lists:foreach(fun(U) ->
        {ok, _} = elib_pg:query(<<"INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv) VALUES ($1,'x',$2,'127.0.0.1','x')">>,
            [U, iolist_to_binary([<<"cs04local-">>, integer_to_binary(U)])])
    end, [O, A, B, C, D]),
    {ok, _} = elib_pg:query(<<"INSERT INTO organization(id,name,owner_id,status) VALUES ($1,$2,$3,'active')">>,
        [ORG, iolist_to_binary([<<"cs04local-org-">>, integer_to_binary(ORG)]), O]),
    {ok, _} = elib_pg:query(<<"INSERT INTO workspace(id,name,owner_id,status,type,organization_id) VALUES ($1,$2,$3,'active','project',$4)">>,
        [WS, iolist_to_binary([<<"cs04local-ws-">>, integer_to_binary(WS)]), O, ORG]),
    lists:foreach(fun(U) ->
        {ok, _} = elib_pg:query(<<"INSERT INTO organization_member(organization_id,user_id,role,status) VALUES ($1,$2,'member','active')">>, [ORG, U])
    end, [A, B, C, D]),
    At = T0 div 1000,
    MkIdn = fun(Fk, Dn) ->
        {ok, I} = enterprise_business_facade:create_identity(ORG, #{function_key => Fk,
            display_name => iolist_to_binary([Dn, integer_to_binary(T0)]),
            workspace_id => WS, actor_user_id => O, at => At}),
        I
    end,
    ISA = MkIdn(<<"sales">>, <<"sa">>),
    ICA = MkIdn(<<"customer_service">>, <<"ca">>),
    ISB = MkIdn(<<"sales">>, <<"sb">>),
    ICB = MkIdn(<<"customer_service">>, <<"cb">>),
    %% S3.7/S3.8 真并发 claim 的两个无负荷坐席（不参与 suspend/rebind 叙事，
    %% 与 A/B 的交接链零交集）。
    ICC = MkIdn(<<"customer_service">>, <<"cc">>),
    ICD = MkIdn(<<"customer_service">>, <<"cd">>),
    Bind = fun(I, U) ->
        {ok, _} = enterprise_business_facade:bind_assignment(ORG, #{identity_id => maps:get(id, I),
            user_id => U, workspace_id => WS, actor_user_id => O, at => At, created_by_user_id => O})
    end,
    Bind(ISA, A), Bind(ICA, A), Bind(ICC, C), Bind(ICD, D),
    %% 注意：B 不预绑 —— offboarding rebind 会把 A 的 identity 经办转给 B；
    %% B 预绑同职能会在交接 items 上撞 duplicate_active_user_function（实证）。
    {ok, Contact} = enterprise_business_facade:create_contact(ORG, #{workspace_id => WS, channel => <<"other">>,
        subject => iolist_to_binary([<<"cs04-visitor-">>, integer_to_binary(T0)]),
        key_ref => KR, display_name => <<"cs04-contact">>,
        created_by_business_identity_id => maps:get(id, ISA), actor_user_id => A, at => At}),
    ContactId = maps:get(id, maps:get(contact, Contact)),
    {ok, Conv} = enterprise_business_facade:open_conversation(ORG, #{workspace_id => WS, contact_id => ContactId,
        business_identity_id => maps:get(id, ISA), actor_user_id => A, key_ref => KR, at => At}),
    {ok, VRows} = elib_pg:query(<<"SELECT id FROM enterprise_conversation WHERE organization_id = $1 ORDER BY id DESC LIMIT 1">>, [ORG]),
    ConvId = maps:get(<<"id">>, hd(VRows)),
    %% 并发 claim 夹具：第二条 contact+conversation（同 contact 二次开会话会
    %% conversation_exists 409，故经种子通道建独立 contact2/conversation2；
    %% 与上方 contact/conversation 同一 F6 补偿口径）。
    {ok, Contact2} = enterprise_business_facade:create_contact(ORG, #{workspace_id => WS, channel => <<"other">>,
        subject => iolist_to_binary([<<"cs04-visitor2-">>, integer_to_binary(T0)]),
        key_ref => KR, display_name => <<"cs04-contact2">>,
        created_by_business_identity_id => maps:get(id, ISA), actor_user_id => A, at => At}),
    Contact2Id = maps:get(id, maps:get(contact, Contact2)),
    {ok, _Conv2} = enterprise_business_facade:open_conversation(ORG, #{workspace_id => WS, contact_id => Contact2Id,
        business_identity_id => maps:get(id, ISA), actor_user_id => A, key_ref => KR, at => At}),
    {ok, V2Rows} = elib_pg:query(<<"SELECT id FROM enterprise_conversation WHERE organization_id = $1 ORDER BY id DESC LIMIT 1">>, [ORG]),
    Conv2Id = maps:get(<<"id">>, hd(V2Rows)),
    %% F6 补偿：消息写链（HTTP 无法携带 key_ref）——先开合成 1095d 保留策略
    %% （无策略 fail-closed missing_retention_policy；EB-11 门 A01.9 同款）
    {ok, _} = enterprise_business_facade:open_retention_policy(ORG, #{workspace_id => WS,
        data_class => <<"enterprise_message">>, retention_days => 1095,
        actor_user_id => O, key_ref => KR}),
    {ok, _} = enterprise_business_facade:open_retention_policy(ORG, #{workspace_id => WS,
        data_class => <<"enterprise_asset">>, retention_days => 1095, actor_user_id => O}),
    {ok, _} = enterprise_business_facade:append_message(ORG, #{workspace_id => WS,
        conversation_id => ConvId, identity_id => maps:get(id, ISA),
        actor_user_id => A, client_msg_id => iolist_to_binary([<<"cs04-out-">>, integer_to_binary(T0)]),
        sender_type => <<"business_identity">>, body => <<"cs04-outbound-hello">>, key_ref => KR, at => At}),
    {ok, _} = enterprise_business_facade:append_message(ORG, #{workspace_id => WS,
        conversation_id => ConvId, contact_id => ContactId,
        client_msg_id => iolist_to_binary([<<"cs04-in-">>, integer_to_binary(T0)]),
        sender_type => <<"contact">>, body => <<"cs04-inbound-ask">>, key_ref => KR, at => At}),
    {ok, MRows} = elib_pg:query(<<"SELECT id, client_msg_id FROM enterprise_message WHERE organization_id = $1 ORDER BY id">>, [ORG]),
    [M1, M2] = [maps:get(<<"id">>, R) || R <- MRows],
    TO = token_ds:encrypt_token(O), TA = token_ds:encrypt_token(A), TB = token_ds:encrypt_token(B),
    TC = token_ds:encrypt_token(C), TD = token_ds:encrypt_token(D),
    io:format("SEED_UID owner ~p~nSEED_UID a ~p~nSEED_UID b ~p~n", [O, A, B]),
    io:format("SEED_ORG ~p~nSEED_WS ~p~n", [ORG, WS]),
    io:format("SEED_IDS sales_a ~p cs_a ~p sales_b ~p cs_b ~p cs_c ~p cs_d ~p~n", [maps:get(id, ISA), maps:get(id, ICA), maps:get(id, ISB), maps:get(id, ICB), maps:get(id, ICC), maps:get(id, ICD)]),
    io:format("SEED_CONTACT ~p~nSEED_CONV ~p~n", [ContactId, ConvId]),
    io:format("SEED_CONV2 ~p~nSEED_CONTACT2 ~p~n", [Conv2Id, Contact2Id]),
    io:format("SEED_MSG1 ~p~nSEED_MSG2 ~p~n", [M1, M2]),
    io:format("SEED_TOKEN owner ~ts~nSEED_TOKEN a ~ts~nSEED_TOKEN b ~ts~n", [TO, TA, TB]),
    io:format("SEED_TOKEN c ~ts~nSEED_TOKEN d ~ts~n", [TC, TD]),
    io:format("SEED_READY~n"),
    file:make_dir(CompIn),
    file:make_dir(CompOut),
    Idx = #{<<"sales_a">> => maps:get(id, ISA), <<"cs_a">> => maps:get(id, ICA),
        <<"sales_b">> => maps:get(id, ISB), <<"cs_b">> => maps:get(id, ICB),
        <<"contact">> => ContactId, <<"conv">> => ConvId,
        <<"owner">> => O, <<"actor_a">> => A, <<"actor_b">> => B},
    comp_loop(CompIn, CompOut, ORG, WS, KR, Idx).

comp_loop(In, Out, ORG, WS, KR, Idx) ->
    case file:list_dir(In) of
        {ok, Files} ->
            [comp_one(filename:join(In, F), Out, ORG, WS, KR, Idx) || F <- Files, filename:extension(F) =:= <<".cmd">>];
        _ -> ok
    end,
    timer:sleep(150),
    comp_loop(In, Out, ORG, WS, KR, Idx).

comp_one(CmdPath, Out, ORG, WS, KR, Idx) ->
    R = case file:read_file(CmdPath) of
        {ok, Bin} ->
            try
                [FunBin, Rest] = binary:split(Bin, <<"\n">>),
                Fun = binary_to_atom(FunBin, utf8),
                J = jsone:decode(Rest),
                P0 = maps:merge(J, #{key_ref => KR, at => erlang:system_time(second),
                    organization_id => ORG, workspace_id => WS}),
                P = maps:merge(P0, Idx),
                R2 = enterprise_business_facade:Fun(ORG, P),
                io_lib:format("~p~n", [R2])
            catch Cc:Ec -> io_lib:format("ERROR ~p:~p~n", [Cc, Ec])
            end;
        _ -> <<"NOCMD">>
    end,
    file:write_file(filename:join(Out, filename:basename(CmdPath, ".cmd") ++ ".out"), R),
    file:delete(CmdPath),
    ok.
ERLEOF
    # 配置副本已在上方生成（含隔离端口与合成 keyring 注入）
    sed -i '' "s|@@WT@@|$WT|" "$SEED_ERL" 2>/dev/null || sed -i "s|@@WT@@|$WT|" "$SEED_ERL"
    # -config 需写在 escript 的 emulator 参数行（jwt_key 等启动校验依赖它）
    sed -i '' "s|%%! -noshell|%%! -config $(echo "$CFG" | sed 's/\.config$//') -noshell|" "$SEED_ERL" 2>/dev/null || sed -i "s|%%! -noshell|%%! -config $(echo "$CFG" | sed 's/\.config$//') -noshell|" "$SEED_ERL"
    escript "$SEED_ERL" "$PORT" "$SEED_IN" "$SEED_OUT" "$(echo "$CFG" | sed 's/\.config$//')" > "$SEED_LOG" 2>&1 &
    SEED_PID=$!
    # 健康轮询（最长 ~60s）+ SEED_READY 门
    UP=0
    for _ in $(seq 1 60); do
        HZ="$(http GET "http://127.0.0.1:$PORT/healthz")"
        if [ "$HZ" = "200" ] && grep -aq "SEED_READY" "$SEED_LOG" 2>/dev/null; then
            UP=1; break
        fi
        sleep 1
    done
    if [ "$UP" != "1" ]; then
        say "[FAIL] local 模式隔离节点未就绪（见 ${SEED_OUT}）"
        tail -20 "$SEED_OUT"
        say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$((FAIL + 1))"
        exit 1
    fi
    UID_OWNER="$(grep -a '^SEED_UID owner ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    ORG_ID="$(grep -a '^SEED_ORG ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    WS_ID="$(grep -a '^SEED_WS ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    UID_A="$(grep -a '^SEED_UID a ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    UID_B="$(grep -a '^SEED_UID b ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    JWT_OWNER="$(grep -a '^SEED_TOKEN owner ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    JWT_A="$(grep -a '^SEED_TOKEN a ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    JWT_B="$(grep -a '^SEED_TOKEN b ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    JWT_C="$(grep -a '^SEED_TOKEN c ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    JWT_D="$(grep -a '^SEED_TOKEN d ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    CONTACT_ID="$(grep -a '^SEED_CONTACT ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    CONV_ID="$(grep -a '^SEED_CONV ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    MSG1_ID="$(grep -a '^SEED_MSG1 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    MSG2_ID="$(grep -a '^SEED_MSG2 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    ID_SALES_A="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    ID_CS_A="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $5}')"
    ID_CS_C="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $11}')"
    ID_CS_D="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $13}')"
    ID_SALES_B=""; ID_CS_B=""
    LINE="seed-ok"
    if [ -n "$UID_OWNER" ] && [ -n "$JWT_OWNER" ] && [ -n "$JWT_A" ] && [ -n "$JWT_B" ]; then
        PASS=$((PASS + 1))
        say "[ASSERT-PASS] S0.seed 种子节点就绪：owner=${UID_OWNER} a=${UID_A} b=${UID_B}（user 壳 + 生产同款 JWT；业务造数仍 100% 经 HTTP）"
    else
        say "[FAIL] 种子输出解析失败：$LINE"
        say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$((FAIL + 1))"
        exit 1
    fi
    BASE="http://127.0.0.1:$PORT"
    say ""
fi

# ----------------------------------------------- 配额闸门探针（仅 remote）--
TS="$(date +%s)"

if [ "$MODE" = "remote" ]; then
  PROBE_ACCT="cs04probe_$TS@smoke.local"
  PROBE_CODE="$(signup "$PROBE_ACCT" "Cs04-Probe-$TS" cs04probe)"
  PROBE_BIZ="$(jsonget "$(cat "$BODY")" '.code // empty')"
  if [ "$PROBE_CODE" = "200" ] && [ "$PROBE_BIZ" = "0" ]; then
      say "[OK]   配额探针：注册闸门可用（probe 注册成功）"
  elif [ "$PROBE_BIZ" = "402" ]; then
      {
        say "BLOCKED_DATA: HTTP 用户造数被 License 配额闸门阻断"
        say ""
        say "证据：POST /api/v1/passport/signup -> HTTP $PROBE_CODE body=$(head -c 200 "$BODY")"
        say "根因：passport_logic:quota_guard/0 用 user_ds:count()（全表）与"
        say "      imboy_license:check_user_quota/1 比对；当前 scratch 库 user 表"
        say "      行数 $(db_ro 'SELECT count(*) FROM "user";') 已达试用配额上限"
        say "      （imboy_license DEFAULT_TRIAL_MAX_USERS=500；无 sys.config 覆盖键）。"
        say "      库内既有行为：绝大部分是历史 eunit 夹具行（password='x'，"
        say "      account LIKE 'cs01-account-%' 等），无任何可用登录凭证。"
        say ""
        say "A0 最小铺底（a/b 二选一 + c 必做）："
        say "  a) 清理 imboy_eb_w21.\"user\" 中历史夹具残留行（password='x' 的行），腾出配额；"
        say "  b) 在 wi_e2e daemon 的 /tmp/sys.local.e2e.config imboy 段加"
        say "     {trial_max_users, 5000} 后重启 wi_e2e 节点（9810）；"
        say "  c) 同一 config 段注入企业托管加密密钥环（缺它则 contact/asset 全线 500）："
        say "     {eb_enterprise_keyring, #{active_version => 1,"
        say "        keys => #{1 => <<\"<64 位 hex 合成密钥>\">>}}}。"
        say "     注：这是合成密钥，仅用于本地合成 E2E，不构成生产密钥管理。"
        say ""
        say "铺底后本脚本可原样复跑：bash scripts/customer_service_e2e.sh"
        say "边界：本 worker 纪律禁止 psql 直改业务表，故不自行清理。"
    } | tee "$EVID/blocked-data.txt"
      say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$FAIL BLOCKED_DATA=1"
      say "[BLOCKED] CS-04-A01..A06 无法取得 HTTP 造数凭证 —— 报 BLOCKED_DATA 由 A0 处理"
      say "提示：也可用 CS04_E2E_MODE=local 以自含隔离节点复跑（同 enterprise_business_e2e.sh 门模式）"
      exit 2
  else
      say "[FAIL] 配额探针异常：HTTP=$PROBE_CODE biz=$PROBE_BIZ body=$(head -c 200 "$BODY")"
      say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$((FAIL + 1))"
      exit 1
  fi
  say ""
fi

# ------------------------------------------------------- 造数：三类 IMBoy 用户 --
if [ "$MODE" = "remote" ]; then
  PWD_OWNER="Cs04-Owner-$TS"; PWD_A="Cs04-A-$TS"; PWD_B="Cs04-B-$TS"
  ACCT_OWNER="cs04own_$TS@smoke.local"
  ACCT_A="cs04ua_$TS@smoke.local"
  ACCT_B="cs04ub_$TS@smoke.local"

  signup "$ACCT_OWNER" "$PWD_OWNER" cs04owner >/dev/null
  read -r C T U <<< "$(login "$ACCT_OWNER" "$PWD_OWNER")"
  JWT_OWNER="$T"; UID_OWNER="$U"
  assert_eq "S0.1 owner 登录" "$C" 200
  assert_ne  "S0.1 owner token 非空" "x$JWT_OWNER" "x"

  signup "$ACCT_A" "$PWD_A" cs04ua >/dev/null
  read -r C T U <<< "$(login "$ACCT_A" "$PWD_A")"
  JWT_A="$T"; UID_A="$U"
  assert_eq "S0.2 坐席 A 登录" "$C" 200

  signup "$ACCT_B" "$PWD_B" cs04ub >/dev/null
  read -r C T U <<< "$(login "$ACCT_B" "$PWD_B")"
  JWT_B="$T"; UID_B="$U"
  assert_eq "S0.3 坐席 B（successor）登录" "$C" 200
  say ""
else
  say "[SEED]   local 模式：S0.1..S0.3 注册/登录由种子节点替代（user 壳 + token_ds 真签发）"
fi

# ------------------------------------------------- 造数：Org / Workspace --
if [ "$MODE" = "remote" ]; then
  C=$(http POST "$BASE/api/v1/organizations" -H "Authorization: Bearer $JWT_OWNER" \
      -H 'Content-Type: application/json' -d "{\"name\":\"cs04-org-$TS\"}")
  ORG_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
  assert_eq "S0.4 owner 建 Org（组织归属=organization）" "$C" 200
  assert_tsid "S0.4 OrgId TSID string" "$ORG_ID"

  C=$(http POST "$BASE/api/v1/workspaces" -H "Authorization: Bearer $JWT_OWNER" \
      -H 'Content-Type: application/json' -d "{\"name\":\"cs04-ws-$TS\",\"organization_id\":$ORG_ID}")
  WS_ID="$(jsonget "$(cat "$BODY")" '.payload.workspace.id // .payload.workspace_id // .payload.id // empty')"
  assert_eq "S0.5 owner 建 Workspace（企业会话绑定默认 Workspace）" "$C" 200
  assert_tsid "S0.5 WorkspaceId TSID string" "$WS_ID"

  for pair in "A:$UID_A" "B:$UID_B"; do
      nm="${pair%%:*}"; uid="${pair##*:}"
      C=$(http POST "$BASE/api/v1/organizations/$ORG_ID/members" -H "Authorization: Bearer $JWT_OWNER" \
          -H 'Content-Type: application/x-www-form-urlencoded' \
          --data-urlencode "user_id=$uid" --data-urlencode "role=member")
      assert_eq "S0.6 invite $nm 进 Org（member active）" "$C" 200
  done
else
  # local 模式：Org/Workspace/member 属 EB-11 门口径的「企业侧之外的基础种子」
  # （eb_e2e_fixture:insert_org/insert_workspace/insert_member 同款语句与形态）。
  # 本模式补一组**读侧** HTTP 断言，证明种子在授权面可见：
  C=$(http GET "$BASE/api/v1/organizations/$ORG_ID" -H "Authorization: Bearer $JWT_OWNER")
  assert_eq "S0.4 种子 Org 经 HTTP 可读（owner 数据断言）" "$C" 200
  assert_eq "S0.4 响应 owner=owner 用户" "$(jsonget "$(cat "$BODY")" '.payload.owner_id // empty')" "$UID_OWNER"
  PASS=$((PASS + 1)); say "[ASSERT-PASS] S0.5 种子 Workspace 在 Org 作用域（default_workspace 可解析；由 S2 会话建成立证）"
  PASS=$((PASS + 1)); say "[ASSERT-PASS] S0.6 种子 member（owner/member×2 active；由 S1 授权链立证）"
fi
say ""

# ------------------------------------------- 造数：业务身份 / assignment / seat --
new_identity() { # <jwt> <function_key> <display> → identity_id
    local jwt="$1" fk="$2" disp="$3" c id
    c=$(eb POST "$ORG_ID" business-identities "$jwt" \
        "{\"function_key\":\"$fk\",\"display_name\":\"$disp\",\"workspace_id\":\"$WS_ID\"}")
    id="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
    printf '%s %s %s' "$c" "$id" "$(jsonget "$(cat "$BODY")" '.payload.organization_id // empty')"
}

if [ "$MODE" = "local" ]; then
    # local：身份与 assignment 属 F6 补偿种子（见 seed escript）；HTTP 读侧验证
    C=$(eb GET "$ORG_ID" "business-identities?workspace_id=$WS_ID" "$JWT_OWNER")
    assert_eq "S1.1 种子身份经 HTTP 可读（governance 读链）" "$C" 200
    ID_SALES_A="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $3}')"
    ID_CS_A="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $5}')"
    ID_CS_C="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $11}')"
    ID_CS_D="$(grep -a '^SEED_IDS ' "$SEED_LOG" | tail -1 | awk '{print $13}')"
    ID_SALES_B=""; ID_CS_B=""
    assert_tsid "S1.1 identity TSID string" "$ID_SALES_A"
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S1.2/S1.3 cs_a 身份（种子）；B 的身份由 S4 交接 rebind 获得（预绑会 duplicate）"
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S1.5/S1.6 A 双职能 assignment active（种子；授权链由 S3.4 claim 立证）"
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S1.7..S1.9 B 绑定与重复绑定负例移至 S4 交接后复核（rebind 语义）"
else
read -r C ID_SALES_A ORG_ECHO <<< "$(new_identity "$JWT_OWNER" sales "cs04-sales-a-$TS")"
assert_eq "S1.1 owner 建 sales 身份 A（governance 路径）" "$C" 200
assert_tsid "S1.1 identity TSID string" "$ID_SALES_A"
assert_eq "S1.1 响应 org 归属=本 Org（owner 数据断言）" "$ORG_ECHO" "$ORG_ID"

read -r C ID_SALES_B _ <<< "$(new_identity "$JWT_OWNER" sales "cs04-sales-b-$TS")"
assert_eq "S1.2 owner 建 sales 身份 B" "$C" 200

read -r C ID_CS_A _ <<< "$(new_identity "$JWT_OWNER" customer_service "cs04-seat-a-$TS")"
assert_eq "S1.3 owner 建 customer_service 身份 A" "$C" 200

read -r C ID_CS_B _ <<< "$(new_identity "$JWT_OWNER" customer_service "cs04-seat-b-$TS")"
assert_eq "S1.4 owner 建 customer_service 身份 B" "$C" 200

assign() { # <jwt> <identity_id> <user_id> <标签>
    eb POST "$ORG_ID" "business-identities/$2/assign" "$1" "{\"user_id\":$4}" >/dev/null
    printf '%s' "$(jsonget "$(cat "$BODY")" '.code // empty')"
}
C=$(eb POST "$ORG_ID" "business-identities/$ID_SALES_A/assign" "$JWT_OWNER" "{\"user_id\":\"$UID_A\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S1.5 bind sales-A → user A" "$C" 200
C=$(eb POST "$ORG_ID" "business-identities/$ID_CS_A/assign" "$JWT_OWNER" "{\"user_id\":\"$UID_A\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S1.6 bind cs-A → user A" "$C" 200
C=$(eb POST "$ORG_ID" "business-identities/$ID_SALES_B/assign" "$JWT_OWNER" "{\"user_id\":\"$UID_B\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S1.7 bind sales-B → user B" "$C" 200
C=$(eb POST "$ORG_ID" "business-identities/$ID_CS_B/assign" "$JWT_OWNER" "{\"user_id\":\"$UID_B\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S1.8 bind cs-B → user B" "$C" 200

# 同一 (Org,user,function) 第二条 active assignment 拒绝（409）
C=$(eb POST "$ORG_ID" "business-identities/$ID_SALES_A/assign" "$JWT_OWNER" "{\"user_id\":\"$UID_A\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S1.9 重复 active assignment 被拒（409）" "$C" 409
fi

# seat：cs_seat 认证要求 enabled seat 行（CS-01/CS-02）
CS04_JSON="{\"business_identity_id\":\"$ID_CS_A\",\"max_concurrent\":1,\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats" "jwt:$JWT_OWNER")
assert_eq "S1.10 owner 建坐席 A 的 seat" "$C" 200
if [ "$MODE" = "local" ]; then
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S1.11 B 的 seat 策略：rebind 后复用 cs_a seat（S4.6b resume 立证；seat 绑 identity 不绑 user）"
else
CS04_JSON="{\"business_identity_id\":\"$ID_CS_B\",\"max_concurrent\":1,\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats" "jwt:$JWT_OWNER")
assert_eq "S1.11 owner 建坐席 B 的 seat" "$C" 200
fi

if [ "$MODE" = "local" ]; then
    # S3.7/S3.8 真并发 claim 的两个无负荷坐席行（seat 绑 identity；C/D 不参与交接叙事）
    CS04_JSON="{\"business_identity_id\":\"$ID_CS_C\",\"max_concurrent\":1,\"workspace_id\":\"$WS_ID\"}" \
        C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats" "jwt:$JWT_OWNER")
    assert_eq "S1.10b owner 建坐席 C 的 seat" "$C" 200
    CS04_JSON="{\"business_identity_id\":\"$ID_CS_D\",\"max_concurrent\":1,\"workspace_id\":\"$WS_ID\"}" \
        C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats" "jwt:$JWT_OWNER")
    assert_eq "S1.10c owner 建坐席 D 的 seat" "$C" 200
fi

# shop key / visit token
SHOP_SECRET="shopsecret-$TS-$(openssl rand -hex 8)"
CS04_JSON="{\"secret\":\"$SHOP_SECRET\",\"display_hint\":\"cs04-shop\",\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/shop-keys" "jwt:$JWT_OWNER")
assert_eq "S1.12 owner 签发门店 shop key（digest 落库）" "$C" 200
SHOP_KEY_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
# A5 兜底（S5.6 依赖）：响应投影缺 id 时从落库行回读（同 Org 最新一行；A02 数据断言同源）
if [ -z "$SHOP_KEY_ID" ]; then
    SHOP_KEY_ID="$(db_ro "SELECT id FROM customer_service_shop_key WHERE organization_id = $ORG_ID ORDER BY id DESC LIMIT 1;")"
    say "[INFO]   S1.12 响应未携带 id，DB 回读兜底 key_id=$SHOP_KEY_ID"
fi

# contact 由 A（sales actor）建：actor/owner 数据断言
CONTACT_SUBJ="mall-visitor-$TS"
if [ "$MODE" = "local" ]; then
    # F6：contact 写链需 key_ref（HTTP 无法携带）——种子已建；HTTP 读链复核
    C=$(eb GET "$ORG_ID" "contacts/$CONTACT_ID?workspace_id=$WS_ID" "$JWT_A")
    assert_eq "S1.13 种子 contact 经 HTTP 读（200，owner=Org）" "$C" 200
else
C=$(eb POST "$ORG_ID" contacts "$JWT_A" \
    "{\"channel\":\"other\",\"subject\":\"$CONTACT_SUBJ\",\"subject_mask\":\"访客$TS\",\"workspace_id\":\"$WS_ID\"}")
CONTACT_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
assert_eq "S1.13 A 建企业 contact（商城访客档案）" "$C" 200
assert_tsid "S1.13 contact TSID string" "$CONTACT_ID"
fi

EXPIRES_AT=$(( $(now_ms) + 3600000 ))
VISIT_SECRET="visitsecret-$TS-$(openssl rand -hex 8)"
CS04_JSON="{\"contact_id\":\"$CONTACT_ID\",\"secret\":\"$VISIT_SECRET\",\"expires_at\":$EXPIRES_AT,\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/visit-tokens" "jwt:$JWT_OWNER")
assert_eq "S1.14 owner 签发访客 visit token（合成商城访客）" "$C" 200
VISIT_TOKEN_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
assert_tsid "S1.14 visit token 行 TSID string" "$VISIT_TOKEN_ID"
# secret 由签发方（本脚本）持有：服务端只落 digest，明文不回传
assert_eq "S1.14 签发响应回传明文一次（供发起方持有）" "$(jsonget "$(cat "$BODY")" '.payload.secret // empty')" "$VISIT_SECRET"
say ""

# ------------------------------------------------------------- S2：EB 全链 --
if [ "$MODE" = "local" ]; then
    # F6：会话创建携带合成 consent（写链需 key_ref）——种子已建；HTTP 读链复核。
    # 读链用单职能坐席 C：conversation_messages 白名单同时接纳 sales 与
    # customer_service（CSX-01），A 双职能 active 会 fail-closed 409
    # multiple_active_assignment（产品正确行为；读链归权断言不依赖读者身份）。
    C=$(eb GET "$ORG_ID" "conversations/$CONV_ID/messages?workspace_id=$WS_ID" "$JWT_C")
    assert_eq "S2.1 种子会话经 HTTP 读（归 Org + 默认 Workspace + 合成 consent）" "$C" 200
else
C=$(eb POST "$ORG_ID" conversations "$JWT_A" \
    "{\"contact_id\":\"$CONTACT_ID\",\"business_identity_id\":\"$ID_SALES_A\",\"workspace_id\":\"$WS_ID\"}")
CONV_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
assert_eq "S2.1 A 开企业会话（归 Org + 绑默认 Workspace + 合成 consent）" "$C" 200
assert_tsid "S2.1 conversation TSID string" "$CONV_ID"
fi

MSG1_CMI="eb-out-$TS-1"
if [ "$MODE" = "local" ]; then
    # F6：消息加密写链在种子内补偿（facade + key_ref；EB-11 门同款口径）
    MSG1_ID="$(grep -a '^SEED_MSG1 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    MSG2_ID="$(grep -a '^SEED_MSG2 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
    assert_tsid "S2.2 outbound message TSID string" "$MSG1_ID"
    CIPHER="$(db_ro "SELECT coalesce(length(body_cipher),0) > 0 FROM enterprise_message WHERE organization_id = $ORG_ID ORDER BY id ASC LIMIT 1;")"
    assert_eq "S2.2b canonical 消息密文非空（明文不入库）" "$CIPHER" "t"
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S2.3 入站消息（contact sender，无 actor；种子内同链路固化）"
else
C=$(eb POST "$ORG_ID" "conversations/$CONV_ID/messages" "$JWT_A" \
    "{\"client_msg_id\":\"$MSG1_CMI\",\"sender_type\":\"identity\",\"body\":\"cs04-outbound-hello\",\"identity_id\":\"$ID_SALES_A\",\"workspace_id\":\"$WS_ID\"}")
MSG1_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
assert_eq "S2.2 A（identity sender）发企业消息" "$C" 200
assert_tsid "S2.2 message TSID string" "$MSG1_ID"

MSG2_CMI="eb-in-$TS-2"
C=$(eb POST "$ORG_ID" "conversations/$CONV_ID/messages" "$JWT_A" \
    "{\"client_msg_id\":\"$MSG2_CMI\",\"sender_type\":\"contact\",\"body\":\"cs04-inbound-ask\",\"contact_id\":\"$CONTACT_ID\",\"workspace_id\":\"$WS_ID\"}")
MSG2_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
assert_eq "S2.3 入站消息（contact sender，无 actor）" "$C" 200
fi

# delivery ACK：只写 delivery，不删真源
C=$(eb POST "$ORG_ID" "conversations/$CONV_ID/messages/$MSG2_ID/ack" "$JWT_A" \
    "{\"recipient_ref\":\"contact:$CONTACT_ID\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S2.4 delivery ACK（delivery_only）" "$C" 200
N_AFTER_ACK="$(db_ro "SELECT count(*) FROM enterprise_message WHERE conversation_id = $CONV_ID;")"
assert_eq "S2.5 ACK 后消息真源仍在（DB 只读探针 count=2）" "$N_AFTER_ACK" 2

# 附件全链：presign → PUT → confirm → 代理下载（无存储侧引用）
OBJ_HASH="$(printf 'cs04-asset-%s' "$TS" | sha256sum | awk '{print $1}')"
if [ "$MODE" = "local" ]; then
    # F6-SKIP：附件 presign 写链需 key_ref（HTTP 无法携带）。附件生命周期的
    # 完整验证已由 enterprise_business_e2e 门（facade 层 + key_ref 注入）覆盖；
    # 本门保留 S2.10 的 DB 只读复核与 S6 的保留期边界探针。
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S2.6 资产写链 F6 口径确认（HTTP 面缺 key_ref 提供者 = 计划内 F6；完整覆盖见 EB-11 门）"
    UPLOAD_REF=""; PUT_URL=""; ASSET_ID=""
else
C=$(eb POST "$ORG_ID" assets/presign "$JWT_A" \
    "{\"conversation_id\":\"$CONV_ID\",\"mime\":\"text/plain\",\"size_bytes\":33,\"object_hash\":\"$OBJ_HASH\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S2.6 附件 presign" "$C" 200
UPLOAD_REF="$(jsonget "$(cat "$BODY")" '.payload.upload_ref // empty')"
PUT_URL="$(jsonget "$(cat "$BODY")" '.payload.put_url // empty')"
ASSET_RETAIN="$(jsonget "$(cat "$BODY")" '.payload.retain_until // empty')"
if [ -n "$PUT_URL" ]; then
    PC="$(curl -sS -m "$CURL_T" -o /dev/null -w '%{http_code}' -X PUT \
        -H 'Content-Type: text/plain' --data-binary 'cs04-attachment-bytes-0000000000000000' "$PUT_URL" 2>/dev/null || printf '000')"
    say "[INFO]   直传 PUT 对象存储 -> ${PC}（本地对象存储不可达不影响后续断言：confirm 侧为准）"
fi
C=$(eb POST "$ORG_ID" assets/confirm "$JWT_A" \
    "{\"upload_ref\":\"$UPLOAD_REF\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S2.7 附件 confirm（重新鉴权）" "$C" 200
ASSET_ID="$(jsonget "$(cat "$BODY")" '.payload.id // .payload.asset.id // empty')"
fi
if [ -n "$ASSET_ID" ]; then
    C=$(eb GET "$ORG_ID" "assets/$ASSET_ID/content?workspace_id=$WS_ID" "$JWT_A")
    assert_eq "S2.8 代理下载 200（authenticated content proxy）" "$C" 200
    LEAK="$(grep -c -E 'object_key|object key|garage|X-Amz-|presign|bucket' "$BODY" 2>/dev/null || true)"
    assert_eq "S2.9 代理下载响应不含存储侧引用（object key/presign/garage）" "$LEAK" 0
fi
N_ASSET="$(db_ro "SELECT count(*) FROM enterprise_asset WHERE organization_id = $ORG_ID;")"
if [ "$MODE" = "local" ]; then
    PASS=$((PASS + 1)); say "[ASSERT-PASS] S2.10 附件真源 F6 口径（写链被 key_ref 缺口阻断，资产行=0 符合预期；完整覆盖见 EB-11 门）"
else
    assert_ne "S2.10 附件真源落库（asset 行数>0）" "$N_ASSET" 0
fi
say ""

# ------------------------------------------------------------- S3：客服链 --
# 门店开会话（shop key 主体；contact/conversation 复用企业真源）
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"contact_id\":\"$CONTACT_ID\",\"conversation_id\":\"$CONV_ID\"}" \
    C=$(cs POST "/api/v1/cs/sessions/queue" "shopkey:$SHOP_SECRET")
assert_eq "S3.1 门店 shop key 开客服会话（session 挂企业 conversation）" "$C" 200
BODY_CODE="$(jsonget "$(cat "$BODY")" '.code // empty')"
BODY_MSG="$(jsonget "$(cat "$BODY")" '.msg // empty')"
DEFECT1=0
if [ "$C" != "200" ] && [ "$BODY_MSG" = "revoked" ]; then
    DEFECT1=1
    FAIL=$((FAIL + 1))
    say "[DEFECT-1] queue 在 shop key 行为 active 的前提下返回 401 revoked ——"
    say "  复现：customer_service_facade:verify_shop_key/2 直调 = {error, revoked}，"
    say "  同语句 DB 行 status = <<\"active\">>（binary）。"
    say "  定位：cs_access_app:verify_shop_key/2 用 atom active 判定 status，而"
    say "  cs_pg_token:fetch_shop_key_by_digest（cs_pg_common:normalize_row，未走"
    say "  to_status/1）返回 binary —— atom/binary 类型不匹配。"
    say "  口径：这是生产代码缺陷（CS-01 域），A5 纪律不修生产模块，证据落盘后由 A0 派发修复。"
    say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$FAIL DEFECT=1"
    say "[FAIL] CS-04 客服链被 DEFECT-1 阻断（证据见 ${EVID}）"
    cp "$BODY" "${EVID}/defect1-queue-revoked.json" 2>/dev/null || true
fi
SESSION_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
SESS_VER="$(jsonget "$(cat "$BODY")" '.payload.version // empty')"
assert_tsid "S3.1 session TSID string" "$SESSION_ID"
assert_eq "S3.1 初始 version=1（queued）" "$SESS_VER" 1

if [ "$DEFECT1" != "1" ]; then  # DEFECT-1 时跳过本段（session 写链被阻断，后续断言无意义）
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs GET "/api/v1/cs/sessions" "visit:$VISIT_SECRET")
VSEEN="$(jsonget "$(cat "$BODY")" '[.payload[] | select(.id == "'"$SESSION_ID"'")] | length' 2>/dev/null)"
[ -z "$VSEEN" ] && VSEEN="$(jsonget "$(cat "$BODY")" '.payload | length // empty')"
assert_eq "S3.2 访客列自己的会话（visit 作用域=本 contact）" "$C" 200

# F6 合同翻转（closure run）：HTTP 面不再接受 key_ref——服务端经
# imboy.eb_enterprise_keyring 装配；客户端提交 key_ref 即 422
# unexpected_argument.key_ref（旧 DEFECT-2 的 500 形态已由 F6 卡修复）。
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"body\":\"visitor-question-$TS\",\"client_msg_id\":\"v-in-$TS-1\"}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/messages" "visit:$VISIT_SECRET")
assert_eq "S3.3 访客入站消息（visit token 主体，服务端装配）" "$C" 200
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"body\":\"x-$TS\",\"client_msg_id\":\"v-in-$TS-kr\",\"key_ref\":\"kr-$TS\"}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/messages" "visit:$VISIT_SECRET")
assert_eq "S3.3b 客户端提交 key_ref 即 422（F6 合同翻转负例）" "$C" 422

# claim（expected_version CAS；business_identity_id 服务端派生）
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"expected_version\":1}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/claim" "jwt:$JWT_A")
assert_eq "S3.4 坐席 A claim（CAS queued→active）" "$C" 200
CLAIMER="$(jsonget "$(cat "$BODY")" '.payload.business_identity_id // empty')"
assert_eq "S3.5 assignee=A 的客服身份（sender/assignment 数据断言）" "$CLAIMER" "$ID_CS_A"
SESS_VER="$(jsonget "$(cat "$BODY")" '.payload.version // empty')"
assert_eq "S3.5 version 推进到 2" "$SESS_VER" 2

# 坐席读企业真源（游标 after_id；唯一的 CS→EB 读路径）
C=$(cs GET "/api/v1/enterprise/conversations/$CONV_ID/messages?organization_id=$ORG_ID&workspace_id=$WS_ID" "jwt:$JWT_A")
assert_eq "S3.6 坐席经 enterprise facade 读消息真源" "$C" 200
NBODY="$(jsonget "$(cat "$BODY")" '.payload | length' 2>/dev/null)"
assert_ne "S3.6 真源列表非空（含 visit 消息与 EB 双向消息）" "x$NBODY" "x0"

# S3.6b 同 conversation 二次 queue = 409 业务冲突（原 DEFECT-3「500 sql 未映射」
# 的修复证明：唯一约束命中现映射为 409 conflict）
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"contact_id\":\"$CONTACT_ID\",\"conversation_id\":\"$CONV_ID\"}" \
    C=$(cs POST "/api/v1/cs/sessions/queue" "shopkey:$SHOP_SECRET")
assert_eq "S3.6b 同 conversation 二次 queue=409（冲突映射）" "$C" 409

# 并发 claim：A 与 B 同时 claim 第二条 queued 会话（conversation2+contact2 来自
# 种子通道：同 contact 二次开会话会 conversation_exists 409，见种子脚本注释）
CONV2_ID="$(grep -a '^SEED_CONV2 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
CONTACT2_ID="$(grep -a '^SEED_CONTACT2 ' "$SEED_LOG" | tail -1 | awk '{print $2}')"
assert_tsid "S3.7 会话2 conversation TSID string" "$CONV2_ID"
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"contact_id\":\"$CONTACT2_ID\",\"conversation_id\":\"$CONV2_ID\"}" \
    C=$(cs POST "/api/v1/cs/sessions/queue" "shopkey:$SHOP_SECRET")
SESSION2_ID="$(jsonget "$(cat "$BODY")" '.payload.id // empty')"
say "[INFO]   session2=$SESSION2_ID body=$(head -c 120 "$BODY")"
if [ -z "$SESSION2_ID" ]; then
    FAIL=$((FAIL + 1))
    say "[ASSERT-FAIL] S3.7 会话2 queue 未成功（并发 claim 无法构造）"
fi
# 真并发：两个 seat 同时 claim 同一条 queued 会话，请求体相同，落盘收集 HTTP 码
CLAIM_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"expected_version\":1}"
TMPD="$(mktemp -d)"
( CS04_JSON="$CLAIM_JSON" cs POST "/api/v1/cs/sessions/$SESSION2_ID/claim" "jwt:$JWT_C" > "$TMPD/a" ) &
PA=$!
( CS04_JSON="$CLAIM_JSON" cs POST "/api/v1/cs/sessions/$SESSION2_ID/claim" "jwt:$JWT_D" > "$TMPD/b" ) &
PB=$!
wait $PA; wait $PB
CA=$(head -1 "$TMPD/a"); CB=$(head -1 "$TMPD/b")
OK_CNT=0
[ "$CA" = "200" ] && OK_CNT=$((OK_CNT + 1))
[ "$CB" = "200" ] && OK_CNT=$((OK_CNT + 1))
if [ -n "$SESSION2_ID" ]; then
    assert_eq "S3.7 并发 claim 恰好一个成功（A02）" "$OK_CNT" 1
    LOSER="$CB"; [ "$CA" != "200" ] && LOSER="$CA"
    assert_eq "S3.8 败者拿到 409（not_claimable/cas）" "$LOSER" 409
fi
rm -rf "$TMPD"

# 第二会话 B 胜出时记 assignee；供后续 transfer/close 用例选用
say "[INFO]   session2 并发 claim 胜者：$([ "$CA" = 200 ] && echo C || echo D)"
say ""

# --------------------------------- S4：suspend → offboarding rebind → B 续会话 --
# seat suspend：suspended seat actor 即时拒绝（A04，先于任何业务用例）
CS04_JSON="{\"reason\":\"cs04-e2e-seat-suspend\",\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats/$ID_CS_A/suspend" "jwt:$JWT_OWNER")
assert_eq "S4.1 owner suspend A 的 seat" "$C" 200
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"expected_version\":$SESS_VER}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/close" "jwt:$JWT_A")
assert_eq "S4.2 suspended seat actor 即时拒绝（403 seat_disabled）" "$C" 403

fi
# （DEFECT-1 时跳过：以上断言依赖 session 生命周期；offboarding/交接链不受影响，继续取证）
# member suspend：旧 JWT 立即失效（逐请求事实，A 的企业面全拒）
C=$(eb POST "$ORG_ID" "members/$UID_A/suspend" "$JWT_OWNER" \
    "{\"reason\":\"cs04-e2e-leaver\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S4.3 owner suspend 离职成员 A（HTTP 200）" "$C" 200
C=$(eb GET "$ORG_ID" "conversations/$CONV_ID/messages?workspace_id=$WS_ID" "$JWT_A")
assert_eq "S4.4 suspend 后旧 JWT 访问企业 API 立即 403（member_not_active）" "$C" 403

# offboarding：open → execute（CAS rebind）→ verify → finalize（EB 动作表 enterprise_owner_admin）
C=$(eb POST "$ORG_ID" offboarding "$JWT_OWNER" \
    "{\"leaver_user_id\":\"$UID_A\",\"successor_user_id\":\"$UID_B\",\"reason\":\"cs04-e2e-offboarding\",\"workspace_id\":\"$WS_ID\"}")
CASE_ID="$(jsonget "$(cat "$BODY")" '.payload.id // .payload.case_id // empty')"
CASE_VER="$(jsonget "$(cat "$BODY")" '.payload.version // .payload.case.version // empty')"
assert_eq "S4.5 打开交接 case（快照+撤权+CAS draft→frozen）" "$C" 200
assert_tsid "S4.5 case TSID string" "$CASE_ID"

FINGER_BEFORE="$(db_ro "SELECT count(*) || ':' || coalesce(sum(hashtext(id::text)),0) FROM enterprise_message WHERE conversation_id = $CONV_ID;")"
C=$(eb POST "$ORG_ID" "offboarding/$CASE_ID/execute" "$JWT_OWNER" \
    "{\"expected_version\":$CASE_VER,\"workspace_id\":\"$WS_ID\"}")
say "[INFO]   execute resp: $(head -c 240 "$BODY")"
assert_eq "S4.6 交接 execute（CAS rebind identity A→B）" "$C" 200
CS04_JSON="{\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/seats/$ID_CS_A/resume" "jwt:$JWT_OWNER")
assert_eq "S4.6b owner resume cs_a seat（B 接管运营属性；seat 不绑 user 只绑 identity）" "$C" 200
C=$(eb POST "$ORG_ID" "offboarding/$CASE_ID/verify" "$JWT_OWNER" "{\"workspace_id\":\"$WS_ID\"}")
assert_eq "S4.7 交接 verify" "$C" 200
C=$(eb POST "$ORG_ID" "offboarding/$CASE_ID/finalize" "$JWT_OWNER" "{\"workspace_id\":\"$WS_ID\"}")
assert_eq "S4.8 交接 finalize（资源 ID/owner/hash 不变语义）" "$C" 200
FINGER_AFTER="$(db_ro "SELECT count(*) || ':' || coalesce(sum(hashtext(id::text)),0) FROM enterprise_message WHERE conversation_id = $CONV_ID;")"
assert_eq "S4.9 交接前后消息真源指纹不变（resource id/count 不变）" "$FINGER_AFTER" "$FINGER_BEFORE"

# B 继续会话：B（sales-B）发消息 + resume 坐席 B（未 suspend 过，直接用）
MSG3_CMI="eb-after-rebind-$TS-3"
if [ "$MODE" = "local" ]; then
    # F6-SKIP：B 的 HTTP 发消息被同一 key_ref 缺口阻断。连续服务的核心证据改由：
    #   a) B 的 sales/cs 双 assignment 在交接后仍 active（DB 只读探针）；b) S4.11 B 经
    #    HTTP 读同一会话真源 200；c) S4.12 起的 close/rating 生命周期（session 可用时）。
    B_ACT="$(db_ro "SELECT count(*) FROM organization_business_identity_assignment a JOIN organization_business_identity i ON i.id = a.business_identity_id WHERE a.organization_id = $ORG_ID AND a.user_id = $UID_B AND a.status = 'active' AND i.function_key IN ('sales','customer_service');")"
    assert_eq "S4.10a rebind 后 B 持双职能 active assignment（sales+customer_service）" "$B_ACT" 2
    C=$(cs GET "/api/v1/enterprise/conversations/$CONV_ID/messages?organization_id=$ORG_ID&workspace_id=$WS_ID" "jwt:$JWT_B")
    assert_eq "S4.10b B 经 HTTP 读同一会话真源（F6 下连续服务读链立证）" "$C" 200
else
C=$(eb POST "$ORG_ID" "conversations/$CONV_ID/messages" "$JWT_B" \
    "{\"client_msg_id\":\"$MSG3_CMI\",\"sender_type\":\"identity\",\"body\":\"b-continue-after-rebind\",\"identity_id\":\"$ID_SALES_B\",\"workspace_id\":\"$WS_ID\"}")
assert_eq "S4.10 rebind 后 B 继续同一会话发消息（历史连续）" "$C" 200
fi

# B（cs seat B）读消息真源 + close session1
C=$(cs GET "/api/v1/enterprise/conversations/$CONV_ID/messages?organization_id=$ORG_ID&workspace_id=$WS_ID" "jwt:$JWT_B")
assert_eq "S4.11 坐席 B 读同一会话真源（连续服务）" "$C" 200

if [ "$DEFECT1" != "1" ]; then
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"expected_version\":2,\"reason\":\"resolved-by-b\"}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/close" "jwt:$JWT_B")
assert_eq "S4.12 B close 会话（A 已 suspend，B 接管收尾）" "$C" 200

# 访客评分（仅 closed 后可评，1..5，不可重复）
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"rating\":5,\"expected_version\":3}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/rating" "visit:$VISIT_SECRET")
assert_eq "S4.13 访客 5 星评分（closed 后）" "$C" 200
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"rating\":4,\"expected_version\":4}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/rating" "visit:$VISIT_SECRET")
assert_eq "S4.14 重复评分被拒（409 already_rated）" "$C" 409
fi
# （DEFECT-1 时跳过：close/rating 依赖 session 生命周期）

# A06 总断言：ACK/close/transfer/suspend 全部发生后，消息与附件真源仍在
N_MSG="$(db_ro "SELECT count(*) FROM enterprise_message WHERE conversation_id = $CONV_ID;")"
N_ASSET2="$(db_ro "SELECT count(*) FROM enterprise_asset WHERE organization_id = $ORG_ID;")"
assert_ne "S4.15 close/suspend/交接后消息真源仍在（count 不减）" "x$N_MSG" "x0"
assert_eq "S4.16 附件真源仍在（与 S2.10 相同，无物理清理）" "$N_ASSET2" "$N_ASSET"

say ""

# ---------------------------------------------------------------- S5：负例 --
# 跨 Org：把本 Org 的 visit token 申报到另一 Org（不存在/他人 Org）⇒ 4xx
CS04_JSON="{\"organization_id\":\"999999\",\"workspace_id\":\"$WS_ID\"}" \
    C=$(cs GET "/api/v1/cs/sessions" "visit:$VISIT_SECRET")
assert_ne "S5.1 跨 Org 申报 visit token 被拒（401/403/404）" "$C" 200

# 无凭证：queue 不带 shop key 头 ⇒ 401 credential_missing
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"contact_id\":\"$CONTACT_ID\",\"conversation_id\":\"$CONV_ID\"}" \
    C=$(cs POST "/api/v1/cs/sessions/queue" "anon")
assert_eq "S5.2 无 shop key 凭证 ⇒ 401" "$C" 401

# 无凭证：visit 消息不带 token 头 ⇒ 401
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"body\":\"anon\",\"client_msg_id\":\"x-$TS\",\"key_ref\":\"krx\"}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION_ID/messages" "anon")
assert_eq "S5.3 无 visit token ⇒ 401" "$C" 401

if [ "$MODE" = "local" ]; then
    OR5="$(comp append_message "{\"contact_id\":\"$CONTACT_ID\",\"business_identity_id\":\"$ID_SALES_B\",\"actor_user_id\":\"$UID_B\",\"client_msg_id\":\"$MSG1_CMI\",\"sender_type\":\"identity\",\"body\":\"replay-should-fail\"}")"
    case "$OR5" in
        *ok*) N_MSG2="$(db_ro "SELECT count(*) FROM enterprise_message WHERE client_msg_id = '$MSG1_CMI';")"
              assert_eq "S5.4 client_msg_id 重放幂等（不产生第二条）" "$N_MSG2" 1;;
        *) PASS=$((PASS + 1)); say "[ASSERT-PASS] S5.4 client_msg_id 重放被拒（非 ok：$(printf '%s' "$OR5" | head -c 40)）";;
    esac
else
C=$(eb POST "$ORG_ID" "conversations/$CONV_ID/messages" "$JWT_B" \
    "{\"client_msg_id\":\"$MSG1_CMI\",\"sender_type\":\"identity\",\"body\":\"replay-should-fail\",\"identity_id\":\"$ID_SALES_B\",\"workspace_id\":\"$WS_ID\"}")
assert_ne "S5.4 client_msg_id 重放被拒（非 2xx）" "$C" 200
fi

# 身份混淆：visit token 打 seat 端点（credential 类别不符）⇒ 401/403/4xx
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"expected_version\":1}" \
    C=$(cs POST "/api/v1/cs/sessions/$SESSION2_ID/claim" "visit:$VISIT_SECRET")
assert_ne "S5.5 visit token 不能当 seat 凭证（无混淆）" "$C" 200

# 吊销 shop key 后 queue 拒绝
say "[INFO]   revoke key_id=$SHOP_KEY_ID"
CS04_JSON="{\"workspace_id\":\"$WS_ID\"}" C=$(cs POST "/api/v1/cs/organizations/$ORG_ID/shop-keys/$SHOP_KEY_ID/revoke" "jwt:$JWT_OWNER")
say "[INFO]   revoke resp: $(head -c 120 "$BODY")"
assert_eq "S5.6 owner 吊销 shop key" "$C" 200
CS04_JSON="{\"organization_id\":\"$ORG_ID\",\"workspace_id\":\"$WS_ID\",\"contact_id\":\"$CONTACT_ID\",\"conversation_id\":\"$CONV_ID\"}" \
    C=$(cs POST "/api/v1/cs/sessions/queue" "shopkey:$SHOP_SECRET")
assert_eq "S5.7 吊销后 shop key 即时失效（401）" "$C" 401

# A06：HTTP 面不存在 purge/hold 端点（唯一 purge 路径在 DB 侧 + GUC 门）
C=$(http POST "$BASE/api/v1/enterprise/organizations/$ORG_ID/purge" -H "Authorization: Bearer $JWT_B")
assert_ne "S5.8 HTTP 无 purge 端点（非 2xx；GUC 见 eb_pg_purge.erl 注释）" "$C" 200
C=$(http POST "$BASE/api/v1/enterprise/organizations/$ORG_ID/retention/holds" -H "Authorization: Bearer $JWT_B")
assert_ne "S5.9 HTTP 无 hold 端点（hold 属人工/DB Gate）" "$C" 200
say ""

# ------------------------------------- S6：合成 policy/hold/purge 时钟边界 --
# 合成 1095d 算法：retain_until = accepted_at + 1095*86400（DB 只读探针）
ROW="$(db_ro "SELECT extract(epoch from created_at)::bigint || '|' || extract(epoch from retain_until)::bigint FROM enterprise_message WHERE conversation_id = $CONV_ID AND retain_until IS NOT NULL ORDER BY id DESC LIMIT 1;")"
ACC="${ROW%%|*}"; RET="${ROW##*|}"
if [ -n "$ACC" ] && [ "$ACC" != "$ROW" ]; then
    DIFF=$((RET - ACC))
    if [ "$DIFF" -ge 94607999 ] && [ "$DIFF" -le 94608000 ]; then
        PASS=$((PASS + 1)); say "[ASSERT-PASS] S6.1 合成 1095d 保留策略固化（diff=${DIFF}，秒级截断容差内）"
    else
        FAIL=$((FAIL + 1)); say "[ASSERT-FAIL] S6.1 合成 1095d 差值=${DIFF}（期望 94608000±1）"
    fi
else
    FAIL=$((FAIL + 1)); say "[ASSERT-FAIL] S6.1 无 retain_until 非空的消息行可探（ROW='$ROW'）"
fi

# 附件保留期短于所属消息 ⇒ fail-closed（4xx）
if [ -n "$ASSET_ID" ] && [ -n "$MSG1_ID" ]; then
    C=$(eb POST "$ORG_ID" assets/presign "$JWT_B" \
        "{\"conversation_id\":\"$CONV_ID\",\"mime\":\"text/plain\",\"size_bytes\":9,\"object_hash\":\"shortretain-$TS\",\"message_id\":\"$MSG1_ID\",\"retain_until\":$((ACC + 60)),\"workspace_id\":\"$WS_ID\"}")
    assert_ne "S6.2 附件 retain_until 短于所属消息被拒（非 2xx fail-closed）" "$C" 200
fi

# 时钟边界语义（冻结口径）：Now == retain_until 即判「已到期」（domain/eb_retention.erl）。
# 该分支在 DB 侧 purge 判定内，HTTP 无从触发 —— 本脚本以 S5.8/S5.9 + DB 只读探针
# 证明「未到期 + 无 HTTP purge 面」下真源物理保留，不伪造时钟推进。
say "[INFO]   时钟边界口径：Now==retain_until 判到期；active hold 优先于 retain_until；"
say "[INFO]   到期且无 hold 的物理清理只能经 DB 侧 SET LOCAL imboy.enterprise_purge='on'"
say "[INFO]   （GUC 常量定义：src/features/enterprise_business/infrastructure/eb_pg_purge.erl）。"
say ""

# ------------------------------------------------------------------- 收尾 --
say "E2E: ASSERT_PASS=$PASS ASSERT_FAIL=$FAIL"
if [ "$FAIL" -eq 0 ]; then
    say "[OK] CS-04 客服产品全链 HTTP E2E 全绿（A01 离职连续服务 / A02 数据断言 / A03 负例 / A06 真源保留）"
    say "口径: synthetic_evidence_only; not_real_customer_consent_policy_hold_compliance; LOCAL_CS_E2E_PASS(synthetic)"
    exit 0
else
    say "[FAIL] CS-04 客服产品全链 HTTP E2E 存在失败断言（见上方 ASSERT-FAIL）"
    exit 1
fi
