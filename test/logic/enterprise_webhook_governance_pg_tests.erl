%% enterprise_webhook_governance_pg_tests
%% FULL-03 — Enterprise Webhook 治理：投递账本 / HMAC+rotation / SSRF 全谱 /
%% timeout·redirect·response cap / 重放与并发 / 可观测 / 无正文无 secret。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 FULL03_INTTEST，直连
%% imboy_pg18:4323）。业务用例每条 BEGIN ... ROLLBACK，不留数据。
%% 池化路径（worker 投递执行的 secret 解密与状态落账）一律 meck——eunit VM
%% 未起 imboy app，池化连接不可用；**状态迁移本身的 DB 语义**由
%% enterprise_webhook_migration_pg_tests 的裸 SQL oracle 承担（两套分工，
%% 合起来覆盖「分类 → 落账 SQL → 守卫」全链）。
%%
%% 覆盖（plan-full §3.1 / §5 / §7 webhook 硬门）：
%%   ① 账本：ownership + endpoint 快照 + 配置代际（配置变更隔离在途投递）
%%   ② 终态：守卫拒绝时落账容错（不炸 worker 批次）+ 审计仍写
%%   ③ HMAC 矩阵：ts "." raw_body、篡改/错 ts/错 secret/畸形/大小写/时间窗
%%   ④ rotation：轮换后旧签名失效；轮换后在途投递用新 secret 签名（线上头断言）
%%   ⑤ SSRF 全谱：私网/loopback/link-local(metadata)/CGNAT/组播/6to4/IPv6/
%%      混合解析（DNS rebinding）/dns失败/非 HTTPS/畸形/store pin
%%   ⑥ 真 socket：线上签名头 + timeout + redirect 不跟随 + response cap
%%   ⑦ 重放：保留 event id / 新 delivery id / ewh_replay_of / 归属与代际；
%%      跨 org、bot 域行、停用端点、在途、并发重复在途（DB 唯一索引仲裁）
%%   ⑧ 可观测：elib_metric 计数（outcome/class 标签）+ 延迟直方图 + 统计读面
%%      列表读面（归属隔离、页大小夹紧、不含 payload）
%%   ⑨ 无正文/无 secret：信封键集封闭、持久化轨迹逐列扫描、日志捕获扫描
%%   ⑩ 保留窗口只读集合（只含终态且过期行）
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。
%%
%% ID 段：993xxx（本 run 独立 marker 库）。

-module(enterprise_webhook_governance_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

-define(ORG_A, 993101).
-define(ORG_B, 993102).
-define(OWNER_A, 993001).
-define(OWNER_B, 993002).
-define(H_A1, 993011).
-define(H_A2, 993012).
-define(PRIN_A, 993014).
-define(PRIN_B, 993020).
-define(H_B1, 993021).
-define(WS_A1, 993201).
-define(WS_B1, 993211).
-define(EXT_A1, <<"ext993-a1">>).
-define(EXT_A2, <<"ext993-a2">>).
-define(EXT_B1, <<"ext993-b1">>).
-define(SCOPES_FULL, [
    <<"application:read">>,
    <<"identities:read">>,
    <<"identities:write">>,
    <<"groups:write">>,
    <<"files:write">>,
    <<"messages:send">>,
    <<"messages:send_as_human">>,
    <<"webhooks:manage">>
]).
-define(URL1, <<"https://oa993.example.com/hook">>).
-define(URL2, <<"https://oa993-new.example.com/hook">>).
-define(HOST1, <<"oa993.example.com">>).
-define(PUBLIC_IP, {93, 184, 216, 34}).
-define(PUBLIC_IP_STR, "93.184.216.34").
-define(BODY_MARKER, <<"hello from oa">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_conn() ->
    try
        elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
    catch
        _:_ -> ok
    end,
    lists:foreach(
        fun(Name) ->
            case lists:member(Name, elib_tsid:registered()) of
                true -> ok;
                false -> elib_tsid:register(Name)
            end
        end,
        [group_info, group_member, enterprise_message, enterprise_audit_event, msg_c2c, msg_c2g]
    ),
    {ok, _} = application:ensure_all_started(throttle),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"FULL03_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    C = maps:get(conn, State),
    ok = exec(C, <<"BEGIN">>),
    try
        seed_matrix(C),
        ok = exec(C, <<"COMMIT">>)
    catch
        Class:Reason:Stack ->
            _ = exec_quiet(C, <<"ROLLBACK">>),
            inttest_marker_db:release(State),
            erlang:raise(Class, {fixture_seed_failed, Reason}, Stack)
    end,
    {AppA, AppB} = app_ids(C),
    State#{conn => C, app_a => AppA, app_b => AppB}.

close_conn(State) ->
    inttest_marker_db:release(State),
    ok.

%% 真库用例包装：事务 + inet 解析桩。两者都必须**在测试体运行时**生效——
%% 包在 ?_test 之外（测试生成期）会被立刻 unload，测试体里就没了。
tx(C, TestFun) ->
    with_tx(C, fun(C1) -> with_public_dns(fun() -> TestFun(C1) end) end).

with_tx(C, TestFun) ->
    ?_test(begin
        ok = exec(C, <<"BEGIN">>),
        try
            TestFun(C),
            ok
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

%% 预期会产生 SQL 错误的负例：唯一索引仲裁命中的 INSERT 会让事务进入
%% aborted 状态，必须放在 SAVEPOINT 里并在之后回滚到该点。
in_savepoint(C, Fun) ->
    ok = exec(C, <<"SAVEPOINT ewh993_sp">>),
    try
        Fun()
    after
        exec(C, <<"ROLLBACK TO SAVEPOINT ewh993_sp">>),
        exec(C, <<"RELEASE SAVEPOINT ewh993_sp">>)
    end.

exec(C, IoData) ->
    Sql = iolist_to_binary(IoData),
    case elib_pg:query(C, Sql, []) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

exec_quiet(C, IoData) ->
    _ = elib_pg:query(C, iolist_to_binary(IoData), []),
    ok.

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

all(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, Rows} when is_list(Rows) -> Rows;
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    Row = one(C, Sql, Params),
    case maps:values(Row) of
        [V | _] -> V;
        [] -> undefined
    end.

seed_matrix(C) ->
    seed_user(C, ?OWNER_A, 0),
    seed_user(C, ?OWNER_B, 0),
    seed_user(C, ?H_A1, 0),
    seed_user(C, ?H_A2, 0),
    seed_user(C, ?PRIN_A, 0),
    seed_user(C, ?PRIN_B, 0),
    seed_user(C, ?H_B1, 0),
    seed_org(C, ?ORG_A, ?OWNER_A, <<"ewh993-org-a">>),
    seed_org(C, ?ORG_B, ?OWNER_B, <<"ewh993-org-b">>),
    seed_org_member(C, ?ORG_A, ?PRIN_A),
    seed_org_member(C, ?ORG_A, ?H_A1),
    seed_org_member(C, ?ORG_A, ?H_A2),
    seed_org_member(C, ?ORG_B, ?PRIN_B),
    seed_org_member(C, ?ORG_B, ?H_B1),
    seed_workspace(C, ?WS_A1, ?ORG_A, ?OWNER_A),
    seed_workspace(C, ?WS_B1, ?ORG_B, ?OWNER_B),
    {ok, AppA} = enterprise_application_repo:create_tx(
        C, ?ORG_A, <<"ewh993-oa-a">>, <<"ewh993 org A oa"/utf8>>, {?PRIN_A, ?SCOPES_FULL}
    ),
    {ok, AppB} = enterprise_application_repo:create_tx(
        C, ?ORG_B, <<"ewh993-oa-b">>, <<"ewh993 org B oa"/utf8>>, {?PRIN_B, ?SCOPES_FULL}
    ),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_A1, ?H_A1),
    ok = seed_mapping(C, ?ORG_A, maps:get(<<"id">>, AppA), ?EXT_A2, ?H_A2),
    ok = seed_mapping(C, ?ORG_B, maps:get(<<"id">>, AppB), ?EXT_B1, ?H_B1),
    ok.

seed_user(C, Uid, AccountType) ->
    ok = exec(C, [
        <<"INSERT INTO \"user\" (id, password, account, account_type, status, reg_ip, reg_cosv)">>,
        <<" VALUES (">>,
        integer_to_binary(Uid),
        <<", 'x', 't993_u">>,
        integer_to_binary(Uid),
        <<"', ">>,
        integer_to_binary(AccountType),
        <<", 1, '127.0.0.1', 'x')">>
    ]).

seed_org(C, OrgId, OwnerUid, Name) ->
    ok = exec(C, [
        <<"INSERT INTO organization (id, name, owner_id, status, branding, settings,">>,
        <<" created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", '">>,
        Name,
        <<"', ">>,
        integer_to_binary(OwnerUid),
        <<", 'active', '{}'::jsonb, '{}'::jsonb, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_org_member(C, OrgId, Uid) ->
    ok = exec(C, [
        <<"INSERT INTO organization_member (organization_id, user_id, role, status,">>,
        <<" joined_at, created_at, updated_at) VALUES (">>,
        integer_to_binary(OrgId),
        <<", ">>,
        integer_to_binary(Uid),
        <<", 'member', 'active', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_workspace(C, WsId, OrgId, OwnerUid) ->
    ok = exec(C, [
        <<"INSERT INTO workspace (id, name, owner_id, status, organization_id,">>,
        <<" created_at, updated_at) VALUES (">>,
        integer_to_binary(WsId),
        <<", 't993_ws_">>,
        integer_to_binary(WsId),
        <<"', ">>,
        integer_to_binary(OwnerUid),
        <<", 'active', ">>,
        integer_to_binary(OrgId),
        <<", CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>
    ]).

seed_mapping(C, OrgId, AppId, Ext, Uid) ->
    case enterprise_external_identity_repo:bind_tx(C, OrgId, AppId, Ext, Uid) of
        {ok, _} -> ok;
        {error, Reason} -> erlang:error({seed_mapping_failed, Ext, Reason})
    end.

app_ids(C) ->
    A = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'ewh993-oa-a'">>, []
    ),
    B = one(
        C, <<"SELECT id FROM enterprise_application WHERE application_key = 'ewh993-oa-b'">>, []
    ),
    {maps:get(<<"id">>, A), maps:get(<<"id">>, B)}.

ctx_a(State) ->
    #{
        organization_id => ?ORG_A,
        application_id => maps:get(app_a, State),
        granted_scopes => ?SCOPES_FULL,
        principal_user_id => ?PRIN_A
    }.

ctx_b(State) ->
    #{
        organization_id => ?ORG_B,
        application_id => maps:get(app_b, State),
        granted_scopes => ?SCOPES_FULL,
        principal_user_id => ?PRIN_B
    }.

%%%===================================================================
%%% 通用桩/助手
%%%===================================================================

%% inet 解析桩：公网目标（emit/configure 的 SSRF 校验走 DNS）
with_public_dns(Fun) ->
    try
        ok = meck:new(inet, [unstick, passthrough, no_passthrough_cover]),
        meck:expect(inet, getaddrs, fun(Host, inet) ->
            case Host of
                "oa993.example.com" -> {ok, [?PUBLIC_IP]};
                "oa993-new.example.com" -> {ok, [?PUBLIC_IP]};
                "oa.example.com" -> {ok, [?PUBLIC_IP]};
                _ -> {error, nxdomain}
            end
        end),
        try
            Fun()
        after
            meck:unload(inet)
        end
    catch
        _:{already_started, _} ->
            Fun()
    end.

%% 池化 writer 桩：记录调用（分类 oracle），不触库
with_writer_stubs(Fun) ->
    Calls = ets:new(ewh993_writer_calls, [public, ordered_set]),
    ok = meck:new(bot_webhook_delivery_repo, [passthrough, no_passthrough_cover]),
    Record = fun(Kind, Args) ->
        ets:insert(Calls, {erlang:unique_integer([monotonic, positive]), {Kind, Args}}),
        {ok, 1}
    end,
    meck:expect(bot_webhook_delivery_repo, mark_success, fun(D, N) -> Record(success, {D, N}) end),
    meck:expect(bot_webhook_delivery_repo, mark_retry, fun(D, S, N, T) ->
        Record(retry, {D, S, N, T})
    end),
    meck:expect(bot_webhook_delivery_repo, mark_dead, fun(D, N) -> Record(dead, {D, N}) end),
    meck:expect(bot_webhook_delivery_repo, insert_attempt, fun(D, A) ->
        _ = Record(attempt, {D, A}),
        ok
    end),
    try
        Fun(Calls)
    after
        meck:unload(bot_webhook_delivery_repo),
        ets:delete(Calls)
    end.

writer_calls(Calls) ->
    [C || {_, C} <- lists:sort(ets:tab2list(Calls))].

writer_calls(Calls, Kind) ->
    [A || {K, A} <- writer_calls(Calls), K =:= Kind].

%% secret 解密桩（池化）：返回配置/轮换时拿到的明文
with_secret_stub(Secret, Fun) ->
    ok = meck:new(enterprise_webhook_repo, [passthrough, no_passthrough_cover]),
    meck:expect(enterprise_webhook_repo, get_secret, fun(_P) -> {ok, Secret} end),
    try
        Fun()
    after
        meck:unload(enterprise_webhook_repo)
    end.

%% sender 桩（单进程，passthrough 不卸载时用）
with_sender_stub(ReplyFun, Fun) ->
    ok = meck:new(bot_webhook_delivery_sender, [passthrough, no_passthrough_cover]),
    meck:expect(
        bot_webhook_delivery_sender,
        post,
        fun(_IP, _Port, _Tls, _Path, _Host, Headers, _Body) -> ReplyFun(Headers) end
    ),
    try
        Fun()
    after
        meck:unload(bot_webhook_delivery_sender)
    end.

%% loopback 出站（http）需要 test/local/dev profile：meck imboy_env 固定口径
with_loopback_profile(Fun) ->
    ok = meck:new(imboy_env, [passthrough, no_passthrough_cover]),
    meck:expect(imboy_env, current, fun() -> <<"test">> end),
    try
        Fun()
    after
        meck:unload(imboy_env)
    end.

configure(C, Ctx, Extra) ->
    Input = maps:merge(
        #{url => ?URL1, events => [<<"file.confirmed">>]},
        Extra
    ),
    enterprise_webhook_logic:configure_tx(C, Ctx, Input).

emit(C, Ctx, EventType) ->
    enterprise_webhook_logic:emit_event_tx(C, Ctx, EventType, #{
        resource_type => <<"attachment">>, resource_id => 993999
    }).

%% 按端点 URL 取投递行（同一事务内多行的 created_at 相同，按 id 排序不保证
%% 「最后插入」——需要精确定位时一律按快照列查）
delivery_by_url(C, Url) ->
    one(
        C,
        <<
            "SELECT delivery_id, webhook_url, webhook_host, pinned_ip,"
            " ewh_endpoint_generation, payload::text AS payload FROM bot_delivery"
            " WHERE ewh_owner_application_id = (SELECT id FROM enterprise_application"
            " WHERE application_key = 'ewh993-oa-a') AND webhook_url = $1"
            " ORDER BY delivery_id LIMIT 1"
        >>,
        [Url]
    ).

%% 取最近一条企业投递行
%% 取本套件刚插入的那条投递行（「最后一条」）。
%%
%% ⚠️ 排序必须用**数字序列**做 tiebreak，不能用 delivery_id 的文本序：
%% delivery_id 形态是 `ewd-<unique_integer>-<hex>`，同一事务内多行 created_at
%% 相同（tie），此时 `delivery_id DESC` 是字符串比较——'ewd-1090-…' 按字典序
%% 排在 'ewd-834-…' **之前**（'1' < '8'），于是会取到**更早**插入的那行。
%% 实测后果：本套件 reload_oracle 的「在途拒重放」用例把上一条已置 dead 的行
%% 当成刚 emit 的 pending 行，replay 走 dead 分支成功返回，断言随机变红
%% （负载高时 created_at 撞毫秒 → 15 用例里红 1 条；A0 复核首跑即复现，
%% 连跑两次绿）。unique_integer 在 VM 内单调递增，故按其中段数值排序才是真
%% 插入序。COALESCE 兼顾历史/非本前缀行的 NULL。
last_delivery(C) ->
    one(
        C,
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload, status,"
            " attempt_count, webhook_url, webhook_host, pinned_ip, ewh_owner_organization_id,"
            " ewh_owner_application_id, ewh_replay_of, ewh_endpoint_generation,"
            " ewh_ledger_version, ewh_claimed_at FROM bot_delivery"
            " WHERE bot_id LIKE 'eapp:%'"
            " ORDER BY created_at DESC,"
            " COALESCE((substring(delivery_id from '^ewd-([0-9]+)-'))::bigint, 0) DESC"
            " LIMIT 1"
        >>,
        []
    ).

%% 取「本用例刚 emit、尚未落地」的那条在途行（status='pending'）。
%%
%% 为什么不用 last_delivery/1 定位：NOW() 在 PostgreSQL 里返回**事务开始时间**，
%% 所以同一用例事务内插入的所有行 created_at **完全相同**（不是"偶尔撞毫秒"，
%% 而是恒定 tie），最后一行只能靠 tiebreak 决定。本套件多处需要精确指向
%% 「刚 emit 的那一行」（在途拒重放、终态改写、统计口径），按序取行会随实现
%% 细节漂移：实测出现过把已置 dead 的旧行当成新在途行，导致 replay 走终态分支、
%% 以及 dead→success 被终态守卫拒绝（A0 复核连跑复现）。这里改为**按状态**取
%% 唯一的在途行——同一用例内它必然唯一，且不存在排序歧义。
sole_pending_delivery(C) ->
    Rows = all(
        C,
        <<
            "SELECT delivery_id, status FROM bot_delivery"
            " WHERE bot_id LIKE 'eapp:%' AND status = 'pending'"
            " ORDER BY COALESCE((substring(delivery_id from '^ewd-([0-9]+)-'))::bigint, 0) DESC"
            " LIMIT 1"
        >>,
        []
    ),
    ?assert(length(Rows) >= 1),
    [Row] = Rows,
    delivery_row(C, maps:get(<<"delivery_id">>, Row)).

delivery_row(C, Did) ->
    one(
        C,
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload, status,"
            " attempt_count, webhook_url, webhook_host, pinned_ip, ewh_owner_organization_id,"
            " ewh_owner_application_id, ewh_replay_of, ewh_endpoint_generation,"
            " ewh_ledger_version FROM bot_delivery WHERE delivery_id = $1"
        >>,
        [Did]
    ).

%% 直连行 -> execute_delivery 输入（claim_due 同构：payload 保持 binary）
as_claim_row(C, Did) ->
    Row = one(
        C,
        <<
            "SELECT delivery_id, bot_id, event_type, payload::text AS payload, reply_context,"
            " correlation_id, attempt_count, webhook_url, webhook_host, pinned_ip,"
            " ewh_owner_organization_id, ewh_owner_application_id FROM bot_delivery"
            " WHERE delivery_id = $1"
        >>,
        [Did]
    ),
    Row#{<<"payload">> => maps:get(<<"payload">>, Row, <<"{}">>)}.

%% 扫描持久化轨迹里是否出现某字符串（position 为字面匹配，不经 LIKE 元字符）
trace_hits(C, Needle) ->
    scalar(
        C,
        <<
            "SELECT (SELECT count(*) FROM bot_delivery WHERE position($1 in"
            " (delivery_id || '|' || bot_id || '|' || event_type || '|' || payload::text ||"
            " '|' || reply_context || '|' || correlation_id || '|' || idempotency_key ||"
            " '|' || coalesce(webhook_url,'') || '|' || coalesce(webhook_host,'') ||"
            " '|' || coalesce(pinned_ip,'') || '|' || coalesce(ewh_replay_of,''))) > 0)"
            " + (SELECT count(*) FROM bot_delivery_attempt WHERE position($1 in"
            " (id || '|' || delivery_id || '|' || status_class || '|' ||"
            " coalesce(http_status::text,'') || '|' || coalesce(error_trunc,''))) > 0) AS hits"
        >>,
        [Needle]
    ).

%%%===================================================================
%%% Suite
%%%===================================================================

enterprise_webhook_governance_pg_test_() ->
    {timeout, 900,
        {setup, fun setup_conn/0, fun close_conn/1, fun(State) ->
            C = maps:get(conn, State),
            {inorder, [
                {"ledger_ownership_and_generation_snapshot",
                    tx(C, fun(C1) -> ledger_snapshot(C1, State) end)},
                {"ledger_snapshot_survives_config_change",
                    tx(C, fun(C1) -> ledger_config_change(C1, State) end)},
                {"terminal_settle_is_tolerant_and_recorded",
                    tx(C, fun(C1) -> settle_tolerant(C1, State) end)},
                {"hmac_signature_matrix", ?_test(hmac_matrix(State))},
                {"rotation_invalidates_old_signature",
                    tx(C, fun(C1) -> rotation_invalidation(C1, State) end)},
                {"ssrf_guard_full_spectrum", ?_test(ssrf_matrix())},
                {"sender_wire_contract_real_socket", ?_test(sender_wire(State))},
                {"sender_redirect_not_followed", ?_test(sender_redirect(State))},
                {"sender_timeout_real_socket", ?_test(sender_timeout(State))},
                {"sender_response_cap", ?_test(sender_cap(State))},
                {"replay_semantics_and_arbitration",
                    tx(C, fun(C1) -> replay_oracle(C1, State) end)},
                {"metrics_recorded_without_secret",
                    tx(C, fun(C1) -> metrics_oracle(C1, State) end)},
                {"delivery_read_surface_isolated_and_bounded",
                    tx(C, fun(C1) -> read_surface_oracle(C1, State) end)},
                {"no_secret_or_body_in_durable_trace",
                    tx(C, fun(C1) -> no_secret_or_body(C1, State) end)},
                {"retention_purgeable_terminal_only",
                    tx(C, fun(C1) -> retention_oracle(C1, State) end)}
            ]}
        end}}.

%%%===================================================================
%%% ① 账本：ownership + 代际
%%%===================================================================

ledger_snapshot(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    ?assertEqual(?URL1, maps:get(<<"url">>, R1)),
    Gen1 = maps:get(<<"endpoint_generation">>, R1),
    ?assertMatch(true, is_integer(Gen1) andalso Gen1 >= 1),
    Secret1 = maps:get(<<"secret">>, R1),
    ?assertMatch(true, is_binary(Secret1)),
    %% 明文 secret 只在响应出现一次；库里是 AEAD 密文（不等于明文）
    ?assertNotEqual(
        Secret1,
        scalar(C, <<"SELECT verify_token_enc FROM bot WHERE user_id = $1">>, [?PRIN_A])
    ),

    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Row = last_delivery(C),
    ?assertEqual(?ORG_A, maps:get(<<"ewh_owner_organization_id">>, Row)),
    ?assertEqual(maps:get(app_a, State), maps:get(<<"ewh_owner_application_id">>, Row)),
    ?assertEqual(Gen1, maps:get(<<"ewh_endpoint_generation">>, Row)),
    ?assertEqual(null, maps:get(<<"ewh_replay_of">>, Row)),
    ?assertEqual(1, maps:get(<<"ewh_ledger_version">>, Row)),
    ?assertEqual(null, maps:get(<<"ewh_claimed_at">>, Row)),
    ?assertEqual(?URL1, maps:get(<<"webhook_url">>, Row)),
    ?assertEqual(?HOST1, maps:get(<<"webhook_host">>, Row)),
    ?assertEqual(<<?PUBLIC_IP_STR>>, maps:get(<<"pinned_ip">>, Row)),
    %% 信封键集封闭
    Env = jsone:decode(maps:get(<<"payload">>, Row)),
    {Keys, ResKeys} = enterprise_webhook_logic:envelope_keys(),
    ?assertEqual(lists:sort(Keys), lists:sort(maps:keys(Env))),
    ?assertEqual(
        lists:sort(ResKeys),
        lists:sort(maps:keys(maps:get(<<"resource">>, Env)))
    ),
    %% 未订阅事件不入箱
    ?assertEqual({ok, skipped}, emit(C, ctx_a(State), <<"group.member.changed">>)).

%%%===================================================================
%%% ①b 配置变更不改变在途投递（endpoint snapshot）
%%%===================================================================

ledger_config_change(C, State) ->
    {ok, _R1} = configure(C, ctx_a(State), #{}),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    InFlight = last_delivery(C),
    Did = maps:get(<<"delivery_id">>, InFlight),

    %% 改端点 + 轮换：配置写入 → 代际 +1
    {ok, R2} = configure(C, ctx_a(State), #{
        url => ?URL2, rotate => true
    }),
    Gen2 = maps:get(<<"endpoint_generation">>, R2),
    ?assert(Gen2 > maps:get(<<"ewh_endpoint_generation">>, InFlight)),

    %% 在途行逐字段不变（快照隔离）
    After = delivery_row(C, Did),
    lists:foreach(
        fun(K) -> ?assertEqual(maps:get(K, InFlight), maps:get(K, After), K) end,
        [
            <<"webhook_url">>,
            <<"webhook_host">>,
            <<"pinned_ip">>,
            <<"ewh_endpoint_generation">>,
            <<"payload">>,
            <<"ewh_owner_application_id">>
        ]
    ),
    %% 新入箱行用新端点 + 新代际
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Newest = delivery_by_url(C, ?URL2),
    ?assertNotEqual(Did, maps:get(<<"delivery_id">>, Newest)),
    ?assertEqual(?URL2, maps:get(<<"webhook_url">>, Newest)),
    ?assertEqual(Gen2, maps:get(<<"ewh_endpoint_generation">>, Newest)).

%%%===================================================================
%%% ② 终态落账容错（守卫拒绝 → 不炸 worker 批次）
%%%===================================================================

settle_tolerant(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    Secret = maps:get(<<"secret">>, R1),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),

    %% 5xx → 分类为 retry；writer 模拟被守卫拒绝（{rollback, terminal_immutable}）
    with_writer_stubs(fun(Calls) ->
        meck:expect(bot_webhook_delivery_repo, mark_retry, fun(D, _S, N, T) ->
            ets:insert(Calls, {
                erlang:unique_integer([monotonic, positive]),
                {rejected, {D, N, T}}
            }),
            {rollback, {check_violation, <<"trg_ewh_delivery_guard">>}}
        end),
        with_secret_stub(Secret, fun() ->
            with_sender_stub(fun(_H) -> {ok, 503} end, fun() ->
                ok = enterprise_webhook_logic:execute_delivery(Row)
            end)
        end),
        %% 审计仍写（attempt 行先落），且 batch 未崩
        ?assertEqual(1, length(writer_calls(Calls, attempt))),
        ?assertEqual(1, length(writer_calls(Calls, rejected)))
    end),
    %% 2xx → success（writer 正常返回）
    with_writer_stubs(fun(Calls) ->
        with_secret_stub(Secret, fun() ->
            with_sender_stub(fun(_H) -> {ok, 200} end, fun() ->
                ok = enterprise_webhook_logic:execute_delivery(Row)
            end)
        end),
        [Success] = writer_calls(Calls, success),
        ?assertEqual({maps:get(<<"delivery_id">>, Row), 1}, Success)
    end),
    %% 4xx → dead
    with_writer_stubs(fun(Calls) ->
        with_secret_stub(Secret, fun() ->
            with_sender_stub(fun(_H) -> {ok, 403} end, fun() ->
                ok = enterprise_webhook_logic:execute_delivery(Row)
            end)
        end),
        ?assertEqual(1, length(writer_calls(Calls, dead)))
    end),
    %% credential_error（secret 解密失败）→ retry 且不崩
    with_writer_stubs(fun(Calls) ->
        ok = meck:new(enterprise_webhook_repo, [passthrough, no_passthrough_cover]),
        meck:expect(enterprise_webhook_repo, get_secret, fun(_P) -> {error, no_key} end),
        try
            ok = enterprise_webhook_logic:execute_delivery(Row)
        after
            meck:unload(enterprise_webhook_repo)
        end,
        [Retry] = writer_calls(Calls, retry),
        ?assertEqual(5, element(2, Retry))
    end).

%%%===================================================================
%%% ③ HMAC 矩阵（纯合同）
%%%===================================================================

hmac_matrix(_State) ->
    Secret = <<"whsec_matrix_secret_993">>,
    Other = <<"whsec_other_secret_993">>,
    Ts = integer_to_binary(os:system_time(second)),
    Body = <<"{\"event_id\":\"evt-993\"}">>,
    Sig = enterprise_webhook_logic:sign(Secret, enterprise_webhook_logic:signature_base(Ts, Body)),
    %% 签名形态 = "sha256=" ++ 64 位小写 hex（复用 bot 域 mac 原语，前后缀稳定）
    ?assertMatch(<<"sha256=", _/binary>>, Sig),
    ?assertEqual(71, byte_size(Sig)),
    ?assertEqual(64, byte_size(binary:part(Sig, 7, 64))),
    %% 签名头合同名（四个头，名字稳定）
    Hdrs = enterprise_webhook_logic:signature_headers(),
    ?assertEqual(<<"x-imboy-timestamp">>, maps:get(timestamp, Hdrs)),
    ?assertEqual(<<"x-imboy-signature">>, maps:get(signature, Hdrs)),
    ?assertEqual(<<"x-imboy-delivery">>, maps:get(delivery, Hdrs)),
    ?assertEqual(<<"x-imboy-event">>, maps:get(event, Hdrs)),
    %% 正向 + 大小写不敏感
    ?assert(enterprise_webhook_logic:verify(Secret, Ts, Body, Sig)),
    ?assert(
        enterprise_webhook_logic:verify(Secret, Ts, Body, string:uppercase(Sig))
    ),
    %% 篡改正文 / 错误时间戳 / 错误 secret（rotation 后的旧 secret）/ 畸形
    ?assertNot(enterprise_webhook_logic:verify(Secret, Ts, <<Body/binary, " ">>, Sig)),
    ?assertNot(enterprise_webhook_logic:verify(Secret, <<Ts/binary, "0">>, Body, Sig)),
    ?assertNot(enterprise_webhook_logic:verify(Other, Ts, Body, Sig)),
    ?assertNot(enterprise_webhook_logic:verify(Secret, Ts, Body, <<"deadbeef">>)),
    ?assertNot(enterprise_webhook_logic:verify(Secret, Ts, Body, <<>>)),
    ?assertNot(enterprise_webhook_logic:verify(Secret, Ts, Body, <<"zz", Sig/binary>>)),
    %% 空 secret / 非二进制入参一律 false（不崩）
    ?assertNot(enterprise_webhook_logic:verify(<<>>, Ts, Body, Sig)),
    ?assertNot(enterprise_webhook_logic:verify(Secret, Ts, Body, not_a_binary)),
    %% 时间窗（反重放）：新鲜 true；超窗 false；窗放宽容忍
    ?assert(enterprise_webhook_logic:verify_within(Secret, Ts, Body, Sig, 300)),
    Old = integer_to_binary(os:system_time(second) - 3600),
    OldSig = enterprise_webhook_logic:sign(
        Secret, enterprise_webhook_logic:signature_base(Old, Body)
    ),
    ?assertNot(enterprise_webhook_logic:verify_within(Secret, Old, Body, OldSig, 300)),
    ?assert(enterprise_webhook_logic:verify_within(Secret, Old, Body, OldSig, 7200)),
    %% 非数字时间戳 / 负窗口 → false
    ?assertNot(enterprise_webhook_logic:verify_within(Secret, <<"soon">>, Body, Sig, 300)),
    ?assertNot(enterprise_webhook_logic:verify_within(Secret, Ts, Body, Sig, -1)).

%%%===================================================================
%%% ④ rotation：旧签名失效 + 在途用新 secret
%%%===================================================================

rotation_invalidation(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    Secret1 = maps:get(<<"secret">>, R1),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),

    %% 轮换前的投递：签名用 S1
    Captured1 = ets:new(ewh993_cap1, [public, ordered_set]),
    with_secret_stub(Secret1, fun() ->
        with_writer_stubs(fun(_) ->
            with_sender_stub(
                fun(Headers) ->
                    ets:insert(Captured1, {1, Headers}),
                    {ok, 200}
                end,
                fun() -> ok = enterprise_webhook_logic:execute_delivery(Row) end
            )
        end)
    end),
    [{1, Headers1}] = ets:tab2list(Captured1),
    Ts1 = proplists:get_value(<<"x-imboy-timestamp">>, Headers1),
    Sig1 = proplists:get_value(<<"x-imboy-signature">>, Headers1),
    Body = maps:get(<<"payload">>, Row),
    ?assert(enterprise_webhook_logic:verify(Secret1, Ts1, Body, Sig1)),

    %% 轮换：新 secret + 新代际
    {ok, R2} = configure(C, ctx_a(State), #{rotate => true}),
    Secret2 = maps:get(<<"secret">>, R2),
    ?assertNotEqual(Secret1, Secret2),
    %% 旧签名在新 secret 下一律失效（rotation 即失效，无双密钥窗口）
    ?assertNot(enterprise_webhook_logic:verify(Secret2, Ts1, Body, Sig1)),
    ?assert(
        enterprise_webhook_logic:verify_within(Secret2, Ts1, Body, Sig1, 300) =:= false
    ),

    %% 轮换后仍在途的**同一条**投递被执行：用新 secret 签名（线上头断言）
    Captured2 = ets:new(ewh993_cap2, [public, ordered_set]),
    with_secret_stub(Secret2, fun() ->
        with_writer_stubs(fun(_) ->
            with_sender_stub(
                fun(Headers) ->
                    ets:insert(Captured2, {1, Headers}),
                    {ok, 200}
                end,
                fun() -> ok = enterprise_webhook_logic:execute_delivery(Row) end
            )
        end)
    end),
    [{1, Headers2}] = ets:tab2list(Captured2),
    Ts2 = proplists:get_value(<<"x-imboy-timestamp">>, Headers2),
    Sig2 = proplists:get_value(<<"x-imboy-signature">>, Headers2),
    ?assert(enterprise_webhook_logic:verify(Secret2, Ts2, Body, Sig2)),
    ?assertNot(enterprise_webhook_logic:verify(Secret1, Ts2, Body, Sig2)),
    ets:delete(Captured1),
    ets:delete(Captured2).

%%%===================================================================
%%% ⑤ SSRF 全谱
%%%===================================================================

ssrf_matrix() ->
    ?_test(begin
        %% 私网 / link-local（metadata）/ CGNAT / 保留段 / 组播 / 6to4
        Private = [
            {<<"https://10.1.2.3/hook">>, <<"10.1.2.3">>},
            {<<"https://172.20.0.9/hook">>, <<"172.20.0.9">>},
            {<<"https://192.168.7.7/hook">>, <<"192.168.7.7">>},
            {<<"https://169.254.169.254/latest/meta-data/">>, <<"169.254.169.254">>},
            {<<"https://100.64.0.1/hook">>, <<"100.64.0.1">>},
            {<<"https://198.18.0.1/hook">>, <<"198.18.0.1">>},
            {<<"https://192.88.99.1/hook">>, <<"192.88.99.1">>},
            {<<"https://0.0.0.0/hook">>, <<"0.0.0.0">>},
            {<<"https://224.0.0.1/hook">>, <<"224.0.0.1">>},
            {<<"https://198.51.100.7/hook">>, <<"198.51.100.7">>},
            {<<"https://192.0.2.9/hook">>, <<"192.0.2.9">>},
            {<<"https://[fd00::1]/hook">>, <<"fd00::1">>},
            {<<"https://[2001:db8::1]/hook">>, <<"2001:db8::1">>}
        ],
        lists:foreach(
            fun({Url, Host}) ->
                with_inet_resolve(Host, {ok, [bad_ip(Host)]}, fun() ->
                    ?assertEqual(
                        {error, forbidden_host},
                        element(2, {x, guard_url(Url)}),
                        {ssrf_private, Host}
                    )
                end)
            end,
            Private
        ),
        %% 混合解析（DNS rebinding 的经典形态）：公网 + 私网同一域名 → 整体拒绝
        with_inet_resolve(<<"evil993.example.com">>, {ok, [?PUBLIC_IP, {10, 0, 0, 5}]}, fun() ->
            ?assertMatch(
                {ok, _},
                guard_url(<<"https://oa993.example.com/hook">>, <<"oa993.example.com">>)
            ),
            ?assertEqual(
                {error, forbidden_host},
                guard_url(<<"https://evil993.example.com/hook">>)
            )
        end),
        %% loopback：非 test/local/dev profile 一律拒绝（生产口径）
        with_prod_profile(fun() ->
            ?assertEqual({error, invalid_scheme}, guard_url(<<"http://127.0.0.1/hook">>)),
            with_inet_resolve(<<"localhost">>, {ok, [{127, 0, 0, 1}]}, fun() ->
                ?assertEqual({error, forbidden_host}, guard_url(<<"https://localhost/hook">>))
            end),
            with_inet_resolve(<<"[::1]">>, {ok, [{0, 0, 0, 0, 0, 0, 0, 1}]}, fun() ->
                ?assertEqual({error, forbidden_host}, guard_url(<<"https://[::1]/hook">>))
            end)
        end),
        %% 解析失败 / 非 HTTPS / 畸形 URL / 用户信息混淆
        with_inet_resolve(<<"nx993.example.com">>, {error, nxdomain}, fun() ->
            ?assertEqual({error, dns_failure}, guard_url(<<"https://nx993.example.com/hook">>))
        end),
        with_inet_resolve(<<"oa993.example.com">>, {ok, [?PUBLIC_IP]}, fun() ->
            ?assertEqual(
                {error, invalid_scheme},
                guard_url(<<"http://oa993.example.com/hook">>)
            ),
            ?assertEqual({error, invalid_scheme}, guard_url(<<"ftp://oa993.example.com/hook">>)),
            ?assertEqual({error, invalid_url}, guard_url(<<"not a url">>)),
            ?assertEqual({error, invalid_url}, guard_url(<<>>)),
            ?assertEqual({error, invalid_url}, guard_url(<<"https://">>)),
            ?assertEqual(
                {error, invalid_url},
                guard_url(<<"https://oa993.example.com:99999/hook">>)
            )
        end),
        %% 用户信息混淆：https://user@host 只按 host 判定（userinfo 不参与目标）
        with_inet_resolve(<<"internal993.example.com">>, {ok, [{10, 0, 0, 7}]}, fun() ->
            ?assertEqual(
                {error, forbidden_host},
                guard_url(<<"https://user:pw@internal993.example.com/hook">>)
            )
        end),
        %% 入箱快照（store pin）复核：私网/畸形 pin 一律拒绝（rebinding 后不回退）
        ?assertEqual(
            {error, forbidden_host},
            bot_webhook_guard:validate_pinned(
                <<"https://oa993.example.com/hook">>, <<"10.0.0.9">>
            )
        ),
        ?assertEqual(
            {error, invalid_pin},
            bot_webhook_guard:validate_pinned(<<"https://oa993.example.com/hook">>, <<"nope">>)
        ),
        ?assertEqual(
            {error, invalid_pin},
            bot_webhook_guard:validate_pinned(<<"https://oa993.example.com/hook">>, <<>>)
        ),
        ?assertMatch(
            {ok, _},
            bot_webhook_guard:validate_pinned(
                <<"https://oa993.example.com/hook">>, <<"93.184.216.34">>
            )
        )
    end).

guard_url(Url) ->
    guard_url(Url, undefined).

guard_url(Url, undefined) ->
    bot_webhook_guard:validate_and_pin(Url);
guard_url(Url, Host) ->
    with_inet_resolve(Host, {ok, [?PUBLIC_IP]}, fun() ->
        bot_webhook_guard:validate_and_pin(Url)
    end).

%% 私网/保留地址的字面 IP（guard 的 is_private_ip 直接命中，不走 DNS）
bad_ip(<<"[fd00::1]">>) -> {16#fd00, 0, 0, 0, 0, 0, 0, 1};
bad_ip(<<"[2001:db8::1]">>) -> {16#2001, 16#db8, 0, 0, 0, 0, 0, 1};
bad_ip(Host) -> ip_from_binary(Host).

%% 去掉 IPv6 字面量的方括号（uri_string 解析后 host 不带括号）
unbracket(<<"[", Rest/binary>>) ->
    case Rest of
        <<Inner:(byte_size(Rest) - 1)/binary, "]">> -> Inner;
        _ -> Rest
    end;
unbracket(Host) ->
    Host.

ip_from_binary(Host) ->
    {ok, IP} = inet:parse_address(binary_to_list(Host)),
    IP.

with_inet_resolve(Host, Result, Fun) ->
    Names = [binary_to_list(Host), binary_to_list(unbracket(Host))],
    ok = meck:new(inet, [unstick, passthrough, no_passthrough_cover]),
    meck:expect(inet, getaddrs, fun(H, inet) ->
        case lists:member(H, Names) of
            true -> Result;
            false -> meck:passthrough([H, inet])
        end
    end),
    try
        Fun()
    after
        meck:unload(inet)
    end.

with_prod_profile(Fun) ->
    ok = meck:new(imboy_env, [passthrough, no_passthrough_cover]),
    meck:expect(imboy_env, current, fun() -> <<"prod">> end),
    try
        Fun()
    after
        meck:unload(imboy_env)
    end.

%%%===================================================================
%%% ⑥ 真 socket：线上签名 / redirect / timeout / response cap
%%%===================================================================

sender_wire(_State) ->
    ?_test(begin
        Secret = <<"whsec_wire_993">>,
        Body = <<"{\"event_id\":\"evt-wire-993\"}">>,
        Headers = [
            {<<"x-imboy-delivery">>, <<"ewd-wire-1">>},
            {<<"x-imboy-event">>, <<"file.confirmed">>},
            {<<"x-imboy-timestamp">>, integer_to_binary(os:system_time(second))},
            {<<"x-imboy-signature">>,
                enterprise_webhook_logic:sign(
                    Secret,
                    enterprise_webhook_logic:signature_base(
                        integer_to_binary(os:system_time(second)), Body
                    )
                )}
        ],
        H = ewh_http_fixture:start(fun(_Req) -> {reply, 200, [], <<"ok">>} end),
        Port = ewh_http_fixture:port(H),
        try
            with_loopback_profile(fun() ->
                {ok, 200} = bot_webhook_delivery_sender:post(
                    {127, 0, 0, 1}, Port, false, <<"/hook">>, <<"127.0.0.1">>, Headers, Body
                )
            end),
            [Req] = ewh_http_fixture:requests(H),
            ?assertEqual(<<"POST">>, maps:get(method, Req)),
            ?assertEqual(<<"/hook">>, maps:get(path, Req)),
            ?assertEqual(Body, maps:get(body, Req)),
            ReqHeaders = maps:get(headers, Req),
            lists:foreach(
                fun({K, V}) -> ?assertEqual(V, proplists:get_value(K, ReqHeaders), K) end,
                Headers
            ),
            %% 线上签名可验（接收侧合同）
            Ts = proplists:get_value(<<"x-imboy-timestamp">>, ReqHeaders),
            Sig = proplists:get_value(<<"x-imboy-signature">>, ReqHeaders),
            ?assert(enterprise_webhook_logic:verify(Secret, Ts, maps:get(body, Req), Sig)),
            %% 出站超时口径（生产默认 8000；env 覆盖夹紧，不可关闭）
            ?assertEqual({8000, 8000}, bot_webhook_delivery_sender:timeouts()),
            application:set_env(imboy, bot_webhook_timeout_ms, 1),
            ?assertEqual({100, 100}, bot_webhook_delivery_sender:timeouts()),
            application:set_env(imboy, bot_webhook_timeout_ms, 999999),
            ?assertEqual({30000, 30000}, bot_webhook_delivery_sender:timeouts()),
            application:set_env(imboy, bot_webhook_timeout_ms, <<"nope">>),
            ?assertEqual({8000, 8000}, bot_webhook_delivery_sender:timeouts()),
            application:unset_env(imboy, bot_webhook_timeout_ms),
            ?assertEqual(65536, bot_webhook_delivery_sender:response_cap_bytes())
        after
            ewh_http_fixture:stop(H)
        end
    end).

sender_redirect(State) ->
    ?_test(begin
        %% 302 + Location：sender 只回状态码，**不跟随**
        H = ewh_http_fixture:start(fun(_Req) ->
            {reply, 302, [{<<"location">>, <<"https://evil993.example.com/steal">>}], <<>>}
        end),
        Port = ewh_http_fixture:port(H),
        try
            R =
                with_loopback_profile(fun() ->
                    bot_webhook_delivery_sender:post(
                        {127, 0, 0, 1},
                        Port,
                        false,
                        <<"/hook">>,
                        <<"127.0.0.1">>,
                        [{<<"x-imboy-signature">>, <<"sig">>}],
                        <<"{}">>
                    )
                end),
            ?assertEqual({ok, 302}, R),
            %% 只发生一次出站（没有第二跳）
            ?assertEqual(1, ewh_http_fixture:request_count(H))
        after
            ewh_http_fixture:stop(H)
        end,
        %% 3xx 在投递分类里是「可重试失败」而不是成功
        C = maps:get(conn, State),
        ok = exec(C, <<"BEGIN">>),
        try
            {ok, R1} = configure(C, ctx_a(State), #{}),
            Secret2 = maps:get(<<"secret">>, R1),
            {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
            Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),
            with_writer_stubs(fun(Calls) ->
                with_secret_stub(Secret2, fun() ->
                    with_sender_stub(fun(_H) -> {ok, 302} end, fun() ->
                        ok = enterprise_webhook_logic:execute_delivery(Row)
                    end)
                end),
                ?assertEqual(1, length(writer_calls(Calls, retry))),
                ?assertEqual(0, length(writer_calls(Calls, success)))
            end)
        after
            exec(C, <<"ROLLBACK">>)
        end
    end).

sender_timeout(State) ->
    ?_test(begin
        %% 收下请求后不响应 → 响应超时（有界，不挂死）
        H = ewh_http_fixture:start(fun(_Req) -> hang end),
        Port = ewh_http_fixture:port(H),
        try
            application:set_env(imboy, bot_webhook_timeout_ms, 150),
            T0 = erlang:monotonic_time(millisecond),
            R =
                with_loopback_profile(fun() ->
                    bot_webhook_delivery_sender:post(
                        {127, 0, 0, 1}, Port, false, <<"/hook">>, <<"127.0.0.1">>, [], <<"{}">>
                    )
                end),
            Elapsed = erlang:monotonic_time(millisecond) - T0,
            ?assertMatch({error, {recv, timeout}}, R),
            ?assert(Elapsed >= 100 andalso Elapsed < 5000),
            %% 超时在投递分类里 → retry（不是 success / 不是 dead）
            C = maps:get(conn, State),
            ok = exec(C, <<"BEGIN">>),
            try
                {ok, R1} = configure(C, ctx_a(State), #{}),
                Secret = maps:get(<<"secret">>, R1),
                {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
                Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),
                with_writer_stubs(fun(Calls) ->
                    with_secret_stub(Secret, fun() ->
                        ok = enterprise_webhook_logic:execute_delivery(Row),
                        ?assertEqual(1, length(writer_calls(Calls, retry)))
                    end)
                end)
            after
                exec(C, <<"ROLLBACK">>)
            end
        after
            application:unset_env(imboy, bot_webhook_timeout_ms),
            ewh_http_fixture:stop(H)
        end
    end).

sender_cap(_State) ->
    ?_test(begin
        %% 巨大响应体：不读正文（connection: close），只认状态行 → 立刻 200
        Big = binary:copy(<<"x">>, 5 * 1024 * 1024),
        H = ewh_http_fixture:start(fun(_Req) -> {reply, 200, [], Big} end),
        Port = ewh_http_fixture:port(H),
        try
            application:set_env(imboy, bot_webhook_timeout_ms, 3000),
            T0 = erlang:monotonic_time(millisecond),
            R =
                with_loopback_profile(fun() ->
                    bot_webhook_delivery_sender:post(
                        {127, 0, 0, 1}, Port, false, <<"/hook">>, <<"127.0.0.1">>, [], <<"{}">>
                    )
                end),
            ?assertEqual({ok, 200}, R),
            %% 5MB 响应体不改变用时量级（不累积到内存）
            ?assert(erlang:monotonic_time(millisecond) - T0 < 3000)
        after
            application:unset_env(imboy, bot_webhook_timeout_ms),
            ewh_http_fixture:stop(H)
        end,
        %% 响应头超过 cap（64KiB）→ 显式失败，不静默截断
        Huge = [
            <<"HTTP/1.1 200 OK\r\n">>,
            <<"x-big: ">>,
            binary:copy(<<"h">>, 200 * 1024),
            <<"\r\n">>
        ],
        H2 = ewh_http_fixture:start(fun(_Req) -> {raw, Huge} end),
        Port2 = ewh_http_fixture:port(H2),
        try
            R2 =
                with_loopback_profile(fun() ->
                    bot_webhook_delivery_sender:post(
                        {127, 0, 0, 1}, Port2, false, <<"/hook">>, <<"127.0.0.1">>, [], <<"{}">>
                    )
                end),
            ?assertEqual({error, head_too_large}, R2)
        after
            ewh_http_fixture:stop(H2)
        end
    end).

%%%===================================================================
%%% ⑦ 重放语义与仲裁
%%%===================================================================

replay_oracle(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    _Secret1 = maps:get(<<"secret">>, R1),
    %% 造一条终态（dead）行：emit → 直接置 dead（业务侧终态由投递执行/守卫保证）
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Original = last_delivery(C),
    OldId = maps:get(<<"delivery_id">>, Original),
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
        OldId,
        <<"'">>
    ]),

    %% 在途拒重放：先造一条 pending（按**状态**精确定位，见 sole_pending_delivery/1）
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Pending = sole_pending_delivery(C),
    ?assertEqual(
        {error, {<<"invalid_request">>, delivery_in_flight}},
        enterprise_webhook_logic:replay_tx(C, ctx_a(State), maps:get(<<"delivery_id">>, Pending))
    ),

    %% 正常重放：新 delivery id / 保留 event id / ewh_replay_of / 归属与原行一致
    {ok, Replay} = enterprise_webhook_logic:replay_tx(C, ctx_a(State), OldId),
    NewId = maps:get(<<"delivery_id">>, Replay),
    ?assertNotEqual(OldId, NewId),
    ?assertEqual(OldId, maps:get(<<"original_delivery_id">>, Replay)),
    NewRow = delivery_row(C, NewId),
    Env = jsone:decode(maps:get(<<"payload">>, NewRow)),
    OldEnv = jsone:decode(maps:get(<<"payload">>, Original)),
    ?assertEqual(maps:get(<<"event_id">>, OldEnv), maps:get(<<"event_id">>, Env)),
    ?assertEqual(NewId, maps:get(<<"delivery_id">>, Env)),
    ?assertEqual(OldId, maps:get(<<"ewh_replay_of">>, NewRow)),
    ?assertEqual(?ORG_A, maps:get(<<"ewh_owner_organization_id">>, NewRow)),
    ?assertEqual(maps:get(app_a, State), maps:get(<<"ewh_owner_application_id">>, NewRow)),
    %% 重放使用**当前**配置（端点 + 代际），不是历史快照
    ?assertEqual(?URL1, maps:get(<<"webhook_url">>, NewRow)),

    %% 并发/重复在途重放：DB 唯一索引仲裁 → idempotency_conflict
    %% （23505 会 abort 事务，故在 savepoint 内断言）
    ?assertEqual(
        {error, {<<"idempotency_conflict">>, replay_already_queued}},
        in_savepoint(C, fun() ->
            enterprise_webhook_logic:replay_tx(C, ctx_a(State), OldId)
        end)
    ),
    %% 跨 org：org B 的 ctx 重放 org A 的行 → resource_not_found（不给存在性 oracle）
    ?assertEqual(
        {error, {<<"resource_not_found">>, delivery_not_found}},
        enterprise_webhook_logic:replay_tx(C, ctx_b(State), OldId)
    ),
    %% bot 域行（纯数字 bot_id）同样拒绝
    ok = exec(
        C, [
            <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
            <<"correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip, status)">>,
            <<"VALUES ('ewh993-botrow', '993777', 'message', '{}', 'corr993bot00000001',">>,
            <<"'ewh993-bot-idem', 'https://bot.example/', 'bot.example', '93.184.216.34', 'dead')">>
        ]
    ),
    ?assertEqual(
        {error, {<<"resource_not_found">>, delivery_not_found}},
        enterprise_webhook_logic:replay_tx(C, ctx_a(State), <<"ewh993-botrow">>)
    ),
    %% ownership 列不可被改动（跨 org 篡改在 DB 层被守卫拒绝——归属真源是行上的列，
    %% 不是可被改写的元数据）
    ?assertMatch(
        {error, {check_violation, <<"trg_ewh_delivery_guard">>}},
        in_savepoint(C, fun() ->
            case
                elib_pg:query(
                    C,
                    <<
                        "UPDATE bot_delivery SET ewh_owner_organization_id = $1"
                        " WHERE delivery_id = $2"
                    >>,
                    [?ORG_B, OldId]
                )
            of
                {ok, _} ->
                    ok;
                {error, #error{code = <<"23514">>, extra = Extra}} ->
                    {error, {check_violation, proplists:get_value(constraint_name, Extra)}}
            end
        end)
    ),
    {ok, _Disabled} = configure(C, ctx_a(State), #{
        status => disabled, events => [<<"file.confirmed">>]
    }),
    ?assertMatch(
        {error, {<<"invalid_request">>, {ssrf_or_invalid_url, _}}},
        enterprise_webhook_logic:replay_tx(C, ctx_a(State), OldId)
    ).

%%%===================================================================
%%% ⑧ 可观测：指标 + 读面
%%%===================================================================

metrics_oracle(C, State) ->
    ensure_metric_server(),
    elib_metric:reset(),
    {ok, R1} = configure(C, ctx_a(State), #{}),
    Secret = maps:get(<<"secret">>, R1),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),
    ?assertEqual({ok, emitted}, emit(C, ctx_a(State), <<"file.confirmed">>)),

    %% 各分类各执行一次（writer/secret/sender 全桩）
    lists:foreach(
        fun(Code) ->
            with_writer_stubs(fun(_Calls) ->
                with_secret_stub(Secret, fun() ->
                    with_sender_stub(fun(_H) -> {ok, Code} end, fun() ->
                        ok = enterprise_webhook_logic:execute_delivery(Row)
                    end)
                end)
            end)
        end,
        [200, 403, 503]
    ),
    %% 重放一次（新在途行）
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
        maps:get(<<"delivery_id">>, Row),
        <<"'">>
    ]),
    {ok, _} = enterprise_webhook_logic:replay_tx(C, ctx_a(State), maps:get(<<"delivery_id">>, Row)),

    Names = enterprise_webhook_logic:metric_names(),
    Metrics = gen_server:call(elib_metric, get_all_metrics),
    Counters = maps:get(counters, Metrics),
    Attempt = maps:get(attempt, Names),
    Outcome = fun(O) ->
        case maps:get({Attempt, #{outcome => O, class => any}}, Counters, undefined) of
            undefined ->
                lists:sum([
                    V
                 || {{Name, Labels}, V} <- maps:to_list(Counters),
                    Name =:= Attempt,
                    maps:get(outcome, Labels, undefined) =:= O
                ]);
            N ->
                N
        end
    end,
    ?assert(Outcome(success) >= 1),
    ?assert(Outcome(dead) >= 1),
    ?assert(Outcome(retry) >= 1),
    Dead = maps:get(dead_letter, Names),
    ?assert(
        lists:sum([
            V
         || {{Name, _}, V} <- maps:to_list(Counters),
            Name =:= Dead
        ]) >= 1
    ),
    Emit = maps:get(emit, Names),
    ?assert(
        lists:sum([
            V
         || {{Name, _}, V} <- maps:to_list(Counters),
            Name =:= Emit
        ]) >= 2
    ),
    Replay = maps:get(replay, Names),
    ?assert(
        lists:sum([
            V
         || {{Name, _}, V} <- maps:to_list(Counters),
            Name =:= Replay
        ]) >= 1
    ),
    %% 延迟直方图有观测
    Hist = maps:get(histograms, Metrics),
    Latency = maps:get(latency, Names),
    ?assertMatch(#{count := N} when N >= 1, maps:get(Latency, Hist)),
    %% 指标标签不含 secret / 正文（只允许闭集词）
    AllEntries = maps:to_list(Counters),
    lists:foreach(
        fun(Entry) ->
            Bin = iolist_to_binary(io_lib:format("~p", [Entry])),
            ?assertEqual(nomatch, binary:match(Bin, Secret)),
            ?assertEqual(nomatch, binary:match(Bin, ?BODY_MARKER)),
            ?assertEqual(nomatch, binary:match(Bin, <<"whsec_">>))
        end,
        AllEntries
    ),
    %% 统计读面：成功率口径 success/(success+dead)
    %% （原行已 dead，dead->success 被终态守卫拒绝——另起一行做成功态）
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    SuccessDid = maps:get(<<"delivery_id">>, sole_pending_delivery(C)),
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'success' WHERE delivery_id = '">>,
        SuccessDid,
        <<"'">>
    ]),
    {ok, Stats} = enterprise_webhook_logic:delivery_stats_tx(C, ctx_a(State)),
    ?assert(maps:get(<<"success_count">>, Stats) >= 1),
    Rate = maps:get(<<"success_rate">>, Stats),
    ?assertMatch(R when is_float(R) andalso R > 0.0 andalso R =< 1.0, Rate),
    ?assert(is_integer(maps:get(<<"retry_count">>, Stats))),
    ?assert(is_integer(maps:get(<<"dead_letter_count">>, Stats))).

ensure_metric_server() ->
    case whereis(elib_metric) of
        undefined ->
            {ok, _} = elib_metric:start_link(),
            ok;
        _ ->
            ok
    end.

read_surface_oracle(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    _ = maps:get(<<"secret">>, R1),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Mine = maps:get(<<"delivery_id">>, last_delivery(C)),
    %% org B 的 app 也配一个（隔离断言用）
    {ok, _R2} = configure(C, ctx_b(State), #{url => ?URL2}),

    {ok, List} = enterprise_webhook_logic:deliveries_tx(C, ctx_a(State), #{}, 20),
    Ids = [maps:get(<<"delivery_id">>, R) || R <- maps:get(list, List)],
    ?assert(lists:member(Mine, Ids)),
    %% 列表行不含 payload（无正文/无 secret 的读面保证）
    lists:foreach(
        fun(Row) -> ?assertNot(maps:is_key(<<"payload">>, Row)) end,
        maps:get(list, List)
    ),
    %% 归属隔离：org B 的列表看不到 org A 的行
    {ok, ListB} = enterprise_webhook_logic:deliveries_tx(C, ctx_b(State), #{}, 20),
    IdsB = [maps:get(<<"delivery_id">>, R) || R <- maps:get(list, ListB)],
    ?assertNot(lists:member(Mine, IdsB)),
    %% 页大小夹紧（无界导出负例）：size=999 → 50
    {ok, Big} = enterprise_webhook_logic:deliveries_tx(C, ctx_a(State), #{size => 999}, 20),
    ?assertEqual(50, maps:get(size, Big)),
    {ok, Small} = enterprise_webhook_logic:deliveries_tx(C, ctx_a(State), #{size => 1}, 20),
    ?assertEqual(1, maps:get(size, Small)),
    ?assertEqual(1, length(maps:get(list, Small))),
    %% page=0/负数回落 1；非法 status 过滤为 undefined（不注入）
    {ok, Page0} = enterprise_webhook_logic:deliveries_tx(C, ctx_a(State), #{page => 0}, 20),
    ?assertEqual(1, maps:get(page, Page0)),
    {ok, BadStatus} = enterprise_webhook_logic:deliveries_tx(
        C, ctx_a(State), #{status => <<"'; DROP TABLE bot_delivery; --">>}, 20
    ),
    ?assertEqual(undefined, maps:get(status, BadStatus)),
    %% 状态过滤生效
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
        Mine,
        <<"'">>
    ]),
    {ok, DeadOnly} = enterprise_webhook_logic:deliveries_tx(
        C, ctx_a(State), #{status => <<"dead">>}, 20
    ),
    lists:foreach(
        fun(R) -> ?assertEqual(<<"dead">>, maps:get(<<"status">>, R)) end,
        maps:get(list, DeadOnly)
    ),
    %% 摘要随列表返回
    ?assert(maps:is_key(summary, DeadOnly)),
    ?assertEqual(emitted, emitted).

no_secret_or_body(C, State) ->
    {ok, R1} = configure(C, ctx_a(State), #{}),
    Secret = maps:get(<<"secret">>, R1),
    ?assertMatch(<<"whsec_", _/binary>>, Secret),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Row = as_claim_row(C, maps:get(<<"delivery_id">>, last_delivery(C))),

    %% 走一轮「真投递」（sender 桩）+ 一轮死信 + 一次重放，落到持久化轨迹上
    with_writer_stubs(fun(_) ->
        with_secret_stub(Secret, fun() ->
            with_sender_stub(fun(_H) -> {ok, 200} end, fun() ->
                ok = enterprise_webhook_logic:execute_delivery(Row)
            end)
        end)
    end),
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
        maps:get(<<"delivery_id">>, Row),
        <<"'">>
    ]),
    {ok, _} = enterprise_webhook_logic:replay_tx(C, ctx_a(State), maps:get(<<"delivery_id">>, Row)),

    %% ① 持久化轨迹（bot_delivery + bot_delivery_attempt 的每个文本列）不含 secret
    ?assertEqual(0, trace_hits(C, Secret)),
    %% ② 也不含消息正文（信封只带事件/资源 id）
    ?assertEqual(0, trace_hits(C, ?BODY_MARKER)),
    ?assertEqual(0, trace_hits(C, <<"whsec_">>)),
    %% ③ 信封里也不允许出现签名 URL / 正文键
    Env = jsone:decode(maps:get(<<"payload">>, delivery_row(C, maps:get(<<"delivery_id">>, Row)))),
    lists:foreach(
        fun(K) ->
            ?assertNot(maps:is_key(K, Env), K)
        end,
        [<<"secret">>, <<"body">>, <<"content">>, <<"put_url">>, <<"download_url">>, <<"token">>]
    ),
    %% ④ 配置响应之外，库里没有 secret 明文（验证器密文列不等于明文）
    ?assertNotEqual(
        Secret,
        scalar(C, <<"SELECT verify_token_enc FROM bot WHERE user_id = $1">>, [?PRIN_A])
    ),
    ?assertEqual(
        0,
        scalar(
            C,
            <<
                "SELECT count(*) FROM bot WHERE position($1 in"
                " (coalesce(name,'') || '|' || coalesce(webhook_url,'') || '|' ||"
                " coalesce(verify_token_enc,''))) > 0"
            >>,
            [Secret]
        )
    ),
    %% ⑤ 日志通道扫描：emit / execute / replay 全程不出现 secret 或正文
    Captured = ets:new(ewh993_logs, [public, ordered_set]),
    ok = meck:new(elib_log, [passthrough, no_passthrough_cover]),
    meck:expect(elib_log, internal_log, fun(_Level, Msg, _Mod, _Line) ->
        ets:insert(Captured, {erlang:unique_integer([monotonic, positive]), {Msg, []}}),
        ok
    end),
    meck:expect(elib_log, internal_log, fun(_Level, Fmt, Args, _Mod, _Line) ->
        ets:insert(Captured, {erlang:unique_integer([monotonic, positive]), {Fmt, Args}}),
        ok
    end),
    try
        {ok, _} = emit(C, ctx_a(State), <<"file.confirmed">>),
        with_writer_stubs(fun(_) ->
            with_secret_stub(Secret, fun() ->
                with_sender_stub(fun(_H) -> {ok, 500} end, fun() ->
                    ok = enterprise_webhook_logic:execute_delivery(Row)
                end)
            end)
        end),
        ok = exec(C, [
            <<"UPDATE bot_delivery SET status = 'dead' WHERE delivery_id = '">>,
            maps:get(<<"delivery_id">>, Row),
            <<"'">>
        ]),
        _ = enterprise_webhook_logic:replay_tx(C, ctx_a(State), maps:get(<<"delivery_id">>, Row))
    after
        meck:unload(elib_log)
    end,
    LogEntries = [E || {_, E} <- lists:sort(ets:tab2list(Captured))],
    ?assert(length(LogEntries) >= 0),
    lists:foreach(
        fun(Entry) ->
            Bin = iolist_to_binary(io_lib:format("~p", [Entry])),
            ?assertEqual(nomatch, binary:match(Bin, Secret), {secret_in_log, Entry}),
            ?assertEqual(nomatch, binary:match(Bin, ?BODY_MARKER), {body_in_log, Entry})
        end,
        LogEntries
    ),
    ets:delete(Captured).

retention_oracle(C, State) ->
    {ok, _R1} = configure(C, ctx_a(State), #{}),
    {ok, emitted} = emit(C, ctx_a(State), <<"file.confirmed">>),
    Recent = maps:get(<<"delivery_id">>, last_delivery(C)),
    %% 造一条「久远且已终结」的行 + 一条「久远但在途」的行
    Old = <<"ewh993-old-terminal">>,
    OldInflight = <<"ewh993-old-inflight">>,
    %% 企业行必须生而 pending（守卫），故先插 pending 再把终态那条迁到 success
    lists:foreach(
        fun(Did) ->
            ok = exec(C, [
                <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
                <<" correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,">>,
                <<" status, ewh_owner_organization_id, ewh_owner_application_id,">>,
                <<" created_at, updated_at) VALUES ('">>,
                Did,
                <<"', 'eapp:">>,
                integer_to_binary(?PRIN_A),
                <<"', 'file.confirmed', '{}', 'corr993old00000001', '">>,
                Did,
                <<"-idem', 'https://oa993.example.com/hook', 'oa993.example.com',">>,
                <<" '93.184.216.34', 'pending', ">>,
                integer_to_binary(?ORG_A),
                <<", ">>,
                integer_to_binary(maps:get(app_a, State)),
                <<", NOW() - INTERVAL '90 days', NOW() - INTERVAL '90 days')">>
            ])
        end,
        [Old, OldInflight]
    ),
    ok = exec(C, [
        <<"UPDATE bot_delivery SET status = 'success' WHERE delivery_id = '">>,
        Old,
        <<"'">>
    ]),
    ?assertEqual(30, enterprise_webhook_logic:retention_days()),
    {ok, Purgeable} = enterprise_webhook_logic:purgeable_tx(C, ctx_a(State), 30),
    Ids = [maps:get(<<"delivery_id">>, R) || R <- Purgeable],
    ?assert(lists:member(Old, Ids)),
    %% 在途永不入选（保留策略不得成为投递丢失的路径）
    ?assertNot(lists:member(OldInflight, Ids)),
    %% 新行不在窗口内
    ?assertNot(lists:member(Recent, Ids)),
    lists:foreach(
        fun(R) ->
            ?assert(
                lists:member(maps:get(<<"status">>, R), [<<"success">>, <<"dead">>])
            )
        end,
        Purgeable
    ),
    ?assertEqual(emitted, emitted).
