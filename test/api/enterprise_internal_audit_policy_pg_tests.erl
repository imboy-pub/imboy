-module(enterprise_internal_audit_policy_pg_tests).

%%%
% INT-BE-03 —— /api/internal/v1/* 写操作审计政策**冻结测试**（验收
% INT-API-02B）：真 Cowboy listener + 真中间件链 + disposable PG
% （intbe02_http_support 同款 harness，与 INT-BE-02 conformance 套件互不
% 干扰——各用一次性 marker 库，listener 均 port 0 系统分配）。
%
% 覆盖（冻结表 enterprise_internal_audit_policy）：
%   ① 政策覆盖完整性：policies() 精确覆盖路由表全部非 GET operation
%      （REQUIRED 27 + DEVIATION 4 = 31，无遗漏无表外）；
%   ② REQUIRED_AUDIT 逐条：真 HTTP mutation 成功 → 对应 action 审计计数
%      +1，且审计行含七字段口径（organization_id / resource_type +
%      resource_id / action / actor_role+actor_user_id /
%      detail.origin_application_id / detail.correlation_id）；
%   ③ 原子性：审计写入失败（meck append_tx 注入）→ 业务整体回滚——业务行
%      与审计行**都不落库**；业务失败（invalid_request）→ 同样两者皆无；
%   ④ 幂等重放：同 key 同 body 精确重放 → 200 + Idempotent-Replayed 头，
%      审计计数不变（重放不重执行业务/audit）；INT-14 重放已消费 code →
%      统一 404 拒绝，无重复审计（single_use_code 冻结条款）；INT-44 每次签发
%      新票并新增审计，由共享 HTTP 审计助手验证；
%   ⑤ REGISTERED_DEVIATION：INT-03/16/17/07 执行后不产生任何审计行
%      （豁免条款冻结；最终全表 action 分布与期望分布精确相等）。
%
% 本套件不改任何生产代码语义；meck 仅用于「审计写入失败」的方向性注入
% （被测代码路径是 logic 内真实 append_tx 调用，翻转其成功值），不伪造
% 任何审计数据。
%%%

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% ① 政策冻结表完整性（纯静态，无 PG）
%%%===================================================================

policy_coverage_test_() ->
    {timeout, 15, fun policy_coverage/0}.

policy_coverage() ->
    NonGet = [
        maps:get(id, R)
     || R <- enterprise_internal_routes:routes(),
        maps:get(method, R) =/= <<"GET">>
    ],
    Required = enterprise_internal_audit_policy:required_ids(),
    Deviation = enterprise_internal_audit_policy:deviation_ids(),
    %% 精确二分覆盖：REQUIRED ∪ DEVIATION = 全部非 GET，交集为空
    ?assertEqual(
        lists:sort(NonGet),
        lists:sort(Required ++ Deviation),
        {policy_gap, lists:sort(NonGet -- (Required ++ Deviation)),
            lists:sort((Required ++ Deviation) -- NonGet)}
    ),
    ?assertEqual(
        length(Required) + length(Deviation),
        length(lists:usort(Required ++ Deviation)),
        overlapping_verdicts
    ),
    ?assertEqual(44, length(enterprise_internal_routes:routes())),
    ?assertEqual(31, length(NonGet)),
    ?assertEqual(27, length(Required)),
    ?assertEqual(4, length(Deviation)),
    lists:foreach(
        fun(Policy) ->
            Id = maps:get(id, Policy),
            [Route] = [R || R <- enterprise_internal_routes:routes(), maps:get(id, R) =:= Id],
            ?assertEqual(maps:get(method, Route), maps:get(method, Policy), {policy_method, Id}),
            ?assertEqual(maps:get(path, Route), maps:get(path, Policy), {policy_path, Id})
        end,
        enterprise_internal_audit_policy:policies()
    ),
    lists:foreach(
        fun({Id, Action, Resource}) ->
            Policy = policy_of(Id),
            ?assertEqual(required_audit, maps:get(verdict, Policy)),
            ?assertEqual(Action, maps:get(audit_action, Policy)),
            ?assertEqual(Resource, maps:get(audit_resource_type, Policy))
        end,
        [
            {<<"INT-43">>, <<"directory.synced">>, <<"directory_sync">>},
            {<<"INT-44">>, <<"oa.login_ticket_issued">>, <<"enterprise_application">>}
        ]
    ),
    %% DEVIATION 冻结集合（豁免不得静默扩散）
    ?assertEqual(
        lists:sort([<<"INT-03">>, <<"INT-07">>, <<"INT-16">>, <<"INT-17">>]),
        lists:sort(Deviation)
    ),
    lists:foreach(
        fun(Id) ->
            P = policy_of(Id),
            ?assertNotEqual(<<>>, maps:get(reason, P, <<>>), {deviation_needs_reason, Id}),
            ?assertEqual(
                null, maps:get(audit_action, P, undefined), {deviation_has_no_action, Id}
            )
        end,
        Deviation
    ),
    %% REQUIRED 冻结 action / resource_type 非空且 verdict/audit_action 可查
    lists:foreach(
        fun(Id) ->
            ?assertEqual(
                required_audit, enterprise_internal_audit_policy:verdict(Id)
            ),
            ?assert(
                is_binary(enterprise_internal_audit_policy:audit_action(Id)),
                {required_needs_action, Id}
            ),
            P = policy_of(Id),
            ?assert(is_binary(maps:get(audit_resource_type, P)), {required_needs_rtype, Id})
        end,
        Required
    ),
    %% 未登记 operation → fail-closed（verdict 抛错而非放行）
    ?assertError(
        {audit_policy_missing, <<"INT-404">>},
        enterprise_internal_audit_policy:verdict(<<"INT-404">>)
    ).

policy_of(Id) ->
    [P] = [P || P <- enterprise_internal_audit_policy:policies(), maps:get(id, P) =:= Id],
    P.

%%%===================================================================
%%% ②③④⑤ 真 HTTP + 真 PG（disposable marker 库）
%%%===================================================================

audit_policy_http_test_() ->
    {timeout, 900,
        {setup, fun intbe02_http_support:setup_all/0, fun intbe02_http_support:teardown_all/1,
            fun(S) -> {timeout, 900, fun() -> run_audit(S) end} end}}.

run_audit(S) ->
    #{conn := C} = S,
    enterprise_internal_audit_http_checks:reset(),
    seed_delivery_row(C, S),
    Ctx = positive_with_audit(S),
    atomicity(S, Ctx),
    replay_no_dup(S, Ctx),
    ok = enterprise_internal_audit_http_checks:seats(S),
    enterprise_channel_write_http_checks:run(S),
    enterprise_workspace_write_http_checks:run(S),
    ok = enterprise_internal_audit_http_checks:directory_sync(S),
    ok = enterprise_internal_audit_http_checks:oa_login(S),
    enterprise_internal_audit_http_checks:assert_complete(),
    [].

%% ------------------------------------------------------------------
%% ② REQUIRED 逐条：成功 → 审计 +1 + 七字段
%% ------------------------------------------------------------------

positive_with_audit(S) ->
    Port = maps:get(port, S),
    C = maps:get(conn, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    SSO = intbe02_http_support:auth(maps:get(cred_sso, S)),
    H1 = intbe02_http_support:fixture(ext_h1, x),
    H2 = intbe02_http_support:fixture(ext_h2, x),
    H3 = intbe02_http_support:fixture(ext_h3, x),
    WsA1 = intbe02_http_support:fixture(ws_a1, x),
    Redirect = intbe02_http_support:fixture(redirect, x),
    Nonce = intbe02_http_support:fixture(nonce, x),

    %% ---- INT-02 绑定（seed 的 bind 走 repo 直调、不经 logic、无审计，
    %%      本条是第一笔 identity.mapping.bound）----
    http_ok(
        C,
        Port,
        <<"PUT">>,
        <<"/api/internal/v1/identity-mappings">>,
        #{<<"external_user_id">> => <<"intbe03-ext-h4">>, <<"user_id">> => 995017},
        A,
        <<"intbe03-idem-02">>
    ),
    assert_audit_increment(C, <<"identity.mapping.bound">>, 0, 1),

    %% ---- INT-04 建群 ----
    Body04 = #{
        <<"workspace_id">> => WsA1,
        <<"title">> => <<"intbe03 audit group"/utf8>>,
        <<"members">> => [H1, H2]
    },
    #{<<"group_id">> := Gid} = http_ok(
        C, Port, <<"POST">>, <<"/api/internal/v1/groups">>, Body04, A, <<"intbe03-idem-04">>
    ),
    assert_audit_increment(C, <<"group.created">>, 0, 1),
    %% 七字段：org / action / actor_role / application / correlation + resource
    assert_audit_row(C, #{
        <<"action">> => <<"group.created">>,
        <<"organization_id">> => intbe02_http_support:fixture(org_a, x),
        <<"resource_type">> => <<"group">>,
        <<"resource_id">> => Gid,
        <<"actor_role">> => <<"enterprise_application">>,
        <<"actor_user_id">> => 995014,
        <<"origin_application_id">> => integer_to_binary(app_id(S, <<"intbe02-oa-a">>))
    }),

    %% ---- INT-05 加成员 ----
    GrpPath = <<"/api/internal/v1/groups/", (integer_to_binary(Gid))/binary>>,
    GrpMembersPath = <<GrpPath/binary, "/members">>,
    http_ok(
        C,
        Port,
        <<"PUT">>,
        GrpMembersPath,
        #{<<"external_user_ids">> => [H3]},
        A,
        <<"intbe03-idem-05">>
    ),
    assert_audit_increment(C, <<"group.members.added">>, 0, 1),

    %% ---- INT-20 角色 ----
    http_ok(
        C,
        Port,
        <<"PUT">>,
        <<GrpMembersPath/binary, "/roles">>,
        #{<<"roles">> => [#{<<"external_user_id">> => H3, <<"role">> => 2}]},
        A,
        <<"intbe03-idem-20">>
    ),
    assert_audit_increment(C, <<"group.member_roles.set">>, 0, 1),

    %% ---- INT-19 改名 ----
    http_ok(
        C,
        Port,
        <<"PATCH">>,
        GrpPath,
        #{<<"title">> => <<"intbe03 renamed"/utf8>>},
        A,
        <<"intbe03-idem-19">>
    ),
    assert_audit_increment(C, <<"group.updated">>, 0, 1),

    %% ---- INT-06 移成员 ----
    http_ok(
        C,
        Port,
        <<"DELETE">>,
        GrpMembersPath,
        #{<<"external_user_ids">> => [H3]},
        A,
        <<"intbe03-idem-06">>
    ),
    assert_audit_increment(C, <<"group.members.removed">>, 0, 1),

    %% ---- INT-07 presign（DEVIATION：无审计行）----
    #{<<"object_key">> := ObjectKey} = Registration = http_ok(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/files/presign">>,
        #{<<"file_name">> => <<"intbe03-report.txt">>, <<"mime_type">> => <<"text/plain">>},
        A,
        <<"intbe03-idem-07">>
    ),
    assert_audit_increment(C, <<"group.updated">>, 1, 1),
    ?assertEqual(0, audit_count(C, <<"file.presigned">>), presign_deviation_zero_rows),
    ?assertEqual(0, audit_count(C, <<"file.presign">>), presign_deviation_zero_rows),

    %% ---- INT-08 confirm（REQUIRED；真实 capability PUT 固定内容）----
    Bytes = binary:copy(<<"x">>, 2048),
    ok = intbe02_http_support:upload_file(Port, Registration, Bytes),
    Resp08 =
        enterprise_internal_audit_http_checks:http(
            C,
            Port,
            <<"POST">>,
            <<"/api/internal/v1/files/confirm">>,
            #{
                <<"object_key">> => ObjectKey,
                <<"file_hash256">> => hash256(<<"x">>)
            },
            maps:merge(A, intbe02_http_support:idem(<<"intbe03-idem-08">>))
        ),
    ?assertEqual(200, maps:get(status, Resp08), maps:get(body, Resp08)),
    #{<<"size">> := 2048, <<"mime_type">> := <<"text/plain">>} =
        jsone:decode(maps:get(body, Resp08)),
    Confirmed = intbe02_http_support:one(C,
        <<"SELECT file_hash256 FROM attachment WHERE path=$1">>, [ObjectKey]),
    ?assertEqual(hash256(Bytes), maps:get(<<"file_hash256">>, Confirmed)),
    assert_audit_increment(C, <<"file.confirmed">>, 0, 1),
    assert_audit_row(C, #{
        <<"action">> => <<"file.confirmed">>,
        <<"organization_id">> => intbe02_http_support:fixture(org_a, x),
        <<"resource_type">> => <<"attachment">>,
        <<"resource_id">> => resource_id_of_action(C, <<"file.confirmed">>),
        <<"actor_role">> => <<"enterprise_application">>
    }),

    %% ---- INT-22 governance（REQUIRED；DEVIATION 不含本条）----
    http_ok(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/files/governance">>,
        #{
            <<"op">> => <<"set_retention">>,
            <<"object_key">> => ObjectKey,
            <<"retention_days">> => 3650
        },
        A,
        <<"intbe03-idem-22">>
    ),
    assert_audit_increment(C, <<"file.governance.op">>, 0, 1),
    GovOp = detail_of_action(C, <<"file.governance.op">>, <<"op">>),
    ?assertEqual(<<"set_retention">>, GovOp),

    %% ---- INT-09 direct（REQUIRED；已有审计行）----
    with_dns(fun() ->
        http_ok(
            C,
            Port,
            <<"POST">>,
            <<"/api/internal/v1/messages/direct">>,
            #{
                <<"sender_mode">> => <<"human">>,
                <<"sender_user_id">> => H1,
                <<"recipient_user_id">> => H2,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"intbe03 audit direct"/utf8>>
            },
            A,
            <<"intbe03-idem-09">>
        )
    end),
    assert_audit_increment(C, <<"message.enterprise.accepted">>, 0, 1),

    %% ---- INT-10 群发（REQUIRED；同 action 第二行）----
    with_dns(fun() ->
        http_ok(
            C,
            Port,
            <<"POST">>,
            <<GrpPath/binary, "/messages">>,
            #{
                <<"sender_mode">> => <<"human">>,
                <<"sender_user_id">> => H1,
                <<"msg_type">> => <<"text">>,
                <<"content">> => <<"intbe03 audit group msg"/utf8>>
            },
            A,
            <<"intbe03-idem-10">>
        )
    end),
    assert_audit_increment(C, <<"message.enterprise.accepted">>, 1, 2),

    %% ---- INT-11 好友申请（REQUIRED）----
    http_ok(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/friend-requests">>,
        #{<<"sender_user_id">> => H2, <<"target_user_id">> => H1},
        A,
        <<"intbe03-idem-11">>
    ),
    assert_audit_increment(C, <<"friend_request.created">>, 0, 1),

    %% ---- INT-12 webhook 配置（REQUIRED）----
    with_dns(fun() ->
        http_ok(
            C,
            Port,
            <<"PUT">>,
            <<"/api/internal/v1/webhook">>,
            #{
                <<"url">> => <<"https://oa.customer.example.com/intbe03/hook">>,
                <<"events">> => [<<"message.enterprise.accepted">>, <<"file.confirmed">>]
            },
            A,
            <<"intbe03-idem-12">>
        )
    end),
    assert_audit_increment(C, <<"webhook.configured">>, 0, 1),

    %% ---- INT-13 replay（REQUIRED）----
    with_dns(fun() ->
        http_ok(
            C,
            Port,
            <<"POST">>,
            <<"/api/internal/v1/webhook/deliveries/intbe03-dlv-0001/replay">>,
            #{},
            A,
            <<"intbe03-idem-13">>
        )
    end),
    assert_audit_increment(C, <<"webhook.delivery.replayed">>, 0, 1),
    NewDlv = detail_of_action(C, <<"webhook.delivery.replayed">>, <<"delivery_id">>),
    ?assert(is_binary(NewDlv), new_delivery_in_detail),

    %% ---- INT-32 测试投递（REQUIRED；v1.1.1 追加，端点已由 INT-12 配置）----
    with_dns(fun() ->
        http_ok(
            C,
            Port,
            <<"POST">>,
            <<"/api/internal/v1/webhook/test-delivery">>,
            #{},
            A,
            <<"intbe03-idem-32">>
        )
    end),
    assert_audit_increment(C, <<"webhook.test_delivered">>, 0, 1),

    %% ---- INT-14 SSO exchange（REQUIRED；幂等豁免但审计 REQUIRED）----
    {ok, #{<<"code">> := SsoCode}} =
        enterprise_oa_sso_logic:issue_code_tx(C, 995017, #{
            <<"application_key">> => <<"intbe02-oa-sso">>,
            <<"redirect_uri">> => Redirect,
            <<"nonce">> => Nonce
        }),
    Body14 = #{<<"code">> => SsoCode, <<"redirect_uri">> => Redirect, <<"nonce">> => Nonce},
    http_ok_no_idem(C, Port, <<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, Body14, SSO),
    assert_audit_increment(C, <<"oa.sso.exchanged">>, 0, 1),
    assert_audit_row(C, #{
        <<"action">> => <<"oa.sso.exchanged">>,
        <<"organization_id">> => intbe02_http_support:fixture(org_a, x),
        <<"resource_type">> => <<"enterprise_oa_sso">>,
        <<"actor_role">> => <<"enterprise_application">>,
        <<"actor_user_id">> => 995017
    }),

    %% ---- INT-15 撤销映射（REQUIRED）----
    http_ok(
        C,
        Port,
        <<"DELETE">>,
        <<"/api/internal/v1/identity-mappings">>,
        #{<<"external_user_id">> => <<"intbe03-ext-h4">>},
        A,
        <<"intbe03-idem-15">>
    ),
    assert_audit_increment(C, <<"identity.mapping.revoked">>, 0, 1),

    %% ---- INT-03/16/17（DEVIATION 只读 POST：执行后无任何审计行）----
    http_ok_no_idem(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/identity-mappings/resolve">>,
        #{<<"external_user_ids">> => [H1]},
        A
    ),
    http_ok_no_idem(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/identity-mappings/directory">>,
        #{<<"page_size">> => 10},
        A
    ),
    http_ok_no_idem(
        C,
        Port,
        <<"POST">>,
        <<"/api/internal/v1/directory/users">>,
        #{<<"workspace_id">> => WsA1},
        A
    ),
    TotalBefore = total_audit(C),
    ?assertEqual(16, TotalBefore),

    %% ---- INT-21 归档（REQUIRED；最后执行）----
    http_ok(C, Port, <<"DELETE">>, GrpPath, #{}, A, <<"intbe03-idem-21">>),
    assert_audit_increment(C, <<"group.archived">>, 0, 1),

    %% 全表 action 分布与冻结政策期望**精确**相等（DEVIATION 零行的机械证明）
    Expected = #{
        <<"identity.mapping.bound">> => 1,
        <<"group.created">> => 1,
        <<"group.members.added">> => 1,
        <<"group.members.removed">> => 1,
        <<"group.member_roles.set">> => 1,
        <<"group.updated">> => 1,
        <<"group.archived">> => 1,
        <<"file.confirmed">> => 1,
        <<"file.governance.op">> => 1,
        <<"message.enterprise.accepted">> => 2,
        <<"friend_request.created">> => 1,
        <<"webhook.configured">> => 1,
        <<"webhook.delivery.replayed">> => 1,
        <<"webhook.test_delivered">> => 1,
        <<"oa.sso.exchanged">> => 1,
        <<"identity.mapping.revoked">> => 1
    },
    ?assertEqual(Expected, audit_distribution(C)),
    Ctx = #{
        gid => Gid,
        object_key => ObjectKey,
        body04 => Body04,
        body14 => Body14,
        sso_headers => SSO
    },
    Ctx.

%% ------------------------------------------------------------------
%% ③ 原子性：审计失败 / 业务失败 → 业务行与审计行都不落
%% ------------------------------------------------------------------

atomicity(S, Ctx) ->
    Port = maps:get(port, S),
    C = maps:get(conn, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    Body = #{
        <<"workspace_id">> => intbe02_http_support:fixture(ws_a1, x),
        <<"title">> => <<"audit-atomic-probe"/utf8>>,
        <<"members">> => [intbe02_http_support:fixture(ext_h1, x)]
    },

    %% (a) 审计写入失败注入：append_tx 返回 error → 业务整体回滚
    Before = audit_count(C, <<"group.created">>),
    GroupCount0 = group_count(C),
    ok = meck:new(enterprise_audit_event_repo, [passthrough, no_link]),
    ok = meck:expect(
        enterprise_audit_event_repo,
        append_tx,
        fun(_Conn, _OrgId, _Event) -> {error, audit_down} end
    ),
    Resp = intbe02_http_support:http(
        Port,
        <<"POST">>,
        <<"/api/internal/v1/groups">>,
        Body,
        maps:merge(A, intbe02_http_support:idem(<<"intbe03-idem-atomic">>))
    ),
    try
        meck:unload(enterprise_audit_event_repo)
    catch
        _:_ -> ok
    end,
    ?assertEqual(500, maps:get(status, Resp), {atomic_status, maps:get(body, Resp)}),
    ?assertNotEqual(nomatch, binary:match(maps:get(body, Resp), <<"internal_error">>)),
    ?assertEqual(GroupCount0, group_count(C), business_row_must_rollback),
    ?assertEqual(Before, audit_count(C, <<"group.created">>), audit_row_must_rollback),

    %% (b) 业务失败（空更新语义前置：缺 title 且缺 introduction → 400）：
    %%     审计行同样不落（审计只挂在成功路径）
    BadResp = intbe02_http_support:http(
        Port,
        <<"POST">>,
        <<"/api/internal/v1/groups">>,
        #{<<"workspace_id">> => intbe02_http_support:fixture(ws_a1, x)},
        maps:merge(A, intbe02_http_support:idem(<<"intbe03-idem-bad">>))
    ),
    ?assertEqual(400, maps:get(status, BadResp)),
    ?assertEqual(GroupCount0, group_count(C), bad_business_must_not_insert),
    ?assertEqual(Before, audit_count(C, <<"group.created">>), bad_business_no_audit),
    _ = Ctx,
    ok.

%% ------------------------------------------------------------------
%% ④ 幂等重放：不重复审计（或符合 INT-14 single_use_code 条款）
%% ------------------------------------------------------------------

replay_no_dup(S, Ctx) ->
    Port = maps:get(port, S),
    C = maps:get(conn, S),
    A = intbe02_http_support:auth(maps:get(cred_a, S)),
    #{body04 := Body04, body14 := Body14, sso_headers := SSO} = Ctx,

    %% (a) INT-04 同 key 同 body 精确重放：200 + Idempotent-Replayed 头 +
    %%     审计计数不变（§11 重放不重执行业务/audit）
    Before = audit_count(C, <<"group.created">>),
    Replay = intbe02_http_support:http(
        Port,
        <<"POST">>,
        <<"/api/internal/v1/groups">>,
        Body04,
        maps:merge(A, intbe02_http_support:idem(<<"intbe03-idem-04">>))
    ),
    ?assertEqual(200, maps:get(status, Replay)),
    ?assertEqual(
        <<"true">>,
        maps:get(
            <<"idempotent-replayed">>, maps:get(headers, Replay, #{}), undefined
        ),
        replay_header_missing
    ),
    ?assertEqual(Before, audit_count(C, <<"group.created">>), replay_must_not_reaudit),

    %% (b) INT-14 重放已消费 code：统一 404 拒绝 + 审计计数不变（CAS 条款）
    SsoBefore = audit_count(C, <<"oa.sso.exchanged">>),
    Replay14 = intbe02_http_support:http(
        Port, <<"POST">>, <<"/api/internal/v1/oa/sso/exchange">>, Body14, SSO
    ),
    assert_err(Replay14, <<"resource_not_found">>),
    ?assertEqual(SsoBefore, audit_count(C, <<"oa.sso.exchanged">>)),
    ok.

%%%===================================================================
%%% harness helpers
%%%===================================================================

%% INT-13 正例需要本 App 归属、终态（success）的 bot_delivery 行（与
%% conformance 同款两步：插 pending → UPDATE 迁终态；DB 守卫要求生而
%% pending）。
seed_delivery_row(C, S) ->
    AppA = integer_to_binary(maps:get(app_a, S)),
    ok = intbe02_http_support:sql_exec(C, [
        <<"INSERT INTO bot (user_id, name, username, owner_uid, webhook_url, events,">>,
        <<" is_public, status, created_at, updated_at) VALUES (995014, 'intbe03 bot',">>,
        <<" 'intbe03-bot-995014', 995001, 'https://oa.customer.example.com/intbe03/hook',">>,
        <<" '[]'::jsonb, false, 1, NOW(), NOW())">>
    ]),
    ok = intbe02_http_support:sql_exec(C, [
        <<"INSERT INTO bot_delivery (delivery_id, bot_id, event_type, payload,">>,
        <<" correlation_id, idempotency_key, webhook_url, webhook_host, pinned_ip,">>,
        <<" status, ewh_owner_organization_id, ewh_owner_application_id,">>,
        <<" created_at, updated_at) VALUES ('intbe03-dlv-0001', 'eapp:995014',">>,
        <<" 'file.confirmed', '{}', 'intbe03-corr-0001', 'intbe03-dlv-idem-0001',">>,
        <<" 'https://oa.customer.example.com/intbe03/hook',">>,
        <<" 'oa.customer.example.com', '93.184.216.34', 'pending', 995101, ">>,
        AppA,
        <<", NOW() - INTERVAL '2 days', NOW() - INTERVAL '2 days')">>
    ]),
    ok = intbe02_http_support:sql_exec(
        C, <<"UPDATE bot_delivery SET status = 'success' WHERE delivery_id = 'intbe03-dlv-0001'">>
    ),
    ok.

http_ok(C, Port, Method, Path, Body, Headers, IdemKey) ->
    Resp =
        enterprise_internal_audit_http_checks:http(
            C,
            Port,
            Method,
            Path,
            Body,
            maps:merge(Headers, intbe02_http_support:idem(IdemKey))
        ),
    ?assertEqual(
        200, maps:get(status, Resp), {unexpected_status, Method, Path, maps:get(body, Resp)}
    ),
    jsone:decode(maps:get(body, Resp)).

http_ok_no_idem(C, Port, Method, Path, Body, Headers) ->
    Resp = enterprise_internal_audit_http_checks:http(C, Port, Method, Path, Body, Headers),
    ?assertEqual(
        200, maps:get(status, Resp), {unexpected_status, Method, Path, maps:get(body, Resp)}
    ),
    jsone:decode(maps:get(body, Resp)).

assert_err(Resp, Code) ->
    ?assertEqual(enterprise_internal_error:http_status(Code), maps:get(status, Resp)),
    ?assertNotEqual(nomatch, binary:match(maps:get(body, Resp), Code)).

with_dns(Fun) -> intbe02_http_support:with_public_dns(Fun).

app_id(S, Key) ->
    maps:get(
        Key,
        #{
            <<"intbe02-oa-a">> => maps:get(app_a, S),
            <<"intbe02-oa-sso">> => maps:get(app_sso, S)
        }
    ).

%% ---- 审计断言（marker 库直连 SQL；append-only 表只读查询）----

audit_count(C, Action) ->
    Row = intbe02_http_support:one(
        C, <<"SELECT count(*)::int AS n FROM enterprise_audit_event WHERE action = $1">>, [
            Action
        ]
    ),
    maps:get(<<"n">>, Row).

total_audit(C) ->
    Row = intbe02_http_support:one(
        C, <<"SELECT count(*)::int AS n FROM enterprise_audit_event">>, []
    ),
    maps:get(<<"n">>, Row).

audit_distribution(C) ->
    case
        elib_pg:query(
            C,
            <<"SELECT action, count(*)::int AS n FROM enterprise_audit_event GROUP BY action">>,
            []
        )
    of
        {ok, Rows} ->
            maps:from_list([{maps:get(<<"action">>, R), maps:get(<<"n">>, R)} || R <- Rows]);
        {error, Reason} ->
            erlang:error({intbe03_sql_error, Reason})
    end.

%% REQUIRED action 计数变化断言（from → to）
assert_audit_increment(C, Action, From, To) ->
    ?assertEqual(To, audit_count(C, Action), {audit_count_mismatch, Action, From, To}).

%% 七字段口径断言（可给出各列期望；resource_id/actor 可能为 null 的语义见
%% 冻结政策表——null 资源与 actor 明细均冻结在政策内）
assert_audit_row(C, Expected) ->
    Action = maps:get(<<"action">>, Expected),
    Row = intbe02_http_support:one(
        C,
        <<
            "SELECT organization_id, resource_type, resource_id, action, actor_user_id,"
            " actor_role, detail->>'origin_application_id' AS origin_application_id,"
            " detail->>'correlation_id' AS correlation_id"
            " FROM enterprise_audit_event WHERE action = $1"
        >>,
        [Action]
    ),
    lists:foreach(
        fun({K, V}) ->
            ?assertEqual(V, maps:get(K, Row, undefined), {audit_field_mismatch, Action, K})
        end,
        maps:to_list(Expected)
    ),
    ?assert(
        is_binary(maps:get(<<"correlation_id">>, Row, undefined)) andalso
            maps:get(<<"correlation_id">>, Row) =/= <<>>,
        {audit_correlation_missing, Action}
    ),
    ?assert(
        is_binary(maps:get(<<"origin_application_id">>, Row, undefined)),
        {audit_application_missing, Action}
    ).

resource_id_of_action(C, Action) ->
    Row = intbe02_http_support:one(
        C, <<"SELECT resource_id FROM enterprise_audit_event WHERE action = $1">>, [Action]
    ),
    maps:get(<<"resource_id">>, Row).

detail_of_action(C, Action, Key) ->
    Row = intbe02_http_support:one(
        C,
        iolist_to_binary([
            <<"SELECT detail->>'">>, Key, <<"' AS v FROM enterprise_audit_event WHERE action = $1">>
        ]),
        [Action]
    ),
    maps:get(<<"v">>, Row).

group_count(C) ->
    Row = intbe02_http_support:one(
        C,
        <<"SELECT count(*)::int AS n FROM \"group\" WHERE title = 'audit-atomic-probe'">>,
        []
    ),
    maps:get(<<"n">>, Row).

hash256(S) ->
    Bin = crypto:hash(sha256, S),
    <<<<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Bin>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
