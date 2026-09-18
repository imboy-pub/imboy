-module(cs_org_compat_tests).

%% ORG-08：Organization V1 × Customer Service 兼容矩阵（Human path 全矩阵）。
%%
%% 依据 `2026-09-16-enterprise-organization-cs-compatibility.md` §7 Acceptance：
%%   * CS-ORG-A01 active operator assignment + suspend member
%%     → Enterprise/CS auth 立即拒绝（auth + DB integration）；
%%   * CS-ORG-A02 active assignment + direct remove
%%     → offboarding guard 拒绝（PG 23514 offboarding_required）；
%%   * CS-ORG-A03 handover complete + remove member
%%     → 成功，Seat/Session 保持（PG row assertions）；
%%   * CS-ORG-A04 archived Org + create/claim Session
%%     → fail closed（{error, organization_archived}；restore 后恢复）；
%%   * CS-ORG-A05 Org member but no Workspace member + access Session
%%     → denied（governance/membership 不授予 Session 面；scope 显式）；
%%   * CS-ORG-A06 default Workspace changes + read existing Session
%%     → persisted workspace unchanged（显式 scope，无默认重解析）；
%%   * CS-ORG-A07/A08 Agent path —— 依赖 ORG-06 + Agent Grant（ORG-08 卡：
%%     「Human path 先行；Agent operator path 不在本任务」），本轮 NOT_STARTED，
%%     由 ORG-06 后的 Agent/CS integration 补齐。
%%
%% 冻结语义自证（零重解释）：Seat 仍是 Business Identity 运营 Profile、
%% Session 显式 Org+Workspace、operator=active Assignment —— A03/A04/A06 的
%% PG row 断言即其运行时证据。
%%
%% 运行：make eunit-local t=cs_org_compat_tests
%% PG：一次性容器 imboy-org08-pg18 @127.0.0.1:4393（可用 ORG08_PGPORT 覆盖）；
%% 环境不可用 ⇒ erlang:error/1（不是 skip）：环境问题不得被当成 PASS。

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_org_compat_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    ensure_test_pg_conf(),
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} ->
            {ok, Conn};
        {error, Reason} ->
            erlang:error({cs_org_compat_db_unavailable, Reason})
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_suspend_member_denies_cs_and_eb_auth/0},
        {timeout, 60, fun a02_direct_remove_blocked_by_offboarding_guard/0},
        {timeout, 60, fun a03_handover_then_remove_keeps_seat_and_session/0},
        {timeout, 60, fun a04_archived_org_denies_new_session_and_claim/0},
        {timeout, 60, fun a04_archive_idempotent_and_cs_state_untouched/0},
        {timeout, 60, fun a05_org_membership_alone_does_not_open_session_surface/0},
        {timeout, 60, fun a06_default_workspace_change_keeps_persisted_session_scope/0}
    ];
cases({error, Reason}) ->
    erlang:error({cs_org_compat_db_unavailable, Reason}).

%% ===================================================================
%% CS-ORG-A01
%% ===================================================================

a01_suspend_member_denies_cs_and_eb_auth() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    try
        %% A01 前置：active operator assignment（customer_service 职能）+ enabled
        %% seat → cs_seat 放行；owner 治理面放行。
        CsAssignment = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity_assignment"
                " (id,organization_id,business_identity_id,function_key,user_id,status,assigned_by,version)"
                " VALUES ($1,$2,$3,'customer_service',$4,'active',$5,1)"
            >>,
            [CsAssignment, Org, Service, Actor, Owner]
        ),
        {ok, _} = seat_auth(Org, Actor),
        {ok, _} = owner_auth(Org, Owner),
        %% suspend member（即时撤权，不自动改 Assignment/Seat —— §6 矩阵）
        ok = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='suspended', updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, Actor]
        ),
        %% CS 坐席面：下一个请求即拒（逐请求 facts，无缓存）
        {error, {member_not_active, suspended}} = seat_auth(Org, Actor),
        %% EB 治理面同样即时拒绝：admin 成员 suspend 后 governance 立即失效。
        %% （owner 行不可用同法验证：C04 deferred guard 拒绝 suspend/移除
        %% owner_id 指向的成员行，须先 transfer——恰是冻结语义，不在本测试绕过。）
        Admin = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO \"user\"(id,password,account,reg_ip,reg_cosv)"
                " VALUES ($1,'x',$2,'127.0.0.1','x')"
            >>,
            [Admin, admin_account(Admin)]
        ),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'admin','active')"
            >>,
            [Org, Admin]
        ),
        {ok, _} = owner_auth(Org, Admin),
        ok = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='suspended', updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, Admin]
        ),
        {error, {member_not_active, suspended}} = owner_auth(Org, Admin),
        %% §6：suspend 不自动结束 Assignment / 不动 Seat（冻结语义自证）
        ActiveAssignments = ?FIX:scalar(
            <<
                "SELECT count(*) FROM organization_business_identity_assignment"
                " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
            >>,
            [Org, Actor]
        ),
        ?assertEqual(2, ActiveAssignments),
        ?assertEqual(1, ?FIX:count(Org, seats))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS-ORG-A02
%% ===================================================================

a02_direct_remove_blocked_by_offboarding_guard() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    try
        %% 前置：active assignment（fixture 自带）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM organization_business_identity_assignment"
                    " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
                >>,
                [Org, Actor]
            )
        ),
        %% 直接 remove → DB offboarding guard 23514（trg_organization_member_offboarding_guard）
        {error, Err} = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='removed', updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
            >>,
            [Org, Actor]
        ),
        ?assertEqual(<<"23514">>, pg_error_code(Err)),
        ?assert(
            binary:match(pg_error_message(Err), <<"offboarding_required">>) =/= nomatch
        ),
        %% 成员行未被改动（fail-closed 无部分效果）
        ?assertEqual(<<"active">>, member_status(Org, Actor))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS-ORG-A03
%% ===================================================================

a03_handover_then_remove_keeps_seat_and_session() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Contact = maps:get(contact_id, Scope),
    Conversation = maps:get(conversation_id, Scope),
    try
        %% 既有 Session（历史不删）
        {ok, Session} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000000
        }),
        SessionId = maps:get(id, Session),
        %% handover：active assignment → ended（offboarding 交接完成）
        ok = ?FIX:exec(
            <<
                "UPDATE organization_business_identity_assignment"
                " SET status='ended', ended_at=CURRENT_TIMESTAMP, updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
            >>,
            [Org, Actor]
        ),
        %% remove member 成功
        ok = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='removed', updated_at=CURRENT_TIMESTAMP"
                " WHERE organization_id=$1 AND user_id=$2 AND status='active'"
            >>,
            [Org, Actor]
        ),
        %% Seat/Session 保持（§6：remove 不删历史、Seat 保留）
        ?assertEqual(1, ?FIX:count(Org, seats)),
        ?assertEqual(1, ?FIX:count(Org, sessions)),
        Row = session_row(Org, Workspace, SessionId),
        ?assertEqual(<<"queued">>, maps:get(<<"status">>, Row)),
        %% queued 会话未绑定坐席 identity（claim 才绑定）——主体字段零迁移
        ?assertEqual(null, maps:get(<<"business_identity_id">>, Row)),
        ?assertEqual(Contact, maps:get(<<"contact_id">>, Row)),
        ?assertEqual(Conversation, maps:get(<<"conversation_id">>, Row))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS-ORG-A04
%% ===================================================================

a04_archived_org_denies_new_session_and_claim() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Actor = maps:get(actor_user_id, Scope),
    Service = maps:get(service_identity_id, Scope),
    Contact = maps:get(contact_id, Scope),
    Conversation = maps:get(conversation_id, Scope),
    try
        %% active 期：开会话成功（queued）
        {ok, Session} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000000
        }),
        SessionId = maps:get(id, Session),
        %% ORG-02 archive command（真实生命周期入口，C16）
        {ok, #{<<"status">> := <<"archived">>}} = organization_logic:archive(Owner, Org),
        %% 新 Session 拒绝（稳定 denial）
        {error, organization_archived} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000001
        }),
        %% 新 claim 拒绝（稳定 denial）
        {error, organization_archived} = customer_service_facade:claim(Org, #{
            workspace_id => Workspace,
            session_id => SessionId,
            expected_version => 1,
            at => 1700000000002,
            business_identity_id => Service,
            actor_user_id => Actor
        }),
        %% 授权只读放行（C16：archived 允许只读），历史 Session 原样
        {ok, Fetched} = customer_service_facade:fetch_session(Org, #{
            workspace_id => Workspace, session_id => SessionId
        }),
        ?assertEqual(queued, maps:get(status, Fetched)),
        ?assertEqual(Workspace, maps:get(workspace_id, Fetched)),
        %% restore 后新写恢复（Invitation/Restore 完成后再进 CS onboarding）
        {ok, #{<<"status">> := <<"active">>}} = organization_logic:restore(Owner, Org),
        %% 新 conversation（真源在 enterprise_conversation，FK 要求真实存在）
        Conv2 = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO enterprise_conversation"
                " (id,organization_id,workspace_id,contact_id,business_identity_id,status,version,"
                "  notice_version,consent_at,consent_subject,consent_evidence_kind)"
                " VALUES ($1,$2,$3,$4,$5,'active',1,'cs08-notice-v2',CURRENT_TIMESTAMP,$6,'synthetic')"
            >>,
            [Conv2, Org, Workspace, Contact, Service, conv_name(Conv2)]
        ),
        %% 新 conversation 上的新 Session 可开（旧 queued 会话占用原 conversation，
        %% 同会话未关闭唯一索引按既有合同裁决 conflict）
        {ok, _} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conv2,
            at => 1700000000003
        }),
        {ok, _} = customer_service_facade:claim(Org, #{
            workspace_id => Workspace,
            session_id => SessionId,
            expected_version => 1,
            at => 1700000000004,
            business_identity_id => Service,
            actor_user_id => Actor
        })
    after
        ?FIX:cleanup(Scope)
    end.

a04_archive_idempotent_and_cs_state_untouched() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Contact = maps:get(contact_id, Scope),
    Conversation = maps:get(conversation_id, Scope),
    try
        {ok, _} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000000
        }),
        {ok, _} = organization_logic:archive(Owner, Org),
        SessionsBefore = ?FIX:count(Org, sessions),
        EventsBefore = ?FIX:count(Org, events),
        SeatsBefore = ?FIX:count(Org, seats),
        %% 重复 archive：幂等（返回稳定当前状态，不产生重复审计/CS 副作用）
        {ok, #{<<"status">> := <<"archived">>}} = organization_logic:archive(Owner, Org),
        {error, organization_archived} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000001
        }),
        %% CS state 零改动（ORG-08 IDEMPOTENCY：重复 denial 不修改 CS state）
        ?assertEqual(SessionsBefore, ?FIX:count(Org, sessions)),
        ?assertEqual(EventsBefore, ?FIX:count(Org, events)),
        ?assertEqual(SeatsBefore, ?FIX:count(Org, seats))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS-ORG-A05
%% ===================================================================

a05_org_membership_alone_does_not_open_session_surface() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    OtherWorkspace = maps:get(other_workspace_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Contact = maps:get(contact_id, Scope),
    Conversation = maps:get(conversation_id, Scope),
    try
        {ok, Session} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000000
        }),
        SessionId = maps:get(id, Session),
        %% owner（active member，有治理角色，无 assignment、无 Workspace
        %% membership）拿不到 Session 面：治理/成员事实不授予坐席业务权限，
        %% 也不授予 CS 职能身份（C15 分层：membership active 必要不充分）。
        {error, {permission_missing, <<"conversation.read">>}} = seat_auth(Org, Owner),
        %% 有 sales 职能 assignment 的成员同样进不了 CS 面（职能不匹配
        %% required_function → identity_assignment_missing；职能不互相替代）
        {error, identity_assignment_missing} = seat_auth(Org, maps:get(actor_user_id, Scope)),
        %% seat actor 的 Session 作用域是**显式** (Org, Workspace)：换
        %% workspace 申报即 not_found（不从 default Workspace 重解析）。
        {error, not_found} = customer_service_facade:fetch_session(Org, #{
            workspace_id => OtherWorkspace, session_id => SessionId
        }),
        {ok, _} = customer_service_facade:fetch_session(Org, #{
            workspace_id => Workspace, session_id => SessionId
        })
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% CS-ORG-A06
%% ===================================================================

a06_default_workspace_change_keeps_persisted_session_scope() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Workspace = maps:get(workspace_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    Contact = maps:get(contact_id, Scope),
    Conversation = maps:get(conversation_id, Scope),
    try
        %% 同 Org 第二个 Workspace（作为新默认）
        Ws2 = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO workspace(id,name,owner_id,status,type,organization_id)"
                " VALUES ($1,$2,$3,'active','project',$4)"
            >>,
            [Ws2, ws_name(Ws2), Owner, Org]
        ),
        {ok, Session} = customer_service_facade:open_session(Org, #{
            workspace_id => Workspace,
            contact_id => Contact,
            conversation_id => Conversation,
            at => 1700000000000
        }),
        SessionId = maps:get(id, Session),
        %% default Workspace 显式改设（ORG-05 关系表，只读消费）
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_default_workspace(organization_id, workspace_id)"
                " VALUES ($1,$2) ON CONFLICT (organization_id) DO UPDATE"
                " SET workspace_id = EXCLUDED.workspace_id, updated_at = CURRENT_TIMESTAMP"
            >>,
            [Org, Ws2]
        ),
        %% 既有 Session 的持久化 workspace 不变（不随默认漂移/重解析）
        {ok, Fetched} = customer_service_facade:fetch_session(Org, #{
            workspace_id => Workspace, session_id => SessionId
        }),
        ?assertEqual(Workspace, maps:get(workspace_id, Fetched)),
        Row = session_row(Org, Workspace, SessionId),
        ?assertEqual(Workspace, maps:get(<<"workspace_id">>, Row)),
        %% 换默认后旧 Session 不能经新默认 Workspace 读取（显式 scope 不可挪移）
        {error, not_found} = customer_service_facade:fetch_session(Org, #{
            workspace_id => Ws2, session_id => SessionId
        }),
        %% 默认 Workspace 指向 Ws2
        ?assertEqual(
            Ws2,
            ?FIX:scalar(
                <<
                    "SELECT workspace_id FROM organization_default_workspace"
                    " WHERE organization_id=$1"
                >>,
                [Org]
            )
        ),
        ok = ?FIX:exec(<<"DELETE FROM organization_default_workspace WHERE organization_id=$1">>, [
            Org
        ])
    after
        _ = ?FIX:exec(<<"DELETE FROM organization_default_workspace WHERE organization_id=$1">>, [
            Org
        ]),
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% ORG-08 一次性验证 PG：仅在显式提供 ORG08_PGPORT 时覆盖 pg_conf（ORG-08
%% run 的复现路径）；缺省不动——由 EUNIT_CONFIG 注入本 run scratch 的
%% pg_conf。旧版硬编码 4393/imboy_v1，验证容器不存在时 setup 恒 no_pool
%% （把环境巧合当成了前提）。
ensure_test_pg_conf() ->
    _ = application:load(imboy),
    case os:getenv("ORG08_PGPORT") of
        false ->
            ok;
        PortStr ->
            Port = list_to_integer(PortStr),
            PgConf = #{
                name => pgsql,
                max_count => 40,
                init_count => 5,
                start_mfa =>
                    {epgsql, connect, [
                        #{
                            host => "127.0.0.1",
                            username => "imboy_user",
                            password => "abc54321",
                            database => "imboy_v1",
                            port => Port,
                            ssl => false,
                            timeout => 4000,
                            codecs => [{epgsql_codec_rfc3339_bin, []}]
                        }
                    ]}
            },
            application:set_env(imboy, pg_conf, PgConf)
    end.

%% cs_seat principal：active member + customer_service active assignment +
%% seat enabled（A04 坐席门），事实装配用真实 eb_pg_auth_facts（真 PG）。
seat_auth(OrgId, Uid) ->
    Metadata = #{
        auth_context => cs_seat,
        surface => tenant,
        required_function => <<"customer_service">>,
        required_permission => <<"conversation.read">>
    },
    cs_auth:authorize(Metadata, #{headers => #{}}, #{
        current_uid => Uid,
        organization_id => OrgId,
        auth_facts => eb_pg_auth_facts
    }).

%% enterprise_owner_admin principal：active member + owner/admin 治理角色。
owner_auth(OrgId, Uid) ->
    Metadata = #{
        auth_context => enterprise_owner_admin,
        surface => tenant,
        required_governance => [<<"owner">>, <<"admin">>]
    },
    cs_auth:authorize(Metadata, #{headers => #{}}, #{
        current_uid => Uid,
        organization_id => OrgId,
        auth_facts => eb_pg_auth_facts
    }).

session_row(Org, Workspace, SessionId) ->
    case
        elib_pg:query(
            <<
                "SELECT id, organization_id, workspace_id, contact_id, conversation_id,"
                " business_identity_id, status FROM customer_service_session"
                " WHERE organization_id=$1 AND workspace_id=$2 AND id=$3"
            >>,
            [Org, Workspace, SessionId]
        )
    of
        {ok, [Row | _]} -> Row;
        _ -> erlang:error({session_row_missing, Org, Workspace, SessionId})
    end.

member_status(Org, Uid) ->
    ?FIX:scalar(
        <<"SELECT status FROM organization_member WHERE organization_id=$1 AND user_id=$2">>,
        [Org, Uid]
    ).

%% epgsql #error{} 取值辅助（形状同 cs_pg_tests:error_code/1）。
pg_error_code({error, _Severity, Code, _Codename, _Message, _Extra}) -> Code;
pg_error_code(_Other) -> undefined.

pg_error_message({error, _Severity, _Code, _Codename, Message, _Extra}) -> Message;
pg_error_message(_Other) -> undefined.

ws_name(Id) ->
    <<"cs08-ws2-", (integer_to_binary(Id))/binary>>.

admin_account(Id) ->
    <<"cs08-admin-", (integer_to_binary(Id))/binary>>.

conv_name(Id) ->
    <<"cs08-consent-", (integer_to_binary(Id))/binary>>.
