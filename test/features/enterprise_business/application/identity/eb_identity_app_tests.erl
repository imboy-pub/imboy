%%% @doc EB-05 业务身份与经办关系的 application 用例套件（真库）。
%%%
%%% 覆盖作业书 §5 的 A01 / A03 / A04（identity 侧）：
%%%   * A01：sales / customer_service identity 可创建并绑定 active member；
%%%     同 Org / 同 user / 同 function_key 的第二个 active assignment 被拒，
%%%     且**被拒后行数/关键字段摘要不变**（负例落在同一 Org+user+function_key 上）。
%%%   * A03：不同 actor（不同 user）执行同一业务动作，Org owner 归属不变，
%%%     actor / 审计字段如实记录。
%%%   * A04：跨 Org 与 suspended / 非成员负例**零副作用**。
%%%
%%% 隔离：只用 `eb_pg_test_fixture:new_scope/0` 的随机 TSID 合成租户；不 TRUNCATE、
%%% 不写共享库、不删别人的行。合成命名前缀 `eb05-`。
%%%
%%% 环境不可用 ⇒ `erlang:error/1`（**不是** skip）：环境问题不得被当成 PASS。
-module(eb_identity_app_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, eb_pg_test_fixture).

identity_app_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    eunit_runner:eunit_setup_with_db().

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun a01_create_sales_and_customer_service_identities/0},
        {timeout, 60, fun a01_unknown_function_key_and_empty_name_rejected/0},
        {timeout, 60, fun a01_second_active_same_org_user_function_rejected/0},
        {timeout, 60, fun a01_first_bind_of_free_identity_inserts_new_active_row/0},
        {timeout, 60, fun a01_identity_already_bound_to_other_member_rejected/0},
        {timeout, 60, fun a01_end_then_bind_reopen_is_a_cas_write/0},
        {timeout, 60, fun a01_end_assignment_requires_assignee_match/0},
        {timeout, 60, fun a03_actor_changes_do_not_change_org_owner/0},
        {timeout, 60, fun a04_suspended_and_non_member_have_zero_side_effects/0},
        {timeout, 60, fun a04_cross_org_negatives_have_zero_side_effects/0},
        {timeout, 60, fun a04_owner_key_is_rejected_before_any_write/0},
        {timeout, 60, fun a04_real_member_fact_port_rejects_suspended_and_non_member/0},
        {timeout, 60, fun a05_list_identities_returns_only_own_org_rows/0},
        {timeout, 60, fun a05_list_identities_cross_org_is_empty_not_error/0},
        {timeout, 60, fun a05_list_identities_keyset_is_not_offset/0},
        {timeout, 60, fun a06_first_bind_inserts_then_duplicate_rejected/0},
        {timeout, 60, fun a10_member_fact_port_is_readonly_fact_not_authorization/0}
    ];
cases({error, Reason}) ->
    erlang:error({eb05_identity_suite_db_unavailable, Reason}).

%% ===================================================================
%% EB-05-A01
%% ===================================================================

a01_create_sales_and_customer_service_identities() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Before = identity_count(Org),
        {ok, Sales} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"sales">>,
            display_name => <<"eb05-sales-first">>,
            actor_user_id => Actor
        }),
        {ok, Service} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"customer_service">>,
            display_name => <<"eb05-service-first">>,
            actor_user_id => Actor
        }),
        %% 两个 V1 职能都可创建；owner 恒为 Organization
        ?assertEqual(Org, maps:get(organization_id, Sales)),
        ?assertEqual(Org, maps:get(organization_id, Service)),
        ?assertEqual(Ws, maps:get(workspace_id, Sales)),
        ?assertEqual(<<"sales">>, maps:get(function_key, Sales)),
        ?assertEqual(<<"customer_service">>, maps:get(function_key, Service)),
        ?assertEqual(active, maps:get(status, Sales)),
        ?assertEqual(1, maps:get(version, Sales)),
        %% actor 只是审计快照，不是 owner
        ?assertEqual(Actor, maps:get(created_by_user_id, Sales)),
        ?assert(is_integer(maps:get(id, Sales))),
        ?assertNotEqual(maps:get(id, Sales), maps:get(id, Service)),
        %% 真库核对：行数 +2，且两行 organization_id 都是该 Org
        ?assertEqual(Before + 2, identity_count(Org)),
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM organization_business_identity"
                    " WHERE organization_id=$1 AND created_by_user_id=$2"
                >>,
                [Org, Actor],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

a01_unknown_function_key_and_empty_name_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Before = identity_count(Org),
        %% 第三个职能（manager 等）V1 一律拒绝：function_key 不等于权限，不得用字符串扩权
        ?assertEqual(
            {error, {unknown_function_key, <<"manager">>}},
            eb_identity_app:create_identity(Org, #{
                workspace_id => Ws,
                function_key => <<"manager">>,
                display_name => <<"eb05-manager">>
            })
        ),
        ?assertEqual(
            {error, empty_display_name},
            eb_identity_app:create_identity(Org, #{
                workspace_id => Ws,
                function_key => <<"sales">>,
                display_name => <<"   ">>
            })
        ),
        %% 非整数 Org / 缺 workspace → fail-closed，不触库
        ?assertMatch(
            {error, {invalid_organization_id, _}},
            eb_identity_app:create_identity(undefined, #{
                workspace_id => Ws, function_key => <<"sales">>, display_name => <<"eb05-x">>
            })
        ),
        ?assertMatch(
            {error, {invalid_workspace_id, _}},
            eb_identity_app:create_identity(Org, #{
                function_key => <<"sales">>, display_name => <<"eb05-x">>
            })
        ),
        ?assertEqual(Before, identity_count(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% 负例落在**同一 Org、同一 user、同一 function_key**上（换 Org 或换 function 不算）。
a01_second_active_same_org_user_function_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        %% 夹具已给 Actor 建立一条 active 的 sales 经办关系
        ?assertEqual(1, active_assignments_org(Org)),
        {ok, SecondSales} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"sales">>,
            display_name => <<"eb05-sales-second">>,
            actor_user_id => Actor
        }),
        SecondId = maps:get(id, SecondSales),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        %% 第二个 active (Org, user, sales) 必须被拒
        ?assertEqual(
            {error, {duplicate_active_user_function, {Org, Actor, <<"sales">>}}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => SecondId,
                user_id => Actor,
                member_facts => fun(_, _) -> active end
            })
        ),
        %% 被拒后：行数未变，且关键字段摘要未变（零副作用）
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org)),
        ?assertEqual(0, active_rows_for_identity(Org, SecondId)),
        ?assertEqual(1, active_assignments_org(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% 同一 user 换 function_key **不受**基数规则拦截（证明上一条不是笼统拒绝）。
%% 并且这条路径是**首次绑定**：该 identity 从未有过经办行 ⇒ 必须走
%% `insert_assignment/3` **新建**一行（CAS 改既有行做不到这件事）。
a01_first_bind_of_free_identity_inserts_new_active_row() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        RowsBefore = all_assignments(Org),
        %% 前置事实：该 identity 一行都没有（不是「有 ended 行待 CAS」）
        ?assertEqual(0, rows_for_identity_total(Org, Service)),
        {ok, Bound} = bind(Org, #{
            workspace_id => Ws,
            identity_id => Service,
            user_id => Actor,
            member_facts => fun(_, _) -> active end
        }),
        ?assertEqual(active, maps:get(status, Bound)),
        %% mode=insert 证明走的是 INSERT 而不是 CAS reopen
        ?assertEqual(insert, maps:get(mode, Bound)),
        ?assertEqual(Actor, maps:get(user_id, Bound)),
        ?assertEqual(<<"customer_service">>, maps:get(function_key, Bound)),
        ?assert(is_integer(maps:get(assignment_id, Bound))),
        %% 真库：新建 1 行 active，总行数 +1（不是复用旧行）
        ?assertEqual(1, active_rows_for_identity(Org, Service)),
        ?assertEqual(RowsBefore + 1, all_assignments(Org)),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM organization_business_identity_assignment"
                    " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
                    "   AND user_id=$3 AND function_key='customer_service' AND version=1"
                    "   AND ended_at IS NULL"
                >>,
                [Org, Service, Actor],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

a01_identity_already_bound_to_other_member_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        %% 同一 identity 同时最多一个 active：换 user 也不行（handover/offboarding 才能改经办人）
        ?assertEqual(
            {error, {identity_bound_to_other_member, Actor}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => Sales,
                user_id => Owner,
                member_facts => fun(_, _) -> active end
            })
        ),
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% A01 的正向路径：end → bind(reopen) 是**真 CAS 写**（不是内存状态）。
a01_end_then_bind_reopen_is_a_cas_write() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        RowsBefore = all_assignments(Org),
        {ok, Ended} = eb_identity_app:end_assignment(Org, #{
            workspace_id => Ws,
            identity_id => Sales,
            user_id => Actor,
            end_reason => <<"eb05-rotate">>,
            actor_user_id => Owner
        }),
        ?assertEqual(ended, maps:get(status, Ended)),
        ?assertEqual(Actor, maps:get(user_id, Ended)),
        ?assert(is_integer(maps:get(audit_id, Ended))),
        %% 真库：status/ended_at 已落库（不是返回值自述）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM organization_business_identity_assignment"
                    " WHERE organization_id=$1 AND business_identity_id=$2"
                    "   AND status='ended' AND ended_at IS NOT NULL"
                >>,
                [Org, Sales],
                0
            )
        ),
        ?assertEqual(0, active_rows_for_identity(Org, Sales)),
        %% 重新绑定同一 active member → CAS {ended,reopen} → active
        {ok, Bound} = bind(Org, #{
            workspace_id => Ws,
            identity_id => Sales,
            user_id => Actor,
            actor_user_id => Owner,
            member_facts => fun(_, _) -> active end
        }),
        ?assertEqual(active, maps:get(status, Bound)),
        ?assertEqual(reopen, maps:get(mode, Bound)),
        ?assertEqual(1, active_rows_for_identity(Org, Sales)),
        %% 复用同一行：总行数不变（不是插了新行）
        ?assertEqual(RowsBefore, all_assignments(Org)),
        %% 两次 CAS 迁移（active→ended、ended→active）各 +1：夹具初始版本 1 ⇒ 3
        ?assertEqual(
            3,
            ?FIX:scalar(
                <<
                    "SELECT version FROM organization_business_identity_assignment"
                    " WHERE organization_id=$1 AND business_identity_id=$2"
                >>,
                [Org, Sales],
                0
            )
        ),
        %% 审计：actor 与 action 如实记录，owner 仍是 Org
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_type='organization_business_identity_assignment'"
                    "   AND resource_id=$2 AND actor_user_id=$3"
                    "   AND action='business_identity_assignment.end'"
                >>,
                [Org, maps:get(assignment_id, Ended), Owner],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

a01_end_assignment_requires_assignee_match() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        %% 传入的 user 不是当前经办人 → 拒绝且零副作用
        ?assertMatch(
            {error, {assignee_mismatch, {Sales, Owner, _Other}}},
            eb_identity_app:end_assignment(Org, #{
                workspace_id => Ws,
                identity_id => Sales,
                user_id => Owner,
                end_reason => <<"eb05-wrong-assignee">>
            })
        ),
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org)),
        %% 空 end_reason 也 fail-closed
        ?assertMatch(
            {error, {invalid_end_reason, _}},
            eb_identity_app:end_assignment(Org, #{
                workspace_id => Ws,
                identity_id => Sales,
                user_id => Actor,
                end_reason => <<>>
            })
        ),
        ?assertEqual(CountBefore, all_assignments(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A03：actor 变化不改变 Org owner
%% ===================================================================

a03_actor_changes_do_not_change_org_owner() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Owner = maps:get(owner_user_id, Scope),
        ?assertNotEqual(Actor, Owner),
        {ok, ByOwner} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"sales">>,
            display_name => <<"eb05-by-owner">>,
            actor_user_id => Owner
        }),
        {ok, ByActor} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"customer_service">>,
            display_name => <<"eb05-by-actor">>,
            actor_user_id => Actor
        }),
        %% owner 恒为 Organization（不因 actor 变化而改变）
        ?assertEqual(Org, maps:get(organization_id, ByOwner)),
        ?assertEqual(Org, maps:get(organization_id, ByActor)),
        %% actor 字段如实记录（两人各一条）
        ?assertEqual(Owner, maps:get(created_by_user_id, ByOwner)),
        ?assertEqual(Actor, maps:get(created_by_user_id, ByActor)),
        %% 审计同样：organization_id = Org，actor_user_id 如实
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND actor_user_id=$3"
                    "   AND action='business_identity.create'"
                >>,
                [Org, maps:get(id, ByOwner), Owner],
                0
            )
        ),
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_id=$2 AND actor_user_id=$3"
                    "   AND action='business_identity.create'"
                >>,
                [Org, maps:get(id, ByActor), Actor],
                0
            )
        ),
        %% 两行 organization_id 都是 Org
        ?assertEqual(
            2,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM organization_business_identity"
                    " WHERE organization_id=$1 AND id = ANY($2::bigint[])"
                >>,
                [Org, [maps:get(id, ByOwner), maps:get(id, ByActor)]],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A04：suspended / 非成员 / 跨 Org 负例零副作用
%% ===================================================================

a04_suspended_and_non_member_have_zero_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        %% suspended / removed / 非成员：一律拒绝，且零副作用
        lists:foreach(
            fun(Status) ->
                ?assertEqual(
                    {error, {member_not_active, Status}},
                    bind(Org, #{
                        workspace_id => Ws,
                        identity_id => Service,
                        user_id => Actor,
                        member_facts => fun(_, _) -> Status end
                    })
                )
            end,
            [suspended, removed, not_a_member]
        ),
        %% 成员事实先于租户数据判定：identity 不存在也先报 member_not_active
        ?assertEqual(
            {error, {member_not_active, suspended}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => 999999999999,
                user_id => Actor,
                member_facts => fun(_, _) -> suspended end
            })
        ),
        %% 未注入成员事实源时走**真实只读事实 Port**（EB-03R P10）：
        %% 不存在的实现模块 ⇒ fail-closed（不默认放行、不静默降级）
        ?assertMatch(
            {error, {member_fact_query_failed, _}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => Service,
                user_id => Actor,
                member_fact => eb05_no_such_member_fact_module
            })
        ),
        %% 事实源自身报错 → 原样透传
        ?assertEqual(
            {error, {db, down}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => Service,
                user_id => Actor,
                member_facts => fun(_, _) -> {error, {db, down}} end
            })
        ),
        %% 全程零副作用
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org)),
        ?assertEqual(0, active_rows_for_identity(Org, Service))
    after
        ?FIX:cleanup(Scope)
    end.

a04_cross_org_negatives_have_zero_side_effects() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        CountBefore = all_assignments(Org),
        IdentityBefore = identity_count(Org),
        DigestBefore = assignment_digest(Org),
        %% 跨 Org / 跨 Workspace 读同一 identity → not_found（SQL 同语句带 Org+WS）
        ?assertEqual(
            {error, {identity_not_found, Sales}},
            bind(OtherOrg, #{
                workspace_id => OtherWs,
                identity_id => Sales,
                user_id => Actor,
                member_facts => fun(_, _) -> active end
            })
        ),
        ?assertEqual(
            {error, {identity_not_found, Sales}},
            bind(Org, #{
                workspace_id => OtherWs,
                identity_id => Sales,
                user_id => Actor,
                member_facts => fun(_, _) -> active end
            })
        ),
        %% 跨 Org 结束关系 → 同样 not_found，真实关系仍 active
        ?assertEqual(
            {error, {identity_not_found, Sales}},
            eb_identity_app:end_assignment(OtherOrg, #{
                workspace_id => OtherWs,
                identity_id => Sales,
                user_id => Actor,
                end_reason => <<"eb05-cross-org">>
            })
        ),
        ?assertEqual(1, active_rows_for_identity(Org, Sales)),
        %% Workspace 不属于该 Org → 创建被 store 的租户自检拒绝，零写入
        ?assertEqual(
            {error, {workspace_not_in_org, OtherWs}},
            eb_identity_app:create_identity(Org, #{
                workspace_id => OtherWs,
                function_key => <<"sales">>,
                display_name => <<"eb05-cross-ws">>,
                actor_user_id => Actor
            })
        ),
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(IdentityBefore, identity_count(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org))
    after
        ?FIX:cleanup(Scope)
    end.

a04_owner_key_is_rejected_before_any_write() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Owner = maps:get(owner_user_id, Scope),
        Before = identity_count(Org),
        %% 把自然人 user 当 owner（显式 owner_user_id）→ domain 语义直接拒绝，零写入
        ?assertEqual(
            {error, user_cannot_be_owner},
            eb_identity_app:create_identity(Org, #{
                workspace_id => Ws,
                function_key => <<"sales">>,
                display_name => <<"eb05-user-owner">>,
                owner_user_id => Owner
            })
        ),
        ?assertEqual(Before, identity_count(Org)),
        %% fail-closed 形状：非整数 Org 一律不触库（列举亦然）
        ?assertMatch(
            {error, {invalid_organization_id, _}},
            eb_identity_app:list_identities(nope, #{workspace_id => Ws})
        ),
        ?assertMatch(
            {error, {invalid_workspace_id, _}},
            eb_identity_app:list_identities(Org, #{})
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A04（真事实源）：不注入 fun，走 EB-03R 的只读事实 Port
%% ===================================================================

%% A04 的 suspended / 非成员负例此前靠调用方注入 `member_facts => fun/2`。
%% E5-10 之后默认事实源是 `eb_member_fact_port` 的真实现（`eb_member_fact_pg`），
%% 因此这里**不注入任何 fun**，直接以真实 `organization_member` 行为判据：
%%   * suspended / removed 成员 → 拒；
%%   * 无成员关系（Port 返回 `{error, no_member}`）→ 归一为 not_a_member → 拒；
%%   * active 成员 → 放行并落到首次绑定 INSERT。
a04_real_member_fact_port_rejects_suspended_and_non_member() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Peer = maps:get(peer_user_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Outsider = ?FIX:id(),
        %% Peer 在夹具里没有成员行 → 真实落一行 suspended
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_member(organization_id,user_id,role,status)"
                " VALUES ($1,$2,'member','suspended')"
            >>,
            [Org, Peer]
        ),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        %% 真事实源：suspended → 拒
        ?assertEqual(
            {error, {member_not_active, suspended}},
            bind(Org, #{workspace_id => Ws, identity_id => Service, user_id => Peer})
        ),
        %% 真事实源：removed → 拒
        ok = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='removed'"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, Peer]
        ),
        ?assertEqual(
            {error, {member_not_active, removed}},
            bind(Org, #{workspace_id => Ws, identity_id => Service, user_id => Peer})
        ),
        %% 真事实源：无成员关系 ⇒ Port 报 no_member ⇒ not_a_member（不默认放行）
        ?assertEqual(
            {error, {member_not_active, not_a_member}},
            bind(Org, #{workspace_id => Ws, identity_id => Service, user_id => Outsider})
        ),
        %% 负例全部零副作用（行数 + 关键字段摘要逐字不变）
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org)),
        ?assertEqual(0, active_rows_for_identity(Org, Service)),
        %% 真事实源正向：夹具里的 Actor 是 active 成员 ⇒ 成员门放行
        {ok, Bound} = bind(Org, #{
            workspace_id => Ws, identity_id => Service, user_id => Actor
        }),
        ?assertEqual(active, maps:get(status, Bound)),
        ?assertEqual(insert, maps:get(mode, Bound)),
        ?assertEqual(1, active_rows_for_identity(Org, Service))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A05：身份列举（§5 GET /business-identities）
%% ===================================================================

%% A05 正例 + 反例：只返回**本 Org** 行才算通过。
%% 反例落在「他 Org 的 identity」上 —— 若按 Org 过滤失效，Foreign 会混进结果 ⇒ 必须红。
a05_list_identities_returns_only_own_org_rows() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        Service = maps:get(service_identity_id, Scope),
        Foreign = ?FIX:id(),
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version)"
                " VALUES ($1,$2,'sales',$3,'active',1)"
            >>,
            [Foreign, OtherOrg, <<"eb05-foreign-", (integer_to_binary(Foreign))/binary>>]
        ),
        {ok, Rows} = eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        Ids = [maps:get(id, Row) || Row <- Rows],
        ?assertEqual(lists:sort([Sales, Service]), lists:sort(Ids)),
        %% 负例：他 Org 的任一行出现即红
        ?assertNot(lists:member(Foreign, Ids)),
        lists:foreach(
            fun(Row) ->
                ?assertEqual(Org, maps:get(organization_id, Row)),
                ?assertEqual(Ws, maps:get(workspace_id, Row)),
                ?assertEqual(active, maps:get(status, Row))
            end,
            Rows
        )
    after
        ?FIX:cleanup(Scope)
    end.

a05_list_identities_cross_org_is_empty_not_error() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        OtherOrg = maps:get(other_org_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        %% 他 Org（配自己的 Workspace）→ 空列表，且是 ok 而非内部错误
        ?assertEqual(
            {ok, []},
            eb_identity_app:list_identities(OtherOrg, #{workspace_id => OtherWs})
        ),
        %% 本 Org 配他 Org 的 Workspace：租户键不成立 ⇒ 空（不是报错）
        ?assertEqual({ok, []}, eb_identity_app:list_identities(Org, #{workspace_id => OtherWs})),
        %% 他 Org 配本 Org 的 Workspace ⇒ 空
        ?assertEqual({ok, []}, eb_identity_app:list_identities(OtherOrg, #{workspace_id => Ws})),
        %% 明确不得再出现旧的能力缺口错误（E5-1 的判据：非 capability_missing）
        {ok, Rows} = eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        ?assert(length(Rows) >= 2)
    after
        ?FIX:cleanup(Scope)
    end.

%% A07 的 identity 侧孪生：**键集**分页（非 offset）。
%% 判据：分页之间插入一条 **id 小于游标** 的行 —— 键集结果不受影响；
%% 若实现成 `OFFSET 2`，该行会把窗口整体后移，`Page2` 首元素将 ≤ 游标 ⇒ 必红。
a05_list_identities_keyset_is_not_offset() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        lists:foreach(
            fun(N) ->
                {ok, _} = eb_identity_app:create_identity(Org, #{
                    workspace_id => Ws,
                    function_key => <<"customer_service">>,
                    display_name => <<"eb05-page-", (integer_to_binary(N))/binary>>
                })
            end,
            lists:seq(1, 3)
        ),
        {ok, All} = eb_identity_app:list_identities(Org, #{workspace_id => Ws}),
        AllIds = [maps:get(id, Row) || Row <- All],
        ?assertEqual(5, length(AllIds)),
        %% 契约：按键升序返回（键集分页的前提）
        ?assertEqual(lists:sort(AllIds), AllIds),
        [First, Second | _] = AllIds,
        {ok, Page1} = eb_identity_app:list_identities(Org, #{workspace_id => Ws, limit => 2}),
        ?assertEqual([First, Second], [maps:get(id, Row) || Row <- Page1]),
        %% 分页之间插入一条 id < 游标 的行
        SmallId = First - 1,
        ok = ?FIX:exec(
            <<
                "INSERT INTO organization_business_identity"
                " (id,organization_id,function_key,display_name,status,version)"
                " VALUES ($1,$2,'sales',$3,'active',1)"
            >>,
            [SmallId, Org, <<"eb05-small-", (integer_to_binary(SmallId))/binary>>]
        ),
        {ok, Page2} = eb_identity_app:list_identities(Org, #{
            workspace_id => Ws, after_id => Second
        }),
        Page2Ids = [maps:get(id, Row) || Row <- Page2],
        Expected = [Id || Id <- AllIds, Id > Second],
        ?assertEqual(Expected, Page2Ids),
        %% 键集语义：结果严格大于游标；offset 实现会在这里带回 ≤ 游标的行
        ?assert(lists:all(fun(Id) -> Id > Second end, Page2Ids)),
        ?assertNot(lists:member(SmallId, Page2Ids)),
        %% 游标之后无行 ⇒ 空（不得因 offset 语义返回尾部）
        {ok, Tail} = eb_identity_app:list_identities(Org, #{
            workspace_id => Ws, after_id => lists:last(AllIds)
        }),
        ?assertEqual([], [maps:get(id, Row) || Row <- Tail])
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A06：首次绑定正向（insert_assignment 那条无既有行的路径）
%% ===================================================================

%% A06 判据：**只走 CAS 改既有行不算通过** —— 首次绑定必须真跑过 INSERT。
%%   * 首次绑定成功：mode=insert、真库新增 1 行 active、版本从 1 起（CAS reopen 会是 3）；
%%   * 同 Org / user / function 再绑一次 ⇒ 被拒（不是静默覆盖），且零副作用。
a06_first_bind_inserts_then_duplicate_rejected() ->
    Scope = ?FIX:new_scope(),
    try
        Org = org(Scope),
        Ws = ws(Scope),
        Actor = maps:get(actor_user_id, Scope),
        Sales = maps:get(sales_identity_id, Scope),
        RowsBefore = all_assignments(Org),
        %% 该 identity 在夹具里从未被经办过（销售经办落在 sales 上，这里先结束它）
        _ = eb_identity_app:end_assignment(Org, #{
            workspace_id => Ws,
            identity_id => Sales,
            user_id => Actor,
            end_reason => <<"eb05-a06-free-the-identity">>
        }),
        %% 结束之后同一 identity 若再绑同一 user，会走 CAS reopen；为隔离出
        %% 「首次绑定」路径，这里改用**另一个 free identity**（customer_service）。
        Service = maps:get(service_identity_id, Scope),
        ?assertEqual(0, rows_for_identity_total(Org, Service)),
        {ok, First} = bind(Org, #{
            workspace_id => Ws,
            identity_id => Service,
            user_id => Actor,
            actor_user_id => maps:get(owner_user_id, Scope),
            member_facts => fun(_, _) -> active end
        }),
        ?assertEqual(insert, maps:get(mode, First)),
        ?assertEqual(1, maps:get(version, First)),
        ?assertEqual(RowsBefore + 1, all_assignments(Org)),
        %% 同 (Org, user, customer_service) 再绑一次 ⇒ 被基数规则拒绝
        {ok, Another} = eb_identity_app:create_identity(Org, #{
            workspace_id => Ws,
            function_key => <<"customer_service">>,
            display_name => <<"eb05-a06-second-service">>,
            actor_user_id => Actor
        }),
        AnotherId = maps:get(id, Another),
        CountBefore = all_assignments(Org),
        DigestBefore = assignment_digest(Org),
        ?assertEqual(
            {error, {duplicate_active_user_function, {Org, Actor, <<"customer_service">>}}},
            bind(Org, #{
                workspace_id => Ws,
                identity_id => AnotherId,
                user_id => Actor,
                member_facts => fun(_, _) -> active end
            })
        ),
        %% 拒绝零副作用
        ?assertEqual(CountBefore, all_assignments(Org)),
        ?assertEqual(DigestBefore, assignment_digest(Org)),
        ?assertEqual(0, rows_for_identity_total(Org, AnotherId)),
        %% 首次绑定的那一行仍然 active（未被覆盖 / 未被改状态）
        ?assertEqual(1, active_rows_for_identity(Org, Service)),
        %% 审计如实记录 mode=insert
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM enterprise_audit_event"
                    " WHERE organization_id=$1 AND resource_type='organization_business_identity_assignment'"
                    "   AND resource_id=$2 AND action='business_identity_assignment.bind'"
                >>,
                [Org, maps:get(assignment_id, First)],
                0
            )
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% EB-05-A10（E5-10）：成员事实只能是**只读事实**，且不得当授权结论
%% ===================================================================

a10_member_fact_port_is_readonly_fact_not_authorization() ->
    Port = read_source(
        <<"src/features/enterprise_business/application/eb_member_fact_port.erl">>
    ),
    Impl = read_source(
        <<"src/features/enterprise_business/infrastructure/eb_member_fact_pg.erl">>
    ),
    App = read_source(
        <<"src/features/enterprise_business/application/identity/eb_identity_app.erl">>
    ),
    %% 只读事实 Port 恰好两个 callback，且都不是写形状
    ?assertEqual(2, length(binary:matches(Port, <<"-callback ">>))),
    ?assertNotEqual(nomatch, binary:match(Port, <<"-callback member_status(">>)),
    ?assertNotEqual(nomatch, binary:match(Port, <<"-callback default_workspace(">>)),
    lists:foreach(
        fun(Token) -> ?assertEqual(nomatch, binary:match(Port, Token)) end,
        [
            <<"-callback insert">>,
            <<"-callback update">>,
            <<"-callback delete">>,
            <<"-callback advance">>,
            <<"-callback append">>,
            <<"-callback purge">>
        ]
    ),
    %% 实现侧零写 SQL
    LowerImpl = string:lowercase(Impl),
    lists:foreach(
        fun(Token) -> ?assertEqual(nomatch, binary:match(LowerImpl, Token)) end,
        [<<"insert into">>, <<"update organization_member">>, <<"delete from">>]
    ),
    ?assertNotEqual(nomatch, binary:match(LowerImpl, <<"select">>)),
    %% 应用层**真的消费**该只读事实（E5-10：接到 Port，而不是继续只靠注入）
    ?assertNotEqual(nomatch, binary:match(App, <<"MemberFact:member_status(">>)),
    %% 事实 ≠ 授权结论：成员事实不写回、不改状态、不产出授权字段
    ?assertEqual(nomatch, binary:match(App, <<"MemberFact:insert">>)),
    ?assertEqual(nomatch, binary:match(App, <<"MemberFact:update">>)),
    %% 不得自建 eb_member_app（用户裁决 §三）
    ?assertEqual(
        nomatch,
        binary:match(App, <<"eb_member_app">>)
    ).

%% ===================================================================
%% 辅助
%% ===================================================================

bind(Org, Params) ->
    eb_identity_app:bind_assignment(Org, Params).

org(Scope) -> maps:get(org_id, Scope).

ws(Scope) -> maps:get(workspace_id, Scope).

identity_count(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM organization_business_identity WHERE organization_id=$1">>,
        [Org],
        -1
    ).

all_assignments(Org) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM organization_business_identity_assignment WHERE organization_id=$1">>,
        [Org],
        -1
    ).

active_assignments_org(Org) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM organization_business_identity_assignment"
            " WHERE organization_id=$1 AND status='active'"
        >>,
        [Org],
        -1
    ).

active_rows_for_identity(Org, IdentityId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM organization_business_identity_assignment"
            " WHERE organization_id=$1 AND business_identity_id=$2 AND status='active'"
        >>,
        [Org, IdentityId],
        -1
    ).

%% 关键字段摘要：拒绝路径必须逐字不变（不是「恰好没数据」）。
assignment_digest(Org) ->
    Blob = ?FIX:scalar(
        <<
            "SELECT coalesce(string_agg("
            "  id::text || ':' || business_identity_id::text || ':' || coalesce(user_id::text,'-')"
            "  || ':' || function_key || ':' || status || ':' || version::text"
            "  || ':' || coalesce(ended_at::text,'-'), '|' ORDER BY id), '')"
            "  FROM organization_business_identity_assignment WHERE organization_id=$1"
        >>,
        [Org],
        <<>>
    ),
    crypto:hash(sha256, term_to_binary(Blob)).

%% 该 identity 的全部经办行（不分状态）——用于证明「首次绑定」路径没有既有行可 CAS。
rows_for_identity_total(Org, IdentityId) ->
    ?FIX:scalar(
        <<
            "SELECT count(*) FROM organization_business_identity_assignment"
            " WHERE organization_id=$1 AND business_identity_id=$2"
        >>,
        [Org, IdentityId],
        -1
    ).

%% 读被测源码（A10 的静态只读判据；找不到文件是**环境问题**，必须报错而不是放过）。
read_source(Rel) ->
    Candidates = [
        filename:join(code:lib_dir(imboy), binary_to_list(Rel)),
        binary_to_list(Rel)
    ],
    case [Path || Path <- Candidates, filelib:is_regular(Path)] of
        [Path | _] ->
            {ok, Bin} = file:read_file(Path),
            Bin;
        [] ->
            erlang:error({eb05_source_not_found, Rel, Candidates})
    end.
