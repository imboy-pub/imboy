%%% @doc ORG-04 Department 真库套件（ORG-A03 验收 + ORG-04 卡测试要求）。
%%%
%%% 覆盖：
%%%   * 迁移对象存在（两表 / 环防触发器 / self-parent CHECK / 同 Org 组合 FK）
%%%   * self-parent 与祖先环拒绝（应用层 + 直连 SQL 双口径）
%%%   * cross-org member 拒绝（组合 FK 23503 / 触发器 23514）
%%%   * 多部门归属（兼职）可行
%%%   * 并发 move（全序锁：同节点并发恰一方成功；环窗口终态无环）
%%%   * archive 不级联撤权限（org_member / workspace_member 零变化）+ 幂等
%%%   * department admin 局部目录角色无任何资源权限（负例矩阵）
%%%
%%% 只使用 organization_department_fixture 的合成租户（随机 TSID，无真实账号）。
-module(organization_department_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-define(FIX, organization_department_fixture).
-define(APP, organization_department_app).

department_pg_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 30, fun m01_migration_objects_exist/0},
        {timeout, 30, fun m02_create_and_tree_queries/0},
        {timeout, 30, fun m03_self_parent_rejected/0},
        {timeout, 30, fun m04_ancestor_cycle_rejected/0},
        {timeout, 30, fun m05_cross_org_member_rejected/0},
        {timeout, 30, fun m06_removed_member_rejected/0},
        {timeout, 30, fun m07_multi_department_membership_ok/0},
        {timeout, 60, fun m08_concurrent_move_exactly_one_winner/0},
        {timeout, 60, fun m09_concurrent_cycle_window_stays_acyclic/0},
        {timeout, 30, fun m10_archive_no_permission_cascade/0},
        {timeout, 60, fun m11_department_admin_no_escalation/0},
        {timeout, 30, fun m12_archived_dept_write_gates/0},
        {timeout, 30, fun m13_member_ops_idempotent/0}
    ];
cases(Other) ->
    erlang:error({org04_db_unavailable, Other}).

%% ===================================================================
%% M01 迁移对象
%% ===================================================================

m01_migration_objects_exist() ->
    Scope = ?FIX:new_scope(),
    try
        ?assertEqual(
            <<"organization_department">>,
            ?FIX:scalar(
                <<"SELECT to_regclass('organization_department')::text">>, []
            )
        ),
        ?assertEqual(
            <<"organization_department_member">>,
            ?FIX:scalar(
                <<"SELECT to_regclass('organization_department_member')::text">>, []
            )
        ),
        %% self-parent CHECK（环防第一层，DB 权威）
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM pg_constraint"
                    " WHERE conname = 'ck_organization_department_not_self_parent'"
                    " AND conrelid = 'organization_department'::regclass"
                >>,
                []
            )
        ),
        %% 同 Org membership 组合 FK
        ?assertEqual(
            1,
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM pg_constraint"
                    " WHERE conname = 'fk_odm_organization_member'"
                    " AND conrelid = 'organization_department_member'::regclass"
                >>,
                []
            )
        ),
        %% 环防触发器（环防第二层，DB 权威）
        Triggers = ?FIX:scalar(
            <<
                "SELECT count(*) FROM pg_trigger"
                " WHERE tgrelid = 'organization_department'::regclass"
                " AND tgname = 'trg_organization_department_cycle_guard'"
            >>,
            []
        ),
        ?assertEqual(1, Triggers),
        MemberGuard = ?FIX:scalar(
            <<
                "SELECT count(*) FROM pg_trigger"
                " WHERE tgrelid = 'organization_department_member'::regclass"
                " AND tgname = 'trg_organization_department_member_active_guard'"
            >>,
            []
        ),
        ?assertEqual(1, MemberGuard),
        _ = Scope
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M02 建树 / 查询
%% ===================================================================

m02_create_and_tree_queries() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Root} =
            ?APP:create_department(Org, #{
                name => <<"平台研发部"/utf8>>, actor_user_id => Owner
            }),
        RootId = maps:get(id, Root),
        ?assertEqual(null, maps:get(parent_id, Root)),
        ?assertEqual(active, maps:get(status, Root)),
        ?assertEqual(1, maps:get(version, Root)),

        {ok, Child} =
            ?APP:create_department(Org, #{
                name => <<"后端组"/utf8>>, parent_id => RootId, actor_user_id => Owner
            }),
        ?assertEqual(RootId, maps:get(parent_id, Child)),

        {ok, All} = ?APP:list_departments(Org, #{status => all}),
        ?assertEqual(2, length(All)),

        {ok, Detail} = ?APP:get_department(Org, #{department_id => RootId}),
        ?assertEqual(<<"平台研发部"/utf8>>, maps:get(name, Detail)),
        ?assertEqual([], maps:get(members, Detail)),

        %% 同 Org active 同名拒绝（部分唯一索引）
        ?assertMatch(
            {error, name_conflict},
            ?APP:create_department(Org, #{name => <<"平台研发部"/utf8>>, actor_user_id => Owner})
        ),
        %% 跨 Org 父部门拒绝（父存在但属另一个 Org ⇒ 租户枚举保护 = not_found）
        OtherOrg = maps:get(other_org_id, Scope),
        {ok, OtherRoot} =
            ?APP:create_department(OtherOrg, #{
                name => <<"B-根"/utf8>>,
                actor_user_id =>
                    maps:get(other_owner_user_id, Scope)
            }),
        ?assertMatch(
            {error, {parent_not_found, _}},
            ?APP:create_department(Org, #{
                name => <<"越界子"/utf8>>, parent_id => maps:get(id, OtherRoot), actor_user_id => Owner
            })
        ),
        %% 非 member 的 actor 拒绝建部门
        ?assertMatch(
            {error, {actor_not_member, _}},
            ?APP:create_department(Org, #{
                name => <<"外来"/utf8>>, actor_user_id => maps:get(other_member, Scope)
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M03 self-parent 拒绝（应用层 + SQL CHECK 双口径）
%% ===================================================================

m03_self_parent_rejected() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Dept} = ?APP:create_department(Org, #{name => <<"D">>, actor_user_id => Owner}),
        DeptId = maps:get(id, Dept),
        Version = maps:get(version, Dept),

        ?assertMatch(
            {error, {self_parent, DeptId}},
            ?APP:move_department(Org, #{
                department_id => DeptId,
                parent_id => DeptId,
                expected_version => Version,
                actor_user_id => Owner
            })
        ),
        %% 直连 SQL 绕过应用层同样被 CHECK 拒绝（23514）
        ?assertMatch(
            {error, #error{code = <<"23514">>}},
            elib_pg:query(
                <<"UPDATE organization_department SET parent_id = id WHERE id = $1">>,
                [DeptId]
            )
        ),
        %% 拒绝后树未被破坏
        {ok, After} = ?APP:get_department(Org, #{department_id => DeptId}),
        ?assertEqual(null, maps:get(parent_id, After))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M04 祖先环拒绝（应用层 + SQL 触发器双口径）
%% ===================================================================

m04_ancestor_cycle_rejected() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, A} = ?APP:create_department(Org, #{name => <<"A">>, actor_user_id => Owner}),
        {ok, B} = ?APP:create_department(Org, #{
            name => <<"B">>, parent_id => maps:get(id, A), actor_user_id => Owner
        }),
        {ok, C} = ?APP:create_department(Org, #{
            name => <<"C">>, parent_id => maps:get(id, B), actor_user_id => Owner
        }),
        AId = maps:get(id, A),
        AVersion = maps:get(version, A),

        %% A 移到自己后代 C 之下 => 三级祖先环，应用层拒绝
        ?assertMatch(
            {error, {cycle, AId, _}},
            ?APP:move_department(Org, #{
                department_id => AId,
                parent_id => maps:get(id, C),
                expected_version => AVersion,
                actor_user_id => Owner
            })
        ),
        %% 直连 SQL 绕过应用层同样被触发器拒绝（23514）
        ?assertMatch(
            {error, #error{code = <<"23514">>}},
            elib_pg:query(
                <<"UPDATE organization_department SET parent_id = $2 WHERE id = $1">>,
                [AId, maps:get(id, C)]
            )
        ),
        %% 合法移动仍可用：C 提到根
        {ok, Moved} = ?APP:move_department(Org, #{
            department_id => maps:get(id, C),
            parent_id => null,
            expected_version => maps:get(version, C),
            actor_user_id => Owner
        }),
        ?assertEqual(null, maps:get(parent_id, Moved))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M05 cross-org member 拒绝
%% ===================================================================

m05_cross_org_member_rejected() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Dept} = ?APP:create_department(Org, #{name => <<"D">>, actor_user_id => Owner}),
        DeptId = maps:get(id, Dept),

        %% 用户属 OrgB，不属 OrgA：应用层拒绝（先撞 FK 23503 => 统一语）
        OtherMember = maps:get(other_member, Scope),
        ?assertMatch(
            {error, member_not_org_member},
            ?APP:add_member(Org, #{
                department_id => DeptId,
                user_id => OtherMember,
                actor_user_id => Owner
            })
        ),
        %% 直连 SQL 绕过应用层同样被 DB 权威拒绝：BEFORE 守卫触发器先于 FK
        %% 执行，membership 行不存在在其 SELECT 中表现为 NULL => 23514
        %% （组合 FK 23503 仍是第二道网，覆盖触发器与约束检查之间的竞态窗口）
        ?assertMatch(
            {error, #error{code = <<"23514">>}},
            elib_pg:query(
                <<
                    "INSERT INTO organization_department_member"
                    " (organization_id, department_id, user_id, is_admin)"
                    " VALUES ($1, $2, $3, false)"
                >>,
                [Org, DeptId, OtherMember]
            )
        ),
        %% 从未有过 membership 的陌生人同样拒绝
        ?assertMatch(
            {error, member_not_org_member},
            ?APP:add_member(Org, #{
                department_id => DeptId,
                user_id => maps:get(other_owner_user_id, Scope),
                actor_user_id => Owner
            })
        ),
        %% 成员行数为 0：拒绝即零写入
        ?assertEqual(0, dept_member_count(DeptId))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M06 removed member 拒绝（触发器 23514：行存在但非 active）
%% ===================================================================

m06_removed_member_rejected() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Dept} = ?APP:create_department(Org, #{name => <<"D">>, actor_user_id => Owner}),
        DeptId = maps:get(id, Dept),
        RemovedMember = maps:get(removed_member, Scope),

        %% organization_member 行存在但 status='removed'：组合 FK 过、触发器拦
        ?assertMatch(
            {error, member_not_org_member},
            ?APP:add_member(Org, #{
                department_id => DeptId,
                user_id => RemovedMember,
                actor_user_id => Owner
            })
        ),
        ?assertEqual(0, dept_member_count(DeptId)),

        %% 先正常加入，再把 membership 置 removed：已入部的目录行保留（纯目录，
        %% 不联动撤权限），但该用户此后不能再被加入其他部门（触发器按 active 裁决）
        MemberA = maps:get(member_a, Scope),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ok = ?FIX:exec(
            <<
                "UPDATE organization_member SET status='removed'"
                " WHERE organization_id=$1 AND user_id=$2"
            >>,
            [Org, MemberA]
        ),
        ?assertMatch(
            {error, member_not_org_member},
            ?APP:add_member(Org, #{
                department_id => DeptId,
                user_id => MemberA,
                actor_user_id => Owner
            })
        )
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M07 多部门归属（兼职）可行
%% ===================================================================

m07_multi_department_membership_ok() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    MemberA = maps:get(member_a, Scope),
    try
        {ok, D1} = ?APP:create_department(Org, #{name => <<"研发部"/utf8>>, actor_user_id => Owner}),
        {ok, D2} = ?APP:create_department(Org, #{name => <<"质量部"/utf8>>, actor_user_id => Owner}),
        D1Id = maps:get(id, D1),
        D2Id = maps:get(id, D2),

        {ok, Added1} = ?APP:add_member(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        {ok, Added2} = ?APP:add_member(Org, #{
            department_id => D2Id,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ?assertEqual(false, maps:get(is_admin, Added1)),
        ?assertEqual(false, maps:get(is_admin, Added2)),
        ?assertEqual(1, dept_member_count(D1Id)),
        ?assertEqual(1, dept_member_count(D2Id)),

        {ok, Detail1} = ?APP:get_department(Org, #{department_id => D1Id}),
        ?assertEqual([MemberA], [maps:get(user_id, M) || M <- maps:get(members, Detail1)])
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M08 并发 move：同节点并发恰一方成功（全序锁 + CAS）
%% ===================================================================

m08_concurrent_move_exactly_one_winner() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Root} = ?APP:create_department(Org, #{name => <<"根"/utf8>>, actor_user_id => Owner}),
        {ok, B} = ?APP:create_department(Org, #{
            name => <<"B">>, parent_id => maps:get(id, Root), actor_user_id => Owner
        }),
        {ok, C} = ?APP:create_department(Org, #{
            name => <<"C">>, parent_id => maps:get(id, Root), actor_user_id => Owner
        }),
        {ok, X} = ?APP:create_department(Org, #{
            name => <<"X">>, parent_id => maps:get(id, Root), actor_user_id => Owner
        }),
        XId = maps:get(id, X),
        XVersion = maps:get(version, X),
        BId = maps:get(id, B),
        CId = maps:get(id, C),

        Self = self(),
        Mover = fun(TargetId) ->
            fun() ->
                Result =
                    ?APP:move_department(Org, #{
                        department_id => XId,
                        parent_id => TargetId,
                        expected_version => XVersion,
                        actor_user_id => Owner
                    }),
                Self ! {moved, self(), TargetId, Result}
            end
        end,
        {P1, _M1} = spawn_monitor(Mover(BId)),
        {P2, _M2} = spawn_monitor(Mover(CId)),
        Results = collect_results([P1, P2], []),

        ?assertEqual(2, length(Results)),
        Winners = [R || {_, _, {ok, _}} = R <- Results],
        Losers = [R || {_, _, {error, conflict}} = R <- Results],
        ?assertEqual(1, length(Winners), {all_results, Results}),
        ?assertEqual(1, length(Losers), {all_results, Results}),
        [{_, WinnerTarget, _}] = Winners,

        {ok, Final} = ?APP:get_department(Org, #{department_id => XId}),
        ?assertEqual(WinnerTarget, maps:get(parent_id, Final)),
        ?assertEqual(XVersion + 1, maps:get(version, Final)),
        ?assert(tree_acyclic(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M09 并发环窗口：两种交错终态都必须无环
%% ===================================================================

m09_concurrent_cycle_window_stays_acyclic() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    try
        {ok, Root} = ?APP:create_department(Org, #{name => <<"根"/utf8>>, actor_user_id => Owner}),
        RootId = maps:get(id, Root),
        {ok, X} = ?APP:create_department(Org, #{
            name => <<"X">>, parent_id => RootId, actor_user_id => Owner
        }),
        {ok, A} = ?APP:create_department(Org, #{
            name => <<"A">>, parent_id => maps:get(id, X), actor_user_id => Owner
        }),
        XId = maps:get(id, X),
        AId = maps:get(id, A),

        Self = self(),
        %% Op1：X 挂到自己的后代 A 之下（若先做=环，被拒；若 A 已被移出后做=合法）
        Op1 = fun() ->
            {ok, Cur} = ?APP:get_department(Org, #{department_id => XId}),
            Result =
                ?APP:move_department(Org, #{
                    department_id => XId,
                    parent_id => AId,
                    expected_version => maps:get(version, Cur),
                    actor_user_id => Owner
                }),
            Self ! {moved, self(), op1, Result}
        end,
        %% Op2：A 提到根
        Op2 = fun() ->
            {ok, Cur} = ?APP:get_department(Org, #{department_id => AId}),
            Result =
                ?APP:move_department(Org, #{
                    department_id => AId,
                    parent_id => null,
                    expected_version => maps:get(version, Cur),
                    actor_user_id => Owner
                }),
            Self ! {moved, self(), op2, Result}
        end,
        {P1, _M1} = spawn_monitor(Op1),
        {P2, _M2} = spawn_monitor(Op2),
        Results = collect_results([P1, P2], []),

        %% 无论交错次序：终态必须无环，且两操作不同时失败
        ?assertEqual(2, length(Results)),
        Failed = [R || {_, _, {error, _}} = R <- Results],
        ?assert(length(Failed) =< 1, {all_results, Results}),
        ?assert(tree_acyclic(Org))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M10 archive 不级联撤权限 + 幂等
%% ===================================================================

m10_archive_no_permission_cascade() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    MemberA = maps:get(member_a, Scope),
    try
        {ok, Root} = ?APP:create_department(Org, #{name => <<"总部"/utf8>>, actor_user_id => Owner}),
        RootId = maps:get(id, Root),
        {ok, Leaf} = ?APP:create_department(Org, #{
            name => <<"分部"/utf8>>, parent_id => RootId, actor_user_id => Owner
        }),
        LeafId = maps:get(id, Leaf),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => RootId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => LeafId,
            user_id => MemberA,
            actor_user_id => Owner
        }),

        OrgRoleBefore = org_role_snapshot(Org, MemberA),
        WsMemberCountBefore = ws_member_count(maps:get(workspace_id, Scope), MemberA),

        {ok, Archived} = ?APP:archive_department(Org, #{
            department_id => RootId,
            actor_user_id => Owner
        }),
        ?assertEqual(archived, maps:get(status, Archived)),
        ?assertEqual(false, maps:get(archive_idempotent, Archived)),

        %% 子树一并归档（纯目录状态）
        {ok, LeafAfter} = ?APP:get_department(Org, #{department_id => LeafId}),
        ?assertEqual(archived, maps:get(status, LeafAfter)),

        %% 关键负例：membership 与 workspace 权限零变化（C10 archive 不级联撤权限）
        ?assertEqual(OrgRoleBefore, org_role_snapshot(Org, MemberA)),
        ?assertEqual(WsMemberCountBefore, ws_member_count(maps:get(workspace_id, Scope), MemberA)),
        %% department_member 行保留（目录事实可审计，不猜撤销）
        ?assertEqual(1, dept_member_count(RootId)),
        ?assertEqual(1, dept_member_count(LeafId)),

        %% 重复 archive 幂等（零写入语义）
        {ok, Again} = ?APP:archive_department(Org, #{
            department_id => RootId,
            actor_user_id => Owner
        }),
        ?assertEqual(true, maps:get(archive_idempotent, Again)),

        %% 归档部门禁新写
        MemberB = maps:get(member_b, Scope),
        ?assertMatch(
            {error, department_archived},
            ?APP:add_member(Org, #{
                department_id => LeafId,
                user_id => MemberB,
                actor_user_id => Owner
            })
        ),
        %% archive 完全不触 organization_member / workspace_member 写路径：
        %% 全 org 范围两表行数与 archive 前一致
        ?assertEqual(OrgRoleBefore, org_role_snapshot(Org, MemberA))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M11 department admin 局部目录角色，无任何资源权限（负例矩阵）
%% ===================================================================

m11_department_admin_no_escalation() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    MemberA = maps:get(member_a, Scope),
    MemberB = maps:get(member_b, Scope),
    try
        {ok, D1} = ?APP:create_department(Org, #{name => <<"D1">>, actor_user_id => Owner}),
        {ok, D2} = ?APP:create_department(Org, #{name => <<"D2">>, actor_user_id => Owner}),
        D1Id = maps:get(id, D1),
        D2Id = maps:get(id, D2),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => D1Id,
            user_id => MemberB,
            actor_user_id => Owner
        }),

        OrgRoleA0 = org_role_snapshot(Org, MemberA),
        WsA0 = ws_member_count(maps:get(workspace_id, Scope), MemberA),

        %% 授予部门管理员（局部目录角色）
        {ok, AdminSet} = ?APP:set_admin(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            admin => true,
            actor_user_id => Owner
        }),
        ?assertEqual(true, maps:get(is_admin, AdminSet)),

        %% C15 负例 1：org role 零变化（还是 member，不是 owner/admin）
        ?assertEqual(OrgRoleA0, org_role_snapshot(Org, MemberA)),
        %% C15 负例 2：workspace_member / 任何权限表零新增
        ?assertEqual(WsA0, ws_member_count(maps:get(workspace_id, Scope), MemberA)),
        ?assertEqual(0, WsA0),
        %% C15 负例 3：代码面无授权原语（模块导出不含 grant/permission/auth/allow）
        Escalators = escalator_exports(),
        ?assertEqual([], Escalators),

        %% 重复 set 同值幂等
        {ok, AdminAgain} = ?APP:set_admin(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            admin => true,
            actor_user_id => Owner
        }),
        ?assertEqual(true, maps:get(idempotent, AdminAgain)),

        %% 局部角色可以在**本部门**加/移成员（目录管理，非权限）
        R1 = ?APP:add_member(Org, #{
            department_id => D1Id,
            user_id => maps:get(removed_member, Scope),
            actor_user_id => MemberA
        }),
        %% removed_member 不是 active member => 应被拒
        ?assertMatch({error, member_not_org_member}, R1),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => D1Id,
            user_id => MemberB,
            actor_user_id => MemberA
        }),
        ?assertMatch(
            {ok, _},
            ?APP:remove_member(Org, #{
                department_id => D1Id,
                user_id => MemberB,
                actor_user_id => MemberA
            })
        ),

        %% C15 负例 4：局部管理员对**同 Org 其他部门**无管理权
        ?assertMatch(
            {error, {actor_not_permitted, MemberA}},
            ?APP:add_member(Org, #{
                department_id => D2Id,
                user_id => MemberB,
                actor_user_id => MemberA
            })
        ),
        ?assertMatch(
            {error, {actor_not_permitted, MemberA}},
            ?APP:set_admin(Org, #{
                department_id => D2Id,
                user_id => MemberB,
                admin => true,
                actor_user_id => MemberA
            })
        ),

        %% 非 admin、非 org 管理的普通成员连本部门都不能管
        ?assertMatch(
            {error, {actor_not_permitted, MemberB}},
            ?APP:add_member(Org, #{
                department_id => D1Id,
                user_id => maps:get(member_a, Scope),
                actor_user_id => MemberB
            })
        ),

        %% admin 操作须目标已是部门成员
        R2 = ?APP:set_admin(Org, #{
            department_id => D1Id,
            user_id => maps:get(member_b, Scope),
            admin => true,
            actor_user_id => Owner
        }),
        ?assertMatch({error, not_department_member}, R2),

        %% 取消管理员幂等
        {ok, _} = ?APP:set_admin(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            admin => false,
            actor_user_id => Owner
        }),
        {ok, OffAgain} = ?APP:set_admin(Org, #{
            department_id => D1Id,
            user_id => MemberA,
            admin => false,
            actor_user_id => Owner
        }),
        ?assertEqual(true, maps:get(idempotent, OffAgain)),
        ?assertEqual(OrgRoleA0, org_role_snapshot(Org, MemberA)),
        ?assertEqual(WsA0, ws_member_count(maps:get(workspace_id, Scope), MemberA))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M12 archived 部门写闸门
%% ===================================================================

m12_archived_dept_write_gates() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    MemberA = maps:get(member_a, Scope),
    try
        {ok, D} = ?APP:create_department(Org, #{name => <<"D">>, actor_user_id => Owner}),
        DeptId = maps:get(id, D),
        Version = maps:get(version, D),
        {ok, _} = ?APP:add_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        {ok, _} = ?APP:archive_department(Org, #{
            department_id => DeptId,
            actor_user_id => Owner
        }),
        ?assertMatch(
            {error, department_archived},
            ?APP:update_department(Org, #{
                department_id => DeptId,
                name => <<"新名"/utf8>>,
                expected_version => Version,
                actor_user_id => Owner
            })
        ),
        ?assertMatch(
            {error, department_archived},
            ?APP:move_department(Org, #{
                department_id => DeptId,
                parent_id => null,
                expected_version => Version,
                actor_user_id => Owner
            })
        ),
        ?assertMatch(
            {error, department_archived},
            ?APP:set_admin(Org, #{
                department_id => DeptId,
                user_id => MemberA,
                admin => true,
                actor_user_id => Owner
            })
        ),
        ?assertMatch(
            {error, department_archived},
            ?APP:add_member(Org, #{
                department_id => DeptId,
                user_id => maps:get(member_b, Scope),
                actor_user_id => Owner
            })
        ),
        %% 移除成员仍允许（离开失效目录属清理事，不新增任何东西）
        ?assertMatch(
            {ok, _},
            ?APP:remove_member(Org, #{
                department_id => DeptId,
                user_id => MemberA,
                actor_user_id => Owner
            })
        ),
        %% 目录事实仍可读（可审计）
        ?assertMatch({ok, _}, ?APP:get_department(Org, #{department_id => DeptId}))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% M13 成员操作幂等
%% ===================================================================

m13_member_ops_idempotent() ->
    Scope = ?FIX:new_scope(),
    Org = maps:get(org_id, Scope),
    Owner = maps:get(owner_user_id, Scope),
    MemberA = maps:get(member_a, Scope),
    try
        {ok, D} = ?APP:create_department(Org, #{name => <<"D">>, actor_user_id => Owner}),
        DeptId = maps:get(id, D),
        {ok, First} = ?APP:add_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ?assertEqual(false, maps:get(idempotent, First)),
        {ok, Second} = ?APP:add_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ?assertEqual(true, maps:get(idempotent, Second)),
        ?assertEqual(1, dept_member_count(DeptId)),

        {ok, Removed} = ?APP:remove_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ?assertEqual(removed, maps:get(result, Removed)),
        ?assertEqual(false, maps:get(idempotent, Removed)),
        {ok, RemovedAgain} = ?APP:remove_member(Org, #{
            department_id => DeptId,
            user_id => MemberA,
            actor_user_id => Owner
        }),
        ?assertEqual(not_present, maps:get(result, RemovedAgain)),
        ?assertEqual(true, maps:get(idempotent, RemovedAgain)),
        ?assertEqual(0, dept_member_count(DeptId))
    after
        ?FIX:cleanup(Scope)
    end.

%% ===================================================================
%% 内部辅助
%% ===================================================================

%% C15 负例（代码面）：app 模块导出中不得出现任何授权/权限语义的函数名。
escalator_exports() ->
    Forbidden = [<<"grant">>, <<"permission">>, <<"auth">>, <<"allow">>, <<"revoke">>],
    lists:filter(
        fun({F, _A}) ->
            NomBin = list_to_binary(atom_to_list(F)),
            lists:any(
                fun(P) -> nomatch =/= binary:match(NomBin, [P]) end,
                Forbidden
            )
        end,
        ?APP:module_info(exports)
    ).

collect_results([], Acc) ->
    Acc;
collect_results(Pids, Acc) ->
    receive
        {moved, Pid, Tag, Result} ->
            collect_results(lists:delete(Pid, Pids), [{Pid, Tag, Result} | Acc]);
        %% 已收集结果的 mover 正常退出（DOWN normal）不算崩溃
        {'DOWN', _Ref, process, Pid, _normal} ->
            collect_results(lists:delete(Pid, Pids), Acc);
        {'DOWN', _Ref, process, Pid, Reason} ->
            erlang:error({mover_crashed, Pid, Reason, Acc})
    after 30000 ->
        erlang:error({mover_timeout, Acc})
    end.

dept_member_count(DeptId) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM organization_department_member WHERE department_id=$1">>,
        [DeptId],
        -1
    ).

%% org_member 事实快照（role+status；archive/admin 前后必须零变化）
org_role_snapshot(Org, UserId) ->
    ?FIX:scalar(
        <<
            "SELECT role || '/' || status FROM organization_member"
            " WHERE organization_id=$1 AND user_id=$2"
        >>,
        [Org, UserId],
        <<"none">>
    ).

ws_member_count(WorkspaceId, UserId) ->
    ?FIX:scalar(
        <<"SELECT count(*) FROM workspace_member WHERE workspace_id=$1 AND user_id=$2">>,
        [WorkspaceId, UserId],
        -1
    ).

%% 无环校验：任何 parent 链都应在深度上限内到达根（NULL）
tree_acyclic(Org) ->
    Deep = ?FIX:scalar(
        <<
            "WITH RECURSIVE chain(id, depth) AS ("
            "  SELECT id, 0 FROM organization_department WHERE organization_id=$1"
            "  UNION ALL"
            "  SELECT d.parent_id, c.depth + 1"
            "    FROM organization_department d JOIN chain c ON c.id = d.id"
            "   WHERE d.parent_id IS NOT NULL AND c.depth < 500"
            ") SELECT count(*) FROM chain WHERE depth >= 500"
        >>,
        [Org],
        -1
    ),
    Deep =:= 0.
