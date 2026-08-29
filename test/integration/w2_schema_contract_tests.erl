-module(w2_schema_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% W2 Schema Contract 断言套件（PG 集成测试，真库不可用自动 skip）
%%%
%%% 契约来源：channel-firstclass W2 Scope Contract（execution ledger §4，2026-08-29 H1 放行），
%%% 迁移载体：00000081_project_w2_foundation。
%%%
%%% 断言范围（计划 ZC-01 验收逐条对应）：
%%%   * 三表 + project.links 列就位（project_member / project_milestone / project_channel_rel）
%%%   * 复合 FK 子集约束：project_member ⊆ project(同 workspace)，
%%%     project_member ⊆ workspace_member，project_channel_rel 的 channel 必须同 workspace
%%%   * 重复成员/关联只一行（PK 唯一约束）
%%%   * removed Workspace Member 被 DB 拒绝（写入端 + 移除端双向触发器）
%%%   * project.links 形状触发器（array of {name,url} 非空字符串）
%%%   * milestone 状态不变式 planned→reached（status/reached_at 同步 CHECK）
%%%   * project_event CHECK 扩展接受 W2 事件类型
%%%   * W0 存量回填不变式：owner 为 active workspace_member 的 project 必有 active owner 成员行
%%%
%%% 写侧用例：造数 autocommit，行为断言在显式事务内，try/after 统一
%%% ROLLBACK + 级联清理（共享库零残留）；可延迟约束触发器用
%%% SET CONSTRAINTS ALL IMMEDIATE 在事务内强制校验（违规错误在该语句上返回）。

-define(W2_EVENT_TYPES, [
    <<"member_invited">>,
    <<"member_removed">>,
    <<"member_owner_transferred">>,
    <<"milestone_created">>,
    <<"milestone_updated">>,
    <<"milestone_reached">>,
    <<"channel_linked">>,
    <<"channel_unlinked">>,
    <<"links_updated">>
]).

%%% ===================================================================
%%% 结构：三表 + project.links 列存在
%%% ===================================================================

w2_tables_and_links_column_present_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        lists:foreach(
            fun(Tb) ->
                {ok, _, [{Exists}]} = epgsql:equery(
                    Conn,
                    <<"SELECT to_regclass('public.' || $1)::text IS NOT NULL">>,
                    [Tb]
                ),
                ?assertEqual(
                    true,
                    Exists,
                    io_lib:format("W2 Scope Contract 违约：表 ~s 应存在（迁移 00000081）", [Tb])
                )
            end,
            [<<"project_member">>, <<"project_milestone">>, <<"project_channel_rel">>]
        ),
        {ok, _, [{LinksCnt}]} = epgsql:equery(
            Conn,
            <<"SELECT count(*)::bigint FROM information_schema.columns",
                " WHERE table_schema = 'public' AND table_name = 'project'",
                "   AND column_name = 'links'">>,
            []
        ),
        ?assertEqual(1, LinksCnt, "W2 违约：project.links 列应存在（Resources 聚合载体）"),
        ok
    end).

%%% ===================================================================
%%% 结构：复合 FK / CHECK / 触发器 就位
%%% ===================================================================

w2_constraints_and_triggers_present_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        %% 复合 FK：成员/里程碑/关联的 (workspace_id, project_id) 引用 project(id, workspace_id)
        assert_constraint(Conn, <<"project_member">>, <<"fk_project_member_project">>, <<"f">>),
        assert_constraint(
            Conn, <<"project_milestone">>, <<"fk_project_milestone_project">>, <<"f">>
        ),
        assert_constraint(
            Conn, <<"project_channel_rel">>, <<"fk_project_channel_rel_project">>, <<"f">>
        ),
        assert_constraint(
            Conn, <<"project_channel_rel">>, <<"fk_project_channel_rel_channel">>, <<"f">>
        ),
        %% 成员子集：project_member (workspace_id,user_id) → workspace_member(workspace_id,user_id)
        assert_constraint(Conn, <<"project_member">>, <<"fk_project_member_ws_member">>, <<"f">>),
        %% PK 即唯一约束（重复成员/关联只一行的 DB 层保证）
        assert_constraint(Conn, <<"project_member">>, <<"project_member_pkey">>, <<"p">>),
        assert_constraint(Conn, <<"project_milestone">>, <<"project_milestone_pkey">>, <<"p">>),
        assert_constraint(Conn, <<"project_channel_rel">>, <<"project_channel_rel_pkey">>, <<"p">>),
        %% channel 复合唯一（复合 FK 引用目标）
        assert_constraint(Conn, <<"channel">>, <<"uk_channel_id_workspace">>, <<"u">>),
        %% CHECK
        assert_constraint(Conn, <<"project_member">>, <<"chk_project_member_status">>, <<"c">>),
        assert_constraint(
            Conn, <<"project_milestone">>, <<"chk_project_milestone_status">>, <<"c">>
        ),
        assert_constraint(
            Conn, <<"project_milestone">>, <<"chk_project_milestone_reached">>, <<"c">>
        ),
        %% 触发器：写入端 active 校验 / 移除端兜底 / links 形状
        assert_trigger(Conn, <<"project_member">>, <<"trg_project_member_ws_active">>),
        assert_trigger(Conn, <<"workspace_member">>, <<"trg_workspace_member_remove_guard_pm">>),
        assert_trigger(Conn, <<"project">>, <<"trg_project_links_shape">>),
        ok
    end).

%%% ===================================================================
%%% 行为：project_member 子集（写入端）——removed workspace_member 被拒绝
%%% ===================================================================

member_rejects_removed_ws_member_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, OtherUid} = setup_project(Conn, <<"W2SC-rejected-member">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            %% 把 OtherUid 的 workspace_member 置 removed（延迟触发器暂不报）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE workspace_member SET status = 'removed'",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            %% 插入 active project_member（写入端延迟触发器提交时必须拒绝；
            %% INSERT 本身成功，违规由下方 IMMEDIATE 化强制检出）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_member",
                    " (workspace_id, project_id, user_id, invited_by, joined_at, status)",
                    " VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP, 'active')">>,
                [WsId, ProjId, OtherUid, OwnerUid]
            ),
            assert_deferred_violation(Conn, <<"23514">>)
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：project_member 子集（移除端）——残留 active 成员时移除 workspace_member 被拒绝
%%% ===================================================================

ws_member_remove_rejected_when_pm_active_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(Conn, <<"W2SC-remove-guard">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            %% OtherUid 先成为合法 active project_member
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_member",
                    " (workspace_id, project_id, user_id, joined_at, status)",
                    " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
                [WsId, ProjId, OtherUid]
            ),
            %% 直接移除其 workspace_member（未先清 project_member）→ 延迟校验必须拒绝
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE workspace_member SET status = 'removed'",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            assert_deferred_violation(Conn, <<"23514">>)
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：channel 关联跨 Workspace 被复合 FK 拒绝
%%% ===================================================================

channel_rel_rejects_cross_workspace_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, _OtherUid} = setup_project(Conn, <<"W2SC-cross-ws">>),
        Ws2Id = elib_tsid:generate(workspace),
        ChId = elib_tsid:generate(channel),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            %% 另一个 workspace 的频道（scope=workspace、自身合法存在）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO workspace (id, name, owner_id, status)",
                    " VALUES ($1, $2, $3, 'active')">>,
                [Ws2Id, <<"W2SC-cross-ws-2">>, OwnerUid]
            ),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)",
                    " VALUES ($1, $2, $3, 1, 'workspace', $4)">>,
                [ChId, <<"W2SC-cross-ws-ch">>, OwnerUid, Ws2Id]
            ),
            expect_pg_error(
                Conn,
                <<"INSERT INTO project_channel_rel",
                    " (workspace_id, project_id, channel_id, created_at)",
                    " VALUES ($1, $2, $3, CURRENT_TIMESTAMP)">>,
                [WsId, ProjId, ChId],
                <<"23503">>
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, Ws2Id),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：personal 频道无法关联（workspace_id 为 NULL 不满足复合 FK）
%%% ===================================================================

channel_rel_rejects_personal_channel_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, _OtherUid} = setup_project(Conn, <<"W2SC-personal-ch">>),
        ChId = elib_tsid:generate(channel),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO channel (id, name, creator_uid, status, scope)",
                    " VALUES ($1, $2, $3, 1, 'personal')">>,
                [ChId, <<"W2SC-personal-ch">>, OwnerUid]
            ),
            expect_pg_error(
                Conn,
                <<"INSERT INTO project_channel_rel",
                    " (workspace_id, project_id, channel_id, created_at)",
                    " VALUES ($1, $2, $3, CURRENT_TIMESTAMP)">>,
                [WsId, ProjId, ChId],
                <<"23503">>
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId),
            safe_delete(Conn, <<"DELETE FROM channel WHERE id = $1">>, [ChId])
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：重复成员 / 重复关联只允许一行（PK 唯一）
%%% ===================================================================

member_and_rel_unique_single_row_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(Conn, <<"W2SC-unique">>),
        ChId = elib_tsid:generate(channel),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            InsertMember =
                <<"INSERT INTO project_member",
                    " (workspace_id, project_id, user_id, joined_at, status)",
                    " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
            {ok, 1} = epgsql:equery(Conn, InsertMember, [WsId, ProjId, OtherUid]),
            expect_pg_error(Conn, InsertMember, [WsId, ProjId, OtherUid], <<"23505">>),

            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO channel (id, name, creator_uid, status, scope, workspace_id)",
                    " VALUES ($1, $2, $3, 1, 'workspace', $4)">>,
                [ChId, <<"W2SC-unique-ch">>, OtherUid, WsId]
            ),
            InsertRel =
                <<"INSERT INTO project_channel_rel",
                    " (workspace_id, project_id, channel_id, created_at)",
                    " VALUES ($1, $2, $3, CURRENT_TIMESTAMP)">>,
            {ok, 1} = epgsql:equery(Conn, InsertRel, [WsId, ProjId, ChId]),
            expect_pg_error(Conn, InsertRel, [WsId, ProjId, ChId], <<"23505">>)
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：project.links 形状触发器（array of {name,url} 非空字符串）
%%% ===================================================================

links_shape_trigger_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, _OtherUid} = setup_project(Conn, <<"W2SC-links">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            %% 合法：空数组、合法对象数组
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE project SET links = $1 WHERE id = $2">>,
                [<<"[]">>, ProjId]
            ),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE project SET links = $1 WHERE id = $2">>,
                [<<"[{\"name\":\"Docs\",\"url\":\"https://example.com\"}]">>, ProjId]
            ),
            %% 非法：非数组
            expect_pg_error(
                Conn,
                <<"UPDATE project SET links = $1 WHERE id = $2">>,
                [<<"\"not-an-array\"">>, ProjId],
                <<"23514">>
            ),
            %% 非法：元素缺 url
            expect_pg_error(
                Conn,
                <<"UPDATE project SET links = $1 WHERE id = $2">>,
                [<<"[{\"name\":\"Docs\"}]">>, ProjId],
                <<"23514">>
            ),
            %% 非法：空 name
            expect_pg_error(
                Conn,
                <<"UPDATE project SET links = $1 WHERE id = $2">>,
                [<<"[{\"name\":\"\",\"url\":\"https://example.com\"}]">>, ProjId],
                <<"23514">>
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：milestone 状态不变式（planned→reached；status/reached_at 同步）
%%% ===================================================================

milestone_status_invariant_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, _OtherUid} = setup_project(Conn, <<"W2SC-milestone">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            %% 无命名默认生成器（project_milestone 生成器由 ZC-03 应用层接线时再注册）
            MsId = elib_tsid:generate(),
            %% planned 无 reached_at：合法（due_date 用 SQL 字面量——本连接的
            %% rfc3339 codec 不接受 binary date 参数）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_milestone",
                    " (id, workspace_id, project_id, name, due_date, status)",
                    " VALUES ($1, $2, $3, $4, DATE '2026-09-30', 'planned')">>,
                [MsId, WsId, ProjId, <<"W2SC-M1">>]
            ),
            %% reached + reached_at：合法
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE project_milestone SET status = 'reached', reached_at = CURRENT_TIMESTAMP",
                    " WHERE id = $1">>,
                [MsId]
            ),
            %% 非法状态值
            expect_pg_error(
                Conn,
                <<"UPDATE project_milestone SET status = 'done' WHERE id = $1">>,
                [MsId],
                <<"23514">>
            ),
            %% reached 但缺 reached_at
            expect_pg_error(
                Conn,
                <<"UPDATE project_milestone SET reached_at = NULL WHERE id = $1">>,
                [MsId],
                <<"23514">>
            ),
            %% planned 但带 reached_at
            expect_pg_error(
                Conn,
                <<"UPDATE project_milestone SET status = 'planned' WHERE id = $1">>,
                [MsId],
                <<"23514">>
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 行为：project_event CHECK 接受 W2 事件类型；W0 值与未知值语义不变
%%% ===================================================================

event_type_w2_values_accepted_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, _OtherUid} = setup_project(Conn, <<"W2SC-event">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_w2">>),
            lists:foreach(
                fun(EvType) ->
                    EvId = elib_tsid:generate(project_event),
                    {ok, 1} = epgsql:equery(
                        Conn,
                        <<"INSERT INTO project_event (id, project_id, actor_id, event_type)",
                            " VALUES ($1, $2, $3, $4)">>,
                        [EvId, ProjId, OwnerUid, EvType]
                    )
                end,
                ?W2_EVENT_TYPES
            ),
            %% W0 既有值不受影响
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_event (id, project_id, actor_id, event_type)",
                    " VALUES ($1, $2, $3, 'project_created')">>,
                [elib_tsid:generate(project_event), ProjId, OwnerUid]
            ),
            %% 未知值仍被 CHECK 拒绝
            expect_pg_error(
                Conn,
                <<"INSERT INTO project_event (id, project_id, actor_id, event_type)",
                    " VALUES ($1, $2, $3, 'bogus_type')">>,
                [elib_tsid:generate(project_event), ProjId, OwnerUid],
                <<"23514">>
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 回填：W0 存量不变式——owner 为 active workspace_member 的 project
%%% 必有 active 的 owner project_member 行（Owner 自动入项目）
%%% ===================================================================

w0_owner_backfill_invariant_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {ok, _, [{Orphans}]} = epgsql:equery(
            Conn,
            <<"SELECT count(*)::bigint FROM project p",
                " WHERE EXISTS (SELECT 1 FROM workspace_member wm",
                "   WHERE wm.workspace_id = p.workspace_id",
                "     AND wm.user_id = p.owner_id AND wm.status = 'active')",
                "   AND NOT EXISTS (SELECT 1 FROM project_member pm",
                "   WHERE pm.project_id = p.id",
                "     AND pm.user_id = p.owner_id AND pm.status = 'active')">>,
            []
        ),
        ?assertEqual(
            0,
            Orphans,
            "W2 回填缺口：存在 active workspace_member 担任 owner 的 project 无 active owner 成员行"
        ),
        ok
    end).

%%% ===================================================================
%%% Internal
%%% ===================================================================

assert_constraint(Conn, Table, ConName, WantType) ->
    %% contype 为单字符常量（代码内字面量，非外部输入）内联进 SQL——
    %% 参数化绑定会被驱动推断为 bpchar 且 binary 编码失败
    {ok, _, [{Cnt}]} = epgsql:equery(
        Conn,
        iolist_to_binary([
            <<"SELECT count(*)::bigint FROM pg_constraint">>,
            <<" WHERE conrelid = to_regclass('public.' || $1)">>,
            <<"   AND conname = $2 AND contype = '">>,
            WantType,
            <<"'">>
        ]),
        [Table, ConName]
    ),
    ?assertEqual(
        1,
        Cnt,
        io_lib:format("W2 违约：~s 缺少约束 ~s（类型 ~s）", [Table, ConName, WantType])
    ),
    ok.

assert_trigger(Conn, Table, TrgName) ->
    {ok, _, [{Cnt}]} = epgsql:equery(
        Conn,
        <<"SELECT count(*)::bigint FROM pg_trigger",
            " WHERE tgrelid = to_regclass('public.' || $1)",
            "   AND tgname = $2 AND NOT tgisinternal">>,
        [Table, TrgName]
    ),
    ?assertEqual(
        1,
        Cnt,
        io_lib:format("W2 违约：~s 缺少触发器 ~s", [Table, TrgName])
    ),
    ok.

%% SET CONSTRAINTS ALL IMMEDIATE 强制校验本事务 pending 的延迟约束触发器；
%% 违规错误在该语句上返回（错误码必须等于 WantCode），通过则断言失败。
assert_deferred_violation(Conn, WantCode) ->
    Res = epgsql:squery(Conn, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
    case Res of
        {error, #error{code = Code}} ->
            ?assertEqual(WantCode, Code);
        _OkChecked ->
            ?assertEqual({deferred_violation_expected, WantCode}, Res)
    end.

%% 执行一条期望失败的语句并断言 PG 错误码。探测语句包在独立保存点
%% sp_probe 内：失败后 ROLLBACK TO 只撤销探测语句自身（失败语句会使
%% 事务进入 aborted 态，必须回滚保存点才能继续），事务内先前的成功
%% 写入不受影响。
expect_pg_error(Conn, Sql, Params, WantCode) ->
    {ok, _, _} = epgsql:squery(Conn, <<"SAVEPOINT sp_probe">>),
    Res = epgsql:equery(Conn, Sql, Params),
    {ok, _, _} = epgsql:squery(Conn, <<"ROLLBACK TO SAVEPOINT sp_probe">>),
    {ok, _, _} = epgsql:squery(Conn, <<"RELEASE SAVEPOINT sp_probe">>),
    case Res of
        {error, #error{code = Code}} ->
            ?assertEqual(WantCode, Code);
        Other ->
            ?assertEqual({pg_error_expected, WantCode}, Other)
    end.

%% 复用两个既有 user 建 workspace + 双成员 + project（全部由调用方事务/清理负责）
setup_project(Conn, Tag) ->
    {ok, _, [{OwnerUid}, {OtherUid}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 2">>, []
    ),
    WsId = elib_tsid:generate(workspace),
    ProjId = elib_tsid:generate(project),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status)", " VALUES ($1, $2, $3, 'active')">>,
        [WsId, Tag, OwnerUid]
    ),
    {ok, 2} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, joined_at, status)",
            " VALUES ($1, $2, 'owner', NULL, CURRENT_TIMESTAMP, 'active'),",
            "        ($1, $3, 'member', $2, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, OwnerUid, OtherUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, owner_id, status)",
            " VALUES ($1, $2, $3, $4, 'active')">>,
        [ProjId, WsId, Tag, OwnerUid]
    ),
    {WsId, ProjId, OwnerUid, OtherUid}.

cleanup_workspace(Conn, WsId) ->
    %% CASCADE 带走 workspace_member / project / project_member / milestone / rel；
    %% 当前连接已崩（如参数编码崩掉连接进程）时换池内新连接兜底，防残留
    safe_delete(Conn, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]).

safe_delete(Conn, Sql, Params) ->
    case catch epgsql:equery(Conn, Sql, Params) of
        {ok, _} ->
            ok;
        _Failed ->
            case pooler:take_member(pgsql) of
                error_no_members ->
                    ok;
                C2 ->
                    try epgsql:equery(C2, Sql, Params) of
                        _ -> ok
                    after
                        ok = pooler:return_member(pgsql, C2, ok)
                    end
            end
    end.
