-module(project_milestone_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% ZC-03 W2 Milestone — 真库集成测试（共享开发库 127.0.0.1:4323/imboy_v1，
%%% fixture 自清理：删 workspace 级联全部从属行）
%%%
%%% 覆盖计划 TDD 用例：
%%%   1 CRUD/字段白名单（name/due_date/status；未知字段无列可落）
%%%   2 planned→reached（reached_at 同步写入）
%%%   3 重复 reach 幂等（不产生重复事件行）
%%%   4 非法回退拒绝（reached→planned：无回退端点 + DB CHECK 强制）
%%%   5 Guest 403（写拒、读允）
%%%   6 非 Project Member 直达 403（写/读均拒）
%%%   7 事件同事务（写操作事件行齐全；事件写失败整体回滚无孤儿业务行）
%%%   归档 Workspace 拒写允读（稳定错误码 980）。
%%%
%%% due_date 经应用层解析为 {Y,M,D} tuple 入库（本连接自定义 rfc3339 codec
%%% 仅覆盖 timestamptz，date 列传 binary 会崩——ZC-01 已知坑）；读路径
%%% （find_by_id/list）由 repo 归一为 ISO YYYY-MM-DD binary（API/前端契约
%%% 见 imboyapp project_w2_model.dart due_date(YYYY-MM-DD|null)——ZC-08
%%% 缺陷立项修复：此前 tuple 直出被响应层格式化成 "{2026,9,30}" 串）。

%%% ===================================================================
%%% 1+2+3+7：全生命周期（create/update/reach/幂等 reach + 事件行齐全）
%%% ===================================================================

lifecycle_and_events_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Owner = maps:get(owner, F),
            Pid = maps:get(project_id, F),
            %% create
            {ok, Ms1} = project_milestone_logic:create(Owner, Pid, <<"M1">>, <<"2026-09-30">>),
            MsId1 = maps:get(<<"id">>, Ms1),
            ?assertEqual(<<"planned">>, maps:get(<<"status">>, Ms1)),
            ?assertEqual(<<"2026-09-30">>, maps:get(<<"due_date">>, Ms1)),
            ?assertEqual(null, maps:get(<<"reached_at">>, Ms1)),
            {ok, Ms2} = project_milestone_logic:create(Owner, Pid, <<"M2">>, null),
            MsId2 = maps:get(<<"id">>, Ms2),
            %% update（active project member 也可写）
            {ok, Updated} = project_milestone_logic:update(
                maps:get(member, F), MsId1, <<"M1-renamed">>, <<"2026-10-08">>
            ),
            ?assertEqual(<<"M1-renamed">>, maps:get(<<"name">>, Updated)),
            ?assertEqual(<<"2026-10-08">>, maps:get(<<"due_date">>, Updated)),
            ?assertEqual(<<"planned">>, maps:get(<<"status">>, Updated)),
            %% 事件行齐全：create + update + create（按 target 计）
            ?assertEqual(3, event_count(Conn, Pid)),
            %% reach：planned→reached，reached_at 同步写入
            {ok, Reached, reached} = project_milestone_logic:reach(Owner, MsId1),
            ?assertEqual(<<"reached">>, maps:get(<<"status">>, Reached)),
            ?assertNotEqual(null, maps:get(<<"reached_at">>, Reached)),
            %% 3：重复 reach 幂等——成功、不再写事件
            {ok, _, already_reached} = project_milestone_logic:reach(Owner, MsId1),
            ?assertEqual(4, event_count(Conn, Pid)),
            %% list：分页 + status 过滤
            {ok, All} = project_milestone_logic:list(Owner, Pid, all, 1, 10),
            ?assertEqual(2, length(maps:get(list, All))),
            {ok, Planned} = project_milestone_logic:list(Owner, Pid, <<"planned">>, 1, 10),
            ?assertEqual([MsId2], [maps:get(<<"id">>, R) || R <- maps:get(list, Planned)]),
            {ok, Page1} = project_milestone_logic:list(Owner, Pid, all, 1, 1),
            ?assertEqual(1, length(maps:get(list, Page1))),
            %% 事件类型白名单（chk_project_event_type 契约内的值）；
            %% created_at 同精度下顺序不定，按多重集合断言
            Types = event_types(Conn, Pid),
            ?assertEqual(
                lists:sort([
                    <<"milestone_created">>,
                    <<"milestone_updated">>,
                    <<"milestone_created">>,
                    <<"milestone_reached">>
                ]),
                lists:sort(Types)
            )
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 7：事件写失败 → 整体回滚（无孤儿业务行）
%%% ===================================================================

event_failure_rolls_back_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            %% 真库事务 + mock 事件写：事件写失败 → with_tx 整体回滚
            {ok, _} = meck_helper:setup_mock(project_event_repo, [
                {'insert_tx', 2, fun(_Conn0, _Data) ->
                    {error, {simulated, event_write_failed}}
                end}
            ]),
            Err =
                try
                    project_milestone_logic:create(
                        maps:get(owner, F), maps:get(project_id, F), <<"M-fail">>, null
                    )
                after
                    meck_helper:cleanup_mock(project_event_repo)
                end,
            %% 里程碑业务行未落库（同事务回滚，无孤儿）
            ?assertMatch({error, _}, Err),
            ?assertEqual(0, milestone_count(Conn, maps:get(project_id, F))),
            %% 事件也没有落库
            ?assertEqual(0, event_count(Conn, maps:get(project_id, F)))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 5：Guest 写 403、读允许
%%% ===================================================================

guest_write_forbidden_read_allowed_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Pid = maps:get(project_id, F),
            Guest = maps:get(guest, F),
            ?assertMatch(
                {error, {403, _}}, project_milestone_logic:create(Guest, Pid, <<"G">>, null)
            ),
            ?assertEqual(0, event_count(Conn, Pid)),
            {ok, Ms} = project_milestone_logic:create(maps:get(owner, F), Pid, <<"M1">>, null),
            MsId = maps:get(<<"id">>, Ms),
            ?assertMatch({error, {403, _}}, project_milestone_logic:reach(Guest, MsId)),
            ?assertMatch(
                {error, {403, _}}, project_milestone_logic:update(Guest, MsId, <<"X">>, undefined)
            ),
            %% guest 在册可读（只读）
            ?assertMatch({ok, _}, project_milestone_logic:list(Guest, Pid, all, 1, 10))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 6：非 Project Member 直达 milestone 接口 403（写/读均拒）
%%% ===================================================================

non_project_member_forbidden_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full_without_project_members(Conn),
        try
            Pid = maps:get(project_id, F),
            Member = maps:get(member, F),
            ?assertMatch(
                {error, {403, _}}, project_milestone_logic:create(Member, Pid, <<"N">>, null)
            ),
            ?assertMatch(
                {error, {403, _}}, project_milestone_logic:list(Member, Pid, all, 1, 10)
            ),
            ?assertEqual(0, event_count(Conn, Pid))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 归档 Workspace：拒写（980）允读
%%% ===================================================================

archived_workspace_rejects_write_allows_read_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Pid = maps:get(project_id, F),
            Owner = maps:get(owner, F),
            {ok, Ms} = project_milestone_logic:create(Owner, Pid, <<"M1">>, null),
            MsId = maps:get(<<"id">>, Ms),
            archive_workspace(Conn, maps:get(ws_id, F)),
            ?assertMatch(
                {error, {980, _}}, project_milestone_logic:create(Owner, Pid, <<"M2">>, null)
            ),
            ?assertMatch(
                {error, {980, _}}, project_milestone_logic:update(Owner, MsId, <<"X">>, undefined)
            ),
            ?assertMatch({error, {980, _}}, project_milestone_logic:reach(Owner, MsId)),
            %% 拒写不落行不落事件
            ?assertEqual(1, milestone_count(Conn, Pid)),
            ?assertEqual(1, event_count(Conn, Pid)),
            %% 归档下允许读
            ?assertMatch({ok, _}, project_milestone_logic:list(Owner, Pid, all, 1, 10)),
            restore_workspace(Conn, maps:get(ws_id, F))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 4：非法回退拒绝——无回退端点 + DB CHECK（reached→planned 必违约）
%%% ===================================================================

reached_revert_rejected_by_db_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Pid = maps:get(project_id, F),
            Owner = maps:get(owner, F),
            {ok, Ms} = project_milestone_logic:create(Owner, Pid, <<"M1">>, null),
            MsId = maps:get(<<"id">>, Ms),
            {ok, _, reached} = project_milestone_logic:reach(Owner, MsId),
            %% reached→planned 回退在 DB 层即被 chk_project_milestone_reached 拒绝
            {error, #error{code = <<"23514">>}} =
                epgsql:equery(
                    Conn,
                    <<"UPDATE project_milestone SET status = 'planned' WHERE id = $1">>,
                    [MsId]
                ),
            Row = project_milestone_repo:find_by_id(MsId, <<"status,reached_at">>),
            ?assertEqual(<<"reached">>, maps:get(<<"status">>, Row))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 1（续）：字段校验（due_date 格式、name 长度）
%%% ===================================================================

field_validation_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Pid = maps:get(project_id, F),
            Owner = maps:get(owner, F),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(Owner, Pid, <<"M">>, <<"2026-13-99">>)
            ),
            ?assertMatch(
                {error, {400, _}}, project_milestone_logic:create(Owner, Pid, <<"M">>, <<"bad">>)
            ),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(Owner, Pid, binary:copy(<<"长"/utf8>>, 201), null)
            ),
            ?assertEqual(0, milestone_count(Conn, Pid))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% 8：due_date 读路径 ISO 契约（find_by_id / list 均为 YYYY-MM-DD binary；
%%% 清空后保持 null）
%%% ===================================================================

due_date_iso_read_contract_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        F = setup_full(Conn),
        try
            Owner = maps:get(owner, F),
            Pid = maps:get(project_id, F),
            {ok, Ms} = project_milestone_logic:create(Owner, Pid, <<"ISO-M">>, <<"2026-09-30">>),
            ?assertEqual(<<"2026-09-30">>, maps:get(<<"due_date">>, Ms)),
            {ok, All} = project_milestone_logic:list(Owner, Pid, all, 1, 10),
            [Row] = maps:get(list, All),
            ?assertEqual(<<"2026-09-30">>, maps:get(<<"due_date">>, Row)),
            %% 清空 → null（读路径保持 null，不产出 ISO 空串/占位值）
            {ok, Cleared} =
                project_milestone_logic:update(
                    Owner, maps:get(<<"id">>, Ms), undefined, null
                ),
            ?assertEqual(null, maps:get(<<"due_date">>, Cleared))
        after
            cleanup_full(Conn, F)
        end
    end).

%%% ===================================================================
%%% Internal（fixture 自清理：删 workspace 级联 project / project_member /
%%% project_milestone / project_event / workspace_member）
%%% ===================================================================

setup_full(Conn) ->
    F0 = setup_base(Conn),
    %% setup_base 已插入 owner 行；此处补 Member + Guest
    %%（Project Member ⊆ active Workspace Member）
    lists:foreach(
        fun(Role) ->
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_member (workspace_id, project_id, user_id, status)",
                    " VALUES ($1, $2, $3, 'active')">>,
                [maps:get(ws_id, F0), maps:get(project_id, F0), maps:get(Role, F0)]
            )
        end,
        [member, guest]
    ),
    F0.

setup_full_without_project_members(Conn) ->
    setup_base(Conn).

setup_base(Conn) ->
    {ok, _, Rows} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 3">>
    ),
    [OwnerUid, MemberUid, GuestUid] = [Uid || {Uid} <- Rows],
    WsId = elib_tsid:generate(),
    ProjectId = elib_tsid:generate(),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status, created_at)",
            " VALUES ($1, $2, $3, 'active', CURRENT_TIMESTAMP)">>,
        [WsId, <<"ZC03-milestone-integration">>, OwnerUid]
    ),
    WsMembers =
        [{OwnerUid, <<"owner">>}, {MemberUid, <<"member">>}, {GuestUid, <<"guest">>}],
    lists:foreach(
        fun({Uid, Role}) ->
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at)",
                    " VALUES ($1, $2, $3, 'active', CURRENT_TIMESTAMP)">>,
                [WsId, Uid, Role]
            )
        end,
        WsMembers
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, description, owner_id, status, created_at)",
            " VALUES ($1, $2, $3, '', $4, 'active', CURRENT_TIMESTAMP)">>,
        [ProjectId, WsId, <<"ZC03-milestone-integration-project">>, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_member (workspace_id, project_id, user_id, status)",
            " VALUES ($1, $2, $3, 'active')">>,
        [WsId, ProjectId, OwnerUid]
    ),
    #{
        ws_id => WsId,
        project_id => ProjectId,
        owner => OwnerUid,
        member => MemberUid,
        guest => GuestUid
    }.

archive_workspace(Conn, WsId) ->
    {ok, 1} = epgsql:equery(
        Conn,
        <<"UPDATE workspace SET status = 'archived',",
            " archived_at = '2026-08-29T00:00:00Z' WHERE id = $1">>,
        [WsId]
    ),
    ok.

restore_workspace(Conn, WsId) ->
    {ok, 1} = epgsql:equery(
        Conn,
        <<"UPDATE workspace SET status = 'active', archived_at = NULL WHERE id = $1">>,
        [WsId]
    ),
    ok.

milestone_count(Conn, ProjectId) ->
    {ok, _, [{Cnt}]} = epgsql:equery(
        Conn,
        <<"SELECT count(*)::bigint FROM project_milestone WHERE project_id = $1">>,
        [ProjectId]
    ),
    Cnt.

event_count(Conn, ProjectId) ->
    {ok, _, [{Cnt}]} = epgsql:equery(
        Conn,
        <<"SELECT count(*)::bigint FROM project_event",
            " WHERE project_id = $1 AND event_type LIKE 'milestone%'">>,
        [ProjectId]
    ),
    Cnt.

event_types(Conn, ProjectId) ->
    {ok, _, Rows} = epgsql:equery(
        Conn,
        <<"SELECT event_type FROM project_event",
            " WHERE project_id = $1 AND event_type LIKE 'milestone%' ORDER BY created_at ASC, id ASC">>,
        [ProjectId]
    ),
    [T || {T} <- Rows].

cleanup_full(Conn, F) ->
    %% 先删 project（级联 project_member/milestone/event），再删 workspace
    %%（级联 workspace_member 时 trg_workspace_member_remove_guard_pm 才不会
    %% 因残留 active project_member 而 fail-closed 中断清理）
    {ok, _} = epgsql:equery(
        Conn, <<"DELETE FROM project WHERE id = $1">>, [maps:get(project_id, F)]
    ),
    {ok, _} = epgsql:equery(
        Conn, <<"DELETE FROM workspace WHERE id = $1">>, [maps:get(ws_id, F)]
    ),
    ok.
