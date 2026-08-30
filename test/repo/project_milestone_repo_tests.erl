-module(project_milestone_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% ZC-03 W2 Milestone — project_milestone_repo 真库测试（不可用自动 skip）
%%%
%%% 覆盖：add_tx 插入（due_date 以 {Y,M,D} tuple 传参——本连接自定义
%%% rfc3339 codec 仅覆盖 timestamptz，date 列传 binary 会崩）、
%%% find_by_id / find_tx 回读（due_date 读路径归一为 ISO YYYY-MM-DD
%%% binary——ZC-08 缺陷立项修复契约）、list_by_project 分页与 status 过滤、
%%% update_fields_tx 更新、find_project_member(_tx) 只读查询。

repo_roundtrip_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            DueDate = {2026, 9, 30},
            Now = elib_dt:now(),
            {ok, MsId} = project_milestone_repo:add_tx(Conn, #{
                <<"workspace_id">> => maps:get(ws_id, Fixture),
                <<"project_id">> => maps:get(project_id, Fixture),
                <<"name">> => <<"M1">>,
                <<"due_date">> => DueDate,
                <<"status">> => <<"planned">>,
                <<"created_at">> => Now,
                <<"updated_at">> => Now
            }),
            Row = project_milestone_repo:find_by_id(
                MsId, <<"id,project_id,name,due_date,status,reached_at">>
            ),
            ?assertEqual(<<"M1">>, maps:get(<<"name">>, Row)),
            %% date 列写入走 epgsql 原生 codec，读路径由 repo 归一为 ISO
            ?assertEqual(<<"2026-09-30">>, maps:get(<<"due_date">>, Row)),
            ?assertEqual(<<"planned">>, maps:get(<<"status">>, Row)),
            ?assertEqual(null, maps:get(<<"reached_at">>, Row))
        after
            cleanup_project(Conn, Fixture)
        end
    end).

repo_null_due_date_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            Now = elib_dt:now(),
            {ok, MsId} = project_milestone_repo:add_tx(Conn, #{
                <<"workspace_id">> => maps:get(ws_id, Fixture),
                <<"project_id">> => maps:get(project_id, Fixture),
                <<"name">> => <<"M-null">>,
                <<"due_date">> => null,
                <<"status">> => <<"planned">>,
                <<"created_at">> => Now,
                <<"updated_at">> => Now
            }),
            Row = project_milestone_repo:find_by_id(MsId, <<"name,due_date">>),
            ?assertEqual(null, maps:get(<<"due_date">>, Row))
        after
            cleanup_project(Conn, Fixture)
        end
    end).

repo_update_fields_tx_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            MsId = insert_milestone(Conn, Fixture, <<"M-old">>, {2026, 9, 30}),
            {ok, 1} = project_milestone_repo:update_fields_tx(Conn, MsId, #{
                <<"name">> => <<"M-new">>, <<"due_date">> => {2026, 10, 8}
            }),
            Row = project_milestone_repo:find_by_id(MsId, <<"name,due_date">>),
            ?assertEqual(<<"M-new">>, maps:get(<<"name">>, Row)),
            ?assertEqual(<<"2026-10-08">>, maps:get(<<"due_date">>, Row))
        after
            cleanup_project(Conn, Fixture)
        end
    end).

repo_list_by_project_pagination_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            Ids = [
                insert_milestone(Conn, Fixture, <<"L", (integer_to_binary(N))/binary>>, null)
             || N <- lists:seq(1, 3)
            ],
            {ok, Page1} = project_milestone_repo:list_by_project(
                maps:get(project_id, Fixture), all, 1, 2
            ),
            ?assertEqual(2, length(Page1)),
            {ok, Page2} = project_milestone_repo:list_by_project(
                maps:get(project_id, Fixture), all, 2, 2
            ),
            ?assertEqual(1, length(Page2)),
            %% 稳定排序 id ASC
            Got = [maps:get(<<"id">>, Row) || Row <- Page1 ++ Page2],
            ?assertEqual(lists:sort(Ids), Got),
            %% status 过滤
            {ok, Planned} = project_milestone_repo:list_by_project(
                maps:get(project_id, Fixture), <<"planned">>, 1, 10
            ),
            ?assertEqual(3, length(Planned)),
            {ok, Reached} = project_milestone_repo:list_by_project(
                maps:get(project_id, Fixture), <<"reached">>, 1, 10
            ),
            ?assertEqual([], Reached)
        after
            cleanup_project(Conn, Fixture)
        end
    end).

repo_count_by_project_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            Pid = maps:get(project_id, Fixture),
            insert_milestone(Conn, Fixture, <<"C1">>, null),
            Ms2 = insert_milestone(Conn, Fixture, <<"C2">>, null),
            %% M-7：admin_page 的 total 数据源——独立 COUNT 与数据页同 WHERE 语义
            {ok, 2} = project_milestone_repo:count_by_project(Pid, all),
            {ok, 2} = project_milestone_repo:count_by_project(Pid, <<"planned">>),
            {ok, 0} = project_milestone_repo:count_by_project(Pid, <<"reached">>),
            {ok, 1} = project_milestone_repo:update_fields_tx(Conn, Ms2, #{
                <<"status">> => <<"reached">>,
                <<"reached_at">> => elib_dt:now(),
                <<"updated_at">> => elib_dt:now()
            }),
            {ok, 1} = project_milestone_repo:count_by_project(Pid, <<"reached">>),
            {ok, 1} = project_milestone_repo:count_by_project(Pid, <<"planned">>)
        after
            cleanup_project(Conn, Fixture)
        end
    end).

repo_find_project_member_tx_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        Fixture = setup_project(Conn),
        try
            %% setup_project 已插入 owner 的 active project_member 行
            Row = project_milestone_repo:find_project_member_tx(
                Conn, maps:get(project_id, Fixture), maps:get(owner_uid, Fixture), <<"status">>
            ),
            ?assertEqual(<<"active">>, maps:get(<<"status">>, Row)),
            %% 不在册用户（共享开发库取不存在的 ID）→ 空 map
            OutsiderUid = maps:get(owner_uid, Fixture) + 999999,
            ?assertEqual(
                #{},
                project_milestone_repo:find_project_member_tx(
                    Conn, maps:get(project_id, Fixture), OutsiderUid, <<"status">>
                )
            )
        after
            cleanup_project(Conn, Fixture)
        end
    end).

%%% ===================================================================
%%% Internal（fixture 自清理：删 workspace 级联 project / project_member /
%%% project_milestone / project_event，见 00000076/78/81 FK CASCADE）
%%% ===================================================================

setup_project(Conn) ->
    {ok, _, [{OwnerUid}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 1">>
    ),
    WsId = elib_tsid:generate(),
    ProjectId = elib_tsid:generate(),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status, created_at)",
            " VALUES ($1, $2, $3, 'active', CURRENT_TIMESTAMP)">>,
        [WsId, <<"ZC03-milestone-repo">>, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at)",
            " VALUES ($1, $2, 'owner', 'active', CURRENT_TIMESTAMP)">>,
        [WsId, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, description, owner_id, status, created_at)",
            " VALUES ($1, $2, $3, '', $4, 'active', CURRENT_TIMESTAMP)">>,
        [ProjectId, WsId, <<"ZC03-milestone-repo-project">>, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_member (workspace_id, project_id, user_id, status)",
            " VALUES ($1, $2, $3, 'active')">>,
        [WsId, ProjectId, OwnerUid]
    ),
    #{ws_id => WsId, project_id => ProjectId, owner_uid => OwnerUid}.

insert_milestone(Conn, Fixture, Name, DueDate) ->
    Now = elib_dt:now(),
    {ok, MsId} = project_milestone_repo:add_tx(Conn, #{
        <<"workspace_id">> => maps:get(ws_id, Fixture),
        <<"project_id">> => maps:get(project_id, Fixture),
        <<"name">> => Name,
        <<"due_date">> => DueDate,
        <<"status">> => <<"planned">>,
        <<"created_at">> => Now,
        <<"updated_at">> => Now
    }),
    MsId.

cleanup_project(Conn, Fixture) ->
    %% 先删 project（级联 project_member/milestone/event），再删 workspace，
    %% 避开 trg_workspace_member_remove_guard_pm 的 fail-closed 兜底
    {ok, _} = epgsql:equery(
        Conn, <<"DELETE FROM project WHERE id = $1">>, [maps:get(project_id, Fixture)]
    ),
    {ok, _} = epgsql:equery(
        Conn, <<"DELETE FROM workspace WHERE id = $1">>, [maps:get(ws_id, Fixture)]
    ),
    ok.
