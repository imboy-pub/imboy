-module(project_member_repo_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-02 — project_member_repo 集成测试（真库，不可用自动 skip）
%%%
%%% 覆盖：
%%%   * upsert_active_tx：新增 changed / 重复 unchanged（只一行）/ 移除后再邀请恢复
%%%   * remove_tx：active→removed 返回 1 行；重复移除 0 行（幂等根）
%%%   * page_by_project：只列 active、JOIN user、分页 total
%%%   * unfinished_assignee_tasks_tx：新 Owner 未完成 task 冲突校验的数据源
%%%   * ensure_owner_member_tx：Owner 自动入项目（建 Project 同事务接线点，
%%%     ZC-05 由 project_ds:create 事务内调用）
%%% 写侧造数 autocommit，try/after 统一清理（共享库零残留）。

%%% ===================================================================
%%% upsert / remove 幂等（重复成员只一行）
%%% ===================================================================

upsert_and_remove_idempotent_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-upsert">>),
        try
            %% 首次插入 → changed
            ?assertMatch(
                {ok, changed, #{<<"status">> := <<"active">>}},
                project_member_repo:upsert_active_tx(
                    Conn, WsId, ProjId, OtherUid, _OwnerUid
                )
            ),
            %% 重复邀请 → unchanged，仍只一行
            ?assertMatch(
                {ok, unchanged, _},
                project_member_repo:upsert_active_tx(Conn, WsId, ProjId, OtherUid, _OwnerUid)
            ),
            ?assertEqual(1, count_member(Conn, ProjId, OtherUid)),
            %% 移除 → 1 行；重复移除 → 0 行（幂等根）
            ?assertEqual({ok, 1}, project_member_repo:remove_tx(Conn, ProjId, OtherUid)),
            ?assertEqual({ok, 0}, project_member_repo:remove_tx(Conn, ProjId, OtherUid)),
            ?assertEqual(
                <<"removed">>, member_status(Conn, ProjId, OtherUid), "软删保留历史行"
            ),
            %% 移除后再邀请 → changed 恢复 active（应用层覆盖激活语义），仍只一行
            ?assertMatch(
                {ok, changed, #{<<"status">> := <<"active">>}},
                project_member_repo:upsert_active_tx(Conn, WsId, ProjId, OtherUid, _OwnerUid)
            ),
            ?assertEqual(1, count_member(Conn, ProjId, OtherUid))
        after
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 非 active workspace_member 写入被 DB 延迟触发器拒绝（repo 层透传 23514）
%%% ===================================================================

upsert_rejects_non_active_ws_member_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-guard">>),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            _ = epgsql:squery(Conn, <<"SAVEPOINT sp_pm">>),
            %% 先把 OtherUid 的 workspace_member 置 removed（未提交）
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE workspace_member SET status = 'removed'",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            Res = project_member_repo:upsert_active_tx(Conn, WsId, ProjId, OtherUid, OwnerUid),
            case Res of
                {error, #error{code = <<"23514">>}} ->
                    %% epgsql 同连接报错后进入 aborted 态（即时约束版本），回滚清理
                    ok;
                {ok, changed, _} ->
                    %% 可延迟版本：INSERT 成功，COMMIT/IMMEDIATE 化时才报
                    Res2 = epgsql:squery(Conn, <<"SET CONSTRAINTS ALL IMMEDIATE">>),
                    case Res2 of
                        {error, #error{code = <<"23514">>}} -> ok;
                        Other -> ?assert(false, io_lib:format("expected 23514, got ~p", [Other]))
                    end;
                Other3 ->
                    ?assert(false, io_lib:format("unexpected repo result: ~p", [Other3]))
            end
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 成员分页列表（只列 active + JOIN user + total）
%%% ===================================================================

page_by_project_lists_active_only_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-page">>),
        try
            {ok, changed, _} =
                project_member_repo:upsert_active_tx(Conn, WsId, ProjId, OtherUid, OwnerUid),
            {ok, 1} = project_member_repo:remove_tx(Conn, ProjId, OtherUid),
            %% 只有 removed 行：active 列表为空、total=0
            {ok, #{list := [], total := 0}} =
                project_member_repo:page_by_project(
                    ProjId,
                    1,
                    10,
                    <<
                        "pm.workspace_id,pm.project_id,pm.user_id,pm.invited_by,pm.joined_at,"
                        "pm.status,u.nickname,u.avatar,u.account"
                    >>
                ),
            %% OtherUid 恢复 + Owner 补成员行：两个 active；removed 行不计入
            {ok, changed, _} =
                project_member_repo:upsert_active_tx(Conn, WsId, ProjId, OtherUid, OwnerUid),
            ok = project_member_ds:ensure_owner_member_tx(Conn, WsId, ProjId, OwnerUid),
            {ok, #{list := Rows, total := Total, total_page := TotalPage}} =
                project_member_repo:page_by_project(
                    ProjId,
                    1,
                    10,
                    <<
                        "pm.workspace_id,pm.project_id,pm.user_id,pm.invited_by,pm.joined_at,"
                        "pm.status,u.nickname,u.avatar,u.account"
                    >>
                ),
            ?assertEqual(2, Total, "owner + OtherUid 两个 active 成员"),
            ?assertEqual(1, TotalPage),
            ?assertEqual(2, length(Rows)),
            Uids = [maps:get(<<"user_id">>, Row) || Row <- Rows],
            ?assertEqual(true, lists:member(OwnerUid, Uids), "Owner 成员行在列"),
            ?assertEqual(true, lists:member(OtherUid, Uids)),
            %% 行含 user JOIN 列（nickname 键存在即可，值可能为 NULL）
            lists:foreach(fun(Row) -> ?assert(maps:is_key(<<"nickname">>, Row)) end, Rows),
            %% 第 2 页 size=1：稳定排序下翻页有数据
            {ok, #{list := Rows2, total_page := TP2}} =
                project_member_repo:page_by_project(
                    ProjId,
                    2,
                    1,
                    <<"pm.workspace_id,pm.project_id,pm.user_id,u.nickname">>
                ),
            ?assertEqual(1, length(Rows2)),
            ?assertEqual(2, TP2)
        after
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% 未完成 assignee task 查询（Owner 转移冲突校验数据源）
%%% ===================================================================

unfinished_assignee_tasks_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-tasks">>),
        TaskId = elib_tsid:generate(project_task),
        try
            {ok, []} = project_member_repo:unfinished_assignee_tasks_tx(Conn, ProjId, OtherUid),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"INSERT INTO project_task (id, project_id, title, creator_id, assignee_id, status)",
                    " VALUES ($1, $2, $3, $4, $5, 'todo')">>,
                [TaskId, ProjId, <<"transfer-conflict-task">>, _OwnerUid, OtherUid]
            ),
            {ok, [#{<<"id">> := TaskId}]} =
                project_member_repo:unfinished_assignee_tasks_tx(Conn, ProjId, OtherUid),
            %% done 任务不算未完成
            {ok, 1} = epgsql:equery(
                Conn, <<"UPDATE project_task SET status = 'done' WHERE id = $1">>, [TaskId]
            ),
            {ok, []} = project_member_repo:unfinished_assignee_tasks_tx(Conn, ProjId, OtherUid)
        after
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% Owner 自动入项目（建 Project 同事务接线点；ZC-05 消费）
%%% ===================================================================

ensure_owner_member_tx_creates_owner_row_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {WsId, ProjId, OwnerUid, _OtherUid} = setup_project(Conn, <<"W2PM-owner-join">>),
        try
            %% 模拟建 Project 事务内调用：owner 行不存在时补写 active 成员行
            ok = project_member_ds:ensure_owner_member_tx(Conn, WsId, ProjId, OwnerUid),
            ?assertEqual(<<"active">>, member_status(Conn, ProjId, OwnerUid)),
            ?assertEqual(1, count_member(Conn, ProjId, OwnerUid)),
            %% 重复调用幂等（回填/重放安全）
            ok = project_member_ds:ensure_owner_member_tx(Conn, WsId, ProjId, OwnerUid),
            ?assertEqual(1, count_member(Conn, ProjId, OwnerUid))
        after
            cleanup_workspace(Conn, WsId)
        end,
        ok
    end).

%%% ===================================================================
%%% Internal
%%% ===================================================================

count_member(Conn, ProjId, Uid) ->
    {ok, _, [{Count}]} = epgsql:equery(
        Conn,
        <<"SELECT count(*)::bigint FROM project_member WHERE project_id = $1 AND user_id = $2">>,
        [ProjId, Uid]
    ),
    Count.

member_status(Conn, ProjId, Uid) ->
    {ok, _, [{Status}]} = epgsql:equery(
        Conn,
        <<"SELECT status FROM project_member WHERE project_id = $1 AND user_id = $2">>,
        [ProjId, Uid]
    ),
    Status.

%% 复用两个既有 user 建 workspace + 双成员 + project（调用方负责清理）
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
    %% CASCADE 带走 workspace_member / project / project_member / project_task / event
    case catch epgsql:equery(Conn, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]) of
        {ok, _} ->
            ok;
        _Failed ->
            case pooler:take_member(pgsql) of
                error_no_members ->
                    ok;
                C2 ->
                    try epgsql:equery(C2, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]) of
                        _ -> ok
                    after
                        ok = pooler:return_member(pgsql, C2, ok)
                    end
            end
    end.
