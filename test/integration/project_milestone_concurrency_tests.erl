-module(project_milestone_concurrency_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-09R M-2 — 并发 reach 竞争（真库，不可用自动 skip）
%%%
%%% 修复前竞态（READ COMMITTED）：T1/T2 都读到 planned；T1 提交后 T2 的
%%% UPDATE（WHERE 仅 id）仍执行 → reached_at 覆盖 + milestone_reached 事件
%%% 两条。修复后 UPDATE 带 status='planned' 守卫（mark_reached_tx），
%%% 后提交者命中 0 行按 already_reached 幂等短路（不写事件）。
%%% 应用层+DB 层并发证据：
%%%   * 应用事务层：两连接并发 project_milestone_ds:reach 同一 planned 里程碑
%%%     ——恰好一个 reached + 一个 already_reached，恰好一条 milestone_reached
%%%     事件，最终 reached_at 单值；
%%%   * DB 行锁层：两连接同守卫谓词 UPDATE，受影响行数之和恒为 1。
%%% 双连接模式参考 project_member_concurrency_tests（take_conn + sleep 同步）。

%%% ===================================================================
%%% 场景 A：应用事务层——两进程并发 ds reach 同一里程碑（幂等 + 单事件）
%%% ===================================================================

concurrent_ds_reach_idempotent_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, Conn} = take_conn(),
        {WsId, ProjId, OwnerUid, MsId} = setup_milestone(Conn, <<"W2MS-cc-reach">>),
        try
            Self = self(),
            Worker =
                fun() ->
                    Result = project_milestone_ds:reach(OwnerUid, MsId),
                    Self ! {reach_done, self(), Result}
                end,
            PidA = spawn(Worker),
            PidB = spawn(Worker),
            Results = receive_results([PidA, PidB], []),
            %% 恰好一个 reached + 一个 already_reached（顺序不限）
            Flags = [Flag || {ok, _, Flag} <- Results],
            ?assertEqual(
                [already_reached, reached],
                lists:sort(Flags),
                io_lib:format("unexpected results: ~p", [Results])
            ),
            %% 恰好一条 milestone_reached 事件（幂等短路不写事件）
            {ok, _, [{EventCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'milestone_reached'">>,
                [ProjId]
            ),
            ?assertEqual(1, EventCnt, "并发 reach 只允许一条 milestone_reached 事件"),
            %% 终态：reached 单行、status/reached_at 一致且唯一
            {ok, _, [{Status, ReachedAt, Cnt}]} = epgsql:equery(
                Conn,
                <<"SELECT status, reached_at, count(*)::bigint FROM project_milestone",
                    " WHERE id = $1 GROUP BY status, reached_at">>,
                [MsId]
            ),
            ?assertEqual(1, Cnt, "reached_at 必须单值（不得被并发覆盖成多行形态）"),
            ?assertEqual(<<"reached">>, Status),
            ?assertNotEqual(null, ReachedAt)
        after
            cleanup_workspace(Conn, WsId),
            ok = pooler:return_member(pgsql, Conn, ok)
        end,
        ok
    end).

%%% ===================================================================
%%% 场景 B：DB 行锁层——两连接同守卫谓词 UPDATE，受影响行数之和为 1
%%% ===================================================================

concurrent_guarded_update_single_row_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, ConnA} = take_conn(),
        {WsId, _ProjId, _OwnerUid, MsId} = setup_milestone(ConnA, <<"W2MS-cc-lock">>),
        {ok, ConnB} = take_conn(),
        try
            %% A 先拿行锁（未提交；谓词命中 planned → 1 行）
            _ = epgsql:squery(ConnA, <<"BEGIN">>),
            {ok, 1} = epgsql:equery(
                ConnA,
                <<"UPDATE project_milestone SET status = 'reached',",
                    " reached_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP",
                    " WHERE id = $1 AND status = 'planned'">>,
                [MsId]
            ),
            %% B 的同谓词 UPDATE 被 A 的行锁阻塞
            Self = self(),
            _B = spawn(fun() ->
                UpdateRes = epgsql:equery(
                    ConnB,
                    <<"UPDATE project_milestone SET status = 'reached',",
                        " reached_at = CURRENT_TIMESTAMP, updated_at = CURRENT_TIMESTAMP",
                        " WHERE id = $1 AND status = 'planned'">>,
                    [MsId]
                ),
                Self ! {b_done, UpdateRes}
            end),
            timer:sleep(150),
            %% A 提交（先拿锁者胜）
            _ = epgsql:squery(ConnA, <<"COMMIT">>),
            receive
                {b_done, UpdateRes} ->
                    %% READ COMMITTED 下 B 重评估谓词：行已被 A 置 reached →
                    %% 守卫谓词不命中 → 0 行；两连接受影响行数之和恒为 1（1+0）
                    ?assertEqual(
                        {ok, 0},
                        UpdateRes,
                        "行锁线性化：守卫谓词下 B 不得重复更新同一行"
                    )
            after 5000 ->
                ?assert(false, "B blocked forever after A commit")
            end
        after
            catch epgsql:squery(ConnA, <<"ROLLBACK">>),
            catch epgsql:squery(ConnB, <<"ROLLBACK">>),
            ok = pooler:return_member(pgsql, ConnB, ok),
            cleanup_workspace(ConnA, WsId),
            ok = pooler:return_member(pgsql, ConnA, ok)
        end,
        ok
    end).

%%% ===================================================================
%%% Internal（fixture 自清理：删 workspace 级联 project / project_member /
%%% project_milestone / project_event / workspace_member）
%%% ===================================================================

receive_results([], Acc) ->
    lists:reverse(Acc);
receive_results(Pids, Acc) ->
    receive
        {reach_done, Pid, Result} ->
            receive_results(Pids -- [Pid], [Result | Acc])
    after 10000 ->
        ?assert(false, "concurrent reach timed out")
    end.

%% 复用既有 user 建 workspace + owner 双层成员 + project + planned 里程碑
setup_milestone(Conn, Tag) ->
    {ok, _, [{OwnerUid}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 1">>, []
    ),
    WsId = elib_tsid:generate(),
    ProjId = elib_tsid:generate(),
    MsId = elib_tsid:generate(),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status)", " VALUES ($1, $2, $3, 'active')">>,
        [WsId, Tag, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace_member (workspace_id, user_id, role, invited_by, joined_at, status)",
            " VALUES ($1, $2, 'owner', NULL, CURRENT_TIMESTAMP, 'active')">>,
        [WsId, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project (id, workspace_id, name, owner_id, status)",
            " VALUES ($1, $2, $3, $4, 'active')">>,
        [ProjId, WsId, Tag, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_member (workspace_id, project_id, user_id, status)",
            " VALUES ($1, $2, $3, 'active')">>,
        [WsId, ProjId, OwnerUid]
    ),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO project_milestone (id, workspace_id, project_id, name, status,",
            " created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, 'planned', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)">>,
        [MsId, WsId, ProjId, Tag]
    ),
    {WsId, ProjId, OwnerUid, MsId}.

cleanup_workspace(Conn, WsId) ->
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

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            %% 二次尝试（池瞬时耗尽）
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
