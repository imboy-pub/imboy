-module(project_member_concurrency_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-02 TDD 用例 7 — 并发移除竞争（真库，不可用自动 skip）
%%%
%%% 三层成立的应用层+DB层并发证据（DB 约束层已由 w2_schema_contract_tests 覆盖）：
%%%   * 应用事务层：两个连接并发调用 project_member_ds:remove 移除同一成员——
%%%     行锁线性化，恰好一个 removed + 一个 already_removed，恰好一条
%%%     member_removed 事件（事件只在真实变更时写，重复移除幂等无事件）
%%%   * DB 行锁层：两连接同 SQL UPDATE 同一 active 行，受影响行数之和恒为 1
%%%   * 移除端触发器层：workspace_member 移除与 project_member 移除并发——
%%%     先清 project_member 者定胜负；project_member 仍 active 时
%%%     workspace_member 移除在 COMMIT 被延迟触发器拒绝（23514）
%%% 双连接模式参考 workspace_archive_concurrency_tests（take_conn + sleep 同步）。

%%% ===================================================================
%%% 场景 A：应用事务层——两进程并发 ds remove 同一成员（幂等 + 单事件）
%%% ===================================================================

concurrent_ds_remove_idempotent_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, Conn} = take_conn(),
        {WsId, ProjId, OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-cc-ds">>),
        {ok, 1} = epgsql:equery(
            Conn,
            <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
                " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
            [WsId, ProjId, OtherUid]
        ),
        try
            Self = self(),
            Worker =
                fun() ->
                    Result = project_member_ds:remove(OwnerUid, ProjId, OtherUid),
                    Self ! {remove_done, self(), Result}
                end,
            PidA = spawn(Worker),
            PidB = spawn(Worker),
            Results = receive_results([PidA, PidB], []),
            %% 恰好一个 removed + 一个 already_removed（顺序不限）
            Flags = [Flag || {ok, _, Flag} <- Results],
            ?assertEqual(
                [already_removed, removed],
                lists:sort(Flags),
                io_lib:format("unexpected results: ~p", [Results])
            ),
            %% 恰好一条 member_removed 事件（幂等移除不写事件）
            {ok, _, [{EventCnt}]} = epgsql:equery(
                Conn,
                <<"SELECT count(*)::bigint FROM project_event",
                    " WHERE project_id = $1 AND event_type = 'member_removed'">>,
                [ProjId]
            ),
            ?assertEqual(1, EventCnt, "并发移除只允许一条 member_removed 事件"),
            %% 终态：removed，只一行（两列行是单个二元组）
            {ok, _, [{Status, Cnt}]} = epgsql:equery(
                Conn,
                <<"SELECT status, count(*)::bigint FROM project_member",
                    " WHERE project_id = $1 AND user_id = $2 GROUP BY status">>,
                [ProjId, OtherUid]
            ),
            ?assertEqual(1, Cnt),
            ?assertEqual(<<"removed">>, Status)
        after
            cleanup_workspace(Conn, WsId),
            ok = pooler:return_member(pgsql, Conn, ok)
        end,
        ok
    end).

%%% ===================================================================
%%% 场景 B：DB 行锁层——两连接同 UPDATE 同一 active 行，受影响行数之和为 1
%%% ===================================================================

concurrent_raw_update_single_row_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, ConnA} = take_conn(),
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(ConnA, <<"W2PM-cc-lock">>),
        {ok, 1} = epgsql:equery(
            ConnA,
            <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
                " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
            [WsId, ProjId, OtherUid]
        ),
        {ok, ConnB} = take_conn(),
        try
            %% A 先拿行锁（未提交）
            _ = epgsql:squery(ConnA, <<"BEGIN">>),
            {ok, 1} = epgsql:equery(
                ConnA,
                <<"UPDATE project_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                    " WHERE project_id = $1 AND user_id = $2 AND status = 'active'">>,
                [ProjId, OtherUid]
            ),
            %% B 的同谓词 UPDATE 被 A 的行锁阻塞
            Self = self(),
            _B = spawn(fun() ->
                UpdateRes = epgsql:equery(
                    ConnB,
                    <<"UPDATE project_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                        " WHERE project_id = $1 AND user_id = $2 AND status = 'active'">>,
                    [ProjId, OtherUid]
                ),
                Self ! {b_done, UpdateRes}
            end),
            timer:sleep(150),
            %% A 提交（先拿锁者胜）
            _ = epgsql:squery(ConnA, <<"COMMIT">>),
            receive
                {b_done, UpdateRes} ->
                    %% READ COMMITTED 下 B 重评估谓词：行已被 A 置 removed → 0 行；
                    %% 两连接受影响行数之和恒为 1（1 + 0），无重复移除
                    ?assertEqual(
                        {ok, 0},
                        UpdateRes,
                        "行锁线性化：B 不得重复更新同一行"
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
%%% 场景 C：project_member 仍 active 时，workspace_member 移除在 COMMIT 被拒
%%% ===================================================================

ws_remove_rejected_while_pm_active_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, Conn} = take_conn(),
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(Conn, <<"W2PM-cc-guard">>),
        {ok, 1} = epgsql:equery(
            Conn,
            <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
                " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
            [WsId, ProjId, OtherUid]
        ),
        try
            _ = epgsql:squery(Conn, <<"BEGIN">>),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE workspace_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            %% 残留 active project_member → 延迟触发器在 COMMIT 拒绝（23514）
            case epgsql:squery(Conn, <<"COMMIT">>) of
                {error, #error{code = <<"23514">>}} ->
                    ok;
                Other1 ->
                    ?assert(false, io_lib:format("expected 23514 at commit, got ~p", [Other1]))
            end,
            _ = epgsql:squery(Conn, <<"ROLLBACK">>),
            %% 正确次序：先移除项目成员，再移除工作区成员 → 全部成功
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE project_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                    " WHERE project_id = $1 AND user_id = $2 AND status = 'active'">>,
                [ProjId, OtherUid]
            ),
            {ok, 1} = epgsql:equery(
                Conn,
                <<"UPDATE workspace_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            )
        after
            catch epgsql:squery(Conn, <<"ROLLBACK">>),
            cleanup_workspace(Conn, WsId),
            ok = pooler:return_member(pgsql, Conn, ok)
        end,
        ok
    end).

%%% ===================================================================
%%% 场景 D：双连接竞争——A 事务移除 project_member（未提交），B 移除
%%% workspace_member 并先 COMMIT：B 的移除端延迟触发器 EXISTS 读其事务
%%% 快照（读不到 A 未提交的 removed）→ 立即 23514，B 败出回滚；
%%% A 随后提交胜出。即"先清 project_member 者定胜负"的真实 DB 语义。
%%% ===================================================================

ws_remove_loses_race_to_concurrent_pm_remove_test_() ->
    ?TEST_WITH_DB_TIMEOUT(20, fun() ->
        {ok, ConnA} = take_conn(),
        {WsId, ProjId, _OwnerUid, OtherUid} = setup_project(ConnA, <<"W2PM-cc-race">>),
        {ok, 1} = epgsql:equery(
            ConnA,
            <<"INSERT INTO project_member (workspace_id, project_id, user_id, joined_at, status)",
                " VALUES ($1, $2, $3, CURRENT_TIMESTAMP, 'active')">>,
            [WsId, ProjId, OtherUid]
        ),
        {ok, ConnB} = take_conn(),
        try
            %% A：先清 project_member（持行锁未提交）
            _ = epgsql:squery(ConnA, <<"BEGIN">>),
            {ok, 1} = epgsql:equery(
                ConnA,
                <<"UPDATE project_member SET status = 'removed', updated_at = CURRENT_TIMESTAMP",
                    " WHERE project_id = $1 AND user_id = $2 AND status = 'active'">>,
                [ProjId, OtherUid]
            ),
            %% B：移除 workspace_member 并先 COMMIT——触发器快照里 pm 仍 active
            _ = epgsql:squery(ConnB, <<"BEGIN">>),
            {ok, 1} = epgsql:equery(
                ConnB,
                <<"UPDATE workspace_member SET status = 'removed',",
                    " updated_at = CURRENT_TIMESTAMP",
                    " WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            CommitRes = epgsql:squery(ConnB, <<"COMMIT">>),
            case CommitRes of
                {error, #error{code = <<"23514">>}} ->
                    ok;
                Other ->
                    ?assertEqual(<<"23514">>, Other, "B commit 应被移除端触发器拒绝")
            end,
            _ = epgsql:squery(ConnB, <<"ROLLBACK">>),
            %% A 随后提交胜出（清 pm 的移除合法）
            _ = epgsql:squery(ConnA, <<"COMMIT">>),
            %% 终态：pm removed；B 的 ws 移除已回滚（wm 仍 active）
            {ok, _, [{PmStatus}]} = epgsql:equery(
                ConnA,
                <<"SELECT status FROM project_member WHERE project_id = $1 AND user_id = $2">>,
                [ProjId, OtherUid]
            ),
            {ok, _, [{WmStatus}]} = epgsql:equery(
                ConnA,
                <<"SELECT status FROM workspace_member WHERE workspace_id = $1 AND user_id = $2">>,
                [WsId, OtherUid]
            ),
            ?assertEqual(<<"removed">>, PmStatus),
            ?assertEqual(<<"active">>, WmStatus, "输掉竞争的 ws 移除应整体回滚")
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
%%% Internal
%%% ===================================================================

receive_results([], Acc) ->
    lists:reverse(Acc);
receive_results(Pids, Acc) ->
    receive
        {remove_done, Pid, Result} ->
            receive_results(Pids -- [Pid], [Result | Acc])
    after 10000 ->
        ?assert(false, "concurrent remove timed out")
    end.

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
