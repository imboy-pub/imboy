-module(workspace_archive_concurrency_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("error_code.hrl").

%%% 双体验 v2.5.2 WP4/T7 — 归档并发线性化 PG 集成测试（真库，不可用自动 skip）
%%%
%%% 计划 §七 T7 VALIDATE ③：写并发竞争测试——进程 A 长事务写群消息
%%% （此处以业务写事务的 workspace 行 FOR UPDATE 锁模拟）、进程 B 归档：
%%%   * A 先拿锁：A 完成后归档成功（无漏写）；
%%%   * B 先拿锁：A 的写被拒（稳定错误码 980）。
%%% 先拿锁者胜，无归档完成后的漏写。

%%% ===================================================================
%%% 场景 A：业务写先拿锁 → 完成后归档成功 → 后续写被拒
%%% ===================================================================

writer_first_archive_waits_test_() ->
    ?TEST_WITH_CONN(fun(ConnA) ->
        {WsId, OwnerUid} = setup_workspace(ConnA),
        try
            %% A：业务写事务开头锁 workspace 行（与 msg_c2g write_msg 的
            %% guard 首语句同款 SQL——先锁者胜的写侧）
            _ = epgsql:squery(ConnA, <<"BEGIN">>),
            {ok, _, _} = epgsql:equery(
                ConnA, <<"SELECT status FROM workspace WHERE id = $1 FOR UPDATE">>, [WsId]
            ),
            %% A 事务内模拟业务写（同事务副作用）
            {ok, 1} = epgsql:equery(
                ConnA,
                <<"UPDATE workspace SET updated_at = CURRENT_TIMESTAMP WHERE id = $1">>,
                [WsId]
            ),
            %% B：并发归档（归档事务 = UPDATE workspace SET status='archived'...）
            Self = self(),
            _B = spawn(fun() ->
                {ok, ConnB} = take_conn(),
                try
                    %% UPDATE 自带行锁：被 A 的 FOR UPDATE 阻塞直到 A COMMIT
                    {ok, Count} = epgsql:equery(
                        ConnB,
                        <<"UPDATE workspace SET status = 'archived', archived_at = $1,",
                            " archived_by = $2 WHERE id = $3 AND status = 'active'">>,
                        [<<"2026-08-26T00:00:00Z">>, OwnerUid, WsId]
                    ),
                    Self ! {archive_done, Count}
                after
                    ok = pooler:return_member(pgsql, ConnB, ok)
                end
            end),
            %% 确保 B 已启动并在锁上等待
            timer:sleep(150),
            %% A 完成业务写并提交（先拿锁者胜：A 的写成功）
            _ = epgsql:squery(ConnA, <<"COMMIT">>),
            receive
                {archive_done, 1} ->
                    %% A 完成后归档成功
                    ok;
                {archive_done, Other} ->
                    ?assert(false, io_lib:format("archive unexpected count ~p", [Other]))
            after 5000 ->
                ?assert(false, "archive blocked forever after writer commit")
            end,
            %% 归档完成后：后续业务写被拒（稳定错误码 980）
            ?assertMatch(
                {error, {980, _}},
                workspace_guard:ensure_writable_tx(ConnA, {workspace, WsId})
            )
        after
            cleanup_workspace(ConnA, WsId)
        end
    end).

%%% ===================================================================
%%% 场景 B：归档先拿锁并提交 → 业务写的 FOR UPDATE 阻塞后读到 archived → 拒绝
%%% ===================================================================

archive_first_writer_rejected_test_() ->
    ?TEST_WITH_CONN(fun(ConnA) ->
        {WsId, OwnerUid} = setup_workspace(ConnA),
        try
            %% B 先归档并提交（拿锁并完成）
            {ok, ConnB} = take_conn(),
            {ok, 1} = epgsql:equery(
                ConnB,
                <<"UPDATE workspace SET status = 'archived', archived_at = $1,",
                    " archived_by = $2 WHERE id = $3 AND status = 'active'">>,
                [<<"2026-08-26T00:00:00Z">>, OwnerUid, WsId]
            ),
            ok = pooler:return_member(pgsql, ConnB, ok),
            %% A 的业务写事务：guard 首语句 FOR UPDATE 阻塞后读到 archived → 拒绝
            ?assertMatch(
                {error, {980, _}},
                workspace_guard:ensure_writable_tx(ConnA, {workspace, WsId})
            ),
            %% restore 后放行（恢复后写操作恢复）
            {ok, 1} = epgsql:equery(
                ConnA,
                <<"UPDATE workspace SET status = 'active', archived_at = NULL,",
                    " archived_by = NULL WHERE id = $1 AND status = 'archived'">>,
                [WsId]
            ),
            ?assertEqual(
                ok, workspace_guard:ensure_writable_tx(ConnA, {workspace, WsId})
            )
        after
            cleanup_workspace(ConnA, WsId)
        end
    end).

%%% ===================================================================
%%% Internal
%%% ===================================================================

setup_workspace(Conn) ->
    {ok, _, [{OwnerUid}]} = epgsql:equery(
        Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 1">>
    ),
    WsId = elib_tsid:generate(workspace),
    {ok, 1} = epgsql:equery(
        Conn,
        <<"INSERT INTO workspace (id, name, owner_id, status, created_at)",
            " VALUES ($1, $2, $3, 'active', CURRENT_TIMESTAMP)">>,
        [WsId, <<"T7-concurrency-test">>, OwnerUid]
    ),
    {WsId, OwnerUid}.

cleanup_workspace(Conn, WsId) ->
    {ok, _} = epgsql:equery(Conn, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]),
    ok.

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
