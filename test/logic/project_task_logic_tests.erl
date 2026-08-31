-module(project_task_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP4/T6b — project_task_logic 单元测试
%%% 覆盖：四态状态机（前向一步/回退任意/跳级与同态非法）、
%%% assignee 非 active workspace member 400（W0）、assignee 变更同校验、
%%% create 语义幂等、状态事件同事务（同一 Conn 写 task_status 事件）、
%%% 非法流转无孤儿事件、Guest 只读 403、archived 拒写 980。

-define(WS_ID, 800001).
-define(OWNER, 900001).
-define(MEMBER2, 900002).
-define(GUEST, 900003).
-define(OUTSIDER, 900004).
-define(PROJECT_ID, 700001).
-define(TASK_ID, 600001).

%%% ===================================================================
%%% 状态机纯函数
%%% ===================================================================

status_rank_test_() ->
    [
        {"forward one step legal", fun() ->
            ?assert(project_task_logic:legal_transition(<<"todo">>, <<"doing">>)),
            ?assert(project_task_logic:legal_transition(<<"doing">>, <<"review">>)),
            ?assert(project_task_logic:legal_transition(<<"review">>, <<"done">>))
        end},
        {"backward any legal", fun() ->
            ?assert(project_task_logic:legal_transition(<<"doing">>, <<"todo">>)),
            ?assert(project_task_logic:legal_transition(<<"review">>, <<"todo">>)),
            ?assert(project_task_logic:legal_transition(<<"review">>, <<"doing">>)),
            ?assert(project_task_logic:legal_transition(<<"done">>, <<"todo">>)),
            ?assert(project_task_logic:legal_transition(<<"done">>, <<"doing">>)),
            ?assert(project_task_logic:legal_transition(<<"done">>, <<"review">>))
        end},
        {"skip forward illegal", fun() ->
            ?assertNot(project_task_logic:legal_transition(<<"todo">>, <<"review">>)),
            ?assertNot(project_task_logic:legal_transition(<<"todo">>, <<"done">>)),
            ?assertNot(project_task_logic:legal_transition(<<"doing">>, <<"done">>))
        end},
        {"same status illegal", fun() ->
            ?assertNot(project_task_logic:legal_transition(<<"todo">>, <<"todo">>)),
            ?assertNot(project_task_logic:legal_transition(<<"done">>, <<"done">>))
        end},
        {"invalid status values illegal", fun() ->
            ?assertNot(project_task_logic:legal_transition(<<"todo">>, <<"blocked">>)),
            ?assertNot(project_task_logic:legal_transition(<<"archived">>, <<"todo">>))
        end},
        {"valid status domain", fun() ->
            [
                ?assert(project_task_logic:valid_status(S))
             || S <- [<<"todo">>, <<"doing">>, <<"review">>, <<"done">>]
            ],
            ?assertNot(project_task_logic:valid_status(<<"blocked">>))
        end}
    ].

%%% ===================================================================
%%% DS 事务行为（mock 事务 + repo）
%%% ===================================================================

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

ws_status() ->
    case get({task_tests, ws_status}) of
        S when is_binary(S) -> S;
        _ -> <<"active">>
    end.

task_row(Status) ->
    #{
        <<"id">> => ?TASK_ID,
        <<"project_id">> => ?PROJECT_ID,
        <<"title">> => <<"任务A"/utf8>>,
        <<"creator_id">> => ?OWNER,
        <<"assignee_id">> => ?MEMBER2,
        <<"status">> => Status,
        <<"sort">> => 0
    }.

%% 事务 mock：guard FOR UPDATE / workspace_member 状态 / 幂等查询按 SQL 分派
%% 全量跑时 sql_driver=pgsql，repo SQL 表名带 public. 前缀；匹配前归一
norm_sql(Sql) when is_binary(Sql) ->
    binary:replace(Sql, <<"public.">>, <<>>, [global]);
norm_sql(Sql) ->
    norm_sql(iolist_to_binary(Sql)).

%% 注意子句顺序：workspace_member 是 workspace 的前缀扩展，
%% 更具体的 member 前缀必须放在前面，否则会被 workspace 子句截胡
task_tx_query(Sql) when is_binary(Sql) ->
    task_tx_query_norm(norm_sql(Sql));
task_tx_query(Sql) ->
    task_tx_query_norm(iolist_to_binary(Sql)).

task_tx_query_norm(<<"SELECT status FROM workspace_member", _/binary>>) ->
    %% workspace_member_repo:find_tx（assignee 校验，单列 status）
    {ok, [#{<<"status">> => member_status()}]};
task_tx_query_norm(<<"SELECT role,status", _/binary>>) ->
    %% workspace_member_repo:find_tx（role,status 双列，upsert 路径）
    {ok, [#{<<"role">> => 1, <<"status">> => member_status()}]};
task_tx_query_norm(<<"SELECT status FROM workspace", _/binary>>) ->
    {ok, [#{<<"status">> => ws_status()}]};
task_tx_query_norm(_) ->
    {ok, []}.

member_status() ->
    case get({task_tests, member_status}) of
        S when is_binary(S) -> S;
        _ -> <<"active">>
    end.

task_mocks(CurrStatus) ->
    Self = self(),
    [
        {project_logic, [
            {'detail', 2, fun
                (?OWNER, ?PROJECT_ID) ->
                    {ok, #{<<"id">> => ?PROJECT_ID, <<"workspace_id">> => ?WS_ID}};
                %% Guest 只读 / 非成员：ensure_can_write 透传 403
                (?GUEST, ?PROJECT_ID) ->
                    {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                (?OUTSIDER, ?PROJECT_ID) ->
                    {error, {403, <<"非工作区成员"/utf8>>}}
            end}
        ]},
        {project_task_ds, [
            %% 只 mock 非事务辅助（find_by_id / list），事务主体走真实 DS
            {'find_by_id', 1, fun
                (?TASK_ID) -> task_row(CurrStatus);
                (_) -> #{}
            end}
        ]},
        {project_repo, [
            {'find_tx', 3, fun(_Conn, ?PROJECT_ID, _) ->
                #{<<"id">> => ?PROJECT_ID, <<"workspace_id">> => ?WS_ID}
            end},
            {'find_by_id', 2, fun(?PROJECT_ID, <<"workspace_id">>) ->
                #{<<"workspace_id">> => ?WS_ID}
            end}
        ]},
        {project_task_repo, [
            {'find_tx', 3, fun
                (_Conn, ?TASK_ID, _) -> task_row(CurrStatus);
                (_, _, _) -> #{}
            end},
            {'add_tx', 2, fun(_Conn, Data) ->
                Self ! {task_add, maps:get(<<"assignee_id">>, Data, nil)},
                {ok, ?TASK_ID}
            end},
            {'update_fields_tx', 3, fun(_Conn, ?TASK_ID, Data) ->
                Self !
                    {task_fields, maps:get(<<"status">>, Data, undefined),
                        maps:get(<<"assignee_id">>, Data, undefined)},
                {ok, 1}
            end},
            {'find_idempotent_tx', 4, fun(_Conn, _Pid, _Cid, _Title) ->
                case get({task_tests, idempotent_hit}) of
                    true -> #{<<"id">> => ?TASK_ID};
                    _ -> #{}
                end
            end},
            {'list_by_project', 4, fun(_, _, _, _) -> {ok, []} end}
        ]},
        {project_event_repo, [
            {'insert_tx', 2, fun(Conn, Data) ->
                Self ! {event_insert, Conn, Data},
                {ok, 500001}
            end}
        ]},
        {workspace_logic, [
            {'ensure_can_create_resource', 2, fun(?WS_ID, U) ->
                case U of
                    ?GUEST -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                    ?OUTSIDER -> {error, {403, <<"非工作区成员"/utf8>>}};
                    _ -> ok
                end
            end},
            {'ensure_member', 2, fun(?WS_ID, U) ->
                case U of
                    ?OUTSIDER -> {error, {403, <<"非工作区成员"/utf8>>}};
                    _ -> {ok, <<"member">>}
                end
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            {'one', 2, fun(Sql, _) ->
                case norm_sql(Sql) of
                    <<"SELECT id FROM workspace", _/binary>> ->
                        {ok, #{<<"id">> => ?WS_ID}};
                    %% task/repo find_by_id(列名版) 直查（passthrough 落到真库）
                    <<"SELECT id,project_id,title", _/binary>> ->
                        {ok, task_row(CurrStatus)};
                    Other ->
                        erlang:error({mock_clause_miss, Other})
                end
            end},
            {'query', 3, fun(_C, Sql, _P) -> task_tx_query(Sql) end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]}
    ].

%% 先前用例的 mock 哨兵消息会残留在同一进程信箱（simple fun 与 generator
%% 同进程），接收前必须排空，否则 receive 会误取旧消息
drain_mailbox() ->
    receive
        _ -> drain_mailbox()
    after 0 -> ok
    end.

%% ⚠️ eunit 不解释 {Desc, fun} 返回的 {setup,...} spec（探针实证），
%% ?WITH_MECKS 包在 {Desc, fun} 体内 = 静默空转。此 helper 立即执行等价语义：
%% setup → 执行断言 → cleanup，使断言真实生效（simple fun 与 generator 同进程，
%% Self 哨兵可用，无需改进程字典）。
run_with_mocks(MockConfigs, TestFun) ->
    lists:foreach(
        fun({Module, Expectations}) ->
            case meck_helper:setup_mock(Module, Expectations) of
                {ok, _} -> ok;
                {error, Reason} -> erlang:error({mock_setup_failed, Module, Reason})
            end
        end,
        MockConfigs
    ),
    try
        TestFun()
    after
        lists:foreach(
            fun({Module, _}) -> meck_helper:cleanup_mock(Module) end,
            MockConfigs
        )
    end.

%%% ===================================================================
%%% assignee 校验（W0：同 workspace active workspace_member）
%%% ===================================================================

assignee_validation_test_() ->
    [
        {"create with active member assignee ok", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {ok, _, created},
                    project_task_logic:create(?OWNER, ?PROJECT_ID, <<"任务A"/utf8>>, ?MEMBER2, 0)
                )
            end)
        end},
        {"create with non-member assignee rejected 400", fun() ->
            put({task_tests, member_status}, <<"removed">>),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:create(?OWNER, ?PROJECT_ID, <<"任务A"/utf8>>, ?MEMBER2, 0)
                ),
                %% 进程字典跨用例共享（simple fun 同进程），用后复原
                put({task_tests, member_status}, <<"active">>),
                ok
            end)
        end},
        {"create with unassigned (0) skips assignee check", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {ok, _, created},
                    project_task_logic:create(?OWNER, ?PROJECT_ID, <<"任务B"/utf8>>, 0, 0)
                )
            end)
        end},
        {"update assignee change revalidates 400", fun() ->
            put({task_tests, member_status}, <<"removed">>),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:update(?OWNER, ?TASK_ID, undefined, ?MEMBER2, undefined)
                ),
                put({task_tests, member_status}, <<"active">>),
                ok
            end)
        end},
        {"update assignee active ok", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {ok, _},
                    project_task_logic:update(?OWNER, ?TASK_ID, undefined, ?MEMBER2, undefined)
                )
            end)
        end}
    ].

%%% ===================================================================
%%% 幂等 / 权限 / 归档
%%% ===================================================================

create_idempotent_test_() ->
    {"duplicate create returns existing task", fun() ->
        put({task_tests, idempotent_hit}, true),
        run_with_mocks(task_mocks(<<"todo">>), fun() ->
            ?assertMatch(
                {ok, #{<<"id">> := ?TASK_ID}, existing},
                project_task_logic:create(?OWNER, ?PROJECT_ID, <<"任务A"/utf8>>, ?MEMBER2, 0)
            ),
            %% 幂等标记用后清除，防泄漏进后续用例
            erase({task_tests, idempotent_hit}),
            ok
        end)
    end}.

permission_and_archive_test_() ->
    [
        {"guest cannot create task (403)", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    project_task_logic:create(?GUEST, ?PROJECT_ID, <<"任务A"/utf8>>, 0, 0)
                )
            end)
        end},
        {"non member cannot create task (403)", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    project_task_logic:create(?OUTSIDER, ?PROJECT_ID, <<"任务A"/utf8>>, 0, 0)
                )
            end)
        end},
        {"guest can read task detail (read-only)", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch({ok, _}, project_task_logic:detail(?GUEST, ?TASK_ID))
            end)
        end},
        {"archived workspace rejects create with 980", fun() ->
            put({task_tests, ws_status}, <<"archived">>),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    project_task_logic:create(?OWNER, ?PROJECT_ID, <<"任务A"/utf8>>, 0, 0)
                ),
                %% 进程字典跨用例共享，用后复原
                put({task_tests, ws_status}, <<"active">>),
                ok
            end)
        end},
        {"empty title rejected 400", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:create(?OWNER, ?PROJECT_ID, <<>>, 0, 0)
                )
            end)
        end}
    ].

%%% ===================================================================
%%% 状态流转 + 事件原子性（同一 Conn）
%%% ===================================================================

transition_with_event_test_() ->
    [
        {"legal transition todo->doing writes event in same tx", fun() ->
            drain_mailbox(),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {ok, _}, project_task_logic:change_status(?OWNER, ?TASK_ID, <<"doing">>)
                ),
                receive
                    {event_insert, Conn, Data} ->
                        %% 事件与状态更新在同一事务连接（fake_conn）内
                        ?assertEqual(fake_conn, Conn),
                        ?assertEqual(?PROJECT_ID, maps:get(<<"project_id">>, Data)),
                        ?assertEqual(?TASK_ID, maps:get(<<"target_id">>, Data)),
                        ?assertEqual(<<"task_status">>, maps:get(<<"event_type">>, Data)),
                        ?assertEqual(?OWNER, maps:get(<<"actor_id">>, Data))
                    %% 注意：不能用万能 Other 兜底——update_fields_tx 的
                    %% task_fields 哨兵先到会被它误捕（receive 按序匹配，
                    %% 不跳过）；非匹配消息自动留在信箱，无需处理
                after 500 ->
                    ?assert(false, "task_status event not written")
                end
            end)
        end},
        {"rollback transition done->todo also writes event (backward legal)", fun() ->
            run_with_mocks(task_mocks(<<"done">>), fun() ->
                ?assertMatch(
                    {ok, _}, project_task_logic:change_status(?OWNER, ?TASK_ID, <<"todo">>)
                )
            end)
        end},
        {"illegal skip transition rejected 400 without event (no orphan)", fun() ->
            drain_mailbox(),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:change_status(?OWNER, ?TASK_ID, <<"done">>)
                ),
                receive
                    {event_insert, _, _} -> ?assert(false, "orphan event on illegal transition")
                after 0 -> ok
                end
            end)
        end},
        {"illegal same-status transition rejected 400", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:change_status(?OWNER, ?TASK_ID, <<"todo">>)
                )
            end)
        end},
        {"invalid target status rejected 400", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_task_logic:change_status(?OWNER, ?TASK_ID, <<"blocked">>)
                )
            end)
        end},
        {"archived workspace rejects transition with 980 and no event", fun() ->
            drain_mailbox(),
            put({task_tests, ws_status}, <<"archived">>),
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    project_task_logic:change_status(?OWNER, ?TASK_ID, <<"doing">>)
                ),
                put({task_tests, ws_status}, <<"active">>),
                receive
                    {event_insert, _, _} -> ?assert(false, "orphan event when archived")
                after 0 -> ok
                end
            end)
        end},
        {"guest cannot transition (403)", fun() ->
            run_with_mocks(task_mocks(<<"todo">>), fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    project_task_logic:change_status(?GUEST, ?TASK_ID, <<"doing">>)
                )
            end)
        end}
    ].
