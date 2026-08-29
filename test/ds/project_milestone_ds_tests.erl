-module(project_milestone_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% ZC-03 W2 Milestone — project_milestone_ds 事务行为单测（mock 事务 + repo）
%%%
%%% 覆盖：create/update/reach 的守卫链（404/980 归档/403 guest/403 非项目成员）、
%%% 状态机（planned→reached 写 reached_at；已 reached 幂等无事件）、
%%% 事件与业务写同 Conn（同事务可见性）、事件写失败整体 abort（无孤儿）、
%%% 无字段 update 为 no-op（不写行不写事件）。
%%% ZC-09R：reach 经 mark_reached_tx 带 status='planned' 并发守卫（M-2，
%%% {ok,0} 幂等短路；真库并发证据见 project_milestone_concurrency_tests）。
%%%
%%% 结构说明（ZC-09R H-1 改造）：?WITH_MECK_TESTS 是本文件本地宏
%%%（{setup, S, C, [用例]} 规范 context 结构）——"{Desc, fun() -> fixture end}"
%%% 包装式会让 EUnit 空转判 ok（内层断言从不执行），勿回退该形态。

-define(WS_ID, 810001).
-define(OWNER, 910001).
-define(MEMBER2, 910002).
-define(GUEST, 910003).
-define(OUTSIDER, 910004).
-define(PROJECT_ID, 710001).
-define(MS_ID, 610001).

%% 本地宏：一次 mock 安装 + 多个真实执行的内层用例（EUnit 规范 context）
-define(WITH_MECK_TESTS(MockConfigs, Tests),
    {setup,
        fun() ->
            lists:foreach(
                fun({Module, Expectations}) ->
                    case meck_helper:setup_mock(Module, Expectations) of
                        {ok, _} ->
                            ok;
                        {error, Reason} ->
                            ?debugFmt("Mock setup failed for ~p: ~p", [Module, Reason])
                    end
                end,
                MockConfigs
            )
        end,
        fun(_) ->
            lists:foreach(
                fun({Module, _Expectations}) -> meck_helper:cleanup_mock(Module) end,
                MockConfigs
            )
        end,
        Tests}
).

%%% ===================================================================
%%% mock 基建
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
    case get({ms_ds_tests, ws_status}) of
        S when is_binary(S) -> S;
        _ -> <<"active">>
    end.

ws_member_row(Uid) ->
    case Uid of
        ?OWNER -> #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>};
        ?MEMBER2 -> #{<<"role">> => <<"member">>, <<"status">> => <<"active">>};
        ?GUEST -> #{<<"role">> => <<"guest">>, <<"status">> => <<"active">>};
        _ -> #{}
    end.

pm_row(Uid) ->
    ShouldExist =
        case get({ms_ds_tests, pm_exists}) of
            true -> true;
            _ -> false
        end,
    case Uid =:= ?MEMBER2 andalso ShouldExist of
        true -> #{<<"status">> => <<"active">>};
        _ -> #{}
    end.

ms_row(Status) ->
    #{
        <<"id">> => ?MS_ID,
        <<"project_id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"name">> => <<"M1">>,
        <<"status">> => Status
    }.

ds_mocks(CurrStatus) ->
    [
        {project_repo, [
            {'find_tx', 3, fun
                (_Conn, ?PROJECT_ID, _) ->
                    #{
                        <<"id">> => ?PROJECT_ID,
                        <<"workspace_id">> => ?WS_ID,
                        <<"owner_id">> => ?OWNER
                    };
                (_, _, _) ->
                    #{}
            end}
        ]},
        %% 归档写守卫 mock（ZC-09R 真实执行暴露：原空转形态下未 mock
        %% workspace_guard，经 workspace_resolver 真查库 fail-closed 503）
        {workspace_guard, [
            {'ensure_writable_tx', 2, fun(_Conn, _Target) ->
                case get({ms_ds_tests, ws_status}) of
                    <<"archived">> -> {error, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}};
                    _ -> ok
                end
            end},
            {'abort_on_error', 1, fun
                (ok) -> ok;
                ({error, Reason}) -> throw({abort_tx, Reason})
            end}
        ]},
        {workspace_member_repo, [
            {'find_tx', 4, fun(_Conn, _WsId, Uid, _) -> ws_member_row(Uid) end}
        ]},
        {project_milestone_repo, [
            {'find_tx', 3, fun
                (_Conn, ?MS_ID, _) -> ms_row(CurrStatus);
                (_, _, _) -> #{}
            end},
            {'find_by_id', 2, fun
                (?MS_ID, _) -> ms_row(CurrStatus);
                (_, _) -> #{}
            end},
            {'find_project_member_tx', 4, fun(_Conn, _Pid, Uid, _) -> pm_row(Uid) end},
            {'add_tx', 2, fun(_Conn, Data) ->
                %% self() 在 mock 调用时求值 = 测试用例进程（勿在构建期闭包捕获）
                self() !
                    {ms_add, maps:get(<<"name">>, Data), maps:get(<<"due_date">>, Data),
                        maps:get(<<"status">>, Data)},
                {ok, ?MS_ID}
            end},
            {'update_fields_tx', 3, fun(_Conn, ?MS_ID, Data) ->
                self() !
                    {ms_update, maps:get(<<"status">>, Data, undefined),
                        maps:get(<<"reached_at">>, Data, undefined),
                        maps:get(<<"name">>, Data, undefined),
                        maps:get(<<"due_date">>, Data, undefined)},
                {ok, 1}
            end},
            %% reach 并发守卫更新（M-2：WHERE 带 status='planned'；mock 默认
            %% 命中 1 行，语义与真库 planned 行一致）
            {'mark_reached_tx', 3, fun(_Conn, ?MS_ID, Data) ->
                self() !
                    {ms_update, maps:get(<<"status">>, Data, undefined),
                        maps:get(<<"reached_at">>, Data, undefined),
                        maps:get(<<"name">>, Data, undefined),
                        maps:get(<<"due_date">>, Data, undefined)},
                {ok, 1}
            end},
            {'list_by_project', 4, fun(_, _, _, _) -> {ok, []} end}
        ]},
        {project_event_repo, [
            {'insert_tx', 2, fun(Conn, Data) ->
                self() ! {event_insert, Conn, Data},
                {ok, 510001}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()}
        ]}
    ].

%% 每个内层用例开头调用：显式重置进程字典状态 + 清空邮箱（防用例间串态：
%% EUnit 同 context 用例默认在同一进程连续执行）
reset_state() ->
    erase({ms_ds_tests, ws_status}),
    drain_msgs().

drain_msgs() ->
    receive
        _ -> drain_msgs()
    after 0 -> ok
    end.

no_event() ->
    receive
        {event_insert, _, _} -> ?assert(false, "orphan event written")
    after 0 ->
        ok
    end.

%%% ===================================================================
%%% create：事件同 Conn + 守卫链
%%% ===================================================================

create_ok_writes_event_same_conn_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"planned">>), [
        {"create writes milestone_created event on same tx conn", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_ds:create(?OWNER, ?PROJECT_ID, <<"M1">>, {2026, 9, 30})
            ),
            receive
                {ms_add, Name, DueDate, Status} ->
                    ?assertEqual(<<"M1">>, Name),
                    ?assertEqual({2026, 9, 30}, DueDate),
                    ?assertEqual(<<"planned">>, Status)
            after 500 ->
                ?assert(false, "milestone not inserted")
            end,
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(?PROJECT_ID, maps:get(<<"project_id">>, Data)),
                    ?assertEqual(?MS_ID, maps:get(<<"target_id">>, Data)),
                    ?assertEqual(<<"milestone_created">>, maps:get(<<"event_type">>, Data)),
                    ?assertEqual(?OWNER, maps:get(<<"actor_id">>, Data))
            after 500 ->
                ?assert(false, "milestone_created event not written")
            end
        end}
    ]).

create_guards_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"planned">>), [
        {"create on missing project 404 without event", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {404, _}},
                project_milestone_ds:create(?OWNER, 42, <<"M1">>, null)
            ),
            no_event()
        end},
        {"create in archived workspace 980 without event", fun() ->
            reset_state(),
            put({ms_ds_tests, ws_status}, <<"archived">>),
            ?assertMatch(
                {error, {980, _}},
                project_milestone_ds:create(?OWNER, ?PROJECT_ID, <<"M1">>, null)
            ),
            no_event()
        end},
        {"create by workspace guest 403 without event", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {403, _}},
                project_milestone_ds:create(?GUEST, ?PROJECT_ID, <<"M1">>, null)
            ),
            no_event()
        end},
        {"create by ws member but not project member 403 without event", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {403, _}},
                project_milestone_ds:create(?MEMBER2, ?PROJECT_ID, <<"M1">>, null)
            ),
            no_event()
        end}
    ]).

%%% ===================================================================
%%% reach：单向状态机 + 幂等 + 事件（M-2：mark_reached_tx 守卫更新）
%%% ===================================================================

reach_state_machine_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"planned">>), [
        {"reach planned->reached writes reached_at and event on same conn", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _, reached}, project_milestone_ds:reach(?OWNER, ?MS_ID)
            ),
            receive
                {ms_update, Status, ReachedAt, undefined, undefined} ->
                    ?assertEqual(<<"reached">>, Status),
                    ?assert(is_binary(ReachedAt) orelse tuple_size(ReachedAt) >= 2)
            after 500 ->
                ?assert(false, "reach update not written")
            end,
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"milestone_reached">>, maps:get(<<"event_type">>, Data))
            after 500 ->
                ?assert(false, "milestone_reached event not written")
            end
        end},
        {"reach missing milestone 404 without event", fun() ->
            reset_state(),
            ?assertMatch({error, {404, _}}, project_milestone_ds:reach(?OWNER, 42)),
            no_event()
        end},
        {"reach in archived workspace 980 without event", fun() ->
            reset_state(),
            put({ms_ds_tests, ws_status}, <<"archived">>),
            ?assertMatch({error, {980, _}}, project_milestone_ds:reach(?OWNER, ?MS_ID)),
            no_event()
        end},
        {"reach by guest 403 without event", fun() ->
            reset_state(),
            ?assertMatch({error, {403, _}}, project_milestone_ds:reach(?GUEST, ?MS_ID)),
            no_event()
        end}
    ]).

%% 已 reached 幂等：独立 fixture（当前状态行即为 reached）
reach_idempotent_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"reached">>), [
        {"reach on already reached is idempotent without update or event", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _, already_reached}, project_milestone_ds:reach(?OWNER, ?MS_ID)
            ),
            receive
                {ms_update, _, _, _, _} ->
                    ?assert(false, "idempotent reach must not update")
            after 0 ->
                ok
            end,
            no_event()
        end}
    ]).

%%% ===================================================================
%%% update：字段白名单 + 事件
%%% ===================================================================

update_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"planned">>), [
        {"update name+due_date writes fields and milestone_updated event", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _},
                project_milestone_ds:update(?OWNER, ?MS_ID, <<"M2">>, {2026, 10, 8})
            ),
            receive
                {ms_update, undefined, undefined, Name, DueDate} ->
                    ?assertEqual(<<"M2">>, Name),
                    ?assertEqual({2026, 10, 8}, DueDate)
            after 500 ->
                ?assert(false, "update not written")
            end,
            receive
                {event_insert, _Conn, Data} ->
                    ?assertEqual(<<"milestone_updated">>, maps:get(<<"event_type">>, Data))
            after 500 ->
                ?assert(false, "milestone_updated event not written")
            end
        end},
        {"update with no fields is a no-op without update or event", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_ds:update(?OWNER, ?MS_ID, undefined, undefined)
            ),
            receive
                {ms_update, _, _, _, _} ->
                    ?assert(false, "no-op update must not write")
            after 0 ->
                ok
            end,
            no_event()
        end},
        {"update missing milestone 404 without event", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {404, _}},
                project_milestone_ds:update(?OWNER, 42, <<"M2">>, undefined)
            ),
            no_event()
        end}
    ]).

%%% ===================================================================
%%% 事件写失败 → 整体 abort（无孤儿业务行；真库整体回滚见集成测试）
%%% ===================================================================

event_failure_aborts_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(<<"planned">>), [
        {"event insert failure aborts tx and propagates error", fun() ->
            reset_state(),
            %% 追加覆盖 insert_tx 使其失败（mock 调用时求值 = 用例执行进程）
            meck:expect(project_event_repo, insert_tx, 2, fun(_Conn, _Data) ->
                self() ! {event_insert, failed_conn, #{}},
                {error, {simulated, event_write_failed}}
            end),
            ?assertMatch(
                {error, _}, project_milestone_ds:create(?OWNER, ?PROJECT_ID, <<"M1">>, null)
            ),
            receive
                {event_insert, _, _} -> ok
            after 500 ->
                ?assert(false, "event write not attempted")
            end
        end}
    ]).
