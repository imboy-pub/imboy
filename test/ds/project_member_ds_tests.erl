-module(project_member_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-02 — project_member_ds 单元测试（mock 事务 + repo）
%%% 覆盖：
%%%   * invite：created 写 member_invited 事件（同一 Conn=同一事务）、
%%%     已 active 幂等 existing 无事件、目标非 active workspace_member 400
%%%     （DB 触发器之前的应用层同事务前置校验，失败无部分写入）、
%%%     项目不存在 404、archived workspace 980
%%%   * remove：active→removed 写 member_removed 事件、
%%%     重复移除幂等 already_removed 无事件、项目不存在 404
%%%   * transfer_owner：双 upsert + owner 列更新 + member_owner_transferred 事件、
%%%     新 Owner 有未完成 assignee task 409（无任何写入）、Guest 目标 409、
%%%     非 active workspace_member 目标 409、非 Owner actor 403（事务内复检）
%%% 事件类型契约：chk_project_event_type 14 值，本 DS 只写
%%% member_invited / member_removed / member_owner_transferred 三值。
%%%
%%% 结构说明：?WITH_MECK_TESTS 是本文件本地宏（{setup, S, C, [用例]} 规范
%%% context 结构）——勿改用 "{Desc, fun() -> fixture end}" 包装式（EUnit 会把
%%% 返回 fixture 的普通 test fun 判 ok 而不执行内层断言，详见执行台账）。

-define(WS_ID, 810001).
-define(PROJECT_ID, 710001).
-define(MISSING_PROJECT_ID, 710002).
-define(OWNER, 910001).
-define(TARGET, 910002).
-define(OUTSIDER, 910003).
-define(EVENT_ID, 510001).

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
%%% Mock 基建
%%% ===================================================================

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

%% 每个内层用例开头调用：显式重置全部 mock 行为状态（防用例间串态）
reset_state() ->
    put({pm_ds_tests, ws_status}, <<"active">>),
    put({pm_ds_tests, ws_member_status}, <<"active">>),
    put({pm_ds_tests, ws_member_role}, <<"member">>),
    put({pm_ds_tests, upsert_result}, changed),
    put({pm_ds_tests, remove_count}, 1),
    put({pm_ds_tests, unfinished}, false),
    ok.

drain_msgs() ->
    receive
        _ -> drain_msgs()
    after 0 -> ok
    end.

project_row() ->
    #{
        <<"id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"owner_id">> => ?OWNER
    }.

member_row(Uid, Status) ->
    #{
        <<"workspace_id">> => ?WS_ID,
        <<"project_id">> => ?PROJECT_ID,
        <<"user_id">> => Uid,
        <<"invited_by">> => ?OWNER,
        <<"status">> => Status,
        <<"joined_at">> => <<"2026-08-29T00:00:00Z">>
    }.

ds_mocks() ->
    [
        {project_repo, [
            {'find_tx', 3, fun
                (_Conn, ?MISSING_PROJECT_ID, _Col) -> #{};
                (_Conn, ?PROJECT_ID, _Col) -> project_row()
            end},
            {'update_fields_tx', 3, fun(Conn, Pid, Data) ->
                %% self() 在 mock 调用时求值 = 测试进程（勿在 setup 闭包捕获）
                self() ! {project_update, Conn, Pid, Data},
                {ok, 1}
            end}
        ]},
        {workspace_guard, [
            {'ensure_writable_tx', 2, fun(_Conn, _Target) ->
                case get({pm_ds_tests, ws_status}) of
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
            {'find_tx', 4, fun(_Conn, _WsId, _Uid, _Col) ->
                case get({pm_ds_tests, ws_member_status}) of
                    <<"active">> ->
                        #{
                            <<"status">> => <<"active">>,
                            <<"role">> => get({pm_ds_tests, ws_member_role})
                        };
                    _ ->
                        #{}
                end
            end}
        ]},
        {project_member_repo, [
            {'upsert_active_tx', 5, fun(Conn, _WsId, _Pid, Uid, InvitedBy) ->
                self() ! {pm_upsert, Conn, Uid, InvitedBy},
                {ok, get({pm_ds_tests, upsert_result}), member_row(Uid, <<"active">>)}
            end},
            {'remove_tx', 3, fun(Conn, _Pid, Uid) ->
                self() ! {pm_remove, Conn, Uid},
                {ok, get({pm_ds_tests, remove_count})}
            end},
            {'unfinished_assignee_tasks_tx', 3, fun(_Conn, _Pid, _Uid) ->
                case get({pm_ds_tests, unfinished}) of
                    true ->
                        {ok, [
                            #{
                                <<"id">> => 990001,
                                <<"title">> => <<"未完成任务">>,
                                <<"status">> => <<"todo">>
                            }
                        ]};
                    _ ->
                        {ok, []}
                end
            end}
        ]},
        {project_event_repo, [
            {'insert_tx', 2, fun(Conn, Data) ->
                self() ! {event_insert, Conn, Data},
                {ok, ?EVENT_ID}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()}
        ]}
    ].

expect_no_more_msgs() ->
    receive
        Other -> ?assert(false, io_lib:format("unexpected message: ~p", [Other]))
    after 0 -> ok
    end.

%%% ===================================================================
%%% invite（邀请/恢复；幂等）
%%% ===================================================================

invite_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(), [
        {"invite new member writes member_invited event in same tx", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {ok, #{<<"user_id">> := ?TARGET}, created},
                project_member_ds:invite(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            receive
                {pm_upsert, Conn1, Uid, InvitedBy} ->
                    ?assertEqual(fake_conn, Conn1),
                    ?assertEqual(?TARGET, Uid),
                    ?assertEqual(?OWNER, InvitedBy)
            after 500 -> ?assert(false, "upsert not called")
            end,
            receive
                {event_insert, Conn2, Data} ->
                    %% 事件与成员写同一 Conn（同事务，无孤儿事件）
                    ?assertEqual(fake_conn, Conn2),
                    ?assertEqual(?PROJECT_ID, maps:get(<<"project_id">>, Data)),
                    ?assertEqual(<<"member_invited">>, maps:get(<<"event_type">>, Data)),
                    ?assertEqual(?TARGET, maps:get(<<"target_id">>, Data)),
                    ?assertEqual(?OWNER, maps:get(<<"actor_id">>, Data))
            after 500 -> ?assert(false, "member_invited event not written")
            end,
            expect_no_more_msgs()
        end},
        {"invite already-active member returns existing without event", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, upsert_result}, unchanged),
            ?assertMatch(
                {ok, #{<<"user_id">> := ?TARGET}, existing},
                project_member_ds:invite(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            %% upsert 照常发生（返回 unchanged），但不得写事件
            receive
                {pm_upsert, _, _, _} -> ok
            after 500 -> ?assert(false, "upsert not called")
            end,
            expect_no_more_msgs()
        end},
        {"invite non-active workspace member rejected 400 without writes", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_member_status}, <<"removed">>),
            ?assertMatch(
                {error, {400, _}},
                project_member_ds:invite(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"invite on missing project rejected 404", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}},
                project_member_ds:invite(?OWNER, ?MISSING_PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"invite on archived workspace rejected 980", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_status}, <<"archived">>),
            ?assertMatch(
                {error, {980, _}},
                project_member_ds:invite(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"invite rejected 403 when actor is not project owner (in-tx recheck, M-3)", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_member_ds:invite(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end}
    ]).

%%% ===================================================================
%%% remove（软删移除；幂等）
%%% ===================================================================

remove_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(), [
        {"remove active member writes member_removed event in same tx", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {ok, #{user_id := ?TARGET, status := <<"removed">>}, removed},
                project_member_ds:remove(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            receive
                {pm_remove, ConnR, ?TARGET} -> ?assertEqual(fake_conn, ConnR)
            after 500 -> ?assert(false, "remove_tx not called")
            end,
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"member_removed">>, maps:get(<<"event_type">>, Data)),
                    ?assertEqual(?TARGET, maps:get(<<"target_id">>, Data))
            after 500 -> ?assert(false, "member_removed event not written")
            end,
            expect_no_more_msgs()
        end},
        {"remove already-removed member is idempotent without event", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, remove_count}, 0),
            ?assertMatch(
                {ok, #{status := <<"removed">>}, already_removed},
                project_member_ds:remove(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            receive
                {pm_remove, _, _} -> ok
            after 0 -> ok
            end,
            expect_no_more_msgs()
        end},
        {"remove on missing project rejected 404", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}},
                project_member_ds:remove(?OWNER, ?MISSING_PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"remove on archived workspace rejected 980", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_status}, <<"archived">>),
            ?assertMatch(
                {error, {980, _}},
                project_member_ds:remove(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"remove rejected 403 when actor lacks governance (in-tx recheck, M-3)", fun() ->
            reset_state(),
            drain_msgs(),
            %% 非 project owner，且 ws role=member（无治理权）
            ?assertMatch(
                {error, {403, _}},
                project_member_ds:remove(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"remove by workspace owner (non project owner) passes in-tx governance recheck", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_member_role}, <<"owner">>),
            ?assertMatch(
                {ok, #{user_id := ?TARGET, status := <<"removed">>}, removed},
                project_member_ds:remove(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            receive
                {pm_remove, ConnR, ?TARGET} -> ?assertEqual(fake_conn, ConnR)
            after 500 -> ?assert(false, "remove_tx not called")
            end,
            receive
                {event_insert, Conn, Data} ->
                    ?assertEqual(fake_conn, Conn),
                    ?assertEqual(<<"member_removed">>, maps:get(<<"event_type">>, Data))
            after 500 -> ?assert(false, "member_removed event not written")
            end,
            expect_no_more_msgs()
        end}
    ]).

%%% ===================================================================
%%% transfer_owner（项目 Owner 转移）
%%% ===================================================================

transfer_owner_test_() ->
    ?WITH_MECK_TESTS(ds_mocks(), [
        {"transfer ok upserts both members updates owner and writes event", fun() ->
            reset_state(),
            drain_msgs(),
            {ok, Result} = project_member_ds:transfer_owner(?OWNER, ?PROJECT_ID, ?TARGET),
            ?assertEqual(?TARGET, maps:get(owner_id, Result)),
            ?assertEqual(?OWNER, maps:get(previous_owner_id, Result)),
            %% 新 Owner upsert（invited_by=旧 Owner）
            receive
                {pm_upsert, Conn1, ?TARGET, InvBy1} ->
                    ?assertEqual(fake_conn, Conn1),
                    ?assertEqual(?OWNER, InvBy1)
            after 500 -> ?assert(false, "target upsert missing")
            end,
            %% 原 Owner 保持成员身份（invited_by=NULL）
            receive
                {pm_upsert, Conn2, ?OWNER, null} -> ?assertEqual(fake_conn, Conn2)
            after 500 -> ?assert(false, "previous owner upsert missing")
            end,
            receive
                {project_update, Conn3, ?PROJECT_ID, Data} ->
                    ?assertEqual(fake_conn, Conn3),
                    ?assertEqual(?TARGET, maps:get(<<"owner_id">>, Data))
            after 500 -> ?assert(false, "project owner_id update missing")
            end,
            receive
                {event_insert, Conn4, EventData} ->
                    ?assertEqual(fake_conn, Conn4),
                    ?assertEqual(
                        <<"member_owner_transferred">>, maps:get(<<"event_type">>, EventData)
                    ),
                    ?assertEqual(?TARGET, maps:get(<<"target_id">>, EventData)),
                    Payload = jsone:decode(
                        maps:get(<<"payload">>, EventData), [{object_format, map}]
                    ),
                    ?assertEqual(?OWNER, maps:get(<<"from">>, Payload)),
                    ?assertEqual(?TARGET, maps:get(<<"to">>, Payload)),
                    ?assertEqual(?OWNER, maps:get(<<"actor">>, Payload))
            after 500 -> ?assert(false, "member_owner_transferred event not written")
            end,
            expect_no_more_msgs()
        end},
        {"transfer rejected 409 when target has unfinished assignee tasks", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, unfinished}, true),
            ?assertMatch(
                {error, {409, _}},
                project_member_ds:transfer_owner(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"transfer rejected 409 when target is workspace guest", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_member_role}, <<"guest">>),
            ?assertMatch(
                {error, {409, _}},
                project_member_ds:transfer_owner(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"transfer rejected 409 when target is not active workspace member", fun() ->
            reset_state(),
            drain_msgs(),
            put({pm_ds_tests, ws_member_status}, <<"removed">>),
            ?assertMatch(
                {error, {409, _}},
                project_member_ds:transfer_owner(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"transfer rejected 403 when actor is not project owner (in-tx recheck)", fun() ->
            reset_state(),
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_member_ds:transfer_owner(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end}
    ]).
