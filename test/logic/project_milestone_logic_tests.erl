-module(project_milestone_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% ZC-03 W2 Milestone — project_milestone_logic 单元测试（mock DS/repo）
%%%
%%% 覆盖：字段白名单校验（name 必填≤200；due_date 仅 YYYY-MM-DD/null；
%%% 未知字段忽略；status 拒绝语义由 handler 层保证）、
%%% due_date 解析（合法/非法/清空/保留）、读权限（404/非工作区成员 403/
%%% 非项目成员 403/guest 可读/owner 与 active project member 可读）、
%%% 分页归一（Page≥1，Size 1..100，status 过滤白名单）、
%%% DS 稳定错误码透传（980/403）与未知错误归一 500、
%%% admin_page total 用独立 COUNT（M-7）。
%%%
%%% 结构说明（ZC-09R H-1 改造）：?WITH_MECK_TESTS 是本文件本地宏
%%%（{setup, S, C, [用例]} 规范 context 结构）——"{Desc, fun() -> fixture end}"
%%% 包装式会让 EUnit 空转判 ok（内层断言从不执行），勿回退该形态。

-define(WS_ID, 820001).
-define(OWNER, 920001).
-define(MEMBER2, 920002).
-define(OUTSIDER, 920004).
-define(WS_OWNER, 920005).
-define(PROJECT_ID, 720001).
-define(MS_ID, 620001).

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
%%% 用例间状态复位（EUnit 同 context 用例在同一进程连续执行，
%%% 进程字典与测试进程邮箱会跨用例残留）
%%% ===================================================================

reset_state() ->
    erase({ms_logic_tests, pm_row}),
    erase({ms_logic_tests, ds_err}),
    drain_msgs().

drain_msgs() ->
    receive
        _ -> drain_msgs()
    after 0 -> ok
    end.

%%% ===================================================================
%%% 校验与解析（纯逻辑）
%%% ===================================================================

create_validation_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"create empty name rejected 400 without ds call", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<>>, null)
            ),
            ?assertEqual(0, meck:num_calls(project_milestone_ds, create, 4))
        end},
        {"create name over 200 chars rejected 400", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(
                    ?OWNER, ?PROJECT_ID, binary:copy(<<"长"/utf8>>, 201), null
                )
            )
        end},
        {"create invalid due_date rejected 400", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, <<"2026-13-40">>)
            ),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, <<"not-a-date">>)
            )
        end},
        {"create with valid due_date passes {Y,M,D} tuple to ds", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, <<"2026-09-30">>)
            ),
            receive
                {ds_create, Uid, Pid, Name, DueDate} ->
                    ?assertEqual(?OWNER, Uid),
                    ?assertEqual(?PROJECT_ID, Pid),
                    ?assertEqual(<<"M1">>, Name),
                    ?assertEqual({2026, 9, 30}, DueDate)
            after 500 ->
                ?assert(false, "ds create not called")
            end
        end},
        {"create name at 200 chars boundary accepted", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, binary:copy(<<"a">>, 200), null)
            )
        end}
    ]).

update_parsing_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"update parses due_date binary to tuple and keeps undefined name", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _},
                project_milestone_logic:update(?OWNER, ?MS_ID, undefined, <<"2026-10-08">>)
            ),
            receive
                {ds_update, _Uid, _MsId, Name, DueDate} ->
                    ?assertEqual(undefined, Name),
                    ?assertEqual({2026, 10, 8}, DueDate)
            after 500 ->
                ?assert(false, "ds update not called")
            end
        end},
        {"update null due_date means clear", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_logic:update(?OWNER, ?MS_ID, <<"N">>, null)
            ),
            receive
                {ds_update, _, _, Name, DueDate} ->
                    ?assertEqual(<<"N">>, Name),
                    ?assertEqual(null, DueDate)
            after 500 ->
                ?assert(false, "ds update not called")
            end
        end},
        {"update empty name rejected 400", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {400, _}},
                project_milestone_logic:update(?OWNER, ?MS_ID, <<>>, undefined)
            )
        end}
    ]).

%%% ===================================================================
%%% list：读权限 + 分页归一
%%% ===================================================================

list_permission_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"list by owner ok", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_logic:list(?OWNER, ?PROJECT_ID, all, 1, 10)
            )
        end},
        {"list by active project member ok (guest readable)", fun() ->
            reset_state(),
            put({ms_logic_tests, pm_row}, <<"active">>),
            ?assertMatch(
                {ok, _}, project_milestone_logic:list(?MEMBER2, ?PROJECT_ID, all, 1, 10)
            )
        end},
        {"list by workspace owner governance ok", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_logic:list(?WS_OWNER, ?PROJECT_ID, all, 1, 10)
            )
        end},
        {"list by non project member 403", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {403, _}},
                project_milestone_logic:list(?MEMBER2, ?PROJECT_ID, all, 1, 10)
            )
        end},
        {"list by non workspace member 403 (from project detail)", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {403, _}},
                project_milestone_logic:list(?OUTSIDER, ?PROJECT_ID, all, 1, 10)
            )
        end},
        {"list unknown project 404", fun() ->
            reset_state(),
            ?assertMatch(
                {error, {404, _}},
                project_milestone_logic:list(?OWNER, 42, all, 1, 10)
            )
        end}
    ]).

list_normalization_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"list normalizes page/size and filters invalid status to all", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_logic:list(?OWNER, ?PROJECT_ID, <<"weird">>, 0, 500)
            ),
            receive
                {ds_list, _Pid, Status, Page, Size} ->
                    ?assertEqual(all, Status),
                    ?assertEqual(1, Page),
                    ?assertEqual(100, Size)
            after 500 ->
                ?assert(false, "ds list not called")
            end
        end},
        {"list keeps planned status filter", fun() ->
            reset_state(),
            ?assertMatch(
                {ok, _}, project_milestone_logic:list(?OWNER, ?PROJECT_ID, <<"planned">>, 2, 10)
            ),
            receive
                {ds_list, _, Status, Page, Size} ->
                    ?assertEqual(<<"planned">>, Status),
                    ?assertEqual(2, Page),
                    ?assertEqual(10, Size)
            after 500 ->
                ?assert(false, "ds list not called")
            end
        end}
    ]).

%%% ===================================================================
%%% reach 与错误归一
%%% ===================================================================

reach_and_errors_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"reach delegates to ds", fun() ->
            reset_state(),
            ?assertMatch({ok, _, reached}, project_milestone_logic:reach(?OWNER, ?MS_ID)),
            receive
                {ds_reach, Uid, MsId} ->
                    ?assertEqual(?OWNER, Uid),
                    ?assertEqual(?MS_ID, MsId)
            after 500 ->
                ?assert(false, "ds reach not called")
            end
        end},
        {"archived error 980 passes through from ds", fun() ->
            reset_state(),
            put({ms_logic_tests, ds_err}, {980, <<"工作区已归档，写操作被拒绝"/utf8>>}),
            ?assertMatch(
                {error, {980, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, null)
            )
        end},
        {"guest 403 passes through from ds", fun() ->
            reset_state(),
            put({ms_logic_tests, ds_err}, {403, <<"Guest 角色只能查看里程碑，写操作被拒绝"/utf8>>}),
            ?assertMatch(
                {error, {403, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, null)
            )
        end},
        {"unknown ds error normalized to 500", fun() ->
            reset_state(),
            put({ms_logic_tests, ds_err}, {db_down, whatever}),
            ?assertMatch(
                {error, {500, _}},
                project_milestone_logic:create(?OWNER, ?PROJECT_ID, <<"M1">>, null)
            )
        end}
    ]).

%%% ===================================================================
%%% admin_page：total 用独立 COUNT（M-7）
%%% ===================================================================

admin_page_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"admin page total comes from count not page rows", fun() ->
            reset_state(),
            meck:expect(project_milestone_repo, list_by_project, 4, fun(_Pid, _Status, _P, _S) ->
                {ok, [#{<<"id">> => 1}, #{<<"id">> => 2}]}
            end),
            meck:expect(project_milestone_repo, count_by_project, 2, fun(_Pid, _Status) ->
                {ok, 7}
            end),
            {ok, P} = project_milestone_logic:admin_page(?PROJECT_ID, all, 1, 2),
            ?assertEqual(7, maps:get(total, P), "total 必须取独立 COUNT 而非当前页行数"),
            ?assertEqual(4, maps:get(total_page, P)),
            ?assertEqual(2, length(maps:get(list, P)))
        end},
        {"admin page count failure normalized to 500", fun() ->
            reset_state(),
            meck:expect(project_milestone_repo, list_by_project, 4, fun(_Pid, _Status, _P, _S) ->
                {ok, [#{<<"id">> => 1}]}
            end),
            meck:expect(project_milestone_repo, count_by_project, 2, fun(_Pid, _Status) ->
                {error, {simulated, count_failed}}
            end),
            ?assertMatch(
                {error, {500, _}},
                project_milestone_logic:admin_page(?PROJECT_ID, all, 1, 2)
            )
        end}
    ]).

%%% ===================================================================
%%% mock 基建
%%% ===================================================================

logic_mocks() ->
    Project = #{
        <<"id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"owner_id">> => ?OWNER
    },
    [
        {project_milestone_ds, [
            {'create', 4, fun(Uid, Pid, Name, DueDate) ->
                %% self() 在 mock 调用时求值 = 测试用例进程（勿在构建期闭包捕获）
                self() ! {ds_create, Uid, Pid, Name, DueDate},
                case get({ms_logic_tests, ds_err}) of
                    {C, M} when is_integer(C) -> {error, {C, M}};
                    {C, M} -> {error, {C, M}};
                    _ -> {ok, #{<<"id">> => ?MS_ID, <<"name">> => Name}}
                end
            end},
            {'update', 4, fun(Uid, MsId, Name, DueDate) ->
                self() ! {ds_update, Uid, MsId, Name, DueDate},
                {ok, #{<<"id">> => MsId}}
            end},
            {'reach', 2, fun(Uid, MsId) ->
                self() ! {ds_reach, Uid, MsId},
                {ok, #{<<"id">> => MsId}, reached}
            end},
            {'list_by_project', 4, fun(Pid, Status, Page, Size) ->
                self() ! {ds_list, Pid, Status, Page, Size},
                {ok, []}
            end}
        ]},
        {project_member_logic, [
            {'ensure_can_read', 2, fun
                (_Uid, 42) ->
                    {error, {404, <<"项目不存在"/utf8>>}};
                (?OUTSIDER, _) ->
                    {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}};
                (?MEMBER2, ?PROJECT_ID) ->
                    case get({ms_logic_tests, pm_row}) of
                        <<"active">> -> {ok, Project};
                        _ -> {error, {403, <<"仅项目成员可访问该项目资源"/utf8>>}}
                    end;
                (_Uid, ?PROJECT_ID) ->
                    {ok, Project}
            end}
        ]},
        {project_milestone_repo, [
            {'list_by_project', 4, fun(_Pid, _Status, _Page, _Size) -> {ok, []} end},
            {'count_by_project', 2, fun(_Pid, _Status) -> {ok, 0} end}
        ]}
    ].
