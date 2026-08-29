-module(project_member_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% W2 ZC-02 — project_member_logic 单元测试（权限模型）
%%%
%%% 权限模型（W2 计划 §ZC-02 纲要细化，实现决策）：
%%%   * 读成员列表：Project Owner / Workspace Owner（治理）/ active Project Member ✅；
%%%     非 Project Member 的普通 Workspace Member 直接打 project id → 403（W2 隔离）；
%%%     非 Workspace Member → 403
%%%   * 邀请 / Owner 转移：仅 Project Owner（Guest 身份一律 403 只读）
%%%   * 移除：Project Owner 或 Workspace Owner（治理权，"可移除任何 project member"）；
%%%     移除 Project Owner 本身 → 409（须先转移）
%%%   * Guest（含 active Project Member 的 Guest）：一切写操作 403（只读）
%%%   * 非 active Workspace Member：403（DB 触发器兜底前的应用层前置校验）
%%%
%%% 结构说明：?WITH_MECK_TESTS 是本文件本地宏（{setup, S, C, [用例]} 规范
%%% context 结构）——"{Desc, fun() -> fixture end}" 包装式会让 EUnit 空转判 ok。

-define(WS_ID, 820001).
-define(PROJECT_ID, 720001).
-define(MISSING_PROJECT_ID, 720002).
-define(TARGET, 920009).

%% Owner 920001：Project Owner + Workspace Owner
-define(OWNER, 920001).
%% 工作区 Owner（非 Project Owner；治理权：仅移除）
-define(WS_OWNER2, 920005).
%% 普通工作区成员 + active Project Member
-define(MEMBER2, 920002).
%% 普通工作区成员，非 Project Member（直接 ID 直访者）
-define(WSMEMBER, 920003).
%% 工作区 Guest（active Project Member；只读）
-define(GUEST, 920004).
%% 非工作区成员
-define(OUTSIDER, 920006).

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
%%% Mock 基建（按 Uid 参数化，无进程字典状态）
%%% ===================================================================

project_row() ->
    #{
        <<"id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"owner_id">> => ?OWNER,
        <<"name">> => <<"W2 成员测试项目">>
    }.

my_role(Uid) ->
    case Uid of
        ?OWNER -> {ok, <<"owner">>};
        ?WS_OWNER2 -> {ok, <<"owner">>};
        ?MEMBER2 -> {ok, <<"member">>};
        ?WSMEMBER -> {ok, <<"member">>};
        ?GUEST -> {ok, <<"guest">>};
        _ -> {error, {403, <<"非工作区成员，禁止访问该资源"/utf8>>}}
    end.

pm_row(Uid) ->
    case Uid of
        ?MEMBER2 -> #{<<"status">> => <<"active">>};
        ?GUEST -> #{<<"status">> => <<"active">>};
        _ -> #{}
    end.

logic_mocks() ->
    [
        {project_ds, [
            {'find_by_id', 1, fun
                (?MISSING_PROJECT_ID) -> #{};
                (?PROJECT_ID) -> project_row()
            end}
        ]},
        {workspace_logic, [
            {'my_role', 2, fun(_WsId, Uid) -> my_role(Uid) end}
        ]},
        {project_member_ds, [
            {'find', 2, fun(_Pid, Uid) -> pm_row(Uid) end},
            {'list', 3, fun(Pid, Page, Size) ->
                self() ! {ds_list, Pid, Page, Size},
                {ok, #{list => [], page => Page, size => Size, total => 0, total_page => 0}}
            end},
            {'invite', 3, fun(Actor, Pid, Target) ->
                self() ! {ds_invite, Actor, Pid, Target},
                {ok, #{}, created}
            end},
            {'remove', 3, fun(Actor, Pid, Target) ->
                self() ! {ds_remove, Actor, Pid, Target},
                {ok, #{user_id => Target, status => <<"removed">>}, removed}
            end},
            {'transfer_owner', 3, fun(Actor, Pid, Target) ->
                self() ! {ds_transfer, Actor, Pid, Target},
                {ok, #{owner_id => Target, previous_owner_id => Actor}}
            end}
        ]}
    ].

drain_msgs() ->
    receive
        _ -> drain_msgs()
    after 0 -> ok
    end.

expect_no_more_msgs() ->
    receive
        Other -> ?assert(false, io_lib:format("unexpected message: ~p", [Other]))
    after 0 -> ok
    end.

%%% ===================================================================
%%% 读：成员列表（直接 ID 403 / Guest 只读可读 / 隔离）
%%% ===================================================================

list_permission_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"project owner reads member list", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, #{list := []}}, project_member_logic:list(?OWNER, ?PROJECT_ID, 1, 10)
            ),
            receive
                {ds_list, ?PROJECT_ID, 1, 10} -> ok
            after 0 -> ?assert(false, "ds list not called")
            end
        end},
        {"active project member reads member list", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, #{list := []}}, project_member_logic:list(?MEMBER2, ?PROJECT_ID, 1, 10)
            )
        end},
        {"guest project member reads member list (read-only)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, #{list := []}}, project_member_logic:list(?GUEST, ?PROJECT_ID, 1, 10)
            )
        end},
        {"workspace owner reads member list (governance)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, #{list := []}}, project_member_logic:list(?WS_OWNER2, ?PROJECT_ID, 1, 10)
            )
        end},
        {"non-project workspace member direct id access rejected 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:list(?WSMEMBER, ?PROJECT_ID, 1, 10)
            ),
            expect_no_more_msgs()
        end},
        {"non workspace member rejected 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:list(?OUTSIDER, ?PROJECT_ID, 1, 10)
            ),
            expect_no_more_msgs()
        end},
        {"member list on missing project rejected 404", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {404, _}}, project_member_logic:list(?OWNER, ?MISSING_PROJECT_ID, 1, 10)
            ),
            expect_no_more_msgs()
        end}
    ]).

%%% ===================================================================
%%% 写：邀请（仅 Project Owner；Guest 403）
%%% ===================================================================

invite_permission_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"project owner invites", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _, created}, project_member_logic:invite(?OWNER, ?PROJECT_ID, ?TARGET)
            ),
            receive
                {ds_invite, ?OWNER, ?PROJECT_ID, ?TARGET} -> ok
            after 0 -> ?assert(false, "ds invite not called")
            end
        end},
        {"workspace owner (non project owner) cannot invite 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:invite(?WS_OWNER2, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"ordinary member cannot invite 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:invite(?MEMBER2, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"guest cannot invite 403 (read-only)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:invite(?GUEST, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"non workspace member cannot invite 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:invite(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"invite with invalid target uid rejected 400", fun() ->
            drain_msgs(),
            ?assertMatch({error, {400, _}}, project_member_logic:invite(?OWNER, ?PROJECT_ID, 0)),
            expect_no_more_msgs()
        end}
    ]).

%%% ===================================================================
%%% 写：移除（Project Owner / Workspace Owner 治理权；移除 Owner 409）
%%% ===================================================================

remove_permission_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"project owner removes member", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _, removed}, project_member_logic:remove(?OWNER, ?PROJECT_ID, ?MEMBER2)
            ),
            receive
                {ds_remove, ?OWNER, ?PROJECT_ID, ?MEMBER2} -> ok
            after 0 -> ?assert(false, "ds remove not called")
            end
        end},
        {"workspace owner removes any project member (governance)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, _, removed}, project_member_logic:remove(?WS_OWNER2, ?PROJECT_ID, ?MEMBER2)
            ),
            receive
                {ds_remove, ?WS_OWNER2, ?PROJECT_ID, ?MEMBER2} -> ok
            after 0 -> ?assert(false, "ds remove not called")
            end
        end},
        {"ordinary member cannot remove 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:remove(?MEMBER2, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"guest cannot remove 403 (read-only)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:remove(?GUEST, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end},
        {"removing the project owner rejected 409 (transfer first)", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {409, _}},
                project_member_logic:remove(?WS_OWNER2, ?PROJECT_ID, ?OWNER)
            ),
            expect_no_more_msgs()
        end},
        {"non workspace member cannot remove 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}}, project_member_logic:remove(?OUTSIDER, ?PROJECT_ID, ?TARGET)
            ),
            expect_no_more_msgs()
        end}
    ]).

%%% ===================================================================
%%% 写：Owner 转移（仅 Project Owner）
%%% ===================================================================

transfer_permission_test_() ->
    ?WITH_MECK_TESTS(logic_mocks(), [
        {"project owner transfers ownership", fun() ->
            drain_msgs(),
            ?assertMatch(
                {ok, #{owner_id := ?MEMBER2}},
                project_member_logic:transfer_owner(?OWNER, ?PROJECT_ID, ?MEMBER2)
            ),
            receive
                {ds_transfer, ?OWNER, ?PROJECT_ID, ?MEMBER2} -> ok
            after 0 -> ?assert(false, "ds transfer not called")
            end
        end},
        {"workspace owner (non project owner) cannot transfer 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_member_logic:transfer_owner(?WS_OWNER2, ?PROJECT_ID, ?MEMBER2)
            ),
            expect_no_more_msgs()
        end},
        {"transfer to self rejected 400", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {400, _}},
                project_member_logic:transfer_owner(?OWNER, ?PROJECT_ID, ?OWNER)
            ),
            expect_no_more_msgs()
        end},
        {"non workspace member cannot transfer 403", fun() ->
            drain_msgs(),
            ?assertMatch(
                {error, {403, _}},
                project_member_logic:transfer_owner(?OUTSIDER, ?PROJECT_ID, ?MEMBER2)
            ),
            expect_no_more_msgs()
        end}
    ]).
