-module(project_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP4/T6a — project_logic 单元测试
%%% 覆盖：创建矩阵（Owner/Member 可建、Guest/非成员 403）、creator=owner、
%%% 列表/详情可见性（active 成员可读）、Guest 只读（写 403）、
%%% 状态值域、archived 拒写（稳定错误码 980）、移除成员时 task 冲突清单
%%% （workspace_logic 复用 workspace_member_repo:unfinished_tasks_of_user）。

-define(WS_ID, 800001).
-define(OWNER, 900001).
-define(MEMBER2, 900002).
-define(GUEST, 900003).
-define(OUTSIDER, 900004).
-define(PROJECT_ID, 700001).

%% 模拟事务语义 + guard 的 workspace 状态（进程字典控制 active/archived）
base_mocks() ->
    Self = self(),
    [
        {workspace_ds, [
            {'find_by_id', 1, fun
                (?WS_ID) -> #{<<"id">> => ?WS_ID};
                (_) -> #{}
            end}
        ]},
        {workspace_logic, [
            {'ensure_can_create_resource', 2, fun
                (?WS_ID, U) ->
                    case U of
                        ?GUEST -> {error, {403, <<"Guest 角色不能创建工作区资源"/utf8>>}};
                        ?OUTSIDER -> {error, {403, <<"非工作区成员"/utf8>>}};
                        _ -> ok
                    end;
                %% 非 800001 的 workspace：模拟 my_role 查不到成员归属
                %% （底层 load_workspace 404），使 create 走 404 透传
                (_, _) ->
                    {error, {404, <<"工作区不存在"/utf8>>}}
            end},
            {'ensure_member', 2, fun(?WS_ID, U) ->
                case U of
                    ?OUTSIDER -> {error, {403, <<"非工作区成员"/utf8>>}};
                    _ -> {ok, <<"member">>}
                end
            end}
        ]},
        {project_repo, [
            {'add_tx', 2, fun(_Conn, Data) ->
                Self ! {project_add, maps:get(<<"owner_id">>, Data)},
                {ok, ?PROJECT_ID}
            end},
            {'find_tx', 3, fun(_Conn, ?PROJECT_ID, _) ->
                #{<<"id">> => ?PROJECT_ID, <<"workspace_id">> => ?WS_ID}
            end},
            {'update_fields_tx', 3, fun(_Conn, _Id, _Data) ->
                Self ! {project_update, _Id},
                {ok, 1}
            end},
            {'page_by_workspace', 4, fun(_, _, _, _) ->
                {ok, #{list => [], page => 1, size => 10, total => 0, total_page => 0}}
            end},
            {'find_by_id', 2, fun
                (?PROJECT_ID, _) -> project_row();
                (_, _) -> #{}
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, tx_fun()},
            %% resolver：workspace 存在性（自动提交连接）
            {'one', 2, fun(<<"SELECT id FROM workspace", _/binary>>, _) ->
                {ok, #{<<"id">> => ?WS_ID}}
            end},
            %% 注意：meck 同函数二次 expect 会覆盖（非追加），多前缀须合并为
            %% 一个多子句 fun
            {'query', 3, fun
                %% W2 接线：create 前置 ensure_owner_member_tx 回查成员行
                (_C, <<"SELECT workspace_id,project_id,user_id", _/binary>>, _) ->
                    {ok, []};
                %% guard 事务版 FOR UPDATE 状态查询
                (_C, <<"SELECT status FROM workspace", _/binary>>, _) ->
                    {ok, [#{<<"status">> => ws_status()}]}
            end},
            {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
        ]}
    ].

tx_fun() ->
    fun(Fun) ->
        try
            Fun(fake_conn)
        catch
            throw:{abort_tx, Reason} -> {error, Reason}
        end
    end.

ws_status() ->
    case get({project_tests, ws_status}) of
        S when is_binary(S) -> S;
        _ -> <<"active">>
    end.

project_row() ->
    #{
        <<"id">> => ?PROJECT_ID,
        <<"workspace_id">> => ?WS_ID,
        <<"name">> => <<"官网改版"/utf8>>,
        <<"description">> => <<>>,
        <<"owner_id">> => ?OWNER,
        <<"status">> => <<"active">>
    }.

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
%%% 创建矩阵
%%% ===================================================================

create_matrix_test_() ->
    [
        {"owner can create", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({ok, _}, project_logic:create(?OWNER, ?WS_ID, <<"P1">>, <<>>)),
                receive
                    {project_add, ?OWNER} -> ok
                after 500 -> ?assert(false, "creator not set as owner_id")
                end
            end)
        end},
        {"member can create", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({ok, _}, project_logic:create(?MEMBER2, ?WS_ID, <<"P2">>, <<>>)),
                receive
                    {project_add, ?MEMBER2} -> ok
                after 500 -> ?assert(false, "member creator not owner_id")
                end
            end)
        end},
        {"guest cannot create (403)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {403, _}}, project_logic:create(?GUEST, ?WS_ID, <<"P3">>, <<>>)
                )
            end)
        end},
        {"non member cannot create (403)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {403, _}}, project_logic:create(?OUTSIDER, ?WS_ID, <<"P4">>, <<>>)
                )
            end)
        end},
        {"archived workspace rejects create with stable code 980", fun() ->
            put({project_tests, ws_status}, <<"archived">>),
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    project_logic:create(?OWNER, ?WS_ID, <<"P5">>, <<>>)
                ),
                %% 进程字典跨用例共享（simple fun 同进程），必须复原状态
                put({project_tests, ws_status}, <<"active">>),
                ok
            end)
        end},
        {"empty name rejected 400", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {400, _}}, project_logic:create(?OWNER, ?WS_ID, <<>>, <<>>)
                )
            end)
        end},
        {"workspace not found 404", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {404, _}}, project_logic:create(?OWNER, 999999, <<"P6">>, <<>>)
                )
            end)
        end}
    ].

%%% ===================================================================
%%% 可见性（W0：Project 对 active Workspace Member 可见）
%%% ===================================================================

visibility_test_() ->
    [
        {"active member can read detail", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {ok, #{<<"id">> := ?PROJECT_ID}}, project_logic:detail(?MEMBER2, ?PROJECT_ID)
                )
            end)
        end},
        {"guest can read detail (read-only role)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({ok, _}, project_logic:detail(?GUEST, ?PROJECT_ID))
            end)
        end},
        {"non member cannot read detail (403)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({error, {403, _}}, project_logic:detail(?OUTSIDER, ?PROJECT_ID))
            end)
        end},
        {"active member can list", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({ok, #{list := []}}, project_logic:list(?MEMBER2, ?WS_ID, 1, 10))
            end)
        end},
        {"non member cannot list (403)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch({error, {403, _}}, project_logic:list(?OUTSIDER, ?WS_ID, 1, 10))
            end)
        end}
    ].

%%% ===================================================================
%%% Guest 只读 + 状态流转
%%% ===================================================================

write_permission_test_() ->
    [
        {"guest cannot update (403)", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {403, _}},
                    project_logic:update(?GUEST, ?PROJECT_ID, <<"新名"/utf8>>, undefined)
                )
            end)
        end},
        {"member can update profile", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {ok, _},
                    project_logic:update(?MEMBER2, ?PROJECT_ID, <<"新名"/utf8>>, <<"描述"/utf8>>)
                ),
                receive
                    {project_update, ?PROJECT_ID} -> ok
                after 500 -> ?assert(false, "update not executed")
                end
            end)
        end},
        {"member can change status active->done", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {ok, _}, project_logic:update_status(?MEMBER2, ?PROJECT_ID, <<"done">>)
                )
            end)
        end},
        {"invalid status rejected 400", fun() ->
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {400, _}},
                    project_logic:update_status(?MEMBER2, ?PROJECT_ID, <<"archived">>)
                )
            end)
        end},
        {"archived workspace rejects update with 980", fun() ->
            put({project_tests, ws_status}, <<"archived">>),
            run_with_mocks(base_mocks(), fun() ->
                ?assertMatch(
                    {error, {980, _}},
                    project_logic:update(?MEMBER2, ?PROJECT_ID, <<"新名"/utf8>>, undefined)
                ),
                put({project_tests, ws_status}, <<"active">>),
                ok
            end)
        end}
    ].

%%% ===================================================================
%%% T6a-2：移除成员时 Task 冲突清单（workspace_logic 复用钩子验证）
%%% 数据来源：workspace_member_repo:unfinished_tasks_of_user/3
%%% （project_task JOIN project WHERE assignee_id=? AND status<>'done'）
%%% ===================================================================

remove_member_conflict_test_() ->
    Self = self(),
    {"remove_member returns 409 conflict list when assignee has unfinished tasks", fun() ->
        run_with_mocks(
            [
                {workspace_ds, [
                    %% ds 现行 1 元包装（原 2 元契约已收进 repo 层）
                    {'find_by_id', 1, fun(?WS_ID) ->
                        #{<<"id">> => ?WS_ID, <<"owner_id">> => ?OWNER}
                    end}
                ]},
                {workspace_member_repo, [
                    {'find', 3, fun(?WS_ID, ?OWNER, _) ->
                        #{<<"role">> => <<"owner">>, <<"status">> => <<"active">>}
                    end},
                    {'owned_projects_of_user', 3, fun(_, _, _) -> {ok, []} end},
                    {'unfinished_tasks_of_user', 3, fun(_, ?WS_ID, ?MEMBER2) ->
                        Self ! conflict_queried,
                        {ok, [
                            #{
                                <<"id">> => 600001,
                                <<"title">> => <<"未完成任务A"/utf8>>,
                                <<"status">> => <<"todo">>,
                                <<"project_id">> => ?PROJECT_ID
                            }
                        ]}
                    end},
                    {'list_active_workspace_groups_of_user', 3, fun(_, _, _) -> {ok, []} end},
                    {'remove_tx', 3, fun(_, _, _) ->
                        Self ! remove_executed_forbidden,
                        ok
                    end}
                ]},
                {elib_pg, [
                    {'with_tx', 1, tx_fun()},
                    %% ensure_not_archived 自动提交读（ds find_by_id/2 → repo one/2）
                    {'one', 2, fun(<<"SELECT status FROM workspace", _/binary>>, _) ->
                        {ok, #{<<"status">> => <<"active">>}}
                    end},
                    {'query', 3, fun(_C, _S, _P) -> {ok, []} end},
                    {'execute', 3, fun(_C, _S, _P) -> {ok, 1} end}
                ]}
            ],
            fun() ->
                Result = workspace_logic:remove_member(?OWNER, ?WS_ID, ?MEMBER2),
                {error, {409, Msg}} = Result,
                ?assertNotEqual(nomatch, binary:match(Msg, <<"membership_conflict">>)),
                receive
                    conflict_queried -> ok
                after 500 -> ?assert(false, "unfinished_tasks_of_user not consulted")
                end,
                receive
                    remove_executed_forbidden -> ?assert(false, "must not remove on conflict")
                after 0 -> ok
                end
            end
        )
    end}.

%%% ===================================================================
%%% 内部
%%% ===================================================================
