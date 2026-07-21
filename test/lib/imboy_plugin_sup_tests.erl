-module(imboy_plugin_sup_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin_sup 与 imboy_plugin_generic_sup 的 EUnit 测试
%%% Phase 1 切片 1：插件总监督树骨架
%%%
%%% 覆盖 / Coverage:
%%%   1. imboy_plugin_sup 启动后 4 个 plugin sup 全部注册
%%%   2. 每个 plugin sup 是真 supervisor 进程
%%%   3. 每个 plugin sup 当前 children 列表为空（Phase 1 切片 1）
%%%   4. Isolation：杀 channel_sup 后 moment_sup 不受影响（one_for_one）
%%%   5. imboy_plugin_generic_sup 单独可启动（独立注册名）
%%%
%%% 测试隔离 / Isolation:
%%%   - 每个 fixture setup 启动 imboy_plugin_sup（注册全局 ?MODULE 名）
%%%   - cleanup 时 exit(shutdown) 级联终止全部子 sup
%%%   - 测试间串行（多个 sup 共享 local 注册名，并发会冲突）
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% Test helpers
%% ===================================================================

setup() ->
    {ok, Pid} = imboy_plugin_sup:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            %% 先 unlink，防止 shutdown EXIT 信号经 link 传播到 test process 导致
            %% eunit "unexpected termination of test process::shutdown"
            unlink(Pid),
            exit(Pid, shutdown),
            wait_for_dead(Pid, 50);
        false ->
            ok
    end.

wait_for_dead(_Pid, 0) ->
    timeout;
wait_for_dead(Pid, N) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(20),
            wait_for_dead(Pid, N - 1)
    end.

%% ===================================================================
%% 1. imboy_plugin_sup 启动后 router_registry + ws_action_registry + 4 个 plugin sup 全部注册
%% ===================================================================

sup_starts_with_4_plugin_sups_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(Pid) ->
        Children = supervisor:which_children(Pid),
        [
            %% 6 个子进程：router_registry + ws_action_registry（WS 路由查表前置）+ 4 plugin sup
            ?_assertEqual(6, length(Children)),
            ?_assert(lists:keymember(imboy_router_registry, 1, Children)),
            ?_assert(lists:keymember(imboy_ws_action_registry, 1, Children)),
            ?_assert(lists:keymember(channel_sup, 1, Children)),
            ?_assert(lists:keymember(moment_sup, 1, Children)),
            ?_assert(lists:keymember(location_sup, 1, Children)),
            ?_assert(lists:keymember(group_collab_sup, 1, Children))
        ]
    end}.

%% ===================================================================
%% 2. 每个 plugin sup 是真 supervisor 进程（local 注册名可达）
%% ===================================================================

each_plugin_sup_registered_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_assert(is_pid(whereis(channel_sup))),
            ?_assert(is_pid(whereis(moment_sup))),
            ?_assert(is_pid(whereis(location_sup))),
            ?_assert(is_pid(whereis(group_collab_sup)))
        ]
    end}.

%% ===================================================================
%% 3. 每个 plugin sup 当前无 children worker（Phase 1 切片 1 骨架）
%% ===================================================================

each_plugin_sup_has_empty_children_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        [
            ?_assertEqual([], supervisor:which_children(channel_sup)),
            ?_assertEqual([], supervisor:which_children(moment_sup)),
            ?_assertEqual([], supervisor:which_children(location_sup)),
            ?_assertEqual([], supervisor:which_children(group_collab_sup))
        ]
    end}.

%% ===================================================================
%% 4. Isolation：杀 channel_sup 后 moment/location/group_collab 不受影响（one_for_one）
%% ===================================================================

isolation_kill_channel_does_not_affect_others_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_Pid) ->
        ChannelPid = whereis(channel_sup),
        MomentPid = whereis(moment_sup),
        LocationPid = whereis(location_sup),
        GroupPid = whereis(group_collab_sup),
        %% 杀 channel_sup
        exit(ChannelPid, kill),
        %% 等待 imboy_plugin_sup 处理 EXIT 并重启
        timer:sleep(100),
        NewChannelPid = whereis(channel_sup),
        NewMomentPid = whereis(moment_sup),
        NewLocationPid = whereis(location_sup),
        NewGroupPid = whereis(group_collab_sup),
        [
            %% channel_sup 应被重启（permanent restart）
            ?_assert(is_pid(NewChannelPid)),
            ?_assertNotEqual(ChannelPid, NewChannelPid),
            %% moment/location/group_collab sup 必须保持原 pid（one_for_one 隔离）
            ?_assertEqual(MomentPid, NewMomentPid),
            ?_assertEqual(LocationPid, NewLocationPid),
            ?_assertEqual(GroupPid, NewGroupPid)
        ]
    end}.

%% ===================================================================
%% 5. imboy_plugin_generic_sup 模块单独可启动（独立注册名）
%% ===================================================================

generic_sup_can_register_arbitrary_name_test_() ->
    ?TEST_SIMPLE(fun() ->
        TestName = my_test_plugin_sup,
        {ok, Pid} = imboy_plugin_generic_sup:start_link(TestName),
        ?assert(is_pid(Pid)),
        ?assertEqual(Pid, whereis(TestName)),
        ?assertEqual([], supervisor:which_children(Pid)),
        %% 必须 unlink 才能 exit shutdown 不传播 EXIT 给 test process
        unlink(Pid),
        exit(Pid, shutdown)
    end).
