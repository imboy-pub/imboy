-module(imboy_plugin_lifecycle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_plugin_lifecycle gen_statem 纯状态机测试
%%%
%%% 覆盖 lifecycle.md §4 状态转换图所有边（约 25 条）。
%%% 切片 1：不调用任何外部组件，仅验证状态转换逻辑。
%%%===================================================================

%% ── helpers ──

-define(PLUGIN, test_plugin).

start_lifecycle() ->
    start_lifecycle(#{}).
start_lifecycle(ExtraOpts) ->
    Name = ?PLUGIN,
    Opts = maps:merge(#{name => Name}, ExtraOpts),
    {ok, Pid} = imboy_plugin_lifecycle:start_link(Opts),
    Pid.

stop_lifecycle(Pid) ->
    gen_statem:stop(Pid).

state_of(Pid) ->
    gen_statem:call(Pid, get_state).

%% ── 初始状态 ──

initial_state_is_unknown_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ?assertEqual(unknown, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

%% ── install 流程 ──

install_transitions_to_installed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"priv/plugins/test_plugin">>}),
        ?assertEqual(installed, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

install_from_non_unknown_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertEqual(installed, state_of(Pid)),
        Result = gen_statem:call(Pid, {install, <<"path2">>}),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

%% ── enable 流程 ──

enable_from_installed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

enable_from_disabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),
        ?assertEqual(disabled, state_of(Pid)),
        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

enable_from_unknown_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        Result = gen_statem:call(Pid, enable),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

enable_when_already_enabled_is_idempotent_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        Result = gen_statem:call(Pid, enable),
        ?assertEqual(ok, Result),
        ?assertEqual(enabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

%% ── disable 流程 ──

disable_from_enabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),
        ?assertEqual(disabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

disable_from_installed_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        Result = gen_statem:call(Pid, disable),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

%% ── upgrade 流程 ──

upgrade_from_enabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertEqual(enabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

upgrade_from_installed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertEqual(installed, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

upgrade_from_disabled_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),
        Result = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

%% ── uninstall 流程 ──

uninstall_from_installed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        Result = gen_statem:call(Pid, {uninstall, preserve_data}),
        ?assertEqual(ok, Result),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid))
    end).

uninstall_from_disabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),
        Result = gen_statem:call(Pid, {uninstall, drop_data}),
        ?assertEqual(ok, Result),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid))
    end).

uninstall_from_enabled_auto_disables_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        Result = gen_statem:call(Pid, {uninstall, preserve_data}),
        ?assertEqual(ok, Result),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid))
    end).

uninstall_from_unknown_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        Result = gen_statem:call(Pid, {uninstall, preserve_data}),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

%% ── cancel ──

cancel_from_stable_state_rejected_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        Result = gen_statem:call(Pid, cancel),
        ?assertMatch({error, invalid_state_transition}, Result),
        stop_lifecycle(Pid)
    end).

%% ── health_check ──

health_check_from_enabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        Result = gen_statem:call(Pid, health_check),
        ?assertMatch({ok, #{state := enabled}}, Result),
        ?assertEqual(enabled, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

health_check_from_installed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        Result = gen_statem:call(Pid, health_check),
        ?assertMatch({ok, #{state := installed}}, Result),
        ?assertEqual(installed, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

health_check_from_disabled_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),
        Result = gen_statem:call(Pid, health_check),
        ?assertMatch({ok, #{state := disabled}}, Result),
        stop_lifecycle(Pid)
    end).

%% ── failed 状态 ──

reset_from_failed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        gen_statem:cast(Pid, {inject_failure, test_error}),
        timer:sleep(50),
        ?assertEqual(failed, state_of(Pid)),
        ok = gen_statem:call(Pid, reset),
        ?assertEqual(unknown, state_of(Pid)),
        stop_lifecycle(Pid)
    end).

force_uninstall_from_failed_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        gen_statem:cast(Pid, {inject_failure, test_error}),
        timer:sleep(50),
        ?assertEqual(failed, state_of(Pid)),
        Result = gen_statem:call(Pid, {force_uninstall, preserve_data}),
        ?assertEqual(ok, Result),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid))
    end).

%% ── full happy path ──

full_happy_path_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ?assertEqual(unknown, state_of(Pid)),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertEqual(installed, state_of(Pid)),

        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),

        ok = gen_statem:call(Pid, disable),
        ?assertEqual(disabled, state_of(Pid)),

        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),

        ok = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertEqual(enabled, state_of(Pid)),

        ok = gen_statem:call(Pid, {uninstall, drop_data}),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid))
    end).
