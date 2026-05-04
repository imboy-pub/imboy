-module(imboy_plugin_lifecycle_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% imboy_plugin_lifecycle gen_statem 状态机测试
%%%
%%% 覆盖 lifecycle.md §4 状态转换图所有边（约 25 条）。
%%% 切片 1：状态转换逻辑。
%%% 切片 2：组件接线（meck mock 验证调用序列）。
%%%===================================================================

%% ── helpers ──

-define(PLUGIN, test_plugin).

-define(MANIFEST, #{
    name => ?PLUGIN,
    version => <<"1.0.0">>,
    routes => []
}).

mock_components() ->
    mock_components(#{}).

mock_components(Overrides) ->
    Manifest = maps:get(manifest, Overrides, ?MANIFEST),
    Mods = [
        {imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> ok end}
        ]},
        {imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, Manifest} end}
        ]},
        {imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1, fun(_) -> ok end},
            {find_dependents, 1, fun(_) -> ok end}
        ]},
        {imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]},
        {imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]}
    ],
    lists:foreach(fun({Mod, Exps}) ->
        meck_helper:cleanup_mock(Mod),
        {ok, _} = meck_helper:setup_mock(Mod, Exps)
    end, Mods).

unmock_components() ->
    lists:foreach(fun(Mod) ->
        _ = catch meck_helper:cleanup_mock(Mod)
    end, [
        imboy_plugin_signature,
        imboy_plugin_toml,
        imboy_plugin_dependency,
        imboy_plugin_loader,
        imboy_router_registry
    ]).

start_lifecycle() ->
    start_lifecycle(#{}).

start_lifecycle(ExtraOpts) ->
    mock_components(ExtraOpts),
    Opts = maps:merge(#{name => ?PLUGIN}, ExtraOpts),
    {ok, Pid} = imboy_plugin_lifecycle:start_link(Opts),
    Pid.

stop_lifecycle(Pid) ->
    _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
    unmock_components(),
    catch gen_statem:stop(Pid).

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
        ?assertNot(is_process_alive(Pid)),
        unmock_components()
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
        ?assertNot(is_process_alive(Pid)),
        unmock_components()
    end).

uninstall_from_enabled_auto_disables_test_() ->
    ?_test(begin
        Pid = start_lifecycle(),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        Result = gen_statem:call(Pid, {uninstall, preserve_data}),
        ?assertEqual(ok, Result),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid)),
        unmock_components()
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
        ?assertNot(is_process_alive(Pid)),
        unmock_components()
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
        ?assertNot(is_process_alive(Pid)),
        unmock_components()
    end).

%%%===================================================================
%%% S2: Component wiring tests — 验证组件调用序列
%%%===================================================================

install_calls_all_components_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        ok = gen_statem:call(Pid, {install, <<"path/to/plugin">>}),
        ?assertEqual(installed, state_of(Pid)),

        meck_helper:verify_called_once(imboy_plugin_signature, verify_file, 2),
        meck_helper:verify_called_once(imboy_plugin_toml, load, 1),
        meck_helper:verify_called_once(imboy_plugin_dependency,
                                        validate_constraints, 1),
        meck_helper:verify_called_once(imboy_plugin_loader, scan, 0),

        gen_statem:stop(Pid),
        unmock_components()
    end).

install_fails_on_bad_signature_test_() ->
    ?_test(begin
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> {error, signature_mismatch} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, ?MANIFEST} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1, fun(_) -> ok end},
            {find_dependents, 1, fun(_) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        Result = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertMatch({error, {verify_signature, signature_mismatch}}, Result),
        %% S3: atomic rollback succeeds (no side effects) → back to unknown
        ?assertEqual(unknown, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

install_fails_on_bad_manifest_test_() ->
    ?_test(begin
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {error, parse_error} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
        {check_enable_deps, 1, fun(_) -> ok end},
        {find_dependents, 1, fun(_) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        Result = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertMatch({error, {parse_manifest, parse_error}}, Result),
        %% S3: atomic rollback succeeds (no side effects) → back to unknown
        ?assertEqual(unknown, state_of(Pid)),

        %% toml failed, dependency should NOT have been called
        ?assertEqual(0, meck:num_calls(imboy_plugin_dependency,
                                        validate_constraints, 1)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

install_fails_on_dependency_error_test_() ->
    ?_test(begin
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, ?MANIFEST} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1,
             fun(_) -> {error, {missing_dep, foo, <<"^1.0">>}} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        Result = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertMatch({error, {validate_dependencies,
                              {missing_dep, foo, <<"^1.0">>}}}, Result),
        %% S3: atomic rollback succeeds (no side effects) → back to unknown
        ?assertEqual(unknown, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

enable_registers_routes_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{routes => [{<<"/api/test">>, test_handler, []}]},
        mock_components(#{manifest => Manifest}),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        meck_helper:verify_called_once(imboy_router_registry, register, 2),
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

enable_no_routes_skips_register_test_() ->
    ?_test(begin
        mock_components(#{manifest => ?MANIFEST}),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        ?assertEqual(0, meck:num_calls(imboy_router_registry, register, 2)),
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

disable_unregisters_routes_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),

        meck_helper:verify_called_once(imboy_router_registry, unregister, 1),
        ?assertEqual(disabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

upgrade_revalidates_dependencies_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        %% Reset call counts before upgrade
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
        {check_enable_deps, 1, fun(_) -> ok end},
        {find_dependents, 1, fun(_) -> ok end}
        ]),

        ok = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        meck_helper:verify_called_once(imboy_plugin_dependency,
                                        validate_constraints, 1),
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

upgrade_fails_on_dep_break_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        %% Make dependency check fail for upgrade
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1,
             fun(_) -> {error, {version_mismatch, bar, <<"^2.0">>}} end}
        ]),

        Result = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertMatch({error, {validate_dependencies, _}}, Result),
        %% S3: atomic rollback succeeds (no side effects) → back to enabled
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

uninstall_cleans_up_persistent_term_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        %% persistent_term should be set
        ?assertMatch(#{name := ?PLUGIN},
                     persistent_term:get({imboy_plugin_manifest, ?PLUGIN})),

        ok = gen_statem:call(Pid, {uninstall, preserve_data}),
        timer:sleep(50),
        ?assertNot(is_process_alive(Pid)),

        %% persistent_term should be erased (key no longer exists)
        ?assertMatch({'EXIT', _},
                     catch persistent_term:get({imboy_plugin_manifest, ?PLUGIN})),
        unmock_components()
    end).

reset_clears_state_data_test_() ->
    ?_test(begin
        mock_components(),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        gen_statem:cast(Pid, {inject_failure, test_error}),
        timer:sleep(50),
        ?assertEqual(failed, state_of(Pid)),

        ok = gen_statem:call(Pid, reset),
        ?assertEqual(unknown, state_of(Pid)),

        %% Can re-install after reset
        ok = gen_statem:call(Pid, {install, <<"path2">>}),
        ?assertEqual(installed, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

%%%===================================================================
%%% S3: Rollback tests — 回滚策略验证
%%%
%%% 注意：rollback_strategy 在 install 成功后才写入 Data。
%%% 因此测试不同策略需要：先成功 install → 再在后续操作触发失败。
%%%===================================================================

%% ── Install early failure: atomic rollback with no undo (no side effects) ──

install_early_fail_atomic_no_side_effects_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{lifecycle => #{rollback_strategy => atomic}},
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> {error, bad_sig} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, Manifest} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
        {check_enable_deps, 1, fun(_) -> ok end},
        {find_dependents, 1, fun(_) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        Result = gen_statem:call(Pid, {install, <<"path">>}),

        ?assertMatch({error, {verify_signature, bad_sig}}, Result),
        %% Atomic rollback: no side effects → back to unknown
        ?assertEqual(unknown, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

%% ── Enable fails: atomic rollback undoes registered routes ──
%% Strategy is read from manifest during install, applied on enable failure.

enable_fails_manual_strategy_goes_to_failed_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{
            routes => [{<<"/api/test">>, test_handler, []}],
            lifecycle => #{rollback_strategy => manual}
        },
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, Manifest} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
        {check_enable_deps, 1, fun(_) -> ok end},
        {find_dependents, 1, fun(_) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        %% Register succeeds first, then we check manual strategy
        CallCount = atomics:new(1, [{signed, false}]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) ->
                atomics:add(CallCount, 1, 1),
                ok
            end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        %% Install succeeds → strategy is manual
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertEqual(installed, state_of(Pid)),

        %% Now make register fail for enable
        meck_helper:cleanup_mock(imboy_router_registry),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> {error, route_conflict} end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        Result = gen_statem:call(Pid, enable),
        ?assertMatch({error, {register_routes, route_conflict}}, Result),

        %% Manual strategy: no undo → failed
        ?assertEqual(failed, state_of(Pid)),

        %% unregister should NOT have been called (manual skips undo)
        ?assertEqual(0, meck:num_calls(imboy_router_registry, unregister, 1)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).

%% ── Enable fails with atomic: register succeeds, next step fails → undo ──
%% Current enable only has register_routes step. If it succeeds, enable succeeds.
%% So this tests the case where register itself fails (no undo recorded).

enable_register_fails_atomic_back_to_installed_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{
            routes => [{<<"/api/test">>, test_handler, []}],
            lifecycle => #{rollback_strategy => atomic}
        },
        unmock_components(),
        meck_helper:setup_mock(imboy_plugin_signature, [
            {verify_file, 2, fun(_, _) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_toml, [
            {load, 1, fun(_) -> {ok, Manifest} end}
        ]),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
        {check_enable_deps, 1, fun(_) -> ok end},
        {find_dependents, 1, fun(_) -> ok end}
        ]),
        meck_helper:setup_mock(imboy_plugin_loader, [
            {scan, 0, fun() -> ok end}
        ]),
        meck_helper:setup_mock(imboy_router_registry, [
            {register, 2, fun(_, _) -> {error, route_conflict} end},
            {unregister, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        ok = gen_statem:call(Pid, {install, <<"path">>}),

        Result = gen_statem:call(Pid, enable),
        ?assertMatch({error, {register_routes, route_conflict}}, Result),

        %% Atomic: register failed before add_undo → no undo needed → back to installed
        ?assertEqual(installed, state_of(Pid)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).

%% ── Upgrade fails with best_effort → always goes to failed ──

upgrade_fails_best_effort_to_failed_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{lifecycle => #{rollback_strategy => best_effort}},
        mock_components(#{manifest => Manifest}),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),

        %% Make dependency check fail for upgrade
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1,
             fun(_) -> {error, {version_mismatch, bar, <<"^2.0">>}} end}
        ]),

        Result = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertMatch({error, {validate_dependencies, _}}, Result),

        %% best_effort: always goes to failed
        ?assertEqual(failed, state_of(Pid)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).

%% ── Default strategy is atomic ──

default_strategy_is_atomic_test_() ->
    ?_test(begin
        %% Manifest without lifecycle field → defaults to atomic
        mock_components(#{manifest => ?MANIFEST}),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ?assertEqual(installed, state_of(Pid)),

        ok = gen_statem:call(Pid, enable),
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        unmock_components()
    end).

%% ── Atomic rollback on upgrade: no side effects → back to enabled ──

upgrade_fails_atomic_no_side_effects_back_to_enabled_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{lifecycle => #{rollback_strategy => atomic}},
        mock_components(#{manifest => Manifest}),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        %% Make dep fail for upgrade
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1,
             fun(_) -> {error, dep_broken} end}
        ]),

        Result = gen_statem:call(Pid, {upgrade, <<"2.0.0">>}),
        ?assertMatch({error, {validate_dependencies, dep_broken}}, Result),

        %% Atomic: no side effects in upgrade steps → back to enabled
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).

%%%===================================================================
%%% S4: Audit log tests — 审计日志集成验证
%%%===================================================================

audit_writes_on_install_success_test_() ->
    ?_test(begin
        mock_components(),
        meck_helper:setup_mock(imboy_plugin_audit_ds, [
            {write, 1, fun(_) -> ok end}
        ]),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),

        %% Audit write should have been called at least once (install success)
        timer:sleep(50),
        ?assert(meck:num_calls(imboy_plugin_audit_ds, write, 1) >= 1),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        meck_helper:cleanup_mock(imboy_plugin_audit_ds),
        unmock_components()
    end).

audit_writes_on_full_lifecycle_test_() ->
    ?_test(begin
        mock_components(),
        meck_helper:setup_mock(imboy_plugin_audit_ds, [
            {write, 1, fun(_) -> ok end}
        ]),
        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),

        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),
        ok = gen_statem:call(Pid, disable),

        timer:sleep(50),
        %% At least 3 audit calls: install, enable, disable
        ?assert(meck:num_calls(imboy_plugin_audit_ds, write, 1) >= 3),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        meck_helper:cleanup_mock(imboy_plugin_audit_ds),
        unmock_components()
    end).

%%%===================================================================
%%% S4: Cascade tests — 依赖联动验证
%%%===================================================================

enable_fails_when_deps_not_enabled_test_() ->
    ?_test(begin
        Manifest = ?MANIFEST#{depends_on => #{other_plugin => <<"^1.0">>}},
        mock_components(#{manifest => Manifest}),
        %% Override check_enable_deps to return error
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1,
             fun(_) -> {error, {deps_not_enabled, [other_plugin]}} end},
            {find_dependents, 1, fun(_) -> ok end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        ok = gen_statem:call(Pid, {install, <<"path">>}),

        Result = gen_statem:call(Pid, enable),
        ?assertMatch({error, {check_enable_deps,
                              {deps_not_enabled, [other_plugin]}}}, Result),
        ?assertEqual(installed, state_of(Pid)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).

disable_fails_when_has_dependents_test_() ->
    ?_test(begin
        mock_components(),
        %% Override find_dependents to return error
        meck_helper:cleanup_mock(imboy_plugin_dependency),
        meck_helper:setup_mock(imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1, fun(_) -> ok end},
            {find_dependents, 1,
             fun(_) -> {error, {has_dependents, [dependent_plugin]}} end}
        ]),

        {ok, Pid} = imboy_plugin_lifecycle:start_link(#{name => ?PLUGIN}),
        ok = gen_statem:call(Pid, {install, <<"path">>}),
        ok = gen_statem:call(Pid, enable),

        Result = gen_statem:call(Pid, disable),
        ?assertMatch({error, {has_dependents, [dependent_plugin]}}, Result),
        %% State should remain enabled (cascade check rejected disable)
        ?assertEqual(enabled, state_of(Pid)),

        gen_statem:stop(Pid),
        _ = catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
        unmock_components()
    end).
