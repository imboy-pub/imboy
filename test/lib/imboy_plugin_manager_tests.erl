-module(imboy_plugin_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PLUGIN, test_plugin).

%% ===================================================================
%% Test fixture — 复用 lifecycle 测试的 mock_components 模式
%% ===================================================================

setup() ->
    application:set_env(imboy, env, test),
    setup_mocks([path_mocks() | base_mocks()]).

%% SEC-02 真实路径校验组：不 mock imboy_plugin_path
setup_real_path() ->
    application:set_env(imboy, env, test),
    setup_mocks(base_mocks()).

base_mocks() ->
    [
        {imboy_plugin_signature, [{verify_file, 2, fun(_, _) -> ok end}]},
        {imboy_plugin_toml, [
            {load, 1, fun(_) ->
                {ok, #{
                    name => ?PLUGIN,
                    version => <<"1.0.0">>,
                    depends_on => #{},
                    routes => []
                }}
            end}
        ]},
        {imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1, fun(_) -> ok end},
            {find_dependents, 1, fun(_) -> ok end}
        ]},
        {imboy_plugin_loader, [{scan, 0, fun() -> ok end}]},
        {imboy_router_registry, [
            {register, 2, fun(_, _) -> ok end},
            {unregister, 1, fun(_) -> ok end}
        ]}
    ].

%% SEC-02：既有用例的 <<"/tmp/test_plugin">> 假路径关注 manager 语义，
%% 路径收口组件 mock 放行；真实负向矩阵见文件尾 install_*_rejected 组
%% 与 imboy_plugin_path_tests。
path_mocks() ->
    {imboy_plugin_path, [
        {resolve, 1, fun(P) -> {ok, P} end},
        {ensure_file_within, 1, fun(_) -> ok end}
    ]}.

setup_mocks(Mods) ->
    lists:foreach(
        fun({Mod, Exps}) ->
            meck_helper:cleanup_mock(Mod),
            {ok, _} = meck_helper:setup_mock(Mod, Exps)
        end,
        Mods
    ),
    %% 套件隔离治理（main 线）：app 常驻后 priv/plugins 的真实插件（channel 等）
    %% 已进入 persistent_term，list_plugins 会把它们一起列出。暂存并清空全部
    %% manifest 键，cleanup 恢复，保证本模块内插件列表受控。
    RealManifests = [
        {K, V}
     || {K, V} <- persistent_term:get(),
        (is_tuple(K) andalso tuple_size(K) =:= 2 andalso
            element(1, K) =:= imboy_plugin_manifest)
    ],
    lists:foreach(fun({K, _}) -> persistent_term:erase(K) end, RealManifests),
    put(saved_manifests, RealManifests),
    ok.

cleanup(_) ->
    case imboy_plugin_manager:find_lifecycle(?PLUGIN) of
        undefined -> ok;
        Pid -> catch gen_statem:stop(Pid)
    end,
    catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
    catch persistent_term:erase({imboy_plugin_lifecycle, ?PLUGIN}),
    case get(saved_manifests) of
        undefined ->
            ok;
        Saved ->
            lists:foreach(fun({K, V}) -> persistent_term:put(K, V) end, Saved),
            erase(saved_manifests)
    end,
    lists:foreach(fun(M) -> catch meck_helper:cleanup_mock(M) end, [
        imboy_plugin_path,
        imboy_plugin_signature,
        imboy_plugin_toml,
        imboy_plugin_dependency,
        imboy_plugin_loader,
        imboy_router_registry
    ]),
    ok.

%% ===================================================================
%% Tests
%% ===================================================================

find_lifecycle_unknown_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        ?_assertEqual(undefined, imboy_plugin_manager:find_lifecycle(?PLUGIN))
    end}.

list_plugins_empty_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, Items} = imboy_plugin_manager:list_plugins(),
            ?assertEqual([], Items)
        end
    end}.

install_success_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            Result = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            ?assertMatch({ok, #{name := ?PLUGIN}}, Result),
            Pid = imboy_plugin_manager:find_lifecycle(?PLUGIN),
            ?assert(is_pid(Pid)),
            ?assert(persistent_term:get({imboy_plugin_manifest, ?PLUGIN}, undefined) =/= undefined)
        end
    end}.

install_duplicate_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            ?assertMatch({error, {invalid_state_transition, installed}}, Result)
        end
    end}.

enable_success_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:enable(?PLUGIN),
            ?assertEqual(ok, Result),
            {ok, #{state := State}} = imboy_plugin_manager:get_state(?PLUGIN),
            ?assertEqual(enabled, State)
        end
    end}.

disable_success_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            ok = imboy_plugin_manager:enable(?PLUGIN),
            Result = imboy_plugin_manager:disable(?PLUGIN),
            ?assertEqual(ok, Result),
            {ok, #{state := State}} = imboy_plugin_manager:get_state(?PLUGIN),
            ?assertEqual(disabled, State)
        end
    end}.

enable_not_found_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        ?_assertEqual({error, not_found}, imboy_plugin_manager:enable(nonexistent))
    end}.

get_state_installed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            {ok, #{name := Name, state := State}} = imboy_plugin_manager:get_state(?PLUGIN),
            ?assertEqual(?PLUGIN, Name),
            ?assertEqual(installed, State)
        end
    end}.

health_check_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            {ok, #{name := Name}} = imboy_plugin_manager:health_check(?PLUGIN),
            ?assertEqual(?PLUGIN, Name)
        end
    end}.

reset_from_failed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Pid = imboy_plugin_manager:find_lifecycle(?PLUGIN),
            gen_statem:cast(Pid, {inject_failure, test_error}),
            timer:sleep(50),
            {ok, #{state := failed}} = imboy_plugin_manager:get_state(?PLUGIN),
            Result = imboy_plugin_manager:reset(?PLUGIN),
            ?assertEqual(ok, Result),
            {ok, #{state := unknown}} = imboy_plugin_manager:get_state(?PLUGIN)
        end
    end}.

list_plugins_with_one_installed_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            {ok, Items} = imboy_plugin_manager:list_plugins(),
            ?assertEqual(1, length(Items)),
            [Item] = Items,
            ?assertEqual(?PLUGIN, maps:get(name, Item)),
            ?assertEqual(<<"1.0.0">>, maps:get(version, Item)),
            ?assertEqual(installed, maps:get(state, Item))
        end
    end}.

upgrade_success_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:upgrade(?PLUGIN, <<"2.0.0">>),
            ?assertEqual(ok, Result)
        end
    end}.

get_plugin_not_found_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        ?_assertEqual({error, not_found}, imboy_plugin_manager:get_plugin(nonexistent))
    end}.

%% ===================================================================
%% SEC-02: install Path 入口层收口（真实 imboy_plugin_path 校验）
%% ===================================================================

install_outside_root_rejected_test_() ->
    {setup, fun setup_real_path/0, fun cleanup/1, fun(_) ->
        fun() ->
            %% 默认 plugin_root = priv/plugins（cwd 相对）；/tmp 在根外 → 拒，
            %% 且不启动 lifecycle statem（无副作用）
            Result = imboy_plugin_manager:install(?PLUGIN, <<"/tmp">>),
            ?assertMatch({error, {resolve_path, _}}, Result),
            ?assertEqual(undefined, imboy_plugin_manager:find_lifecycle(?PLUGIN)),
            ?assertEqual(
                undefined,
                persistent_term:get({imboy_plugin_manifest, ?PLUGIN}, undefined)
            )
        end
    end}.

install_dotdot_rejected_test_() ->
    {setup, fun setup_real_path/0, fun cleanup/1, fun(_) ->
        fun() ->
            Result = imboy_plugin_manager:install(?PLUGIN, <<"priv/plugins/../../etc">>),
            ?assertMatch({error, {resolve_path, _}}, Result),
            ?assertEqual(undefined, imboy_plugin_manager:find_lifecycle(?PLUGIN))
        end
    end}.

%% 生命周期写操作总开关默认禁用回归（A-28，防止收口改动破坏现有门控）
lifecycle_enabled_default_false_test_() ->
    {setup,
        fun() ->
            Saved = application:get_env(imboy, plugin_lifecycle_enabled),
            Saved
        end,
        fun(Saved) ->
            case Saved of
                undefined -> application:unset_env(imboy, plugin_lifecycle_enabled);
                {ok, V} -> application:set_env(imboy, plugin_lifecycle_enabled, V)
            end
        end,
        fun(Saved) ->
            fun() ->
                case Saved of
                    {ok, _} -> ok;
                    _ -> ?assertEqual(false, imboy_plugin_manager:lifecycle_enabled())
                end,
                application:set_env(imboy, plugin_lifecycle_enabled, true),
                ?assertEqual(true, imboy_plugin_manager:lifecycle_enabled()),
                application:set_env(imboy, plugin_lifecycle_enabled, false),
                ?assertEqual(false, imboy_plugin_manager:lifecycle_enabled())
            end
        end}.
