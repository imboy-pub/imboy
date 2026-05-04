-module(imboy_plugin_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-define(PLUGIN, test_plugin).

%% ===================================================================
%% Test fixture — 复用 lifecycle 测试的 mock_components 模式
%% ===================================================================

setup() ->
    application:set_env(imboy, env, test),
    Mods = [
        {imboy_plugin_signature, [{verify_file, 2, fun(_, _) -> ok end}]},
        {imboy_plugin_toml, [{load, 1, fun(_) ->
            {ok, #{name => ?PLUGIN, version => <<"1.0.0">>,
                   depends_on => #{}, routes => []}}
        end}]},
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
    ],
    lists:foreach(fun({Mod, Exps}) ->
        meck_helper:cleanup_mock(Mod),
        {ok, _} = meck_helper:setup_mock(Mod, Exps)
    end, Mods),
    ok.

cleanup(_) ->
    case imboy_plugin_manager:find_lifecycle(?PLUGIN) of
        undefined -> ok;
        Pid -> catch gen_statem:stop(Pid)
    end,
    catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN}),
    catch persistent_term:erase({imboy_plugin_lifecycle, ?PLUGIN}),
    lists:foreach(fun(M) -> catch meck_helper:cleanup_mock(M) end, [
        imboy_plugin_signature, imboy_plugin_toml, imboy_plugin_dependency,
        imboy_plugin_loader, imboy_router_registry
    ]),
    ok.

%% ===================================================================
%% Tests
%% ===================================================================

find_lifecycle_unknown_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        ?_assertEqual(undefined, imboy_plugin_manager:find_lifecycle(?PLUGIN))
     end}.

list_plugins_empty_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, Items} = imboy_plugin_manager:list_plugins(),
            ?assertEqual([], Items)
        end
     end}.

install_success_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            Result = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            ?assertMatch({ok, #{name := ?PLUGIN}}, Result),
            Pid = imboy_plugin_manager:find_lifecycle(?PLUGIN),
            ?assert(is_pid(Pid)),
            ?assert(persistent_term:get({imboy_plugin_manifest, ?PLUGIN}, undefined) =/= undefined)
        end
     end}.

install_duplicate_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            ?assertMatch({error, {invalid_state_transition, installed}}, Result)
        end
     end}.

enable_success_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:enable(?PLUGIN),
            ?assertEqual(ok, Result),
            {ok, #{state := State}} = imboy_plugin_manager:get_state(?PLUGIN),
            ?assertEqual(enabled, State)
        end
     end}.

disable_success_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        ?_assertEqual({error, not_found}, imboy_plugin_manager:enable(nonexistent))
     end}.

get_state_installed_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            {ok, #{name := Name, state := State}} = imboy_plugin_manager:get_state(?PLUGIN),
            ?assertEqual(?PLUGIN, Name),
            ?assertEqual(installed, State)
        end
     end}.

health_check_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            {ok, #{name := Name}} = imboy_plugin_manager:health_check(?PLUGIN),
            ?assertEqual(?PLUGIN, Name)
        end
     end}.

reset_from_failed_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
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
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN, <<"/tmp/test_plugin">>),
            Result = imboy_plugin_manager:upgrade(?PLUGIN, <<"2.0.0">>),
            ?assertEqual(ok, Result)
        end
     end}.

get_plugin_not_found_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) ->
        ?_assertEqual({error, not_found}, imboy_plugin_manager:get_plugin(nonexistent))
     end}.
