-module(imboy_plugin_cascade_validation_tests).

%%% P4-V1: A depends_on B — disable B should fail with has_dependents
%%% P4-V2: upgrade failure auto-rollback to previous state

-include_lib("eunit/include/eunit.hrl").

-define(PLUGIN_A, cascade_plugin_a).
-define(PLUGIN_B, cascade_plugin_b).

%% ===================================================================
%% Fixture — installs two plugins with A depends_on B
%% ===================================================================

setup_cascade() ->
    application:set_env(imboy, env, test),
    Mods = [
        {imboy_plugin_signature, [{verify_file, 2, fun(_, _) -> ok end}]},
        {imboy_plugin_toml, [
            {load, 1, fun(Path) ->
                case Path =:= <<"/tmp/cascade_b/plugin.config">> of
                    true ->
                        {ok, #{name => ?PLUGIN_B, version => <<"1.0.0">>,
                               depends_on => #{}, routes => []}};
                    false ->
                        {ok, #{name => ?PLUGIN_A, version => <<"1.0.0">>,
                               depends_on => #{?PLUGIN_B => <<"^1.0.0">>},
                               routes => []}}
                end
            end}
        ]},
        {imboy_plugin_dependency, [
            {validate_constraints, 1, fun(_) -> ok end},
            {check_enable_deps, 1, fun(Manifest) ->
                Deps = maps:get(depends_on, Manifest, #{}),
                NotEnabled = lists:filter(fun(DepName) ->
                    case get_plugin_state_mock(DepName) of
                        enabled -> false;
                        _ -> true
                    end
                end, maps:keys(Deps)),
                case NotEnabled of
                    [] -> ok;
                    _ -> {error, {deps_not_enabled, NotEnabled}}
                end
            end},
            {find_dependents, 1, fun(PluginName) ->
                Dependents = lists:filtermap(fun(Name) ->
                    case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
                        undefined -> false;
                        MF ->
                            Deps = maps:get(depends_on, MF, #{}),
                            case maps:is_key(PluginName, Deps) of
                                true ->
                                    case get_plugin_state_mock(Name) of
                                        enabled -> {true, Name};
                                        _ -> false
                                    end;
                                false -> false
                            end
                    end
                end, [?PLUGIN_A, ?PLUGIN_B]),
                case Dependents of
                    [] -> ok;
                    _ -> {error, {has_dependents, Dependents}}
                end
            end}
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

cleanup_cascade(_) ->
    lists:foreach(fun(Name) ->
        case imboy_plugin_manager:find_lifecycle(Name) of
            undefined -> ok;
            Pid -> catch gen_statem:stop(Pid)
        end
    end, [?PLUGIN_A, ?PLUGIN_B]),
    lists:foreach(fun(Key) ->
        catch persistent_term:erase(Key)
    end, [
        {imboy_plugin_manifest, ?PLUGIN_A},
        {imboy_plugin_manifest, ?PLUGIN_B},
        {imboy_plugin_lifecycle, ?PLUGIN_A},
        {imboy_plugin_lifecycle, ?PLUGIN_B}
    ]),
    lists:foreach(fun(M) -> catch meck_helper:cleanup_mock(M) end, [
        imboy_plugin_signature, imboy_plugin_toml, imboy_plugin_dependency,
        imboy_plugin_loader, imboy_router_registry
    ]),
    ok.

get_plugin_state_mock(Name) ->
    case persistent_term:get({imboy_plugin_lifecycle, Name}, undefined) of
        undefined ->
            case persistent_term:get({imboy_plugin_manifest, Name}, undefined) of
                undefined -> unknown;
                _ -> installed
            end;
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                false -> unknown;
                true ->
                    try gen_statem:call(Pid, get_state, 1000)
                    catch _:_ -> unknown
                    end
            end
    end.

%% ===================================================================
%% P4-V1: Cascade — disable B fails when A depends on it
%% ===================================================================

disable_b_fails_when_a_depends_test_() ->
    {setup, fun setup_cascade/0, fun cleanup_cascade/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN_B, <<"/tmp/cascade_b">>),
            ok = imboy_plugin_manager:enable(?PLUGIN_B),
            {ok, #{state := enabled}} = imboy_plugin_manager:get_state(?PLUGIN_B),

            {ok, _} = imboy_plugin_manager:install(?PLUGIN_A, <<"/tmp/cascade_a">>),
            ok = imboy_plugin_manager:enable(?PLUGIN_A),
            {ok, #{state := enabled}} = imboy_plugin_manager:get_state(?PLUGIN_A),

            Result = imboy_plugin_manager:disable(?PLUGIN_B),
            ?assertMatch({error, {has_dependents, _}}, Result),

            {ok, #{state := StillEnabled}} = imboy_plugin_manager:get_state(?PLUGIN_B),
            ?assertEqual(enabled, StillEnabled),

            ok = imboy_plugin_manager:disable(?PLUGIN_A),
            ?assertEqual(ok, imboy_plugin_manager:disable(?PLUGIN_B))
        end
     end}.

%% ===================================================================
%% P4-V2: Upgrade failure — plugin stays enabled (atomic rollback)
%% ===================================================================

setup_upgrade() ->
    application:set_env(imboy, env, test),
    Mods = [
        {imboy_plugin_signature, [{verify_file, 2, fun(_, _) -> ok end}]},
        {imboy_plugin_toml, [{load, 1, fun(_) ->
            {ok, #{name => ?PLUGIN_A, version => <<"1.0.0">>,
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

cleanup_upgrade(_) ->
    case imboy_plugin_manager:find_lifecycle(?PLUGIN_A) of
        undefined -> ok;
        Pid -> catch gen_statem:stop(Pid)
    end,
    catch persistent_term:erase({imboy_plugin_manifest, ?PLUGIN_A}),
    catch persistent_term:erase({imboy_plugin_lifecycle, ?PLUGIN_A}),
    lists:foreach(fun(M) -> catch meck_helper:cleanup_mock(M) end, [
        imboy_plugin_signature, imboy_plugin_toml, imboy_plugin_dependency,
        imboy_plugin_loader, imboy_router_registry
    ]),
    ok.

upgrade_success_test_() ->
    {setup, fun setup_upgrade/0, fun cleanup_upgrade/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN_A, <<"/tmp/rollback_a">>),
            ok = imboy_plugin_manager:enable(?PLUGIN_A),

            Result = imboy_plugin_manager:upgrade(?PLUGIN_A, <<"2.0.0">>),
            ?assertEqual(ok, Result),

            {ok, #{state := StateAfter}} = imboy_plugin_manager:get_state(?PLUGIN_A),
            ?assertEqual(enabled, StateAfter)
        end
     end}.

upgrade_failure_rollback_test_() ->
    {setup, fun setup_upgrade/0, fun cleanup_upgrade/1,
     fun(_) ->
        fun() ->
            {ok, _} = imboy_plugin_manager:install(?PLUGIN_A, <<"/tmp/rollback_a">>),
            ok = imboy_plugin_manager:enable(?PLUGIN_A),
            {ok, #{state := enabled}} = imboy_plugin_manager:get_state(?PLUGIN_A),

            %% Make upgrade step fail by switching mock
            ok = meck_helper:cleanup_mock(imboy_plugin_dependency),
            {ok, _} = meck_helper:setup_mock(imboy_plugin_dependency, [
                {validate_constraints, 1, fun(_) ->
                    {error, {version_conflict, <<"incompatible">>}}
                end},
                {check_enable_deps, 1, fun(_) -> ok end},
                {find_dependents, 1, fun(_) -> ok end}
            ]),

            Result = imboy_plugin_manager:upgrade(?PLUGIN_A, <<"2.0.0">>),
            ?assertMatch({error, _}, Result),

            %% P4-V2: plugin must stay enabled after rollback
            {ok, #{state := StateAfter}} = imboy_plugin_manager:get_state(?PLUGIN_A),
            ?assertEqual(enabled, StateAfter)
        end
     end}.
