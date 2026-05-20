-module(imboy_plugin_tests).

-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%-------------------------------------------------------------------
%%% @doc
%%% imboy_plugin behaviour 的 EUnit 测试
%%% EUnit tests for the imboy_plugin behaviour
%%%
%%% 目标 / Goal:
%%%   1. 验证 behaviour 契约的 callback 列表完整、签名正确
%%%   2. 通过 mock plugin (imboy_plugin_dummy) 验证 behaviour 可被合规实现
%%%   3. 验证 contract_version/0 返回值
%%%
%%% Source of truth: docs/plugin/contract.md §4.1
%%% Mock implementation: test/lib/imboy_plugin_dummy.erl
%%% @end
%%%-------------------------------------------------------------------

%% ===================================================================
%% behaviour_info 测试 / behaviour_info tests
%% ===================================================================

callbacks_count_test_() ->
    ?TEST_SIMPLE(fun() ->
        Callbacks = imboy_plugin:behaviour_info(callbacks),
        ?assertEqual(7, length(Callbacks))
    end).

callbacks_signatures_test_() ->
    ?TEST_SIMPLE(fun() ->
        Callbacks = lists:sort(imboy_plugin:behaviour_info(callbacks)),
        Expected = lists:sort([
            {manifest, 0},
            {start, 1},
            {stop, 1},
            {migrate, 3},
            {routes, 0},
            {capabilities, 0},
            {health, 0}
        ]),
        ?assertEqual(Expected, Callbacks)
    end).

%% ===================================================================
%% contract_version/0 测试
%% ===================================================================

contract_version_returns_one_zero_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({1, 0}, imboy_plugin:contract_version())
    end).

contract_version_shape_test_() ->
    ?TEST_SIMPLE(fun() ->
        {Major, Minor} = imboy_plugin:contract_version(),
        ?assert(is_integer(Major)),
        ?assert(Major > 0),
        ?assert(is_integer(Minor)),
        ?assert(Minor >= 0)
    end).

%% ===================================================================
%% Mock plugin 测试 / mock plugin tests
%% ===================================================================

dummy_plugin_module_loads_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertMatch({module, imboy_plugin_dummy},
                     code:ensure_loaded(imboy_plugin_dummy))
    end).

dummy_plugin_declares_behaviour_test_() ->
    ?TEST_SIMPLE(fun() ->
        Attrs = imboy_plugin_dummy:module_info(attributes),
        BehaviourDeclared =
            lists:member({behaviour, [imboy_plugin]}, Attrs)
            orelse lists:member({behavior, [imboy_plugin]}, Attrs),
        ?assert(BehaviourDeclared)
    end).

%% ===================================================================
%% manifest/0 形状测试 / manifest shape tests
%% ===================================================================

dummy_plugin_manifest_required_fields_test_() ->
    ?TEST_SIMPLE(fun() ->
        M = imboy_plugin_dummy:manifest(),
        ?assert(is_map(M)),
        ?assertMatch(
            #{
                name := imboy_plugin_dummy,
                version := <<"0.1.0">>,
                contract_version := {1, 0},
                kind := plugin
            },
            M
        )
    end).

dummy_plugin_manifest_has_industrial_extensions_test_() ->
    %% 验证工业级扩展位字段全部存在（CRITICAL：契约 v1.0 不可缺）
    %% Verify all industrial extension fields are present (CRITICAL: required by contract v1.0)
    ?TEST_SIMPLE(fun() ->
        M = imboy_plugin_dummy:manifest(),
        RequiredKeys = [
            name, version, contract_version, kind, description,
            depends_on, requires_capabilities, min_core_version,
            children, routes, migrations, features,
            limits, budget, degrade, circuit_breaker,
            entries, i18n,
            subscribes, publishes, publishes_meta,
            audit, meta
        ],
        lists:foreach(
            fun(Key) ->
                ?assert(maps:is_key(Key, M),
                        lists:flatten(io_lib:format("缺少字段 / missing key: ~p", [Key])))
            end,
            RequiredKeys
        )
    end).

dummy_plugin_manifest_limits_shape_test_() ->
    ?TEST_SIMPLE(fun() ->
        #{limits := L} = imboy_plugin_dummy:manifest(),
        ?assertMatch(
            #{
                rps_per_uid := _,
                max_concurrent_calls := _,
                msg_quota_per_day := _
            },
            L
        )
    end).

dummy_plugin_manifest_budget_shape_test_() ->
    ?TEST_SIMPLE(fun() ->
        #{budget := B} = imboy_plugin_dummy:manifest(),
        ?assertMatch(
            #{
                max_connections := _,
                max_subscriptions := _,
                mem_mb_soft_limit := _
            },
            B
        )
    end).

dummy_plugin_manifest_degrade_valid_enum_test_() ->
    ?TEST_SIMPLE(fun() ->
        #{degrade := #{on_unhealthy := V}} = imboy_plugin_dummy:manifest(),
        ?assert(lists:member(V, [isolate, route_404, pass_through]))
    end).

dummy_plugin_manifest_circuit_breaker_shape_test_() ->
    ?TEST_SIMPLE(fun() ->
        #{circuit_breaker := CB} = imboy_plugin_dummy:manifest(),
        ?assertMatch(
            #{enabled := _, threshold_pct := _, window_sec := _},
            CB
        )
    end).

dummy_plugin_manifest_migration_table_prefix_matches_name_test_() ->
    %% 契约 §3.3：migrations.table_prefix 必须等于 name + "_"
    %% Contract §3.3: migrations.table_prefix MUST equal name + "_"
    ?TEST_SIMPLE(fun() ->
        M = imboy_plugin_dummy:manifest(),
        Name = maps:get(name, M),
        Prefix = maps:get(table_prefix, maps:get(migrations, M)),
        Expected = <<(atom_to_binary(Name, utf8))/binary, "_">>,
        ?assertEqual(Expected, Prefix)
    end).

%% ===================================================================
%% 各 callback 行为测试 / per-callback behaviour tests
%% ===================================================================

dummy_plugin_start_returns_ok_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({ok, undefined}, imboy_plugin_dummy:start(#{}))
    end).

dummy_plugin_stop_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_plugin_dummy:stop(undefined))
    end).

dummy_plugin_migrate_up_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_plugin_dummy:migrate(up, <<"0">>, <<"0.1.0">>))
    end).

dummy_plugin_migrate_down_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, imboy_plugin_dummy:migrate(down, <<"0.1.0">>, <<"0">>))
    end).

dummy_plugin_routes_is_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_list(imboy_plugin_dummy:routes()))
    end).

dummy_plugin_capabilities_is_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(is_list(imboy_plugin_dummy:capabilities()))
    end).

dummy_plugin_health_returns_report_test_() ->
    ?TEST_SIMPLE(fun() ->
        Report = imboy_plugin_dummy:health(),
        ?assert(is_map(Report)),
        ?assertMatch(#{status := healthy}, Report),
        Status = maps:get(status, Report),
        ?assert(lists:member(Status, [healthy, degraded, unhealthy]))
    end).
