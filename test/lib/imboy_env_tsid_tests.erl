%%% imboy_env_tsid_tests — TSID-07 部署配置链合同（V10）
%%%
%%% 覆盖两层：
%%%   1) env 层（imboy_env_overrides:override_tsid/0）：
%%%      IMBOY_TSID_* 类型解析——非法值 fail-closed（{invalid_env,...}），
%%%      合法值写入 application env；上界不在此层查（分层语义：类型在 env 层，
%%%      10-bit 布局在 combine_node 层）
%%%   2) 配置链端到端（imboy_sup:tsid_init_config/0 → elib_tsid:init/1）：
%%%      env > application env > 默认 的合成；越界 node/dc 在 combine_node
%%%      处 error（sup init 失败 = AC-07C 运行时 fail-closed）
%%%   3) 误配合同：tsid_lock_provider 键已随 V10 删除，任何环境显式设置即拒启
%%%
%%% AC-07D：本套测试涉及的 TSID 变量全部为非敏感项（节点号/时序参数）。
-module(imboy_env_tsid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WITH_ENV(EnvVars, Expr),
    (fun() ->
        EnvSnapshot = snapshot_env(),
        try
            setup_env(EnvVars),
            Expr
        after
            restore_env(EnvSnapshot)
        end
    end)()
).

setup_env(Vars) ->
    lists:foreach(
        fun(Name) ->
            os:unsetenv(Name),
            application:unset_env(imboy, env_key(Name))
        end,
        env_vars()
    ),
    [
        os:putenv(Name, Value)
     || {Name, Value} <- Vars
    ],
    ok.

env_vars() ->
    [
        "IMBOY_TSID_DC_ID",
        "IMBOY_TSID_NODE_ID",
        "IMBOY_TSID_DC_BITS",
        "IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS",
        "IMBOY_TSID_MAX_CLOCK_DRIFT_MS"
    ].

snapshot_env() ->
    [{Name, os:getenv(Name), application:get_env(imboy, env_key(Name))} || Name <- env_vars()].

restore_env(Snapshot) ->
    lists:foreach(
        fun({Name, OsValue, AppValue}) ->
            case OsValue of
                false -> os:unsetenv(Name);
                _ -> os:putenv(Name, OsValue)
            end,
            restore_app_env([{env_key(Name), AppValue}])
        end,
        Snapshot
    ).

env_key("IMBOY_TSID_DC_ID") -> tsid_dc_id;
env_key("IMBOY_TSID_NODE_ID") -> tsid_node_id;
env_key("IMBOY_TSID_DC_BITS") -> tsid_dc_bits;
env_key("IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS") -> tsid_capacity_wait_timeout_ms;
env_key("IMBOY_TSID_MAX_CLOCK_DRIFT_MS") -> tsid_max_clock_drift_ms.

environment_restored_after_error_test() ->
    Snapshot = snapshot_env(),
    ?assertError(
        fixture_failure,
        ?WITH_ENV(
            [{"IMBOY_TSID_NODE_ID", "5"}],
            begin
                ok = imboy_env_overrides:override_tsid(),
                error(fixture_failure)
            end
        )
    ),
    ?assertEqual(Snapshot, snapshot_env()).

%% ===================================================================
%% env 层：合法值
%% ===================================================================

valid_values_applied_test() ->
    ?WITH_ENV(
        [
            {"IMBOY_TSID_DC_ID", "2"},
            {"IMBOY_TSID_NODE_ID", "0"},
            {"IMBOY_TSID_DC_BITS", "3"},
            {"IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS", "250"}
        ],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ?assertEqual({ok, 2}, application:get_env(imboy, tsid_dc_id)),
            ?assertEqual({ok, 0}, application:get_env(imboy, tsid_node_id)),
            ?assertEqual({ok, 3}, application:get_env(imboy, tsid_dc_bits)),
            ?assertEqual({ok, 250}, application:get_env(imboy, tsid_capacity_wait_timeout_ms))
        end
    ).

%% 未设置的键不写 application env（保留默认/静态配置）
unset_keys_untouched_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_DC_BITS", "4"}],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ?assertEqual({ok, 4}, application:get_env(imboy, tsid_dc_bits)),
            ?assertEqual(undefined, application:get_env(imboy, tsid_dc_id))
        end
    ).

%% ===================================================================
%% env 层：非法值 fail-closed（AC-07C）
%% ===================================================================

invalid_node_id_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_NODE_ID", "-1"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_NODE_ID", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

non_integer_dc_id_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_DC_ID", "one"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_DC_ID", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

zero_capacity_wait_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS", "0"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

%% ===================================================================
%% 配置链端到端：imboy_sup:tsid_init_config/0 → elib_tsid:init/1
%% ===================================================================

%% env > application env > 默认：env 设置的值进入 init config
config_chain_env_priority_test() ->
    ?WITH_ENV(
        [
            {"IMBOY_TSID_DC_ID", "1"},
            {"IMBOY_TSID_NODE_ID", "5"},
            {"IMBOY_TSID_DC_BITS", "3"}
        ],
        begin
            %% application env 故意设不同值，证明 env 优先
            application:set_env(imboy, tsid_node_id, 99),
            ok = imboy_env_overrides:override_tsid(),
            Cfg = imboy_sup:tsid_init_config(),
            ?assertEqual(1, maps:get(dc_id, Cfg)),
            ?assertEqual(5, maps:get(node_id, Cfg)),
            ?assertEqual(3, maps:get(dc_bits, Cfg)),
            application:unset_env(imboy, tsid_node_id)
        end
    ).

%% 默认链：无 env 无 application env → 默认 dc=1/node=1/bits=3 → 129
config_chain_defaults_test() ->
    ?WITH_ENV([], begin
        Cfg = imboy_sup:tsid_init_config(),
        ?assertEqual(1, maps:get(dc_id, Cfg)),
        ?assertEqual(1, maps:get(node_id, Cfg)),
        ?assertEqual(3, maps:get(dc_bits, Cfg)),
        ?assertEqual(100, maps:get(capacity_wait_timeout_ms, Cfg)),
        ?assertEqual(5000, maps:get(max_clock_drift_ms, Cfg))
    end).

%% 阈值键经 env 链透传进 init config
clock_drift_threshold_chain_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_MAX_CLOCK_DRIFT_MS", "3000"}],
        begin
            ok = imboy_env_overrides:override_tsid(),
            Cfg = imboy_sup:tsid_init_config(),
            ?assertEqual(3000, maps:get(max_clock_drift_ms, Cfg))
        end
    ).

%% 非法阈值 fail-closed
zero_clock_drift_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_MAX_CLOCK_DRIFT_MS", "0"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_MAX_CLOCK_DRIFT_MS", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

%% 越界 node_id（dc_bits=3 上限 127）→ combine_node error →
%% init 抛错 → sup init 失败（AC-07C 运行时 fail-closed）
config_chain_out_of_range_rejected_test() ->
    ?WITH_ENV(
        [
            {"IMBOY_TSID_DC_ID", "1"},
            {"IMBOY_TSID_NODE_ID", "200"},
            {"IMBOY_TSID_DC_BITS", "3"}
        ],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ?assertMatch(
                {elib_tsid_invalid_config, _},
                catch_typed(fun() -> elib_tsid:init(imboy_sup:tsid_init_config()) end)
            )
        end
    ).

%% ===================================================================
%% 误配合同：tsid_lock_provider 键已随 V10 删除（无跨实例互斥），
%% 任何环境显式设置即拒启
%% ===================================================================

lock_provider_key_forbidden_test() ->
    Snap = snapshot_app_env([tsid_lock_provider]),
    try
        application:set_env(imboy, tsid_lock_provider, registry),
        ?assertMatch(
            {tsid_config_forbidden, #{key := tsid_lock_provider}},
            catch_typed(fun imboy_sup:tsid_init_config/0)
        )
    after
        restore_app_env(Snap)
    end.

%% ===================================================================
%% 内部助手
%% ===================================================================

catch_typed(Fun) ->
    try
        Fun()
    catch
        error:R -> R
    end.

%% application env 快照/还原：测试用例 after 必须还原进入前状态
%%（而非无条件 unset），保住 eunit 轨道其它套件的 env 假设。
snapshot_app_env(Keys) ->
    [{K, application:get_env(imboy, K)} || K <- Keys].

restore_app_env([{K, undefined} | T]) ->
    application:unset_env(imboy, K),
    restore_app_env(T);
restore_app_env([{K, {ok, V}} | T]) ->
    application:set_env(imboy, K, V),
    restore_app_env(T);
restore_app_env([]) ->
    ok.
