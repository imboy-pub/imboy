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

%% 每个测试自恢复：os env 与 application env 用后即清
-define(WITH_ENV(EnvVars, Expr), begin
    setup_env(EnvVars),
    try
        Expr
    after
        cleanup_env(EnvVars)
    end
end).

setup_env(Vars) ->
    [
        begin
            os:putenv(Name, Value),
            application:unset_env(imboy, env_key(Name))
        end
     || {Name, Value} <- Vars
    ].

cleanup_env(Vars) ->
    [
        begin
            os:unsetenv(Name),
            application:unset_env(imboy, env_key(Name))
        end
     || {Name, _} <- Vars
    ].

env_key("IMBOY_TSID_DC_ID") -> tsid_dc_id;
env_key("IMBOY_TSID_NODE_ID") -> tsid_node_id;
env_key("IMBOY_TSID_DC_BITS") -> tsid_dc_bits;
env_key("IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS") -> tsid_capacity_wait_timeout_ms;
env_key("IMBOY_TSID_MAX_CLOCK_DRIFT_MS") -> tsid_max_clock_drift_ms.

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
    Cfg = imboy_sup:tsid_init_config(),
    ?assertEqual(1, maps:get(dc_id, Cfg)),
    ?assertEqual(1, maps:get(node_id, Cfg)),
    ?assertEqual(3, maps:get(dc_bits, Cfg)),
    ?assertEqual(100, maps:get(capacity_wait_timeout_ms, Cfg)),
    ?assertEqual(5000, maps:get(max_clock_drift_ms, Cfg)).

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

%% 配置链端到端：tsid_init_config 的合成结果可直接被 elib_tsid:init 接受
config_chain_feeds_init_test() ->
    elib_tsid:reset_for_test(),
    ?WITH_ENV(
        [
            {"IMBOY_TSID_DC_ID", "2"},
            {"IMBOY_TSID_NODE_ID", "3"},
            {"IMBOY_TSID_DC_BITS", "3"}
        ],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ok = elib_tsid:init(imboy_sup:tsid_init_config()),
            Id = elib_tsid:generate(user),
            P = elib_tsid:parse(Id),
            %% dc_bits=3 → node bits=7；(2 bsl 7) bor 3 = 259
            ?assertEqual(2, maps:get(dc_id, P)),
            ?assertEqual(3, maps:get(node_id, P))
        end
    ),
    %% 测试轨道不留空 runtime：后台 worker（plugin audit 等）随时可能发号，
    %% reset 后必须恢复默认配置（W7 全量轨道 12 条 not_initialized 噪音根因）
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).

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
