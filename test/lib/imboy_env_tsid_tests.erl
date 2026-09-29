%%% imboy_env_tsid_tests — TSID-07 部署配置链合同
%%%
%%% 覆盖两层：
%%%   1) env 层（imboy_env_overrides:override_tsid/0）：
%%%      IMBOY_TSID_* 类型解析——非法值 fail-closed（{invalid_env,...}），
%%%      合法值写入 application env；上界不在此层查（分层语义：类型在 env 层，
%%%      10-bit 布局在 guard/combine_node 层）
%%%   2) 配置链端到端（imboy_sup:tsid_guard_config/0）：
%%%      env > application env > 默认 的合成；越界 node/dc 在 combine_node
%%%      处 error（sup 启动即失败 = AC-07C 运行时 fail-closed）
%%%
%%% AC-07D：本套测试涉及的 TSID 变量全部为非敏感项（路径/节点号/时序参数）。
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

env_key("IMBOY_TSID_STATE_DIR") -> tsid_state_dir;
env_key("IMBOY_TSID_DC_ID") -> tsid_dc_id;
env_key("IMBOY_TSID_NODE_ID") -> tsid_node_id;
env_key("IMBOY_TSID_DC_BITS") -> tsid_dc_bits;
env_key("IMBOY_TSID_STORE_BOOTSTRAP") -> tsid_store_bootstrap;
env_key("IMBOY_TSID_MAX_LOGICAL_LEAD_MS") -> tsid_max_logical_lead_ms;
env_key("IMBOY_TSID_CAPACITY_WAIT_TIMEOUT_MS") -> tsid_capacity_wait_timeout_ms;
env_key("IMBOY_TSID_FENCE_WINDOW_MS") -> tsid_fence_window_ms;
env_key("IMBOY_TSID_FENCE_RENEW_MARGIN_MS") -> tsid_fence_renew_margin_ms;
env_key("IMBOY_TSID_STARTUP_CLOCK_WAIT_TIMEOUT_MS") -> tsid_startup_clock_wait_timeout_ms.

%% ===================================================================
%% env 层：合法值
%% ===================================================================

valid_values_applied_test() ->
    ?WITH_ENV(
        [
            {"IMBOY_TSID_STATE_DIR", " /var/lib/imboy/tsid "},
            {"IMBOY_TSID_DC_ID", "2"},
            {"IMBOY_TSID_NODE_ID", "0"},
            {"IMBOY_TSID_DC_BITS", "3"},
            {"IMBOY_TSID_STORE_BOOTSTRAP", "fresh"},
            {"IMBOY_TSID_MAX_LOGICAL_LEAD_MS", "7"},
            {"IMBOY_TSID_FENCE_WINDOW_MS", "2000"}
        ],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ?assertEqual(<<"/var/lib/imboy/tsid">>, key_bin(tsid_state_dir)),
            ?assertEqual({ok, 2}, application:get_env(imboy, tsid_dc_id)),
            ?assertEqual({ok, 0}, application:get_env(imboy, tsid_node_id)),
            ?assertEqual({ok, 3}, application:get_env(imboy, tsid_dc_bits)),
            ?assertEqual({ok, fresh}, application:get_env(imboy, tsid_store_bootstrap)),
            ?assertEqual({ok, 7}, application:get_env(imboy, tsid_max_logical_lead_ms)),
            ?assertEqual({ok, 2000}, application:get_env(imboy, tsid_fence_window_ms))
        end
    ).

%% 未设置的键不写 application env（保留默认/静态配置）
unset_keys_untouched_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_DC_BITS", "4"}],
        begin
            ok = imboy_env_overrides:override_tsid(),
            ?assertEqual({ok, 4}, application:get_env(imboy, tsid_dc_bits)),
            ?assertEqual(undefined, application:get_env(imboy, tsid_state_dir))
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

zero_lead_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_MAX_LOGICAL_LEAD_MS", "0"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_MAX_LOGICAL_LEAD_MS", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

invalid_bootstrap_rejected_test() ->
    ?WITH_ENV(
        [{"IMBOY_TSID_STORE_BOOTSTRAP", "auto"}],
        ?assertMatch(
            {invalid_env, "IMBOY_TSID_STORE_BOOTSTRAP", _},
            catch_typed(fun imboy_env_overrides:override_tsid/0)
        )
    ).

%% ===================================================================
%% 配置链端到端：imboy_sup:tsid_guard_config/0
%% ===================================================================

%% env > application env > 默认：env 设置的值进入 guard config
config_chain_env_priority_test() ->
    ?WITH_ENV(
        [
            {"IMBOY_TSID_DC_ID", "1"},
            {"IMBOY_TSID_NODE_ID", "5"},
            {"IMBOY_TSID_DC_BITS", "3"},
            {"IMBOY_TSID_STATE_DIR", "/tmp/tsid_chain_test"}
        ],
        begin
            %% application env 故意设不同值，证明 env 优先
            application:set_env(imboy, tsid_node_id, 99),
            ok = imboy_env_overrides:override_tsid(),
            Cfg = imboy_sup:tsid_guard_config(),
            %% dc_bits=3 → combined_node = DcId bsl 7 bor NodeId = 1*128+5 = 133
            ?assertEqual(133, maps:get(combined_node, Cfg)),
            ?assertEqual("/tmp/tsid_chain_test", maps:get(root, Cfg)),
            ?assertEqual(existing, maps:get(store_bootstrap, Cfg)),
            application:unset_env(imboy, tsid_node_id)
        end
    ).

%% 默认链：无 env 无 application env → 默认 dc=1/node=1/bits=3 → 129，
%% state dir 回落 code:priv_dir
config_chain_defaults_test() ->
    Cfg = imboy_sup:tsid_guard_config(),
    ?assertEqual(129, maps:get(combined_node, Cfg)),
    ?assertEqual(existing, maps:get(store_bootstrap, Cfg)),
    ?assertEqual(512, maps:get(max_logical_lead_ms, Cfg)),
    ?assertEqual(1000, maps:get(fence_window_ms, Cfg)).

%% 越界 node_id（dc_bits=3 上限 127）→ combine_node error →
%% tsid_guard_config 抛错 → sup child 启动失败（AC-07C 运行时 fail-closed）
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
                catch_typed(fun imboy_sup:tsid_guard_config/0)
            )
        end
    ).

%% ===================================================================
%% 内部助手
%% ===================================================================

%% state_dir 是 string trim 后存 string，这里宽松断言（binary 或 string 均转 binary）
key_bin(Key) ->
    case application:get_env(imboy, Key) of
        {ok, V} when is_binary(V) -> V;
        {ok, V} when is_list(V) -> unicode:characters_to_binary(V);
        undefined -> undefined
    end.

catch_typed(Fun) ->
    try
        Fun()
    catch
        error:R -> R
    end.
