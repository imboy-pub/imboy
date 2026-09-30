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
%%%   3) R1 加固合同：lock_provider 环境分档硬编码（配置面为零）+
%%%      误配键拒启（tsid_lock_provider 任何环境禁设；seam 双键仅
%%%      test 轨道合法，非 test 设置即 error）
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
%% R1 加固合同：lock_provider 环境分档硬编码 + 误配键拒启
%%（杜绝配置误设：配置面为零，越轨配置 fail-fast）
%% ===================================================================

%% 分档纯函数值域矩阵：prod 与一切未知值恒 flock（默认即最保守生产配置）；
%% test 恒 registry。<<"pro">>/<<"production">> 等非白名单值一律按生产对待。
lock_provider_matrix_test() ->
    ?assertEqual(flock, imboy_sup:lock_provider_for(<<"prod">>)),
    ?assertEqual(flock, imboy_sup:lock_provider_for(<<"pro">>)),
    ?assertEqual(flock, imboy_sup:lock_provider_for(<<"production">>)),
    ?assertEqual(flock, imboy_sup:lock_provider_for(<<"staging">>)),
    ?assertEqual(registry, imboy_sup:lock_provider_for(<<"test">>)).

%% local 探测注入：finder 返回 false → registry（macOS 形态）；
%% 找到路径 → flock（Linux 形态）。不依赖本机是否装 flock。
local_lock_provider_probe_test() ->
    ?assertEqual(registry, imboy_sup:local_lock_provider(fun(_) -> false end)),
    ?assertEqual(flock, imboy_sup:local_lock_provider(fun(_) -> "/usr/bin/flock" end)).

%% tsid_lock_provider 键已从配置面删除：prod 轨道显式设置即拒启
%%（防误设 registry 致跨 VM 互斥失效——双实例红线绕过）。
%% after 用 snapshot/restore 还原进入前状态（而非无条件 unset）——
%% eunit 全量轨道的 eunit_setup 可能已设置同名键，无条件清除会在
%% app 重启时使 guard 走真实现（schema_drift 拒启 → boot 波动）。
lock_provider_key_forbidden_test() ->
    with_runtime_env(<<"prod">>, fun() ->
        Snap = snapshot_app_env([tsid_lock_provider]),
        try
            application:set_env(imboy, tsid_lock_provider, registry),
            ?assertMatch(
                {tsid_config_forbidden, #{key := tsid_lock_provider}},
                catch_typed(fun imboy_sup:tsid_guard_config/0)
            )
        after
            restore_app_env(Snap)
        end
    end).

%% 非 test 轨道设置 seam 键 → 拒启（测试基建泄漏进生产配置的检测）：
%% 防假 scan 从 floor 0 起跳导致的 ID 重用（唯一性反例）。
seam_keys_forbidden_outside_test_test() ->
    with_runtime_env(<<"prod">>, fun() ->
        Snap = snapshot_app_env([tsid_bootstrap_env_fun, tsid_bootstrap_scan_fun]),
        try
            application:set_env(imboy, tsid_bootstrap_env_fun, fun(_) -> false end),
            application:set_env(
                imboy, tsid_bootstrap_scan_fun, fun(_) -> {ok, #{floor_safe_before => 0}} end
            ),
            ?assertMatch(
                {tsid_config_forbidden, #{
                    keys := [tsid_bootstrap_env_fun, tsid_bootstrap_scan_fun]
                }},
                catch_typed(fun imboy_sup:tsid_guard_config/0)
            )
        after
            restore_app_env(Snap)
        end
    end).

%% test 轨道：seam 成对注入透传 + lock_provider 恒 registry；
%% 半设（清 env 留 scan）按配置错误拒启。after 还原进入前状态，
%% 保住 eunit 轨道 eunit_setup 的 seam 设置不被测试清掉。
seam_passthrough_test_track_test() ->
    with_runtime_env(<<"test">>, fun() ->
        EnvF = fun(_) -> false end,
        ScanF = fun(_) -> {ok, #{floor_safe_before => 0}} end,
        Snap = snapshot_app_env([tsid_bootstrap_env_fun, tsid_bootstrap_scan_fun]),
        try
            application:set_env(imboy, tsid_bootstrap_env_fun, EnvF),
            application:set_env(imboy, tsid_bootstrap_scan_fun, ScanF),
            Cfg = imboy_sup:tsid_guard_config(),
            ?assertEqual(registry, maps:get(lock_provider, Cfg)),
            ?assertEqual(EnvF, maps:get(bootstrap_env_fun, Cfg)),
            ?assertEqual(ScanF, maps:get(bootstrap_scan_fun, Cfg)),
            %% 半设：清 env 留 scan → 拒启
            application:unset_env(imboy, tsid_bootstrap_env_fun),
            ?assertMatch(
                {tsid_config_invalid, _},
                catch_typed(fun imboy_sup:tsid_guard_config/0)
            )
        after
            restore_app_env(Snap)
        end
    end).

%% prod 端到端：干净 prod 配置（无任何 TSID 特殊键）→ lock_provider 恒
%% flock、无 seam 键。eunit 轨道 eunit_setup 会预设 seam 双键，而 prod
%% 分档下它们的存在本身即 forbidden（拒启是正确行为，见
%% seam_keys_forbidden_outside_test_test）——故本用例先快照并临时清空
%% 轨道预设，验证"干净"形态，测后还原。
prod_lock_provider_hardcoded_test() ->
    with_runtime_env(<<"prod">>, fun() ->
        Snap = snapshot_app_env([tsid_bootstrap_env_fun, tsid_bootstrap_scan_fun]),
        try
            application:unset_env(imboy, tsid_bootstrap_env_fun),
            application:unset_env(imboy, tsid_bootstrap_scan_fun),
            Cfg = imboy_sup:tsid_guard_config(),
            ?assertEqual(flock, maps:get(lock_provider, Cfg)),
            ?assertEqual(false, maps:is_key(bootstrap_env_fun, Cfg)),
            ?assertEqual(false, maps:is_key(bootstrap_scan_fun, Cfg))
        after
            restore_app_env(Snap)
        end
    end).

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

%% application env 快照/还原：eunit 全量轨道下 eunit_setup 会预设
%% tsid seam 键，测试用例 after 必须还原进入前状态（而非无条件 unset），
%% 否则测试之后 app 一旦重启（boot 重试等）guard 将失去 seam 走真实现
%% （scratch 库 schema_drift → 拒启 → boot 波动）。与 adm_passport_handler
%% 测试的 snapshot_runtime_env/restore_runtime_env 同款纪律。
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

%% R1 加固合同用例的运行时分档模拟：IMBOYENV OS 变量优先于 application
%% env（imboy_env:current/0 合同），故直接 putenv 覆盖；测后恢复原值，
%% 不破坏外部启动方式（eunit 轨道 eunit_setup 已声明 test，还原后回到
%% test；分套件独立跑时还原为未设置 → current() 缺省 prod，同样正确）。
with_runtime_env(EnvBin, Fun) ->
    Old = os:getenv("IMBOYENV"),
    os:putenv("IMBOYENV", binary_to_list(EnvBin)),
    try
        Fun()
    after
        case Old of
            false -> os:unsetenv("IMBOYENV");
            _ -> os:putenv("IMBOYENV", Old)
        end
    end.
