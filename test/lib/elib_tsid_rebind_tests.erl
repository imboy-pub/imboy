%%%-------------------------------------------------------------------
%%% @doc catalog digest 显式重绑（rebind）状态机测试矩阵
%%%
%%% 只测 elib_tsid_bootstrap:decide/1 的重绑判定与崩溃恢复语义：全部
%%% 经注入 fun（env/scan/wall clock）与 tmpdir manifest 构造，绝不启动
%%% guard、绝不触碰 VM 级 TSID runtime、绝不连库——因此本套件参与全量
%%% eunit 轨道（无需 Excl；guard 级重绑集成见 elib_tsid_guard_tests）。
%%%
%%% 溯源口径：v1/v2/v3 digest 全部取自 elib_tsid_catalog:known_versions()
%%% （真实已发布清单），v2→v3 为已验证邻接；v1→v3 跨步与捏造 digest 均
%%% 未经验证。
%%%
%%% 矩阵覆盖（对应任务阶段 D）：
%%%   - 无 rebind 意图的 catalog_changed（默认行为钉住）
%%%   - ACK 正确 / old 错 / new 错 / 交换 / 过期（v1→v2 ACK 撞 v2→v3 失配）
%%%   - ACK 格式非法（前缀/段数/十六进制/长度/类型）
%%%   - 未知 transition（捏造 digest；v1→v3 跨步）
%%%   - scan/schema/DB 失败（error / 非法 floor / 非法结果形状）
%%%   - floor 三态（store>scan / scan>store / 相等）与 scan=0 绝不降低
%%%   - lifetime_lock_held 缺失 → rebind_lock_not_held
%%%   - store_lost 优先于重绑授权；默认顺序（无 ACK 时 floor=0 失配仍是
%%%     catalog_changed）
%%%   - ACK 惰性（digest 匹配 / manifest 缺失状态下不参与判定）
%%%   - 崩溃窗口：persist 前 / persist 后 manifest 前 / manifest durable 后
%%%-------------------------------------------------------------------
-module(elib_tsid_rebind_tests).

-include_lib("eunit/include/eunit.hrl").

-define(NODE, 7).
-define(EPOCH_MS, 1735689600000).
-define(ENV_ACK, "IMBOY_TSID_BOOTSTRAP_REBIND_ACK").
-define(ACK_PREFIX, "I-CONFIRM-OLD-WRITER-STOPPED-AND-REBIND").

%% ===================================================================
%% Helpers
%% ===================================================================

known_digest(V) ->
    {V, D} = lists:keyfind(V, 1, elib_tsid_catalog:known_versions()),
    D.

hex(D) ->
    binary_to_list(binary:encode_hex(D)).

ack(OldD, NewD) ->
    ?ACK_PREFIX ++ ":" ++ hex(OldD) ++ ":" ++ hex(NewD).

tmp_path() ->
    Dir =
        "/tmp/tsid_rebind_test_" ++
            integer_to_list(erlang:phash2(self())) ++ "_" ++
            integer_to_list(os:system_time(nanosecond)),
    %% 与真实 guard 布局一致：<root>/node-<NNNN>/tsid.bootstrap——
    %% crash 窗口用例在同一 root 下开真实 store 时目录互相咬合。
    NodeDir = filename:join(Dir, io_lib:format("node-~4..0B", [?NODE])),
    ok = filelib:ensure_dir(NodeDir ++ "/x"),
    filename:join(NodeDir, "tsid.bootstrap").

env(Map) ->
    fun(Name) ->
        case maps:find(Name, Map) of
            {ok, V} -> V;
            error -> false
        end
    end.

%% 重绑场景基础 ctx：manifest 绑 v2、当前 catalog v3、ACK 精确绑定
%% v2→v3、store floor 5000、scan floor 100、持锁。
ctx(Overrides) ->
    Base = #{
        store_floor => 5000,
        catalog_digest => known_digest(3),
        combined_node => ?NODE,
        manifest_path => tmp_path(),
        lifetime_lock_held => true,
        env_fun => env(#{?ENV_ACK => ack(known_digest(2), known_digest(3))}),
        scan_fun => fun(_) -> {ok, #{floor_safe_before => 100}} end,
        wall_clock_fun => fun(millisecond) -> ?EPOCH_MS + 5000 end
    },
    maps:merge(Base, Overrides).

v2_manifest(Floor) ->
    #{
        mode => auto_scan,
        floor_safe_before => Floor,
        combined_node => ?NODE,
        catalog_digest => known_digest(2),
        created_at_rel_ms => 5000
    }.

%% 落盘一份 v2 绑定的割接 manifest 并返回其 ctx（默认即重绑悬置态）
v2_state(Ovr) ->
    C = ctx(Ovr),
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C), v2_manifest(5000)),
    C.

%% ===================================================================
%% 默认行为：无 rebind 意图 → catalog_changed（合同钉住，不得回归）
%% ===================================================================

default_catalog_changed_without_ack_test() ->
    C = v2_state(#{env_fun => env(#{})}),
    ?assertEqual({stop, catalog_changed}, elib_tsid_bootstrap:decide(C)).

%% 无 ACK 时 floor=0 + digest 失配：维持 catalog_changed（默认停止顺序
%% 不因重绑代码存在而改变；store_lost 只在 ACK 悬置的重绑路径中前置）。
default_catalog_changed_before_store_lost_test() ->
    C0 = v2_state(#{env_fun => env(#{})}),
    C = C0#{store_open_error => no_valid_slot},
    ?assertEqual({stop, catalog_changed}, elib_tsid_bootstrap:decide(C)).

%% ===================================================================
%% ACK 正确：v2→v3 真实已验证 transition 的授权
%% ===================================================================

ack_correct_authorizes_test() ->
    C = v2_state(#{}),
    ?assertMatch(
        {ok, #{
            action := rebind_floor,
            %% ProposedFloor = max(store 5000, scan 100)
            floor_safe_before := 5000,
            mode := catalog_rebind,
            combined_node := ?NODE,
            created_at_rel_ms := 5000
        }},
        elib_tsid_bootstrap:decide(C)
    ),
    {ok, #{catalog_digest := D}} = elib_tsid_bootstrap:decide(C),
    ?assertEqual(known_digest(3), D).

%% scan 收到当前 catalog 全量清单与 rebind 场景标记
ack_scan_receives_current_catalog_test() ->
    Self = self(),
    C =
        v2_state(#{
            scan_fun =>
                fun(Opts) ->
                    Self ! {rebind_scan_opts, Opts},
                    {ok, #{floor_safe_before => 1}}
                end
        }),
    {ok, _} = elib_tsid_bootstrap:decide(C),
    receive
        {rebind_scan_opts, Opts} ->
            ?assertEqual(elib_tsid_catalog:primary_keys(), maps:get(catalog, Opts)),
            ?assertEqual(true, maps:get(rebind, Opts))
    after 1000 ->
        error(no_scan_call)
    end.

%% 十六进制大小写不敏感（binary:decode_hex 口径），digest 绑定语义不变
ack_uppercase_hex_binds_test() ->
    Upper = fun(D) -> string:to_upper(hex(D)) end,
    AckVal = ?ACK_PREFIX ++ ":" ++ Upper(known_digest(2)) ++ ":" ++ Upper(known_digest(3)),
    C = v2_state(#{env_fun => env(#{?ENV_ACK => AckVal})}),
    ?assertMatch({ok, #{action := rebind_floor}}, elib_tsid_bootstrap:decide(C)).

%% ===================================================================
%% ACK 错绑：old/new 错、交换、过期
%% ===================================================================

ack_wrong_old_test() ->
    C = v2_state(#{env_fun => env(#{?ENV_ACK => ack(known_digest(1), known_digest(3))})}),
    ?assertMatch(
        {stop, {bootstrap_env, {rebind_ack_digest_mismatch, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

ack_wrong_new_test() ->
    C = v2_state(#{env_fun => env(#{?ENV_ACK => ack(known_digest(2), known_digest(2))})}),
    ?assertMatch(
        {stop, {bootstrap_env, {rebind_ack_digest_mismatch, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

ack_swapped_test() ->
    %% old/new 交换：old=current v3 ≠ manifest v2，new=v2 ≠ current v3。
    C = v2_state(#{env_fun => env(#{?ENV_ACK => ack(known_digest(3), known_digest(2))})}),
    ?assertMatch(
        {stop, {bootstrap_env, {rebind_ack_digest_mismatch, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

ack_expired_test() ->
    %% 过期 ACK：绑定上一代 transition v1→v2，而当前悬置的是 v2→v3。
    C = v2_state(#{env_fun => env(#{?ENV_ACK => ack(known_digest(1), known_digest(2))})}),
    ?assertMatch(
        {stop, {bootstrap_env, {rebind_ack_digest_mismatch, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

%% 错绑 detail 携带四路 digest 证据（ack_old/ack_new/manifest/current）
ack_mismatch_detail_test() ->
    C = v2_state(#{env_fun => env(#{?ENV_ACK => ack(known_digest(1), known_digest(2))})}),
    {stop, {bootstrap_env, {rebind_ack_digest_mismatch, Detail}}} =
        elib_tsid_bootstrap:decide(C),
    ?assertEqual(
        #{
            ack_old_digest => known_digest(1),
            ack_new_digest => known_digest(2),
            manifest_digest => known_digest(2),
            current_digest => known_digest(3)
        },
        Detail
    ).

%% ===================================================================
%% ACK 格式非法矩阵（失配状态下 fail-closed，不静默取默认）
%% ===================================================================

ack_malformed_matrix_test() ->
    V2 = known_digest(2),
    V3 = known_digest(3),
    Bads =
        [
            %% 前缀错/不完整
            "I-CONFIRM-OLD-WRITER-STOPPED",
            ?ACK_PREFIX ++ " ",
            %% 段数错
            ?ACK_PREFIX ++ ":" ++ hex(V2),
            ?ACK_PREFIX ++ ":" ++ hex(V2) ++ ":" ++ hex(V3) ++ ":extra",
            "yes",
            %% 十六进制非法/长度错
            ?ACK_PREFIX ++ ":zz" ++ string:slice(hex(V2), 2) ++ ":" ++ hex(V3),
            ?ACK_PREFIX ++ ":" ++ string:slice(hex(V2), 1) ++ ":" ++ hex(V3),
            ?ACK_PREFIX ++ ":" ++ hex(V2) ++ ":" ++ string:slice(hex(V3), 0, 63),
            %% 大小写以外的空白不被 trim
            ?ACK_PREFIX ++ ": " ++ hex(V2) ++ ":" ++ hex(V3)
        ],
    lists:foreach(
        fun(Bad) ->
            C = v2_state(#{env_fun => env(#{?ENV_ACK => Bad})}),
            ?assertEqual(
                {stop, {bootstrap_env, {bad_rebind_ack, Bad}}},
                elib_tsid_bootstrap:decide(C),
                {bad_ack_value, Bad}
            )
        end,
        Bads
    ).

ack_non_string_value_test() ->
    C = v2_state(#{env_fun => env(#{?ENV_ACK => <<61:32>>})}),
    ?assertMatch(
        {stop, {bootstrap_env, {bad_rebind_ack, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

%% ===================================================================
%% 未知 transition：捏造 digest / 跨步 v1→v3 / 降级 v3→v2
%% ===================================================================

transition_unknown_digest_test() ->
    Fake = <<99:256/unsigned-big>>,
    C = v2_state(#{
        env_fun => env(#{?ENV_ACK => ack(Fake, known_digest(3))})
    }),
    %% Fake ≠ manifest v2 → 先撞 digest 绑定（更具体的错误）。
    ?assertMatch(
        {stop, {bootstrap_env, {rebind_ack_digest_mismatch, _}}},
        elib_tsid_bootstrap:decide(C)
    ).

transition_unpublished_manifest_digest_test() ->
    %% 最纯粹的未知 transition：manifest 本身绑未发布 digest，ACK 精确
    %% 绑定通过后，old digest 在 known_versions 溯源中查无 → unrecognized。
    Fake = <<99:256/unsigned-big>>,
    C0 = ctx(#{env_fun => env(#{?ENV_ACK => ack(Fake, known_digest(3))})}),
    MFake = (v2_manifest(5000))#{catalog_digest => Fake},
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C0), MFake),
    ?assertEqual(
        {stop, blocked_catalog_transition_unrecognized},
        elib_tsid_bootstrap:decide(C0)
    ).

transition_unrecognized_via_manifest_test() ->
    %% manifest 绑 v1（真实历史 digest）、当前 v3：ACK 精确绑定 v1→v3，
    %% 但 v1→v3 跨步从未发布验证 → BLOCKED_CATALOG_TRANSITION_UNRECOGNIZED。
    C0 =
        ctx(#{
            env_fun => env(#{?ENV_ACK => ack(known_digest(1), known_digest(3))})
        }),
    M1 = (v2_manifest(5000))#{catalog_digest => known_digest(1)},
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C0), M1),
    ?assertEqual(
        {stop, blocked_catalog_transition_unrecognized},
        elib_tsid_bootstrap:decide(C0)
    ).

transition_downgrade_unrecognized_test() ->
    %% 降级（manifest 绑 v3、当前 v2）在真实代码内不可达（当前恒 v3）；
    %% 以 v2 为当前、v3 为 manifest 的镜像构造验证：{3,2} 不在 allowlist。
    C0 =
        ctx(#{
            catalog_digest => known_digest(2),
            env_fun => env(#{?ENV_ACK => ack(known_digest(3), known_digest(2))})
        }),
    M3 = (v2_manifest(5000))#{catalog_digest => known_digest(3)},
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C0), M3),
    ?assertEqual(
        {stop, blocked_catalog_transition_unrecognized},
        elib_tsid_bootstrap:decide(C0)
    ).

%% ===================================================================
%% 前置条件：lifetime lock 与 store floor
%% ===================================================================

lock_not_held_stops_test() ->
    C = v2_state(#{lifetime_lock_held => false}),
    ?assertEqual({stop, rebind_lock_not_held}, elib_tsid_bootstrap:decide(C)),
    C2 = v2_state(#{lifetime_lock_held => other}),
    ?assertEqual({stop, rebind_lock_not_held}, elib_tsid_bootstrap:decide(C2)).

lock_key_absent_defaults_unheld_test() ->
    C0 = v2_state(#{}),
    C = maps:remove(lifetime_lock_held, C0),
    ?assertEqual({stop, rebind_lock_not_held}, elib_tsid_bootstrap:decide(C)).

store_lost_beats_rebind_test() ->
    %% manifest 在 + 双槽丢失（floor=0）+ ACK 精确：重绑不得借 persist
    %% 重建已丢 store → store_lost。
    C0 = v2_state(#{}),
    C = C0#{store_open_error => no_valid_slot},
    ?assertEqual({stop, store_lost}, elib_tsid_bootstrap:decide(C)).

store_identity_mismatch_beats_rebind_test() ->
    C = v2_state(#{store_open_error => {config_mismatch, #{}}}),
    ?assertEqual({stop, store_identity_mismatch}, elib_tsid_bootstrap:decide(C)).

%% ===================================================================
%% scan/schema/DB 失败（R6）
%% ===================================================================

scan_error_stops_test() ->
    C = v2_state(#{scan_fun => fun(_) -> {error, conn_refused} end}),
    ?assertEqual({stop, {rebind_scan, conn_refused}}, elib_tsid_bootstrap:decide(C)).

scan_schema_drift_stops_test() ->
    Drift = {error, {schema_drift, #{missing => [some_table], wrong_type => []}}},
    C = v2_state(#{scan_fun => fun(_) -> Drift end}),
    ?assertEqual({stop, {rebind_scan, element(2, Drift)}}, elib_tsid_bootstrap:decide(C)).

scan_invalid_floor_stops_test() ->
    C1 = v2_state(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => -1}} end}),
    ?assertMatch(
        {stop, {rebind_scan, {invalid_floor, -1}}},
        elib_tsid_bootstrap:decide(C1)
    ),
    C2 = v2_state(#{scan_fun => fun(_) -> {ok, #{}} end}),
    ?assertMatch(
        {stop, {rebind_scan, {invalid_scan_result, _}}},
        elib_tsid_bootstrap:decide(C2)
    ).

%% ===================================================================
%% ProposedFloor = max(StoreFloor, ScanFloor)（R7：绝不降低）
%% ===================================================================

floor_store_above_scan_test() ->
    C = v2_state(#{
        store_floor => 9000,
        scan_fun => fun(_) -> {ok, #{floor_safe_before => 100}} end
    }),
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 9000}},
        elib_tsid_bootstrap:decide(C)
    ).

floor_scan_above_store_test() ->
    C = v2_state(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => 12000}} end}),
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 12000}},
        elib_tsid_bootstrap:decide(C)
    ).

floor_equal_test() ->
    C = v2_state(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => 5000}} end}),
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 5000}},
        elib_tsid_bootstrap:decide(C)
    ).

%% 扫描空库（floor=0）也绝不把 store floor 拉低：数据库当前 max 不是
%% 已删除历史 ID 的证明。
floor_scan_zero_never_lowers_test() ->
    C = v2_state(#{scan_fun => fun(_) -> {ok, #{floor_safe_before => 0}} end}),
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 5000}},
        elib_tsid_bootstrap:decide(C)
    ).

%% ===================================================================
%% ACK 惰性：非悬置状态下不参与判定
%% ===================================================================

ack_inert_when_digest_matches_test() ->
    C0 = ctx(#{}),
    %% manifest 绑当前 v3：正常重启，ACK（针对 v2→v3）已过期但仍惰性。
    M = (v2_manifest(5000))#{catalog_digest => known_digest(3)},
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C0), M),
    ?assertMatch(
        {ok, #{action := proceed_existing}},
        elib_tsid_bootstrap:decide(C0)
    ).

ack_malformed_inert_when_digest_matches_test() ->
    %% 同上但 ACK 值格式非法：与 LEGACY_ACK 在 manifest-present 分支不
    %% 读取的行为对称——REBIND_ACK 只在 catalog_changed 悬置时被消费，
    %% 其余状态整体惰性（含格式），正常重启不因残留变量误伤。
    C0 = ctx(#{env_fun => env(#{?ENV_ACK => "not-an-ack"})}),
    M = (v2_manifest(5000))#{catalog_digest => known_digest(3)},
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C0), M),
    ?assertMatch(
        {ok, #{action := proceed_existing}},
        elib_tsid_bootstrap:decide(C0)
    ).

ack_inert_without_manifest_test() ->
    %% manifest 缺失：REBIND_ACK 不被读取（LEGACY_ACK/MODE 流程照旧）。
    C = ctx(#{
        store_floor => 0,
        env_fun => env(#{?ENV_ACK => ack(known_digest(2), known_digest(3))}),
        scan_fun => fun(_) -> {ok, #{floor_safe_before => 42}} end
    }),
    ?assertMatch(
        {ok, #{action := proceed_floor, floor_safe_before := 42}},
        elib_tsid_bootstrap:decide(C)
    ).

%% ===================================================================
%% 崩溃窗口（decide 级）：persist 前 / persist 后 manifest 前 / manifest 后
%% ===================================================================

crash_before_persist_test() ->
    %% 授权后、persist 前崩溃：磁盘无任何变化。重启：无 ACK → 仍
    %% catalog_changed；携 ACK → 重新授权同一 floor。
    C = v2_state(#{}),
    {ok, _} = elib_tsid_bootstrap:decide(C),
    %% 磁盘事实：manifest 仍绑 v2，store 无槽（ctx floor 为调用方视角）。
    ?assertMatch(
        {stop, catalog_changed},
        elib_tsid_bootstrap:decide(C#{env_fun => env(#{})})
    ),
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 5000}},
        elib_tsid_bootstrap:decide(C)
    ).

crash_after_persist_before_manifest_test() ->
    %% persist 已落盘（store floor 提高到 9000）、manifest 未写即崩溃：
    %% 重启携 ACK 重扫（scan=100，远低于已持久化 floor）→ 授权 floor
    %% = max(9000, 100) = 9000，绝不回退到旧授权的 5000。
    C = v2_state(#{}),
    {ok, #{floor_safe_before := Floor0}} = elib_tsid_bootstrap:decide(C),
    ?assertEqual(5000, Floor0),
    %% 模拟 guard 的 persist 已成功（真实 durable 写 + readback）
    Root = filename:dirname(filename:dirname(maps:get(manifest_path, C))),
    {ok, S0} = elib_tsid_store:open(#{
        root => Root, combined_node => ?NODE, dc_bits => 3, store_bootstrap => fresh
    }),
    {ok, _S1} = elib_tsid_store:persist(S0, 9000),
    %% 重启视角：store floor=9000（从磁盘读得），manifest 仍绑 v2。
    C2 = C#{
        store_floor => 9000,
        scan_fun => fun(_) -> {ok, #{floor_safe_before => 100}} end
    },
    ?assertMatch(
        {ok, #{action := rebind_floor, floor_safe_before := 9000}},
        elib_tsid_bootstrap:decide(C2)
    ),
    %% 无 ACK 的重启仍 STOP（manifest 未换绑）。
    ?assertMatch(
        {stop, catalog_changed},
        elib_tsid_bootstrap:decide(C2#{env_fun => env(#{})})
    ).

crash_after_manifest_before_runtime_test() ->
    %% manifest 已 durable 换绑、runtime 未发布即崩溃：重启走正常恢复
    %% （proceed_existing），不需要 ACK，floor 取 store 与 manifest 状态。
    C = v2_state(#{}),
    {ok, Ret} = elib_tsid_bootstrap:decide(C),
    %% 模拟 guard：persist(授权 floor) → write_manifest（新 digest 绑定）
    Root = filename:dirname(filename:dirname(maps:get(manifest_path, C))),
    {ok, S0} = elib_tsid_store:open(#{
        root => Root, combined_node => ?NODE, dc_bits => 3, store_bootstrap => fresh
    }),
    {ok, _S1} = elib_tsid_store:persist(S0, maps:get(floor_safe_before, Ret)),
    Manifest = maps:remove(action, Ret),
    ok = elib_tsid_bootstrap:write_manifest(maps:get(manifest_path, C), Manifest),
    %% 重启：manifest 绑 v3 且 floor 在 → proceed_existing（无任何 ACK）。
    C2 = C#{
        catalog_digest => known_digest(3),
        store_floor => 5000,
        env_fun => env(#{})
    },
    ?assertMatch(
        {ok, #{action := proceed_existing}},
        elib_tsid_bootstrap:decide(C2)
    ),
    %% 换绑 manifest 的 mode 是 catalog_rebind、digest 是新 v3。
    {ok, M} = elib_tsid_bootstrap:read_manifest(maps:get(manifest_path, C)),
    ?assertEqual(catalog_rebind, maps:get(mode, M)),
    ?assertEqual(known_digest(3), maps:get(catalog_digest, M)).

%% 重绑 manifest 落盘字段合同：write_manifest 接受 decide 产物原样
%%（action 键被忽略），floor/digest/node 精确保留。
rebind_manifest_fields_test() ->
    C = v2_state(#{}),
    {ok, Ret} = elib_tsid_bootstrap:decide(C),
    P = maps:get(manifest_path, C),
    ok = elib_tsid_bootstrap:write_manifest(P, Ret),
    {ok, M} = elib_tsid_bootstrap:read_manifest(P),
    ?assertEqual(
        #{
            version => 1,
            mode => catalog_rebind,
            combined_node => ?NODE,
            floor_safe_before => 5000,
            created_at_rel_ms => 5000,
            catalog_digest => known_digest(3)
        },
        M
    ).
