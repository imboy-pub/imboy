-module(elib_tsid_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EPOCH_MS, 1735689600000).
-define(MAX_ID, 9223372036854775807).
-define(MAX_REL, 4398046511103).

-define(SETUP, fun() ->
    %% V10：不同配置 re-init 被拒绝，每个测试先显式 reset 再 init，
    %% 保证测试顺序无关的确定性隔离
    elib_tsid:reset_for_test(),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
end).

%% ===================================================================
%% 基础功能
%% ===================================================================

init_test() ->
    ?SETUP(),
    ok.

generate_positive_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(user),
    ?assert(Id > 0),
    ?assert(is_integer(Id)).

generate_within_bigint_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(user),
    ?assert(Id > 0),
    ?assert(Id =< ?MAX_ID).

generate_max_19_digits_test() ->
    ?SETUP(),
    lists:foreach(
        fun(_) ->
            Id = elib_tsid:generate(msg_store),
            ?assert(Id =< ?MAX_ID)
        end,
        lists:seq(1, 100)
    ).

uniqueness_sequential_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(user, 5000),
    ?assertEqual(5000, length(Ids)),
    ?assertEqual(5000, length(lists:usort(Ids))),
    ?assertEqual(Ids, lists:sort(Ids)).

uniqueness_concurrent_test() ->
    ?SETUP(),
    Self = self(),
    Workers = 10,
    N = 1000,
    [
        spawn(fun() ->
            Ids = elib_tsid:generate_n(user, N),
            Self ! {ids, Ids}
        end)
     || _ <- lists:seq(1, Workers)
    ],
    AllIds = collect_ids(Workers, []),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(Workers * N, length(lists:usort(AllIds))).

monotonic_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(user, 1000),
    Sorted = lists:sort(Ids),
    ?assertEqual(Ids, Sorted).

collect_ids(0, Acc) ->
    Acc;
collect_ids(N, Acc) ->
    receive
        {ids, Ids} -> collect_ids(N - 1, Acc ++ Ids)
    after 30000 ->
        exit(collect_ids_timeout)
    end.

collect_by_name(0, Acc) ->
    Acc;
collect_by_name(N, Acc) ->
    receive
        {ids, Name, Ids} ->
            collect_by_name(N - 1, maps:update_with(Name, fun(L) -> L ++ Ids end, Ids, Acc))
    after 30000 ->
        exit(collect_ids_timeout)
    end.

%% ===================================================================
%% 解析 / 提取
%% ===================================================================

parse_roundtrip_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(user),
    #{id := Id, timestamp := Ts, sequence := Seq} = elib_tsid:parse(Id),
    ?assert(is_integer(Ts)),
    ?assert(Ts >= ?EPOCH_MS),
    ?assert(Seq >= 0),
    ?assert(Seq < 2048).

timestamp_extraction_test() ->
    ?SETUP(),
    NowWall = elib_tsid:wall_clock_ms(),
    Id = elib_tsid:generate(user),
    Ts = elib_tsid:timestamp(Id),
    ?assert(abs(Ts - NowWall) =< 1000).

node_id_extraction_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(user),
    ?assertEqual(129, elib_tsid:node_id(Id)).

%% ===================================================================
%% dc_bits / 节点边界
%% ===================================================================

dc_bits_0_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 1023, dc_bits => 0}),
    Id = elib_tsid:generate(user),
    ?assertEqual(1023, elib_tsid:node_id(Id)).

dc_bits_5_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 31, node_id => 31, dc_bits => 5}),
    Id = elib_tsid:generate(user),
    P = elib_tsid:parse(Id),
    ?assertEqual(31, maps:get(dc_id, P)),
    ?assertEqual(31, maps:get(node_id, P)).

different_nodes_no_collision_test() ->
    lists:foreach(
        fun({Dc, Node}) ->
            elib_tsid:reset_for_test(),
            ok = elib_tsid:init(#{dc_id => Dc, node_id => Node, dc_bits => 3}),
            Id = elib_tsid:generate(user),
            #{dc_id := Dc, node_id := Node} = elib_tsid:parse(Id),
            ok
        end,
        [{0, 0}, {0, 127}, {7, 127}]
    ).

dc_bits_all_boundaries_test() ->
    lists:foreach(
        fun(DcBits) ->
            elib_tsid:reset_for_test(),
            NodeBits = 10 - DcBits,
            MaxDc = (1 bsl DcBits) - 1,
            MaxNode = (1 bsl NodeBits) - 1,
            ok = elib_tsid:init(#{
                dc_id => MaxDc, node_id => MaxNode, dc_bits => DcBits
            }),
            Id = elib_tsid:generate(user),
            ?assert(Id > 0),
            ?assert(Id =< ?MAX_ID),
            P = elib_tsid:parse(Id),
            ?assertEqual(MaxDc, maps:get(dc_id, P)),
            ?assertEqual(MaxNode, maps:get(node_id, P))
        end,
        lists:seq(0, 10)
    ).

combine_node_rejects_overflow_test() ->
    %% NodeId 溢出 node 位段必须 error（防 dc 位被覆盖的静默碰撞）
    ?assertMatch(
        {elib_tsid_invalid_config, _},
        catch_typed(fun() -> elib_tsid:combine_node(1, 200, 3) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_config, _},
        catch_typed(fun() -> elib_tsid:combine_node(8, 1, 3) end)
    ),
    ?assertEqual(129, elib_tsid:combine_node(1, 1, 3)).

%% ===================================================================
%% V10 多 Namespace 独立游标
%% ===================================================================

same_namespace_unique_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(user, 1000),
    ?assertEqual(1000, length(lists:usort(Ids))).

cross_namespace_may_repeat_test() ->
    %% V10 核心语义：跨 Namespace 数值允许重复。冻结时钟同毫秒内，
    %% user 耗尽 2048 槽后，group_info 独立游标从同毫秒 seq=0 起步——
    %% 两 namespace 的首个 ID 数值必然相等。
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 200000, fun() ->
        UserIds = elib_tsid:generate_n(user, 2048),
        #{sequence := 2047} = elib_tsid:parse(lists:last(UserIds)),
        %% 耗尽后本毫秒 user 无法继续（无借支：时钟冻结 → 有界失败）
        ?assertMatch(
            {elib_tsid_capacity_exhausted, _},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        ),
        %% group_info 独立游标：同毫秒同 seq=0 起步 → 与 user 首号数值相同
        GroupFirst = elib_tsid:generate(group_info),
        ?assertEqual(hd(UserIds), GroupFirst)
    end).

%% group 是 group_info 的别名：共享游标，行为完全一致
group_alias_shares_cursor_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 300000, fun() ->
        Ids = [
            elib_tsid:generate(group),
            elib_tsid:generate(group_info),
            elib_tsid:generate(group)
        ],
        %% 同一游标：序列连续（0,1,2）
        ?assertEqual(
            [0, 1, 2],
            [S || #{sequence := S} <- [elib_tsid:parse(I) || I <- Ids]]
        )
    end).

%% audit / enterprise_audit / enterprise_audit_event 三标签一游标
audit_alias_shares_cursor_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 300000, fun() ->
        Ids = [
            elib_tsid:generate(audit),
            elib_tsid:generate(enterprise_audit),
            elib_tsid:generate(enterprise_audit_event)
        ],
        ?assertEqual(
            [0, 1, 2],
            [S || #{sequence := S} <- [elib_tsid:parse(I) || I <- Ids]]
        ),
        ?assertEqual(3, length(lists:usort(Ids)))
    end).

unknown_namespace_typed_error_test() ->
    ?SETUP(),
    ?assertMatch(
        {elib_tsid_generator_not_registered, _},
        catch_typed(fun() -> elib_tsid:generate(definitely_not_a_namespace) end)
    ),
    ?assertMatch(
        {elib_tsid_generator_not_registered, _},
        catch_typed(fun() -> elib_tsid:generate_n(no_such_ns, 3) end)
    ).

namespace_inventory_complete_test() ->
    ?SETUP(),
    %% W1 冻结清单中的每一个 canonical 名称都可生成
    lists:foreach(
        fun(Name) ->
            Id = elib_tsid:generate(Name),
            ?assert(Id > 0, {namespace_failed, Name})
        end,
        elib_tsid:namespaces()
    ).

registered_includes_namespaces_and_aliases_test() ->
    ?SETUP(),
    Registered = elib_tsid:registered(),
    ?assert(lists:member(default, Registered)),
    lists:foreach(
        fun(N) -> ?assert(lists:member(N, Registered), {missing, N}) end,
        elib_tsid:namespaces()
    ),
    lists:foreach(
        fun(A) -> ?assert(lists:member(A, Registered), {missing_alias, A}) end,
        maps:keys(elib_tsid:aliases())
    ).

registered_no_duplicate_default_test() ->
    %% 审计修复（V10-A5）：lookup 在 publish_runtime 已合并 default 键，
    %% registered/0 不得重复追加（修复前 149 项、去重 148 项）
    ?SETUP(),
    Registered = elib_tsid:registered(),
    ?assertEqual(lists:usort(Registered), Registered),
    ?assertEqual(
        length(elib_tsid:namespaces()) + map_size(elib_tsid:aliases()) + 1,
        length(Registered)
    ).

%% register/1 是静态清单的幂等断言：已知名恒 ok，未知名 error
register_idempotent_check_test() ->
    ?SETUP(),
    ok = elib_tsid:register(user),
    ok = elib_tsid:register([group, audit, enterprise_audit]),
    ?assertMatch(
        {elib_tsid_generator_not_registered, _},
        catch_typed(fun() -> elib_tsid:register(not_in_inventory) end)
    ).

default_generator_available_test() ->
    %% default 仅供测试/脚手架，但必须可用（W7 全量轨道 192 失败的根因即
    %% lookup 漏建 default 槽位）
    ?SETUP(),
    ?assert(lists:member(default, elib_tsid:registered())),
    ?assert(elib_tsid:generate(default) > 0),
    ?assertEqual(3, length(elib_tsid:generate_n(default, 3))),
    ok = elib_tsid:register(default).

register_before_init_rejected_test() ->
    elib_tsid:reset_for_test(),
    ?assertMatch(
        {elib_tsid_not_initialized, _},
        catch_typed(fun() -> elib_tsid:register(user) end)
    ),
    %% 无 runtime 即无任何可用生成器（default 也依赖 init；V10-A5 起
    %% registered/0 不再对空 lookup 硬造 [default]）
    ?assertEqual([], elib_tsid:registered()),
    %% 不留空 runtime（后台 worker 保护，见 imboy_env_tsid_tests 同款注释）
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).

%% ===================================================================
%% 无借支铁律（V10）
%% ===================================================================

exhaustion_bounded_error_no_borrow_test() ->
    %% 冻结时钟：2048 用尽后有界 typed error（~100ms deadline），绝不借未来毫秒
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 400000, fun() ->
        Ids = elib_tsid:generate_n(user, 2048),
        ?assertEqual(2048, length(lists:usort(Ids))),
        ?assertEqual(
            [?EPOCH_MS + 400000],
            lists:usort([elib_tsid:timestamp(I) || I <- Ids])
        ),
        T0 = erlang:monotonic_time(millisecond),
        ?assertMatch(
            {elib_tsid_capacity_exhausted, #{phase := clock_wait}},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        ),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assert(Elapsed >= 90 andalso Elapsed =< 1000, {bounded_wait_ms, Elapsed})
    end).

batch_endpoint_no_borrow_frozen_clock_test() ->
    %% 审计负例（V10-A1）：冻结毫秒 M 发 1 个后紧跟满容量批量 2048——
    %% 区间滚入 M+1，无借支检查必须命中并 bounded typed error，
    %% 绝不把 M+1 的未来毫秒号直接发出（修复前 last_lead_ms=1）
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 400000, fun() ->
        _ = elib_tsid:generate(user),
        T0 = erlang:monotonic_time(millisecond),
        ?assertMatch(
            {elib_tsid_capacity_exhausted, #{phase := clock_wait}},
            catch_typed(fun() -> elib_tsid:generate_n(user, 2048) end)
        ),
        Elapsed = erlang:monotonic_time(millisecond) - T0,
        ?assert(Elapsed >= 90 andalso Elapsed =< 1000, {bounded_wait_ms, Elapsed})
    end).

exhaustion_recovered_after_clock_advance_test() ->
    %% 时钟推进后同 namespace 恢复发号，时间戳 = 新毫秒
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 400000, fun() ->
        _ = elib_tsid:generate_n(user, 2048),
        ok
    end),
    with_fixed_clock(?EPOCH_MS + 400001, fun() ->
        Id = elib_tsid:generate(user),
        ?assertEqual(?EPOCH_MS + 400001, elib_tsid:timestamp(Id))
    end).

cursor_absorbs_rollback_test() ->
    %% 回拨后游标吸收：不重复、不回退，沿旧毫秒剩余槽位继续
    ?SETUP(),
    Ids1 = with_fixed_clock(?EPOCH_MS + 500000, fun() ->
        elib_tsid:generate_n(user, 100)
    end),
    Ids2 = with_fixed_clock(?EPOCH_MS + 499000, fun() ->
        elib_tsid:generate_n(user, 100)
    end),
    All = Ids1 ++ Ids2,
    ?assertEqual(200, length(All)),
    ?assertEqual(200, length(lists:usort(All))),
    %% 回拨批的第一号 = 前一批水位的下一槽（同毫秒 seq+1，或耗尽时下一毫秒）
    #{sequence := S1} = elib_tsid:parse(lists:last(Ids1)),
    #{sequence := S2} = elib_tsid:parse(hd(Ids2)),
    case S1 of
        2047 ->
            ?assertEqual(?EPOCH_MS + 500001, elib_tsid:timestamp(hd(Ids2)));
        _ ->
            ?assertEqual(S1 + 1, S2),
            ?assertEqual(?EPOCH_MS + 500000, elib_tsid:timestamp(hd(Ids2)))
    end.

jump_then_rollback_unique_test() ->
    ?SETUP(),
    Ids1 = with_fixed_clock(?EPOCH_MS + 600000, fun() -> elib_tsid:generate_n(user, 20) end),
    Ids2 = with_fixed_clock(?EPOCH_MS + 610000, fun() -> elib_tsid:generate_n(user, 20) end),
    Ids3 = with_fixed_clock(?EPOCH_MS + 600000, fun() -> elib_tsid:generate_n(user, 20) end),
    All = Ids1 ++ Ids2 ++ Ids3,
    ?assertEqual(60, length(lists:usort(All))).

rollback_three_states_test() ->
    lists:foreach(
        fun(RollbackMs) ->
            elib_tsid:reset_for_test(),
            ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
            Base = ?EPOCH_MS + 700000,
            Ids1 = with_fixed_clock(Base, fun() -> elib_tsid:generate_n(user, 10) end),
            Ids2 = with_fixed_clock(Base - RollbackMs, fun() ->
                elib_tsid:generate_n(user, 10)
            end),
            ?assertEqual(20, length(lists:usort(Ids1 ++ Ids2)), {rollback, RollbackMs})
        end,
        [1, 100, 1000, 10000, 60000]
    ).

%% ===================================================================
%% 边界与金样本（位布局 42/10/11 冻结）
%% ===================================================================

golden_vectors_test() ->
    Vectors = [
        {0, 0, 1},
        {0, 1, 0},
        {0, 1023, 2047},
        {1, 0, 0},
        {1, 129, 5},
        {5619271, 517, 1234},
        {?MAX_REL, 1023, 2047}
    ],
    lists:foreach(
        fun({Ts, Node, Seq}) ->
            Id = (Ts bsl 21) bor (Node bsl 11) bor Seq,
            ?assertEqual(Id, elib_tsid:slot_to_id((Ts bsl 11) bor Seq, Node)),
            ?assertEqual(Node, elib_tsid:node_id(Id)),
            ?assertEqual(?EPOCH_MS + Ts, elib_tsid:timestamp(Id)),
            #{id := Id, timestamp := TsMs, sequence := Seq} = elib_tsid:parse(Id),
            ?assertEqual(?EPOCH_MS + Ts, TsMs)
        end,
        Vectors
    ),
    ?assertMatch({elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:parse(0) end)),
    ?assertMatch({elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:parse(-1) end)),
    ?assertMatch(
        {elib_tsid_invalid_input, _},
        catch_typed(fun() -> elib_tsid:parse(not_integer) end)
    ).

max_id_parse_test() ->
    P = elib_tsid:parse(?MAX_ID),
    ?assertEqual(?MAX_ID, maps:get(id, P)),
    ?assertEqual(?EPOCH_MS + ?MAX_REL, maps:get(timestamp, P)),
    ?assertEqual(1023, elib_tsid:node_id(?MAX_ID)),
    ?assertEqual(2047, maps:get(sequence, P)),
    ?assertEqual(7, maps:get(dc_id, P)),
    ?assertEqual(127, maps:get(node_id, P)),
    ?assertMatch({{2164, 5, 15}, {7, 35, 11}}, maps:get(created_at, P)),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:parse(?MAX_ID + 1) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:timestamp(?MAX_ID + 1) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _},
        catch_typed(fun() -> elib_tsid:node_id(?MAX_ID + 1) end)
    ).

last_ms_exhaustion_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    LastId = with_fixed_clock(?EPOCH_MS + ?MAX_REL, fun() ->
        Ids = elib_tsid:generate_n(user, 2048),
        ?assertEqual(2048, length(lists:usort(Ids))),
        ?assertEqual((?MAX_REL bsl 21) bor (129 bsl 11) bor 2047, lists:last(Ids)),
        lists:last(Ids)
    end),
    ?assert(LastId =< ?MAX_ID),
    with_fixed_clock(?EPOCH_MS + ?MAX_REL, fun() ->
        ?assertMatch(
            {elib_tsid_timestamp_exhausted, _},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        )
    end).

clock_beyond_2164_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + ?MAX_REL + 5000, fun() ->
        ?assertMatch(
            {elib_tsid_timestamp_exhausted, _},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        )
    end).

before_epoch_fail_closed_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS - 1000, fun() ->
        ?assertMatch(
            {elib_tsid_clock_before_epoch, _},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        )
    end).

%% ===================================================================
%% corpus
%% ===================================================================

decimal_boundary_corpus_test() ->
    Valid = #{
        <<"1">> => 1,
        <<"9">> => 9,
        <<"123">> => 123,
        <<"007">> => 7,
        <<"9223372036854775807">> => ?MAX_ID
    },
    maps:fold(
        fun(Bin, Expect, _) ->
            ?assertEqual({ok, Expect}, elib_tsid:from_binary(Bin))
        end,
        ok,
        Valid
    ),
    Invalid = [
        <<>>,
        <<"0">>,
        <<"00">>,
        <<"-1">>,
        <<"+7">>,
        <<" 7">>,
        <<"7 ">>,
        <<"0x10">>,
        <<"abc">>,
        <<"1.5">>,
        <<"9223372036854775808">>,
        <<"999999999999999999999999999999999999">>
    ],
    lists:foreach(
        fun(Bin) -> ?assertEqual(error, elib_tsid:from_binary(Bin)) end,
        Invalid
    ).

calendar_precision_test() ->
    P = elib_tsid:parse(?MAX_ID),
    ?assertEqual(6133736111103, maps:get(timestamp, P)),
    ?assertMatch({{2164, 5, 15}, {7, 35, 11}}, maps:get(created_at, P)).

%% ===================================================================
%% init 幂等 / 配置冻结
%% ===================================================================

same_config_reinit_idempotent_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id1 = elib_tsid:generate(user),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id2 = elib_tsid:generate(user),
    ?assert(Id2 > Id1).

different_config_reinit_rejected_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id1 = elib_tsid:generate(user),
    ?assertMatch(
        {elib_tsid_already_initialized, _},
        catch_typed(fun() -> elib_tsid:init(#{dc_id => 1, node_id => 2, dc_bits => 3}) end)
    ),
    ?assertMatch(
        {elib_tsid_already_initialized, _},
        catch_typed(fun() -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 4}) end)
    ),
    ?assertMatch(
        {elib_tsid_already_initialized, _},
        catch_typed(fun() ->
            elib_tsid:init(#{
                dc_id => 1, node_id => 1, dc_bits => 3, capacity_wait_timeout_ms => 500
            })
        end)
    ),
    Id2 = elib_tsid:generate(user),
    ?assert(Id2 > Id1),
    ?assertEqual(129, elib_tsid:node_id(Id2)).

capacity_wait_validation_test() ->
    elib_tsid:reset_for_test(),
    ?assertMatch(
        {elib_tsid_invalid_config, _},
        catch_typed(fun() ->
            elib_tsid:init(#{dc_id => 1, node_id => 1, capacity_wait_timeout_ms => 0})
        end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_config, _},
        catch_typed(fun() ->
            elib_tsid:init(#{dc_id => 1, node_id => 1, capacity_wait_timeout_ms => not_int})
        end)
    ),
    %% 两断言均在 init 抛错后结束，runtime 留空——补恢复（后台 worker 保护）
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}).

readiness_test() ->
    elib_tsid:reset_for_test(),
    ?assertEqual(not_initialized, elib_tsid:readiness()),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    ?assertEqual(ready, elib_tsid:readiness()).

%% ===================================================================
%% W4 时钟安全闸：readiness 低频评估 + 生成入口阻断
%% ===================================================================

clock_drift_beyond_threshold_blocks_generate_test_() ->
    %% {timeout,60} 防御：5s 默认 timetrap 在全量并发饥饿下若击杀本测试，
    %% with_runtime_drift 的 after 还原不会执行，+10s offset 将永久毒化
    %% 共享 runtime（2026-10-10 全量级联实证；runner clock_unsafe 自愈为
    %% 纵深防御）
    {timeout, 60, fun clock_drift_beyond_threshold_blocks_generate_body/0}.

clock_drift_beyond_threshold_blocks_generate_body() ->
    %% 模拟固定 Offset 时钟与系统钟漂移超限：readiness 翻闸 → generate
    %% 拒绝（异常作用于生成入口）；恢复 offset → 重新 ready 可发号
    ?SETUP(),
    ?assertEqual(ready, elib_tsid:readiness()),
    ok = with_runtime_drift(10000, fun() ->
        ?assertEqual(clock_unsafe, elib_tsid:readiness()),
        ?assertMatch(
            {elib_tsid_clock_unsafe, _},
            catch_typed(fun() -> elib_tsid:generate(user) end)
        ),
        ok
    end),
    ?assertEqual(ready, elib_tsid:readiness()),
    ?assert(elib_tsid:generate(user) > 0).

clock_drift_within_threshold_ready_test() ->
    %% 真实漂移（NTP 正常）远小于默认 5000ms：readiness 恒 ready
    ?SETUP(),
    lists:foreach(
        fun(_) -> ?assertEqual(ready, elib_tsid:readiness()) end,
        lists:seq(1, 3)
    ),
    ?assert(elib_tsid:generate(user) > 0).

clock_drift_threshold_configurable_test_() ->
    %% 同上：防 timetrap 击杀残留漂移注入（见 beyond_threshold 生成器注释）
    {timeout, 60, fun clock_drift_threshold_configurable_body/0}.

clock_drift_threshold_configurable_body() ->
    %% 阈值可配：200ms 上界下 1s 漂移即 unsafe
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{
        dc_id => 1, node_id => 1, dc_bits => 3, max_clock_drift_ms => 200
    }),
    ok = with_runtime_drift(1000, fun() ->
        ?assertEqual(clock_unsafe, elib_tsid:readiness())
    end),
    ?assertEqual(ready, elib_tsid:readiness()).

with_runtime_drift(DriftMs, Fun) ->
    %% 测试注入：改写 runtime 的 clock_offset_ms 制造受控漂移，after 还原
    Runtime = persistent_term:get(elib_tsid_runtime),
    OldOffset = maps:get(clock_offset_ms, Runtime),
    persistent_term:put(
        elib_tsid_runtime,
        Runtime#{clock_offset_ms := OldOffset + DriftMs}
    ),
    try
        Fun()
    after
        persistent_term:put(elib_tsid_runtime, Runtime)
    end.

%% ===================================================================
%% 纯 seam（reserve_candidate / slot_to_id / id_to_slot）
%% ===================================================================

reserve_candidate_fresh_cursor_test() ->
    {ok, First, Last} = elib_tsid:reserve_candidate(0, 100, 5),
    ?assertEqual(100 bsl 11, First),
    ?assertEqual(100 bsl 11 + 4, Last).

reserve_candidate_rollback_holds_cursor_test() ->
    %% 回拨：Now < 游标毫秒 → First 沿旧游标继续，绝不回退
    OldCursor = (500 bsl 11) bor 100,
    {ok, First, _Last} = elib_tsid:reserve_candidate(OldCursor, 400, 1),
    ?assertEqual(OldCursor + 1, First).

reserve_candidate_batch_never_spans_ms_test() ->
    %% 审计修复（V10-A1）：中段起点 + 满容量不再跨毫秒——整体滚入下一
    %% 毫秒 seq 0，终点毫秒恒等于起点毫秒（无借支检查只需覆盖起点）
    {ok, First, Last} = elib_tsid:reserve_candidate((100 bsl 11) bor 2000, 100, 100),
    ?assertEqual(101 bsl 11, First),
    ?assertEqual((101 bsl 11) bor 99, Last),
    ?assertEqual(First bsr 11, Last bsr 11).

reserve_candidate_batch_midms_small_count_stays_test() ->
    %% 中段起点 + 未顶到容量边界的小批量 → 留在当前毫秒
    {ok, First, Last} = elib_tsid:reserve_candidate((100 bsl 11) bor 10, 100, 5),
    ?assertEqual((100 bsl 11) bor 11, First),
    ?assertEqual((100 bsl 11) bor 15, Last).

reserve_candidate_batch_rejects_oversize_test() ->
    %% 单次预留超过单毫秒容量 = 调用方错误（chunk_sizes 上界被绕过）
    ?assertMatch(
        {error, {elib_tsid_invalid_input, _}},
        elib_tsid:reserve_candidate(0, 100, 2049)
    ).

reserve_candidate_before_epoch_test() ->
    ?assertMatch(
        {error, {elib_tsid_clock_before_epoch, _}},
        elib_tsid:reserve_candidate(0, -1, 1)
    ).

reserve_candidate_max_boundary_test() ->
    %% 最后毫秒从 seq 0 起恰好容纳 2048 个（seq 0..2047 不越界）
    {ok, First, Last} = elib_tsid:reserve_candidate(0, ?MAX_REL, 2048),
    ?assertEqual(?MAX_REL bsl 11, First),
    ?assertEqual((?MAX_REL bsl 11) bor 2047, Last),
    %% 游标已在该毫秒 seq>=1 时再要整毫秒 → 越界 typed error
    ?assertMatch(
        {error, {elib_tsid_timestamp_exhausted, _}},
        elib_tsid:reserve_candidate((?MAX_REL bsl 11) bor 1, ?MAX_REL, 2048)
    ).

slot_to_id_layout_test() ->
    ?assertEqual(
        (5 bsl 21) bor (129 bsl 11) bor 7,
        elib_tsid:slot_to_id((5 bsl 11) bor 7, 129)
    ).

id_to_slot_roundtrip_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(user),
    Slot = elib_tsid:id_to_slot(Id),
    #{combined_node := Node} = persistent_term:get(elib_tsid_runtime),
    ?assertEqual(Id, elib_tsid:slot_to_id(Slot, Node)).

id_to_slot_typed_rejection_test() ->
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:id_to_slot(0) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:id_to_slot(-5) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:id_to_slot(?MAX_ID + 1) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:id_to_slot(<<"x">>) end)
    ).

%% ===================================================================
%% 时钟 seam
%% ===================================================================

wall_clock_fixed_offset_test() ->
    %% 生产 wall_clock = monotonic + init 时采集的固定 Offset；
    %% 与真实系统钟的漂移即时接近 0（NTP 正常时）
    ?SETUP(),
    #{clock_offset_ms := Offset} = persistent_term:get(elib_tsid_runtime),
    ?assert(is_integer(Offset)),
    Drift = elib_tsid:wall_clock_ms() - erlang:system_time(millisecond),
    ?assert(abs(Drift) =< 50, {fixed_offset_drift_ms, Drift}).

wall_clock_seam_override_test() ->
    ?SETUP(),
    ?assertEqual(1234567890123, with_fixed_clock(1234567890123, fun elib_tsid:wall_clock_ms/0)),
    %% seam 只作用于调用进程
    Self = self(),
    spawn(fun() -> Self ! {other, elib_tsid:wall_clock_ms()} end),
    receive
        {other, RealMs} ->
            ?assert(abs(RealMs - erlang:system_time(millisecond)) =< 1000)
    after 5000 ->
        exit(seam_isolation_timeout)
    end.

%% ===================================================================
%% 批量语义
%% ===================================================================

batch_sizes_one_cas_test() ->
    ?SETUP(),
    C0 = elib_tsid:reservation_count(),
    _ = elib_tsid:generate_n(user, 100),
    ?assertEqual(C0 + 1, elib_tsid:reservation_count()).

batch_oversized_chunks_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(user, 5000),
    ?assertEqual(5000, length(Ids)),
    ?assertEqual(5000, length(lists:usort(Ids))),
    ?assertEqual(Ids, lists:sort(Ids)).

batch_no_interleave_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + 800000, fun() ->
        Ids = elib_tsid:generate_n(user, 10),
        %% 单调用返回严格连续 slot 区间（无并发干扰时 seq 0..9）
        ?assertEqual(
            lists:seq(0, 9),
            [S || #{sequence := S} <- [elib_tsid:parse(I) || I <- Ids]]
        )
    end).

concurrent_batch_disjoint_ranges_test() ->
    ?SETUP(),
    Self = self(),
    Workers = 8,
    N = 2000,
    [
        spawn(fun() ->
            Ids = elib_tsid:generate_n(user, N),
            Self ! {ids, Ids}
        end)
     || _ <- lists:seq(1, Workers)
    ],
    AllIds = collect_ids(Workers, []),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(Workers * N, length(lists:usort(AllIds))).

%% ===================================================================
%% 高并发混合（跨 namespace 各自独立游标；1M 规模，90s timetrap）
%% ===================================================================

mixed_concurrency_1m_test_() ->
    {timeout, 90, fun mixed_concurrency_1m_body/0}.

mixed_concurrency_1m_body() ->
    %% 无借支合同下批量吞吐上限 = 2048/ms/namespace：1M 突发（最坏全落
    %% 同一 namespace）需 ≥489ms 真实时间推进，默认 100ms 预算必然
    %% capacity_exhausted——显式放大预算，等待真实毫秒推进是契约内行为
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{
        dc_id => 1, node_id => 1, dc_bits => 3, capacity_wait_timeout_ms => 30000
    }),
    Self = self(),
    Workers = 10,
    N = 100000,
    Names = [user, msg_store, group_info, wallet_transaction],
    [
        spawn(fun() ->
            Name = lists:nth(rand:uniform(length(Names)), Names),
            Ids = elib_tsid:generate_n(Name, N),
            Self ! {ids, Name, Ids}
        end)
     || _ <- lists:seq(1, Workers)
    ],
    %% V10：同 namespace 内严格唯一；跨 namespace 数值允许重复，
    %% 故按 namespace 分组后组内断言唯一（全局 usort 会把合法重复合并）
    ByName = collect_by_name(Workers, #{}),
    maps:foreach(
        fun(Name, Ids) ->
            Total = length(Ids),
            ?assertEqual(Total, length(lists:usort(Ids)), {dup_in, Name})
        end,
        ByName
    ),
    ?assertEqual(Workers * N, maps:fold(fun(_K, V, A) -> A + length(V) end, 0, ByName)).

%% ===================================================================
%% 辅助
%% ===================================================================

with_fixed_clock(WallMs, Fun) ->
    put({elib_tsid, test_wall_ms}, WallMs),
    try
        Fun()
    after
        erase({elib_tsid, test_wall_ms})
    end.

catch_typed(Fun) ->
    try
        Fun()
    catch
        error:Reason -> Reason
    end.
