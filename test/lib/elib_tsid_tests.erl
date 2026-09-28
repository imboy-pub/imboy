-module(elib_tsid_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() ->
    %% TSID-02 起不同配置 re-init 被拒绝（F-12），每个测试先显式
    %% reset 再 init，保证测试顺序无关的确定性隔离
    elib_tsid:reset_for_test(),
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3})
end).
-define(SETUP_NAMED, fun() ->
    elib_tsid:reset_for_test(),
    elib_tsid:init(#{
        dc_id => 1,
        node_id => 1,
        dc_bits => 3,
        names => [user, group_info, attachment]
    })
end).

%% ===================================================================
%% 基础功能测试
%% ===================================================================

init_test() ->
    ?SETUP(),
    ok.

generate_positive_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    ?assert(Id > 0),
    ?assert(is_integer(Id)).

generate_within_bigint_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    %% 2^63 - 1
    MaxBigint = 9223372036854775807,
    ?assert(Id > 0),
    ?assert(Id =< MaxBigint).

generate_max_19_digits_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    Digits = length(integer_to_list(Id)),
    ?assert(Digits =< 19).

%% ===================================================================
%% 唯一性测试
%% ===================================================================

uniqueness_sequential_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(10000),
    UniqueIds = lists:usort(Ids),
    ?assertEqual(length(Ids), length(UniqueIds)).

uniqueness_concurrent_test() ->
    ?SETUP(),
    Self = self(),
    N = 1000,
    Workers = 10,
    %% 启动 10 个并发进程, 每个生成 1000 个 ID
    Pids = [
        spawn(fun() ->
            Ids = elib_tsid:generate_n(N),
            Self ! {ids, Ids}
        end)
     || _ <- lists:seq(1, Workers)
    ],
    AllIds = collect_ids(Workers, []),
    UniqueIds = lists:usort(AllIds),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(length(AllIds), length(UniqueIds)),
    _ = Pids,
    ok.

collect_ids(0, Acc) ->
    Acc;
collect_ids(N, Acc) ->
    receive
        {ids, Ids} -> collect_ids(N - 1, Ids ++ Acc)
    after 5000 ->
        error(timeout)
    end.

%% ===================================================================
%% 单调递增测试
%% ===================================================================

monotonic_test() ->
    ?SETUP(),
    Ids = elib_tsid:generate_n(5000),
    ?assertEqual(Ids, lists:sort(Ids)).

%% ===================================================================
%% 解析测试
%% ===================================================================

parse_roundtrip_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    Parsed = elib_tsid:parse(Id),
    ?assertEqual(Id, maps:get(id, Parsed)),
    ?assertEqual(1, maps:get(dc_id, Parsed)),
    ?assertEqual(1, maps:get(node_id, Parsed)),
    ?assert(maps:get(sequence, Parsed) >= 0),
    ?assert(maps:get(timestamp, Parsed) > 1735689600000).

timestamp_extraction_test() ->
    ?SETUP(),
    Before = erlang:system_time(millisecond),
    Id = elib_tsid:generate(),
    After = erlang:system_time(millisecond),
    Ts = elib_tsid:timestamp(Id),
    ?assert(Ts >= Before),
    ?assert(Ts =< After + 1).

node_id_extraction_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    %% dc_bits=3, dc_id=1, node_id=1 → combined = (1 bsl 7) bor 1 = 129
    ?assertEqual(129, elib_tsid:node_id(Id)).

%% ===================================================================
%% Base62 测试
%% ===================================================================

base62_roundtrip_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    Encoded = elib_tsid:to_base62(Id),
    Decoded = elib_tsid:from_base62(Encoded),
    ?assertEqual(Id, Decoded).

base62_length_test() ->
    ?SETUP(),
    Id = elib_tsid:generate(),
    Encoded = elib_tsid:to_base62(Id),
    %% Base62 编码 2^63 ≈ 62^10.7, 所以最长 11 个字符
    ?assert(byte_size(Encoded) =< 11).

%% ===================================================================
%% DC/Node 配置测试
%% ===================================================================

dc_bits_0_test() ->
    %% TSID-02 起不同配置 re-init 被拒绝（F-12），跨配置测试须显式 reset
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 0, node_id => 500, dc_bits => 0}),
    Id = elib_tsid:generate(),
    ?assert(Id > 0).

dc_bits_5_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 31, node_id => 31, dc_bits => 5}),
    Id = elib_tsid:generate(),
    Parsed = elib_tsid:parse(Id),
    ?assertEqual(31, maps:get(dc_id, Parsed)),
    ?assertEqual(31, maps:get(node_id, Parsed)).

%% ===================================================================
%% 边界测试
%% ===================================================================

different_nodes_no_collision_test() ->
    %% 模拟两个不同节点，验证 ID 不冲突
    %% TSID-02 起跨配置场景须显式 reset（F-12：禁止静默重配节点）
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    IdsNode1 = elib_tsid:generate_n(1000),

    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 2, dc_bits => 3}),
    IdsNode2 = elib_tsid:generate_n(1000),

    Combined = IdsNode1 ++ IdsNode2,
    Unique = lists:usort(Combined),
    ?assertEqual(2000, length(Unique)).

%% ===================================================================
%% 命名生成器测试
%% ===================================================================

register_single_test() ->
    ?SETUP(),
    ok = elib_tsid:register(user),
    Names = elib_tsid:registered(),
    ?assert(lists:member(user, Names)).

register_list_test() ->
    ?SETUP(),
    ok = elib_tsid:register([group_info, attachment, channel]),
    Names = elib_tsid:registered(),
    ?assert(lists:member(group_info, Names)),
    ?assert(lists:member(attachment, Names)),
    ?assert(lists:member(channel, Names)).

register_idempotent_test() ->
    ?SETUP(),
    ok = elib_tsid:register(feedback),
    Names1 = elib_tsid:registered(),
    ok = elib_tsid:register(feedback),
    Names2 = elib_tsid:registered(),
    ?assertEqual(Names1, Names2).

init_with_names_test() ->
    ?SETUP_NAMED(),
    Names = elib_tsid:registered(),
    ?assert(lists:member(default, Names)),
    ?assert(lists:member(user, Names)),
    ?assert(lists:member(group_info, Names)),
    ?assert(lists:member(attachment, Names)).

generate_named_test() ->
    ?SETUP_NAMED(),
    UserId = elib_tsid:generate(user),
    GroupId = elib_tsid:generate(group_info),
    AttachId = elib_tsid:generate(attachment),
    ?assert(UserId > 0),
    ?assert(GroupId > 0),
    ?assert(AttachId > 0).

generate_named_unique_within_test() ->
    %% 同一命名生成器内 ID 唯一
    ?SETUP_NAMED(),
    UserIds = elib_tsid:generate_n(user, 5000),
    UniqueIds = lists:usort(UserIds),
    ?assertEqual(5000, length(UniqueIds)).

generate_named_monotonic_test() ->
    %% 同一命名生成器内 ID 单调递增
    ?SETUP_NAMED(),
    UserIds = elib_tsid:generate_n(user, 5000),
    ?assertEqual(UserIds, lists:sort(UserIds)).

named_generators_independent_test() ->
    %% 不同生成器拥有独立的 sequence 计数器
    %% 同一毫秒内可能产生相同数值的 ID (这是预期行为)
    ?SETUP_NAMED(),
    UserIds = elib_tsid:generate_n(user, 100),
    GroupIds = elib_tsid:generate_n(group_info, 100),
    %% 各自内部唯一
    ?assertEqual(100, length(lists:usort(UserIds))),
    ?assertEqual(100, length(lists:usort(GroupIds))),
    %% 但跨生成器可能有交集 (独立号段的正常行为)
    ok.

named_concurrent_unique_test() ->
    %% 同一命名生成器并发下 ID 唯一
    ?SETUP_NAMED(),
    Self = self(),
    N = 500,
    Workers = 10,
    Pids = [
        spawn(fun() ->
            Ids = elib_tsid:generate_n(user, N),
            Self ! {ids, Ids}
        end)
     || _ <- lists:seq(1, Workers)
    ],
    AllIds = collect_ids(Workers, []),
    UniqueIds = lists:usort(AllIds),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(length(AllIds), length(UniqueIds)),
    _ = Pids,
    ok.

unregistered_generator_error_test() ->
    ?SETUP(),
    %% 使用未注册的生成器应该报错
    ?assertError(
        {elib_tsid_generator_not_registered, _},
        elib_tsid:generate(nonexistent_table)
    ).

default_generator_always_available_test() ->
    ?SETUP(),
    %% default 生成器始终可用
    Id = elib_tsid:generate(default),
    ?assert(Id > 0),
    %% generate/0 等同于 generate(default)
    Id2 = elib_tsid:generate(),
    ?assert(Id2 > Id).

registered_includes_default_test() ->
    ?SETUP(),
    Names = elib_tsid:registered(),
    ?assert(lists:member(default, Names)).

from_binary_test() ->
    ?assertEqual({ok, 123}, elib_tsid:from_binary(<<"123">>)),
    ?assertEqual(error, elib_tsid:from_binary(<<"0">>)),
    ?assertEqual(error, elib_tsid:from_binary(<<"-1">>)),
    ?assertEqual(error, elib_tsid:from_binary(<<"bad">>)),
    ?assertEqual(error, elib_tsid:from_binary(123)).

%% ===================================================================
%% TSID-01：时钟与 reservation 契约冻结
%%
%% 全部为固定时钟纯模型测试：时钟以参数注入，不依赖真实 sleep、
%% 不修改系统时钟（AC-01A）。seam 函数为内部实现细节，不属于
%% 公开 API 契约（AC-01B：公开 arity 与健康路径返回不变）。
%% ===================================================================

-define(MAX_REL_TS, 4398046511103).
-define(LAST_VALID_SLOT, ((?MAX_REL_TS bsl 11) bor 2047)).

reserve_candidate_fresh_cursor_test() ->
    %% 全新 cursor（-1）：首个 slot 落在当前毫秒，seq=0
    {ok, First, Last} = elib_tsid:reserve_candidate(-1, 1000, 1),
    ?assertEqual(1000 bsl 11, First),
    ?assertEqual(First, Last).

reserve_candidate_rollback_holds_cursor_test() ->
    %% 时钟回拨（1500 < cursor 毫秒 2000）：不回退，从 cursor 继续
    Old = 2000 bsl 11,
    {ok, First, _Last} = elib_tsid:reserve_candidate(Old, 1500, 1),
    ?assertEqual(Old + 1, First).

reserve_candidate_batch_spans_ms_test() ->
    %% batch 从 seq 末尾跨入下一毫秒：线性 slot 连续展开
    Old = (2000 bsl 11) bor 2047,
    {ok, First, Last} = elib_tsid:reserve_candidate(Old, 2000, 2),
    ?assertEqual(Old + 1, First),
    ?assertEqual((2001 bsl 11) bor 1, Last).

reserve_candidate_before_epoch_test() ->
    %% 纪元前时钟：typed fail-closed，绝不借用未来毫秒
    ?assertMatch(
        {error, {elib_tsid_clock_before_epoch, _}},
        elib_tsid:reserve_candidate(-1, -1, 1)
    ).

reserve_candidate_max_boundary_test() ->
    %% 恰可容纳 42-bit 最后一个 slot；再多 1 个 → typed exhausted
    {ok, _First, Last} = elib_tsid:reserve_candidate(?LAST_VALID_SLOT - 1, ?MAX_REL_TS, 1),
    ?assertEqual(?LAST_VALID_SLOT, Last),
    ?assertMatch(
        {error, {elib_tsid_timestamp_exhausted, _}},
        elib_tsid:reserve_candidate(?LAST_VALID_SLOT, ?MAX_REL_TS, 1)
    ).

reserve_candidate_conflict_refresh_test() ->
    %% CAS 冲突模型：获胜者推进 cursor 后，失败者以刷新后的墙钟重算，
    %% candidate 仍为 max(old + 1, now << 11)（F-05 冻结语义）
    {ok, _F1, L1} = elib_tsid:reserve_candidate(-1, 1000, 1),
    {ok, F2, _L2} = elib_tsid:reserve_candidate(L1, 1100, 1),
    ?assertEqual(max(L1 + 1, 1100 bsl 11), F2).

slot_to_id_layout_test() ->
    %% slot → ID 展开：42/10/11 布局精确可表达，MAX_ID 是最后合法值
    Slot = (1000 bsl 11) bor 5,
    ?assertEqual((1000 bsl 21) bor (129 bsl 11) bor 5, elib_tsid:slot_to_id(Slot, 129)),
    ?assertEqual(9223372036854775807, elib_tsid:slot_to_id(?LAST_VALID_SLOT, 1023)).

id_to_slot_roundtrip_test() ->
    Id = (12345 bsl 21) bor (129 bsl 11) bor 77,
    ?assertEqual(Id, elib_tsid:slot_to_id(elib_tsid:id_to_slot(Id), 129)).

wall_clock_seam_test() ->
    %% 私有时钟 seam：默认真实墙钟；pdict 注入后完全受控（进程隔离）
    Real = elib_tsid:wall_clock_ms(),
    Now = erlang:system_time(millisecond),
    ?assert(Real >= Now - 5000 andalso Real =< Now + 5),
    put({elib_tsid, test_wall_ms}, 12345),
    ?assertEqual(12345, elib_tsid:wall_clock_ms()),
    erase({elib_tsid, test_wall_ms}),
    Real2 = elib_tsid:wall_clock_ms(),
    ?assert(Real2 >= Real).

monotonic_clock_seam_test() ->
    %% 私有单调钟 seam：默认真实单调钟（仅用于测间隔/超时，绝不入 ID）
    M1 = elib_tsid:monotonic_ms(),
    ?assert(M1 =< erlang:monotonic_time(millisecond) + 5),
    put({elib_tsid, test_monotonic_ms}, 42),
    ?assertEqual(42, elib_tsid:monotonic_ms()),
    erase({elib_tsid, test_monotonic_ms}).

%% ===================================================================
%% TSID-02：63-bit 边界、输入校验与 calendar 精度（T-001..T-010）
%%
%% AC-02A 非法输入不再掩码成合法 ID；AC-02B MAX_ID 精确；
%% AC-02C 全部生成值落在正 signed BIGINT；AC-02D calendar 精度如实声明。
%% ===================================================================

-define(EPOCH_MS, 1735689600000).
-define(MAX_ID, 9223372036854775807).
-define(TSID02_MAX_REL, 4398046511103).

catch_typed(Fun) ->
    %% 捕获 error:Reason 并返回 Reason，避免已弃用的裸 catch 表达式
    try
        Fun()
    catch
        error:Reason -> Reason
    end.

with_fixed_clock(WallMs, Fun) ->
    put({elib_tsid, test_wall_ms}, WallMs),
    try
        Fun()
    after
        erase({elib_tsid, test_wall_ms})
    end.

%% T-001 42/10/11 编解码 golden vectors
golden_vectors_test() ->
    Vectors = [
        {0, 0, 1},
        {0, 1, 0},
        {0, 1023, 2047},
        {1, 0, 0},
        {1, 129, 5},
        {5619271, 517, 1234},
        {?TSID02_MAX_REL, 1023, 2047}
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
    %% 全零不是合法 TSID（须 >= 1）
    ?assertMatch({elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:parse(0) end)),
    ?assertMatch({elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:parse(-1) end)),
    ?assertMatch(
        {elib_tsid_invalid_input, _},
        catch_typed(fun() -> elib_tsid:parse(not_integer) end)
    ).

%% T-002 MAX_ID 解析：字段 exact、正数、不掩码
max_id_parse_test() ->
    P = elib_tsid:parse(?MAX_ID),
    ?assertEqual(?MAX_ID, maps:get(id, P)),
    ?assertEqual(?EPOCH_MS + ?TSID02_MAX_REL, maps:get(timestamp, P)),
    ?assertEqual(1023, elib_tsid:node_id(?MAX_ID)),
    ?assertEqual(2047, maps:get(sequence, P)),
    %% dc_bits 默认 3 → 1023 = (7 bsl 7) bor 127
    ?assertEqual(7, maps:get(dc_id, P)),
    ?assertEqual(127, maps:get(node_id, P)),
    ?assertMatch({{2164, 5, 15}, {7, 35, 11}}, maps:get(created_at, P)),
    %% 超界输入必须稳定拒绝，不得掩码成别的 TSID（F-09）
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

%% T-003 最后合法毫秒用尽 → typed exhausted，绝不返回越界 ID
last_ms_exhaustion_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    LastId = with_fixed_clock(?EPOCH_MS + ?TSID02_MAX_REL, fun() ->
        Ids = [elib_tsid:generate() || _ <- lists:seq(1, 2048)],
        ?assertEqual(2048, length(lists:usort(Ids))),
        %% node=129：最后合法 ID = (MAX_REL bsl 21) bor (129 bsl 11) bor 2047
        ?assertEqual((?TSID02_MAX_REL bsl 21) bor (129 bsl 11) bor 2047, lists:last(Ids)),
        lists:last(Ids)
    end),
    ?assert(LastId =< ?MAX_ID),
    %% 第 2049 个：借用下一毫秒 = 越界 → typed error，不签发
    with_fixed_clock(?EPOCH_MS + ?TSID02_MAX_REL, fun() ->
        ?assertMatch(
            {elib_tsid_timestamp_exhausted, _}, catch_typed(fun() -> elib_tsid:generate() end)
        )
    end).

%% T-004 纪元前时钟 → 生成 fail-closed（确定性时钟，无 sleep/改系统钟）
clock_beyond_2164_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS + ?TSID02_MAX_REL + 5000, fun() ->
        ?assertMatch(
            {elib_tsid_timestamp_exhausted, _}, catch_typed(fun() -> elib_tsid:generate() end)
        )
    end).

before_epoch_fail_closed_test() ->
    ?SETUP(),
    with_fixed_clock(?EPOCH_MS - 1000, fun() ->
        ?assertMatch(
            {elib_tsid_clock_before_epoch, _}, catch_typed(fun() -> elib_tsid:generate() end)
        )
    end).

%% T-005 decimal 边界 corpus：仅 1..MAX_ID 的十进制接受
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

%% T-006 Base62 corpus：合法 round-trip；非 TSID 稳定拒绝（F-10）
base62_corpus_test() ->
    lists:foreach(
        fun(Id) ->
            ?assertEqual(Id, elib_tsid:from_base62(elib_tsid:to_base62(Id)))
        end,
        [1, 42, 2097152, 123456789012345, ?MAX_ID]
    ),
    Reject = [
        <<>>,
        <<"0">>,
        <<"a!c">>,
        <<"-z">>,
        <<" ">>,
        <<"zzzzzzzzzzz">>,
        <<"zzzzzzzzzzzz">>
    ],
    lists:foreach(
        fun(Bin) ->
            ?assertMatch(
                {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:from_base62(Bin) end)
            )
        end,
        Reject
    ),
    %% 编码方向同样拒绝非 TSID
    ?assertMatch({elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:to_base62(0) end)),
    ?assertMatch(
        {elib_tsid_invalid_input, _}, catch_typed(fun() -> elib_tsid:to_base62(?MAX_ID + 1) end)
    ),
    ?assertMatch(
        {elib_tsid_invalid_input, _},
        catch_typed(fun() -> elib_tsid:to_base62(-1) end)
    ).

%% T-007 calendar 精度：timestamp 保留毫秒，created_at 是秒精度（F-11）
calendar_precision_test() ->
    P = elib_tsid:parse(?MAX_ID),
    ?assertEqual(6133736111103, maps:get(timestamp, P)),
    ?assertMatch({{2164, 5, 15}, {7, 35, 11}}, maps:get(created_at, P)).

%% T-008 dc_bits 0..10 全边界：CombinedNode exact，无负移位/越界
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
            Id = elib_tsid:generate(),
            ?assert(Id > 0),
            ?assert(Id =< ?MAX_ID),
            P = elib_tsid:parse(Id),
            ?assertEqual(MaxDc, maps:get(dc_id, P)),
            ?assertEqual(MaxNode, maps:get(node_id, P))
        end,
        lists:seq(0, 10)
    ).

%% T-009 重复同配置 init：幂等且 cursor 不回退
same_config_reinit_idempotent_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id1 = elib_tsid:generate(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id2 = elib_tsid:generate(),
    ?assert(Id2 > Id1).

%% T-010 不同配置 re-init：typed reject，旧 runtime 不变（F-12）
different_config_reinit_rejected_test() ->
    elib_tsid:reset_for_test(),
    ok = elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    Id1 = elib_tsid:generate(),
    ?assertMatch(
        {elib_tsid_already_initialized, _},
        catch_typed(fun() -> elib_tsid:init(#{dc_id => 1, node_id => 2, dc_bits => 3}) end)
    ),
    ?assertMatch(
        {elib_tsid_already_initialized, _},
        catch_typed(fun() -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 4}) end)
    ),
    %% 旧 runtime 不变：继续按旧节点生成且不回退
    Id2 = elib_tsid:generate(),
    ?assert(Id2 > Id1),
    ?assertEqual(129, elib_tsid:node_id(Id2)).
