-module(elib_tsid_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}) end).

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
    MaxBigint = 9223372036854775807,  %% 2^63 - 1
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
    Pids = [spawn(fun() ->
        Ids = elib_tsid:generate_n(N),
        Self ! {ids, Ids}
    end) || _ <- lists:seq(1, Workers)],
    AllIds = collect_ids(Workers, []),
    UniqueIds = lists:usort(AllIds),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(length(AllIds), length(UniqueIds)),
    _ = Pids,
    ok.

collect_ids(0, Acc) -> Acc;
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
    elib_tsid:init(#{dc_id => 0, node_id => 500, dc_bits => 0}),
    Id = elib_tsid:generate(),
    ?assert(Id > 0).

dc_bits_5_test() ->
    elib_tsid:init(#{dc_id => 31, node_id => 31, dc_bits => 5}),
    Id = elib_tsid:generate(),
    Parsed = elib_tsid:parse(Id),
    ?assertEqual(31, maps:get(dc_id, Parsed)),
    ?assertEqual(31, maps:get(node_id, Parsed)).

%% ===================================================================
%% 边界测试
%% ===================================================================

different_nodes_no_collision_test() ->
    %% 模拟两个不同节点，验证 ID 不冲突
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}),
    IdsNode1 = elib_tsid:generate_n(1000),

    elib_tsid:init(#{dc_id => 1, node_id => 2, dc_bits => 3}),
    IdsNode2 = elib_tsid:generate_n(1000),

    Combined = IdsNode1 ++ IdsNode2,
    Unique = lists:usort(Combined),
    ?assertEqual(2000, length(Unique)).
