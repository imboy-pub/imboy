-module(elib_tsid_tests).
-include_lib("eunit/include/eunit.hrl").

-define(SETUP, fun() -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3}) end).
-define(SETUP_NAMED, fun() ->
    elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3,
                     names => [user, group_info, attachment]})
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
    Pids = [spawn(fun() ->
        Ids = elib_tsid:generate_n(user, N),
        Self ! {ids, Ids}
    end) || _ <- lists:seq(1, Workers)],
    AllIds = collect_ids(Workers, []),
    UniqueIds = lists:usort(AllIds),
    ?assertEqual(Workers * N, length(AllIds)),
    ?assertEqual(length(AllIds), length(UniqueIds)),
    _ = Pids,
    ok.

unregistered_generator_error_test() ->
    ?SETUP(),
    %% 使用未注册的生成器应该报错
    ?assertError({elib_tsid_generator_not_registered, _},
                 elib_tsid:generate(nonexistent_table)).

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
