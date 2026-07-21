%% @doc 数据库查询性能测试
%% 测试范围：
%% - 用户查询性能
%% - 群组查询性能
%% - 消息查询性能
%% - 复杂查询性能
%% - 索引效果验证
-module(db_query_performance_tests).

-include_lib("eunit/include/eunit.hrl").

%% 性能阈值定义

% 简单查询最大耗时 50ms
-define(MAX_SIMPLE_QUERY_MS, 50).
% 复杂查询最大耗时 200ms
-define(MAX_COMPLEX_QUERY_MS, 200).
% 连接查询最大耗时 300ms
-define(MAX_JOIN_QUERY_MS, 300).

%% 测试夹具
db_performance_test_() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} ->
            {foreach, fun setup/0, fun cleanup/1, [
                {"用户查询性能", fun test_user_query_performance/0},
                {"群组查询性能", fun test_group_query_performance/0},
                {"群成员列表查询性能", fun test_group_member_list_performance/0},
                {"复杂连接查询性能", fun test_complex_join_performance/0}
            ]};
        {error, _Reason} ->
            {"Database not available", fun() -> {skip, "Database not available"} end}
    end.

setup() ->
    % 创建测试数据
    {ok, User1} = create_test_user(<<"db_perf_user1">>),

    % 创建大量用户
    UserIds = lists:map(
        fun(N) ->
            {ok, Uid} = create_test_user(<<"db_perf_user", N/integer>>),
            Uid
        end,
        lists:seq(2, 102)
    ),

    % 创建好友关系
    lists:foreach(
        fun(Uid) ->
            ok = ensure_friends(User1, Uid)
        end,
        UserIds
    ),

    % 创建群组
    {ok, Group} = create_test_group(User1, <<"db_perf_group">>),

    % 添加群成员
    lists:foreach(
        fun(Uid) ->
            ok = group_member_ds:add_member(Group, Uid)
        end,
        UserIds
    ),

    Context = #{
        user1 => User1,
        user_ids => UserIds,
        group => Group
    },
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_user_query_performance() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),

    % 测试单用户查询
    QueryTimes = lists:map(
        fun(Uid) ->
            StartTime = erlang:monotonic_time(millisecond),
            {ok, _} = user_repo:find_by_uid(Uid),
            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end,
        lists:sublist(UserIds, 50)
    ),

    AvgTime = lists:sum(QueryTimes) / length(QueryTimes),

    io:format("~n用户查询性能报告:~n"),
    io:format("  查询次数: ~p~n", [length(QueryTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),
    io:format("  最大耗时: ~p ms~n", [lists:max(QueryTimes)]),

    ?assert(AvgTime =< ?MAX_SIMPLE_QUERY_MS, "用户查询平均耗时超过阈值"),

    ok.

test_group_query_performance() ->
    Context = get_context(),
    Group = maps:get(group, Context),

    % 测试群组信息查询
    QueryTimes = lists:map(
        fun(_) ->
            StartTime = erlang:monotonic_time(millisecond),
            {ok, _} = group_repo:find_by_gid(Group),
            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end,
        lists:seq(1, 50)
    ),

    AvgTime = lists:sum(QueryTimes) / length(QueryTimes),

    io:format("~n群组查询性能报告:~n"),
    io:format("  查询次数: ~p~n", [length(QueryTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    ?assert(AvgTime =< ?MAX_SIMPLE_QUERY_MS, "群组查询平均耗时超过阈值"),

    ok.

test_group_member_list_performance() ->
    Context = get_context(),
    Group = maps:get(group, Context),

    % 测试群成员列表查询
    QueryTimes = lists:map(
        fun(_) ->
            StartTime = erlang:monotonic_time(millisecond),
            {ok, _} = group_member_ds:list_members(Group),
            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end,
        lists:seq(1, 20)
    ),

    AvgTime = lists:sum(QueryTimes) / length(QueryTimes),

    io:format("~n群成员列表查询性能报告:~n"),
    io:format("  成员数量: ~p~n", [101]),
    io:format("  查询次数: ~p~n", [length(QueryTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    ?assert(AvgTime =< ?MAX_COMPLEX_QUERY_MS, "群成员列表查询平均耗时超过阈值"),

    ok.

test_complex_join_performance() ->
    Context = get_context(),
    Group = maps:get(group, Context),

    % 测试复杂连接查询（群成员 + 用户信息）
    QueryTimes = lists:map(
        fun(_) ->
            StartTime = erlang:monotonic_time(millisecond),
            {ok, _} = group_member_ds:list_members_with_info(Group),
            EndTime = erlang:monotonic_time(millisecond),
            EndTime - StartTime
        end,
        lists:seq(1, 10)
    ),

    AvgTime = lists:sum(QueryTimes) / length(QueryTimes),

    io:format("~n复杂连接查询性能报告:~n"),
    io:format("  成员数量: ~p~n", [101]),
    io:format("  查询次数: ~p~n", [length(QueryTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    ?assert(AvgTime =< ?MAX_JOIN_QUERY_MS, "复杂连接查询平均耗时超过阈值"),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

ensure_friends(User1, User2) ->
    NowTs = elib_dt:now(),
    ok = friend_ds:confirm_friend(
        friend_ds:is_friend(User1, User2),
        User1,
        User2,
        <<>>,
        #{<<"is_from">> => 1, <<"source">> => <<"test">>},
        <<>>,
        NowTs
    ),
    ok = friend_ds:confirm_friend(
        friend_ds:is_friend(User2, User1),
        User2,
        User1,
        <<>>,
        #{<<"source">> => <<"test">>},
        <<>>,
        NowTs
    ),
    ok = friend_ds:invalidate_cache(User1, User2),
    imboy_cache:flush({check_relationship3, User1, User2}),
    imboy_cache:flush({check_relationship3, User2, User1}),
    ok.

create_test_user(Nickname) ->
    Uid = elib_tsid:generate(),
    Suffix = integer_to_binary(erlang:phash2(Uid, 1000000000)),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => <<Nickname/binary, "_", Suffix/binary>>,
        <<"mobile">> => list_to_binary(io_lib:format("13~9..0B", [erlang:phash2(Uid, 1000000000)])),
        <<"email">> => <<"test_", Suffix/binary, "@example.com">>,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.

create_test_group(OwnerId, Name) ->
    Gid = elib_tsid:generate(),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
