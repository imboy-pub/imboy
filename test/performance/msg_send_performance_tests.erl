%% @doc 消息发送性能测试
%% 测试范围：
%% - 单聊消息发送性能
%% - 群聊消息发送性能
%% - 批量消息发送性能
%% - 消息查询性能
-module(msg_send_performance_tests).

-include_lib("eunit/include/eunit.hrl").

%% 性能阈值定义
-define(MAX_SINGLE_MSG_TIME_MS, 100).      % 单条消息最大耗时 100ms
-define(MAX_BATCH_MSG_TIME_MS, 5000).      % 批量100条消息最大耗时 5s
-define(MAX_QUERY_TIME_MS, 200).           % 查询最大耗时 200ms
-define(BATCH_COUNT, 100).                 % 批量测试数量

%% 测试夹具
msg_performance_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"单聊消息发送性能", fun test_c2c_send_performance/0},
      {"群聊消息发送性能", fun test_c2g_send_performance/0},
      {"批量消息发送性能", fun test_batch_send_performance/0},
      {"消息查询性能", fun test_query_performance/0},
      {"消息列表加载性能", fun test_list_load_performance/0},
      {"并发发送性能", fun test_concurrent_send_performance/0}
     ]
    }.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    % 创建测试用户
    {ok, User1} = create_test_user(<<"perf_user1">>),
    {ok, User2} = create_test_user(<<"perf_user2">>),
    % 创建好友关系
    ok = ensure_friends(User1, User2),
    % 创建测试群组
    {ok, Group} = create_test_group(User1, <<"perf_test_group">>),
    ok = group_member_ds:add_member(Group, User2),
    Context = #{
        user1 => User1,
        user2 => User2,
        group => Group,
        msg_ids => []
    },
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_c2c_send_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 发送100条单聊消息，计算平均耗时
    Times = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "性能测试消息"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },

        StartTime = erlang:monotonic_time(millisecond),
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),
        EndTime = erlang:monotonic_time(millisecond),

        EndTime - StartTime
    end, lists:seq(1, ?BATCH_COUNT)),

    % 计算统计数据
    AvgTime = lists:sum(Times) / length(Times),
    MaxTime = lists:max(Times),
    MinTime = lists:min(Times),

    % 输出性能报告
    io:format("~n单聊消息发送性能报告:~n"),
    io:format("  总消息数: ~p~n", [?BATCH_COUNT]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),
    io:format("  最大耗时: ~p ms~n", [MaxTime]),
    io:format("  最小耗时: ~p ms~n", [MinTime]),

    % 验证性能阈值
    ?assert(AvgTime =< ?MAX_SINGLE_MSG_TIME_MS, "单聊消息平均耗时超过阈值"),

    ok.

test_c2g_send_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    Group = maps:get(group, Context),

    % 发送100条群聊消息，计算平均耗时
    Times = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "群聊性能测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },

        StartTime = erlang:monotonic_time(millisecond),
        ok = msg_c2g_logic:c2g(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(Group)}),
        EndTime = erlang:monotonic_time(millisecond),

        EndTime - StartTime
    end, lists:seq(1, ?BATCH_COUNT)),

    % 计算统计数据
    AvgTime = lists:sum(Times) / length(Times),
    MaxTime = lists:max(Times),
    MinTime = lists:min(Times),

    % 输出性能报告
    io:format("~n群聊消息发送性能报告:~n"),
    io:format("  总消息数: ~p~n", [?BATCH_COUNT]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),
    io:format("  最大耗时: ~p ms~n", [MaxTime]),
    io:format("  最小耗时: ~p ms~n", [MinTime]),

    % 验证性能阈值
    ?assert(AvgTime =< ?MAX_SINGLE_MSG_TIME_MS, "群聊消息平均耗时超过阈值"),

    ok.

test_batch_send_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 批量发送消息
    StartTime = erlang:monotonic_time(millisecond),

    MsgIds = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "批量测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),
        MsgId
    end, lists:seq(1, ?BATCH_COUNT)),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    % 输出性能报告
    io:format("~n批量消息发送性能报告:~n"),
    io:format("  总消息数: ~p~n", [?BATCH_COUNT]),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("  吞吐量: ~.2f msg/s~n", [?BATCH_COUNT * 1000 / TotalTime]),

    % 验证性能阈值
    ?assert(TotalTime =< ?MAX_BATCH_MSG_TIME_MS, "批量消息发送耗时超过阈值"),

    % 保存消息ID供后续测试使用
    put(batch_msg_ids, MsgIds),

    ok.

test_query_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 先发送一些消息
    MsgIds = lists:map(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "查询测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),
        MsgId
    end, lists:seq(1, 50)),

    % 测试单条消息查询性能
    QueryTimes = lists:map(fun(MsgId) ->
        StartTime = erlang:monotonic_time(millisecond),
        {ok, _} = msg_c2c_repo:find_msg_by_id(MsgId),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, MsgIds),

    AvgQueryTime = lists:sum(QueryTimes) / length(QueryTimes),

    % 输出性能报告
    io:format("~n消息查询性能报告:~n"),
    io:format("  查询次数: ~p~n", [length(MsgIds)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgQueryTime]),

    % 验证性能阈值
    ?assert(AvgQueryTime =< ?MAX_QUERY_TIME_MS, "消息查询平均耗时超过阈值"),

    ok.

test_list_load_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 先发送200条消息
    lists:foreach(fun(N) ->
        MsgId = imboy_hashid:uid(),
        MsgData = #{
            <<"payload">> => <<N/integer, "列表测试"/utf8>>,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"send">>,
            <<"created_at">> => elib_dt:millisecond()
        },
        ok = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)})
    end, lists:seq(1, 200)),

    % 测试分页加载性能
    PageSize = 20,
    LoadTimes = lists:map(fun(Page) ->
        Offset = (Page - 1) * PageSize,
        StartTime = erlang:monotonic_time(millisecond),
        {ok, _} = msg_c2c_repo:list(User1, User2, #{offset => Offset, limit => PageSize}),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, 10)),

    AvgLoadTime = lists:sum(LoadTimes) / length(LoadTimes),

    % 输出性能报告
    io:format("~n消息列表加载性能报告:~n"),
    io:format("  每页数量: ~p~n", [PageSize]),
    io:format("  加载次数: ~p~n", [length(LoadTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgLoadTime]),

    % 验证性能阈值
    ?assert(AvgLoadTime =< ?MAX_QUERY_TIME_MS, "消息列表加载平均耗时超过阈值"),

    ok.

test_concurrent_send_performance() ->
    Context = get_context(),
    User1 = maps:get(user1, Context),
    User2 = maps:get(user2, Context),

    % 并发发送消息
    ConcurrentCount = 50,
    Parent = self(),

    StartTime = erlang:monotonic_time(millisecond),

    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            MsgId = imboy_hashid:uid(),
            MsgData = #{
                <<"payload">> => <<N/integer, "并发测试"/utf8>>,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<"send">>,
                <<"created_at">> => elib_dt:millisecond()
            },
            Result = msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}),
            Parent ! {done, self(), Result}
        end)
    end, lists:seq(1, ConcurrentCount)),

    % 等待所有进程完成
    Results = lists:map(fun(Pid) ->
        receive
            {done, Pid, Result} -> Result
        after 10000 -> timeout
        end
    end, Pids),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    % 统计成功数量
    SuccessCount = length(lists:filter(fun(R) -> R =:= ok end, Results)),

    % 输出性能报告
    io:format("~n并发发送性能报告:~n"),
    io:format("  并发数: ~p~n", [ConcurrentCount]),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  吞吐量: ~.2f msg/s~n", [SuccessCount * 1000 / TotalTime]),

    % 验证性能
    ?assert(SuccessCount >= ConcurrentCount * 0.95, "并发发送成功率低于95%"),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

ensure_friends(User1, User2) ->
    NowTs = elib_dt:now(),
    ok = friend_ds:confirm_friend(friend_ds:is_friend(User1, User2),
                                  User1,
                                  User2,
                                  <<>>,
                                  #{<<"is_from">> => 1, <<"source">> => <<"test">>},
                                  <<>>,
                                  NowTs),
    ok = friend_ds:confirm_friend(friend_ds:is_friend(User2, User1),
                                  User2,
                                  User1,
                                  <<>>,
                                  #{<<"source">> => <<"test">>},
                                  <<>>,
                                  NowTs),
    ok = friend_ds:invalidate_cache(User1, User2),
    imboy_cache:flush({check_relationship3, User1, User2}),
    imboy_cache:flush({check_relationship3, User2, User1}),
    ok.

create_test_user(Nickname) ->
    Uid = binary_to_integer(imboy_hashid:uid()),
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
    Gid = binary_to_integer(imboy_hashid:uid()),
    Group = #{
        <<"gid">> => Gid,
        <<"owner_uid">> => OwnerId,
        <<"name">> => Name,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = group_repo:create(Group),
    ok = group_member_ds:add_member(Gid, OwnerId),
    {ok, Gid}.
