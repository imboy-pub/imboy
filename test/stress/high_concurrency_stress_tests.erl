%% @doc 高并发消息压力测试
%% 测试范围：
%% - 高并发消息发送
%% - 系统资源监控
%% - 稳定性测试
-module(high_concurrency_stress_tests).

-include_lib("eunit/include/eunit.hrl").

%% 压力测试参数
-define(HIGH_CONCURRENT_USERS, 100).     % 高并发用户数
-define(MESSAGES_PER_USER, 50).          % 每用户消息数
-define(TOTAL_MESSAGES, 5000).           % 总消息数
-define(MAX_ACCEPTABLE_FAILURE_RATE, 0.05). % 最大可接受失败率 5%

%% 测试夹具
high_concurrency_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"高并发消息发送压力测试", fun test_high_concurrency_messages/0},
      {"持续消息压力测试", fun test_sustained_message_load/0},
      {"爆发式消息压力测试", fun test_burst_messages/0}
     ]
    }.

setup() ->
    application:set_env(imboy, env, test),
    % 创建大量测试用户
    UserIds = lists:map(fun(N) ->
        {ok, Uid} = create_test_user(<<"stress_user", N/integer>>),
        Uid
    end, lists:seq(1, ?HIGH_CONCURRENT_USERS)),

    % 创建好友关系网格（每个用户与其他部分用户是好友）
    lists:foreach(fun(Uid1) ->
        Friends = lists:filter(fun(Uid2) ->
            Uid1 =/= Uid2 andalso (Uid1 + Uid2) rem 3 =:= 0
        end, UserIds),
        lists:foreach(fun(Uid2) ->
            friend_ds:add_friend(Uid1, Uid2)
        end, Friends)
    end, UserIds),

    #{user_ids => UserIds}.

cleanup(_Context) ->
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_high_concurrency_messages() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),

    io:format("~n========================================~n"),
    io:format("高并发消息压力测试~n"),
    io:format("========================================~n"),
    io:format("用户数: ~p~n", [?HIGH_CONCURRENT_USERS]),
    io:format("每用户消息数: ~p~n", [?MESSAGES_PER_USER]),
    io:format("预期总消息数: ~p~n", [?TOTAL_MESSAGES]),

    Parent = self(),
    StartTime = erlang:monotonic_time(millisecond),

    % 启动并发发送进程
    Pids = lists:map(fun(UserId) ->
        spawn(fun() ->
            Results = lists:map(fun(N) ->
                MsgId = imboy_hashid:uid(),
                % 选择一个好友作为接收者
                FriendId = lists:nth((N rem (?HIGH_CONCURRENT_USERS - 1)) + 1,
                                     lists:delete(UserId, UserIds)),
                MsgData = #{
                    <<"payload">> => <<N/integer, "压力测试消息"/utf8>>,
                    <<"msg_type">> => <<"text">>,
                    <<"action">> => <<"send">>,
                    <<"created_at">> => elib_dt:millisecond()
                },
                try
                    case msg_c2c_logic:c2c(MsgId, UserId, MsgData#{<<"to">> => elib_hashids:encode(FriendId)}) of
                        ok -> success;
                        _ -> failure
                    end
                catch
                    _:_ -> error
                end
            end, lists:seq(1, ?MESSAGES_PER_USER)),
            Parent ! {results, self(), Results}
        end)
    end, UserIds),

    % 收集结果
    AllResults = lists:flatten(lists:map(fun(Pid) ->
        receive
            {results, Pid, Results} -> Results
        after 60000 -> []  % 60秒超时
        end
    end, Pids)),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    % 统计结果
    SuccessCount = length(lists:filter(fun(R) -> R =:= success end, AllResults)),
    FailureCount = length(lists:filter(fun(R) -> R =:= failure end, AllResults)),
    ErrorCount = length(lists:filter(fun(R) -> R =:= error end, AllResults)),
    TotalCount = length(AllResults),

    FailureRate = (FailureCount + ErrorCount) / TotalCount,
    Throughput = TotalCount * 1000 / TotalTime,

    % 输出报告
    io:format("~n----------------------------------------~n"),
    io:format("测试结果:~n"),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  失败数: ~p~n", [FailureCount]),
    io:format("  错误数: ~p~n", [ErrorCount]),
    io:format("  失败率: ~.2f%~n", [FailureRate * 100]),
    io:format("  吞吐量: ~.2f msg/s~n", [Throughput]),
    io:format("========================================~n~n"),

    % 验证
    ?assert(FailureRate < ?MAX_ACCEPTABLE_FAILURE_RATE, "失败率超过阈值"),
    ?assert(SuccessCount >= TotalCount * 0.9, "成功率低于90%"),

    ok.

test_sustained_message_load() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    [User1, User2 | _] = UserIds,

    io:format("~n========================================~n"),
    io:format("持续消息压力测试~n"),
    io:format("========================================~n"),

    % 持续发送消息30秒
    DurationMs = 30000,
    StartTime = erlang:monotonic_time(millisecond),

    Stats = sustain_send_loop(User1, User2, StartTime, DurationMs, #{success => 0, failure => 0, error => 0}),

    EndTime = erlang:monotonic_time(millisecond),
    ActualDuration = EndTime - StartTime,

    SuccessCount = maps:get(success, Stats),
    FailureCount = maps:get(failure, Stats),
    ErrorCount = maps:get(error, Stats),
    TotalCount = SuccessCount + FailureCount + ErrorCount,

    Throughput = TotalCount * 1000 / ActualDuration,

    io:format("~n----------------------------------------~n"),
    io:format("持续压力测试结果:~n"),
    io:format("  实际持续时间: ~p ms~n", [ActualDuration]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  失败数: ~p~n", [FailureCount]),
    io:format("  错误数: ~p~n", [ErrorCount]),
    io:format("  平均吞吐量: ~.2f msg/s~n", [Throughput]),
    io:format("========================================~n~n"),

    ?assert(SuccessCount > 0, "没有成功的消息"),
    ?assert(Throughput > 100, "吞吐量过低"),

    ok.

test_burst_messages() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    [User1, User2 | _] = UserIds,

    io:format("~n========================================~n"),
    io:format("爆发式消息压力测试~n"),
    io:format("========================================~n"),

    % 爆发式发送大量消息
    BurstSize = 1000,
    Parent = self(),

    StartTime = erlang:monotonic_time(millisecond),

    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            MsgId = imboy_hashid:uid(),
            MsgData = #{
                <<"payload">> => <<N/integer, "爆发测试"/utf8>>,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<"send">>,
                <<"created_at">> => elib_dt:millisecond()
            },
            Result = try
                case msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}) of
                    ok -> success;
                    _ -> failure
                end
            catch
                _:_ -> error
            end,
            Parent ! {result, self(), Result}
        end)
    end, lists:seq(1, BurstSize)),

    % 收集结果
    Results = lists:map(fun(Pid) ->
        receive
            {result, Pid, R} -> R
        after 30000 -> timeout
        end
    end, Pids),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    % 统计
    SuccessCount = length(lists:filter(fun(R) -> R =:= success end, Results)),
    FailureCount = length(lists:filter(fun(R) -> R =:= failure end, Results)),
    ErrorCount = length(lists:filter(fun(R) -> R =:= error end, Results)),

    Throughput = BurstSize * 1000 / TotalTime,

    io:format("~n----------------------------------------~n"),
    io:format("爆发测试结果:~n"),
    io:format("  爆发消息数: ~p~n", [BurstSize]),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("  成功数: ~p~n", [SuccessCount]),
    io:format("  失败数: ~p~n", [FailureCount]),
    io:format("  错误数: ~p~n", [ErrorCount]),
    io:format("  峰值吞吐量: ~.2f msg/s~n", [Throughput]),
    io:format("========================================~n~n"),

    ?assert(SuccessCount >= BurstSize * 0.9, "爆发测试成功率低于90%"),

    ok.

%% ===================================================================
%% 内部函数
%% ===================================================================

sustain_send_loop(User1, User2, StartTime, DurationMs, Stats) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    case CurrentTime - StartTime >= DurationMs of
        true ->
            Stats;
        false ->
            MsgId = imboy_hashid:uid(),
            MsgData = #{
                <<"payload">> => <<"持续测试"/utf8>>,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<"send">>,
                <<"created_at">> => elib_dt:millisecond()
            },
            Result = try
                case msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => elib_hashids:encode(User2)}) of
                    ok -> success;
                    _ -> failure
                end
            catch
                _:_ -> error
            end,
            NewStats = maps:update_with(Result, fun(V) -> V + 1 end, 1, Stats),
            sustain_send_loop(User1, User2, StartTime, DurationMs, NewStats)
    end.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    get(test_context).

create_test_user(Nickname) ->
    Uid = imboy_hashid:uid(),
    User = #{
        <<"uid">> => Uid,
        <<"nickname">> => Nickname,
        <<"account">> => Nickname,
        <<"password">> => <<"password123">>,
        <<"created_at">> => elib_dt:millisecond()
    },
    ok = user_repo:create(User),
    {ok, Uid}.
