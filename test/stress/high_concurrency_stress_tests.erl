%% @doc 高并发消息压力测试
%% 测试范围：
%% - 高并发消息发送
%% - 系统资源监控
%% - 稳定性测试
-module(high_concurrency_stress_tests).

-include_lib("eunit/include/eunit.hrl").

%% 压力测试参数
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
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    case eunit_runner:eunit_try_db() of
        {ok, _Driver, _Conn} -> ok;
        {error, _Reason} -> throw({skip, "Database not available"})
    end,
    %% 确保 elib_tsid 已初始化 —— 当 imboy app 未启动时（如独立跑测试），
    %% eunit_runner 走 app_not_started 分支，elib_tsid:init/1 不会被调用，
    %% 此处幂等地兜底初始化，避免 create_test_user 调用 generate/1 crash。
    _ = (catch elib_tsid:init(#{dc_id => 0, node_id => 0, dc_bits => 3})),
    Profile = stress_profile(),
    UserCount = maps:get(user_count, Profile),
    % 创建大量测试用户
    UserIds = lists:map(fun(N) ->
        {ok, Uid} = create_test_user(<<"stress_user", N/integer>>),
        Uid
    end, lists:seq(1, UserCount)),

    % 先建立一个稳定的环状好友关系，保证每个用户至少有一个可发送对象
    RingPairs = lists:zip(UserIds, tl(UserIds) ++ [hd(UserIds)]),
    lists:foreach(fun({Uid1, Uid2}) ->
        ensure_friends(Uid1, Uid2)
    end, RingPairs),

    % 创建好友关系网格（每个用户与其他部分用户是好友）
    lists:foreach(fun(Uid1) ->
        Friends = lists:filter(fun(Uid2) ->
            Uid1 =/= Uid2 andalso (Uid1 + Uid2) rem 3 =:= 0
        end, UserIds),
        lists:foreach(fun(Uid2) ->
            ensure_friends(Uid1, Uid2)
        end, Friends)
    end, UserIds),

    Context = #{user_ids => UserIds, profile => Profile},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_high_concurrency_messages() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    Profile = maps:get(profile, Context),
    UserCount = maps:get(user_count, Profile),
    MessagesPerUser = maps:get(messages_per_user, Profile),
    TotalMessages = UserCount * MessagesPerUser,

    io:format("~n========================================~n"),
    io:format("高并发消息压力测试~n"),
    io:format("========================================~n"),
    io:format("用户数: ~p~n", [UserCount]),
    io:format("每用户消息数: ~p~n", [MessagesPerUser]),
    io:format("预期总消息数: ~p~n", [TotalMessages]),

    Parent = self(),
    StartTime = erlang:monotonic_time(millisecond),

    % 启动并发发送进程
    Pids = lists:map(fun(UserId) ->
        spawn(fun() ->
            Results = lists:map(fun(N) ->
                MsgId = integer_to_binary(elib_tsid:generate()),
                % 使用 setup 中明确建立过的环状好友关系，避免把非好友流量混进成功率统计
                FriendId = next_ring_friend(UserId, UserIds),
                MsgData = #{
                    <<"payload">> => <<N/integer, "压力测试消息"/utf8>>,
                    <<"msg_type">> => <<"text">>,
                    <<"action">> => <<"send">>,
                    <<"created_at">> => elib_dt:millisecond()
                },
                try
                    case msg_c2c_logic:c2c(MsgId, UserId, MsgData#{<<"to">> => integer_to_binary(FriendId)}) of
                        ok -> success;
                        _ -> failure
                    end
                catch
                    _:_ -> error
                end
            end, lists:seq(1, MessagesPerUser)),
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
    Profile = maps:get(profile, Context),
    [User1 | _] = UserIds,
    User2 = next_ring_friend(User1, UserIds),

    io:format("~n========================================~n"),
    io:format("持续消息压力测试~n"),
    io:format("========================================~n"),

    % 控制在 EUnit 的安全时长内，避免长时间压测把整个回归拖死
    DurationMs = maps:get(duration_ms, Profile),
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
    Profile = maps:get(profile, Context),
    [User1 | _] = UserIds,
    User2 = next_ring_friend(User1, UserIds),

    io:format("~n========================================~n"),
    io:format("爆发式消息压力测试~n"),
    io:format("========================================~n"),

    % 根据当前 VM 进程余量自适应，避免直接打到 system_limit
    BurstSize = maps:get(burst_size, Profile),
    Parent = self(),

    StartTime = erlang:monotonic_time(millisecond),

    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            MsgId = integer_to_binary(elib_tsid:generate()),
            MsgData = #{
                <<"payload">> => <<N/integer, "爆发测试"/utf8>>,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<"send">>,
                <<"created_at">> => elib_dt:millisecond()
            },
            Result = try
                case msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}) of
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
            MsgId = integer_to_binary(elib_tsid:generate()),
            MsgData = #{
                <<"payload">> => <<"持续测试"/utf8>>,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<"send">>,
                <<"created_at">> => elib_dt:millisecond()
            },
            Result = try
                case msg_c2c_logic:c2c(MsgId, User1, MsgData#{<<"to">> => integer_to_binary(User2)}) of
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

stress_profile() ->
    Schedulers = erlang:system_info(schedulers_online),
    ProcessLimit = erlang:system_info(process_limit),
    ProcessCount = erlang:system_info(process_count),
    UserCount = clamp(Schedulers * 2, 12, 24),
    MessagesPerUser = clamp(Schedulers, 8, 12),
    DurationMs = clamp(Schedulers * 250, 2000, 5000),
    ProcessHeadroom = erlang:max(ProcessLimit - ProcessCount - 1500, 0),
    BurstSize = clamp(ProcessHeadroom div 20, 100, 200),
    #{
        user_count => UserCount,
        messages_per_user => MessagesPerUser,
        duration_ms => DurationMs,
        burst_size => BurstSize
    }.

clamp(Value, Min, _Max) when Value < Min ->
    Min;
clamp(Value, _Min, Max) when Value > Max ->
    Max;
clamp(Value, _Min, _Max) ->
    Value.

next_ring_friend(UserId, UserIds) ->
    case lists:dropwhile(fun(Uid) -> Uid =/= UserId end, UserIds) of
        [_Current, Next | _] ->
            Next;
        [_Current] ->
            hd(UserIds)
    end.
