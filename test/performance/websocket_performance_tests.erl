%% @doc WebSocket 连接性能测试
%% 测试范围：
%% - 连接建立性能
%% - 消息推送性能
%% - 大量连接性能
%% - 心跳性能
-module(websocket_performance_tests).

-include_lib("eunit/include/eunit.hrl").

%% 性能阈值定义
-define(MAX_CONNECT_TIME_MS, 500).       % 连接建立最大耗时 500ms
-define(MAX_PUSH_TIME_MS, 100).          % 消息推送最大耗时 100ms
-define(CONCURRENT_CONNECTIONS, 100).    % 并发连接数

%% 测试夹具
ws_performance_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"连接建立性能", fun test_connect_performance/0},
      {"消息推送性能", fun test_push_performance/0},
      {"广播消息性能", fun test_broadcast_performance/0},
      {"心跳性能", fun test_heartbeat_performance/0},
      {"消息吞吐量", fun test_throughput/0}
     ]
    }.

setup() ->
    _ = eunit_runner:eunit_setup(),
    application:set_env(imboy, env, test),
    % 创建测试用户
    UserIds = lists:map(fun(N) ->
        {ok, Uid} = create_test_user(<<"ws_user", N/integer>>),
        Uid
    end, lists:seq(1, 10)),
    Context = #{user_ids => UserIds},
    persistent_term:put({?MODULE, test_context}, Context),
    Context.

cleanup(_Context) ->
    persistent_term:erase({?MODULE, test_context}),
    ok.

%% ===================================================================
%% 测试用例
%% ===================================================================

test_connect_performance() ->
    Context = get_context(),
    [User1 | _] = maps:get(user_ids, Context),

    % 生成 token
    Token = token_ds:encrypt_token(User1),

    % 模拟连接建立
    ConnectTimes = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),

        % 模拟 WebSocket 连接处理
        {ok, Pid} = websocket_ds:connect(User1, Token, #{}),

        EndTime = erlang:monotonic_time(millisecond),

        % 清理连接
        websocket_ds:disconnect(Pid),

        EndTime - StartTime
    end, lists:seq(1, 20)),

    AvgTime = lists:sum(ConnectTimes) / length(ConnectTimes),

    io:format("~n连接建立性能报告:~n"),
    io:format("  连接次数: ~p~n", [length(ConnectTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),
    io:format("  最大耗时: ~p ms~n", [lists:max(ConnectTimes)]),

    ?assert(AvgTime =< ?MAX_CONNECT_TIME_MS, "连接建立平均耗时超过阈值"),

    ok.

test_push_performance() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    [User1, User2 | _] = UserIds,

    % 建立连接
    Token1 = token_ds:encrypt_token(User1),
    Token2 = token_ds:encrypt_token(User2),
    {ok, Pid1} = websocket_ds:connect(User1, Token1, #{}),
    {ok, Pid2} = websocket_ds:connect(User2, Token2, #{}),

    % 测试消息推送
    PushTimes = lists:map(fun(N) ->
        MsgId = integer_to_binary(elib_tsid:generate()),
        Payload = #{
            <<"msg_id">> => MsgId,
            <<"content">> => <<N/integer, "推送测试"/utf8>>,
            <<"from">> => integer_to_binary(User1)
        },

        StartTime = erlang:monotonic_time(millisecond),
        ok = msg_s2c_ds:send(User1, [User2], <<"test_push">>, MsgId, null, Payload, nosave),
        EndTime = erlang:monotonic_time(millisecond),

        EndTime - StartTime
    end, lists:seq(1, 50)),

    AvgTime = lists:sum(PushTimes) / length(PushTimes),

    io:format("~n消息推送性能报告:~n"),
    io:format("  推送次数: ~p~n", [length(PushTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    % 清理连接
    websocket_ds:disconnect(Pid1),
    websocket_ds:disconnect(Pid2),

    ?assert(AvgTime =< ?MAX_PUSH_TIME_MS, "消息推送平均耗时超过阈值"),

    ok.

test_broadcast_performance() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    [Sender | Receivers] = UserIds,

    % 为所有用户建立连接
    Pids = lists:map(fun(Uid) ->
        Token = token_ds:encrypt_token(Uid),
        {ok, Pid} = websocket_ds:connect(Uid, Token, #{}),
        Pid
    end, UserIds),

    % 测试广播性能
    BroadcastTimes = lists:map(fun(N) ->
        MsgId = integer_to_binary(elib_tsid:generate()),
        Payload = #{
            <<"msg_id">> => MsgId,
            <<"content">> => <<N/integer, "广播测试"/utf8>>
        },

        StartTime = erlang:monotonic_time(millisecond),
        ok = msg_s2c_ds:send(Sender, Receivers, <<"broadcast">>, MsgId, null, Payload, nosave),
        EndTime = erlang:monotonic_time(millisecond),

        EndTime - StartTime
    end, lists:seq(1, 20)),

    AvgTime = lists:sum(BroadcastTimes) / length(BroadcastTimes),

    io:format("~n广播消息性能报告:~n"),
    io:format("  接收者数量: ~p~n", [length(Receivers)]),
    io:format("  广播次数: ~p~n", [length(BroadcastTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    % 清理连接
    lists:foreach(fun(Pid) -> websocket_ds:disconnect(Pid) end, Pids),

    ?assert(AvgTime =< ?MAX_PUSH_TIME_MS * 2, "广播消息平均耗时超过阈值"),

    ok.

test_heartbeat_performance() ->
    Context = get_context(),
    [User1 | _] = maps:get(user_ids, Context),

    % 建立连接
    Token = token_ds:encrypt_token(User1),
    {ok, Pid} = websocket_ds:connect(User1, Token, #{}),

    % 测试心跳处理性能
    HeartbeatTimes = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(millisecond),
        ok = websocket_ds:heartbeat(Pid),
        EndTime = erlang:monotonic_time(millisecond),
        EndTime - StartTime
    end, lists:seq(1, 100)),

    AvgTime = lists:sum(HeartbeatTimes) / length(HeartbeatTimes),

    io:format("~n心跳性能报告:~n"),
    io:format("  心跳次数: ~p~n", [length(HeartbeatTimes)]),
    io:format("  平均耗时: ~.2f ms~n", [AvgTime]),

    % 清理连接
    websocket_ds:disconnect(Pid),

    ?assert(AvgTime =< 10, "心跳处理平均耗时超过10ms"),

    ok.

test_throughput() ->
    Context = get_context(),
    UserIds = maps:get(user_ids, Context),
    [User1, User2 | _] = UserIds,

    % 建立连接
    Token1 = token_ds:encrypt_token(User1),
    Token2 = token_ds:encrypt_token(User2),
    {ok, Pid1} = websocket_ds:connect(User1, Token1, #{}),
    {ok, Pid2} = websocket_ds:connect(User2, Token2, #{}),

    % 测试消息吞吐量
    MessageCount = 1000,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        MsgId = integer_to_binary(elib_tsid:generate()),
        Payload = #{
            <<"msg_id">> => MsgId,
            <<"content">> => <<N/integer>>,
            <<"from">> => integer_to_binary(User1)
        },
        msg_s2c_ds:send(User1, [User2], <<"throughput_test">>, MsgId, null, Payload, nosave)
    end, lists:seq(1, MessageCount)),

    EndTime = erlang:monotonic_time(millisecond),
    TotalTime = EndTime - StartTime,

    Throughput = MessageCount * 1000 / TotalTime,

    io:format("~n消息吞吐量报告:~n"),
    io:format("  消息总数: ~p~n", [MessageCount]),
    io:format("  总耗时: ~p ms~n", [TotalTime]),
    io:format("  吞吐量: ~.2f msg/s~n", [Throughput]),

    % 清理连接
    websocket_ds:disconnect(Pid1),
    websocket_ds:disconnect(Pid2),

    ?assert(Throughput >= 500, "消息吞吐量低于500 msg/s"),

    ok.

%% ===================================================================
%% 辅助函数
%% ===================================================================

get_context() ->
    persistent_term:get({?MODULE, test_context}).

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
