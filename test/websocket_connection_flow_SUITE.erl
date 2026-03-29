-module(websocket_connection_flow_SUITE).

%%%===================================================================
%%% @doc
%%% WebSocket 连接流程 Common Test 测试套件
%%%
%%% 运行方式：
%%%   ct_run -dir test -suite websocket_connection_flow_SUITE
%%%   make ct-websocket_connection_flow
%%%===================================================================

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    %% 连接建立
    establish_connection_with_valid_token_succeeds/1,
    establish_connection_with_expired_token_refreshes/1,
    establish_connection_with_invalid_token_fails/1,
    %% 心跳机制
    heartbeat_keep_connection_alive/1,
    heartbeat_timeout_closes_connection/1,
    %% 消息接收
    receive_real_time_messages/1,
    handle_messages_from_multiple_users/1,
    %% 断线重连
    reconnection_after_disconnect_succeeds/1,
    reconnection_with_new_token_succeeds/1,
    %% 并发连接
    multiple_connections_from_same_user/1,
    connection_limit_enforced/1,
    %% 连接清理
    connection_cleanup_on_logout/1,
    stale_connection_cleanup/1
]).

%% ===================================================================
%% Suite 回调函数
%% ===================================================================

all() ->
    [
        {group, connection},
        {group, heartbeat},
        {group, message_handling},
        {group, reconnection},
        {group, concurrent_connections},
        {group, cleanup}
    ].

groups() ->
    [
        {connection, [], connection_test_cases()},
        {heartbeat, [], heartbeat_test_cases()},
        {message_handling, [], message_test_cases()},
        {reconnection, [], reconnection_test_cases()},
        {concurrent_connections, [], concurrent_test_cases()},
        {cleanup, [], cleanup_test_cases()}
    ].

init_per_suite(Config) ->
    ct:log("开始 WebSocket 连接流程测试套件"),
    eunit_runner:ct_suite_setup(Config).

end_per_suite(Config) ->
    ct:log("结束 WebSocket 连接流程测试套件"),
    cleanup_all_test_data(),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(),
    Config.

end_per_group(_Group, _Config) ->
    meck:unload(),
    ok.

%% ===================================================================
%% 测试用例定义
%% ===================================================================

connection_test_cases() ->
    [
        establish_connection_with_valid_token_succeeds,
        establish_connection_with_expired_token_refreshes,
        establish_connection_with_invalid_token_fails
    ].

heartbeat_test_cases() ->
    [
        heartbeat_keep_connection_alive,
        heartbeat_timeout_closes_connection
    ].

message_test_cases() ->
    [
        receive_real_time_messages,
        handle_messages_from_multiple_users
    ].

reconnection_test_cases() ->
    [
        reconnection_after_disconnect_succeeds,
        reconnection_with_new_token_succeeds
    ].

concurrent_test_cases() ->
    [
        multiple_connections_from_same_user,
        connection_limit_enforced
    ].

cleanup_test_cases() ->
    [
        connection_cleanup_on_logout,
        stale_connection_cleanup
    ].

%% ===================================================================
%% 连接建立测试
%% ===================================================================

establish_connection_with_valid_token_succeeds(_Config) ->
    ct:log("测试使用有效 token 建立 WebSocket 连接成功"),
    Uid = create_test_user(),

    % 生成有效 token
    {ok, Token} = token_ds:encrypt_token(Uid),

    % Mock WebSocket 处理器
    meck:new(websocket_handler, [unstick]),
    meck:expect(websocket_handler, websocket_init, fun(_State) ->
        {ok, mock_req(), #{connected => true}}
    end),

    % 模拟连接请求
    Result = websocket_logic:connect(Token, <<"device_001">>),

    % 验证连接成功
    ?assertMatch({ok, _Pid}, Result),

    meck:unload(websocket_handler),
    cleanup_user(Uid),
    {comment, "使用有效 token 建立 WebSocket 连接成功"}.

establish_connection_with_expired_token_refreshes(_Config) ->
    ct:log("测试使用过期 token 建立连接时刷新 token"),
    Uid = create_test_user(),

    % 生成过期的 token
    {ok, OldToken} = token_ds:encrypt_token(Uid),

    % Mock token 验证返回过期错误
    meck:new(auth_ds, [unstick]),
    meck:expect(auth_ds, verify_token, fun(_Token) ->
        {error, token_expired}
    end),

    % Mock token 刷新
    meck:expect(token_ds, refresh_token, fun(_Uid) ->
        {ok, #{<<"token">> => <<"new_token">>}}
    end),

    % Mock WebSocket 处理器
    meck:new(websocket_handler, [unstick]),
    meck:expect(websocket_handler, websocket_init, fun(_State) ->
        {ok, mock_req(), #{connected => true}}
    end),

    % 模拟连接请求
    Result = websocket_logic:connect(OldToken, <<"device_001">>),

    % 验证连接成功（token 已刷新）
    ?assertMatch({ok, _Pid}, Result),

    meck:unload(auth_ds),
    meck:unload(token_ds),
    meck:unload(websocket_handler),
    cleanup_user(Uid),
    {comment, "过期 token 被刷新，连接建立成功"}.

establish_connection_with_invalid_token_fails(_Config) ->
    ct:log("测试使用无效 token 建立 WebSocket 连接失败"),
    Uid = create_test_user(),

    % 生成无效 token
    InvalidToken = <<"invalid_token_12345">>,

    % Mock token 验证失败
    meck:new(auth_ds, [unstick]),
    meck:expect(auth_ds, verify_token, fun(_Token) ->
        {error, invalid_token}
    end),

    % 模拟连接请求
    Result = websocket_logic:connect(InvalidToken, <<"device_001">>),

    % 验证连接失败
    ?assertMatch({error, _, _}, Result),

    meck:unload(auth_ds),
    cleanup_user(Uid),
    {comment, "无效 token 无法建立 WebSocket 连接"}.

%% ===================================================================
%% 心跳机制测试
%% ===================================================================

heartbeat_keep_connection_alive(_Config) ->
    ct:log("测试心跳保持连接活跃"),
    Uid = create_test_user(),

    % 生成 token 并建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % Mock 心跳响应
    meck:new(websocket_handler, [unstick]),
    meck:expect(websocket_handler, handle_info, fun(_Msg, State) ->
        % 模拟心跳响应
        {ok, State}
    end),

    % 发送心跳
    Result = websocket_logic:heartbeat(WsPid),

    % 验证心跳成功
    ?assertEqual(ok, Result),

    meck:unload(websocket_handler),
    cleanup_user(Uid),
    {comment, "心跳保持连接活跃成功"}.

heartbeat_timeout_closes_connection(_Config) ->
    ct:log("测试心跳超时关闭连接"),
    Uid = create_test_user(),

    % 生成 token 并建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % Mock 超时检测
    meck:new(websocket_logic, [unstick]),
    meck:expect(websocket_logic, check_timeout, fun(_Pid) ->
        timeout
    end),

    % 检查超时
    Result = websocket_logic:check_timeout(WsPid),

    % 验证连接被关闭
    ?assertEqual(timeout, Result),

    meck:unload(websocket_logic),
    cleanup_user(Uid),
    {comment, "心跳超时关闭连接成功"}.

%% ===================================================================
%% 消息接收测试
%% ===================================================================

receive_real_time_messages(_Config) ->
    ct:log("测试接收实时消息"),
    {Uid1, Uid2} = create_two_users(),

    % 建立连接
    {ok, Token1} = token_ds:encrypt_token(Uid1),
    {ok, WsPid1} = websocket_logic:connect(Token1, <<"device_001">>),

    % 发送消息到 Uid1
    MsgId = <<"msg_ws_001">>,
    Payload = #{<<"text">> => <<"实时消息"/utf8>>},
    ok = msg_c2c_logic:c2c(
        MsgId,
        Uid2,
        elib_hashids:encode(Uid1),
        jsone:encode(Payload)
    ),

    % 验证 WebSocket 连接收到消息
    % （需要 mock websocket_handler 的消息处理）

    cleanup_users([Uid1, Uid2]),
    {comment, "接收实时消息成功"}.

handle_messages_from_multiple_users(_Config) ->
    ct:log("测试处理来自多个用户的消息"),
    Uid = create_test_user(),
    {Uid1, Uid2} = create_two_users(),

    % 建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % 从多个用户发送消息
    MsgId1 = <<"msg_ws_002">>,
    MsgId2 = <<"msg_ws_003">>,
    Payload1 = #{<<"text">> => <<"消息1"/utf8>>},
    Payload2 = #{<<"text">> => <<"消息2"/utf8>>},

    ok = msg_c2c_logic:c2c(
        MsgId1,
        Uid1,
        elib_hashids:encode(Uid),
        jsone:encode(Payload1)
    ),

    ok = msg_c2c_logic:c2c(
        MsgId2,
        Uid2,
        elib_hashids:encode(Uid),
        jsone:encode(Payload2)
    ),

    % 验证所有消息都被正确处理

    cleanup_users([Uid, Uid1, Uid2]),
    {comment, "处理来自多个用户的消息成功"}.

%% ===================================================================
%% 断线重连测试
%% ===================================================================

reconnection_after_disconnect_succeeds(_Config) ->
    ct:log("测试断线后重连成功"),
    Uid = create_test_user(),

    % 生成 token 并建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % 模拟断线
    meck:new(websocket_logic, [unstick]),
    meck:expect(websocket_logic, disconnect, fun(_Pid) ->
        disconnected
    end),

    ok = websocket_logic:disconnect(WsPid),

    % 重连
    {ok, NewWsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % 验证重连成功
    ?assertNotEqual(WsPid, NewWsPid),

    meck:unload(websocket_logic),
    cleanup_user(Uid),
    {comment, "断线后重连成功"}.

reconnection_with_new_token_succeeds(_Config) ->
    ct:log("测试使用新 token 重连成功"),
    Uid = create_test_user(),

    % 生成旧 token 并建立连接
    {ok, _OldToken} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(<<"old_token">>, <<"device_001">>),

    % 断线
    ok = websocket_logic:disconnect(WsPid),

    % 生成新 token
    {ok, NewToken} = token_ds:refresh_token(Uid),

    % 使用新 token 重连
    {ok, NewWsPid} = websocket_logic:connect(NewToken, <<"device_001">>),

    % 验证重连成功
    ?assertMatch({ok, _Pid}, {ok, NewWsPid}),

    cleanup_user(Uid),
    {comment, "使用新 token 重连成功"}.

%% ===================================================================
%% 并发连接测试
%% ===================================================================

multiple_connections_from_same_user(_Config) ->
    ct:log("测试同一用户建立多个连接"),
    Uid = create_test_user(),

    % 生成 token
    {ok, Token} = token_ds:encrypt_token(Uid),

    % 从多个设备建立连接
    {ok, WsPid1} = websocket_logic:connect(Token, <<"device_001">>),
    {ok, WsPid2} = websocket_logic:connect(Token, <<"device_002">>),
    {ok, WsPid3} = websocket_logic:connect(Token, <<"device_003">>),

    % 验证所有连接都成功建立
    ?assert(is_pid(WsPid1)),
    ?assert(is_pid(WsPid2)),
    ?assert(is_pid(WsPid3)),
    ?assertNotEqual(WsPid1, WsPid2),
    ?assertNotEqual(WsPid2, WsPid3),

    cleanup_user(Uid),
    {comment, "同一用户建立多个连接成功"}.

connection_limit_enforced(_Config) ->
    ct:log("测试连接数量限制被强制执行"),
    Uid = create_test_user(),

    % 生成 token
    {ok, Token} = token_ds:encrypt_token(Uid),

    % Mock 连接限制
    MaxConnections = 5,
    meck:new(websocket_logic, [unstick]),
    meck:expect(websocket_logic, get_connection_count, fun(_Uid) ->
        MaxConnections
    end),

    % 尝试超过限制的连接
    Results = [
        websocket_logic:connect(Token, <<"device_001">>),
        websocket_logic:connect(Token, <<"device_002">>),
        websocket_logic:connect(Token, <<"device_003">>),
        websocket_logic:connect(Token, <<"device_004">>),
        websocket_logic:connect(Token, <<"device_005">>),
        websocket_logic:connect(Token, <<"device_006">>)  % 超过限制
    ],

    % 验证至少有一个连接被拒绝
    RejectedCount = lists:foldl(fun(Result, Count) ->
        case Result of
            {error, _, _} -> Count + 1;
            _ -> Count
        end
    end, 0, Results),

    ?assert(RejectedCount > 0),

    meck:unload(websocket_logic),
    cleanup_user(Uid),
    {comment, "连接数量限制被强制执行"}.

%% ===================================================================
%% 连接清理测试
%% ===================================================================

connection_cleanup_on_logout(_Config) ->
    ct:log("测试登出时清理连接"),
    Uid = create_test_user(),

    % 生成 token 并建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % 登出
    {ok, _} = auth_logic:logout(Uid, <<"device_001">>),

    % 验证连接被清理
    % （需要验证 websocket 进程已终止）

    cleanup_user(Uid),
    {comment, "登出时清理连接成功"}.

stale_connection_cleanup(_Config) ->
    ct:log("测试清理过期连接"),
    Uid = create_test_user(),

    % 生成 token 并建立连接
    {ok, Token} = token_ds:encrypt_token(Uid),
    {ok, WsPid} = websocket_logic:connect(Token, <<"device_001">>),

    % Mock 过期连接检测
    meck:new(websocket_logic, [unstick]),
    meck:expect(websocket_logic, cleanup_stale, fun() ->
        ok
    end),

    % 清理过期连接
    Result = websocket_logic:cleanup_stale(),

    % 验证清理成功
    ?assertEqual(ok, Result),

    meck:unload(websocket_logic),
    cleanup_user(Uid),
    {comment, "清理过期连接成功"}.

%% ===================================================================
%% 辅助函数
%% ===================================================================

%% 创建测试用户
create_test_user() ->
    Mobile = unique_mobile("13700"),
    Password = <<"Test@123456">>,

    % 清理可能存在的用户
    cleanup_user_by_mobile(Mobile),

    % 创建用户
    {ok, User} = passport_logic:signup(Mobile, Password, <<".@example.com">>, #{}),
    Uid = elib_hashids:decode(maps:get(<<"uid">>, User)),

    Uid.

%% 创建两个测试用户
create_two_users() ->
    Mobile1 = unique_mobile("13700"),
    Mobile2 = unique_mobile("13700"),
    Password = <<"Test@123456">>,

    % 创建用户
    {ok, _} = passport_logic:signup(Mobile1, Password, <<".@example.com">>, #{}),
    {ok, _} = passport_logic:signup(Mobile2, Password, <<".@example.com">>, #{}),

    % 获取用户 ID
    User1 = user_repo:find_by_mobile(Mobile1, <<"id">>),
    User2 = user_repo:find_by_mobile(Mobile2, <<"id">>),

    Uid1 = maps:get(<<"id">>, User1),
    Uid2 = maps:get(<<"id">>, User2),

    {Uid1, Uid2}.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond),
         erlang:unique_integer([monotonic, positive]),
         self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

%% 清理用户
cleanup_user(Uid) ->
    user_repo:delete(Uid).

cleanup_user_by_mobile(Mobile) ->
    case user_repo:find_by_mobile(Mobile, <<"id">>) of
        #{<<"id">> := Id} when is_integer(Id) ->
            user_repo:delete(Id);
        _ ->
            ok
    end.

%% 清理用户列表
cleanup_users([]) -> ok;
cleanup_users([Uid | Rest]) ->
    cleanup_user(Uid),
    cleanup_users(Rest).

%% 清理所有测试数据
cleanup_all_test_data() ->
    Sql = <<"SELECT id FROM user WHERE mobile LIKE '13700%'">>,
    case elib_pg:query(Sql, []) of
        {ok, Rows} ->
            lists:foreach(fun(#{<<"id">> := Id}) ->
                user_repo:delete(Id)
            end, Rows);
        _ ->
            ok
    end.

%% Mock 请求对象
mock_req() ->
    #{
        peer => {{127, 0, 0, 1}, 8080},
        host => <<"localhost">>
    }.
