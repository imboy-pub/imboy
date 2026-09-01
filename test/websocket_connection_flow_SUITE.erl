-module(websocket_connection_flow_SUITE).

%%%===================================================================
%%% @doc
%%% WebSocket 连接流程 Common Test 测试套件
%%%
%%% 通过内嵌的最小 RFC6455 客户端打真实 cowboy websocket_handler：
%%% 独立 ephemeral listener + 真实 token 认证 + 真实消息路由链路。
%%%
%%% 运行方式：
%%%   IMBOY_TEST_CONFIG=$PWD/config/sys.local.config make ct CT_SUITES=websocket_connection_flow
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
    establish_connection_with_invalid_token_fails/1,
    establish_connection_with_expired_token_fails/1,
    establish_connection_without_token_fails/1,
    handshake_without_subprotocol_returns_400/1,
    %% 心跳机制
    heartbeat_keep_connection_alive/1,
    binary_frame_ignored_on_json_connection/1,
    %% 消息收发
    receive_real_time_messages/1,
    c2c_to_non_friend_receives_not_a_friend_reply/1,
    invalid_json_receives_invalid_json_error/1,
    client_ack_receives_confirm/1,
    %% 断线重连
    reconnection_after_disconnect_succeeds/1,
    reconnection_with_new_token_succeeds/1,
    %% 并发连接
    multiple_connections_from_same_user/1,
    connection_limit_enforced/1,
    %% 连接清理
    connection_cleanup_on_logout/1,
    abrupt_close_marks_user_offline/1
]).

-define(WS_GUID, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>).
-define(LISTENER_REF, ws_flow_test_listener).

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
    Config1 = eunit_runner:ct_suite_setup(Config),
    %% 独立 ephemeral listener：不依赖主 HTTP listener 的端口配置
    %% dispatch 须为 {Host, [{Path, Handler, Opts}]} 编译形态（与 imboy_app 一致），
    %% 裸 Path 规则会被当作 Host 匹配失败 → cowboy_router 直接 400
    Dispatch = cowboy_router:compile([{'_', [{"/api/v1/ws", websocket_handler, #{}}]}]),
    {ok, _Pid} = cowboy:start_clear(
        ?LISTENER_REF,
        [{port, 0}],
        #{
            env => #{dispatch => Dispatch},
            max_connections => infinity
        }
    ),
    Port = ranch:get_port(?LISTENER_REF),
    %% 每轮运行独立的手机号前缀（137 + 2 位 run tag），清理只软删本套件用户
    RunTag = io_lib:format("~2..0B", [erlang:phash2(erlang:unique_integer(), 100) rem 100]),
    [{ws_port, Port}, {ws_mobile_prefix, <<"137", (iolist_to_binary(RunTag))/binary>>} | Config1].

end_per_suite(Config) ->
    ct:log("结束 WebSocket 连接流程测试套件"),
    cowboy:stop_listener(?LISTENER_REF),
    cleanup_all_test_data(Config),
    eunit_runner:ct_suite_cleanup(Config).

init_per_group(_Group, Config) ->
    cleanup_all_test_data(Config),
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
        establish_connection_with_invalid_token_fails,
        establish_connection_with_expired_token_fails,
        establish_connection_without_token_fails,
        handshake_without_subprotocol_returns_400
    ].

heartbeat_test_cases() ->
    [
        heartbeat_keep_connection_alive,
        binary_frame_ignored_on_json_connection
    ].

message_test_cases() ->
    [
        receive_real_time_messages,
        c2c_to_non_friend_receives_not_a_friend_reply,
        invalid_json_receives_invalid_json_error,
        client_ack_receives_confirm
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
        abrupt_close_marks_user_offline
    ].

%% ===================================================================
%% 连接建立测试
%% ===================================================================

establish_connection_with_valid_token_succeeds(Config) ->
    ct:log("测试使用有效 token 建立 WebSocket 连接成功"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    {ok, WS} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], []),

    %% 101 升级完成后 websocket_init 异步执行上线注册，轮询等待
    true = wait_online(Uid, true),

    ws_close(WS),
    true = wait_online(Uid, false),
    cleanup_user(Uid),
    {comment, "使用有效 token 建立 WebSocket 连接成功"}.

establish_connection_with_invalid_token_fails(Config) ->
    ct:log("测试使用无效 token 建立 WebSocket 连接失败"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Did = unique_did(),

    {rejected, 401, Headers, Body} = ws_connect(
        Port, [{<<"token">>, <<"invalid_token_12345">>}, {<<"did">>, Did}], []
    ),

    %% websocket_ds:auth：签名无效 → 401 + x-token-error: invalid + 业务码 706
    <<"invalid">> = proplists:get_value(<<"x-token-error">>, Headers),
    #{<<"code">> := 706} = jsone:decode(Body, [{object_format, map}]),

    %% 拒绝的连接不注册在线状态
    false = user_logic:is_online(Uid),
    cleanup_user(Uid),
    {comment, "无效 token 无法建立 WebSocket 连接"}.

establish_connection_with_expired_token_fails(Config) ->
    ct:log("测试使用过期 token 建立 WebSocket 连接失败"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Did = unique_did(),

    %% 自签 exp 在 1 小时前的 JWT（jwerl exp_leeway 300s，-3600s 必过期）
    JwtKey = config_ds:env(jwt_key, <<>>),
    Now = elib_dt:utc(second),
    ExpiredToken = jwerl:sign(
        #{uid => Uid, exp => Now - 3600, sub => <<"tk">>, did => <<>>}, hs256, JwtKey
    ),

    {rejected, 401, Headers, Body} = ws_connect(
        Port, [{<<"token">>, ExpiredToken}, {<<"did">>, Did}], []
    ),

    %% 过期 token 拒绝连接（要求客户端重新登录），业务码 705
    <<"expired">> = proplists:get_value(<<"x-token-error">>, Headers),
    #{<<"code">> := 705} = jsone:decode(Body, [{object_format, map}]),

    cleanup_user(Uid),
    {comment, "过期 token 被拒绝连接"}.

establish_connection_without_token_fails(Config) ->
    ct:log("测试缺少 token 的握手被拒绝"),
    Port = proplists:get_value(ws_port, Config),
    Did = unique_did(),

    %% 真实契约：缺 token 时 parse_authorization_header(undefined) 返回 <<>>，
    %% 与垃圾 token 同走 706 invalid → 401（websocket_ds:auth 的 412 空串分支
    %% 经该路径不可达）
    {rejected, 401, Headers, Body} = ws_connect(Port, [{<<"did">>, Did}], []),
    <<"invalid">> = proplists:get_value(<<"x-token-error">>, Headers),
    #{<<"code">> := 706} = jsone:decode(Body, [{object_format, map}]),

    {comment, "缺少 token 返回 401 token_invalid"}.

handshake_without_subprotocol_returns_400(Config) ->
    ct:log("测试未声明子协议的握手返回 400"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    %% websocket_ds:check_subprotocols(undefined) → 400：
    %% 服务端要求客户端显式声明 imboy.v2/imboy-protobuf/imboy-json/text 之一
    {rejected, 400, _Headers, _Body} = ws_connect(
        Port, [{<<"token">>, Token}, {<<"did">>, Did}], no_subprotocol
    ),

    cleanup_user(Uid),
    {comment, "未声明子协议返回 400"}.

%% ===================================================================
%% 心跳机制测试
%% ===================================================================

heartbeat_keep_connection_alive(Config) ->
    ct:log("测试应用层心跳保持连接活跃"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),

    {ok, WS0} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, unique_did()}], []),

    %% 应用层心跳契约：text "ping" → "pong"，"PING" → "PONG"
    ok = ws_send_text(WS0, <<"ping">>),
    {ok, <<"pong">>, WS1} = recv_text(WS0, 3000),
    ok = ws_send_text(WS1, <<"PING">>),
    {ok, <<"PONG">>, WS2} = recv_text(WS1, 3000),

    ws_close(WS2),
    cleanup_user(Uid),
    {comment, "心跳保持连接活跃成功"}.

binary_frame_ignored_on_json_connection(Config) ->
    ct:log("测试 JSON 协议连接忽略 binary 帧"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),

    {ok, WS0} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, unique_did()}], []),

    %% imboy-json 连接收到 binary 帧：忽略不回包，连接保持可用
    ok = ws_send_binary(WS0, <<"not-json-binary">>),
    {error, timeout} = try_recv_text(WS0, 500),

    %% 连接仍存活：心跳照常应答
    ok = ws_send_text(WS0, <<"ping">>),
    {ok, <<"pong">>, WS1} = recv_text(WS0, 3000),

    ws_close(WS1),
    cleanup_user(Uid),
    {comment, "binary 帧被忽略，连接保持活跃"}.

%% ===================================================================
%% 消息收发测试
%% ===================================================================

receive_real_time_messages(Config) ->
    ct:log("测试在线用户实时接收单聊消息"),
    Port = proplists:get_value(ws_port, Config),
    {UidA, UidB} = create_two_friends(Config),

    {ok, WSA0} = ws_connect(
        Port, [{<<"token">>, token_ds:encrypt_token(UidA)}, {<<"did">>, unique_did()}], []
    ),
    {ok, WSB0} = ws_connect(
        Port, [{<<"token">>, token_ds:encrypt_token(UidB)}, {<<"did">>, unique_did()}], []
    ),
    true = wait_online(UidA, true),
    true = wait_online(UidB, true),

    %% B → A 发送 C2C：必须带 id/type/to/msg_type/created_at/payload
    MsgId = unique_msgid(),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => integer_to_binary(UidB),
        <<"to">> => integer_to_binary(UidA),
        <<"msg_type">> => <<"text">>,
        <<"created_at">> => elib_dt:now(),
        <<"payload">> => #{<<"text">> => <<"实时消息"/utf8>>}
    },
    ok = ws_send_text(WSB0, jsone:encode(Msg, [native_utf8])),

    %% A 的连接在投递窗口内收到同 id 的 C2C 帧（跳过在线通知等噪声帧）
    {ok, Frame, WSA} = recv_json_matching(
        WSA0,
        fun(#{<<"id">> := Id, <<"type">> := <<"C2C">>}) -> Id =:= MsgId end,
        8000
    ),
    #{<<"id">> := MsgId, <<"type">> := <<"C2C">>} = Frame,

    ws_close(WSA),
    ws_close(WSB0),
    cleanup_users([UidA, UidB]),
    {comment, "接收实时消息成功"}.

c2c_to_non_friend_receives_not_a_friend_reply(Config) ->
    ct:log("测试非好友单聊收到 not_a_friend 回执"),
    Port = proplists:get_value(ws_port, Config),
    [UidA, UidC] = create_n_users(Config, 2),

    {ok, WS0} = ws_connect(
        Port, [{<<"token">>, token_ds:encrypt_token(UidC)}, {<<"did">>, unique_did()}], []
    ),
    true = wait_online(UidC, true),

    %% C → A（非好友）：message_policy 拒发并回 S2C 错误帧给发送方
    MsgId = unique_msgid(),
    Msg = #{
        <<"id">> => MsgId,
        <<"type">> => <<"C2C">>,
        <<"from">> => integer_to_binary(UidC),
        <<"to">> => integer_to_binary(UidA),
        <<"msg_type">> => <<"text">>,
        <<"created_at">> => elib_dt:now(),
        <<"payload">> => #{<<"text">> => <<"hi">>}
    },
    ok = ws_send_text(WS0, jsone:encode(Msg, [native_utf8])),

    {ok, Reply, WS1} = recv_json_matching(
        WS0, fun(#{<<"action">> := <<"not_a_friend">>}) -> true end, 5000
    ),
    #{<<"type">> := <<"S2C">>, <<"action">> := <<"not_a_friend">>} = Reply,

    ws_close(WS1),
    cleanup_users([UidA, UidC]),
    {comment, "非好友单聊被拒并收到回执"}.

invalid_json_receives_invalid_json_error(Config) ->
    ct:log("测试非法 JSON 收到 invalid_json 错误帧"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),

    {ok, WS0} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, unique_did()}], []),

    ok = ws_send_text(WS0, <<"{this is not json">>),
    {ok, Reply, WS1} = recv_json_matching(
        WS0, fun(#{<<"action">> := <<"invalid_json">>}) -> true end, 5000
    ),
    #{<<"type">> := <<"S2C">>, <<"action">> := <<"invalid_json">>} = Reply,

    ws_close(WS1),
    cleanup_user(Uid),
    {comment, "非法 JSON 回结构化错误帧"}.

client_ack_receives_confirm(Config) ->
    ct:log("测试 CLIENT_ACK 收到确认回执"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    {ok, WS0} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], []),

    %% CLIENT_ACK,type,msgid,did：did 必须与连接的 did 一致
    MsgId = unique_msgid(),
    Ack = <<"CLIENT_ACK,C2C,", MsgId/binary, ",", Did/binary>>,
    ok = ws_send_text(WS0, Ack),

    {ok, Reply, WS1} = recv_json_matching(
        WS0,
        fun(#{<<"type">> := <<"CLIENT_ACK_CONFIRM">>, <<"in_reply_to">> := Id}) ->
            Id =:= MsgId
        end,
        5000
    ),
    #{<<"type">> := <<"CLIENT_ACK_CONFIRM">>, <<"in_reply_to">> := MsgId} = Reply,

    ws_close(WS1),
    cleanup_user(Uid),
    {comment, "CLIENT_ACK 收到确认回执"}.

%% ===================================================================
%% 断线重连测试
%% ===================================================================

reconnection_after_disconnect_succeeds(Config) ->
    ct:log("测试断线后重连成功"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    {ok, WS1} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], []),
    true = wait_online(Uid, true),
    ws_close(WS1),
    true = wait_online(Uid, false),

    %% 同 token 重连（token 有效期内可复用）
    {ok, WS2} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], []),
    true = wait_online(Uid, true),

    ws_close(WS2),
    cleanup_user(Uid),
    {comment, "断线后重连成功"}.

reconnection_with_new_token_succeeds(Config) ->
    ct:log("测试使用新 token 重连成功"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token1 = token_ds:encrypt_token(Uid),

    {ok, WS1} = ws_connect(Port, [{<<"token">>, Token1}, {<<"did">>, unique_did()}], []),
    true = wait_online(Uid, true),
    ws_close(WS1),
    true = wait_online(Uid, false),

    Token2 = token_ds:encrypt_token(Uid),
    {ok, WS2} = ws_connect(Port, [{<<"token">>, Token2}, {<<"did">>, unique_did()}], []),
    true = wait_online(Uid, true),

    ws_close(WS2),
    cleanup_user(Uid),
    {comment, "使用新 token 重连成功"}.

%% ===================================================================
%% 并发连接测试
%% ===================================================================

multiple_connections_from_same_user(Config) ->
    ct:log("测试同一用户建立多个设备连接"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),

    Did1 = unique_did(),
    Did3 = unique_did(),
    {ok, WS1} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did1}], []),
    {ok, WS2} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, unique_did()}], []),
    {ok, WS3} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did3}], []),

    %% 三台设备同时在线
    3 = wait_online_count(Uid, 3),

    %% 跨设备登录通知（user_server cast_online 契约）：后登录设备上线时，
    %% 先登录的设备应收到 logged_another_device，且 payload.did 指向新设备。
    %% （历史 bug：白名单/黑名单方向写反，通知只发给了刚登录的设备自己）
    {ok, Notice, _WS1b} = recv_json_matching(
        WS1,
        fun(
            #{
                <<"type">> := <<"S2C">>,
                <<"action">> := <<"logged_another_device">>,
                <<"payload">> := #{<<"did">> := D}
            }
        ) ->
            D =:= Did3
        end,
        5000
    ),
    true = is_map(Notice),

    ws_close(WS1),
    ws_close(WS2),
    ws_close(WS3),
    0 = wait_online_count(Uid, 0),
    cleanup_user(Uid),
    {comment, "同一用户建立多个连接成功"}.

connection_limit_enforced(Config) ->
    ct:log("测试同设备握手限流被强制执行"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    %% throttle_ws：同 did 10/秒 + 22/分钟。25 次快速握手全部落在同一个
    %% 60s 窗口内（远短于 1s），per-minute 上限必然触发至少 3 次 429。
    Results = [
        ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], [])
     || _ <- lists:seq(1, 25)
    ],
    {Upgraded, Rejected429} = lists:foldl(
        fun
            ({ok, WS}, {U, R}) ->
                {[WS | U], R};
            ({rejected, 429, _, _}, {U, R}) ->
                {U, R + 1};
            ({rejected, Other, _, _}, {U, R}) ->
                ct:log("意外的握手结果: ~p", [Other]),
                {U, R}
        end,
        {[], 0},
        Results
    ),

    ?assert(Rejected429 >= 1),
    ?assert(length(Upgraded) < 25),

    lists:foreach(fun ws_close/1, Upgraded),
    0 = wait_online_count(Uid, 0),
    cleanup_user(Uid),
    {comment, "握手限流被强制执行"}.

%% ===================================================================
%% 连接清理测试
%% ===================================================================

connection_cleanup_on_logout(Config) ->
    ct:log("测试登出时从在线注册表清理连接"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),
    Did = unique_did(),

    {ok, WS0} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, Did}], []),
    true = wait_online(Uid, true),

    %% 登出契约：auth_logic:logout 从 imboy_syn 移除该设备（消息不再投递），
    %% 但不主动关闭 socket——连接本身仍可收发（如收到登出确认）
    {ok, <<"success">>} = auth_logic:logout(Uid, Did),
    true = wait_online(Uid, false),

    ok = ws_send_text(WS0, <<"ping">>),
    {ok, <<"pong">>, WS1} = recv_text_matching(WS0, fun(P) -> P =:= <<"pong">> end, 3000),

    ws_close(WS1),
    cleanup_user(Uid),
    {comment, "登出后用户离线，连接保持到客户端主动断开"}.

abrupt_close_marks_user_offline(Config) ->
    ct:log("测试客户端异常断开后用户标记离线"),
    Port = proplists:get_value(ws_port, Config),
    Uid = create_test_user(Config),
    Token = token_ds:encrypt_token(Uid),

    {ok, {ws, Sock, _}} = ws_connect(Port, [{<<"token">>, Token}, {<<"did">>, unique_did()}], []),
    true = wait_online(Uid, true),

    %% 不发 close 帧直接断 TCP：cowboy terminate → user_logic:offline
    ok = gen_tcp:close(Sock),
    true = wait_online(Uid, false),

    cleanup_user(Uid),
    {comment, "异常断开后用户被标记离线"}.

%% ===================================================================
%% 辅助函数：测试数据
%% ===================================================================

create_test_user(Config) ->
    Prefix = proplists:get_value(ws_mobile_prefix, Config),
    Mobile = unique_mobile(Prefix),
    Password = <<"Test@123456">>,

    cleanup_user_by_mobile(Mobile),
    {ok, User} = passport_logic:signup(Mobile, Password, <<".@example.com">>, #{}),
    %% signup 返回 JSON integer 形态的 TSID（已是 integer）
    ec_cnv:to_integer(maps:get(<<"uid">>, User)).

create_n_users(Config, N) ->
    lists:map(fun(_) -> create_test_user(Config) end, lists:seq(1, N)).

create_two_friends(Config) ->
    [Uid1, Uid2] = create_n_users(Config, 2),
    MsgId = unique_msgid(),
    Payload = #{<<"msg">> => <<"test">>},
    ok = friend_logic:add_friend(MsgId, Uid1, integer_to_binary(Uid2), Payload, elib_dt:now()),
    ConfirmData = #{
        <<"from">> => #{<<"remark">> => <<>>, <<"tag">> => <<>>},
        <<"to">> => #{<<"remark">> => <<>>, <<"tag">> => <<>>},
        <<"source">> => <<"search">>
    },
    {ok, _, _, _} = friend_logic:confirm_friend(
        MsgId, Uid2, integer_to_binary(Uid1), jsone:encode(ConfirmData)
    ),
    true = friend_ds:is_friend(Uid1, Uid2),
    {Uid1, Uid2}.

unique_mobile(Prefix) ->
    Suffix = erlang:phash2(
        {erlang:system_time(microsecond), erlang:unique_integer([monotonic, positive]), self()},
        1000000
    ),
    list_to_binary(io_lib:format("~s~6..0B", [Prefix, Suffix])).

unique_did() ->
    <<"wsflow_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

unique_msgid() ->
    <<"wstest_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

cleanup_user(Uid) ->
    user_repo:delete(Uid).

cleanup_users(Uids) ->
    lists:foreach(fun cleanup_user/1, Uids).

cleanup_user_by_mobile(Mobile) ->
    case user_repo:find_by_mobile(Mobile, <<"id">>) of
        #{<<"id">> := Id} when is_integer(Id) ->
            user_repo:delete(Id);
        _ ->
            ok
    end.

%% 按本轮手机号前缀清理（软删），不波及库内其他用户
cleanup_all_test_data(Config) ->
    case proplists:get_value(ws_mobile_prefix, Config) of
        undefined ->
            ok;
        Prefix ->
            Sql = <<"SELECT id FROM \"user\" WHERE mobile LIKE $1">>,
            case elib_pg:query(Sql, [<<Prefix/binary, "%">>]) of
                {ok, Rows} ->
                    lists:foreach(fun(#{<<"id">> := Id}) -> user_repo:delete(Id) end, Rows);
                _ ->
                    ok
            end
    end.

%% ===================================================================
%% 辅助函数：在线状态轮询
%% ===================================================================

wait_online(Uid, Expected) ->
    wait_online(Uid, Expected, 10).

wait_online(_Uid, _Expected, 0) ->
    false;
wait_online(Uid, Expected, N) ->
    case user_logic:is_online(Uid) of
        Expected ->
            true;
        _ ->
            timer:sleep(200),
            wait_online(Uid, Expected, N - 1)
    end.

wait_online_count(Uid, Expected) ->
    wait_online_count(Uid, Expected, 10).

wait_online_count(_Uid, _Expected, 0) ->
    %% 超时未达预期：返回当前实际计数供断言报错展示
    imboy_syn:count_user(_Uid);
wait_online_count(Uid, Expected, N) ->
    case imboy_syn:count_user(Uid) of
        Expected ->
            Expected;
        _ ->
            timer:sleep(200),
            wait_online_count(Uid, Expected, N - 1)
    end.

%% ===================================================================
%% 辅助函数：最小 RFC6455 客户端
%% ===================================================================

%% @doc 发起 WebSocket 握手。子协议固定 imboy-json（除非传 no_subprotocol）。
%% 成功返回 {ok, WS}（含帧缓冲），被 HTTP 层拒绝返回 {rejected, Status, Headers, Body}。
ws_connect(Port, QsPairs, no_subprotocol) ->
    %% 直接不带子协议头构造握手（不可经 <<>> 分支委托——那里会补 imboy-json）
    ws_connect_raw(Port, QsPairs, <<>>);
ws_connect(Port, QsPairs, ExtraHeaders) when ExtraHeaders =:= <<>>; ExtraHeaders =:= [] ->
    ws_connect_raw(Port, QsPairs, <<"Sec-WebSocket-Protocol: imboy-json\r\n">>).

ws_connect_raw(Port, QsPairs, Extra) ->
    {ok, Sock} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}, {packet, raw}], 5000),
    Key = base64:encode(crypto:strong_rand_bytes(16)),
    Qs = uri_string:compose_query(QsPairs),
    Request = iolist_to_binary([
        <<"GET /api/v1/ws?", Qs/binary, " HTTP/1.1\r\n">>,
        <<"Host: 127.0.0.1:", (integer_to_binary(Port))/binary, "\r\n">>,
        <<"Upgrade: websocket\r\n">>,
        <<"Connection: Upgrade\r\n">>,
        <<"Sec-WebSocket-Key: ", Key/binary, "\r\n">>,
        <<"Sec-WebSocket-Version: 13\r\n">>,
        Extra,
        <<"\r\n">>
    ]),
    ok = gen_tcp:send(Sock, Request),
    {ok, HeadBin, Rest} = recv_head(Sock, <<>>),
    [StatusLine | HdrLines] = binary:split(HeadBin, <<"\r\n">>, [global]),
    {ok, Status} = parse_status(StatusLine),
    Headers = [parse_header(L) || L <- HdrLines, L =/= <<>>],
    case Status of
        101 ->
            Expected = base64:encode(crypto:hash(sha, <<Key/binary, ?WS_GUID/binary>>)),
            case proplists:get_value(<<"sec-websocket-accept">>, Headers) of
                Expected ->
                    {ok, {ws, Sock, Rest}};
                Other ->
                    ok = gen_tcp:close(Sock),
                    {error, {bad_sec_websocket_accept, Other, Expected}}
            end;
        _ ->
            ContentLength = binary_to_integer(
                proplists:get_value(<<"content-length">>, Headers, <<"0">>)
            ),
            Body = recv_exact(Sock, Rest, ContentLength),
            ok = gen_tcp:close(Sock),
            {rejected, Status, Headers, Body}
    end.

ws_send_text(WS, Payload) ->
    ws_send_frame(WS, 16#1, Payload).

ws_send_binary(WS, Payload) ->
    ws_send_frame(WS, 16#2, Payload).

ws_close(WS) ->
    %% 尽力发送 close 帧后关闭 TCP；对端可能已先行关闭
    try ws_send_frame(WS, 16#8, <<>>) of
        ok -> ok
    catch
        _:_ -> ok
    end,
    {ws, Sock, _} = WS,
    gen_tcp:close(Sock).

ws_send_frame({ws, Sock, _}, Opcode, Payload) ->
    Len = byte_size(Payload),
    LenBits =
        if
            Len =< 125 -> <<1:1, Len:7>>;
            Len =< 65535 -> <<1:1, 126:7, Len:16>>;
            true -> <<1:1, 127:7, Len:64>>
        end,
    Mask = crypto:strong_rand_bytes(4),
    Masked = mask_payload(Payload, Mask),
    Frame = <<1:1, 0:3, Opcode:4, LenBits/binary, Mask/binary, Masked/binary>>,
    ok = gen_tcp:send(Sock, Frame).

mask_payload(Payload, Mask) ->
    mask_payload(Payload, Mask, 0, <<>>).

mask_payload(<<>>, _Mask, _I, Acc) ->
    Acc;
mask_payload(<<B:8, Rest/binary>>, <<M1:8, M2:8, M3:8, M4:8>> = Mask, I, Acc) ->
    M =
        case I rem 4 of
            0 -> M1;
            1 -> M2;
            2 -> M3;
            3 -> M4
        end,
    mask_payload(Rest, Mask, I + 1, <<Acc/binary, (B bxor M):8>>).

%% @doc 收一帧，跳过 ping/pong/close 帧直到 text 帧。
recv_text(WS, Timeout) ->
    case ws_recv_frame(WS, Timeout) of
        {ok, 16#1, Payload, WS2} ->
            {ok, Payload, WS2};
        {ok, _OtherOpcode, _Payload, WS2} ->
            recv_text(WS2, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

try_recv_text(WS, Timeout) ->
    case recv_text(WS, Timeout) of
        {ok, _Payload, _WS2} = Ok -> Ok;
        {error, Reason} -> {error, Reason}
    end.

recv_json_text(WS, Timeout) ->
    case recv_text(WS, Timeout) of
        {ok, Payload, WS2} ->
            {ok, jsone:decode(Payload, [{object_format, map}]), WS2};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 跳过无关帧（在线通知、离线补偿投递等噪声）直到 Pred(JSON) 为 true。
recv_json_matching(WS, Pred, Timeout) ->
    recv_json_matching(WS, Pred, Timeout, Timeout).

recv_json_matching(WS, Pred, Timeout, Deadline) ->
    case recv_json_text(WS, Timeout) of
        {ok, Frame, WS2} ->
            case Pred(Frame) of
                true ->
                    {ok, Frame, WS2};
                false ->
                    ct:log("跳过无关帧: ~p", [Frame]),
                    recv_json_matching(WS2, Pred, Timeout, Deadline)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

recv_text_matching(WS, Pred, Timeout) ->
    case ws_recv_frame(WS, Timeout) of
        {ok, 16#1, Payload, WS2} ->
            case Pred(Payload) of
                true ->
                    {ok, Payload, WS2};
                false ->
                    ct:log("跳过无关文本帧: ~p", [Payload]),
                    recv_text_matching(WS2, Pred, Timeout)
            end;
        {ok, _OtherOpcode, _Payload, WS2} ->
            recv_text_matching(WS2, Pred, Timeout);
        {error, Reason} ->
            {error, Reason}
    end.

ws_recv_frame({ws, Sock, Buf}, Timeout) ->
    case decode_frame(Buf) of
        {ok, Opcode, Payload, Rest} ->
            {ok, Opcode, Payload, {ws, Sock, Rest}};
        need_more ->
            case gen_tcp:recv(Sock, 0, Timeout) of
                {ok, Data} ->
                    ws_recv_frame({ws, Sock, <<Buf/binary, Data/binary>>}, Timeout);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% 服务端 → 客户端帧不带掩码（RFC6455 §5.1）
decode_frame(<<1:1, 0:3, Opcode:4, 0:1, Len:7, Rest/binary>>) when
    Len < 126, byte_size(Rest) >= Len
->
    <<Payload:Len/binary, Buf2/binary>> = Rest,
    {ok, Opcode, Payload, Buf2};
decode_frame(<<1:1, 0:3, Opcode:4, 0:1, 126:7, Len:16, Rest/binary>>) when
    byte_size(Rest) >= Len
->
    <<Payload:Len/binary, Buf2/binary>> = Rest,
    {ok, Opcode, Payload, Buf2};
decode_frame(<<1:1, 0:3, Opcode:4, 0:1, 127:7, Len:64, Rest/binary>>) when
    byte_size(Rest) >= Len
->
    <<Payload:Len/binary, Buf2/binary>> = Rest,
    {ok, Opcode, Payload, Buf2};
decode_frame(_Buf) ->
    need_more.

%% @doc 读取响应头（直到 \r\n\r\n），返回 {ok, Head, 剩余字节缓冲}
recv_head(Sock, Buf) ->
    case binary:split(Buf, <<"\r\n\r\n">>) of
        [Head, Rest] ->
            {ok, Head, Rest};
        _ ->
            {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
            recv_head(Sock, <<Buf/binary, Data/binary>>)
    end.

recv_exact(_Sock, Buf, N) when byte_size(Buf) >= N ->
    <<Body:N/binary, _/binary>> = Buf,
    Body;
recv_exact(Sock, Buf, N) ->
    {ok, Data} = gen_tcp:recv(Sock, 0, 5000),
    recv_exact(Sock, <<Buf/binary, Data/binary>>, N).

parse_status(<<"HTTP/1.1 ", StatusBin:3/binary, _/binary>>) ->
    {ok, binary_to_integer(StatusBin)};
parse_status(<<"HTTP/1.0 ", StatusBin:3/binary, _/binary>>) ->
    {ok, binary_to_integer(StatusBin)}.

parse_header(Line) ->
    [K, V] = binary:split(Line, <<":">>),
    {lower_ascii(K), trim_leading(V)}.

lower_ascii(Bin) ->
    <<
        <<
            (if
                C >= $A, C =< $Z -> C + 32;
                true -> C
            end)
        >>
     || <<C:8>> <= Bin
    >>.

trim_leading(<<" ", Rest/binary>>) ->
    trim_leading(Rest);
trim_leading(Bin) ->
    Bin.
