-module(websocket_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% websocket_handler 模块的 EUnit 测试
%%%
%%% 目标：验证 WebSocket Handler 功能
%%% 覆盖：连接初始化、消息处理、认证验证、错误处理、边界条件
%%%===================================================================

%% ===================================================================
%% init/2 测试 - WebSocket 握手与初始化
%% ===================================================================

%% @doc 测试设备限流 - 超过限制时返回 429
init_device_throttle_exceeded_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) ->
                {limit_exceeded, 100, 60}
            end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        DID = <<"device_test_123">>,
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => DID,
                <<"cos">> => <<"ios">>,
                <<"vsn">> => <<"1.0.0">>
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({ok, _Req, _State}, Result),

        % 验证返回 429 状态码
        {ok, Req2, _State2} = Result,
        Resp = cowboy_req_h:response(Req2),
        ?assertEqual(429, element(1, Resp))
    end).

%% @doc 测试子协议验证 - 未提供子协议返回 400
init_missing_subprotocol_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun(undefined, _Req) ->
                {ok, cowboy_req_h:new(#{response_status => 400})}
            end}
        ]}
    ], fun() ->
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => <<"device_123">>,
                <<"cos">> => <<"web">>
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({ok, _Req, _State}, Result)
    end).

%% @doc 测试有效子协议接受
init_valid_subprotocol_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun([<<"sip">>], Req0) ->
                Req1 = cowboy_req_h:set_resp_header(<<"sec-websocket-protocol">>, <<"sip">>, Req0),
                {cowboy_websocket, Req1, #{}, #{}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(Auth) -> Auth end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) ->
                {ok, 12345, 1640995200, <<"access">>}
            end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        Token = <<"valid_jwt_token">>,
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => <<"device_789">>,
                <<"authorization">> => Token
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({cowboy_websocket, _Req, _State, _Opt}, Result)
    end).

%% @doc 测试认证失败 - Token 过期
init_auth_token_expired_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun([<<"sip">>], Req0) ->
                Req1 = cowboy_req_h:set_resp_header(<<"sec-websocket-protocol">>, <<"sip">>, Req0),
                {cowboy_websocket, Req1, #{}, #{}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(Auth) -> Auth end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) ->
                {error, 705, <<"token_expired"/utf8>>, #{}}
            end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        ExpiredToken = <<"expired_token">>,
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => <<"device_expired">>,
                <<"authorization">> => ExpiredToken
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({ok, _Req, _State}, Result)
    end).

%% @doc 测试认证失败 - 无效 Token
init_auth_invalid_token_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun([<<"sip">>], Req0) ->
                Req1 = cowboy_req_h:set_resp_header(<<"sec-websocket-protocol">>, <<"sip">>, Req0),
                {cowboy_websocket, Req1, #{}, #{}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(Auth) -> Auth end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) ->
                {error, 401, <<"invalid_token"/utf8>>, #{}}
            end}
        ]}
    ], fun() ->
        InvalidToken = <<"invalid_token">>,
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => <<"device_invalid">>,
                <<"authorization">> => InvalidToken
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({ok, _Req, _State}, Result),
        {ok, _Req2, State2} = Result,
        ?assert(maps:is_key(error, State2))
    end).

%% ===================================================================
%% websocket_init/1 测试 - WebSocket 连接初始化
%% ===================================================================

%% @doc 测试 WebSocket 初始化 - 错误状态
websocket_init_with_error_state_test_() ->
    ?WITH_MECKS([
        {jsone, [
            {'encode', 2, fun(Msg, _Opts) ->
                jsx:encode(Msg)
            end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> 1640995200 end}
        ]}
    ], fun() ->
        State = #{error => 401, did => <<"device_err">>},

        Result = websocket_handler:websocket_init(State),
        ?assertMatch({reply, {text, _Msg}, _State2, hibernate}, Result),
        {reply, {text, Msg}, _State3, hibernate} = Result,

        % 验证错误消息格式
        Decoded = jsx:decode(Msg),
        ?assert(maps:is_key(<<"code">>, Decoded)),
        ?assertEqual(401, maps:get(<<"code">>, Decoded))
    end).

%% @doc 测试 WebSocket 初始化 - 正常状态
websocket_init_normal_state_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) ->
                maps:get(current_uid, S)
            end}
        ]},
        {user_logic, [
            {'online', 4, fun(_Uid, _DType, _Pid, _DID) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_online">>,
            dtype => <<"ios">>
        },

        Result = websocket_handler:websocket_init(State),
        ?assertMatch({ok, _State2, hibernate}, Result)
    end).

%% ===================================================================
%% websocket_handle/2 测试 - WebSocket 消息处理
%% ===================================================================

%% @doc 测试处理 ping 帧
websocket_handle_ping_frame_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},

        % 测试无 payload 的 ping
        Result1 = websocket_handler:websocket_handle(ping, State),
        ?assertMatch({ok, _State, hibernate}, Result1),

        % 测试带 payload 的 ping
        Payload = <<"ping_payload">>,
        Result2 = websocket_handler:websocket_handle({ping, Payload}, State),
        ?assertMatch({ok, _State2, hibernate}, Result2)
    end).

%% @doc 测试应用层心跳消息
websocket_handle_app_heartbeat_test_() ->
    ?TEST_SIMPLE(fun() ->
        State = #{current_uid => 12345},

        % 测试小写 ping
        Result1 = websocket_handler:websocket_handle({text, <<"ping">>}, State),
        ?assertMatch({reply, {text, <<"pong">>}, _State, hibernate}, Result1),

        % 测试大写 PING
        Result2 = websocket_handler:websocket_handle({text, <<"PING">>}, State),
        ?assertMatch({reply, {text, <<"PONG">>}, _State2, hibernate}, Result2)
    end).

%% @doc 测试处理 CLIENT_ACK 消息
websocket_handle_client_ack_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}
        ]},
        {websocket_logic, [
            {'cancel_timer', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {msg_c2c_logic, [
            {'c2c_client_ack', 3, fun(_MsgId, _Uid, _DID) -> ok end}
        ]},
        {jsone, [
            {'encode', 2, fun(Msg, Opts) ->
                jsone:encode(Msg, Opts)
            end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1640995200000 end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_ack">>
        },
        AckMsg = <<"CLIENT_ACK,C2C,msg_123,device_ack">>,

        Result = websocket_handler:websocket_handle({text, AckMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result),
        {reply, {text, Resp}, _State3, hibernate} = Result,

        % 验证 ACK 确认消息格式
        Decoded = jsone:decode(Resp),
        ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"action">>, Decoded)),
        ?assertEqual(<<"msg_123">>, maps:get(<<"id">>, Decoded))
    end).

%% @doc 测试 CLIENT_ACK 参数验证失败 - MsgId 过长
websocket_handle_client_ack_invalid_msgid_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}
        ]},
        {jsone, [
            {'encode', 2, fun(Msg, Opts) ->
                jsone:encode(Msg, Opts)
            end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1640995200000 end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_invalid">>
        },
        % 创建超过 128 字节的 MsgId
        LongMsgId = binary:copy(<<"a">>, 129),
        AckMsg = <<"CLIENT_ACK,C2C,", LongMsgId/binary, ",device_invalid">>,

        Result = websocket_handler:websocket_handle({text, AckMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result),
        {reply, {text, Resp}, _State3, hibernate} = Result,

        Decoded = jsone:decode(Resp),
        ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded)),
        ?assertEqual(<<"invalid_msgid">>, maps:get(<<"reason">>, Decoded))
    end).

%% @doc 测试 CLIENT_ACK 参数验证失败 - DID 不匹配
websocket_handle_client_ack_did_mismatch_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}
        ]},
        {jsone, [
            {'encode', 2, fun(Msg, Opts) ->
                jsone:encode(Msg, Opts)
            end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1640995200000 end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_correct">>
        },
        % 使用不同的 DID
        AckMsg = <<"CLIENT_ACK,C2C,msg_456,device_wrong">>,

        Result = websocket_handler:websocket_handle({text, AckMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result),
        {reply, {text, Resp}, _State3, hibernate} = Result,

        Decoded = jsone:decode(Resp),
        ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded)),
        ?assertEqual(<<"did_mismatch">>, maps:get(<<"reason">>, Decoded))
    end).

%% @doc 测试处理 JSON 消息
websocket_handle_json_message_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}
        ]},
        {message_ds, [
            {'decode_websocket_message', 1, fun(Msg) ->
                jsone:decode(Msg)
            end},
            {'inject_sender_device', 2, fun(Payload, S) ->
                Payload#{<<"sender_did">> => maps:get(did, S, <<>>)}
            end}
        ]},
        {message_router_logic, [
            {'route', 5, fun(_MsgId, _Uid, _Data, _Type, _OrigMsg) -> ok end}
        ]},
        {jsone, [
            {'decode', 1, fun(Msg) -> jsone:decode(Msg) end},
            {'encode', 2, fun(Msg, Opts) -> jsone:encode(Msg, Opts) end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end},
            {'error', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_json">>
        },
        JsonMsg = <<"{\"id\":\"msg_789\",\"type\":\"C2C\",\"payload\":{\"text\":\"Hello\"}}"/utf8>>,

        Result = websocket_handler:websocket_handle({text, JsonMsg}, State),
        ?assertMatch({ok, _State2, hibernate}, Result)
    end).

%% @doc 测试处理 JSON 消息返回响应
websocket_handle_json_message_with_reply_test_() ->
    ?WITH_MECKS([
        {auth_ds, [{'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}]},
        {message_ds, [{'decode_websocket_message', 1, fun(Msg) -> jsone:decode(Msg) end},
                      {'inject_sender_device', 2, fun(Payload, S) -> Payload#{<<"sender_did">> => maps:get(did, S, <<>>)} end}]},
        {message_router_logic, [{'route', 5, fun(_MsgId, _Uid, _Data, _Type, _OrigMsg) -> {reply, #{<<"action">> => <<"ok">>}} end}]},
        {jsone, [{'decode', 1, fun(Msg) -> jsone:decode(Msg) end},
                 {'encode', 2, fun(Msg, Opts) -> jsone:encode(Msg, Opts) end}]},
        {elib_log, [{'warning', 2, fun(_Msg, _Params) -> ok end},
                    {'error', 2, fun(_Msg, _Params) -> ok end}]}
    ], fun() ->
        State = #{current_uid => 12345, did => <<"device_reply">>},
        JsonMsg = <<"{\"id\":\"msg_999\",\"type\":\"C2S\",\"payload\":{}}"/utf8>>,
        Result = websocket_handler:websocket_handle({text, JsonMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理二进制消息
websocket_handle_binary_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        State = #{current_uid => 12345},
        BinaryMsg = <<"binary_data">>,

        Result = websocket_handler:websocket_handle({binary, BinaryMsg}, State),
        ?assertMatch({[{binary, _BinaryMsg}], _State}, Result),
        {[{binary, ReturnedBinary}], _State2} = Result,
        ?assertEqual(BinaryMsg, ReturnedBinary)
    end).

%% @doc 测试处理未知帧类型
websocket_handle_unknown_frame_test_() ->
    ?TEST_SIMPLE(fun() ->
        State = #{current_uid => 12345},

        Result = websocket_handler:websocket_handle(unknown_frame, State),
        ?assertMatch({ok, _State, hibernate}, Result)
    end).

%% ===================================================================
%% websocket_info/2 测试 - Erlang 消息处理
%% ===================================================================

%% @doc 测试处理 map 类型 reply 消息
websocket_info_reply_map_test_() ->
    ?WITH_MECKS([
        {jsone, [
            {'encode', 2, fun(_Msg, _Opts) -> <<"{\"ok\":true}">> end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},
        ReplyMsg = #{<<"action">> => <<"test">>, <<"data">> => <<"value">>},

        Result = websocket_handler:websocket_info({reply, ReplyMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理 list 类型 reply 消息
websocket_info_reply_list_test_() ->
    ?WITH_MECKS([
        {jsone, [
            {'encode', 2, fun(_Msg, _Opts) -> <<"{\"ok\":true}">> end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},
        ReplyMsg = [{<<"action">>, <<"test">>}, {<<"data">>, <<"value">>}],

        Result = websocket_handler:websocket_info({reply, ReplyMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理 binary 类型 reply 消息
websocket_info_reply_binary_test_() ->
    ?TEST_SIMPLE(fun() ->
        State = #{current_uid => 12345},
        ReplyMsg = <<"plain_text">>,

        Result = websocket_handler:websocket_info({reply, ReplyMsg}, State),
        ?assertMatch({reply, {text, ReplyMsg}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理超时消息 - 空列表
websocket_info_timeout_empty_list_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'info', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},
        TimeoutMsg = <<"timeout_msg">>,

        Result = websocket_handler:websocket_info({timeout, make_ref(), {[], {}, TimeoutMsg}}, State),
        ?assertMatch({reply, {text, TimeoutMsg}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理超时消息 - 已收到 ACK
websocket_info_timeout_with_ack_received_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, true} end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end},
            {'info', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345, did => <<"device_timeout">>},
        TimeoutMsg = <<"retry_msg">>,
        MsgList = [2000, 5000],

        Result = websocket_handler:websocket_info(
            {timeout, make_ref(), {MsgList, {12345, <<"device_timeout">>, <<"msg_timeout">>}, TimeoutMsg}}, State),
        ?assertMatch({ok, _State2, hibernate}, Result)
    end).

%% @doc 测试处理超时消息 - 未收到 ACK
websocket_info_timeout_without_ack_test_() ->
    ?WITH_MECKS([
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end}
        ]},
        {message_ds, [
            {'send_next', 6, fun(_Uid, _MsgId, _Msg, _MsLi, _DIDs, _IsRetry) -> ok end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end},
            {'info', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345, did => <<"device_noack">>},
        TimeoutMsg = <<"noack_msg">>,
        MsgList = [2000, 5000],

        Result = websocket_handler:websocket_info(
            {timeout, make_ref(), {MsgList, {12345, <<"device_noack">>}, <<"msg_noack">>}, TimeoutMsg}, State),
        ?assertMatch({reply, {text, TimeoutMsg}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理其他超时消息
websocket_info_timeout_simple_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},
        TimeoutMsg = <<"simple_timeout">>,

        Result = websocket_handler:websocket_info({timeout, make_ref(), TimeoutMsg}, State),
        ?assertMatch({reply, {text, TimeoutMsg}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理 close 消息
websocket_info_close_message_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},
        CloseCode = 1000,
        Reason = <<"normal_closure">>,

        Result = websocket_handler:websocket_info({close, CloseCode, Reason}, State),
        ?assertMatch({reply, {close, CloseCode, Reason}, _State2, hibernate}, Result)
    end).

%% @doc 测试处理 stop 消息
websocket_info_stop_message_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},

        Result = websocket_handler:websocket_info(stop, State),
        ?assertMatch({stop, _State2}, Result)
    end).

%% @doc 测试处理跨节点 ACK 取消消息
websocket_info_ack_cancel_from_remote_test_() ->
    ?WITH_MECKS([
        {websocket_logic, [
            {'handle_ack_cancel', 3, fun(_Uid, _DID, _MsgId) -> ok end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 12345},

        Result = websocket_handler:websocket_info(
            {ack_cancel, 12345, <<"device_cancel">>, <<"msg_cancel">>, 1640995200}, State),
        ?assertMatch({ok, _State2, hibernate}, Result)
    end).

%% @doc 测试处理未知消息
websocket_info_unknown_message_test_() ->
    ?TEST_SIMPLE(fun() ->
        State = #{current_uid => 12345},

        Result = websocket_handler:websocket_info(unknown_message, State),
        ?assertMatch({ok, _State2, hibernate}, Result)
    end).

%% ===================================================================
%% terminate/3 测试 - 连接终止
%% ===================================================================

%% @doc 测试正常终止 - 用户下线
terminate_normal_with_uid_test_() ->
    ?WITH_MECKS([
        {user_logic, [
            {'offline', 3, fun(_Uid, _Pid, _DID) -> ok end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_term">>
        },

        Result = websocket_handler:terminate(normal, cowboy_req_h:new(#{}), State),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试终止 - 没有 current_uid
terminate_without_uid_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{did => <<"device_nouid">>},

        Result = websocket_handler:terminate(normal, cowboy_req_h:new(#{}), State),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试空设备 ID
init_with_empty_did_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun([<<"sip">>], Req0) ->
                Req1 = cowboy_req_h:set_resp_header(<<"sec-websocket-protocol">>, <<"sip">>, Req0),
                {cowboy_websocket, Req1, #{}, #{}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(Auth) -> Auth end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) ->
                {ok, 12345, 1640995200, <<"access">>}
            end}
        ]}
    ], fun() ->
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => <<>>,
                <<"authorization">> => <<"valid_token">>
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({cowboy_websocket, _Req, _State, _Opt}, Result)
    end).

%% @doc 测试超长 DID
init_with_very_long_did_test_() ->
    ?WITH_MECKS([
        {throttle, [
            {'check', 2, fun(_Name, _DID) -> ok end}
        ]},
        {websocket_ds, [
            {'check_subprotocols', 2, fun([<<"sip">>], Req0) ->
                Req1 = cowboy_req_h:set_resp_header(<<"sec-websocket-protocol">>, <<"sip">>, Req0),
                {cowboy_websocket, Req1, #{}, #{}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(Auth) -> Auth end}
        ]},
        {token_ds, [
            {'decrypt_token', 1, fun(_Token) ->
                {ok, 12345, 1640995200, <<"access">>}
            end}
        ]}
    ], fun() ->
        LongDID = binary:copy(<<"d">>, 128),
        MockReq = mock_cowboy_req(#{
            headers => #{
                <<"did">> => LongDID,
                <<"authorization">> => <<"valid_token">>
            }
        }),

        Result = websocket_handler:init(MockReq, #{}),
        ?assertMatch({cowboy_websocket, _Req, _State, _Opt}, Result)
    end).

%% @doc 测试 CLIENT_ACK 格式错误
websocket_handle_client_ack_parse_error_test_() ->
    ?WITH_MECKS([
        {auth_ds, [
            {'current_uid', 1, fun(S) -> maps:get(current_uid, S) end}
        ]},
        {jsone, [
            {'encode', 2, fun(Msg, Opts) ->
                jsone:encode(Msg, Opts)
            end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1640995200000 end}
        ]},
        {elib_log, [
            {'warning', 2, fun(_Msg, _Params) -> ok end},
            {'error', 2, fun(_Msg, _Params) -> ok end}
        ]}
    ], fun() ->
        State = #{
            current_uid => 12345,
            did => <<"device_parse">>
        },
        % 格式错误的 ACK 消息（缺少参数）
        AckMsg = <<"CLIENT_ACK,C2C">>,

        Result = websocket_handler:websocket_handle({text, AckMsg}, State),
        ?assertMatch({reply, {text, _Resp}, _State2, hibernate}, Result),
        {reply, {text, Resp}, _State3, hibernate} = Result,

        Decoded = jsone:decode(Resp),
        ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded))
    end).

%% ===================================================================
%% 内部辅助函数
%% ===================================================================

%% @doc 创建模拟的 Cowboy 请求对象
mock_cowboy_req(Overrides) ->
    Default = #{
        method => <<"GET">>,
        qs => <<>>,
        headers => #{},
        body => <<>>
    },
    Config = maps:merge(Default, Overrides),
    cowboy_req_h:new(Config).
