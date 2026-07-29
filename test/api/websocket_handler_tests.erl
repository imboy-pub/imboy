-module(websocket_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

log_mocks() ->
    [
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end},
            {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end}
        ]}
    ].

init_device_throttle_exceeded_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_) -> [] end},
                {'peer', 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun(_, _, _) -> undefined end},
                {'parse_header', 2, fun(_, _) -> undefined end},
                {'reply', 2, fun(429, _Req) -> req_429 end}
            ]},
            {throttle, [
                {'check', 2, fun(throttle_ws, _Did) -> {limit_exceeded, 10, 60} end}
            ]},
            {elib_log, [
                {'warning', 2, fun(_Fmt, _Args) -> ok end}
            ]}
        ],
        fun() ->
            State0 = #{state_key => v},
            ?assertEqual({ok, req_429, State0}, websocket_handler:init(req0, State0))
        end
    ).

init_subprotocol_ok_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_) -> [] end},
                {'peer', 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun
                    (<<"vsn">>, _, _) -> <<"1.0.0">>;
                    (<<"did">>, _, _) -> <<"did_1">>;
                    (<<"cos">>, _, _) -> <<"ios">>;
                    (<<"authorization">>, _, _) -> <<"token">>;
                    (_, _, Default) -> Default
                end},
                {'parse_header', 2, fun(_, _) -> undefined end}
            ]},
            {throttle, [
                {'check', 2, fun(throttle_ws, _Did) -> ok end}
            ]},
            {websocket_ds, [
                {'check_subprotocols', 2, fun(undefined, _Req0) -> {ok, req1} end}
            ]}
        ],
        fun() ->
            {ok, Req1, State1} = websocket_handler:init(req0, #{}),
            ?assertEqual(req1, Req1),
            ?assertEqual(<<"did_1">>, maps:get(did, State1)),
            ?assertEqual(<<"ios">>, maps:get(dtype, State1))
        end
    ).

init_auth_path_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_) -> [] end},
                {'peer', 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun
                    (<<"vsn">>, _, _) -> <<"1.0.0">>;
                    (<<"did">>, _, _) -> <<"did_2">>;
                    (<<"cos">>, _, _) -> <<"android">>;
                    (<<"authorization">>, _, _) -> <<"Bearer abc">>;
                    (_, _, Default) -> Default
                end},
                {'parse_header', 2, fun(_, _) -> undefined end}
            ]},
            {throttle, [
                {'check', 2, fun(throttle_ws, _Did) -> ok end}
            ]},
            {websocket_ds, [
                {'check_subprotocols', 2, fun(undefined, _Req0) ->
                    {cowboy_websocket, req1, s, o}
                end},
                {'auth', 4, fun(parsed_auth, req1, State1, _Opt0) ->
                    {cowboy_websocket, req1, State1#{current_uid => 123}, #{idle_timeout => 128000}}
                end}
            ]},
            {auth_ds, [
                {'parse_authorization_header', 1, fun(<<"Bearer abc">>) -> parsed_auth end}
            ]}
        ],
        fun() ->
            {cowboy_websocket, Req1, State1, _Opt} = websocket_handler:init(req0, #{}),
            ?assertEqual(req1, Req1),
            ?assertEqual(123, maps:get(current_uid, State1))
        end
    ).

websocket_init_error_state_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> 1700000000 end}
            ]}
        ],
        fun() ->
            State = #{error => 401},
            {reply, [{text, Bin}, close], State2, hibernate} = websocket_handler:websocket_init(
                State
            ),
            ?assertEqual(State, State2),
            Decoded = jsone:decode(Bin, [{object_format, map}]),
            ?assertEqual(401, maps:get(<<"code">>, Decoded))
        end
    ).

websocket_handle_ping_text_test_() ->
    ?TEST_SIMPLE(fun() ->
        {reply, {text, <<"pong">>}, #{}, hibernate} = websocket_handler:websocket_handle(
            {text, <<"ping">>}, #{}
        ),
        ok
    end).

websocket_handle_client_ack_success_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {websocket_logic, [
                    {'cancel_timer', 3, fun(123, <<"did_1">>, <<"msg_1">>) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {msg_c2c_logic, [
                    {'c2c_client_ack', 3, fun(<<"msg_1">>, 123, <<"did_1">>) -> ok end}
                ]}
            ],
        fun() ->
            State = #{did => <<"did_1">>, current_uid => 123},
            {reply, {text, Bin}, _, hibernate} = websocket_handler:websocket_handle(
                {text, <<"CLIENT_ACK,C2C,msg_1,did_1">>}, State
            ),
            Decoded = jsone:decode(Bin, [{object_format, map}]),
            ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"action">>, Decoded))
        end
    ).

websocket_handle_client_ack_did_mismatch_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]}
            ],
        fun() ->
            State = #{did => <<"did_real">>, current_uid => 123},
            {reply, {text, Bin}, _, hibernate} = websocket_handler:websocket_handle(
                {text, <<"CLIENT_ACK,C2C,msg_1,did_fake">>}, State
            ),
            Decoded = jsone:decode(Bin, [{object_format, map}]),
            ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded)),
            ?assertEqual(<<"did_mismatch">>, maps:get(<<"reason">>, Decoded))
        end
    ).

websocket_info_timeout_without_ack_resend_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_cache, [
                    {'get', 1, fun(_Key) -> undefined end}
                ]},
                {message_ds, [
                    {'send_next', 6, fun(
                        1, <<"msg_1">>, <<"raw_msg">>, [100, 200], [<<"did_1">>], true
                    ) ->
                        ok
                    end}
                ]}
            ],
        fun() ->
            State = #{current_uid => 1},
            Info = {timeout, ref1, {[100, 200], {1, <<"did_1">>, <<"msg_1">>}, <<"raw_msg">>}},
            {reply, {text, <<"raw_msg">>}, State2, hibernate} = websocket_handler:websocket_info(
                Info, State
            ),
            ?assertEqual(State, State2)
        end
    ).

websocket_info_timeout_with_ack_received_skip_resend_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_cache, [
                    {'get', 1, fun(_Key) -> {ok, true} end}
                ]},
                {message_ds, [
                    {'send_next', 6, fun(_, _, _, _, _, _) ->
                        erlang:error(unexpected_send_next_call)
                    end}
                ]}
            ],
        fun() ->
            State = #{current_uid => 1},
            Info = {timeout, ref2, {[100, 200], {1, <<"did_1">>, <<"msg_2">>}, <<"raw_msg_2">>}},
            {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
            ?assertEqual(State, State2),
            ?assertEqual(0, meck:num_calls(message_ds, send_next, 6))
        end
    ).

websocket_info_timeout_with_invalid_ack_flag_resend_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_cache, [
                    {'get', 1, fun(_Key) -> {ok, <<"invalid">>} end}
                ]},
                {message_ds, [
                    {'send_next', 6, fun(
                        1, <<"msg_3">>, <<"raw_msg_3">>, [100, 200], [<<"did_1">>], true
                    ) ->
                        ok
                    end}
                ]}
            ],
        fun() ->
            State = #{current_uid => 1},
            Info = {timeout, ref3, {[100, 200], {1, <<"did_1">>, <<"msg_3">>}, <<"raw_msg_3">>}},
            {reply, {text, <<"raw_msg_3">>}, State2, hibernate} = websocket_handler:websocket_info(
                Info, State
            ),
            ?assertEqual(State, State2),
            ?assertEqual(1, meck:num_calls(message_ds, send_next, 6))
        end
    ).

websocket_info_ack_cancel_from_remote_success_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {websocket_logic, [
                    {'handle_ack_cancel', 3, fun(1, <<"did_1">>, <<"msg_ack_1">>) -> ok end}
                ]}
            ],
        fun() ->
            State = #{current_uid => 1},
            Info = {ack_cancel, 1, <<"did_1">>, <<"msg_ack_1">>, 1700000000123},
            {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
            ?assertEqual(State, State2),
            ?assertEqual(1, meck:num_calls(websocket_logic, handle_ack_cancel, 3))
        end
    ).

websocket_info_ack_cancel_from_remote_failure_is_tolerated_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {websocket_logic, [
                    {'handle_ack_cancel', 3, fun(_, _, _) ->
                        erlang:error(simulated_ack_cancel_failure)
                    end}
                ]}
            ],
        fun() ->
            State = #{current_uid => 1},
            Info = {ack_cancel, 1, <<"did_1">>, <<"msg_ack_2">>, 1700000000456},
            {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
            ?assertEqual(State, State2)
        end
    ).

terminate_with_uid_calls_offline_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {user_logic, [
                    {'offline', 3, fun(123, _Pid, <<"did_1">>) -> ok end}
                ]}
            ],
        fun() ->
            ok = websocket_handler:terminate(normal, req, #{current_uid => 123, did => <<"did_1">>})
        end
    ).

%% ===================================================================
%% Protocol negotiation tests (迭代3)
%% ===================================================================

init_stores_protocol_json_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_) -> [] end},
                {'peer', 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun
                    (<<"vsn">>, _, _) -> <<"1.0.0">>;
                    (<<"did">>, _, _) -> <<"did_proto">>;
                    (<<"cos">>, _, _) -> <<"android">>;
                    (<<"authorization">>, _, _) -> <<"Bearer abc">>;
                    (_, _, Default) -> Default
                end},
                {'parse_header', 2, fun(_, _) -> [<<"text">>] end}
            ]},
            {throttle, [
                {'check', 2, fun(throttle_ws, _Did) -> ok end}
            ]},
            {websocket_ds, [
                {'check_subprotocols', 2, fun([<<"text">>], _Req) ->
                    {cowboy_websocket, req1, s, o}
                end},
                {'select_subprotocol', 1, fun([<<"text">>]) -> <<"text">> end},
                {'auth', 4, fun(_Auth, req1, State1, _Opt0) ->
                    {cowboy_websocket, req1, State1#{current_uid => 456}, #{idle_timeout => 128000}}
                end}
            ]},
            {auth_ds, [
                {'parse_authorization_header', 1, fun(_) -> parsed_auth end}
            ]}
        ],
        fun() ->
            {cowboy_websocket, _Req, State, _Opt} = websocket_handler:init(req0, #{}),
            ?assertEqual(json, maps:get(protocol, State))
        end
    ).

init_stores_protocol_protobuf_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_) -> [] end},
                {'peer', 1, fun(_) -> {{127, 0, 0, 1}, 12345} end},
                {'header', 3, fun
                    (<<"vsn">>, _, _) -> <<"1.0.0">>;
                    (<<"did">>, _, _) -> <<"did_proto">>;
                    (<<"cos">>, _, _) -> <<"android">>;
                    (<<"authorization">>, _, _) -> <<"Bearer abc">>;
                    (_, _, Default) -> Default
                end},
                {'parse_header', 2, fun(_, _) -> [<<"imboy-protobuf">>, <<"text">>] end}
            ]},
            {throttle, [
                {'check', 2, fun(throttle_ws, _Did) -> ok end}
            ]},
            {websocket_ds, [
                {'check_subprotocols', 2, fun([<<"imboy-protobuf">>, <<"text">>], _Req) ->
                    {cowboy_websocket, req1, s, o}
                end},
                {'select_subprotocol', 1, fun([<<"imboy-protobuf">>, <<"text">>]) ->
                    <<"imboy-protobuf">>
                end},
                {'auth', 4, fun(_Auth, req1, State1, _Opt0) ->
                    {cowboy_websocket, req1, State1#{current_uid => 789}, #{idle_timeout => 128000}}
                end}
            ]},
            {auth_ds, [
                {'parse_authorization_header', 1, fun(_) -> parsed_auth end}
            ]}
        ],
        fun() ->
            {cowboy_websocket, _Req, State, _Opt} = websocket_handler:init(req0, #{}),
            ?assertEqual(protobuf, maps:get(protocol, State))
        end
    ).

websocket_init_error_always_json_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> 1700000000 end}
            ]}
        ],
        fun() ->
            %% 连接级错误始终使用 JSON text frame，即使协议为 protobuf；随后立即关闭连接
            State = #{error => 401, protocol => protobuf},
            {reply, [{text, Bin}, close], State2, hibernate} = websocket_handler:websocket_init(
                State
            ),
            ?assertEqual(State, State2),
            Decoded = jsone:decode(Bin, [{object_format, map}]),
            ?assertEqual(401, maps:get(<<"code">>, Decoded))
        end
    ).

websocket_handle_binary_ignored_for_json_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{protocol => json},
        {ok, State2, hibernate} = websocket_handler:websocket_handle({binary, <<1, 2, 3>>}, State),
        ?assertEqual(State, State2)
    end).

websocket_info_reply_map_protobuf_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{protocol => protobuf},
        %% 使用合法的 IMBoyMessage 格式
        Msg = #{
            <<"id">> => <<"msg-test">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"test_action">>,
            <<"server_ts">> => 1700000000
        },
        {reply, {binary, Bin}, _State2, hibernate} = websocket_handler:websocket_info(
            {reply, Msg}, State
        ),
        ?assert(is_binary(Bin)),
        ?assert(byte_size(Bin) > 0)
    end).

websocket_info_reply_map_json_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{protocol => json},
        Msg = #{<<"code">> => 200, <<"msg">> => <<"ok">>},
        {reply, {text, Bin}, _State2, hibernate} = websocket_handler:websocket_info(
            {reply, Msg}, State
        ),
        Decoded = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(200, maps:get(<<"code">>, Decoded))
    end).

%% ===================================================================
%% Delivery pipeline protocol adaptation tests (iteration 4)
%% ===================================================================

encode_delivery_frame_json_passthrough_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        %% JSON 协议：预编码消息直接作为 text frame 透传
        JsonMsg = jsone:encode(#{<<"id">> => <<"m1">>, <<"type">> => <<"S2C">>}, [native_utf8]),
        State = #{protocol => json},
        {reply, {text, Bin}, _, hibernate} = websocket_handler:websocket_info(
            {reply, JsonMsg}, State
        ),
        ?assertEqual(JsonMsg, Bin)
    end).

encode_delivery_frame_protobuf_reencode_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        %% Protobuf 协议：JSON 预编码消息被 decode -> reencode 为 protobuf binary frame
        Msg = #{
            <<"id">> => <<"m2">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"pull_offline_msg">>,
            <<"server_ts">> => 1700000000
        },
        JsonMsg = jsone:encode(Msg, [native_utf8]),
        State = #{protocol => protobuf},
        {reply, {binary, PbBin}, _, hibernate} = websocket_handler:websocket_info(
            {reply, JsonMsg}, State
        ),
        ?assert(is_binary(PbBin)),
        ?assert(byte_size(PbBin) > 0),
        %% 验证 protobuf roundtrip
        Decoded = imboy_codec:decode(protobuf, PbBin),
        ?assertEqual(<<"m2">>, maps:get(<<"id">>, Decoded)),
        ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Decoded))
    end).

timeout_delivery_protobuf_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {imboy_cache, [
                    {'get', 1, fun(_Key) -> undefined end}
                ]},
                {message_ds, [
                    {'send_next', 6, fun(_, _, _, _, _, _) -> ok end}
                ]}
            ],
        fun() ->
            %% 验证超时重传路径对 protobuf 客户端使用 binary frame
            Msg = #{
                <<"id">> => <<"m3">>,
                <<"type">> => <<"C2C">>,
                <<"from">> => 111,
                <<"to">> => 222,
                <<"msg_type">> => <<"text">>,
                <<"server_ts">> => 1700000000
            },
            JsonMsg = jsone:encode(Msg, [native_utf8]),
            State = #{current_uid => 1, protocol => protobuf},
            Info = {timeout, ref1, {[100], {1, <<"did_1">>, <<"m3">>}, JsonMsg}},
            {reply, {binary, PbBin}, _, hibernate} = websocket_handler:websocket_info(Info, State),
            ?assert(byte_size(PbBin) > 0),
            Decoded = imboy_codec:decode(protobuf, PbBin),
            ?assertEqual(<<"m3">>, maps:get(<<"id">>, Decoded))
        end
    ).

timeout_final_retry_protobuf_test_() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        %% 最终重试超时也使用协议感知帧
        Msg = #{
            <<"id">> => <<"m4">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"test">>,
            <<"server_ts">> => 1700000000
        },
        JsonMsg = jsone:encode(Msg, [native_utf8]),
        State = #{protocol => protobuf},
        Info = {timeout, ref2, {[], {1, <<"did_1">>, <<"m4">>}, JsonMsg}},
        {reply, {binary, PbBin}, _, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assert(byte_size(PbBin) > 0)
    end).

%% ===================================================================
%% Iteration 5: CLIENT_ACK 协议重构
%% ===================================================================

%% 测试 protobuf 格式的 CLIENT_ACK（二进制帧）
protobuf_client_ack_success_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {websocket_logic, [
                    {'cancel_timer', 3, fun(123, <<"did_1">>, <<"msg_pb_1">>) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {msg_c2c_logic, [
                    {'c2c_client_ack', 3, fun(<<"msg_pb_1">>, 123, <<"did_1">>) -> ok end}
                ]},
                {websocket_ds, [
                    {'select_subprotocol', 1, fun(_) -> <<"imboy-protobuf">> end}
                ]}
            ],
        fun() ->
            State = #{did => <<"did_1">>, current_uid => 123, protocol => protobuf},
            %% 构建 protobuf CLIENT_ACK 消息
            AckPayload = imboy_pb:encode_msg(
                #{msg_direction => 'C2C', msg_id => <<"msg_pb_1">>, did => <<"did_1">>},
                'PayloadClientAck'
            ),
            PbMsg = imboy_codec:encode(
                protobuf,
                #{
                    <<"id">> => <<"msg_pb_1">>,
                    <<"type">> => <<"CLIENT_ACK">>,
                    <<"msg_type">> => <<"client_ack">>,
                    <<"payload">> => AckPayload
                }
            ),
            %% ACK 响应带 in_reply_to（T14 契约），IMBoyMessage schema 装不下，
            %% 故一律退回 JSON 载荷 —— 见 imboy_codec:pb_lossless/1
            {reply, {text, RespBin}, _, hibernate} = websocket_handler:websocket_handle(
                {binary, PbMsg}, State
            ),
            ?assert(byte_size(RespBin) > 0),
            Decoded = imboy_codec:decode(json, RespBin),
            ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"type">>, Decoded)),
            ?assertEqual(<<"msg_pb_1">>, maps:get(<<"in_reply_to">>, Decoded))
        end
    ).

%% 测试 protobuf CLIENT_ACK DID 不匹配
protobuf_client_ack_did_mismatch_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {websocket_ds, [
                    {'select_subprotocol', 1, fun(_) -> <<"imboy-protobuf">> end}
                ]}
            ],
        fun() ->
            State = #{did => <<"did_real">>, current_uid => 123, protocol => protobuf},
            AckPayload = imboy_pb:encode_msg(
                #{msg_direction => 'C2C', msg_id => <<"msg_pb_2">>, did => <<"did_fake">>},
                'PayloadClientAck'
            ),
            PbMsg = imboy_codec:encode(
                protobuf,
                #{
                    <<"id">> => <<"msg_pb_2">>,
                    <<"type">> => <<"CLIENT_ACK">>,
                    <<"msg_type">> => <<"client_ack">>,
                    <<"payload">> => AckPayload
                }
            ),
            {reply, {text, RespBin}, _, hibernate} = websocket_handler:websocket_handle(
                {binary, PbMsg}, State
            ),
            Decoded = imboy_codec:decode(json, RespBin),
            ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded)),
            %% 客户端靠 id / in_reply_to 关联回原消息，reason 说明被拒原因；
            %% 三者任一丢失都会退化成"确认超时→无限重发"
            ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"type">>, Decoded)),
            ?assertEqual(<<"msg_pb_2">>, maps:get(<<"id">>, Decoded)),
            ?assertEqual(<<"msg_pb_2">>, maps:get(<<"in_reply_to">>, Decoded)),
            ?assert(maps:is_key(<<"reason">>, Decoded))
        end
    ).

%% 测试文本 CLIENT_ACK 对 protobuf 客户端返回 binary 帧
text_client_ack_protobuf_response_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {websocket_logic, [
                    {'cancel_timer', 3, fun(123, <<"did_1">>, <<"msg_t1">>) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {msg_c2c_logic, [
                    {'c2c_client_ack', 3, fun(<<"msg_t1">>, 123, <<"did_1">>) -> ok end}
                ]},
                {websocket_ds, [
                    {'select_subprotocol', 1, fun(_) -> <<"imboy-protobuf">> end}
                ]}
            ],
        fun() ->
            %% protobuf 客户端发送文本格式的 CLIENT_ACK（向后兼容）
            State = #{did => <<"did_1">>, current_uid => 123, protocol => protobuf},
            {reply, {text, RespBin}, _, hibernate} = websocket_handler:websocket_handle(
                {text, <<"CLIENT_ACK,C2C,msg_t1,did_1">>}, State
            ),
            ?assert(byte_size(RespBin) > 0),
            Decoded = imboy_codec:decode(json, RespBin),
            ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"type">>, Decoded)),
            ?assertEqual(<<"msg_t1">>, maps:get(<<"in_reply_to">>, Decoded))
        end
    ).

%% ===================================================================
%% Iteration 6: v2 framing 宽容 payload 解码（JSON text / protobuf / CLIENT_ACK text）
%% ===================================================================

%% v2 MSG_C2C 帧，payload 为 UTF-8(JSON) 字符串 —— Dart 客户端主线路径
v2_msg_c2c_json_payload_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {throttle, [
                    {'check', 2, fun(msg_per_user, _) -> ok end}
                ]},
                {message_ds, [
                    {'decode_websocket_message', 1, fun(Bin) ->
                        jsone:decode(Bin, [{object_format, map}])
                    end},
                    {'convert_v1_to_v2', 1, fun(M) -> M end},
                    {'validate_message', 1, fun(M) -> {ok, M} end},
                    {'inject_sender_device', 2, fun(P, _State) -> P end}
                ]},
                {message_router_logic, [
                    {'route', 5, fun(<<"mid-json-1">>, 123, _Data, <<"C2C">>, _Raw) -> ok end}
                ]}
            ],
        fun() ->
            State = #{
                did => <<"did_json">>,
                current_uid => 123,
                protocol => protobuf,
                framing => v2,
                dtype => <<"ios">>
            },
            JsonBin = jsone:encode(
                #{
                    <<"id">> => <<"mid-json-1">>,
                    <<"type">> => <<"C2C">>,
                    <<"from">> => 111,
                    <<"to">> => 222,
                    <<"payload">> => #{<<"text">> => <<"hi">>}
                },
                [native_utf8]
            ),
            Frame = imboy_codec:wrap_v2_frame(16#20, 0, JsonBin),
            {ok, State2, hibernate} = websocket_handler:websocket_handle({binary, Frame}, State),
            ?assertEqual(State, State2),
            ?assertEqual(1, meck:num_calls(message_router_logic, route, 5))
        end
    ).

%% v2 MSG_C2S 帧，payload 为 "CLIENT_ACK,C2C,msg_1,did_1" 纯文本 —— Dart ACK 上行路径
v2_msg_c2s_text_client_ack_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {websocket_logic, [
                    {'cancel_timer', 3, fun(123, <<"did_v2">>, <<"msg_v2_1">>) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {msg_c2c_logic, [
                    {'c2c_client_ack', 3, fun(<<"msg_v2_1">>, 123, <<"did_v2">>) -> ok end}
                ]}
            ],
        fun() ->
            State = #{
                did => <<"did_v2">>,
                current_uid => 123,
                protocol => protobuf,
                framing => v2
            },
            AckText = <<"CLIENT_ACK,C2C,msg_v2_1,did_v2">>,
            Frame = imboy_codec:wrap_v2_frame(16#22, 0, AckText),
            {reply, {binary, RespBin}, _, hibernate} = websocket_handler:websocket_handle(
                {binary, Frame}, State
            ),
            %% v2 连接：帧头保留，载荷退回 JSON
            {ok, RespFrame} = imboy_codec:unwrap_v2_frame(RespBin),
            Decoded = imboy_codec:decode(json, imboy_frame:payload(RespFrame)),
            ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"type">>, Decoded)),
            ?assertEqual(<<"msg_v2_1">>, maps:get(<<"in_reply_to">>, Decoded))
        end
    ).

%% v2 MSG_C2C 帧，payload 为损坏数据（既非 JSON 也非 protobuf）
%% 【T14/P1-3】不应 crash，且应回 ERROR 帧（原为静默丢弃）
v2_msg_c2c_garbage_payload_replies_error_frame_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {throttle, [
                    {'check', 2, fun(msg_per_user, _) -> ok end}
                ]}
            ],
        fun() ->
            State = #{
                did => <<"did_bad">>,
                current_uid => 123,
                protocol => protobuf,
                framing => v2
            },
            Garbage = <<16#FF, 16#FE, "not-json-not-pb">>,
            Frame = imboy_codec:wrap_v2_frame(16#20, 0, Garbage),
            {reply, {binary, RespBin}, State2, hibernate} =
                websocket_handler:websocket_handle({binary, Frame}, State),
            ?assertEqual(State, State2),
            {ok, RespFrame, <<>>} = imboy_frame:decode(RespBin),
            %% FRAME_TYPE_ERROR = 0x06
            ?assertEqual(16#06, imboy_frame:type(RespFrame)),
            ?assertEqual(<<"payload_decode_failed">>, imboy_frame:payload(RespFrame))
        end
    ).

%% 【T14/P1-3】未知 v2 帧类型应回 ERROR 帧（原为静默丢弃）
v2_unknown_frame_type_replies_error_frame_test_() ->
    ?WITH_MECKS(
        log_mocks(),
        fun() ->
            State = #{
                did => <<"did_x">>,
                current_uid => 123,
                protocol => protobuf,
                framing => v2
            },
            %% 0xEF 未定义的帧类型
            Frame = imboy_codec:wrap_v2_frame(16#EF, 0, <<"whatever">>),
            {reply, {binary, RespBin}, _, hibernate} =
                websocket_handler:websocket_handle({binary, Frame}, State),
            {ok, RespFrame, <<>>} = imboy_frame:decode(RespBin),
            ?assertEqual(16#06, imboy_frame:type(RespFrame)),
            ?assertEqual(
                <<"unsupported_frame_type:239">>, imboy_frame:payload(RespFrame)
            )
        end
    ).

%% 【T14/P1-5】版本不匹配的帧应回 ERROR 帧（unsupported_version）
v2_bad_version_frame_replies_error_frame_test_() ->
    ?WITH_MECKS(
        log_mocks(),
        fun() ->
            State = #{
                did => <<"did_x">>,
                current_uid => 123,
                protocol => protobuf,
                framing => v2
            },
            %% 手工构造 Ver=3 的帧（Magic 0x4942）
            BadFrame = <<16#4942:16, 3:8, 0:8, 16#20:8, 0:32>>,
            {reply, {binary, RespBin}, _, hibernate} =
                websocket_handler:websocket_handle({binary, BadFrame}, State),
            {ok, RespFrame, <<>>} = imboy_frame:decode(RespBin),
            ?assertEqual(16#06, imboy_frame:type(RespFrame)),
            ?assertEqual(<<"unsupported_version">>, imboy_frame:payload(RespFrame))
        end
    ).

%% 测试 protobuf CLIENT_ACK 不受速率限制
protobuf_client_ack_bypasses_throttle_test_() ->
    ?WITH_MECKS(
        log_mocks() ++
            [
                {auth_ds, [
                    {'current_uid', 1, fun(_State) -> 123 end}
                ]},
                {websocket_logic, [
                    {'cancel_timer', 3, fun(123, <<"did_1">>, <<"msg_th">>) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> 1700000000123 end}
                ]},
                {msg_c2c_logic, [
                    {'c2c_client_ack', 3, fun(<<"msg_th">>, 123, <<"did_1">>) -> ok end}
                ]},
                {throttle, [
                    %% 速率限制已触发，但 CLIENT_ACK 应绕过
                    {'check', 2, fun(msg_per_user, _) -> {limit_exceeded, 10, 60} end}
                ]},
                {websocket_ds, [
                    {'select_subprotocol', 1, fun(_) -> <<"imboy-protobuf">> end}
                ]}
            ],
        fun() ->
            State = #{did => <<"did_1">>, current_uid => 123, protocol => protobuf},
            AckPayload = imboy_pb:encode_msg(
                #{msg_direction => 'C2C', msg_id => <<"msg_th">>, did => <<"did_1">>},
                'PayloadClientAck'
            ),
            PbMsg = imboy_codec:encode(
                protobuf,
                #{
                    <<"id">> => <<"msg_th">>,
                    <<"type">> => <<"CLIENT_ACK">>,
                    <<"msg_type">> => <<"client_ack">>,
                    <<"payload">> => AckPayload
                }
            ),
            %% 即使速率限制触发，CLIENT_ACK 也应成功
            {reply, {text, RespBin}, _, hibernate} = websocket_handler:websocket_handle(
                {binary, PbMsg}, State
            ),
            Decoded = imboy_codec:decode(json, RespBin),
            ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"type">>, Decoded))
        end
    ).
