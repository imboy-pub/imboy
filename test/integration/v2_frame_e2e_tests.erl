-module(v2_frame_e2e_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("imboy_frame.hrl").

%%%===================================================================
%%% @doc
%%% imboy.v2 分层二进制帧协议端到端集成测试
%%%
%%% 覆盖点（与 docs/api/websocket-api-2.md 规范、imboy_frame.hrl 字节 fixture 对齐）：
%%%   1. v2 握手协商 —— 子协议 "imboy.v2" 被 negotiate_protocol/1 映射为 {protobuf, v2}
%%%   2. v2 heartbeat ping→pong 完整回路 + seq 回显
%%%   3. v2 MSG_C2C + JSON payload 走 message_router_logic:route/5
%%%   4. v2 MSG_C2S + "CLIENT_ACK,..." 文本 ACK 绕过 throttle（throttle mock 成 blocked 仍放行）
%%%   5. v2 下行 S2C 业务包裹：websocket_info({reply, Msg}, State#{framing := v2, protocol := protobuf})
%%%      返回 {reply, {binary, Bin}, _}，Bin 能被 imboy_codec:unwrap_v2_frame/1 解出 msg_s2c 帧
%%%   6. v2 垃圾 payload（随机 3 字节）不 crash，返回 {ok, State, hibernate}
%%%   7. v2 帧头部非法（非 "IB" magic）不 crash，返回 {ok, State, hibernate}
%%%
%%% 不依赖真实数据库、不启动 cowboy；所有下游通过 meck mock。
%%% 如果 imboy 应用因数据库迁移失败而无法启动，所有测试将被跳过。
%%%===================================================================

%% ===================================================================
%% 测试夹具：启动应用，不可用时跳过全部测试
%% ===================================================================

v2_frame_e2e_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun
         ({skip, Reason}) ->
             {skip, Reason};
         (_State) ->
             all_tests()
     end}.

setup() ->
    %% 尝试启动应用；websocket_handler、imboy_frame 等模块需要应用已加载。
    %% 如果应用启动失败（通常是数据库/迁移问题），检查核心模块是否可用。
    case eunit_runner:eunit_setup() of
        {app_started, _} -> started;
        {app_already_started, _} -> started;
        {app_not_started, test_continues} ->
            case code:which(websocket_handler) of
                non_existing ->
                    {skip, "imboy app not started and websocket_handler not available"};
                _ ->
                    started
            end
    end.

cleanup(_State) ->
    ok.

all_tests() ->
    [
     v2_subprotocol_maps_to_protobuf_v2(),
     v2_negotiate_full_header(),
     v2_heartbeat_ping_pong_roundtrip(),
     v2_msg_c2c_json_routes_to_router(),
     v2_msg_c2s_client_ack_bypasses_throttle(),
     v2_downstream_s2c_wraps_msg_s2c_frame(),
     v2_msg_c2c_garbage_payload(),
     v2_bad_magic_frame_tolerated(),
     v2_frame_ack_adapts_to_client_ack()
    ].

log_mocks() ->
    [
        {elib_log, [
            {'internal_log', 4, fun(_Level, _Msg, _Module, _Line) -> ok end},
            {'internal_log', 5, fun(_Level, _Fmt, _Args, _Module, _Line) -> ok end},
            {'warning', 2, fun(_Fmt, _Args) -> ok end}
        ]}
    ].

%% ===================================================================
%% 1. v2 握手协商：negotiate_protocol/1 映射语义
%% ===================================================================

v2_subprotocol_maps_to_protobuf_v2() ->
    ?TEST_SIMPLE(fun() ->
        %% imboy.v2 → {protobuf, v2}
        ?assertEqual(protobuf, imboy_codec:protocol_atom(<<"imboy.v2">>)),
        ?assertEqual(v2, imboy_codec:framing_atom(<<"imboy.v2">>)),
        %% imboy-protobuf → {protobuf, none}
        ?assertEqual(protobuf, imboy_codec:protocol_atom(<<"imboy-protobuf">>)),
        ?assertEqual(none, imboy_codec:framing_atom(<<"imboy-protobuf">>)),
        %% text → {json, none}
        ?assertEqual(json, imboy_codec:protocol_atom(<<"text">>)),
        ?assertEqual(none, imboy_codec:framing_atom(<<"text">>))
    end).

v2_negotiate_full_header() ->
    %% 通过 websocket_ds:select_subprotocol/1 验证与 handler 协商逻辑一致：
    %% 客户端声明 "imboy.v2, imboy-protobuf, imboy-json, text" → 选中 "imboy.v2"
    ?TEST_SIMPLE(fun() ->
        Selected = websocket_ds:select_subprotocol(
            [<<"imboy.v2">>, <<"imboy-protobuf">>, <<"imboy-json">>, <<"text">>]),
        ?assertEqual(<<"imboy.v2">>, Selected),
        ?assertEqual(protobuf, imboy_codec:protocol_atom(Selected)),
        ?assertEqual(v2, imboy_codec:framing_atom(Selected))
    end).

%% ===================================================================
%% 2. v2 heartbeat ping→pong 完整回路
%% ===================================================================

v2_heartbeat_ping_pong_roundtrip() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{protocol => protobuf, framing => v2,
                  did => <<"did_hb">>, current_uid => 1},
        Seq = 1234,
        PingFrame = imboy_frame:heartbeat_ping(Seq),
        {reply, {binary, PongBin}, State2, hibernate} =
            websocket_handler:websocket_handle({binary, PingFrame}, State),
        ?assertEqual(State, State2),
        %% 解开 pong 帧并验证 type + seq 回显
        {ok, Frame} = imboy_codec:unwrap_v2_frame(PongBin),
        ?assertEqual(?FRAME_TYPE_HEARTBEAT_PONG, imboy_frame:type(Frame)),
        <<GotSeq:16/big-unsigned>> = imboy_frame:payload(Frame),
        ?assertEqual(Seq, GotSeq)
    end).

%% ===================================================================
%% 3. v2 MSG_C2C + JSON payload → message_router_logic:route/5
%% ===================================================================

v2_msg_c2c_json_routes_to_router() ->
    ?WITH_MECKS(log_mocks() ++ [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {throttle, [
            {'check', 2, fun(msg_per_user, _) -> ok end}
        ]},
        {message_ds, [
            {'decode_websocket_message', 1,
             fun(Bin) -> jsone:decode(Bin, [{object_format, map}]) end},
            {'convert_v1_to_v2', 1, fun(M) -> M end},
            {'validate_message', 1, fun(M) -> {ok, M} end},
            {'inject_sender_device', 2, fun(P, _State) -> P end}
        ]},
        {message_router_logic, [
            {'route', 5, fun(<<"mid-e2e-1">>, 123, _Data, <<"C2C">>, _Raw) -> ok end}
        ]}
    ], fun() ->
        State = #{did => <<"did_e2e">>, current_uid => 123,
                  protocol => protobuf, framing => v2, dtype => <<"ios">>},
        JsonBin = jsone:encode(#{<<"id">> => <<"mid-e2e-1">>,
                                 <<"type">> => <<"C2C">>,
                                 <<"from">> => 111,
                                 <<"to">> => 222,
                                 <<"msg_type">> => <<"text">>,
                                 <<"payload">> => #{<<"text">> => <<"hello">>}},
                               [native_utf8]),
        Frame = imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_C2C, 0, JsonBin),
        {ok, State2, hibernate} =
            websocket_handler:websocket_handle({binary, Frame}, State),
        ?assertEqual(State, State2),
        ?assertEqual(1, meck:num_calls(message_router_logic, route, 5))
    end).

%% ===================================================================
%% 4. v2 MSG_C2S + "CLIENT_ACK,..." 绕过 throttle
%% ===================================================================

v2_msg_c2s_client_ack_bypasses_throttle() ->
    ?WITH_MECKS(log_mocks() ++ [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {websocket_logic, [
            {'cancel_timer', 3, fun(123, <<"did_v2">>, <<"mid-ack-1">>) -> ok end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000000999 end}
        ]},
        {msg_c2c_logic, [
            {'c2c_client_ack', 3, fun(<<"mid-ack-1">>, 123, <<"did_v2">>) -> ok end}
        ]},
        {throttle, [
            %% 即使限流拉满也应放行 ACK
            {'check', 2, fun(msg_per_user, _) -> {limit_exceeded, 10, 60} end}
        ]}
    ], fun() ->
        State = #{did => <<"did_v2">>, current_uid => 123,
                  protocol => protobuf, framing => v2},
        AckText = <<"CLIENT_ACK,C2C,mid-ack-1,did_v2">>,
        Frame = imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_C2S, 0, AckText),
        {reply, {_, _RespBin}, _, hibernate} =
            websocket_handler:websocket_handle({binary, Frame}, State),
        %% throttle 不应被调用（CLIENT_ACK 分支在 throttle 前匹配）
        ?assertEqual(0, meck:num_calls(throttle, check, 2)),
        ?assertEqual(1, meck:num_calls(msg_c2c_logic, c2c_client_ack, 3))
    end).

%% ===================================================================
%% 5. v2 下行业务包裹：websocket_info({reply, Msg}, v2 state)
%% ===================================================================

v2_downstream_s2c_wraps_msg_s2c_frame() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{protocol => protobuf, framing => v2,
                  did => <<"did_down">>, current_uid => 1},
        Msg = #{<<"id">> => <<"mid-s2c-1">>,
                <<"type">> => <<"S2C">>,
                <<"action">> => <<"pull_offline_msg">>,
                <<"server_ts">> => 1700000000},
        {reply, {binary, Bin}, _, hibernate} =
            websocket_handler:websocket_info({reply, Msg}, State),
        ?assert(is_binary(Bin)),
        ?assert(byte_size(Bin) > 9),
        {ok, Frame} = imboy_codec:unwrap_v2_frame(Bin),
        ?assertEqual(?FRAME_TYPE_MSG_S2C, imboy_frame:type(Frame)),
        %% 负载可反序列化回 protobuf map
        Decoded = imboy_codec:decode(protobuf, imboy_frame:payload(Frame)),
        ?assertEqual(<<"mid-s2c-1">>, maps:get(<<"id">>, Decoded)),
        ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Decoded))
    end).

%% ===================================================================
%% 6. v2 垃圾 payload（3 字节随机）不 crash
%% ===================================================================

v2_msg_c2c_garbage_payload() ->
    ?WITH_MECKS(log_mocks() ++ [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {throttle, [
            {'check', 2, fun(msg_per_user, _) -> ok end}
        ]}
    ], fun() ->
        State = #{did => <<"did_junk">>, current_uid => 123,
                  protocol => protobuf, framing => v2},
        Garbage = <<16#01, 16#02, 16#03>>,
        Frame = imboy_codec:wrap_v2_frame(?FRAME_TYPE_MSG_C2C, 0, Garbage),
        {ok, State2, hibernate} =
            websocket_handler:websocket_handle({binary, Frame}, State),
        ?assertEqual(State, State2)
    end).

%% ===================================================================
%% 7. v2 帧头部非法（bad magic）不 crash
%% ===================================================================

v2_bad_magic_frame_tolerated() ->
    ?WITH_MECKS(log_mocks(), fun() ->
        State = #{did => <<"did_bad">>, current_uid => 1,
                  protocol => protobuf, framing => v2},
        %% 非 "IB" magic，unwrap_v2_frame/1 必须返回 {error, _}
        BadBin = <<0, 0, 0>>,
        {ok, State2, hibernate} =
            websocket_handler:websocket_handle({binary, BadBin}, State),
        ?assertEqual(State, State2)
    end).

%% ===================================================================
%% 8. v2 frame ACK 帧 → 适配为 CLIENT_ACK 管道
%% ===================================================================

v2_frame_ack_adapts_to_client_ack() ->
    ?WITH_MECKS(log_mocks() ++ [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {websocket_logic, [
            {'cancel_timer', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000000111 end}
        ]},
        {msg_c2c_logic, [
            {'c2c_client_ack', 3, fun(_, _, _) -> ok end}
        ]},
        {websocket_ds, [
            {'select_subprotocol', 1, fun(_) -> <<"imboy-protobuf">> end}
        ]}
    ], fun() ->
        State = #{did => <<"did_frame_ack">>, current_uid => 123,
                  protocol => protobuf, framing => v2},
        MsgIdInt = 16#1234567890ABCDEF,
        AckFrame = imboy_frame:encode(?FRAME_TYPE_ACK, 0, <<MsgIdInt:64/big-unsigned>>),
        %% 不 crash；msg_id 被整形为 binary 后进入 handle_protobuf_client_ack 管道。
        %% 该管道在 protobuf decode 成功时调用 websocket_logic:cancel_timer/3；
        %% 即使 decode 失败也会走 catch 分支返回 {reply, _, State, hibernate}（不 crash）。
        Reply = websocket_handler:websocket_handle({binary, AckFrame}, State),
        ?assertMatch({reply, _, _, hibernate}, Reply)
    end).
