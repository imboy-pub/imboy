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
    ?WITH_MECKS([
        {cowboy_req, [
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
    ], fun() ->
        State0 = #{state_key => v},
        ?assertEqual({ok, req_429, State0}, websocket_handler:init(req0, State0))
    end).

init_subprotocol_ok_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
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
    ], fun() ->
        {ok, Req1, State1} = websocket_handler:init(req0, #{}),
        ?assertEqual(req1, Req1),
        ?assertEqual(<<"did_1">>, maps:get(did, State1)),
        ?assertEqual(<<"ios">>, maps:get(dtype, State1))
    end).

init_auth_path_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
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
            {'check_subprotocols', 2, fun(undefined, _Req0) -> {cowboy_websocket, req1, s, o} end},
            {'auth', 4, fun(parsed_auth, req1, State1, _Opt0) ->
                {cowboy_websocket, req1, State1#{current_uid => 123}, #{idle_timeout => 128000}}
            end}
        ]},
        {auth_ds, [
            {'parse_authorization_header', 1, fun(<<"Bearer abc">>) -> parsed_auth end}
        ]}
    ], fun() ->
        {cowboy_websocket, Req1, State1, _Opt} = websocket_handler:init(req0, #{}),
        ?assertEqual(req1, Req1),
        ?assertEqual(123, maps:get(current_uid, State1))
    end).

websocket_init_error_state_test_() ->
    ?WITH_MECKS([
        {elib_dt, [
            {'now', 0, fun() -> 1700000000 end}
        ]}
    ], fun() ->
        State = #{error => 401},
        {reply, {text, Bin}, State2, hibernate} = websocket_handler:websocket_init(State),
        ?assertEqual(State, State2),
        Decoded = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(401, maps:get(<<"code">>, Decoded))
    end).

websocket_handle_ping_text_test_() ->
    ?TEST_SIMPLE(fun() ->
        {reply, {text, <<"pong">>}, #{}, hibernate} = websocket_handler:websocket_handle({text, <<"ping">>}, #{}),
        ok
    end).

websocket_handle_client_ack_success_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
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
    ], fun() ->
        State = #{did => <<"did_1">>, current_uid => 123},
        {reply, {text, Bin}, _, hibernate} = websocket_handler:websocket_handle(
            {text, <<"CLIENT_ACK,C2C,msg_1,did_1">>}, State
        ),
        Decoded = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(<<"CLIENT_ACK_CONFIRM">>, maps:get(<<"action">>, Decoded))
    end).

websocket_handle_client_ack_did_mismatch_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {auth_ds, [
            {'current_uid', 1, fun(_State) -> 123 end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000000123 end}
        ]}
    ], fun() ->
        State = #{did => <<"did_real">>, current_uid => 123},
        {reply, {text, Bin}, _, hibernate} = websocket_handler:websocket_handle(
            {text, <<"CLIENT_ACK,C2C,msg_1,did_fake">>}, State
        ),
        Decoded = jsone:decode(Bin, [{object_format, map}]),
        ?assertEqual(<<"CLIENT_ACK_ERROR">>, maps:get(<<"action">>, Decoded)),
        ?assertEqual(<<"did_mismatch">>, maps:get(<<"reason">>, Decoded))
    end).

websocket_info_timeout_without_ack_resend_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun(_Key) -> undefined end}
        ]},
        {message_ds, [
            {'send_next', 6, fun(1, <<"msg_1">>, <<"raw_msg">>, [100, 200], [<<"did_1">>], false) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 1},
        Info = {timeout, ref1, {[100, 200], {1, <<"did_1">>, <<"msg_1">>}, <<"raw_msg">>}},
        {reply, {text, <<"raw_msg">>}, State2, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assertEqual(State, State2)
    end).

websocket_info_timeout_with_ack_received_skip_resend_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, true} end}
        ]},
        {message_ds, [
            {'send_next', 6, fun(_, _, _, _, _, _) -> erlang:error(unexpected_send_next_call) end}
        ]}
    ], fun() ->
        State = #{current_uid => 1},
        Info = {timeout, ref2, {[100, 200], {1, <<"did_1">>, <<"msg_2">>}, <<"raw_msg_2">>}},
        {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assertEqual(State, State2),
        ?assertEqual(0, meck:num_calls(message_ds, send_next, 6))
    end).

websocket_info_timeout_with_invalid_ack_flag_resend_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {imboy_cache, [
            {'get', 1, fun(_Key) -> {ok, <<"invalid">>} end}
        ]},
        {message_ds, [
            {'send_next', 6, fun(1, <<"msg_3">>, <<"raw_msg_3">>, [100, 200], [<<"did_1">>], false) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 1},
        Info = {timeout, ref3, {[100, 200], {1, <<"did_1">>, <<"msg_3">>}, <<"raw_msg_3">>}},
        {reply, {text, <<"raw_msg_3">>}, State2, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assertEqual(State, State2),
        ?assertEqual(1, meck:num_calls(message_ds, send_next, 6))
    end).

websocket_info_ack_cancel_from_remote_success_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {websocket_logic, [
            {'handle_ack_cancel', 3, fun(1, <<"did_1">>, <<"msg_ack_1">>) -> ok end}
        ]}
    ], fun() ->
        State = #{current_uid => 1},
        Info = {ack_cancel, 1, <<"did_1">>, <<"msg_ack_1">>, 1700000000123},
        {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assertEqual(State, State2),
        ?assertEqual(1, meck:num_calls(websocket_logic, handle_ack_cancel, 3))
    end).

websocket_info_ack_cancel_from_remote_failure_is_tolerated_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {websocket_logic, [
            {'handle_ack_cancel', 3, fun(_, _, _) -> erlang:error(simulated_ack_cancel_failure) end}
        ]}
    ], fun() ->
        State = #{current_uid => 1},
        Info = {ack_cancel, 1, <<"did_1">>, <<"msg_ack_2">>, 1700000000456},
        {ok, State2, hibernate} = websocket_handler:websocket_info(Info, State),
        ?assertEqual(State, State2)
    end).

terminate_with_uid_calls_offline_test_() ->
    ?WITH_MECKS(log_mocks() ++ [
        {user_logic, [
            {'offline', 3, fun(123, _Pid, <<"did_1">>) -> ok end}
        ]}
    ], fun() ->
        ok = websocket_handler:terminate(normal, req, #{current_uid => 123, did => <<"did_1">>})
    end).
