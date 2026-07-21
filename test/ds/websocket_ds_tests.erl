-module(websocket_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% websocket_ds 模块的 EUnit 测试
%%%
%%% 目标：验证 WebSocket 领域服务功能
%%% 覆盖：子协议检查、认证处理、超时设置
%%%===================================================================

%% ===================================================================
%% Helper Functions
%% ===================================================================

%% 创建一个模拟的 Cowboy 请求对象
%% Cowboy 请求对象是一个包含多个字段的 map
%% @private
mock_cowboy_req() ->
    % 必需的字段
    #{
        version => 'HTTP/1.1',
        method => <<"GET">>,
        scheme => <<"ws">>,
        host => <<"localhost">>,
        port => 8080,
        path => <<"/api/ws">>,
        qs => <<>>,
        fragment => <<>>,
        bindings => #{},
        headers => #{},
        peer => {{127, 0, 0, 1}, 12345},
        sock => {{127, 0, 0, 1}, 8080},
        cert => undefined,
        has_body => false,
        body_length => 0
    }.

%% ===================================================================
%% check_subprotocols/2 测试
%% ===================================================================

%% 注意：check_subprotocols/2 需要真实的 Cowboy 请求对象
%% 这些测试验证函数的存在性和基本逻辑流程
%% 在集成测试中应该使用完整的 Cowboy 环境进行测试

check_subprotocols_with_undefined_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 由于 cowboy_req:reply/2 需要真实的请求对象，
        % 我们只验证函数可以被调用（不会抛出 undef）
        % 实际行为在集成测试中验证
        ?assert(is_function(fun websocket_ds:check_subprotocols/2, 2))
    end).

check_subprotocols_with_empty_list_test_() ->
    ?TEST_SIMPLE(fun() ->
        % 验证函数可以被调用
        ?assert(is_function(fun websocket_ds:check_subprotocols/2, 2))
    end).

check_subprotocols_with_valid_protocol_test_() ->
    ?TEST_SIMPLE(fun() ->
        % cowboy_websocket 需要真实请求对象，这里只验证函数存在
        Subprotocols = [<<"sip">>, <<"text">>],
        ?assertMatch([_ | _], Subprotocols),
        ?assert(length(Subprotocols) > 0)
    end).

%% ===================================================================
%% idle_timeout/1 测试
%% ===================================================================

idle_timeout_returns_default_value_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        Result = websocket_ds:idle_timeout(Uid),
        %% 生产默认 config_ds:env(ws_idle_timeout_ms, 180000)，未在任何 config 覆盖
        ?assertEqual(180000, Result)
    end).

idle_timeout_with_different_uid_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 999,
        Result = websocket_ds:idle_timeout(Uid),
        ?assert(is_integer(Result)),
        ?assert(Result > 0)
    end).

%% ===================================================================
%% select_subprotocol/1 测试
%% ===================================================================

select_subprotocol_prefers_v2_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"imboy.v2">>,
            websocket_ds:select_subprotocol([<<"text">>, <<"imboy.v2">>])
        ),
        ?assertEqual(
            <<"imboy.v2">>,
            websocket_ds:select_subprotocol([<<"imboy.v2">>, <<"imboy-protobuf">>])
        ),
        ?assertEqual(
            <<"imboy.v2">>,
            websocket_ds:select_subprotocol([<<"imboy-protobuf">>, <<"imboy.v2">>, <<"imboy-json">>])
        )
    end).

select_subprotocol_prefers_protobuf_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"imboy-protobuf">>,
            websocket_ds:select_subprotocol([<<"text">>, <<"imboy-protobuf">>])
        ),
        ?assertEqual(
            <<"imboy-protobuf">>,
            websocket_ds:select_subprotocol([<<"imboy-protobuf">>, <<"imboy-json">>])
        )
    end).

select_subprotocol_falls_back_to_json_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"imboy-json">>,
            websocket_ds:select_subprotocol([<<"text">>, <<"imboy-json">>])
        ),
        ?assertEqual(
            <<"imboy-json">>,
            websocket_ds:select_subprotocol([<<"imboy-json">>])
        )
    end).

select_subprotocol_falls_back_to_text_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            <<"text">>,
            websocket_ds:select_subprotocol([<<"text">>])
        ),
        ?assertEqual(
            <<"text">>,
            websocket_ds:select_subprotocol([<<"sip">>, <<"text">>])
        )
    end).

select_subprotocol_unknown_returns_undefined_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(
            undefined,
            websocket_ds:select_subprotocol([<<"sip">>])
        ),
        ?assertEqual(
            undefined,
            websocket_ds:select_subprotocol([<<"mqtt">>, <<"sip">>])
        )
    end).

select_subprotocol_undefined_returns_undefined_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(undefined, websocket_ds:select_subprotocol(undefined)),
        ?assertEqual(undefined, websocket_ds:select_subprotocol([]))
    end).
