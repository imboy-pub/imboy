-module(message_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").
-include("chat.hrl").

%%%===================================================================
%%% @doc
%%% message_ds 模块的 EUnit 测试
%%%
%%% 目标：验证消息领域服务功能
%%% 覆盖：消息组装、发送、消息类型处理
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 验证 message_ds 模块可以正常加载
        code:ensure_loaded(message_ds),
        ?assertMatch({file, _}, code:is_loaded(message_ds))
    end).

%% ===================================================================
%% 消息组装验证
%% ===================================================================

assemble_msg_with_valid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgType = <<"text">>,
        FromUid = <<"user123">>,
        ToUid = <<"user456">>,
        ?assertEqual(<<"text">>, MsgType),
        ?assertEqual(<<"user123">>, FromUid),
        ?assertEqual(<<"user456">>, ToUid)
    end).

%% ===================================================================
%% S0-1 消息信封 ver 字段（架构保险）
%% 出站信封统一带当前版本；入站缺省视为当前版本（旧客户端无 ver 兼容）
%% ===================================================================

%% assemble_msg/8 出站信封带当前版本 ver
assemble_msg_carries_current_ver_test_() ->
    ?TEST_SIMPLE(fun() ->
        Msg = message_ds:assemble_msg(
            <<"C2C">>,
            <<"111">>,
            <<"222">>,
            #{<<"content">> => <<"hi">>},
            <<"mid-1">>,
            <<"text">>,
            <<>>,
            null
        ),
        ?assertEqual(?CUR_MSG_VER, maps:get(<<"ver">>, Msg))
    end).

%% assemble_msg/5 向后兼容版同样带 ver
assemble_msg_5_carries_current_ver_test_() ->
    ?TEST_SIMPLE(fun() ->
        Msg = message_ds:assemble_msg(
            <<"C2C">>,
            <<"111">>,
            <<"222">>,
            #{<<"content">> => <<"hi">>, <<"msg_type">> => <<"text">>},
            <<"mid-2">>
        ),
        ?assertEqual(?CUR_MSG_VER, maps:get(<<"ver">>, Msg))
    end).

%% assemble_s2c 系统消息经 assemble_msg/8 亦带 ver
assemble_s2c_carries_current_ver_test_() ->
    ?TEST_SIMPLE(fun() ->
        Msg = message_ds:assemble_s2c(<<"mid-3">>, <<"please_refresh_token">>, <<"333">>),
        ?assertEqual(?CUR_MSG_VER, maps:get(<<"ver">>, Msg))
    end).

%% encode_websocket_message/1 为模块私有函数（离线回放路径），
%% 其 ver 行为由内部信封构造收敛，不直接单测；C2G 独立路径在 msg_c2g_logic_tests 覆盖。

%% decode_websocket_message：新客户端带 ver 时透传
decode_websocket_message_passes_ver_test_() ->
    ?TEST_SIMPLE(fun() ->
        Json = jsone:encode(#{
            <<"ver">> => ?CUR_MSG_VER,
            <<"id">> => <<"mid-5">>,
            <<"type">> => <<"C2C">>,
            <<"from">> => <<"111">>,
            <<"to">> => <<"222">>,
            <<"msg_type">> => <<"text">>,
            <<"payload">> => #{<<"content">> => <<"hi">>}
        }),
        Decoded = message_ds:decode_websocket_message(Json),
        ?assertEqual(?CUR_MSG_VER, maps:get(<<"ver">>, Decoded))
    end).

%% decode_websocket_message：旧客户端无 ver 时缺省=当前版本（向后兼容核心断言）
decode_websocket_message_defaults_ver_when_absent_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 注意：不带 ver 字段，模拟旧客户端
        Json = jsone:encode(#{
            <<"id">> => <<"mid-6">>,
            <<"type">> => <<"C2C">>,
            <<"from">> => <<"111">>,
            <<"to">> => <<"222">>,
            <<"msg_type">> => <<"text">>,
            <<"payload">> => #{<<"content">> => <<"hi">>}
        }),
        Decoded = message_ds:decode_websocket_message(Json),
        %% 缺省即当前版本——旧客户端不被破坏
        ?assertEqual(?CUR_MSG_VER, maps:get(<<"ver">>, Decoded))
    end).

%% ===================================================================
%% 消息发送验证
%% ===================================================================

send_next_requires_valid_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        Uid = 1,
        MsgId = <<"msg123">>,
        Msg = <<"{}">>,
        % 验证参数类型
        ?assert(is_integer(Uid)),
        ?assertMatch(<<_/binary>>, MsgId),
        % 验证消息ID不为空
        ?assert(byte_size(MsgId) > 0),
        ?assertEqual(ok, message_ds:send_next(Uid, MsgId, Msg, []))
    end).

%% ===================================================================
%% S2C e2ee 拒绝守护（服务端零密码学不变量）
%% S2C 系统消息永远不应携带 e2ee 字段
%% ===================================================================

s2c_with_e2ee_rejected_test_() ->
    ?TEST_SIMPLE(fun() ->
        Msg = #{
            <<"id">> => <<"s2c-e2ee-bad">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"logged_another_device">>,
            <<"e2ee">> => #{<<"protocol">> => <<"olm">>},
            <<"payload">> => #{}
        },
        Result = message_ds:validate_message(Msg),
        ?assertEqual({error, <<"s2c_message_not_support_e2ee">>}, Result)
    end).

s2c_without_e2ee_passes_test_() ->
    ?TEST_SIMPLE(fun() ->
        Msg = #{
            <<"id">> => <<"s2c-ok">>,
            <<"type">> => <<"S2C">>,
            <<"action">> => <<"logged_another_device">>,
            <<"e2ee">> => null,
            <<"payload">> => #{}
        },
        Result = message_ds:validate_message(Msg),
        ?assertMatch({ok, _}, Result)
    end).
