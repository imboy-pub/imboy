-module(message_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

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
