-module(msg_s2c_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_s2c_ds 模块的 EUnit 测试
%%%
%%% 目标：验证服务器到客户端消息领域服务功能
%%% 覆盖：系统消息推送、通知消息
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 验证 msg_s2c_ds 模块可以正常加载
        code:ensure_loaded(msg_s2c_ds),
        ?assertMatch({file, _}, code:is_loaded(msg_s2c_ds))
    end).

%% ===================================================================
%% 消息写入测试
%% ===================================================================

write_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        NowTs = imboy_dt:now(millisecond),
        MsgId = <<"msg_s2c_123">>,
        FromId = 0,  % 系统消息
        ToId = 1,
        Payload = #{<<"type">> => <<"notification">>, <<"content">> => <<"System notification">>},
        DeliveredAt = NowTs,
        Result = msg_s2c_ds:write_msg(NowTs, MsgId, FromId, ToId, Payload, DeliveredAt),
        ?assertEqual(ok, Result)
    end).

read_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        Uid = 1,
        Did = <<"device_123">>,
        Result = msg_s2c_ds:read_msg(Uid, Did),
        case Result of
            {ok, Msgs} when is_list(Msgs) -> ?assert(true);
            {ok, _} -> ?assert(true);
            _ -> ?assert(false, "Expected {ok, Messages}")
        end
    end).

delete_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_to_delete_123">>,
        Result = msg_s2c_ds:delete_msg(MsgId),
        ?assertEqual(ok, Result)
    end).

send_test_() ->
    ?WITH_MECK(message_ds, [
        {'send_next', 4, fun(_ToId, _MsgId, _Message, _MsLi) -> ok end}
    ], fun() ->
        FromId = 0,  % 系统消息发送者
        MsgType = #{<<"type">> => <<"notification">>, <<"content">> => <<"Test">>},
        ToUids = [1],  % 接收用户ID列表
        Result = msg_s2c_ds:send(FromId, MsgType, ToUids, no_save),
        ?assertEqual(ok, Result)
    end).
