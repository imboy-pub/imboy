-module(msg_c2s_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2s_ds 模块的 EUnit 测试
%%%
%%% 目标：验证客户端到服务器消息领域服务功能
%%% 覆盖：消息组装、发送逻辑
%%%===================================================================

%% ===================================================================
%% 基础测试验证
%% ===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        %% 验证 msg_c2s_ds 模块可以正常加载
        code:ensure_loaded(msg_c2s_ds),
        ?assertMatch({file, _}, code:is_loaded(msg_c2s_ds))
    end).

%% ===================================================================
%% 消息写入测试
%% ===================================================================

write_msg_test_() ->
    ?TEST_WITH_DB(fun() ->
        MsgId = <<"msg_123">>,
        Data = #{<<"from">> => 1, <<"to">> => 2, <<"content">> => <<"Hello">>},
        Result = msg_c2s_ds:write_msg(MsgId, Data),
        ?assertEqual(ok, Result)
    end).

write_topic_test_() ->
    ?TEST_WITH_DB(fun() ->
        NowTs = imboy_dt:now(millisecond),
        MsgId = <<"msg_topic_123">>,
        FromId = 1,
        ToId = 2,
        Payload = #{<<"type">> => <<"text">>, <<"content">> => <<"Topic message">>},
        PayloadMd5 = imboy_hasher:hash(maps:get(<<"content">>, Payload)),
        Result = msg_c2s_ds:write_topic(NowTs, MsgId, FromId, ToId, Payload, PayloadMd5),
        ?assertEqual(ok, Result)
    end).
