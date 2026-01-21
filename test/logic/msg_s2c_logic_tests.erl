-module(msg_s2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_s2c_logic 模块的 EUnit 测试
%%%
%%% 目标：验证系统消息业务逻辑功能
%%% 覆盖：发送系统消息、消息路由、边界条件
%%%===================================================================

%% ===================================================================
%% send/1 测试
%% ===================================================================

send_system_message_success_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_s2c">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"to_id">> => 100, <<"body">> => <<"系统消息"/utf8>>},
        Result = msg_s2c_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_with_action_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_action">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"to_id">> => 100,
            <<"body">> => <<"操作成功"/utf8>>,
            <<"action">> => <<"notification">>
        },
        Result = msg_s2c_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_with_multiple_recipients_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_multi">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"to_id">> => [100, 200, 300], <<"body">> => <<"广播消息"/utf8>>},
        Result = msg_s2c_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_with_empty_body_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_empty">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"to_id">> => 100, <<"body">> => <<>>},
        Result = msg_s2c_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% s2c/4 测试
%% ===================================================================

s2c_sends_to_user_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_s2c_1">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Action = <<"notification">>,
        MsgId = <<"msg_123">>,
        CurrentUid = 1,
        Data = #{<<"body">> => <<"通知内容"/utf8>>},
        OriginalMsg = <<"{}">>,

        Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

s2c_with_pull_offline_msg_action_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_pull">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Action = <<"pull_offline_msg">>,
        MsgId = <<"msg_456">>,
        CurrentUid = 1,
        Data = #{<<"count">> => 10},
        OriginalMsg = <<"{}">>,

        Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

s2c_with_custom_payload_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_custom">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Action = <<"custom_action">>,
        MsgId = <<"msg_789">>,
        CurrentUid = 1,
        Data = #{
            <<"body">> => <<"自定义消息"/utf8>>,
            <<"extra">> => #{<<"key">> => <<"value">>}
        },
        OriginalMsg = <<"{}">>,

        Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

send_with_zero_to_id_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_zero">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"to_id">> => 0, <<"body">> => <<"测试消息"/utf8>>},
        Result = msg_s2c_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

s2c_with_empty_action_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_empty_action">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Action = <<>>,
        MsgId = <<"msg_empty">>,
        CurrentUid = 1,
        Data = #{<<"body">> => <<"空操作"/utf8>>},
        OriginalMsg = <<"{}">>,

        Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

s2c_with_large_data_test_() ->
    ?WITH_MECKS([
        {msg_s2c_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_large">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Action = <<"big_data">>,
        MsgId = <<"msg_large">>,
        CurrentUid = 1,
        LargeBody = binary:copy(<<"数据"/utf8>>, 1000),
        Data = #{<<"body">> => LargeBody},
        OriginalMsg = <<"{}">>,

        Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data, OriginalMsg),
        ?assertEqual(ok, Result)
    end).
