-module(msg_c2g_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2g_logic 模块的 EUnit 测试
%%%
%%% 目标：验证群聊消息业务逻辑功能
%%% 覆盖：发送群聊消息、消息路由、边界条件
%%%===================================================================

%% ===================================================================
%% send/1 测试
%% ===================================================================

send_message_success_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_123">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"from_id">> => 100, <<"group_id">> => 1, <<"body">> => <<"消息"/utf8>>},
        Result = msg_c2g_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_message_with_payload_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_456">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"from_id">> => 100,
            <<"group_id">> => 1,
            <<"body">> => #{<<"content">> => <<"你好"/utf8>>, <<"type">> => <<"text">>}
        },
        Result = msg_c2g_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_message_with_empty_body_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_789">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"from_id">> => 100, <<"group_id">> => 1, <<"body">> => <<>>},
        Result = msg_c2g_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

send_message_with_large_group_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_large">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        Data = #{<<"from_id">> => 100, <<"group_id">> => 99999, <<"body">> => <<"大群消息"/utf8>>},
        Result = msg_c2g_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

%% ===================================================================
%% c2g/5 测试
%% ===================================================================

c2g_sends_to_group_members_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [100, 200, 300] end}
        ]},
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_c2g">>} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 100,
        Data = #{<<"payload">> => #{<<"content">> => <<"群消息"/utf8>>}, <<"to">> => <<"group_1">>},
        OriginalMsg = <<"{}">>,

        Result = msg_c2g_logic:c2g(MsgId, FromUid, Data, <<"group_1">>, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

c2g_with_offline_members_stores_message_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [100, 200, 300] end}
        ]},
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_offline">>} end}
        ]},
        {msg_store_ds, [
            {'store', 1, fun(_Msg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_456">>,
        FromUid = 100,
        Data = #{<<"payload">> => #{<<"content">> => <<"离线消息"/utf8>>}, <<"to">> => <<"group_2">>},
        OriginalMsg = <<"{}">>,

        Result = msg_c2g_logic:c2g(MsgId, FromUid, Data, <<"group_2">>, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2g_revoke/4 测试
%% ===================================================================

c2g_revoke_success_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'revoke', 3, fun(_MsgId, _Uid, _Data) -> {ok, updated} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_revoke">>,
        FromUid = 100,
        Data = #{<<"old_msg_id">> => <<"old_msg_123">>, <<"to">> => <<"group_1">>},
        To = <<"group_1">>,

        Result = msg_c2g_logic:c2g_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2g_revoke_with_nonexistent_msg_fails_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'revoke', 3, fun(_MsgId, _Uid, _Data) -> {error, not_found} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"msg_not_found">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_revoke_fail">>,
        FromUid = 100,
        Data = #{<<"old_msg_id">> => <<"non_existent">>, <<"to">> => <<"group_1">>},
        To = <<"group_1">>,

        Result = msg_c2g_logic:c2g_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"msg_not_found">>}}, Result)
    end).

%% ===================================================================
%% c2g_edit/4 测试
%% ===================================================================

c2g_edit_success_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'edit', 3, fun(_MsgId, _Uid, _Data) -> {ok, updated} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_edit">>,
        FromUid = 100,
        Data = #{<<"content">> => <<"编辑后的内容"/utf8>>, <<"to">> => <<"group_1">>},
        To = <<"group_1">>,

        Result = msg_c2g_logic:c2g_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2g_edit_with_invalid_msg_id_fails_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'edit', 3, fun(_MsgId, _Uid, _Data) -> {error, not_found} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"msg_not_found">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_edit_fail">>,
        FromUid = 100,
        Data = #{<<"content">> => <<"编辑内容"/utf8>>, <<"to">> => <<"group_1">>},
        To = <<"group_1">>,

        Result = msg_c2g_logic:c2g_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"msg_not_found">>}}, Result)
    end).

%% ===================================================================
%% c2g_client_ack/3 测试
%% ===================================================================

c2g_client_ack_success_test_() ->
    ?WITH_MECK(msg_c2g_timeline_repo, [
        {'client_ack', 1, fun(_Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_123">>,
        Uid = 100,
        Ack = 1,

        Result = msg_c2g_logic:c2g_client_ack(MsgId, Uid, Ack),
        ?assertEqual(ok, Result)
    end).

c2g_client_ack_with_zero_ack_test_() ->
    ?WITH_MECK(msg_c2g_timeline_repo, [
        {'client_ack', 1, fun(_Data) -> ok end}
    ], fun() ->
        MsgId = <<"msg_456">>,
        Uid = 100,
        Ack = 0,

        Result = msg_c2g_logic:c2g_client_ack(MsgId, Uid, Ack),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

send_message_with_missing_fields_test_() ->
    ?WITH_MECKS([
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_partial">>} end}
        ]},
        {message_ds, [
            {'send_next', 2, fun(_Uid, _Msg) -> ok end}
        ]}
    ], fun() ->
        % 缺少 body 字段
        Data = #{<<"from_id">> => 100, <<"group_id">> => 1},
        Result = msg_c2g_logic:send(Data),
        ?assertMatch({ok, _}, Result)
    end).

c2g_with_empty_msg_id_test_() ->
    ?WITH_MECKS([
        {group_ds, [
            {'member_uids', 1, fun(_Gid) -> [100] end}
        ]},
        {msg_c2g_ds, [
            {'save', 1, fun(_Data) -> {ok, <<"msg_id_empty">>} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_Uid, _MsgId, _Msg, _MsLi, _ExcludeDIDs) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<>>,
        FromUid = 100,
        Data = #{<<"payload">> => <<>>},
        To = <<"group_1">>,
        OriginalMsg = <<>>,

        Result = msg_c2g_logic:c2g(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).
