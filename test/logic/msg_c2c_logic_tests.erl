-module(msg_c2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_logic 模块的 EUnit 测试
%%%
%%% 目标：验证单聊消息业务逻辑功能
%%% 覆盖：消息发送、ACK、撤回、编辑、边界条件
%%%===================================================================

%% ===================================================================
%% c2c/3 测试
%% ===================================================================

c2c_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> true end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello"/utf8>>}},
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

c2c_with_non_friend_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> false end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"not_friend">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello"/utf8>>}},
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2c_with_offline_user_stores_message_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> false end}
        ]},
        {msg_store_ds, [
            {'store', 1, fun(_Msg) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello"/utf8>>}},
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2c_client_ack/3 测试
%% ===================================================================

c2c_client_ack_with_valid_ack_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123 end}
        ]},
        {msg_c2c_ds, [
            {'client_ack', 3, fun(_MsgId, _Uid, _Ack) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Ack = 1,

        Result = msg_c2c_logic:c2c_client_ack(MsgId, FromUid, Ack),
        ?assertEqual(ok, Result)
    end).

c2c_client_ack_with_zero_ack_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123 end}
        ]},
        {msg_c2c_ds, [
            {'client_ack', 3, fun(_MsgId, _Uid, _Ack) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Ack = 0,

        Result = msg_c2c_logic:c2c_client_ack(MsgId, FromUid, Ack),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2c_revoke/3 测试
%% ===================================================================

c2c_revoke_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {msg_c2c_ds, [
            {'revoke', 3, fun(_MsgId, _Uid, _Data) -> {ok, updated} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"old_msg_id">> => <<"old_msg_456">>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2c_revoke_with_non_friend_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> false end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"not_friend">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"old_msg_id">> => <<"old_msg_456">>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"not_friend">>}}, Result)
    end).

c2c_revoke_with_invalid_old_msg_id_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {msg_c2c_ds, [
            {'revoke', 3, fun(_MsgId, _Uid, _Data) -> {error, not_found} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"msg_not_found">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"old_msg_id">> => <<"non_existent_msg">>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"msg_not_found">>}}, Result)
    end).

%% ===================================================================
%% c2c_revoke_ack/3 测试
%% ===================================================================

c2c_revoke_ack_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123 end}
        ]},
        {msg_c2c_ds, [
            {'revoke_ack', 3, fun(_MsgId, _Uid, _Data) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"msg_id">> => <<"msg_456">>},

        Result = msg_c2c_logic:c2c_revoke_ack(MsgId, FromUid, Data),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% c2c_edit/3 测试
%% ===================================================================

c2c_edit_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {msg_c2c_ds, [
            {'edit', 3, fun(_MsgId, _Uid, _Data) -> {ok, updated} end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"content">> => <<"updated content"/utf8>>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2c_edit_with_non_friend_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> false end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"not_friend">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"content">> => <<"updated content"/utf8>>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"not_friend">>}}, Result)
    end).

c2c_edit_with_invalid_msg_id_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {msg_c2c_ds, [
            {'edit', 3, fun(_MsgId, _Uid, _Data) -> {error, not_found} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"msg_not_found">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"content">> => <<"updated content"/utf8>>},
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"msg_not_found">>}}, Result)
    end).

%% ===================================================================
%% c2c_edit_ack/3 测试
%% ===================================================================

c2c_edit_ack_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123 end}
        ]},
        {msg_c2c_ds, [
            {'edit_ack', 3, fun(_MsgId, _Uid, _Data) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"msg_id">> => <<"msg_456">>},

        Result = msg_c2c_logic:c2c_edit_ack(MsgId, FromUid, Data),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

c2c_with_empty_msg_id_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<>>,
        FromUid = 123,
        Data = #{<<"payload">> => <<>>},
        To = <<"to_user">>,
        OriginalMsg = <<>>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        % 空消息应该被处理
        ?assertEqual(ok, Result)
    end).

c2c_with_empty_payload_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'is_friend', 2, fun(_FromUid, _ToUid) -> true end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> true end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => <<>>},
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

c2c_with_self_message_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 123 end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> true end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => #{<<"content">> => <<"self message"/utf8>>}},
        To = <<"to_user">>,  % 编码后的自己 ID
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        % 发给自己的消息应该被处理
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 黑名单测试
%% ===================================================================

c2c_with_user_in_denylist_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 1} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"in_denylist">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{<<"payload">> => #{<<"content">> => <<"hello"/utf8>>}},
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"in_denylist">>}}, Result)
    end).

%% ===================================================================
%% 消息 staging 失败测试
%% ===================================================================

c2c_with_stage_failure_returns_error_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _PayloadJson,
                               _FromUid, _ToUid, _CreatedAtRfc, _NowTs) -> error end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"internal_error">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"payload">> => #{<<"content">> => <<"hello"/utf8>>},
            <<"created_at">> => 1737513600000
        },
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertMatch({reply, #{<<"code">> := <<"internal_error">>}}, Result)
    end).

%% ===================================================================
%% 撤回消息权限测试
%% ===================================================================

c2c_revoke_with_permission_denied_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(_) -> 789 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123}}  % 消息属于用户123，当前用户是789
            end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 789,  % 不是消息发送者
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"old_msg_456">>}
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"permission_denied">>}}, Result)
    end).

%% ===================================================================
%% 编辑消息权限测试
%% ===================================================================

c2c_edit_with_permission_denied_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(_) -> 789 end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_ToUid) -> true end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 789,  % 与 from_id 不匹配
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"old_msg_456">>,
                <<"content">> => <<"edited content"/utf8>>,
                <<"msg_type">> => <<"text">>
            }
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"permission_denied">>}}, Result)
    end).

%% ===================================================================
%% 撤回消息 - 离线用户测试
%% ===================================================================

c2c_revoke_with_offline_user_stores_message_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123}}
            end},
            {'revoke_offline_msg', 8, fun(_PayloadJson, _NowTs, _MsgId, _FromId, _ToId, _MsgType, _Action) -> ok end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_ToUid) -> false end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"old_msg_456">>}
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).

%% ===================================================================
%% 编辑消息 - 离线用户测试
%% ===================================================================

c2c_edit_with_offline_user_stores_message_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_ToUid) -> false end}
        ]},
        {msg_c2c_ds, [
            {'edit_offline_msg', 5, fun(_PayloadJson, _NowTs, _MsgId, _FromId, _ToId) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"old_msg_456">>,
                <<"content">> => <<"edited content"/utf8>>,
                <<"msg_type">> => <<"text">>
            }
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).

%% ===================================================================
%% 编辑消息 - 离线存储失败测试
%% ===================================================================

c2c_edit_with_offline_storage_error_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_ToUid) -> false end}
        ]},
        {msg_c2c_ds, [
            {'edit_offline_msg', 5, fun(_PayloadJson, _NowTs, _MsgId, _FromId, _ToId) ->
                {error, database_error}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"old_msg_456">>,
                <<"content">> => <<"edited content"/utf8>>,
                <<"msg_type">> => <<"text">>
            }
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_edit(MsgId, FromUid, Data, To),
        % 即使离线存储失败，也应该返回成功（已响应）
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).

%% ===================================================================
%% 消息类型和字段测试 (v2.0 格式)
%% ===================================================================

c2c_with_msg_type_and_action_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> true end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _PayloadJson,
                               _FromUid, _ToUid, _CreatedAtRfc, _NowTs) -> ok end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"payload">> => #{<<"content">> => <<"hello"/utf8>>},
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<"chat">>,
            <<"e2ee">> => <<>>,
            <<"created_at">> => 1737513600000
        },
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

c2c_with_e2ee_metadata_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {user_device_ds, [
            {'is_online', 2, fun(_ToUid, _ExcludeDIDs) -> true end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_Type, _MsgId, _MsgType, _Action, _E2EE, _PayloadJson,
                               _FromUid, _ToUid, _CreatedAtRfc, _NowTs) -> ok end}
        ]},
        {message_ds, [
            {'send_next', 5, fun(_ToUid, _MsgId, _Msg, _MsLi, _ExcludeDIDs, _IsFromSelf) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"payload">> => <<"encrypted_content_base64">>,
            <<"msg_type">> => <<"e2ee">>,
            <<"action">> => <<>>,
            <<"e2ee">> => <<"e2ee_metadata">>,
            <<"created_at">> => 1737513600000
        },
        To = <<"to_user">>,
        OriginalMsg = <<"{}">>,

        Result = msg_c2c_logic:c2c(MsgId, FromUid, Data, To, OriginalMsg),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 撤回消息 - 消息不属于任何用户测试
%% ===================================================================

c2c_revoke_with_msg_owner_mismatch_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 999}}  % 消息属于999，不是当前用户123
            end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"old_msg_456">>}
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        ?assertMatch({reply, #{<<"code">> := <<"permission_denied">>}}, Result)
    end).

%% ===================================================================
%% 撤回消息 - 离线存储失败测试
%% ===================================================================

c2c_revoke_with_offline_storage_error_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(_MsgId) ->
                {ok, #{<<"from_id">> => 123}}
            end},
            {'revoke_offline_msg', 8, fun(_PayloadJson, _NowTs, _MsgId, _FromId, _ToId, _MsgType, _Action) ->
                {error, database_error}
            end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_ToUid) -> false end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        FromUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"old_msg_456">>}
        },
        To = <<"to_user">>,

        Result = msg_c2c_logic:c2c_revoke(MsgId, FromUid, Data, To),
        % 即使离线存储失败，也应该返回成功
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).
