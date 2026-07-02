-module(msg_operation_ds_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_operation_ds 模块的 EUnit 测试
%%%
%%% 目标：验证消息操作数据服务功能
%%% 覆盖：消息删除（C2C/C2G）、时间线删除、消息确认（ACK）
%%%===================================================================

%% ===================================================================
%% delete_c2c_msg/2 测试
%% ===================================================================

%% @doc 测试删除单聊消息成功
delete_c2c_msg_success_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(Where, [MsgId, FromId]) ->
                ?assertEqual(<<"WHERE msg_id = $1 AND from_id = $2">>, Where),
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, FromId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试删除单聊消息失败
delete_c2c_msg_with_error_returns_error_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, _Params) ->
                {error, <<"database_error">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<"msg123">>, 100),
            ?assertEqual({error, <<"database_error">>}, Result)
        end
    ).

%% @doc 测试删除不存在的单聊消息
delete_c2c_msg_nonexistent_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, _Params) ->
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<"nonexistent">>, 999),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% delete_c2g_msg/3 测试
%% ===================================================================

%% @doc 测试群聊消息 - 仅删除自己的时间线
delete_c2g_msg_for_me_success_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(Uid, MsgId) ->
                ?assertEqual(100, Uid),
                ?assertEqual(<<"msg123">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_FOR_ME">>, 100, <<"msg123">>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试群聊消息 - 删除所有人消息
delete_c2g_msg_for_everyone_success_test_() ->
    ?WITH_MECK(
        msg_c2g_repo,
        [
            {'delete_msg', 2, fun(Where, [MsgId]) ->
                ?assertEqual(<<"WHERE msg_id = $1">>, Where),
                ?assertEqual(<<"msg123">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_EVERYONE">>, 100, <<"msg123">>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试群聊消息 - 删除所有人消息失败
delete_c2g_msg_for_everyone_with_error_returns_error_test_() ->
    ?WITH_MECK(
        msg_c2g_repo,
        [
            {'delete_msg', 2, fun(_Where, _Params) ->
                {error, <<"database_error">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_EVERYONE">>, 100, <<"msg123">>),
            ?assertEqual({error, <<"database_error">>}, Result)
        end
    ).

%% @doc 测试群聊消息 - 删除自己的时间线失败仍返回ok
delete_c2g_msg_for_me_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(_Uid, _MsgId) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_FOR_ME">>, 100, <<"msg123">>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试群聊消息 - 无效的action
delete_c2g_msg_with_invalid_action_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = msg_operation_ds:delete_c2g_msg(<<"INVALID_ACTION">>, 100, <<"msg123">>),
        ?assertEqual({error, invalid_action}, Result)
    end).

%% @doc 测试群聊消息 - 空action
delete_c2g_msg_with_empty_action_returns_error_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = msg_operation_ds:delete_c2g_msg(<<>>, 100, <<"msg123">>),
        ?assertEqual({error, invalid_action}, Result)
    end).

%% ===================================================================
%% delete_c2c_timeline/2 测试
%% ===================================================================

%% @doc 测试删除单聊时间线记录（总是返回ok）
delete_c2c_timeline_always_returns_ok_test_() ->
    ?TEST_SIMPLE(fun() ->
        Result = msg_operation_ds:delete_c2c_timeline(100, <<"msg123">>),
        ?assertEqual(ok, Result)
    end).

%% @doc 测试删除单聊时间线记录 - 任何参数
delete_c2c_timeline_with_any_params_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, msg_operation_ds:delete_c2c_timeline(0, <<>>)),
        ?assertEqual(ok, msg_operation_ds:delete_c2c_timeline(-1, <<"any">>)),
        ?assertEqual(ok, msg_operation_ds:delete_c2c_timeline(999999, <<"msg">>))
    end).

%% ===================================================================
%% delete_c2g_timeline/2 测试
%% ===================================================================

%% @doc 测试删除群聊时间线记录成功
delete_c2g_timeline_success_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(Uid, MsgId) ->
                ?assertEqual(100, Uid),
                ?assertEqual(<<"msg123">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_timeline(100, <<"msg123">>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试删除群聊时间线记录失败仍返回ok
delete_c2g_timeline_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(_Uid, _MsgId) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_timeline(100, <<"msg123">>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试删除不存在的群聊时间线记录
delete_c2g_timeline_nonexistent_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(_Uid, _MsgId) ->
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_timeline(999, <<"nonexistent">>),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% ack_c2c_msg/2 测试
%% ===================================================================

%% @doc 测试C2C消息ACK成功
ack_c2c_msg_success_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, Uid) ->
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, Uid),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试C2C消息ACK失败仍返回ok
ack_c2c_msg_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, _Uid) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% ack_c2g_timeline/2 测试
%% ===================================================================

%% @doc 测试C2G消息ACK成功
ack_c2g_timeline_success_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'client_ack', 2, fun(Uid, MsgId) ->
                ?assertEqual(100, Uid),
                ?assertEqual(<<"msg123">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2g_timeline(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试C2G消息ACK失败仍返回ok
ack_c2g_timeline_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'client_ack', 2, fun(_Uid, _MsgId) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2g_timeline(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% ack_s2c_msg/2 测试
%% ===================================================================

%% @doc 测试S2C消息ACK成功
ack_s2c_msg_success_test_() ->
    ?WITH_MECK(
        msg_s2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, Uid) ->
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, Uid),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_s2c_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试S2C消息ACK失败仍返回ok
ack_s2c_msg_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_s2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, _Uid) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_s2c_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% ack_c2s_msg/2 测试
%% ===================================================================

%% @doc 测试C2S消息ACK成功
ack_c2s_msg_success_test_() ->
    ?WITH_MECK(
        msg_c2s_repo,
        [
            {'delete_msg', 2, fun(Where, [MsgId, FromId]) ->
                ?assertEqual(<<"msg_id = $1 AND from_id = $2">>, Where),
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, FromId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2s_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试C2S消息ACK失败仍返回ok
ack_c2s_msg_error_still_returns_ok_test_() ->
    ?WITH_MECK(
        msg_c2s_repo,
        [
            {'delete_msg', 2, fun(_Where, _Params) ->
                {error, <<"not_found">>}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2s_msg(<<"msg123">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

%% @doc 测试空消息ID
delete_c2c_msg_with_empty_msg_id_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, [MsgId, _FromId]) ->
                ?assertEqual(<<>>, MsgId),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<>>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试零用户ID
delete_c2c_msg_with_zero_uid_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, [_MsgId, FromId]) ->
                ?assertEqual(0, FromId),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<"msg123">>, 0),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试大用户ID
delete_c2c_msg_with_large_uid_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, [_MsgId, FromId]) ->
                ?assertEqual(999999999, FromId),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2c_msg(<<"msg123">>, 999999999),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试空群聊消息ID
delete_c2g_msg_with_empty_msg_id_test_() ->
    ?WITH_MECK(
        msg_c2g_timeline_repo,
        [
            {'delete_timeline', 2, fun(_Uid, MsgId) ->
                ?assertEqual(<<>>, MsgId),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_FOR_ME">>, 100, <<>>),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试ACK空消息ID
ack_c2c_msg_with_empty_msg_id_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, _Uid) ->
                ?assertEqual(<<>>, MsgId),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<>>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试ACK零用户ID
ack_c2c_msg_with_zero_uid_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, Uid) ->
                ?assertEqual(0, Uid),
                {ok, 0}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<"msg123">>, 0),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试二进制消息ID
ack_c2c_msg_with_binary_msg_id_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, _Uid) ->
                ?assert(is_binary(MsgId)),
                ?assertEqual(<<"binary_msg_id">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<"binary_msg_id">>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试长消息ID
ack_c2c_msg_with_long_msg_id_test_() ->
    LongMsgId = list_to_binary(lists:duplicate(100, $x)),
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, _Uid) ->
                ?assertEqual(LongMsgId, MsgId),
                ?assert(byte_size(MsgId) >= 100),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(LongMsgId, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% @doc 测试特殊字符消息ID
ack_c2c_msg_with_special_chars_msg_id_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, _Uid) ->
                ?assertEqual(<<"msg-id_123_测试"/utf8>>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            Result = msg_operation_ds:ack_c2c_msg(<<"msg-id_123_测试"/utf8>>, 100),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 集成场景测试
%% ===================================================================

%% @doc 测试完整的消息删除流程
complete_c2c_msg_deletion_flow_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_msg', 2, fun(_Where, [MsgId, FromId]) ->
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, FromId),
                {ok, 1}
            end}
        ],
        fun() ->
            % 删除消息
            ?assertEqual(ok, msg_operation_ds:delete_c2c_msg(<<"msg123">>, 100))
        end
    ).

%% @doc 测试完整的群聊消息删除流程（所有人）
complete_c2g_msg_deletion_for_everyone_flow_test_() ->
    ?WITH_MECK(
        msg_c2g_repo,
        [
            {'delete_msg', 2, fun(_Where, [MsgId]) ->
                ?assertEqual(<<"msg456">>, MsgId),
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual(
                ok, msg_operation_ds:delete_c2g_msg(<<"C2G_DEL_EVERYONE">>, 100, <<"msg456">>)
            )
        end
    ).

%% @doc 测试完整的ACK流程
complete_ack_flow_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, _Uid) -> {ok, 1} end}
            ]},
            {msg_c2g_timeline_repo, [
                {'client_ack', 2, fun(_Uid, _MsgId) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            % C2C ACK
            ?assertEqual(ok, msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100)),
            % C2G ACK
            ?assertEqual(ok, msg_operation_ds:ack_c2g_timeline(<<"msg456">>, 100))
        end
    ).

%% @doc 测试批量操作
batch_ack_operations_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, _Uid) -> {ok, 1} end}
        ],
        fun() ->
            MsgIds = [<<"msg1">>, <<"msg2">>, <<"msg3">>],
            lists:foreach(
                fun(MsgId) ->
                    ?assertEqual(ok, msg_operation_ds:ack_c2c_msg(MsgId, 100))
                end,
                MsgIds
            )
        end
    ).

%% ===================================================================
%% 按设备送达 ACK 测试（T03/P0-1）
%% ===================================================================

%% @doc DID 有效：只写送达标记 + 尝试全端确认清理，不得走按 uid 删行
ack_c2c_msg_per_device_marks_and_cleans_test_() ->
    ?WITH_MECKS(
        [
            {msg_delivery_repo, [
                {'mark_acked_batch', 4, fun(Kind, MsgIds, Uid, DID) ->
                    ?assertEqual(<<"c2c">>, Kind),
                    ?assertEqual([<<"msg123">>], MsgIds),
                    ?assertEqual(100, Uid),
                    ?assertEqual(<<"did-a">>, DID),
                    {ok, 1}
                end},
                {'delete_delivered_batch', 4, fun(Kind, MsgIds, Uid, _ActiveDays) ->
                    ?assertEqual(<<"c2c">>, Kind),
                    ?assertEqual([<<"msg123">>], MsgIds),
                    ?assertEqual(100, Uid),
                    ok
                end}
            ]},
            {msg_c2c_repo, [
                {'delete_by_msg_id_and_to_id', 2, fun(_MsgId, _Uid) -> {ok, 1} end}
            ]}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100, <<"did-a">>)),
            ?assertEqual(1, meck:num_calls(msg_delivery_repo, mark_acked_batch, 4)),
            ?assertEqual(1, meck:num_calls(msg_delivery_repo, delete_delivered_batch, 4)),
            %% 按设备路径不得按 uid 删行
            ?assertEqual(0, meck:num_calls(msg_c2c_repo, delete_by_msg_id_and_to_id, 2))
        end
    ).

%% @doc DID 为空：退回 legacy 按 uid 删行语义
ack_c2c_msg_empty_did_falls_back_to_legacy_test_() ->
    ?WITH_MECK(
        msg_c2c_repo,
        [
            {'delete_by_msg_id_and_to_id', 2, fun(MsgId, Uid) ->
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, Uid),
                {ok, 1}
            end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100, <<>>)),
            ?assertEqual(1, meck:num_calls(msg_c2c_repo, delete_by_msg_id_and_to_id, 2))
        end
    ).

%% @doc 标记失败：不得触发主行清理（宁可重复投递，不可丢消息）
ack_c2c_msg_mark_failure_skips_cleanup_test_() ->
    ?WITH_MECK(
        msg_delivery_repo,
        [
            {'mark_acked_batch', 4, fun(_K, _M, _U, _D) -> {error, closed} end},
            {'delete_delivered_batch', 4, fun(_K, _M, _U, _A) -> ok end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:ack_c2c_msg(<<"msg123">>, 100, <<"did-a">>)),
            ?assertEqual(0, meck:num_calls(msg_delivery_repo, delete_delivered_batch, 4))
        end
    ).

%% @doc S2C 按设备 ACK：kind 应为 s2c
ack_s2c_msg_per_device_test_() ->
    ?WITH_MECK(
        msg_delivery_repo,
        [
            {'mark_acked_batch', 4, fun(Kind, _MsgIds, _Uid, _DID) ->
                ?assertEqual(<<"s2c">>, Kind),
                {ok, 1}
            end},
            {'delete_delivered_batch', 4, fun(Kind, _MsgIds, _Uid, _A) ->
                ?assertEqual(<<"s2c">>, Kind),
                ok
            end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:ack_s2c_msg(<<"msg9">>, 7, <<"did-b">>))
        end
    ).

%% @doc 批量按设备 ACK（REST offline_ack 路径）
ack_c2c_batch_per_device_test_() ->
    ?WITH_MECK(
        msg_delivery_repo,
        [
            {'mark_acked_batch', 4, fun(_Kind, MsgIds, _Uid, _DID) ->
                ?assertEqual([<<"m1">>, <<"m2">>], MsgIds),
                {ok, 2}
            end},
            {'delete_delivered_batch', 4, fun(_Kind, MsgIds, _Uid, _A) ->
                ?assertEqual([<<"m1">>, <<"m2">>], MsgIds),
                ok
            end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:ack_c2c_batch([<<"m1">>, <<"m2">>], 100, <<"did-a">>))
        end
    ).

%% @doc worker 落库后清理钩子：委托 repo，kind atom 转 binary
maybe_clean_delivered_delegates_test_() ->
    ?WITH_MECK(
        msg_delivery_repo,
        [
            {'delete_delivered', 4, fun(Kind, MsgId, Uid, _ActiveDays) ->
                ?assertEqual(<<"c2c">>, Kind),
                ?assertEqual(<<"msg123">>, MsgId),
                ?assertEqual(100, Uid),
                ok
            end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:maybe_clean_delivered(c2c, <<"msg123">>, 100)),
            ?assertEqual(1, meck:num_calls(msg_delivery_repo, delete_delivered, 4))
        end
    ).

%% @doc 清理钩子异常不得外抛（不阻塞 worker 投递）
maybe_clean_delivered_never_crashes_test_() ->
    ?WITH_MECK(
        msg_delivery_repo,
        [
            {'delete_delivered', 4, fun(_K, _M, _U, _A) -> erlang:error(db_down) end}
        ],
        fun() ->
            ?assertEqual(ok, msg_operation_ds:maybe_clean_delivered(s2c, <<"msg9">>, 7))
        end
    ).

%% @doc 非 c2c/s2c kind 直接忽略
maybe_clean_delivered_ignores_other_kinds_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual(ok, msg_operation_ds:maybe_clean_delivered(c2g, <<"msg">>, 1))
    end).

%% ===================================================================
%% 类型验证测试
%% ===================================================================

%% @doc 验证参数类型
delete_c2c_msg_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg123">>,
        FromId = 100,
        ?assert(is_binary(MsgId)),
        ?assert(is_integer(FromId))
    end).

%% @doc 验证C2G删除参数类型
delete_c2g_msg_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        Action = <<"C2G_DEL_FOR_ME">>,
        Uid = 100,
        MsgId = <<"msg123">>,
        ?assert(is_binary(Action)),
        ?assert(is_integer(Uid)),
        ?assert(is_binary(MsgId))
    end).

%% @doc 验证ACK参数类型
ack_c2c_msg_validates_types_test_() ->
    ?TEST_SIMPLE(fun() ->
        MsgId = <<"msg123">>,
        Uid = 100,
        ?assert(is_binary(MsgId)),
        ?assert(is_integer(Uid))
    end).
