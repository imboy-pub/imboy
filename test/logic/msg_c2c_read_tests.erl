-module(msg_c2c_read_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_c2c_logic 已读回执功能的 EUnit 测试
%%%
%%% 目标：验证单聊消息已读回执业务逻辑功能
%%% 覆盖：已读回执发送、权限验证、在线/离线处理
%%%===================================================================

%% ===================================================================
%% c2c_read/3 测试
%% ===================================================================

c2c_read_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, false} end}
            ]},
            {msg_read_repo, [
                {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(_Uid) -> true end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Ts) -> <<"2025-01-22T00:00:00Z">> end},
                {'millisecond', 0, fun() -> 1737513600000 end}
            ]},
            {message_ds, [
                {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            % 接收者
            CurrentUid = 456,
            Data = #{
                % 发送者
                <<"to">> => <<"789">>,
                % 接收者（自己）
                <<"from">> => <<"456">>,
                <<"payload">> => #{<<"read_at">> => 1737513600000}
            },

            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
        end
    ).

c2c_read_with_non_friend_fails_test_() ->
    %% 非好友且不在黑名单：check_relationship 返回 boolean {false, false}
    %% （真实回归——此前 mock 用 {false, 0} integer 掩盖了 false > 0 的 atom>number quirk）
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {false, false} end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Code, _Msg) ->
                    #{<<"type">> => <<"S2C">>, <<"code">> => Code}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = #{
                <<"to">> => <<"789">>,
                <<"from">> => <<"456">>
            },

            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"code">> := <<"not_a_friend">>}}, Result)
        end
    ).

c2c_read_with_denylisted_user_returns_in_denylist_test_() ->
    %% 在黑名单：check_relationship 返回 {false, true}，应命中 in_denylist 分支
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {false, true} end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Code, _Msg) ->
                    #{<<"type">> => <<"S2C">>, <<"code">> => Code}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = #{
                <<"to">> => <<"789">>,
                <<"from">> => <<"456">>
            },

            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"code">> := <<"in_denylist">>}}, Result)
        end
    ).

c2c_read_with_offline_sender_stores_notification_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, false} end}
            ]},
            {msg_read_repo, [
                {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(_Uid) -> false end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Ts) -> <<"2025-01-22T00:00:00Z">> end},
                {'millisecond', 0, fun() -> 1737513600000 end}
            ]},
            {msg_c2c_ds, [
                {'read_offline_msg', 5, fun(_MsgId, _FromId, _ToId, _ReadAt, _Action) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = #{
                <<"to">> => <<"789">>,
                <<"from">> => <<"456">>,
                <<"payload">> => #{<<"read_at">> => 1737513600000}
            },

            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
        end
    ).

%% ===================================================================
%% c2c_read_ack/3 测试
%% ===================================================================

c2c_read_ack_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            % 发送者
            CurrentUid = 123,
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"789">>,
                <<"payload">> => #{<<"read_at">> => 1737513600000}
            },

            Result = msg_c2c_logic:c2c_read_ack(MsgId, CurrentUid, Data),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

c2c_read_with_empty_msg_id_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, false} end}
            ]},
            {msg_read_repo, [
                {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(_Uid) -> true end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Ts) -> <<"2025-01-22T00:00:00Z">> end},
                {'millisecond', 0, fun() -> 1737513600000 end}
            ]},
            {message_ds, [
                {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<>>,
            CurrentUid = 456,
            Data = #{
                <<"to">> => <<"789">>,
                <<"from">> => <<"456">>
            },

            % 空消息ID - 当前实现仍正常处理（不做空ID校验）
            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
        end
    ).

c2c_read_with_self_message_test_() ->
    ?WITH_MECKS(
        [
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                    #{<<"type">> => <<"S2C">>, <<"code">> => <<"invalid_operation">>}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 123,
            Data = #{
                <<"to">> => <<"123">>,
                <<"from">> => <<"123">>
            },

            % 发给自己的消息不应该有已读回执
            Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"code">> := <<"invalid_operation">>}}, Result)
        end
    ).

%% ===================================================================
%% 幂等性测试
%% ===================================================================

c2c_read_with_duplicate_read_is_idempotent_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, false} end}
            ]},
            {msg_read_repo, [
                {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) ->
                    % 模拟唯一约束冲突，但返回 ok（幂等）
                    ok
                end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(_Uid) -> true end}
            ]},
            {elib_dt, [
                {'to_rfc3339', 1, fun(_Ts) -> <<"2025-01-22T00:00:00Z">> end},
                {'millisecond', 0, fun() -> 1737513600000 end}
            ]},
            {message_ds, [
                {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_123">>,
            CurrentUid = 456,
            Data = #{
                <<"to">> => <<"789">>,
                <<"from">> => <<"456">>
            },

            % 重复发送已读回执应该成功
            Result1 = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            Result2 = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
            ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result1),
            ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result2)
        end
    ).
