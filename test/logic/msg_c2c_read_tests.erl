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
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]},
        {msg_read_repo, [
            {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_Uid) -> true end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 456,  % 接收者
        Data = #{
            <<"to">> => <<"from_user">>,  % 发送者
            <<"from">> => <<"to_user">>,    % 接收者（自己）
            <<"payload">> => #{<<"read_at">> => 1737513600000}
        },

        Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).

c2c_read_with_non_friend_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {false, 0} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"not_a_friend">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 456,
        Data = #{
            <<"to">> => <<"from_user">>,
            <<"from">> => <<"to_user">>
        },

        Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"code">> := <<"not_a_friend">>}}, Result)
    end).

c2c_read_with_offline_sender_stores_notification_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_read_repo, [
            {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_Uid) -> false end}
        ]},
        {msg_c2c_ds, [
            {'read_offline_msg', 5, fun(_MsgId, _FromId, _ToId, _ReadAt, _Action) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 456,
        Data = #{
            <<"to">> => <<"from_user">>,
            <<"from">> => <<"to_user">>,
            <<"payload">> => #{<<"read_at">> => 1737513600000}
        },

        Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result)
    end).

%% ===================================================================
%% c2c_read_ack/3 测试
%% ===================================================================

c2c_read_ack_with_valid_data_succeeds_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,  % 发送者
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"read_at">> => 1737513600000}
        },

        Result = msg_c2c_logic:c2c_read_ack(MsgId, CurrentUid, Data),
        ?assertEqual(ok, Result)
    end).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

c2c_read_with_empty_msg_id_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
        ]},
        {msg_read_repo, [
            {'save_read', 5, fun(_MsgId, _FromUid, _ToUid, _ToDid, _ReadAt) -> ok end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(_Uid) -> true end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<>>,
        CurrentUid = 456,
        Data = #{
            <<"to">> => <<"from_user">>,
            <<"from">> => <<"to_user">>
        },

        % 空消息ID应该返回错误
        Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"S2C">>}}, Result)
    end).

c2c_read_with_self_message_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(_) -> 123 end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(_MsgId, _Code, _Msg) ->
                #{<<"type">> => <<"S2C">>, <<"code">> => <<"invalid_operation">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 123,
        Data = #{
            <<"to">> => <<"self_user">>,
            <<"from">> => <<"self_user">>
        },

        % 发给自己的消息不应该有已读回执
        Result = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"code">> := <<"invalid_operation">>}}, Result)
    end).

%% ===================================================================
%% 幂等性测试
%% ===================================================================

c2c_read_with_duplicate_read_is_idempotent_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 123; (<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(_ToId, _FromUid) -> {true, 0} end}
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
        {message_ds, [
            {'send_next', 4, fun(_ToUid, _MsgId, _Msg, _MsLi) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_123">>,
        CurrentUid = 456,
        Data = #{
            <<"to">> => <<"from_user">>,
            <<"from">> => <<"to_user">>
        },

        % 重复发送已读回执应该成功
        Result1 = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        Result2 = msg_c2c_logic:c2c_read(MsgId, CurrentUid, Data),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result1),
        ?assertMatch({reply, #{<<"type">> := <<"C2C">>}}, Result2)
    end).
