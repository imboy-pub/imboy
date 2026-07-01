-module(msg_s2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% msg_s2c_ds 模块的 EUnit 测试
%%%
%%% 目标：验证系统消息业务逻辑功能
%%% 覆盖：发送系统消息、消息路由、边界条件
%%%===================================================================

%% ===================================================================
%% msg_s2c_ds:send/7 测试
%% ===================================================================

send_system_message_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"msg_id_s2c">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_id_s2c">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
                {'write_msg', 9, fun(
                    _CreatedAt, _Id, _Payload, _From, _To, _ServerTS, _Action, _MsgType, _E2EE
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            FromId = 0,
            ToUids = [100],
            Action = <<"notification">>,
            MsgType = <<>>,
            E2EE = null,
            Payload = #{<<"body">> => <<"系统消息"/utf8>>},
            Save = save,

            Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
            ?assertEqual(ok, Result)
        end
    ).

send_with_action_test_() ->
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"msg_id_action">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_id_action">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
                {'write_msg', 9, fun(
                    _CreatedAt, _Id, _Payload, _From, _To, _ServerTS, _Action, _MsgType, _E2EE
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            FromId = 0,
            ToUids = [100],
            Action = <<"notification">>,
            MsgType = <<>>,
            E2EE = null,
            Payload = #{
                <<"body">> => <<"操作成功"/utf8>>,
                <<"action">> => <<"notification">>
            },
            Save = save,

            Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
            ?assertEqual(ok, Result)
        end
    ).

send_with_multiple_recipients_test_() ->
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"msg_id_multi">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_id_multi">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
                {'write_msg', 9, fun(
                    _CreatedAt, _Id, _Payload, _From, _To, _ServerTS, _Action, _MsgType, _E2EE
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            FromId = 0,
            ToUids = [100, 200, 300],
            Action = <<"broadcast">>,
            MsgType = <<>>,
            E2EE = null,
            Payload = #{<<"body">> => <<"广播消息"/utf8>>},
            Save = save,

            Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
            ?assertEqual(ok, Result)
        end
    ).

send_with_empty_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"msg_id_empty">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_id_empty">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end}
            ]},
            {msg_s2c_repo, [
                {'count_by_to_id', 1, fun(_ToUid) -> 0 end},
                {'write_msg', 9, fun(
                    _CreatedAt, _Id, _Payload, _From, _To, _ServerTS, _Action, _MsgType, _E2EE
                ) ->
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            FromId = 0,
            ToUids = [100],
            Action = <<"notification">>,
            MsgType = <<>>,
            E2EE = null,
            Payload = #{<<"body">> => <<>>},
            Save = save,

            Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% msg_s2c_logic:s2c/4 测试
%% ===================================================================

s2c_sends_to_user_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_Level, _Msg, _Mod, _Line) -> ok end}
            ]},
            {msg_operation_ds, [
                {'delete_c2c_msg', 2, fun(_OldMsgId, _Uid) -> ok end}
            ]},
            {msg_store_ds, [
                {'stage', 10, fun(
                    _T,
                    _MsgId,
                    _MsgType,
                    _Action,
                    _E2EE,
                    _Payload,
                    _From,
                    _To,
                    _CreatedAt,
                    _ServerTs
                ) ->
                    ok
                end},
                {'enqueue', 3, fun(_T, _MsgId, _Data) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_s2c_1">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {elib_retry_config, [
                {'intervals', 1, fun(_Type) -> [0, 1000, 2000] end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2024-01-01T00:00:00Z">> end},
                {'to_rfc3339', 1, fun(_Ts) -> <<"2024-01-01T00:00:00Z">> end}
            ]}
        ],
        fun() ->
            Action = <<"C2C_DEL_EVERYONE">>,
            MsgId = <<"msg_123">>,
            CurrentUid = 1,
            Data = #{
                <<"payload">> => #{<<"old_msg_id">> => <<"old_msg_1">>},
                <<"to">> => <<"2">>
            },

            Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data),
            ?assertMatch({reply, _}, Result)
        end
    ).

s2c_with_pull_offline_msg_action_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(1, 1) -> true end}
            ]},
            {msg_operation_ds, [
                {'delete_c2g_timeline', 2, fun(_Uid, _MsgId) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_pull">>}
                end}
            ]}
        ],
        fun() ->
            Action = <<"C2G_DEL_FOR_ME">>,
            MsgId = <<"msg_456">>,
            CurrentUid = 1,
            Data = #{
                <<"payload">> => #{<<"old_msg_id">> => <<"old_msg_2">>},
                <<"to">> => <<"1">>
            },

            Result = msg_s2c_logic:s2c(Action, MsgId, CurrentUid, Data),
            ?assertMatch({reply, _}, Result)
        end
    ).

%% ===================================================================
%% 边界条件测试
%% ===================================================================

send_with_empty_recipients_test_() ->
    ?TEST_SIMPLE(fun() ->
        FromId = 0,
        ToUids = [],
        Action = <<"notification">>,
        MsgType = <<>>,
        E2EE = null,
        Payload = #{<<"body">> => <<"测试消息"/utf8>>},
        Save = save,

        % 空接收者列表应该直接返回 ok
        Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
        ?assertEqual(ok, Result)
    end).

send_direct_without_save_test_() ->
    ?WITH_MECKS(
        [
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"msg_direct">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"type">> => <<"S2C">>, <<"id">> => <<"msg_direct">>}
                end}
            ]},
            {imboy_syn, [
                {'publish', 2, fun(_ToUid, _EncodedMsg) -> ok end}
            ]}
        ],
        fun() ->
            FromId = 0,
            ToUids = [100],
            Action = <<"notification">>,
            MsgType = <<>>,
            E2EE = null,
            Payload = #{<<"body">> => <<"直发消息"/utf8>>},
            Save = no_save,

            Result = msg_s2c_ds:send(FromId, ToUids, Action, MsgType, E2EE, Payload, Save),
            ?assertEqual(ok, Result)
        end
    ).

%% ===================================================================
%% s2c/4 C2G_DEL_FOR_ME / C2G_DEL_EVERYONE 权限安全回归测试
%% 修复前：非群成员也能触发群消息"删除"广播，且 C2G_DEL_EVERYONE
%%         误调用了单聊删除函数（群消息实际未被删除）。
%% ===================================================================

c2g_del_for_me_rejects_non_member_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(456, 101) -> false end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Action, _To) ->
                    #{<<"action">> => Action}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => 101,
                <<"payload">> => #{<<"old_msg_id">> => <<"old_1">>}
            },
            Result = msg_s2c_logic:s2c(<<"C2G_DEL_FOR_ME">>, <<"msg_1">>, 456, Data),
            ?assertMatch({reply, #{<<"action">> := <<"permission_denied">>}}, Result)
        end
    ).

c2g_del_for_me_allows_member_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(456, 101) -> true end}
            ]},
            {msg_operation_ds, [
                {'delete_c2g_timeline', 2, fun(456, <<"old_1">>) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"action">> => <<"C2G_DEL_FOR_ME">>}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => 101,
                <<"payload">> => #{<<"old_msg_id">> => <<"old_1">>}
            },
            Result = msg_s2c_logic:s2c(<<"C2G_DEL_FOR_ME">>, <<"msg_1">>, 456, Data),
            ?assertMatch({reply, #{<<"action">> := <<"C2G_DEL_FOR_ME">>}}, Result)
        end
    ).

c2g_del_everyone_rejects_non_member_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(456, 101) -> false end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Action, _To) ->
                    #{<<"action">> => Action}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => 101,
                <<"payload">> => #{<<"old_msg_id">> => <<"old_2">>}
            },
            Result = msg_s2c_logic:s2c(<<"C2G_DEL_EVERYONE">>, <<"msg_2">>, 456, Data),
            ?assertMatch({reply, #{<<"action">> := <<"permission_denied">>}}, Result)
        end
    ).

c2g_del_everyone_rejects_non_owner_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(456, 101) -> true end}
            ]},
            {msg_c2g_ds, [
                {'find_msg_by_id', 1, fun(<<"old_3">>) ->
                    {ok, #{<<"from_id">> => 999}}
                end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(_MsgId, Action, _To) ->
                    #{<<"action">> => Action}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => 101,
                <<"payload">> => #{<<"old_msg_id">> => <<"old_3">>}
            },
            %% 456 是群成员，但消息真正的发送者是 999，不允许 456 代为删除
            Result = msg_s2c_logic:s2c(<<"C2G_DEL_EVERYONE">>, <<"msg_3">>, 456, Data),
            ?assertMatch({reply, #{<<"action">> := <<"permission_denied">>}}, Result)
        end
    ).

c2g_del_everyone_allows_owner_and_uses_group_delete_test_() ->
    ?WITH_MECKS(
        [
            {group_ds, [
                {'is_member', 2, fun(456, 101) -> true end},
                {'member_uids', 1, fun(101) -> [456, 789] end}
            ]},
            {msg_c2g_ds, [
                {'find_msg_by_id', 1, fun(<<"old_4">>) ->
                    {ok, #{<<"from_id">> => 456}}
                end}
            ]},
            {msg_operation_ds, [
                {'delete_c2g_msg', 3, fun(<<"C2G_DEL_EVERYONE">>, 456, <<"old_4">>) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-01-01T00:00:00Z">> end},
                {'to_rfc3339', 1, fun(_Ts) -> <<"2026-01-01T00:00:00Z">> end}
            ]},
            {elib_id, [
                {'gen', 1, fun(_Prefix) -> <<"s2c_id">> end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(
                    _Type, _From, _To, _Payload, _MsgId, _MsgType, _Action, _E2EE
                ) ->
                    #{<<"action">> => <<"C2G_DEL_EVERYONE">>}
                end},
                {'send_next', 4, fun(_Uid, _MsgId, _Msg, _MsLi) -> ok end}
            ]},
            {msg_store_ds, [
                {'stage', 10, fun(_A, _B, _C, _D, _E, _F, _G, _H, _I, _J) -> ok end},
                {'enqueue', 3, fun(_A, _B, _C) -> ok end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => 101,
                <<"payload">> => #{<<"old_msg_id">> => <<"old_4">>}
            },
            %% 456 既是群成员，也是原消息发送者，允许删除且应调用群消息删除函数
            Result = msg_s2c_logic:s2c(<<"C2G_DEL_EVERYONE">>, <<"msg_4">>, 456, Data),
            ?assertMatch({reply, #{<<"action">> := <<"C2G_DEL_EVERYONE">>}}, Result),
            ?assert(
                meck:called(msg_operation_ds, delete_c2g_msg, [
                    <<"C2G_DEL_EVERYONE">>, 456, <<"old_4">>
                ])
            ),
            ?assertNot(meck:called(msg_operation_ds, delete_c2c_msg, '_'))
        end
    ).
