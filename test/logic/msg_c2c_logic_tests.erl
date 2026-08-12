-module(msg_c2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

c2c_success_sends_server_ack_and_dispatch_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {elib_async, [
                {'async_retry', 3, fun(Fun, 3, 1000) ->
                    Fun(),
                    ok
                end},
                %% 投递侧已拆出重放边界，走 async/1
                {'async', 1, fun(Fun) ->
                    Fun(),
                    self()
                end}
            ]},
            {push_notification_logic, [
                {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_ok_001">>,
            CurrentUid = 123,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            ok = msg_c2c_logic:c2c(MsgId, CurrentUid, Data),

            Reply =
                receive
                    {reply, Msg} -> Msg
                after 1000 ->
                    timeout
                end,

            ?assertNotEqual(timeout, Reply),
            ?assertEqual(MsgId, maps:get(<<"id">>, Reply)),
            ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).

c2c_not_friend_returns_reply_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {false, 0} end}
            ]},
            {ai_agent_ds, [
                %% ToId=456 非 agent：好友校验原样生效
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, <<"not_a_friend">>, <<"456">>) ->
                    #{<<"id">> => MsgId, <<"error">> => <<"not_a_friend">>}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_not_friend_001">>,
            Data = #{<<"to">> => <<"456">>},

            Result = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertMatch({reply, _}, Result),
            {reply, Msg} = Result,
            ?assertEqual(<<"not_a_friend">>, maps:get(<<"error">>, Msg))
        end
    ).

c2c_in_denylist_returns_reply_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, 2} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, <<"in_denylist">>, <<"456">>) ->
                    #{<<"id">> => MsgId, <<"error">> => <<"in_denylist">>}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_deny_001">>,
            Data = #{<<"to">> => <<"456">>},

            Result = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertMatch({reply, _}, Result),
            {reply, Msg} = Result,
            ?assertEqual(<<"in_denylist">>, maps:get(<<"error">>, Msg))
        end
    ).

%% ===================================================================
%% AI agent 免好友校验（T1.4 配套）：agent 是公开服务账号，任何用户
%% 可直接对话；但用户主动拉黑 agent 后仍拒发（黑名单优先）。
%% ===================================================================

c2c_to_enabled_agent_skips_friend_check_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                %% 非好友 + 未拉黑（check_relationship 真实协议为 {boolean(), boolean()}）：
                %% 无豁免时会被 not_a_friend 拒绝
                {'check_relationship', 2, fun(456, 123) -> {false, false} end}
            ]},
            {ai_agent_ds, [
                %% ToId=456 是启用中 agent → 应豁免好友校验
                {'is_agent', 1, fun(456) -> {true, #{}} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {elib_async, [
                %% agent 回复旁路 fire-and-forget：异步不执行，避免真实 LLM 调用
                {'async', 1, fun(_) -> ok end}
            ]},
            {agent_rate_limiter, [
                {'allow', 2, fun(_, _) -> allow end}
            ]},
            {push_notification_logic, [
                {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_agent_ok_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            ok = msg_c2c_logic:c2c(MsgId, 123, Data),

            Reply =
                receive
                    {reply, Msg} -> Msg
                after 1000 ->
                    timeout
                end,
            ?assertNotEqual(timeout, Reply),
            ?assertEqual(MsgId, maps:get(<<"id">>, Reply)),
            ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, Reply)),
            %% 消息真正走 stage（未被 not_a_friend 拦截）
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11))
        end
    ).

c2c_to_agent_still_blocked_when_denylisted_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                %% 用户拉黑了 agent（InDenylist=2）：黑名单优先，仍拒发
                {'check_relationship', 2, fun(456, 123) -> {false, 2} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> {true, #{}} end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, <<"in_denylist">>, <<"456">>) ->
                    #{<<"id">> => MsgId, <<"error">> => <<"in_denylist">>}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_agent_deny_001">>,
            Data = #{<<"to">> => <<"456">>},

            Result = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertMatch({reply, _}, Result),
            {reply, Msg} = Result,
            ?assertEqual(<<"in_denylist">>, maps:get(<<"error">>, Msg))
        end
    ).

c2c_to_non_agent_keeps_friend_check_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {false, 0} end}
            ]},
            {ai_agent_ds, [
                %% 普通用户不是 agent：好友校验原样生效
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, <<"not_a_friend">>, <<"456">>) ->
                    #{<<"id">> => MsgId, <<"error">> => <<"not_a_friend">>}
                end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_nonagent_001">>,
            Data = #{<<"to">> => <<"456">>},

            Result = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertMatch({reply, _}, Result),
            {reply, Msg} = Result,
            ?assertEqual(<<"not_a_friend">>, maps:get(<<"error">>, Msg))
        end
    ).

c2c_reply_to_missing_message_emits_msg_not_found_reply_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_c2c_repo, [
                {'find_msg_by_id', 1, fun(<<"missing_reply_msg">>) -> {error, not_found} end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, <<"msg_not_found">>, <<"456">>) ->
                    #{<<"id">> => MsgId, <<"type">> => <<"MSG_NOT_FOUND">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_reply_missing_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"reply">>},
                <<"created_at">> => 1708768700000,
                <<"reply_to">> => #{
                    <<"msg_id">> => <<"missing_reply_msg">>,
                    <<"from_id">> => <<"456">>
                }
            },

            %% 实现为同步 {reply, ...}（经消息路由回给发送方），非异步 self() 投递
            {reply, Reply} = msg_c2c_logic:c2c(MsgId, 123, Data),

            ?assertEqual(<<"MSG_NOT_FOUND">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11))
        end
    ).

c2c_client_ack_delegates_to_ack_logic_test_() ->
    ?WITH_MECKS(
        [
            {msg_ack_logic, [
                {'client_ack', 4, fun(<<"c2c">>, <<"msg_ack_001">>, 123, <<"did_1">>) -> ok end}
            ]}
        ],
        fun() ->
            ok = msg_c2c_logic:c2c_client_ack(<<"msg_ack_001">>, 123, <<"did_1">>),
            ?assertEqual(1, meck:num_calls(msg_ack_logic, client_ack, 4))
        end
    ).

extract_reply_info_without_reply_to_returns_empty_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({<<>>, 0, <<>>}, msg_c2c_logic:extract_reply_info(#{}))
    end).

extract_reply_info_with_json_payload_extracts_content_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'find_msg_by_id', 1, fun(<<"origin_msg_001">>) ->
                    {ok, #{<<"payload">> => <<"{\"content\":\"hello reply\"}">>}}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"reply_to">> => #{
                    <<"msg_id">> => <<"origin_msg_001">>,
                    <<"from_id">> => <<"456">>
                }
            },

            {ReplyToMsgId, ReplyToFromId, ReplySnippet} = msg_c2c_logic:extract_reply_info(Data),
            ?assertEqual(<<"origin_msg_001">>, ReplyToMsgId),
            ?assertEqual(456, ReplyToFromId),
            ?assertEqual(<<"hello reply">>, ReplySnippet)
        end
    ).

extract_reply_info_with_non_json_payload_falls_back_to_raw_snippet_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_repo, [
                {'find_msg_by_id', 1, fun(<<"origin_msg_002">>) ->
                    {ok, #{<<"payload">> => <<"plain payload text">>}}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"reply_to">> => #{
                    <<"msg_id">> => <<"origin_msg_002">>,
                    <<"from_id">> => <<"456">>
                }
            },

            {ReplyToMsgId, ReplyToFromId, ReplySnippet} = msg_c2c_logic:extract_reply_info(Data),
            ?assertEqual(<<"origin_msg_002">>, ReplyToMsgId),
            ?assertEqual(456, ReplyToFromId),
            ?assertEqual(<<"plain payload text">>, ReplySnippet)
        end
    ).

c2c_plaintext_blocked_when_encryption_required_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_plaintext_blocked_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            {reply, Reply} = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(
                <<"encrypted_message_required">>,
                maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
            ),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

c2c_e2ee_message_allowed_when_encryption_required_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {elib_async, [
                {'async_retry', 3, fun(Fun, 3, 1000) ->
                    Fun(),
                    ok
                end},
                %% 投递侧已拆出重放边界，走 async/1
                {'async', 1, fun(Fun) ->
                    Fun(),
                    self()
                end}
            ]},
            {push_notification_logic, [
                {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_e2ee_allowed_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => <<"nonce.ciphertext">>,
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"e2ee">>,
                <<"action">> => <<>>,
                <<"e2ee">> => #{<<"e2ee">> => true}
            },

            ok = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

c2c_edit_plaintext_blocked_when_encryption_required_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end},
                {'edit_offline_msg', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {elib_dt, [
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end},
                {'millisecond', 0, fun() -> 1700000060000 end},
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_edit_plaintext_blocked_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_edit_001">>,
                    <<"content">> => <<"new content">>,
                    <<"msg_type">> => <<"text">>
                },
                <<"e2ee">> => null
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(MsgId, 123, Data),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(
                <<"encrypted_message_required">>,
                maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
            ),
            ?assertEqual(0, meck:num_calls(msg_c2c_ds, edit_offline_msg, 5))
        end
    ).

c2c_edit_e2ee_payload_is_opaque_and_relayed_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end},
                {'millisecond', 0, fun() -> 1700000065000 end},
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_e2ee_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(456) -> true end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(456, <<"c2c_edit_e2ee_001">>, _Msg, <<"c2s">>) -> ok end}
            ]}
        ],
        fun() ->
            OpaquePayload = <<"v3-ciphertext-edit-body">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => OpaquePayload,
                <<"msg_type">> => <<"text">>,
                <<"e2ee">> => #{
                    <<"meta_version">> => 3,
                    <<"edit_of">> => <<"orig_c2c_edit_e2ee_001">>,
                    <<"relay_action">> => <<"message_edit">>
                }
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_e2ee_001">>, 123, Data),
            ?assertEqual(<<"message_edit">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(OpaquePayload, maps:get(<<"payload">>, Reply)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).

c2c_revoke_success_online_sends_revoke_ack_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_revoke_001">>) ->
                    {ok, #{
                        <<"from_id">> => 123,
                        <<"created_at">> => 1700000000000
                    }}
                end},
                %% 第 4 参必须是被撤回的原消息 ID（传错则 function_clause 直接挂测试）
                {'revoke_offline_msg', 9, fun(_, _, _, <<"orig_c2c_revoke_001">>, _, _, _, _, _) ->
                    ok
                end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000060000 end},
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end},
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(456) -> true end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(456, <<"c2c_revoke_001">>, _Msg, <<"c2s">>) -> ok end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2c_revoke_001">>}
            },

            {reply, Reply} = msg_c2c_logic:c2c_revoke(<<"c2c_revoke_001">>, 123, Data),
            ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(<<"custom">>, maps:get(<<"msg_type">>, Reply)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            ?assertEqual(0, meck:num_calls(msg_c2c_ds, revoke_offline_msg, 9))
        end
    ).

c2c_revoke_permission_denied_when_operator_not_sender_test_() ->
    ?WITH_MECKS(
        [
            {message_ds, [
                {'assemble_s2c', 3, fun(
                    <<"c2c_revoke_denied_001">>, <<"permission_denied">>, <<"456">>
                ) ->
                    #{
                        <<"id">> => <<"c2c_revoke_denied_001">>,
                        <<"error">> => <<"permission_denied">>
                    }
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"789">>,
                <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2c_revoke_002">>}
            },

            {reply, Reply} = msg_c2c_logic:c2c_revoke(<<"c2c_revoke_denied_001">>, 123, Data),
            ?assertEqual(<<"permission_denied">>, maps:get(<<"error">>, Reply))
        end
    ).

c2c_edit_success_online_sends_edit_ack_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end},
                {'millisecond', 0, fun() -> 1700000065000 end},
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(456) -> true end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(456, <<"c2c_edit_001">>, _Msg, <<"c2s">>) -> ok end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_edit_001">>,
                    <<"content">> => <<"edited content">>,
                    <<"msg_type">> => <<"text">>
                }
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_001">>, 123, Data),
            ReplyPayload = maps:get(<<"payload">>, Reply),
            ?assertEqual(<<"message_edit_ack">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Reply)),
            ?assertEqual(<<"edited content">>, maps:get(<<"content">>, ReplyPayload)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).

c2c_edit_e2ee_offline_is_idempotent_and_keeps_sender_did_test_() ->
    ?WITH_MECKS(
        [
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end},
                {'millisecond', 0, fun() -> 1700000065000 end},
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end},
                {'to_rfc3339', 1, fun(Value) -> Value end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_e2ee_offline_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end},
                {'write_msg_if_absent_with_sender', 9, fun(
                    <<"2026-02-28T12:00:00Z">>,
                    <<"c2c_edit_e2ee_offline_001">>,
                    <<"opaque-edit-ciphertext">>,
                    123,
                    456,
                    <<"2026-02-28T12:00:00Z">>,
                    <<"text">>,
                    _E2EE,
                    <<"did-sender">>
                ) ->
                    ok
                end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(456) -> false end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"msg_type">> => <<"text">>,
                <<"payload">> => <<"opaque-edit-ciphertext">>,
                <<"sender_did">> => <<"did-sender">>,
                <<"e2ee">> => #{
                    <<"meta_version">> => 3,
                    <<"edit_of">> => <<"orig_c2c_edit_e2ee_offline_001">>,
                    <<"relay_action">> => <<"message_edit">>
                }
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(
                <<"c2c_edit_e2ee_offline_001">>, 123, Data
            ),
            ?assertEqual(<<"message_edit">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(<<"opaque-edit-ciphertext">>, maps:get(<<"payload">>, Reply)),
            ?assertEqual(1, meck:num_calls(msg_c2c_ds, write_msg_if_absent_with_sender, 9))
        end
    ).

c2c_edit_permission_denied_when_operator_not_sender_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(
                    <<"c2c_edit_denied_001">>, <<"permission_denied">>, <<"456">>
                ) ->
                    #{<<"id">> => <<"c2c_edit_denied_001">>, <<"error">> => <<"permission_denied">>}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"789">>,
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_edit_002">>,
                    <<"content">> => <<"edited content">>,
                    <<"msg_type">> => <<"text">>
                }
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_denied_001">>, 123, Data),
            ?assertEqual(<<"permission_denied">>, maps:get(<<"error">>, Reply))
        end
    ).

c2c_edit_rejected_when_window_expired_test_() ->
    ?WITH_MECKS(
        [
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_expired_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end}
            ]},
            {elib_dt, [
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end},
                %% 默认窗口 86400s，超出 1 秒
                {'millisecond', 0, fun() -> 1700000000000 + 86400000 + 1000 end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_edit_expired_001">>,
                    <<"content">> => <<"edited content">>,
                    <<"msg_type">> => <<"text">>
                }
            },

            {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_expired_001">>, 123, Data),
            ?assertEqual(<<"message_edit_error">>, maps:get(<<"action">>, Reply)),
            ReplyPayload = maps:get(<<"payload">>, Reply),
            ?assertEqual(409, maps:get(<<"code">>, ReplyPayload)),
            ?assertEqual(
                <<"orig_c2c_edit_expired_001">>, maps:get(<<"original_msg_id">>, ReplyPayload)
            )
        end
    ).

c2c_edit_allowed_when_window_disabled_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_c2c_edit_nowin_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end}
            ]},
            {elib_dt, [
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end},
                %% 距创建已远超 24h，但窗口配置为 0（不限制）
                {'millisecond', 0, fun() -> 1700000000000 + 30 * 86400000 end},
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
            ]},
            {user_logic, [
                {'is_online', 1, fun(456) -> true end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(456, <<"c2c_edit_nowin_001">>, _Msg, <<"c2s">>) ->
                    ok
                end}
            ]}
        ],
        fun() ->
            application:set_env(imboy, msg_edit_window_seconds, 0),
            try
                Data = #{
                    <<"to">> => <<"456">>,
                    <<"from">> => <<"123">>,
                    <<"payload">> => #{
                        <<"original_msg_id">> => <<"orig_c2c_edit_nowin_001">>,
                        <<"content">> => <<"edited content">>,
                        <<"msg_type">> => <<"text">>
                    }
                },

                {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_nowin_001">>, 123, Data),
                ?assertEqual(<<"message_edit_ack">>, maps:get(<<"action">>, Reply))
            after
                application:unset_env(imboy, msg_edit_window_seconds)
            end
        end
    ).

c2c_revoke_ack_persists_action_payload_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000090000 end}
            ]},
            {imboy_message_helper, [
                {'encode_json', 1, fun(_Map) -> <<"{\"action\":\"message_revoke_ack\"}">> end}
            ]},
            {msg_c2c_repo, [
                {'update_payload_by_msg_id', 2, fun(<<"orig_c2c_revoke_003">>, PayloadJson) ->
                    ?assert(is_binary(PayloadJson)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_revoke_003">>
                }
            },

            Result = msg_c2c_logic:c2c_revoke_ack(<<"c2c_revoke_ack_003">>, 123, Data),
            ?assertEqual(ok, Result),
            ?assertEqual(1, meck:num_calls(msg_c2c_repo, update_payload_by_msg_id, 2))
        end
    ).

c2c_edit_ack_persists_action_payload_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000095000 end}
            ]},
            {imboy_message_helper, [
                {'encode_json', 1, fun(_Map) -> <<"{\"action\":\"message_edit_ack\"}">> end}
            ]},
            {msg_c2c_repo, [
                {'update_payload_by_msg_id', 2, fun(<<"orig_c2c_edit_003">>, PayloadJson) ->
                    ?assert(is_binary(PayloadJson)),
                    {ok, 1}
                end}
            ]}
        ],
        fun() ->
            Data = #{
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_c2c_edit_003">>,
                    <<"content">> => <<"edited-content">>,
                    <<"edited_at">> => 1700000095000
                }
            },

            Result = msg_c2c_logic:c2c_edit_ack(<<"c2c_edit_ack_003">>, 123, Data),
            ?assertEqual(ok, Result),
            ?assertEqual(1, meck:num_calls(msg_c2c_repo, update_payload_by_msg_id, 2))
        end
    ).

%% 幂等短路回归：客户端重发（staging 命中唯一约束）只补发 SERVER_ACK，
%% 不得再次进入投递管道（曾经 duplicate 与 new 折叠为同一个 ok 造成重复推送）
c2c_duplicate_resend_acks_without_redelivery_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, duplicate} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]},
            {ai_agent_reply, [
                {'maybe_dispatch', 3, fun(_, _, _) -> ok end}
            ]},
            {billing_meter, [
                {'meter', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_dup_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            ok = msg_c2c_logic:c2c(MsgId, 123, Data),

            Reply =
                receive
                    {reply, Msg} -> Msg
                after 1000 -> timeout
                end,

            %% 补发 SERVER_ACK（客户端等它停止重发）
            ?assertNotEqual(timeout, Reply),
            ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, Reply)),
            %% 投递管道不得再次执行
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(0, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            %% duplicate 不得重复触发旁路副作用
            ?assertEqual(0, meck:num_calls(ai_agent_reply, maybe_dispatch, 3)),
            ?assertEqual(0, meck:num_calls(billing_meter, meter, 2))
        end
    ).

%% ===================================================================
%% 部署级 E2EE 明文拒收门（真实策略判定）
%%
%% 上面三个 *_when_encryption_required 测试把 imboy_policy:validate_message_write/5
%% 整个桩掉，只验证了「门返回 error 时外壳如何应答」这段 wiring，测不出判定契约本身：
%% 明文部署会不会误伤、加密消息会不会被误判成明文，一条都没覆盖。
%% 下面这组只桩 config_ds（部署配置边界），imboy_policy 的判定链
%% effective_capabilities → message_encryption_required → encrypted_message_body
%% 全程真实执行。
%% ===================================================================

%% 部署配置桩：Caps 为 capabilities 覆盖项（#{} = 用 community 档默认 e2ee_mode=optional）
policy_config_meck(Caps) ->
    {config_ds, [
        {'get', 2, fun(_Key, Default) -> Default end},
        {'env', 2, fun
            (product_profile, community) -> community;
            (capabilities, #{}) -> Caps;
            %% 其余 key（msg_rate_* 等）一律取默认值，不干扰被测判定
            (_Key, Default) -> Default
        end}
    ]}.

%% 走通投递管道所需的周边桩（不含 imboy_policy）
c2c_pipeline_mecks() ->
    [
        {friend_ds, [
            {'check_relationship', 2, fun(456, 123) -> {true, false} end}
        ]},
        {ai_agent_ds, [
            {'is_agent', 1, fun(_) -> false end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end},
            {'millisecond', 0, fun() -> 1708768800000 end}
        ]},
        {msg_store_ds, [
            {'stage', 11, fun(_, _, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_async, [
            {'async_retry', 3, fun(Fun, 3, 1000) ->
                Fun(),
                ok
            end},
            {'async', 1, fun(Fun) ->
                Fun(),
                self()
            end}
        ]},
        {push_notification_logic, [
            {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
        ]},
        {message_ds, [
            {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
        ]},
        {imboy_message_helper, [
            {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
        ]},
        {ai_agent_reply, [
            {'maybe_dispatch', 3, fun(_, _, _) -> ok end}
        ]},
        {billing_meter, [
            {'meter', 2, fun(_, _) -> ok end}
        ]}
    ].

plaintext_c2c_data() ->
    #{
        <<"to">> => <<"456">>,
        <<"payload">> => #{<<"content">> => <<"hello">>},
        <<"created_at">> => 1708768700000,
        <<"msg_type">> => <<"text">>,
        <<"action">> => <<>>,
        <<"e2ee">> => null
    }.

%% 【防误伤·关键】明文部署（community 档 e2ee_mode=optional）下明文 C2C 必须照常放行。
%% 这道门经过每一条 C2C，判错就是全站发不出消息。
c2c_plaintext_allowed_when_deployment_does_not_require_e2ee_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{}) | c2c_pipeline_mecks()],
        fun() ->
            ?assertEqual(
                ok, msg_c2c_logic:c2c(<<"msg_plain_allowed_001">>, 123, plaintext_c2c_data())
            ),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).

%% 【防误伤】storage_mode=archived + e2ee_mode=disabled（enterprise 档形态）同样放行
c2c_plaintext_allowed_when_e2ee_disabled_test_() ->
    ?WITH_MECKS(
        [
            policy_config_meck(#{storage_mode => archived, e2ee_mode => disabled})
            | c2c_pipeline_mecks()
        ],
        fun() ->
            ?assertEqual(
                ok, msg_c2c_logic:c2c(<<"msg_plain_allowed_002">>, 123, plaintext_c2c_data())
            ),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11))
        end
    ).

%% e2ee_mode=required 的部署：明文 C2C 被真实策略拒收，且一个字节都没落库
c2c_plaintext_blocked_by_real_policy_when_e2ee_required_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{e2ee_mode => required}) | c2c_pipeline_mecks()],
        fun() ->
            {reply, Reply} = msg_c2c_logic:c2c(
                <<"msg_plain_blocked_001">>, 123, plaintext_c2c_data()
            ),
            ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(
                <<"encrypted_message_required">>,
                maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
            ),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(0, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).

%% storage_mode=secure_e2ee 是另一条触发路径，同样拒收明文
c2c_plaintext_blocked_when_storage_mode_secure_e2ee_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{storage_mode => secure_e2ee}) | c2c_pipeline_mecks()],
        fun() ->
            {reply, Reply} = msg_c2c_logic:c2c(
                <<"msg_plain_blocked_002">>, 123, plaintext_c2c_data()
            ),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 11))
        end
    ).

%% 【防误伤·关键】真实客户端现役形态：加密消息的 msg_type 保留原始类型（text），
%% 只有顶层 e2ee 元数据标识加密。若判定去看 msg_type=<<"e2ee">>，这条会被
%% 误判成明文而在 required 部署下全量拒发。
c2c_encrypted_allowed_when_msg_type_stays_text_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{e2ee_mode => required}) | c2c_pipeline_mecks()],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => <<"bm9uY2U=.Y2lwaGVydGV4dA==">>,
                <<"created_at">> => 1708768700000,
                %% 关键：不是 <<"e2ee">>
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => #{
                    <<"e2ee">> => true,
                    <<"e2ee_ver">> => 1,
                    <<"e2ee_suite">> => <<"RSA-OAEP-256+AES-256-GCM">>,
                    <<"nonce">> => <<"bm9uY2U=">>,
                    <<"keys">> => [#{<<"did">> => <<"d1">>, <<"ek">> => <<"ZWs=">>}]
                }
            },
            ?assertEqual(ok, msg_c2c_logic:c2c(<<"msg_enc_allowed_001">>, 123, Data)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

%% payload 为 map 的加密消息（encode_payload 转 JSON binary）同样不能被误判
c2c_encrypted_allowed_when_payload_is_map_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{e2ee_mode => required}) | c2c_pipeline_mecks()],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"bm9uY2U=.Y2lwaGVy">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"image">>,
                <<"action">> => <<>>,
                <<"e2ee">> => #{<<"e2ee">> => true, <<"nonce">> => <<"bm9uY2U=">>}
            },
            ?assertEqual(ok, msg_c2c_logic:c2c(<<"msg_enc_allowed_002">>, 123, Data)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 11))
        end
    ).

%% 【防误伤】控制帧（撤回）不是内容承载动作，required 部署下必须照常工作。
%% 撤回/已读/各类 ack 只搬运 original_msg_id 等元数据，本身不含明文正文；
%% 若一并拦下，required 部署里用户将永远无法撤回消息。
c2c_revoke_not_blocked_when_e2ee_required_test_() ->
    ?WITH_MECKS(
        [
            policy_config_meck(#{e2ee_mode => required}),
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(<<"orig_revoke_gate_001">>) ->
                    {ok, #{<<"from_id">> => 123, <<"created_at">> => 1700000000000}}
                end},
                {'revoke_offline_msg', 9, fun(_, _, _, _, _, _, _, _, _) -> ok end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000060000 end},
                {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end},
                {'rfc3339_to', 2, fun(_, millisecond) -> 1700000000000 end}
            ]},
            {user_logic, [{'is_online', 1, fun(456) -> false end}]},
            {user_device_logic, [{'online_dids', 1, fun(456) -> [] end}]}
        ],
        fun() ->
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{<<"original_msg_id">> => <<"orig_revoke_gate_001">>}
            },
            {reply, Reply} = msg_c2c_logic:c2c_revoke(<<"msg_revoke_gate_001">>, 123, Data),
            %% 不是 policy_violation，而是正常的撤回确认
            ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(<<"C2C">>, maps:get(<<"type">>, Reply))
        end
    ).

%% 【门后旁路】被门拒收的消息绝不能再触发 agent 旁路：
%% agent 只会以明文 C2C 回投用户，等于把刚拦下的明文从旁路放出去；
%% billing 也不该给一条从未发出的消息计量。
c2c_blocked_message_does_not_trigger_agent_or_billing_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{e2ee_mode => required}) | c2c_pipeline_mecks()],
        fun() ->
            {reply, _} = msg_c2c_logic:c2c(
                <<"msg_blocked_noagent_001">>, 123, plaintext_c2c_data()
            ),
            ?assertEqual(0, meck:num_calls(ai_agent_reply, maybe_dispatch, 3)),
            ?assertEqual(0, meck:num_calls(billing_meter, meter, 2))
        end
    ).

%% 【防误伤】消息被放行时 agent 旁路与计量必须照常触发（上一条修复不得误杀正常链路）
c2c_allowed_message_still_triggers_agent_and_billing_test_() ->
    ?WITH_MECKS(
        [policy_config_meck(#{}) | c2c_pipeline_mecks()],
        fun() ->
            ok = msg_c2c_logic:c2c(<<"msg_allowed_agent_001">>, 123, plaintext_c2c_data()),
            ?assertEqual(1, meck:num_calls(ai_agent_reply, maybe_dispatch, 3)),
            ?assertEqual(1, meck:num_calls(billing_meter, meter, 2))
        end
    ).
