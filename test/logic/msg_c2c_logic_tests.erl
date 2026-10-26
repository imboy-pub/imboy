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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end},
                %% R03/D04：固定服务端时钟，验证 accepted_at 取值来源
                {'millisecond', 0, fun() -> 1708768800000 end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
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
                {'assemble_msg', 8, fun(Type, From, To, Payload, MsgId, MsgType, Action, Meta) ->
                    #{
                        <<"id">> => MsgId,
                        <<"type">> => Type,
                        <<"from">> => From,
                        <<"to">> => To,
                        <<"payload">> => Payload,
                        <<"msg_type">> => MsgType,
                        <<"action">> => Action,
                        <<"e2ee">> => Meta
                    }
                end}
            ]},
            {msg_c2c_delivery_ds, [
                {'send_other_devices', 4, fun(123, <<"server-device">>, <<"msg_c2c_ok_001">>, Msg) ->
                    ?assertEqual(123, maps:get(<<"from">>, Msg)),
                    ?assertEqual(<<"456">>, maps:get(<<"to">>, Msg)),
                    ?assertEqual(<<"server-device">>, maps:get(<<"sender_did">>, Msg)),
                    ok
                end}
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
                <<"from">> => <<"spoofed-account">>,
                <<"sender_did">> => <<"spoofed-device">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            %% Use production connection stamping before the business entry.
            Stamped = message_ds:stamp_sender_device(
                Data,
                #{did => <<"server-device">>, dtype => <<"android">>}
            ),
            ok = msg_c2c_logic:c2c(MsgId, CurrentUid, Stamped),

            Reply =
                receive
                    {reply, Msg} -> Msg
                after 1000 ->
                    timeout
                end,

            ?assertNotEqual(timeout, Reply),
            ?assertEqual(MsgId, maps:get(<<"id">>, Reply)),
            ?assertEqual(<<"C2C_SERVER_ACK">>, maps:get(<<"type">>, Reply)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
            %% R03-1：接受时间 = 固定服务端时钟（elib_dt:millisecond 一次取值，
            %% UTC "Z" 形式），与客户端 created_at（1708768700000）无关
            ?assertEqual(
                elib_dt:to_rfc3339(1708768800000, millisecond, "Z"),
                meck:capture(1, msg_store_ds, stage_with_sequence, 13, 12)
            ),
            %% R05-A：无 expire_secs 时第 13 参为 null（不自毁，旧语义）
            ?assertEqual(
                null, meck:capture(1, msg_store_ds, stage_with_sequence, 13, 13)
            ),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3)),
            ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
            ?assertEqual(1, meck:num_calls(msg_c2c_delivery_ds, send_other_devices, 4))
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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
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
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end}
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

%% 透明 AI 豁免（2026-09-09）：对端是启用中的 agent 时，required 部署的明文门
%% 放行——AI 助手会话是产品声明的明文通道（AI 不进 E2EE，见
%% docs/explanation/ai-companion-flutter-ui.md「E2EE 红线」）。豁免前这条链被
%% policy_violation 拒收，AI 广场「发消息」在任何 strict 部署下都无法触发回复。
c2c_plaintext_allowed_for_agent_peer_when_encryption_required_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {false, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> {true, #{<<"role_status">> => 1}} end}
            ]},
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
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
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_agent_plaintext_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"你好"/utf8>>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            %% agent 豁免好友校验（is_agent→SendIsFriend=true），明文门放行走完整条投递链
            ok = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
            ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3))
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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
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
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
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
            %% 透明 AI 豁免分支：对端是真人（非 agent），明文编辑维持拒收
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
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
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, duplicate, 1}
                end},
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
        {bot_ds, [
            {'is_bot', 1, fun(_) -> false end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end},
            {'millisecond', 0, fun() -> 1708768800000 end}
        ]},
        {msg_store_ds, [
            {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                {ok, new, 1}
            end},
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
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

%% LT02-SEC-01（AI-ID=B）后端二次门锁定：客户端徽章/本地 DB 声称对端是 AI
%% 助手，但服务端权威表 ai_agent_ds:is_agent 为 false 时，required 部署的
%% 明文门必须拒收，且**全副作用为零**：不落库（stage/enqueue=0）、不触发
%% agent 明文回投、不计费、不推送。本测试把 L372 拒收路径与 L1286 零副作用
%% 断言合成计划负例矩阵的精确形态；服务端门是权威二道门，不能替代客户端
%% pre-send 共享身份门（AiPlaintextGate），但对「客户端被污染误判」兜底。
c2c_agent_gate_rejection_zero_side_effects_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {ok, new, 1}
                end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {push_notification_logic, [
                {'maybe_push_for_c2c', 4, fun(_, _, _, _) -> ok end}
            ]},
            {ai_agent_reply, [
                {'maybe_dispatch', 3, fun(_, _, _) -> ok end}
            ]},
            {billing_meter, [
                {'meter', 2, fun(_, _) -> ok end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_agent_gate_zero_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },

            {reply, Reply} = msg_c2c_logic:c2c(MsgId, 123, Data),
            ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
            ?assertEqual(
                <<"encrypted_message_required">>,
                maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
            ),
            %% 零落库
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage_with_sequence, 13)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3)),
            %% 零 agent 触发 / 零计费 / 零推送
            ?assertEqual(0, meck:num_calls(ai_agent_reply, maybe_dispatch, 3)),
            ?assertEqual(0, meck:num_calls(billing_meter, meter, 2)),
            ?assertEqual(0, meck:num_calls(push_notification_logic, maybe_push_for_c2c, 4))
        end
    ).

%% 【硬闸·关键】storage_mode=disabled 是"本部署整体不用 E2EE"的硬闸：哪怕配置里
%% e2ee_mode 仍写着 required（矛盾组合），明文 C2C 也必须放行——这是"一个开关能
%% 关死"的验收面；不放行就等于"关掉 E2EE 的部署里 C2C 文本全部发不出去"。
%% 对照组见 c2c_plaintext_blocked_by_real_policy_when_e2ee_required_test_（同一份
%% 配置去掉硬闸值就恢复拒收）。
c2c_plaintext_allowed_when_storage_mode_hard_off_test_() ->
    ?WITH_MECKS(
        [
            policy_config_meck(#{storage_mode => disabled, e2ee_mode => required})
            | c2c_pipeline_mecks()
        ],
        fun() ->
            ?assertEqual(
                ok,
                msg_c2c_logic:c2c(
                    <<"msg_plain_allowed_hard_off">>, 123, plaintext_c2c_data()
                )
            ),
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage_with_sequence, 13))
        end
    ).

%% ===================================================================
%% Q06（AC11 故障注入）：staging 写入失败的 SERVER_ACK 持久化边界
%% ===================================================================

%% @doc durable 承诺点 = staging 同步写成功。注入 msg_store_ds:stage/11
%% 返回 {error, unavailable}（DB 故障）：必须不发 C2C_SERVER_ACK、不进
%% 投递管道（enqueue 0 次）、受控返回错误 reply。修复前 case StageResult
%% 无 {error,_} 分支，case_clause 直接崩掉调用进程（WS handler）。
c2c_stage_failure_no_ack_no_dispatch_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {ai_agent_ds, [
                {'is_agent', 1, fun(456) -> false end}
            ]},
            {bot_ds, [
                {'is_bot', 1, fun(_) -> false end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                %% 故障注入：staging 同步写入失败
                {'stage_with_sequence', 13, fun(_, _, _, _, _, _, _, _, _, _, _, _, _) ->
                    {error, unavailable}
                end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {message_ds, [
                {'assemble_s2c', 3, fun(MsgId, Action, _To) ->
                    #{<<"id">> => MsgId, <<"type">> => <<"S2C">>, <<"action">> => Action}
                end}
            ]},
            {elib_cnv, [
                {'safe_to_binary', 1, fun(unavailable) -> <<"unavailable">> end}
            ]}
        ],
        fun() ->
            MsgId = <<"msg_c2c_stage_fail_001">>,
            Data = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{<<"content">> => <<"hello">>},
                <<"created_at">> => 1708768700000,
                <<"msg_type">> => <<"text">>,
                <<"action">> => <<>>,
                <<"e2ee">> => null
            },
            Stamped = message_ds:stamp_sender_device(
                Data, #{did => <<"server-device">>, dtype => <<"android">>}
            ),
            %% 受控返回（不崩溃）
            Result = msg_c2c_logic:c2c(MsgId, 123, Stamped),
            ?assertMatch({reply, #{<<"action">> := <<"unavailable">>}}, Result),
            %% 不发 SERVER_ACK
            NoAck =
                receive
                    {reply, #{<<"type">> := <<"C2C_SERVER_ACK">>}} -> got_ack
                after 200 ->
                    none
                end,
            ?assertEqual(none, NoAck),
            %% 不进持久化/投递管道
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

%%%===================================================================
%%% R04（IMBOY-TIMEZONE-REPAIR-V1）：撤回/编辑窗口用可信时间
%%%
%%% 窗口基准 = 首次服务端接受时间 accepted_at（D04），比较对象 = 服务端
%%% 当前时刻；客户端 created_at 任意取值不影响允许时长；时间源全部缺失
%%% 时 fail-closed 明确拒绝。
%%%===================================================================

%% 固定基准：accepted_at = 2023-11-14T22:13:20Z = 1700000000000ms
-define(R04_ACCEPTED_MS, 1700000000000).
-define(R04_ACCEPTED_RFC, <<"2023-11-14T22:13:20Z">>).

r04_revoke_data(OriginalId) ->
    #{
        <<"to">> => <<"456">>,
        <<"from">> => <<"123">>,
        <<"payload">> => #{<<"original_msg_id">> => OriginalId}
    }.

%% R04-1：撤回窗口边界——窗口前/正好边界/边界后（`>` 才超时的现状语义保持）
r04_1_revoke_window_boundaries_test_() ->
    [
        %% 窗口前（接受后 119999ms < 120000ms）：允许
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{
                            <<"from_id">> => 123,
                            <<"accepted_at">> => ?R04_ACCEPTED_RFC,
                            <<"created_at">> => ?R04_ACCEPTED_RFC
                        }}
                    end},
                    {'revoke_offline_msg', 9, fun(_, _, _, _, _, _, _, _, _) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 119999 end},
                    {'now', 0, fun() -> <<"2023-11-14T22:15:19Z">> end}
                ]},
                {user_logic, [{'is_online', 1, fun(456) -> false end}]},
                {user_device_logic, [{'online_dids', 1, fun(456) -> [] end}]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_before">>, 123, r04_revoke_data(<<"orig_r04_1">>)
                ),
                ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply))
            end
        ),
        %% 正好边界（差值 == 窗口，`>` 不成立）：仍允许
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{
                            <<"from_id">> => 123,
                            <<"accepted_at">> => ?R04_ACCEPTED_RFC,
                            <<"created_at">> => ?R04_ACCEPTED_RFC
                        }}
                    end},
                    {'revoke_offline_msg', 9, fun(_, _, _, _, _, _, _, _, _) -> ok end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 120000 end},
                    {'now', 0, fun() -> <<"2023-11-14T22:15:20Z">> end}
                ]},
                {user_logic, [{'is_online', 1, fun(456) -> false end}]},
                {user_device_logic, [{'online_dids', 1, fun(456) -> [] end}]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_boundary">>, 123, r04_revoke_data(<<"orig_r04_1">>)
                ),
                ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply))
            end
        ),
        %% 边界后（差值 > 窗口 1ms）：超时拒绝
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{
                            <<"from_id">> => 123,
                            <<"accepted_at">> => ?R04_ACCEPTED_RFC,
                            <<"created_at">> => ?R04_ACCEPTED_RFC
                        }}
                    end}
                ]},
                {elib_dt, [
                    {'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 120001 end}
                ]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_after">>, 123, r04_revoke_data(<<"orig_r04_1">>)
                ),
                ?assertEqual(<<"message_revoke_error">>, maps:get(<<"action">>, Reply)),
                ?assertEqual(409, maps:get(<<"code">>, maps:get(<<"payload">>, Reply)))
            end
        )
    ].

%% R04-2：设备快慢/时区/重试不改允许时长——客户端 created_at 任意取值
%% （慢 1 天 / 快 1 天 / 完全非法）不影响判定，窗口只看 accepted_at。
r04_2_client_created_at_irrelevant_test_() ->
    ClientVariants = [
        {slow_one_day, ?R04_ACCEPTED_MS - 86400000},
        {fast_one_day, ?R04_ACCEPTED_MS + 86400000},
        {unparsable, <<"not-a-time">>},
        {missing, null}
    ],
    [
        begin
            ClientCreatedAt =
                case Variant of
                    unparsable -> <<"not-a-time">>;
                    missing -> null;
                    _ -> elib_dt:to_rfc3339(Ms, millisecond, "Z")
                end,
            ?WITH_MECKS(
                [
                    {msg_c2c_ds, [
                        {'find_msg_by_id', 1, fun(_) ->
                            {ok, #{
                                <<"from_id">> => 123,
                                <<"accepted_at">> => ?R04_ACCEPTED_RFC,
                                <<"created_at">> => ClientCreatedAt
                            }}
                        end}
                    ]},
                    {elib_dt, [{'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 60000 end}]}
                ],
                fun() ->
                    %% 接受后 60s（< 2 分钟窗口）→ 无论客户端 created_at 多离谱都允许
                    {reply, Reply} = msg_c2c_logic:c2c_revoke(
                        <<"r04_rev_var">>, 123, r04_revoke_data(<<"orig_r04_2">>)
                    ),
                    ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply))
                end
            )
        end
     || {Variant, Ms} <- ClientVariants
    ].

%% R04-3：第一次编辑不重置后续期限——编辑成功后，原消息 accepted_at 不变，
%% 后续编辑仍以首次接受时间为基准（accepted_at 不可变，D04）。
r04_3_first_edit_does_not_reset_deadline_test_() ->
    ?WITH_MECKS(
        [
            {elib_log, [
                {'internal_log', 4, fun(_, _, _, _) -> ok end},
                {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
            ]},
            {msg_c2c_ds, [
                {'find_msg_by_id', 1, fun(_) ->
                    %% 编辑#1 与编辑#2 看到同一原消息行：accepted_at 不随编辑变化
                    {ok, #{
                        <<"from_id">> => 123,
                        <<"accepted_at">> => ?R04_ACCEPTED_RFC,
                        <<"created_at">> => ?R04_ACCEPTED_RFC
                    }}
                end},
                {'update_payload_by_msg_id', 2, fun(_, _) -> ok end}
            ]},
            {elib_dt, [
                %% 编辑#1 时刻：接受后 1 小时（默认 24h 窗口内）
                {'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 3600000 end},
                {'now', 0, fun() -> <<"2023-11-14T23:13:20Z">> end}
            ]},
            {user_logic, [{'is_online', 1, fun(456) -> true end}]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
            ]}
        ],
        fun() ->
            EditData = #{
                <<"to">> => <<"456">>,
                <<"from">> => <<"123">>,
                <<"payload">> => #{
                    <<"original_msg_id">> => <<"orig_r04_3">>,
                    <<"content">> => <<"first edit"/utf8>>,
                    <<"msg_type">> => <<"text">>
                }
            },
            %% 编辑#1：窗口内成功
            {reply, Reply1} = msg_c2c_logic:c2c_edit(<<"r04_edit_1">>, 123, EditData),
            ?assertEqual(<<"message_edit_ack">>, maps:get(<<"action">>, Reply1)),
            %% 编辑#2：距首次接受 24h+1s（默认窗口外）——即便编辑#1 刚发生过，
            %% 期限仍是首次接受时间 + 窗口，不被编辑#1 重置。
            %% 编辑#2 前推进固定时钟（卸载后按同一 passthrough 方式重建桩）
            ok = meck:unload(elib_dt),
            {ok, _} = meck_helper:setup_mock(elib_dt, [
                {'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 86400000 + 1000 end},
                {'now', 0, fun() -> <<"2023-11-15T22:13:21Z">> end}
            ]),
            EditData2 = EditData#{
                <<"payload">> =>
                    (maps:get(<<"payload">>, EditData))#{<<"content">> => <<"second edit"/utf8>>}
            },
            {reply, Reply2} = msg_c2c_logic:c2c_edit(<<"r04_edit_2">>, 123, EditData2),
            ?assertEqual(<<"message_edit_error">>, maps:get(<<"action">>, Reply2)),
            ?assertEqual(409, maps:get(<<"code">>, maps:get(<<"payload">>, Reply2)))
        end
    ).

%% R04-4：旧数据回退与双缺失拒绝——accepted_at NULL 时回退 server_ts（低可信），
%% 全部时间源缺失/非法时 fail-closed 明确拒绝（不再静默放行）。
r04_4_legacy_fallback_and_fail_closed_test_() ->
    [
        %% 旧行：accepted_at NULL → server_ts 回退（低可信），窗口判定照常
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{
                            <<"from_id">> => 123,
                            <<"accepted_at">> => null,
                            <<"server_ts">> => ?R04_ACCEPTED_RFC,
                            <<"created_at">> => null
                        }}
                    end}
                ]},
                {elib_dt, [{'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 130000 end}]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_fallback">>, 123, r04_revoke_data(<<"orig_r04_4">>)
                ),
                ?assertEqual(<<"message_revoke_error">>, maps:get(<<"action">>, Reply)),
                ?assertEqual(409, maps:get(<<"code">>, maps:get(<<"payload">>, Reply)))
            end
        ),
        %% 双缺失：accepted_at/server_ts/created_at 全部缺失 → fail-closed 明确拒绝
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{<<"from_id">> => 123}}
                    end}
                ]},
                {elib_dt, [{'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 1000 end}]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_nosrc">>, 123, r04_revoke_data(<<"orig_r04_4b">>)
                ),
                ?assertEqual(<<"message_revoke_error">>, maps:get(<<"action">>, Reply)),
                ?assertEqual(
                    500, maps:get(<<"code">>, maps:get(<<"payload">>, Reply))
                )
            end
        ),
        %% 双非法：全部时间源存在但都不可解析 → 同样 fail-closed 拒绝
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{
                            <<"from_id">> => 123,
                            <<"accepted_at">> => <<>>,
                            <<"server_ts">> => <<"garbage">>,
                            <<"created_at">> => null
                        }}
                    end}
                ]},
                {elib_dt, [{'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 1000 end}]}
            ],
            fun() ->
                {reply, Reply} = msg_c2c_logic:c2c_revoke(
                    <<"r04_rev_badsrc">>, 123, r04_revoke_data(<<"orig_r04_4c">>)
                ),
                ?assertEqual(<<"message_revoke_error">>, maps:get(<<"action">>, Reply)),
                ?assertEqual(
                    500, maps:get(<<"code">>, maps:get(<<"payload">>, Reply))
                )
            end
        ),
        %% 编辑路径同款 fail-closed（c2c_edit_window_check → no_time_source 分支）
        ?WITH_MECKS(
            [
                {msg_c2c_ds, [
                    {'find_msg_by_id', 1, fun(_) ->
                        {ok, #{<<"from_id">> => 123}}
                    end}
                ]},
                {elib_dt, [{'millisecond', 0, fun() -> ?R04_ACCEPTED_MS + 1000 end}]}
            ],
            fun() ->
                EditData = #{
                    <<"to">> => <<"456">>,
                    <<"from">> => <<"123">>,
                    <<"payload">> => #{
                        <<"original_msg_id">> => <<"orig_r04_4d">>,
                        <<"content">> => <<"x">>,
                        <<"msg_type">> => <<"text">>
                    }
                },
                {reply, Reply} = msg_c2c_logic:c2c_edit(<<"r04_edit_nosrc">>, 123, EditData),
                ?assertEqual(<<"message_edit_error">>, maps:get(<<"action">>, Reply)),
                ?assertEqual(
                    500, maps:get(<<"code">>, maps:get(<<"payload">>, Reply))
                )
            end
        )
    ].
