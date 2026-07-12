-module(msg_c2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

c2c_success_sends_server_ack_and_dispatch_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
            ]},
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10)),
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

c2c_reply_to_missing_message_emits_msg_not_found_reply_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
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
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, new} end}
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10))
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
            {imboy_policy, [
                {'validate_message_write', 5, fun(_, _, _, _, _) ->
                    {error, <<"encrypted_message_required">>}
                end}
            ]},
            {msg_store_ds, [
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
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
            ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10)),
            ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3))
        end
    ).

c2c_e2ee_message_allowed_when_encryption_required_test_() ->
    ?WITH_MECKS(
        [
            {friend_ds, [
                {'check_relationship', 2, fun(456, 123) -> {true, false} end}
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
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, new} end},
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
            ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10)),
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
            {elib_dt, [
                {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
                {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
                {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
            ]},
            {msg_store_ds, [
                {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> {ok, duplicate} end},
                {'enqueue', 3, fun(_, _, _) -> ok end}
            ]},
            {imboy_message_helper, [
                {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
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
            ?assertEqual(0, meck:num_calls(imboy_message_helper, encode_and_send, 4))
        end
    ).
