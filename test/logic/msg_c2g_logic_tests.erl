-module(msg_c2g_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

c2g_success_sends_server_ack_and_dispatch_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end},
            {'encode', 1, fun(1001) -> <<"from_user">> end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> true end},
            {'member_uids', 1, fun(100) -> [1001, 1002, 1003] end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> ok end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2g">>) -> [0, 200] end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_, _, _, _) -> ok end}
        ]},
        {mention_logic, [
            {'create_mentions', 4, fun(_, _, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_ok_001">>,
        CurrentUid = 1001,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"hello group">>, <<"mentions">> => []},
            <<"created_at">> => 1708768700000,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => null
        },

        ok = msg_c2g_logic:c2g(MsgId, CurrentUid, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(MsgId, maps:get(<<"id">>, Reply)),
        ?assertEqual(<<"C2G_SERVER_ACK">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10)),
        ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3)),
        ?assertEqual(2, meck:num_calls(message_ds, send_next, 4)),
        ?assertEqual(0, meck:num_calls(mention_logic, create_mentions, 4))
    end).

c2g_muted_user_gets_error_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> true end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_muted_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"hello">>},
            <<"created_at">> => 1708768700000
        },

        ok = msg_c2g_logic:c2g(MsgId, 1001, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(<<"C2G_ERROR">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(403, maps:get(<<"code">>, Reply))
    end).

c2g_non_member_gets_error_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> false end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_non_member_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"hello">>},
            <<"created_at">> => 1708768700000
        },

        ok = msg_c2g_logic:c2g(MsgId, 1001, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(<<"C2G_ERROR">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(403, maps:get(<<"code">>, Reply))
    end).

c2g_mention_all_requires_admin_role_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> true end}
        ]},
        {group_member_ds, [
            {'check_admin', 2, fun(1001, 100) -> false end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_all_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"hello">>, <<"mentions">> => [<<"all">>]},
            <<"created_at">> => 1708768700000
        },

        ok = msg_c2g_logic:c2g(MsgId, 1001, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(<<"C2G_ERROR">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(403, maps:get(<<"code">>, Reply))
    end).

c2g_reply_to_missing_message_emits_msg_not_found_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100;
                             (<<"from_user">>) -> 777
                         end},
            {'encode', 1, fun(1001) -> <<"from_user">> end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> true end},
            {'member_uids', 1, fun(100) -> [1001, 1002] end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-24T10:00:00Z">> end},
            {'rfc3339_to', 2, fun(<<"2026-02-24T10:00:00Z">>, millisecond) -> 1708768800000 end},
            {'to_rfc3339', 1, fun(1708768700000) -> <<"2026-02-24T09:58:20Z">> end}
        ]},
        {msg_c2g_repo, [
            {'find_msg_by_id', 1, fun(<<"missing_group_reply_msg">>) -> {error, not_found} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, <<"msg_not_found">>, <<"group_1">>) ->
                #{<<"id">> => MsgId, <<"type">> => <<"MSG_NOT_FOUND">>}
            end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_reply_missing_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"reply">>, <<"mentions">> => []},
            <<"created_at">> => 1708768700000,
            <<"reply_to">> => #{
                <<"msg_id">> => <<"missing_group_reply_msg">>,
                <<"from_id">> => <<"from_user">>
            }
        },

        ok = msg_c2g_logic:c2g(MsgId, 1001, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(<<"MSG_NOT_FOUND">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10))
    end).

c2g_client_ack_delegates_to_ack_logic_test_() ->
    ?WITH_MECKS([
        {msg_ack_logic, [
            {'client_ack', 4, fun(<<"c2g">>, <<"msg_ack_001">>, 1001, <<"did_1">>) -> ok end}
        ]}
    ], fun() ->
        ok = msg_c2g_logic:c2g_client_ack(<<"msg_ack_001">>, 1001, <<"did_1">>),
        ?assertEqual(1, meck:num_calls(msg_ack_logic, client_ack, 4))
    end).

c2g_revoke_ack_persists_action_payload_test_() ->
    ?WITH_MECKS([
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
        {msg_c2g_repo, [
            {'update_payload_by_msg_id', 2, fun(<<"orig_c2g_revoke_003">>, PayloadJson) ->
                ?assert(is_binary(PayloadJson)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2g_revoke_003">>
            }
        },

        Result = msg_c2g_logic:c2g_revoke_ack(<<"c2g_revoke_ack_003">>, 1001, Data),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_c2g_repo, update_payload_by_msg_id, 2))
    end).

c2g_edit_ack_persists_action_payload_test_() ->
    ?WITH_MECKS([
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
        {msg_c2g_repo, [
            {'update_payload_by_msg_id', 2, fun(<<"orig_c2g_edit_003">>, PayloadJson) ->
                ?assert(is_binary(PayloadJson)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2g_edit_003">>,
                <<"content">> => <<"edited-content">>,
                <<"edited_at">> => 1700000095000
            }
        },

        Result = msg_c2g_logic:c2g_edit_ack(<<"c2g_edit_ack_003">>, 1001, Data),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_c2g_repo, update_payload_by_msg_id, 2))
    end).

read_stats_success_returns_read_and_total_counts_test_() ->
    ?WITH_MECKS([
        {msg_c2g_timeline_repo, [
            {'find_by_msg_id', 1, fun(<<"msg_stats_001">>) -> {ok, [#{<<"to_gid">> => 88}]} end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 88) -> true end},
            {'member_uids', 1, fun(88) -> [1001, 1002, 1003, 1004] end}
        ]},
        {msg_c2g_repo, [
            {'count_read', 1, fun(<<"msg_stats_001">>) -> 2 end}
        ]}
    ], fun() ->
        ?assertEqual({ok, 2, 4}, msg_c2g_logic:read_stats(<<"msg_stats_001">>, 1001))
    end).

read_stats_permission_denied_for_non_member_test_() ->
    ?WITH_MECKS([
        {msg_c2g_timeline_repo, [
            {'find_by_msg_id', 1, fun(<<"msg_stats_002">>) -> {ok, [#{<<"to_gid">> => 99}]} end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 99) -> false end}
        ]}
    ], fun() ->
        ?assertEqual({error, permission_denied}, msg_c2g_logic:read_stats(<<"msg_stats_002">>, 1001))
    end).

extract_reply_info_without_reply_to_returns_empty_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({<<>>, 0, <<>>}, msg_c2g_logic:extract_reply_info(#{}))
    end).

extract_reply_info_with_json_payload_extracts_content_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 321 end}
        ]},
        {msg_c2g_repo, [
            {'find_msg_by_id', 1, fun(<<"origin_group_msg_001">>) ->
                {ok, #{<<"payload">> => <<"{\"content\":\"hello group reply\"}">>}}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"reply_to">> => #{
                <<"msg_id">> => <<"origin_group_msg_001">>,
                <<"from_id">> => <<"from_user">>
            }
        },

        {ReplyToMsgId, ReplyToFromId, ReplySnippet} = msg_c2g_logic:extract_reply_info(Data),
        ?assertEqual(<<"origin_group_msg_001">>, ReplyToMsgId),
        ?assertEqual(321, ReplyToFromId),
        ?assertEqual(<<"hello group reply">>, ReplySnippet)
    end).

c2g_revoke_success_broadcasts_and_persists_offline_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 88;
                             (<<"from_user">>) -> 1001
                         end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 88) -> true end},
            {'member_uids', 1, fun(88) -> [1001, 1002, 1003] end}
        ]},
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun(<<"orig_c2g_revoke_001">>) ->
                {ok, #{
                    <<"from_id">> => 1001,
                    <<"created_at">> => 1700000000000
                }}
            end},
            {'revoke_offline_msg', 9, fun(_, _, _, _, _, _, _, _, _) -> ok end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000060000 end},
            {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2g">>) -> [0, 200] end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_, _, _, _) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2g_revoke_001">>}
        },

        {reply, Reply} = msg_c2g_logic:c2g_revoke(<<"c2g_revoke_001">>, 1001, Data),
        ReplyPayload = maps:get(<<"payload">>, Reply),
        ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply)),
        ?assertEqual(<<"custom">>, maps:get(<<"msg_type">>, Reply)),
        ?assert(maps:is_key(<<"revoked_at">>, ReplyPayload)),
        ?assertEqual(2, meck:num_calls(message_ds, send_next, 4)),
        ?assertEqual(1, meck:num_calls(msg_c2g_ds, revoke_offline_msg, 9))
    end).

c2g_revoke_permission_denied_when_operator_not_sender_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 88;
                             (<<"from_user">>) -> 2002
                         end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 88) -> true end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(<<"c2g_revoke_denied_001">>, <<"permission_denied">>, <<"group_1">>) ->
                #{<<"id">> => <<"c2g_revoke_denied_001">>, <<"error">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2g_revoke_002">>}
        },

        {reply, Reply} = msg_c2g_logic:c2g_revoke(<<"c2g_revoke_denied_001">>, 1001, Data),
        ?assertEqual(<<"permission_denied">>, maps:get(<<"error">>, Reply))
    end).

c2g_edit_success_broadcasts_and_persists_offline_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 88;
                             (<<"from_user">>) -> 1001
                         end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 88) -> true end},
            {'member_uids', 1, fun(88) -> [1001, 1002, 1003] end}
        ]},
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun(<<"orig_c2g_edit_001">>) ->
                {ok, #{
                    <<"from_id">> => 1001,
                    <<"created_at">> => 1700000000000
                }}
            end},
            {'edit_offline_msg', 6, fun(_, _, _, _, _, _) -> ok end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000065000 end},
            {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2g">>) -> [0, 200] end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_, _, _, _) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2g_edit_001">>,
                <<"content">> => <<"new content">>,
                <<"msg_type">> => <<"text">>
            }
        },

        {reply, Reply} = msg_c2g_logic:c2g_edit(<<"c2g_edit_001">>, 1001, Data),
        ReplyPayload = maps:get(<<"payload">>, Reply),
        ?assertEqual(<<"message_edit_ack">>, maps:get(<<"action">>, Reply)),
        ?assertEqual(<<"text">>, maps:get(<<"msg_type">>, Reply)),
        ?assertEqual(<<"new content">>, maps:get(<<"content">>, ReplyPayload)),
        ?assertEqual(2, meck:num_calls(message_ds, send_next, 4)),
        ?assertEqual(1, meck:num_calls(msg_c2g_ds, edit_offline_msg, 6))
    end).

c2g_plaintext_blocked_when_encryption_required_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> true end},
            {'member_uids', 1, fun(100) -> [1001, 1002, 1003] end}
        ]},
        {imboy_policy, [
            {'validate_message_write', 5, fun(_, _, _, _, _) ->
                {error, <<"encrypted_message_required">>}
            end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> ok end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_plaintext_blocked_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => #{<<"content">> => <<"hello group">>, <<"mentions">> => []},
            <<"created_at">> => 1708768700000,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => null
        },

        {reply, Reply} = msg_c2g_logic:c2g(MsgId, 1001, Data),
        ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
        ?assertEqual(
            <<"encrypted_message_required">>,
            maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
        ),
        ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10)),
        ?assertEqual(0, meck:num_calls(msg_store_ds, enqueue, 3))
    end).

c2g_e2ee_message_allowed_when_encryption_required_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 100 end},
            {'encode', 1, fun(1001) -> <<"from_user">> end}
        ]},
        {group_member_logic, [
            {'check_mute', 2, fun(100, 1001) -> false end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 100) -> true end},
            {'member_uids', 1, fun(100) -> [1001, 1002, 1003] end}
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
            {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> ok end},
            {'enqueue', 3, fun(_, _, _) -> ok end}
        ]},
        {elib_retry_config, [
            {'intervals', 1, fun(<<"c2g">>) -> [0, 200] end}
        ]},
        {message_ds, [
            {'send_next', 4, fun(_, _, _, _) -> ok end}
        ]},
        {mention_logic, [
            {'create_mentions', 4, fun(_, _, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_e2ee_allowed_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"payload">> => <<"nonce.ciphertext">>,
            <<"created_at">> => 1708768700000,
            <<"msg_type">> => <<"e2ee">>,
            <<"action">> => <<>>,
            <<"e2ee">> => #{<<"e2ee">> => true}
        },

        ok = msg_c2g_logic:c2g(MsgId, 1001, Data),
        ?assertEqual(1, meck:num_calls(msg_store_ds, stage, 10)),
        ?assertEqual(1, meck:num_calls(msg_store_ds, enqueue, 3))
    end).

c2g_edit_plaintext_blocked_when_encryption_required_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"group_1">>) -> 88;
                             (<<"from_user">>) -> 1001
                         end}
        ]},
        {group_ds, [
            {'is_member', 2, fun(1001, 88) -> true end}
        ]},
        {msg_c2g_ds, [
            {'find_msg_by_id', 1, fun(<<"orig_c2g_edit_blocked_001">>) ->
                {ok, #{
                    <<"from_id">> => 1001,
                    <<"created_at">> => 1700000000000
                }}
            end},
            {'edit_offline_msg', 6, fun(_, _, _, _, _, _) -> ok end}
        ]},
        {imboy_policy, [
            {'validate_message_write', 5, fun(_, _, _, _, _) ->
                {error, <<"encrypted_message_required">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2g_edit_plaintext_blocked_001">>,
        Data = #{
            <<"to">> => <<"group_1">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2g_edit_blocked_001">>,
                <<"content">> => <<"new content">>,
                <<"msg_type">> => <<"text">>
            },
            <<"e2ee">> => null
        },

        {reply, Reply} = msg_c2g_logic:c2g_edit(MsgId, 1001, Data),
        ?assertEqual(<<"S2C">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(<<"policy_violation">>, maps:get(<<"action">>, Reply)),
        ?assertEqual(
            <<"encrypted_message_required">>,
            maps:get(<<"reason">>, maps:get(<<"payload">>, Reply))
        ),
        ?assertEqual(0, meck:num_calls(msg_c2g_ds, edit_offline_msg, 6))
    end).
