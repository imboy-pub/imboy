-module(msg_c2c_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

c2c_success_sends_server_ack_and_dispatch_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end},
            {'encode', 1, fun(123) -> <<"from_user">> end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(456, 123) -> {true, false} end}
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
        {elib_async, [
            {'async_retry', 3, fun(Fun, 3, 1000) -> Fun(), ok end}
        ]},
        {message_ds, [
            {'assemble_msg', 8, fun(_, _, _, _, MsgId, _, _, _) -> #{<<"id">> => MsgId} end}
        ]},
        {imboy_message_helper, [
            {'encode_and_send', 4, fun(_, _, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2c_ok_001">>,
        CurrentUid = 123,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"payload">> => #{<<"content">> => <<"hello">>},
            <<"created_at">> => 1708768700000,
            <<"msg_type">> => <<"text">>,
            <<"action">> => <<>>,
            <<"e2ee">> => null
        },

        ok = msg_c2c_logic:c2c(MsgId, CurrentUid, Data),

        Reply = receive
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
    end).

c2c_not_friend_returns_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(456, 123) -> {false, 0} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, <<"not_a_friend">>, <<"to_user">>) ->
                #{<<"id">> => MsgId, <<"error">> => <<"not_a_friend">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2c_not_friend_001">>,
        Data = #{<<"to">> => <<"to_user">>},

        Result = msg_c2c_logic:c2c(MsgId, 123, Data),
        ?assertMatch({reply, _}, Result),
        {reply, Msg} = Result,
        ?assertEqual(<<"not_a_friend">>, maps:get(<<"error">>, Msg))
    end).

c2c_in_denylist_returns_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456 end}
        ]},
        {friend_ds, [
            {'check_relationship', 2, fun(456, 123) -> {true, 2} end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(MsgId, <<"in_denylist">>, <<"to_user">>) ->
                #{<<"id">> => MsgId, <<"error">> => <<"in_denylist">>}
            end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2c_deny_001">>,
        Data = #{<<"to">> => <<"to_user">>},

        Result = msg_c2c_logic:c2c(MsgId, 123, Data),
        ?assertMatch({reply, _}, Result),
        {reply, Msg} = Result,
        ?assertEqual(<<"in_denylist">>, maps:get(<<"error">>, Msg))
    end).

c2c_reply_to_missing_message_emits_msg_not_found_reply_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456;
                             (<<"from_user">>) -> 999
                         end},
            {'encode', 1, fun(123) -> <<"from_user">> end}
        ]},
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
            {'assemble_s2c', 3, fun(MsgId, <<"msg_not_found">>, <<"to_user">>) ->
                #{<<"id">> => MsgId, <<"type">> => <<"MSG_NOT_FOUND">>}
            end}
        ]},
        {msg_store_ds, [
            {'stage', 10, fun(_, _, _, _, _, _, _, _, _, _) -> ok end}
        ]}
    ], fun() ->
        MsgId = <<"msg_c2c_reply_missing_001">>,
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"payload">> => #{<<"content">> => <<"reply">>},
            <<"created_at">> => 1708768700000,
            <<"reply_to">> => #{
                <<"msg_id">> => <<"missing_reply_msg">>,
                <<"from_id">> => <<"from_user">>
            }
        },

        ok = msg_c2c_logic:c2c(MsgId, 123, Data),

        Reply = receive
            {reply, Msg} -> Msg
        after 1000 ->
            timeout
        end,

        ?assertNotEqual(timeout, Reply),
        ?assertEqual(<<"MSG_NOT_FOUND">>, maps:get(<<"type">>, Reply)),
        ?assertEqual(0, meck:num_calls(msg_store_ds, stage, 10))
    end).

c2c_client_ack_delegates_to_ack_logic_test_() ->
    ?WITH_MECKS([
        {msg_ack_logic, [
            {'client_ack', 4, fun(<<"c2c">>, <<"msg_ack_001">>, 123, <<"did_1">>) -> ok end}
        ]}
    ], fun() ->
        ok = msg_c2c_logic:c2c_client_ack(<<"msg_ack_001">>, 123, <<"did_1">>),
        ?assertEqual(1, meck:num_calls(msg_ack_logic, client_ack, 4))
    end).

extract_reply_info_without_reply_to_returns_empty_tuple_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertEqual({<<>>, 0, <<>>}, msg_c2c_logic:extract_reply_info(#{}))
    end).

extract_reply_info_with_json_payload_extracts_content_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 456 end}
        ]},
        {msg_c2c_repo, [
            {'find_msg_by_id', 1, fun(<<"origin_msg_001">>) ->
                {ok, #{<<"payload">> => <<"{\"content\":\"hello reply\"}">>}}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"reply_to">> => #{
                <<"msg_id">> => <<"origin_msg_001">>,
                <<"from_id">> => <<"from_user">>
            }
        },

        {ReplyToMsgId, ReplyToFromId, ReplySnippet} = msg_c2c_logic:extract_reply_info(Data),
        ?assertEqual(<<"origin_msg_001">>, ReplyToMsgId),
        ?assertEqual(456, ReplyToFromId),
        ?assertEqual(<<"hello reply">>, ReplySnippet)
    end).

extract_reply_info_with_non_json_payload_falls_back_to_raw_snippet_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"from_user">>) -> 456 end}
        ]},
        {msg_c2c_repo, [
            {'find_msg_by_id', 1, fun(<<"origin_msg_002">>) ->
                {ok, #{<<"payload">> => <<"plain payload text">>}}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"reply_to">> => #{
                <<"msg_id">> => <<"origin_msg_002">>,
                <<"from_id">> => <<"from_user">>
            }
        },

        {ReplyToMsgId, ReplyToFromId, ReplySnippet} = msg_c2c_logic:extract_reply_info(Data),
        ?assertEqual(<<"origin_msg_002">>, ReplyToMsgId),
        ?assertEqual(456, ReplyToFromId),
        ?assertEqual(<<"plain payload text">>, ReplySnippet)
    end).

c2c_revoke_success_online_sends_revoke_ack_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456;
                             (<<"from_user">>) -> 123
                         end}
        ]},
        {msg_c2c_ds, [
            {'find_msg_by_id', 1, fun(<<"orig_c2c_revoke_001">>) ->
                {ok, #{
                    <<"from_id">> => 123,
                    <<"created_at">> => 1700000000000
                }}
            end},
            {'revoke_offline_msg', 8, fun(_, _, _, _, _, _, _, _) -> ok end}
        ]},
        {elib_dt, [
            {'millisecond', 0, fun() -> 1700000060000 end},
            {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(456) -> true end}
        ]},
        {imboy_message_helper, [
            {'encode_and_send', 4, fun(456, <<"c2c_revoke_001">>, _Msg, <<"c2s">>) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2c_revoke_001">>}
        },

        {reply, Reply} = msg_c2c_logic:c2c_revoke(<<"c2c_revoke_001">>, 123, Data),
        ?assertEqual(<<"message_revoke_ack">>, maps:get(<<"action">>, Reply)),
        ?assertEqual(<<"custom">>, maps:get(<<"msg_type">>, Reply)),
        ?assertEqual(1, meck:num_calls(imboy_message_helper, encode_and_send, 4)),
        ?assertEqual(0, meck:num_calls(msg_c2c_ds, revoke_offline_msg, 8))
    end).

c2c_revoke_permission_denied_when_operator_not_sender_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456;
                             (<<"from_user">>) -> 789
                         end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(<<"c2c_revoke_denied_001">>, <<"permission_denied">>, <<"to_user">>) ->
                #{<<"id">> => <<"c2c_revoke_denied_001">>, <<"error">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{<<"original_msg_id">> => <<"orig_c2c_revoke_002">>}
        },

        {reply, Reply} = msg_c2c_logic:c2c_revoke(<<"c2c_revoke_denied_001">>, 123, Data),
        ?assertEqual(<<"permission_denied">>, maps:get(<<"error">>, Reply))
    end).

c2c_edit_success_online_sends_edit_ack_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456;
                             (<<"from_user">>) -> 123
                         end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-28T12:00:00Z">> end},
            {'millisecond', 0, fun() -> 1700000065000 end}
        ]},
        {user_logic, [
            {'is_online', 1, fun(456) -> true end}
        ]},
        {imboy_message_helper, [
            {'encode_and_send', 4, fun(456, <<"c2c_edit_001">>, _Msg, <<"c2s">>) -> ok end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
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
    end).

c2c_edit_permission_denied_when_operator_not_sender_test_() ->
    ?WITH_MECKS([
        {elib_log, [
            {'internal_log', 4, fun(_, _, _, _) -> ok end},
            {'internal_log', 5, fun(_, _, _, _, _) -> ok end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"to_user">>) -> 456;
                             (<<"from_user">>) -> 789
                         end}
        ]},
        {message_ds, [
            {'assemble_s2c', 3, fun(<<"c2c_edit_denied_001">>, <<"permission_denied">>, <<"to_user">>) ->
                #{<<"id">> => <<"c2c_edit_denied_001">>, <<"error">> => <<"permission_denied">>}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"to">> => <<"to_user">>,
            <<"from">> => <<"from_user">>,
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2c_edit_002">>,
                <<"content">> => <<"edited content">>,
                <<"msg_type">> => <<"text">>
            }
        },

        {reply, Reply} = msg_c2c_logic:c2c_edit(<<"c2c_edit_denied_001">>, 123, Data),
        ?assertEqual(<<"permission_denied">>, maps:get(<<"error">>, Reply))
    end).

c2c_revoke_ack_persists_action_payload_test_() ->
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
        {msg_c2c_repo, [
            {'update_payload_by_msg_id', 2, fun(<<"orig_c2c_revoke_003">>, PayloadJson) ->
                ?assert(is_binary(PayloadJson)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
        Data = #{
            <<"payload">> => #{
                <<"original_msg_id">> => <<"orig_c2c_revoke_003">>
            }
        },

        Result = msg_c2c_logic:c2c_revoke_ack(<<"c2c_revoke_ack_003">>, 123, Data),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_c2c_repo, update_payload_by_msg_id, 2))
    end).

c2c_edit_ack_persists_action_payload_test_() ->
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
        {msg_c2c_repo, [
            {'update_payload_by_msg_id', 2, fun(<<"orig_c2c_edit_003">>, PayloadJson) ->
                ?assert(is_binary(PayloadJson)),
                {ok, 1}
            end}
        ]}
    ], fun() ->
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
    end).
