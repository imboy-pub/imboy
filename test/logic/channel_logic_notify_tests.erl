-module(channel_logic_notify_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

notify_channel_subscribed_succeeds_when_send_ok_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], <<"channel_subscribed">>, <<>>, null, Payload, no_save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_subscribed(11, 1001),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_unread_count_sends_expected_payload_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], <<"channel_unread_count">>, <<>>, null, Payload, no_save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(8, maps:get(<<"unread_count">>, Payload)),
                ok
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_unread_count(11, 1001, 8),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_update_uses_subscribers_and_handles_unexpected_return_test_() ->
    Channel = #{<<"id">> => 11, <<"name">> => <<"channel-x">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_updated">>, <<>>, null, Payload, no_save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(Channel, maps:get(<<"channel">>, Payload)),
                unexpected_return
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_update(11, Channel),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(channel_ds, subscriber_uids, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_update_still_returns_ok_when_subscribers_invalid_test_() ->
    Channel = #{<<"id">> => 11, <<"name">> => <<"channel-x">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> {error, db_down} end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_when_subscribers_invalid)
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_update(11, Channel),
        ?assertEqual(ok, Result),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_update_still_returns_ok_when_subscriber_lookup_crashes_test_() ->
    Channel = #{<<"id">> => 11, <<"name">> => <<"channel-x">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> erlang:error(db_down) end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_when_subscriber_lookup_crashes)
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_update(11, Channel),
        ?assertEqual(ok, Result),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_unsubscribed_still_returns_ok_when_send_failed_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], <<"channel_unsubscribed">>, <<>>, null, Payload, no_save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                {error, notify_failed}
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_unsubscribed(11, 1001),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_invitation_created_still_returns_ok_when_send_crashes_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], <<"channel_invitation_created">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                erlang:error(mock_notify_crash)
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_invitation_created(11, 2002),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_order_paid_still_returns_ok_when_send_returns_unexpected_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], Action, <<>>, null, Payload, no_save) ->
                ?assert(lists:member(Action, [<<"channel_order_paid">>, <<"channel_subscribed">>])),
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                unexpected_return
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_order_paid(11, 2002),
        ?assertEqual(ok, Result),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_deleted_still_returns_ok_when_send_failed_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                {error, notify_failed}
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_deleted(11, [1001, 2002]),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_channel_deleted_filters_invalid_uids_and_still_sends_valid_ones_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_channel_deleted(11, [1001, <<"bad">>, -1, 2002]),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

broadcast_channel_message_uses_subscribers_and_still_returns_ok_when_send_failed_test_() ->
    Message = #{<<"id">> => <<"msg_1">>, <<"content">> => <<"hello">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(<<"CHANNEL">>, maps:get(<<"type">>, Payload)),
                ?assertEqual(<<"hello">>, maps:get(<<"content">>, Payload)),
                {error, notify_failed}
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:broadcast_channel_message(11, Message),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(channel_ds, subscriber_uids, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

broadcast_channel_message_filters_invalid_subscribers_and_sends_valid_ones_test_() ->
    Message = #{<<"id">> => <<"msg_1">>, <<"content">> => <<"hello">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, <<"bad">>, 0, 2002] end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(<<"CHANNEL">>, maps:get(<<"type">>, Payload)),
                ?assertEqual(<<"hello">>, maps:get(<<"content">>, Payload)),
                ok
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:broadcast_channel_message(11, Message),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

broadcast_channel_message_still_returns_ok_when_subscribers_invalid_test_() ->
    Message = #{<<"id">> => <<"msg_1">>, <<"content">> => <<"hello">>},
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> {error, db_down} end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(11) -> <<"ch_hash_11">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_when_subscribers_invalid)
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:broadcast_channel_message(11, Message),
        ?assertEqual(ok, Result),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_message_deleted_uses_subscribers_and_still_returns_ok_when_send_failed_test_() ->
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(Value) ->
                case Value of
                    11 -> <<"ch_hash_11">>;
                    99 -> <<"msg_hash_99">>
                end
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_deleted">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(<<"msg_hash_99">>, maps:get(<<"message_id">>, Payload)),
                {error, notify_failed}
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_message_deleted(11, 99),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(channel_ds, subscriber_uids, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_message_revoked_still_returns_ok_when_send_crashes_test_() ->
    RevokedAt = <<"2026-02-24T10:00:00Z">>,
    ?WITH_MECKS([
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {elib_hashids, [
            {'encode', 1, fun(Value) ->
                case Value of
                    11 -> <<"ch_hash_11">>;
                    99 -> <<"msg_hash_99">>;
                    1001 -> <<"uid_hash_1001">>
                end
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_revoked">>, <<>>, null, Payload, save) ->
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(<<"msg_hash_99">>, maps:get(<<"message_id">>, Payload)),
                ?assertEqual(<<"uid_hash_1001">>, maps:get(<<"revoked_by">>, Payload)),
                ?assertEqual(RevokedAt, maps:get(<<"revoked_at">>, Payload)),
                erlang:error(mock_notify_crash)
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_message_revoked(11, 99, 1001, RevokedAt),
        ?assertEqual(ok, Result),
        ?assertEqual(1, meck:num_calls(channel_ds, subscriber_uids, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_invitation_accepted_still_returns_ok_when_second_send_fails_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(Value) ->
                case Value of
                    11 -> <<"ch_hash_11">>;
                    2002 -> <<"uid_hash_2002">>
                end
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, Uids, Action, <<>>, null, Payload, no_save) ->
                case {Uids, Action} of
                    {[1001], <<"channel_invitation_accepted">>} ->
                        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                        ?assertEqual(<<"uid_hash_2002">>, maps:get(<<"invitee_uid">>, Payload)),
                        ok;
                    {[2002], <<"channel_subscribed">>} ->
                        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                        {error, notify_failed}
                end
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_invitation_accepted(11, 1001, 2002),
        ?assertEqual(ok, Result),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
    end).

notify_invitation_accepted_still_returns_ok_when_first_send_crashes_test_() ->
    ?WITH_MECKS([
        {elib_hashids, [
            {'encode', 1, fun(Value) ->
                case Value of
                    11 -> <<"ch_hash_11">>;
                    2002 -> <<"uid_hash_2002">>
                end
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, Uids, Action, <<>>, null, Payload, no_save) ->
                case {Uids, Action} of
                    {[1001], <<"channel_invitation_accepted">>} ->
                        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                        erlang:error(mock_notify_crash);
                    {[2002], <<"channel_subscribed">>} ->
                        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
                        ok
                end
            end}
        ]}
    ], fun() ->
        Result = channel_logic_notify:notify_invitation_accepted(11, 1001, 2002),
        ?assertEqual(ok, Result),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
    end).
