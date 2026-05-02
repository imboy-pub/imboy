-module(channel_logic_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% channel_logic 模块的 EUnit 测试
%%%
%%% 目标：验证频道发送消息核心路径
%%% 覆盖：发送成功、权限校验、存储失败
%%%===================================================================

%% ===================================================================
%% publish_message/5 测试
%% ===================================================================

publish_message_with_admin_role_succeeds_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'publish_message', 5, fun(11, 1001, <<"hello"/utf8>>, <<"text">>, #{<<"ext">> := <<"1">>}) ->
                {ok, 99}
            end},
            {'subscriber_uids', 1, fun(11) -> [2001, 2002] end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(99) ->
                #{
                    <<"id">> => 99,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"content">> => <<"hello"/utf8>>,
                    <<"msg_type">> => <<"text">>,
                    <<"payload">> => <<"{\"ext\":\"1\"}">>
                }
            end}
        ]},
        {channel_subscription_repo, [
            {'list_unread_counts_by_channel', 1, fun(11) -> {ok, []} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2001, 2002], <<"channel_message">>, <<>>, null, BroadcastPayload, save) ->
                ?assertEqual(11, maps:get(<<"channel_id">>, BroadcastPayload)),
                ?assertEqual(<<"CHANNEL">>, maps:get(<<"type">>, BroadcastPayload)),
                ?assertEqual(99, maps:get(<<"id">>, BroadcastPayload)),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    ChannelIdBin,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{<<"ext">> => <<"1">>}
                ),

                ?assertMatch({ok, _}, Result),
                {ok, Message} = Result,
                ?assertEqual(99, maps:get(<<"id">>, Message)),
                ?assertEqual(1001, maps:get(<<"author_id">>, Message)),
                ?assertEqual(<<"hello"/utf8>>, maps:get(<<"content">>, Message))
            end)
        end
    }.

publish_message_with_admin_role_still_returns_ok_when_broadcast_crashes_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'publish_message', 5, fun(11, 1001, <<"hello"/utf8>>, <<"text">>, #{}) ->
                {ok, 109}
            end},
            {'subscriber_uids', 1, fun(11) -> [2001, 2002] end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(109) ->
                #{
                    <<"id">> => 109,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"content">> => <<"hello"/utf8>>,
                    <<"msg_type">> => <<"text">>,
                    <<"payload">> => <<"{}">>
                }
            end}
        ]},
        {channel_subscription_repo, [
            {'list_unread_counts_by_channel', 1, fun(11) -> {ok, []} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2001, 2002], <<"channel_message">>, <<>>, null, BroadcastPayload, save) ->
                ?assertEqual(11, maps:get(<<"channel_id">>, BroadcastPayload)),
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    ChannelIdBin,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{}
                ),

                ?assertMatch({ok, _}, Result),
                {ok, Message} = Result,
                ?assertEqual(109, maps:get(<<"id">>, Message)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

publish_message_with_non_admin_role_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 2002}
            end},
            {'publish_message', 5, fun(_, _, _, _, _) ->
                erlang:error(should_not_call_publish_message)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    ChannelIdBin,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{}
                ),

                ?assertEqual({error, <<"只有管理员可以发布消息"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_ds, publish_message, 5))
            end)
        end
    }.

publish_message_when_storage_fails_returns_error_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 1 end}
        ]},
        {channel_ds, [
            {'publish_message', 5, fun(11, 1001, <<"hello"/utf8>>, <<"text">>, #{}) ->
                {error, <<"db_error">>}
            end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(_) -> erlang:error(should_not_call_find_by_id) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_broadcast) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    ChannelIdBin,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{}
                ),

                ?assertEqual({error, <<"db_error">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

publish_message_returns_error_when_loading_new_message_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'publish_message', 5, fun(11, 1001, <<"hello"/utf8>>, <<"text">>, #{}) ->
                {ok, 99}
            end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(99) -> {error, db_down} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_broadcast_when_load_failed) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    ChannelIdBin,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{}
                ),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

publish_message_accepts_custom_id_fallback_test_() ->
    MockConfigs = [
        {channel_ds, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{<<"id">> => 11}
            end},
            {'publish_message', 5, fun(11, 1001, <<"hello"/utf8>>, <<"text">>, #{}) ->
                {ok, 99}
            end},
            {'subscriber_uids', 1, fun(11) -> [2001, 2002] end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(99) ->
                #{
                    <<"id">> => 99,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"content">> => <<"hello"/utf8>>,
                    <<"msg_type">> => <<"text">>,
                    <<"payload">> => <<"{}">>
                }
            end}
        ]},
        {channel_subscription_repo, [
            {'list_unread_counts_by_channel', 1, fun(11) -> {ok, []} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:publish_message(
                    1001,
                    <<"tech_daily">>,
                    <<"hello"/utf8>>,
                    <<"text">>,
                    #{}
                ),

                ?assertMatch({ok, _}, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, find_by_custom_id, 1))
            end)
        end
    }.

get_channel_by_custom_id_includes_role_for_admin_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{
                    <<"id">> => 11,
                    <<"name">> => <<"Tech Daily">>,
                    <<"creator_uid">> => 1001
                }
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) -> erlang:error(should_not_check_subscription) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_by_custom_id(<<"tech_daily">>, 1001),

                ?assertMatch({ok, _}, Result),
                {ok, Channel} = Result,
                ?assertEqual(2, maps:get(user_role, Channel)),
                ?assertEqual(true, maps:get(is_subscribed, Channel)),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, is_subscribed, 2))
            end)
        end
    }.

get_channel_by_custom_id_checks_subscription_for_non_admin_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{
                    <<"id">> => 11,
                    <<"name">> => <<"Tech Daily">>,
                    <<"creator_uid">> => 1001
                }
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 1001}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(11, 2002) -> true end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_by_custom_id(<<"tech_daily">>, 2002),

                ?assertMatch({ok, _}, Result),
                {ok, Channel} = Result,
                ?assertEqual(0, maps:get(user_role, Channel)),
                ?assertEqual(true, maps:get(is_subscribed, Channel)),
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, is_subscribed, 2))
            end)
        end
    }.

get_channel_by_custom_id_checks_subscription_when_admin_role_unexpected_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{
                    <<"id">> => 11,
                    <<"name">> => <<"Tech Daily">>,
                    <<"creator_uid">> => 3003
                }
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 3003}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 2002) -> {error, db_down} end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(11, 2002) -> false end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_by_custom_id(<<"tech_daily">>, 2002),
                ?assertMatch({ok, _}, Result),
                {ok, Channel} = Result,
                ?assertEqual(0, maps:get(user_role, Channel)),
                ?assertEqual(false, maps:get(is_subscribed, Channel)),
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, is_subscribed, 2))
            end)
        end
    }.

get_channel_returns_error_when_repo_returns_non_map_payload_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> [] end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_channel_payload_invalid)
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel(ChannelIdBin, 1001),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2))
            end)
        end
    }.

get_channel_by_custom_id_returns_error_when_repo_returns_non_map_payload_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) -> [] end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_custom_channel_payload_invalid)
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_custom_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_by_custom_id(<<"tech_daily">>, 1001),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2))
            end)
        end
    }.

get_channel_by_custom_id_returns_error_when_channel_id_invalid_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{
                    <<"id">> => <<"11">>,
                    <<"name">> => <<"Tech Daily">>,
                    <<"creator_uid">> => 1001
                }
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_channel_id_invalid)
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_channel_id_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_by_custom_id(<<"tech_daily">>, 1001),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2))
            end)
        end
    }.

update_channel_success_still_returns_ok_when_notify_crashes_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'update', 2, fun(11, Data) ->
                ?assertEqual(<<"Channel X">>, maps:get(<<"name">>, Data)),
                ?assert(maps:is_key(updated_at, Data)),
                {ok, 1}
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{
                    <<"id">> => 11,
                    <<"creator_uid">> => 1001,
                    <<"name">> => <<"Channel X">>,
                    <<"description">> => <<"desc">>
                }
            end},
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_updated">>, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_channel(1001, ChannelIdBin, #{
                    <<"name">> => <<"Channel X">>,
                    <<"ignored">> => <<"noop">>
                }),
                ?assertMatch({ok, _}, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, update, 2)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

update_channel_returns_permission_denied_when_admin_role_unexpected_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> {error, db_down} end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 3003}
            end},
            {'update', 2, fun(_, _) ->
                erlang:error(should_not_update_when_permission_denied)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_channel(1001, ChannelIdBin, #{
                    <<"name">> => <<"Channel X">>
                }),
                ?assertEqual({error, <<"无权限操作"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, update, 2))
            end)
        end
    }.

update_channel_success_still_returns_ok_when_notify_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'update', 2, fun(11, Data) ->
                ?assertEqual(<<"Channel X">>, maps:get(<<"name">>, Data)),
                ?assert(maps:is_key(updated_at, Data)),
                {ok, 1}
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{
                    <<"id">> => 11,
                    <<"creator_uid">> => 1001,
                    <<"name">> => <<"Channel X">>,
                    <<"description">> => <<"desc">>
                }
            end},
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_updated">>, <<>>, null, _Payload, no_save) ->
                {error, notify_failed}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_channel(1001, ChannelIdBin, #{
                    <<"name">> => <<"Channel X">>,
                    <<"ignored">> => <<"noop">>
                }),
                ?assertMatch({ok, _}, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, update, 2)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

update_channel_returns_error_when_reload_payload_not_map_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_repo, [
            {'update', 2, fun(11, _) -> {ok, 1} end},
            {'find_by_id', 2, fun(11, <<"*">>) -> invalid_payload end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_update', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_reload_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_channel(1001, ChannelIdBin, #{
                    <<"name">> => <<"Channel X">>
                }),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, update, 2)),
                ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_update, 2))
            end)
        end
    }.

delete_channel_success_still_returns_ok_when_notify_crashes_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end},
            {'delete', 1, fun(11) -> {ok, 1} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, _Payload, save) ->
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_channel(1001, ChannelIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, delete, 1)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_channel_success_still_returns_ok_when_notify_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end},
            {'delete', 1, fun(11) -> {ok, 1} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, _Payload, save) ->
                {error, notify_failed}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_channel(1001, ChannelIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, delete, 1)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_channel_still_returns_ok_when_subscriber_lookup_crashes_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> erlang:error(mock_subscriber_lookup_crash) end},
            {'delete', 1, fun(11) -> {ok, 1} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_when_subscriber_lookup_crashes)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_channel(1001, ChannelIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

get_messages_paid_channel_requires_purchase_or_subscription_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 11, <<"creator_uid">> => 1001}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(11, 2002) -> false end}
        ]},
        {channel_subscribe_ds, [
            {'has_purchased', 2, fun(11, 2002) -> false end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(_, _, _) ->
                erlang:error(should_not_load_messages_without_access)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(2002, ChannelIdBin, 0, 20),

                ?assertEqual({error, <<"付费频道需要先购买"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

get_messages_paid_channel_allows_purchased_user_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 11, <<"creator_uid">> => 1001}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(11, 2002) -> false end}
        ]},
        {channel_subscribe_ds, [
            {'has_purchased', 2, fun(11, 2002) -> true end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(11, 0, 20) ->
                {ok, [
                    #{
                        <<"id">> => 99,
                        <<"channel_id">> => 11,
                        <<"author_id">> => 1001,
                        <<"content">> => <<"paid content"/utf8>>
                    }
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(2002, ChannelIdBin, 0, 20),

                ?assertMatch({ok, [_]}, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

get_messages_returns_error_when_repo_query_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(11, 0, 20) -> {error, db_down} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(1001, ChannelIdBin, 0, 20),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

get_messages_returns_error_when_repo_payload_not_list_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(11, 0, 20) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(1001, ChannelIdBin, 0, 20),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_messages_filters_non_map_entries_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(11, 0, 20) ->
                {ok, [
                    #{
                        <<"id">> => 99,
                        <<"channel_id">> => 11,
                        <<"author_id">> => 1001,
                        <<"content">> => <<"hello"/utf8>>
                    },
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(1001, ChannelIdBin, 0, 20),
                ?assertMatch({ok, [_]}, Result),
                {ok, Messages} = Result,
                ?assertEqual(1, length(Messages))
            end)
        end
    }.

get_messages_returns_error_when_channel_payload_invalid_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> [] end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(_, _, _) ->
                erlang:error(should_not_query_messages_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(1001, ChannelIdBin, 0, 20),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

pin_message_returns_error_when_reload_after_update_fails_test_() ->
    MessageIdBin = integer_to_binary(99),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(99) ->
                Calls = case erlang:get(pin_find_call_count) of
                    undefined -> 0;
                    V -> V
                end,
                erlang:put(pin_find_call_count, Calls + 1),
                case Calls of
                    0 -> #{<<"id">> => 99, <<"channel_id">> => 11, <<"author_id">> => 1001};
                    _ -> {error, db_down}
                end
            end},
            {'update', 2, fun(99, _) -> {ok, 1} end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 2 end}
        ]}
    ],
    {setup,
        fun() ->
            erlang:erase(pin_find_call_count),
            setup_mocks(MockConfigs)
        end,
        fun(_) ->
            erlang:erase(pin_find_call_count),
            cleanup_mocks(MockConfigs)
        end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pin_message(1001, MessageIdBin, true),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(2, meck:num_calls(channel_message_repo, find_by_id, 1))
            end)
        end
    }.

pin_message_returns_error_when_repo_returns_non_map_payload_test_() ->
    MessageIdBin = integer_to_binary(198),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(198) -> [] end},
            {'update', 2, fun(_, _) -> erlang:error(should_not_update_when_message_invalid) end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_message_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pin_message(1001, MessageIdBin, true),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, update, 2)),
                ?assertEqual(0, meck:num_calls(channel_logic_common, get_user_role, 2))
            end)
        end
    }.

pin_message_returns_error_when_message_id_decode_unexpected_test_() ->
    MessageIdBin = <<"msg_hash_unexpected">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_query_message_when_message_id_decode_unexpected)
            end},
            {'update', 2, fun(_, _) ->
                erlang:error(should_not_update_message_when_message_id_decode_unexpected)
            end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_message_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pin_message(1001, MessageIdBin, true),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(channel_message_repo, update, 2)),
                ?assertEqual(0, meck:num_calls(channel_logic_common, get_user_role, 2))
            end)
        end
    }.

pin_message_returns_error_when_channel_id_type_invalid_test_() ->
    MessageIdBin = integer_to_binary(199),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(199) ->
                #{
                    <<"id">> => 199,
                    <<"channel_id">> => <<"11">>,
                    <<"author_id">> => 1001
                }
            end},
            {'update', 2, fun(_, _) -> erlang:error(should_not_update_when_channel_id_invalid) end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_channel_id_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pin_message(1001, MessageIdBin, true),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, update, 2)),
                ?assertEqual(0, meck:num_calls(channel_logic_common, get_user_role, 2))
            end)
        end
    }.

delete_message_author_success_still_returns_ok_when_notify_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MessageIdBin = integer_to_binary(99),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(99) ->
                #{
                    <<"id">> => 99,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001
                }
            end},
            {'delete', 1, fun(99) -> {ok, 1} end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_deleted">>, <<>>, null, Payload, save) ->
                ?assertEqual(ChannelIdBin, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(MessageIdBin, maps:get(<<"message_id">>, Payload)),
                {error, notify_failed}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_admin_success_still_returns_ok_when_notify_crashes_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MessageIdBin = integer_to_binary(100),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(100) ->
                #{
                    <<"id">> => 100,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 2002
                }
            end},
            {'delete', 1, fun(100) -> {ok, 1} end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_deleted">>, <<>>, null, Payload, save) ->
                ?assertEqual(ChannelIdBin, maps:get(<<"channel_id">>, Payload)),
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_permission_denied_when_not_author_or_admin_test_() ->
    MessageIdBin = integer_to_binary(101),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(101) ->
                #{
                    <<"id">> => 101,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 2002
                }
            end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete_without_permission) end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) -> erlang:error(should_not_load_subscribers_without_permission) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_without_permission)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"无权限删除此消息"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_error_when_message_not_found_test_() ->
    MessageIdBin = integer_to_binary(404),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(404) -> {error, not_found} end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete_when_message_missing) end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) ->
                erlang:error(should_not_load_subscribers_when_message_missing)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_message_missing)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_repo_error_as_binary_test_() ->
    MessageIdBin = integer_to_binary(102),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(102) ->
                #{
                    <<"id">> => 102,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001
                }
            end},
            {'delete', 1, fun(102) -> {error, db_down} end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) ->
                erlang:error(should_not_load_subscribers_when_delete_failed)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_delete_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_error_when_repo_returns_non_map_payload_test_() ->
    MessageIdBin = integer_to_binary(103),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(103) -> [] end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete_when_message_invalid) end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) ->
                erlang:error(should_not_load_subscribers_when_message_invalid)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_message_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_error_when_required_fields_missing_test_() ->
    MessageIdBin = integer_to_binary(104),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(104) ->
                #{
                    <<"id">> => 104,
                    <<"channel_id">> => 11
                }
            end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete_when_required_fields_missing) end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_required_fields_missing)
            end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) ->
                erlang:error(should_not_load_subscribers_when_required_fields_missing)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_required_fields_missing)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

delete_message_returns_error_when_required_fields_type_invalid_test_() ->
    MessageIdBin = integer_to_binary(105),
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(105) ->
                #{
                    <<"id">> => 105,
                    <<"channel_id">> => <<"11">>,
                    <<"author_id">> => <<"1001">>
                }
            end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete_when_fields_type_invalid) end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_fields_type_invalid)
            end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(_) ->
                erlang:error(should_not_load_subscribers_when_fields_type_invalid)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_fields_type_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:delete_message(1001, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1)),
                ?assertEqual(0, meck:num_calls(channel_logic_common, get_user_role, 2))
            end)
        end
    }.

create_invitation_success_notifies_invitee_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) -> {ok, 501} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(501) ->
                {ok, #{
                    <<"id">> => 501,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 2002
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], <<"channel_invitation_created">>, <<>>, null, Payload, save) ->
                ?assertEqual(11, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertMatch({ok, _}, Result),
                {ok, Invitation} = Result,
                ?assertEqual(11, maps:get(<<"channel_id">>, Invitation)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

create_invitation_rejects_non_private_channel_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_create_invitation) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"只有私有频道支持邀请功能"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3))
            end)
        end
    }.

create_invitation_returns_error_when_channel_not_found_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                {error, not_found}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_create_invitation_when_channel_missing) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3))
            end)
        end
    }.

create_invitation_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                erlang:error(should_not_lookup_channel_when_channel_id_decode_unexpected)
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) ->
                erlang:error(should_not_create_invitation_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, find_by_id, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3))
            end)
        end
    }.

create_invitation_returns_error_when_channel_payload_invalid_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> [] end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) ->
                erlang:error(should_not_create_invitation_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3))
            end)
        end
    }.

create_invitation_rejects_disabled_channel_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 0}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_create_invitation_when_channel_disabled) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"频道已禁用或删除"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3))
            end)
        end
    }.

create_invitation_ds_binary_error_passthrough_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) ->
                {error, <<"邀请创建失败"/utf8>>}
            end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_create_failed)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notify_when_create_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"邀请创建失败"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

create_invitation_ds_atom_error_converted_to_binary_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) ->
                {error, db_timeout}
            end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_create_failed)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notify_when_create_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"db_timeout">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

create_invitation_ds_unexpected_result_converted_to_binary_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) ->
                unexpected_result
            end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_create_returns_unexpected)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_create_returns_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

create_invitation_returns_error_when_loading_created_invitation_fails_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) -> {ok, 501} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(501) -> {error, not_found} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notify_when_invitation_load_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"not_found">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

create_invitation_returns_error_when_loading_created_invitation_payload_not_map_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) -> {ok, 501} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(501) -> {ok, invalid_payload} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notify_when_invitation_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_invitation(1001, ChannelIdBin, 2002),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

get_my_invitations_success_transfers_ids_test_() ->
    ChannelIdBin = integer_to_binary(11),
    InvitationIdBin = integer_to_binary(501),
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_pending_by_invitee', 1, fun(2002) ->
                {ok, [#{
                    <<"id">> => 501,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 2002
                }]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_invitations(2002),

                ?assertMatch({ok, [_]}, Result),
                {ok, [Invitation]} = Result,
                ?assertEqual(501, maps:get(<<"id">>, Invitation)),
                ?assertEqual(11, maps:get(<<"channel_id">>, Invitation)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, list_pending_by_invitee, 1))
            end)
        end
    }.

get_my_invitations_returns_error_on_repo_error_test_() ->
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_pending_by_invitee', 1, fun(2002) ->
                {error, db_down}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_invitations(2002),

                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, list_pending_by_invitee, 1))
            end)
        end
    }.

get_my_invitations_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_pending_by_invitee', 1, fun(2002) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_invitations(2002),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_my_invitations_filters_non_map_entries_test_() ->
    ChannelIdBin = integer_to_binary(11),
    InvitationIdBin = integer_to_binary(501),
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_pending_by_invitee', 1, fun(2002) ->
                {ok, [
                    #{
                        <<"id">> => 501,
                        <<"channel_id">> => 11,
                        <<"inviter_uid">> => 1001,
                        <<"invitee_uid">> => 2002
                    },
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_invitations(2002),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Invitation]} = Result,
                ?assertEqual(501, maps:get(<<"id">>, Invitation)),
                ?assertEqual(11, maps:get(<<"channel_id">>, Invitation))
            end)
        end
    }.

get_sent_invitations_returns_error_on_repo_error_test_() ->
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_by_inviter', 2, fun(1001, 50) ->
                {error, db_down}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_sent_invitations(1001),

                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, list_by_inviter, 2))
            end)
        end
    }.

get_sent_invitations_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_by_inviter', 2, fun(1001, 50) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_sent_invitations(1001),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_sent_invitations_success_transfers_ids_test_() ->
    ChannelIdBin = integer_to_binary(11),
    InvitationIdBin = integer_to_binary(701),
    MockConfigs = [
        {channel_invitation_repo, [
            {'list_by_inviter', 2, fun(1001, 50) ->
                {ok, [#{
                    <<"id">> => 701,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 3003
                }]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_sent_invitations(1001),

                ?assertMatch({ok, [_]}, Result),
                {ok, [Invitation]} = Result,
                ?assertEqual(701, maps:get(<<"id">>, Invitation)),
                ?assertEqual(11, maps:get(<<"channel_id">>, Invitation)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, list_by_inviter, 2))
            end)
        end
    }.

accept_invitation_already_accepted_is_idempotent_and_silent_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(501, 2002) -> {error, already_accepted} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_already_accepted)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_accept_notification_when_already_accepted)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 501),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_success_notifies_inviter_and_invitee_test_() ->
    ChannelIdBin = integer_to_binary(11),
    InviteeUidBin = integer_to_binary(2002),
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(501, 2002) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(501) ->
                {ok, #{
                    <<"id">> => 501,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 2002
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, Uids, Action, <<>>, null, Payload, no_save) ->
                case {Uids, Action} of
                    {[1001], <<"channel_invitation_accepted">>} ->
                        ?assertEqual(11, maps:get(<<"channel_id">>, Payload)),
                        ?assertEqual(2002, maps:get(<<"invitee_uid">>, Payload)),
                        ok;
                    {[2002], <<"channel_subscribed">>} ->
                        ?assertEqual(11, maps:get(<<"channel_id">>, Payload)),
                        ok;
                    _ ->
                        erlang:error({unexpected_notify, Uids, Action, Payload})
                end
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 501),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_success_still_returns_ok_when_notify_fails_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(511, 2002) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(511) ->
                {ok, #{
                    <<"id">> => 511,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 2002
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, _Uids, _Action, <<>>, null, _Payload, no_save) ->
                {error, notify_failed}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 511),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_success_still_returns_ok_when_notify_crashes_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(512, 2002) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(512) ->
                {ok, #{
                    <<"id">> => 512,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 1001,
                    <<"invitee_uid">> => 2002
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, _Uids, _Action, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 512),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_when_invitation_load_fails_still_returns_ok_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(502, 2002) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(502) -> {error, not_found} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_accept_notification_when_invitation_missing)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 502),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_with_invalid_invitation_payload_still_returns_ok_without_notify_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(505, 2002) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(505) ->
                {ok, #{
                    <<"id">> => 505,
                    <<"channel_id">> => <<"11">>,
                    <<"inviter_uid">> => <<"1001">>
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_invitation_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 505),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_binary_error_passthrough_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(503, 2002) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_accept_failed)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_accept_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 503),

                ?assertEqual({error, <<"邀请不存在或已过期"/utf8>>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_atom_error_is_converted_to_binary_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(504, 2002) -> {error, db_timeout} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_accept_failed)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_accept_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 504),

                ?assertEqual({error, <<"db_timeout">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

accept_invitation_unexpected_result_converted_to_binary_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(506, 2002) -> unexpected_result end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) ->
                erlang:error(should_not_load_invitation_when_accept_returns_unexpected)
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_accept_returns_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:accept_invitation(2002, 506),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
                ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

reject_invitation_atom_error_is_converted_to_binary_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'reject_invitation', 2, fun(501, 2002) -> {error, db_timeout} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:reject_invitation(2002, 501),

                ?assertEqual({error, <<"db_timeout">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, reject_invitation, 2))
            end)
        end
    }.

reject_invitation_success_returns_ok_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'reject_invitation', 2, fun(502, 2002) -> ok end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notification_on_reject)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:reject_invitation(2002, 502),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, reject_invitation, 2)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

reject_invitation_binary_error_passthrough_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'reject_invitation', 2, fun(503, 2002) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:reject_invitation(2002, 503),

                ?assertEqual({error, <<"邀请不存在或已过期"/utf8>>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, reject_invitation, 2))
            end)
        end
    }.

reject_invitation_unexpected_return_converted_to_binary_test_() ->
    MockConfigs = [
        {channel_subscribe_ds, [
            {'reject_invitation', 2, fun(504, 2002) -> unexpected_result end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:reject_invitation(2002, 504),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, reject_invitation, 2))
            end)
        end
    }.

create_order_success_returns_transferred_order_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 2002, #{}) -> {ok, <<"ORD001">>} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"id">> => 601,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"order_no">> => <<"ORD001">>
                }}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),

                ?assertMatch({ok, _}, Result),
                {ok, Order} = Result,
                ?assertEqual(11, maps:get(<<"channel_id">>, Order)),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_order, 3))
            end)
        end
    }.

create_order_rejects_non_paid_channel_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_create_order) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),

                ?assertEqual({error, <<"只有付费频道支持购买"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3))
            end)
        end
    }.

create_order_propagates_channel_lookup_error_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                {error, db_down}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_create_order_when_channel_lookup_failed) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),

                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3))
            end)
        end
    }.

create_order_returns_error_when_channel_payload_invalid_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> [] end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) ->
                erlang:error(should_not_create_order_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3))
            end)
        end
    }.

create_order_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                erlang:error(should_not_lookup_channel_when_channel_id_decode_unexpected)
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) ->
                erlang:error(should_not_create_order_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, find_by_id, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3))
            end)
        end
    }.

create_order_returns_not_found_when_order_reload_returns_non_map_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 2002, #{}) -> {ok, <<"ORD_NEW_BAD_PAYLOAD">>} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_NEW_BAD_PAYLOAD">>) -> {ok, []} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result)
            end)
        end
    }.

create_order_unexpected_ds_result_converted_to_binary_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 2002, #{}) -> unexpected_result end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) ->
                erlang:error(should_not_load_order_when_create_order_returns_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_order(2002, ChannelIdBin),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
            end)
        end
    }.

pay_order_requires_owner_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD001">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 3003
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) -> erlang:error(should_not_pay_order) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD001">>),

                ?assertEqual({error, <<"无权操作此订单"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
            end)
        end
    }.

pay_order_propagates_lookup_error_before_owner_check_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_DB_ERR">>) ->
                {error, db_down}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) -> erlang:error(should_not_pay_order_when_lookup_failed) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD_DB_ERR">>),

                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
            end)
        end
    }.

pay_order_returns_not_found_when_repo_returns_non_map_payload_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_BAD_PAYLOAD">>) -> {ok, []} end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) ->
                erlang:error(should_not_pay_order_when_order_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD_BAD_PAYLOAD">>),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
            end)
        end
    }.

pay_order_returns_not_found_when_required_fields_invalid_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_BAD_FIELDS">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD_BAD_FIELDS">>,
                    <<"channel_id">> => <<"11">>,
                    <<"user_id">> => <<"2002">>
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) ->
                erlang:error(should_not_pay_order_when_required_fields_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD_BAD_FIELDS">>),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
            end)
        end
    }.

pay_order_success_sends_paid_and_subscribed_notifications_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD001">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD001">>, PaymentData) ->
                ?assertEqual(<<"mock">>, maps:get(payment_method, PaymentData)),
                PaymentNo = maps:get(payment_no, PaymentData),
                ?assert(is_binary(PaymentNo)),
                ?assertEqual(<<"MOCK_">>, binary:part(PaymentNo, 0, 5)),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], Action, <<>>, null, Payload, no_save) ->
                ?assert(lists:member(Action, [<<"channel_order_paid">>, <<"channel_subscribed">>])),
                ?assertEqual(11, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD001">>),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

pay_order_success_still_returns_ok_when_notify_fails_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001_NOTIFY_ERR">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD001_NOTIFY_ERR">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD001_NOTIFY_ERR">>, _PaymentData) ->
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], Action, <<>>, null, _Payload, no_save) ->
                case Action of
                    <<"channel_order_paid">> -> {error, notify_failed};
                    <<"channel_subscribed">> -> {error, notify_failed}
                end
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD001_NOTIFY_ERR">>),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

pay_order_success_still_returns_ok_when_notify_crashes_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001_NOTIFY_CRASH">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD001_NOTIFY_CRASH">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD001_NOTIFY_CRASH">>, _PaymentData) ->
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [2002], _Action, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD001_NOTIFY_CRASH">>),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
                ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

pay_order_already_paid_does_not_send_duplicate_notifications_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD002">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD002">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD002">>, _PaymentData) ->
                {error, already_paid}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_duplicate_paid_notifications)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD002">>),

                ?assertEqual({error, <<"订单已支付"/utf8>>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

pay_order_not_found_or_expired_returns_readable_error_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD009">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD009">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD009">>, _PaymentData) ->
                {error, not_found_or_expired}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_send_notifications_when_order_expired)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD009">>),

                ?assertEqual({error, <<"订单不存在或已过期"/utf8>>}, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

pay_order_unexpected_ds_result_converted_to_binary_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_UNEXPECTED_DS">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD_UNEXPECTED_DS">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD_UNEXPECTED_DS">>, _PaymentData) ->
                unexpected_result
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_notify_when_pay_order_returns_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:pay_order(2002, <<"ORD_UNEXPECTED_DS">>),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

get_order_returns_transferred_order_for_owner_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD010">>) ->
                {ok, #{
                    <<"id">> => 701,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 2002,
                    <<"order_no">> => <<"ORD010">>
                }}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD010">>),

                ?assertMatch({ok, _}, Result),
                {ok, Order} = Result,
                ?assertEqual(11, maps:get(<<"channel_id">>, Order))
            end)
        end
    }.

get_order_rejects_non_owner_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD011">>) ->
                {ok, #{
                    <<"id">> => 702,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 3003,
                    <<"order_no">> => <<"ORD011">>
                }}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD011">>),
                ?assertEqual({error, <<"无权查看此订单"/utf8>>}, Result)
            end)
        end
    }.

get_order_returns_not_found_message_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD404">>) ->
                {error, not_found}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD404">>),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result)
            end)
        end
    }.

get_order_propagates_lookup_error_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_DB_ERR">>) ->
                {error, db_down}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD_DB_ERR">>),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

get_order_returns_error_when_repo_returns_unexpected_term_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_UNEXPECTED_LOOKUP">>) ->
                unexpected_lookup
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD_UNEXPECTED_LOOKUP">>),
                ?assertEqual({error, <<"unexpected_lookup">>}, Result)
            end)
        end
    }.

get_order_returns_not_found_when_repo_returns_non_map_payload_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_BAD_PAYLOAD">>) -> {ok, []} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD_BAD_PAYLOAD">>),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result)
            end)
        end
    }.

get_order_returns_not_found_when_required_fields_invalid_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_BAD_FIELDS">>) ->
                {ok, #{
                    <<"id">> => 799,
                    <<"channel_id">> => 11,
                    <<"user_id">> => <<"2002">>,
                    <<"order_no">> => <<"ORD_BAD_FIELDS">>
                }}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_order(2002, <<"ORD_BAD_FIELDS">>),
                ?assertEqual({error, <<"订单不存在"/utf8>>}, Result)
            end)
        end
    }.

get_my_orders_returns_transferred_orders_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_order_repo, [
            {'list_by_user', 2, fun(2002, 50) ->
                {ok, [
                    #{
                        <<"id">> => 801,
                        <<"channel_id">> => 11,
                        <<"user_id">> => 2002,
                        <<"order_no">> => <<"ORD_LIST_1">>
                    }
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_orders(2002),

                ?assertMatch({ok, [_]}, Result),
                {ok, [Order]} = Result,
                ?assertEqual(11, maps:get(<<"channel_id">>, Order))
            end)
        end
    }.

get_my_orders_filters_non_map_entries_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_order_repo, [
            {'list_by_user', 2, fun(2002, 50) ->
                {ok, [
                    #{
                        <<"id">> => 802,
                        <<"channel_id">> => 11,
                        <<"user_id">> => 2002,
                        <<"order_no">> => <<"ORD_LIST_2">>
                    },
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_orders(2002),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Order]} = Result,
                ?assertEqual(11, maps:get(<<"channel_id">>, Order))
            end)
        end
    }.

get_my_orders_propagates_repo_error_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'list_by_user', 2, fun(2002, 50) ->
                {error, db_down}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_orders(2002),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

get_my_orders_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'list_by_user', 2, fun(2002, 50) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_orders(2002),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_my_orders_returns_error_when_repo_returns_unexpected_term_test_() ->
    MockConfigs = [
        {channel_order_repo, [
            {'list_by_user', 2, fun(2002, 50) -> unexpected_lookup end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_my_orders(2002),
                ?assertEqual({error, <<"unexpected_lookup">>}, Result)
            end)
        end
    }.

get_admins_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_admin_repo, [
            {'list_by_channel', 1, fun(11) -> {error, db_down} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_admins(11),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

get_admins_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_admin_repo, [
            {'list_by_channel', 1, fun(11) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_admins(11),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_admins_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_admin_repo, [
            {'list_by_channel', 1, fun(11) ->
                {ok, [
                    #{<<"id">> => 1, <<"user_id">> => 1001, <<"channel_id">> => 11},
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_admins(11),
                ?assertMatch({ok, [_]}, Result),
                {ok, Admins} = Result,
                ?assertEqual(1, length(Admins))
            end)
        end
    }.

get_admins_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_admin_repo, [
            {'list_by_channel', 1, fun(_) ->
                erlang:error(should_not_query_admins_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_admins(ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, list_by_channel, 1))
            end)
        end
    }.

update_admin_role_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) ->
                erlang:error(should_not_check_role_when_channel_id_decode_unexpected)
            end},
            {'update_role', 3, fun(_, _, _) ->
                erlang:error(should_not_update_role_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_admin_role(1001, ChannelIdBin, 2002, 2),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2)),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, update_role, 3))
            end)
        end
    }.

update_admin_role_requires_creator_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end},
            {'update_role', 3, fun(_, _, _) -> erlang:error(should_not_update_role) end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 3003}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_admin_role(1001, ChannelIdBin, 2002, 2),

                ?assertEqual({error, <<"无权限操作，仅创建者可修改角色"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, update_role, 3))
            end)
        end
    }.

update_admin_role_creator_success_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end},
            {'update_role', 3, fun(11, 2002, 2) -> {ok, 1} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:update_admin_role(1001, ChannelIdBin, 2002, 2),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_admin_repo, update_role, 3))
            end)
        end
    }.

remove_subscriber_admin_success_updates_counter_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 2002) -> {ok, 1} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(fake_conn, 11, -1) -> {ok, 1} end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(Key) ->
                case Key of
                    {channel_subs, 11} -> ok;
                    {channel, 11} -> ok;
                    _ -> erlang:error({unexpected_cache_key, Key})
                end
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_subscriber(1001, ChannelIdBin, 2002),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
                ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3)),
                ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
            end)
        end
    }.

remove_subscriber_is_idempotent_when_target_already_inactive_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 2002) -> {ok, 0} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(_, _, _) ->
                erlang:error(should_not_increment_subscribers)
            end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(Key) ->
                case Key of
                    {channel_subs, 11} -> ok;
                    {channel, 11} -> ok;
                    _ -> erlang:error({unexpected_cache_key, Key})
                end
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_subscriber(1001, ChannelIdBin, 2002),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
                ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
                ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
            end)
        end
    }.

remove_subscriber_tx_error_returns_failure_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {elib_pg, [
            {'with_tx', 1, fun(Fun) ->
                try
                    Fun(fake_conn)
                catch
                    throw:{abort_tx, Reason} -> {error, Reason}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 2002) -> {error, db_error} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(_, _, _) ->
                erlang:error(should_not_increment_subscribers)
            end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_subscriber(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"移除订阅者失败"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
                ?assertEqual(0, meck:num_calls(imboy_cache, flush, 1))
            end)
        end
    }.

remove_subscriber_requires_admin_role_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{<<"id">> => 11, <<"creator_uid">> => 3003}
            end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(_, _, _) -> erlang:error(should_not_delete_subscriber) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_subscriber(1001, ChannelIdBin, 2002),

                ?assertEqual({error, <<"无权限操作，需要管理员及以上权限"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, delete, 3))
            end)
        end
    }.

get_messages_private_channel_requires_subscription_test_() ->
    ChannelIdBin = integer_to_binary(12),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(12, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 12, <<"type">> => 1, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 12, <<"creator_uid">> => 3003}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(12, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(12, 2002) -> false end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(_, _, _) ->
                erlang:error(should_not_list_messages_without_subscription)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(2002, ChannelIdBin, 0, 20),

                ?assertEqual({error, <<"私有频道仅限订阅用户访问"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

get_messages_private_channel_allows_subscriber_test_() ->
    ChannelIdBin = integer_to_binary(12),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(12, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 12, <<"type">> => 1, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 12, <<"creator_uid">> => 3003}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(12, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(12, 2002) -> true end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(12, 0, 20) ->
                {ok, [#{
                    <<"id">> => 991,
                    <<"channel_id">> => 12,
                    <<"author_id">> => 1001,
                    <<"content">> => <<"private content"/utf8>>
                }]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(2002, ChannelIdBin, 0, 20),

                ?assertMatch({ok, [_]}, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

get_messages_returns_error_when_channel_type_invalid_test_() ->
    ChannelIdBin = integer_to_binary(12),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(12, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 12, <<"type">> => invalid_type, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 12, <<"creator_uid">> => 3003}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(12, 2002) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_when_channel_type_invalid)
            end}
        ]},
        {channel_subscribe_ds, [
            {'has_purchased', 2, fun(_, _) ->
                erlang:error(should_not_check_purchase_when_channel_type_invalid)
            end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(_, _, _) ->
                erlang:error(should_not_list_messages_when_channel_type_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(2002, ChannelIdBin, 0, 20),
                ?assertEqual({error, <<"频道类型无效"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, is_subscribed, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, has_purchased, 2)),
                ?assertEqual(0, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

get_messages_paid_channel_admin_skips_subscription_and_purchase_checks_test_() ->
    ChannelIdBin = integer_to_binary(13),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(13, <<"id,type,status">>) ->
                #{<<"id">> => 13, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(13, 1001) -> 2 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscription_for_admin)
            end}
        ]},
        {channel_subscribe_ds, [
            {'has_purchased', 2, fun(_, _) ->
                erlang:error(should_not_check_purchase_for_admin)
            end}
        ]},
        {channel_message_repo, [
            {'list_by_channel', 3, fun(13, 0, 20) ->
                {ok, [#{
                    <<"id">> => 992,
                    <<"channel_id">> => 13,
                    <<"author_id">> => 1001,
                    <<"content">> => <<"admin content"/utf8>>
                }]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_messages(1001, ChannelIdBin, 0, 20),

                ?assertMatch({ok, [_]}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, is_subscribed, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, has_purchased, 2)),
                ?assertEqual(1, meck:num_calls(channel_message_repo, list_by_channel, 3))
            end)
        end
    }.

subscribe_private_channel_already_subscribed_is_idempotent_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type">>) ->
                #{<<"id">> => 11, <<"type">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'is_invited', 2, fun(11, 2002) -> true end},
            {'subscribe_private', 3, fun(11, 2002, undefined) -> ok end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_subscribed', 2, fun(11, 2002) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, is_invited, 2)),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, subscribe_private, 3)),
                ?assertEqual(1, meck:num_calls(channel_logic_notify, notify_channel_subscribed, 2))
            end)
        end
    }.

subscribe_public_channel_already_subscribed_is_idempotent_test_() ->
    ChannelIdBin = integer_to_binary(12),
    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(12, <<"id,type">>) ->
                #{<<"id">> => 12, <<"type">> => 0}
            end},
            {'subscribe', 2, fun(12, 2002) -> ok end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_subscribed', 2, fun(12, 2002) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2)),
                ?assertEqual(1, meck:num_calls(channel_logic_notify, notify_channel_subscribed, 2))
            end)
        end
    }.

subscribe_paid_channel_already_subscribed_skips_purchase_check_test_() ->
    ChannelIdBin = integer_to_binary(13),
    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(13, <<"id,type">>) ->
                #{<<"id">> => 13, <<"type">> => 2}
            end},
            {'subscribe', 2, fun(13, 2002) -> ok end}
        ]},
        {channel_subscribe_ds, [
            {'has_purchased', 2, fun(13, 2002) -> true end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_subscribed', 2, fun(13, 2002) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),

                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_subscribe_ds, has_purchased, 2)),
                ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2)),
                ?assertEqual(1, meck:num_calls(channel_logic_notify, notify_channel_subscribed, 2))
            end)
        end
    }.

subscribe_public_channel_propagates_ds_atom_error_as_binary_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(14, <<"id,type">>) ->
                #{<<"id">> => 14, <<"type">> => 0}
            end},
            {'subscribe', 2, fun(14, 2002) -> {error, db_down} end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(14, 2002) -> false end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_subscribe_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_subscribed, 2))
            end)
        end
    }.

subscribe_returns_error_when_channel_payload_invalid_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(14, <<"id,type">>) -> [] end},
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_channel_payload_invalid)
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscribe_state_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, is_subscribed, 2)),
                ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
            end)
        end
    }.

subscribe_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                erlang:error(should_not_lookup_channel_when_channel_id_decode_unexpected)
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_check_subscribe_state_when_channel_id_decode_unexpected)
            end}
        ]},
        {channel_ds, [
            {'subscribe', 2, fun(_, _) ->
                erlang:error(should_not_subscribe_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, find_by_id, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, is_subscribed, 2)),
                ?assertEqual(0, meck:num_calls(channel_ds, subscribe, 2))
            end)
        end
    }.

subscribe_private_channel_rejects_unexpected_invitation_state_test_() ->
    ChannelIdBin = integer_to_binary(11),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type">>) ->
                #{<<"id">> => 11, <<"type">> => 1}
            end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(11, 2002) -> false end}
        ]},
        {channel_subscribe_ds, [
            {'is_invited', 2, fun(11, 2002) -> {error, db_down} end},
            {'subscribe_private', 3, fun(_, _, _) ->
                erlang:error(should_not_subscribe_private_when_invitation_state_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"私有频道需要邀请才能订阅"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscribe_ds, subscribe_private, 3))
            end)
        end
    }.

subscribe_returns_error_when_ds_subscribe_fails_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(14, <<"id,type">>) ->
                #{<<"id">> => 14, <<"type">> => 0}
            end},
            {'subscribe', 2, fun(14, 2002) -> {error, unexpected_state} end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_subscribed', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_subscribe_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"unexpected_state">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, subscribe, 2))
            end)
        end
    }.

unsubscribe_propagates_ds_atom_error_as_binary_test_() ->
    ChannelIdBin = integer_to_binary(15),
    MockConfigs = [
        {channel_ds, [
            {'unsubscribe', 2, fun(15, 2002) -> {error, db_down} end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_unsubscribed', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_unsubscribe_failed)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:unsubscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_unsubscribed, 2))
            end)
        end
    }.

unsubscribe_propagates_unexpected_ds_result_as_binary_test_() ->
    ChannelIdBin = integer_to_binary(16),
    MockConfigs = [
        {channel_ds, [
            {'unsubscribe', 2, fun(16, 2002) -> unexpected_result end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_unsubscribed', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_unsubscribe_unexpected_result)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:unsubscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"unexpected_result">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_unsubscribed, 2))
            end)
        end
    }.

unsubscribe_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_ds, [
            {'unsubscribe', 2, fun(_, _) ->
                erlang:error(should_not_call_unsubscribe_when_channel_id_decode_unexpected)
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_unsubscribed', 2, fun(_, _) ->
                erlang:error(should_not_notify_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:unsubscribe(2002, ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_ds, unsubscribe, 2)),
                ?assertEqual(0, meck:num_calls(channel_logic_notify, notify_channel_unsubscribed, 2))
            end)
        end
    }.

%% ===================================================================
%% P0-1: mark_as_read 测试 - 验证仅清除当前用户的未读计数
%% ===================================================================

mark_as_read_clears_unread_only_for_current_user_test_() ->
    ChannelId = 13,
    ChannelIdBin = integer_to_binary(ChannelId),
    MessageIdBin = integer_to_binary(999),
    Uid = 1001,

    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ChannelId, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) -> true end},
            %% 关键验证：clear_unread 必须接收两个参数 (ChannelId, Uid)
            {'clear_unread', 2, fun(ReceivedChannelId, ReceivedUid) ->
                ?assertEqual(ChannelId, ReceivedChannelId),
                ?assertEqual(Uid, ReceivedUid),
                {ok, 1}
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_unread_count', 3, fun(ReceivedChannelId, ReceivedUid, 0) ->
                ?assertEqual(ChannelId, ReceivedChannelId),
                ?assertEqual(Uid, ReceivedUid),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:mark_as_read(Uid, ChannelIdBin, MessageIdBin),

                ?assertEqual(ok, Result),
                %% 验证调用的是 clear_unread/2 而不是 clear_unread/1
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, clear_unread, 2))
            end)
        end
    }.

mark_as_read_returns_error_when_channel_not_found_test_() ->
    ChannelIdBin = <<"invalid_channel_id">>,
    MessageIdBin = integer_to_binary(999),
    Uid = 1001,

    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(_) -> {error, not_found} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:mark_as_read(Uid, ChannelIdBin, MessageIdBin),
                ?assertMatch({error, _}, Result)
            end)
        end
    }.

mark_as_read_falls_back_to_custom_id_when_decode_returns_unexpected_term_test_() ->
    ChannelId = 13,
    ChannelIdBin = <<"tech_daily">>,
    MessageIdBin = integer_to_binary(999),
    Uid = 1001,

    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{<<"id">> => ChannelId}
            end},
            {'find_by_id', 2, fun(13, Fields) ->
                case Fields of
                    <<"id,type,status">> ->
                        #{<<"id">> => 13, <<"type">> => 0, <<"status">> => 1};
                    <<"*">> ->
                        #{<<"id">> => 13, <<"creator_uid">> => 2002}
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) -> 0 end}
        ]},
        {channel_subscription_repo, [
            {'is_subscribed', 2, fun(_, _) -> true end},
            {'clear_unread', 2, fun(ReceivedChannelId, ReceivedUid) ->
                ?assertEqual(ChannelId, ReceivedChannelId),
                ?assertEqual(Uid, ReceivedUid),
                {ok, 1}
            end}
        ]},
        {channel_logic_notify, [
            {'notify_channel_unread_count', 3, fun(ReceivedChannelId, ReceivedUid, 0) ->
                ?assertEqual(ChannelId, ReceivedChannelId),
                ?assertEqual(Uid, ReceivedUid),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:mark_as_read(Uid, ChannelIdBin, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, find_by_custom_id, 1)),
                ?assertEqual(1, meck:num_calls(channel_subscription_repo, clear_unread, 2))
            end)
        end
    }.

mark_as_read_returns_error_when_custom_id_payload_invalid_after_decode_unexpected_test_() ->
    ChannelIdBin = <<"tech_daily">>,
    MessageIdBin = integer_to_binary(999),

    MockConfigs = [
        {channel_repo, [
            {'find_by_custom_id', 1, fun(<<"tech_daily">>) ->
                #{<<"id">> => <<"invalid">>}
            end},
            {'find_by_id', 2, fun(_, _) ->
                erlang:error(should_not_call_find_by_id_when_channel_id_invalid)
            end}
        ]},
        {channel_subscription_repo, [
            {'clear_unread', 2, fun(_, _) ->
                erlang:error(should_not_call_clear_unread_when_channel_missing)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:mark_as_read(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, find_by_custom_id, 1)),
                ?assertEqual(0, meck:num_calls(channel_repo, find_by_id, 2)),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, clear_unread, 2))
            end)
        end
    }.

create_channel_returns_error_when_managed_query_fails_test_() ->
    MockConfigs = [
        {channel_ds, [
            {'list_managed', 1, fun(1001) -> {error, db_down} end},
            {'create_channel', 4, fun(_, _, _, _) ->
                erlang:error(should_not_call_create_channel_when_managed_query_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_channel(1001, <<"my-channel">>, 0, #{}, 10),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_ds, create_channel, 4))
            end)
        end
    }.

create_channel_returns_error_when_managed_payload_not_list_test_() ->
    MockConfigs = [
        {channel_ds, [
            {'list_managed', 1, fun(1001) -> {ok, invalid_payload} end},
            {'create_channel', 4, fun(_, _, _, _) ->
                erlang:error(should_not_call_create_channel_when_managed_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_channel(1001, <<"my-channel">>, 0, #{}, 10),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_ds, create_channel, 4))
            end)
        end
    }.

create_channel_returns_error_when_reload_payload_not_map_test_() ->
    MockConfigs = [
        {channel_ds, [
            {'list_managed', 1, fun(1001) -> {ok, []} end},
            {'create_channel', 4, fun(1001, <<"my-channel">>, 0, #{}) -> {ok, 11} end},
            {'find_by_id', 2, fun(11, <<"*">>) -> invalid_payload end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:create_channel(1001, <<"my-channel">>, 0, #{}, 10),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_ds, create_channel, 4)),
                ?assertEqual(1, meck:num_calls(channel_ds, find_by_id, 2))
            end)
        end
    }.

%% ===================================================================
%% get_channel_stats/1 测试 - P0-3 统计准确性修复
%% ===================================================================

%% P0-3: 验证统计使用 SQL 聚合查询而非加载消息列表
get_channel_stats_uses_aggregation_query_test_() ->
    ChannelId = 11,
    ChannelIdBin = integer_to_binary(ChannelId),

    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ChannelId, <<"name">> => <<"test">>, <<"subscriber_count">> => 100}
            end},
            {'get_reaction_count', 1, fun(_) -> {ok, 50} end}
        ]},
        {elib_pg, [
            %% 验证使用了聚合 SQL 查询
            {'one', 2, fun(Sql, [ReceivedChannelId]) ->
                SqlBin = iolist_to_binary(Sql),
                %% 验证 SQL 包含 COUNT 和 SUM 聚合函数
                ?assert(re:run(SqlBin, <<"COUNT">>) =/= nomatch),
                ?assert(re:run(SqlBin, <<"SUM">>) =/= nomatch),
                ?assertEqual(ChannelId, ReceivedChannelId),
                {ok, #{<<"total_messages">> => 5000, <<"total_views">> => 15000}}
            end}
        ]},
        {channel_message_repo, [
            {'tablename', 0, fun() -> <<"public.channel_message">> end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),

                ?assertMatch({ok, _}, Result),
                {ok, Stats} = Result,
                %% 验证统计结果是聚合查询的结果（而非列表长度）
                ?assertEqual(5000, maps:get(<<"total_messages">>, Stats)),
                ?assertEqual(15000, maps:get(<<"total_views">>, Stats)),
                ?assertEqual(100, maps:get(<<"subscriber_count">>, Stats)),
                ?assertEqual(50, maps:get(<<"total_reactions">>, Stats))
            end)
        end
    }.

%% P0-3: 验证统计在频道不存在时返回错误
get_channel_stats_returns_error_when_channel_not_found_test_() ->
    ChannelIdBin = integer_to_binary(99999),

    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) -> {error, not_found} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),
                ?assertMatch({error, _}, Result)
            end)
        end
    }.

get_channel_stats_returns_error_when_channel_payload_invalid_test_() ->
    ChannelIdBin = integer_to_binary(99998),
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) -> [] end},
            {'get_reaction_count', 1, fun(_) ->
                erlang:error(should_not_query_reaction_when_channel_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, get_reaction_count, 1))
            end)
        end
    }.

get_channel_stats_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                erlang:error(should_not_lookup_channel_when_channel_id_decode_unexpected)
            end},
            {'get_reaction_count', 1, fun(_) ->
                erlang:error(should_not_query_reaction_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, find_by_id, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, get_reaction_count, 1))
            end)
        end
    }.

%% P0-3: 验证统计在无消息时返回 0
get_channel_stats_returns_zero_when_no_messages_test_() ->
    ChannelId = 12,
    ChannelIdBin = integer_to_binary(ChannelId),

    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ChannelId, <<"name">> => <<"empty">>, <<"subscriber_count">> => 5}
            end},
            {'get_reaction_count', 1, fun(_) -> {ok, 0} end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_, _) ->
                {ok, #{<<"total_messages">> => 0, <<"total_views">> => 0}}
            end}
        ]},
        {channel_message_repo, [
            {'tablename', 0, fun() -> <<"public.channel_message">> end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),

                ?assertMatch({ok, _}, Result),
                {ok, Stats} = Result,
                ?assertEqual(0, maps:get(<<"total_messages">>, Stats)),
                ?assertEqual(0, maps:get(<<"total_views">>, Stats))
            end)
        end
    }.

get_channel_stats_returns_error_when_message_aggregation_fails_test_() ->
    ChannelId = 13,
    ChannelIdBin = integer_to_binary(ChannelId),

    MockConfigs = [
        {channel_ds, [
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ChannelId, <<"name">> => <<"agg_err">>, <<"subscriber_count">> => 9}
            end},
            {'get_reaction_count', 1, fun(_) ->
                erlang:error(should_not_call_reaction_count_when_aggregation_fails)
            end}
        ]},
        {channel_message_ds, [
            {'get_stats', 1, fun(_) ->
                {error, db_down}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_ds, get_reaction_count, 1))
            end)
        end
    }.

get_channel_stats_returns_error_when_reaction_query_fails_test_() ->
    ChannelId = 14,
    ChannelIdBin = integer_to_binary(ChannelId),

    MockConfigs = [
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) ->
                #{<<"id">> => ChannelId, <<"name">> => <<"reaction_err">>, <<"subscriber_count">> => 12}
            end},
            {'get_reaction_count', 1, fun(_) -> {error, timeout} end}
        ]},
        {elib_pg, [
            {'one', 2, fun(_, _) ->
                {ok, #{<<"total_messages">> => 3, <<"total_views">> => 17}}
            end}
        ]},
        {channel_message_repo, [
            {'tablename', 0, fun() -> <<"public.channel_message">> end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_channel_stats(ChannelIdBin),
                ?assertEqual({error, <<"timeout">>}, Result)
            end)
        end
    }.

get_daily_stats_returns_error_when_repo_payload_not_list_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_repo, [
            {'get_daily_stats', 2, fun(14, 7) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_daily_stats(ChannelIdBin, 7),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_daily_stats_filters_non_map_entries_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_repo, [
            {'get_daily_stats', 2, fun(14, 7) ->
                {ok, [
                    #{<<"channel_id">> => 14, <<"day">> => <<"2026-02-24">>, <<"messages">> => 12},
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_daily_stats(ChannelIdBin, 7),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Item]} = Result,
                ?assertEqual(14, maps:get(<<"channel_id">>, Item))
            end)
        end
    }.

get_daily_stats_returns_error_when_repo_returns_unexpected_term_test_() ->
    ChannelIdBin = integer_to_binary(14),
    MockConfigs = [
        {channel_repo, [
            {'get_daily_stats', 2, fun(14, 7) -> unexpected_lookup end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_daily_stats(ChannelIdBin, 7),
                ?assertEqual({error, <<"unexpected_lookup">>}, Result)
            end)
        end
    }.

get_daily_stats_returns_error_when_channel_id_decode_unexpected_test_() ->
    ChannelIdBin = <<"ch_hash_unexpected">>,
    MockConfigs = [
        {channel_repo, [
            {'get_daily_stats', 2, fun(_, _) ->
                erlang:error(should_not_query_daily_stats_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_daily_stats(ChannelIdBin, 7),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, get_daily_stats, 2))
            end)
        end
    }.

record_message_view_returns_error_when_message_id_decode_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(_, _) ->
                erlang:error(should_not_check_access_when_message_id_decode_unexpected)
            end}
        ]},
        {channel_repo, [
            {'has_viewed_message', 2, fun(_, _) ->
                erlang:error(should_not_check_view_state_when_message_id_decode_unexpected)
            end},
            {'insert_message_view', 4, fun(_, _, _, _) ->
                erlang:error(should_not_insert_view_when_message_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:record_message_view(1001, <<"ch_hash_11">>, <<"msg_hash_unexpected">>),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, ensure_channel_content_access, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, has_viewed_message, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, insert_message_view, 4))
            end)
        end
    }.

add_reaction_returns_error_when_message_id_decode_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(_, _) ->
                erlang:error(should_not_check_access_when_message_id_decode_unexpected)
            end}
        ]},
        {channel_repo, [
            {'insert_reaction', 5, fun(_, _, _, _, _) ->
                erlang:error(should_not_insert_reaction_when_message_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:add_reaction(1001, <<"ch_hash_11">>, <<"msg_hash_unexpected">>, <<"like">>),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, ensure_channel_content_access, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, insert_reaction, 5))
            end)
        end
    }.

remove_reaction_returns_error_when_message_id_decode_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(_, _) ->
                erlang:error(should_not_check_access_when_message_id_decode_unexpected)
            end}
        ]},
        {channel_repo, [
            {'delete_reaction', 4, fun(_, _, _, _) ->
                erlang:error(should_not_delete_reaction_when_message_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_reaction(1001, <<"ch_hash_11">>, <<"msg_hash_unexpected">>, <<"like">>),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, ensure_channel_content_access, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, delete_reaction, 4))
            end)
        end
    }.

record_message_view_returns_error_when_has_viewed_message_fails_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(1001, 11) -> ok end}
        ]},
        {channel_repo, [
            {'has_viewed_message', 2, fun(99, 1001) -> {error, db_down} end},
            {'insert_message_view', 4, fun(_, _, _, _) ->
                erlang:error(should_not_insert_view_when_has_viewed_message_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:record_message_view(1001, <<"ch_hash_11">>, <<"99">>),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, has_viewed_message, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, insert_message_view, 4))
            end)
        end
    }.

record_message_view_returns_error_when_has_viewed_message_returns_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(1001, 11) -> ok end}
        ]},
        {channel_repo, [
            {'has_viewed_message', 2, fun(99, 1001) -> unexpected_lookup end},
            {'insert_message_view', 4, fun(_, _, _, _) ->
                erlang:error(should_not_insert_view_when_has_viewed_message_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:record_message_view(1001, <<"ch_hash_11">>, <<"99">>),
                ?assertEqual({error, <<"unexpected_lookup">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, has_viewed_message, 2)),
                ?assertEqual(0, meck:num_calls(channel_repo, insert_message_view, 4))
            end)
        end
    }.

record_message_view_returns_error_when_insert_message_view_returns_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(1001, 11) -> ok end}
        ]},
        {channel_repo, [
            {'has_viewed_message', 2, fun(99, 1001) -> false end},
            {'insert_message_view', 4, fun(11, 99, 1001, _) -> unexpected_insert_result end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:record_message_view(1001, <<"ch_hash_11">>, <<"99">>),
                ?assertEqual({error, <<"unexpected_insert_result">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, has_viewed_message, 2)),
                ?assertEqual(1, meck:num_calls(channel_repo, insert_message_view, 4))
            end)
        end
    }.

add_reaction_returns_error_when_insert_reaction_returns_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(1001, 11) -> ok end}
        ]},
        {channel_repo, [
            {'insert_reaction', 5, fun(11, 99, 1001, <<"like">>, _) -> unexpected_insert_result end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:add_reaction(1001, <<"ch_hash_11">>, <<"99">>, <<"like">>),
                ?assertEqual({error, <<"unexpected_insert_result">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, insert_reaction, 5))
            end)
        end
    }.

remove_reaction_returns_error_when_delete_reaction_returns_unexpected_test_() ->
    MockConfigs = [
        {channel_logic_common, [
            {'resolve_channel_id', 1, fun(<<"ch_hash_11">>) -> 11 end},
            {'ensure_channel_content_access', 2, fun(1001, 11) -> ok end}
        ]},
        {channel_repo, [
            {'delete_reaction', 4, fun(11, 99, 1001, <<"like">>) -> unexpected_delete_result end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:remove_reaction(1001, <<"ch_hash_11">>, <<"99">>, <<"like">>),
                ?assertEqual({error, <<"unexpected_delete_result">>}, Result),
                ?assertEqual(1, meck:num_calls(channel_repo, delete_reaction, 4))
            end)
        end
    }.

%% ===================================================================
%% revoke_message/3 测试 - P1 撤回能力
%% ===================================================================

revoke_message_author_success_within_window_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"991">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(991) ->
                #{
                    <<"id">> => 991,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(991, 1001, <<"2026-02-22T10:01:00Z">>) ->
                {ok, 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end},
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {application, [
            {'get_env', 2, fun(App, Key) ->
                case {App, Key} of
                    {imboy, channel_revoke_window_seconds} -> {ok, 120};
                    _ -> undefined
                end
            end}
        ]},
        {elib_dt, [
            {'rfc3339_to', 2, fun(<<"2026-02-22T10:00:00Z">>, millisecond) -> 1708596000000 end},
            {'millisecond', 0, fun() -> 1708596060000 end},
            {'now', 0, fun() -> <<"2026-02-22T10:01:00Z">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_revoked">>, <<>>, null, Payload, save) ->
                ?assertEqual(11, maps:get(<<"channel_id">>, Payload)),
                ?assertEqual(991, maps:get(<<"message_id">>, Payload)),
                ?assertEqual(1001, maps:get(<<"revoked_by">>, Payload)),
                ?assertEqual(<<"2026-02-22T10:01:00Z">>, maps:get(<<"revoked_at">>, Payload)),
                ok
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, revoke, 3)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

revoke_message_author_success_still_returns_ok_when_notify_crashes_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"991">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(991) ->
                #{
                    <<"id">> => 991,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(991, 1001, <<"2026-02-22T10:01:00Z">>) ->
                {ok, 1}
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end},
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {application, [
            {'get_env', 2, fun(App, Key) ->
                case {App, Key} of
                    {imboy, channel_revoke_window_seconds} -> {ok, 120};
                    _ -> undefined
                end
            end}
        ]},
        {elib_dt, [
            {'rfc3339_to', 2, fun(<<"2026-02-22T10:00:00Z">>, millisecond) -> 1708596000000 end},
            {'millisecond', 0, fun() -> 1708596060000 end},
            {'now', 0, fun() -> <<"2026-02-22T10:01:00Z">> end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_revoked">>, <<>>, null, _Payload, save) ->
                erlang:error(mock_notify_crash)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, revoke, 3)),
                ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

revoke_message_returns_timeout_error_when_window_expired_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"991">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(991) ->
                #{
                    <<"id">> => 991,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_when_expired) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end}
        ]},
        {application, [
            {'get_env', 2, fun(App, Key) ->
                case {App, Key} of
                    {imboy, channel_revoke_window_seconds} -> {ok, 120};
                    _ -> undefined
                end
            end}
        ]},
        {elib_dt, [
            {'rfc3339_to', 2, fun(<<"2026-02-22T10:00:00Z">>, millisecond) -> 1708596000000 end},
            {'millisecond', 0, fun() -> 1708597000000 end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"撤回时间已超出限制"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_is_idempotent_when_already_revoked_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"991">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(991) ->
                #{
                    <<"id">> => 991,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => true
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_again) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_send_event_again) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3)),
                ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
            end)
        end
    }.

revoke_message_returns_permission_denied_for_non_author_non_admin_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"991">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(991) ->
                #{
                    <<"id">> => 991,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 2002,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_without_permission) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"无权限撤回此消息"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_returns_error_when_repo_returns_non_map_payload_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"992">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(992) -> [] end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_when_message_invalid) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) -> erlang:error(should_not_check_role_when_message_invalid) end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) -> erlang:error(should_not_load_channel_when_message_invalid) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2)),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_returns_error_when_required_fields_missing_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"993">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(993) ->
                #{
                    <<"id">> => 993,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_when_required_fields_missing) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) -> erlang:error(should_not_check_role_when_required_fields_missing) end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) -> erlang:error(should_not_load_channel_when_required_fields_missing) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2)),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_returns_error_when_required_fields_type_invalid_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"995">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(995) ->
                #{
                    <<"id">> => 995,
                    <<"channel_id">> => <<"11">>,
                    <<"author_id">> => <<"1001">>,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_when_fields_type_invalid) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(_, _) -> erlang:error(should_not_check_role_when_fields_type_invalid) end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(_, _) -> erlang:error(should_not_load_channel_when_fields_type_invalid) end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"消息不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_admin_repo, get_role, 2)),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_treats_non_boolean_revoked_field_as_not_revoked_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"996">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(996) ->
                #{
                    <<"id">> => 996,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => <<"2026-02-22T10:00:00Z">>,
                    <<"revoked">> => <<"unexpected">>
                }
            end},
            {'revoke', 3, fun(996, 1001, <<"2026-02-22T10:01:00Z">>) -> {ok, 0} end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end}
        ]},
        {application, [
            {'get_env', 2, fun(App, Key) ->
                case {App, Key} of
                    {imboy, channel_revoke_window_seconds} -> {ok, 120};
                    _ -> undefined
                end
            end}
        ]},
        {elib_dt, [
            {'rfc3339_to', 2, fun(<<"2026-02-22T10:00:00Z">>, millisecond) -> 1708596000000 end},
            {'millisecond', 0, fun() -> 1708596060000 end},
            {'now', 0, fun() -> <<"2026-02-22T10:01:00Z">> end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

revoke_message_returns_timeout_error_when_created_at_invalid_test_() ->
    ChannelIdBin = <<"11">>,
    MessageIdBin = <<"994">>,
    MockConfigs = [
        {channel_message_repo, [
            {'find_by_id', 1, fun(994) ->
                #{
                    <<"id">> => 994,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001,
                    <<"created_at">> => #{bad => value},
                    <<"revoked">> => false
                }
            end},
            {'revoke', 3, fun(_, _, _) -> erlang:error(should_not_revoke_when_created_at_invalid) end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"*">>) -> #{<<"id">> => 11, <<"creator_uid">> => 3003} end}
        ]},
        {application, [
            {'get_env', 2, fun(App, Key) ->
                case {App, Key} of
                    {imboy, channel_revoke_window_seconds} -> {ok, 120};
                    _ -> undefined
                end
            end}
        ]},
        {elib_dt, [
            {'rfc3339_to', 2, fun(_, _) -> erlang:error(bad_datetime) end},
            {'millisecond', 0, fun() -> 1708596060000 end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:revoke_message(1001, ChannelIdBin, MessageIdBin),
                ?assertEqual({error, <<"撤回时间已超出限制"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_message_repo, revoke, 3))
            end)
        end
    }.

%% ===================================================================
%% facade delegation tests
%% ===================================================================

subscribe_delegates_to_subscription_module_test_() ->
    ChannelIdBin = <<"ch_hash_11">>,
    MockConfigs = [
        {channel_logic_subscription, [
            {'subscribe', 2, fun(1001, <<"ch_hash_11">>) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:subscribe(1001, ChannelIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_logic_subscription, subscribe, 2))
            end)
        end
    }.

unsubscribe_delegates_to_subscription_module_test_() ->
    ChannelIdBin = <<"ch_hash_11">>,
    MockConfigs = [
        {channel_logic_subscription, [
            {'unsubscribe', 2, fun(1001, <<"ch_hash_11">>) -> ok end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:unsubscribe(1001, ChannelIdBin),
                ?assertEqual(ok, Result),
                ?assertEqual(1, meck:num_calls(channel_logic_subscription, unsubscribe, 2))
            end)
        end
    }.

sync_channels_delegates_to_sync_module_test_() ->
    MockConfigs = [
        {channel_logic_sync, [
            {'sync_channels', 2, fun(1001, 1700000000000) ->
                {ok, #{channels => [], server_time => 1700000000123}}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:sync_channels(1001, 1700000000000),
                ?assertEqual(
                    {ok, #{channels => [], server_time => 1700000000123}},
                    Result
                ),
                ?assertEqual(1, meck:num_calls(channel_logic_sync, sync_channels, 2))
            end)
        end
    }.

sync_channels_returns_empty_when_user_has_no_subscriptions_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {ok, []} end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun(_, _) ->
                erlang:error(should_not_query_channels_when_no_subscriptions)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertMatch({ok, _}, Result),
                {ok, Payload} = Result,
                ?assertEqual([], maps:get(channels, Payload)),
                ?assert(is_integer(maps:get(server_time, Payload))),
                ?assertEqual(0, meck:num_calls(channel_repo, list_by_ids_since, 2))
            end)
        end
    }.

sync_channels_returns_error_when_subscription_query_fails_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {error, db_down} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

sync_channels_returns_error_when_subscription_payload_not_list_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {ok, invalid_payload} end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun(_, _) ->
                erlang:error(should_not_query_channels_when_subscription_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_repo, list_by_ids_since, 2))
            end)
        end
    }.

sync_channels_filters_invalid_subscription_entries_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) ->
                {ok, [
                    #{<<"channel_id">> => 11},
                    #{<<"channel_id">> => <<"bad">>},
                    #{},
                    42,
                    #{<<"channel_id">> => 11}
                ]}
            end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun([11], 1700000000000) ->
                {ok, [#{<<"id">> => 11, <<"name">> => <<"ch11">>}]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                #{<<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertMatch({ok, _}, Result),
                {ok, Payload} = Result,
                Channels = maps:get(channels, Payload),
                ?assertEqual(1, length(Channels)),
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, lists:nth(1, Channels)))
            end)
        end
    }.

sync_channels_returns_error_when_delta_query_fails_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {ok, [#{<<"channel_id">> => 11}]} end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun([11], 1700000000000) -> {error, timeout} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertEqual({error, <<"timeout">>}, Result)
            end)
        end
    }.

sync_channels_returns_error_when_delta_payload_not_list_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {ok, [#{<<"channel_id">> => 11}]} end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun([11], 1700000000000) -> {ok, invalid_delta_payload} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_delta_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertEqual({error, <<"invalid_delta_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

sync_channels_filters_non_map_delta_entries_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) -> {ok, [#{<<"channel_id">> => 11}]} end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun([11], 1700000000000) ->
                {ok, [
                    #{<<"id">> => 11, <<"name">> => <<"ch11">>},
                    invalid,
                    [bad],
                    12,
                    #{<<"id">> => 12, <<"name">> => <<"ch12">>}
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                Name = maps:get(<<"name">>, Channel),
                #{
                    <<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>,
                    <<"name">> => Name
                }
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertMatch({ok, _}, Result),
                {ok, Payload} = Result,
                Channels = maps:get(channels, Payload),
                ?assertEqual(2, length(Channels)),
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, lists:nth(1, Channels))),
                ?assertEqual(<<"ch_hash_12">>, maps:get(<<"id">>, lists:nth(2, Channels))),
                ?assertEqual(2, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

sync_channels_success_transfers_channels_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_uid', 1, fun(1001) ->
                {ok, [#{<<"channel_id">> => 11}, #{<<"channel_id">> => 12}]}
            end}
        ]},
        {channel_repo, [
            {'list_by_ids_since', 2, fun([11, 12], 1700000000000) ->
                {ok, [
                    #{<<"id">> => 11, <<"name">> => <<"ch11">>},
                    #{<<"id">> => 12, <<"name">> => <<"ch12">>}
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                Name = maps:get(<<"name">>, Channel),
                #{
                    <<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>,
                    <<"name">> => Name
                }
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_sync:sync_channels(1001, 1700000000000),
                ?assertMatch({ok, _}, Result),
                {ok, Payload} = Result,
                Channels = maps:get(channels, Payload),
                ?assertEqual(2, length(Channels)),
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, lists:nth(1, Channels))),
                ?assertEqual(<<"ch_hash_12">>, maps:get(<<"id">>, lists:nth(2, Channels))),
                ?assert(is_integer(maps:get(server_time, Payload)))
            end)
        end
    }.

get_subscribed_channels_delegates_to_subscription_module_test_() ->
    MockConfigs = [
        {channel_logic_subscription, [
            {'get_subscribed_channels', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"ch_1">>}]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_subscribed_channels(1001),
                ?assertEqual({ok, [#{<<"id">> => <<"ch_1">>}]}, Result),
                ?assertEqual(1, meck:num_calls(channel_logic_subscription, get_subscribed_channels, 1))
            end)
        end
    }.

get_managed_channels_delegates_to_subscription_module_test_() ->
    MockConfigs = [
        {channel_logic_subscription, [
            {'get_managed_channels', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"ch_admin_1">>}]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_managed_channels(1001),
                ?assertEqual({ok, [#{<<"id">> => <<"ch_admin_1">>}]}, Result),
                ?assertEqual(1, meck:num_calls(channel_logic_subscription, get_managed_channels, 1))
            end)
        end
    }.

get_subscribers_delegates_to_subscription_module_test_() ->
    MockConfigs = [
        {channel_logic_subscription, [
            {'get_subscribers', 3, fun(<<"ch_hash_11">>, 10, 20) ->
                {ok, [#{<<"user_id">> => <<"u_1">>}]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic:get_subscribers(<<"ch_hash_11">>, 10, 20),
                ?assertEqual({ok, [#{<<"user_id">> => <<"u_1">>}]}, Result),
                ?assertEqual(1, meck:num_calls(channel_logic_subscription, get_subscribers, 3))
            end)
        end
    }.

get_subscribed_channels_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_subscribed', 2, fun(1001, <<"*">>) -> {error, db_down} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_list_subscribed_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribed_channels(1001),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_subscribed_channels_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_subscribed', 2, fun(1001, <<"*">>) -> {ok, invalid_payload} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribed_channels(1001),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_subscribed_channels_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_subscribed', 2, fun(1001, <<"*">>) ->
                {ok, [
                    #{<<"id">> => 11, <<"name">> => <<"ch11">>},
                    invalid_item
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                #{<<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribed_channels(1001),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Channel]} = Result,
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, Channel))
            end)
        end
    }.

get_managed_channels_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_managed', 1, fun(1001) -> {error, db_down} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_list_managed_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_managed_channels(1001),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_managed_channels_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_managed', 1, fun(1001) -> {ok, invalid_payload} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_managed_channels(1001),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_managed_channels_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_managed', 1, fun(1001) ->
                {ok, [
                    #{<<"id">> => 12, <<"name">> => <<"ch12">>},
                    invalid_item
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                #{<<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_managed_channels(1001),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Channel]} = Result,
                ?assertEqual(<<"ch_hash_12">>, maps:get(<<"id">>, Channel))
            end)
        end
    }.

get_subscribers_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_channel', 3, fun(11, 10, 20) -> {error, db_down} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribers(<<"11">>, 10, 20),
                ?assertEqual({error, <<"db_down">>}, Result)
            end)
        end
    }.

get_subscribers_returns_error_when_channel_id_decode_unexpected_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_channel', 3, fun(_, _, _) ->
                erlang:error(should_not_query_subscribers_when_channel_id_decode_unexpected)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribers(<<"ch_hash_unexpected">>, 10, 20),
                ?assertEqual({error, <<"频道不存在"/utf8>>}, Result),
                ?assertEqual(0, meck:num_calls(channel_subscription_repo, list_by_channel, 3))
            end)
        end
    }.

get_subscribers_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_channel', 3, fun(11, 10, 20) -> {ok, invalid_payload} end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribers(<<"11">>, 10, 20),
                ?assertEqual({error, <<"invalid_payload">>}, Result)
            end)
        end
    }.

get_subscribers_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_subscription_repo, [
            {'list_by_channel', 3, fun(11, 10, 20) ->
                {ok, [
                    #{<<"id">> => 1, <<"user_id">> => 2002},
                    invalid_item
                ]}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_subscription:get_subscribers(<<"11">>, 10, 20),
                ?assertMatch({ok, [_]}, Result),
                {ok, [Subscriber]} = Result,
                ?assert(maps:is_key(<<"id">>, Subscriber)),
                ?assert(maps:is_key(<<"user_id">>, Subscriber))
            end)
        end
    }.

search_channels_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'search', 3, fun(<<"ops">>, 20, <<"*">>) -> {error, db_down} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_search_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:search_channels(<<"ops">>, 20),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

search_channels_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'search', 3, fun(<<"ops">>, 20, <<"*">>) -> {ok, invalid_payload} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_search_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:search_channels(<<"ops">>, 20),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

search_channels_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'search', 3, fun(<<"ops">>, 20, <<"*">>) ->
                {ok, [
                    #{<<"id">> => 11, <<"name">> => <<"ops">>},
                    invalid_item
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                #{<<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:search_channels(<<"ops">>, 20),
                ?assertMatch({ok, [_]}, Result),
                {ok, Channels} = Result,
                ?assertEqual(1, length(Channels)),
                ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, lists:nth(1, Channels))),
                ?assertEqual(1, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_discover_channels_returns_error_when_repo_fails_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_discover', 2, fun(15, <<"*">>) -> {error, db_down} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_discover_fails)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:get_discover_channels(15),
                ?assertEqual({error, <<"db_down">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_discover_channels_returns_error_when_repo_payload_not_list_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_discover', 2, fun(15, <<"*">>) -> {ok, invalid_payload} end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(_) ->
                erlang:error(should_not_call_channel_transfer_when_discover_payload_invalid)
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:get_discover_channels(15),
                ?assertEqual({error, <<"invalid_payload">>}, Result),
                ?assertEqual(0, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

get_discover_channels_filters_non_map_entries_test_() ->
    MockConfigs = [
        {channel_repo, [
            {'list_discover', 2, fun(15, <<"*">>) ->
                {ok, [
                    #{<<"id">> => 12, <<"name">> => <<"discover">>},
                    invalid_item
                ]}
            end}
        ]},
        {channel_logic_common, [
            {'channel_transfer', 1, fun(Channel) ->
                Id = maps:get(<<"id">>, Channel),
                #{<<"id">> => <<"ch_hash_", (integer_to_binary(Id))/binary>>}
            end}
        ]}
    ],
    {setup,
        fun() -> setup_mocks(MockConfigs) end,
        fun(_) -> cleanup_mocks(MockConfigs) end,
        fun(_) ->
            ?_test(begin
                Result = channel_logic_message:get_discover_channels(15),
                ?assertMatch({ok, [_]}, Result),
                {ok, Channels} = Result,
                ?assertEqual(1, length(Channels)),
                ?assertEqual(<<"ch_hash_12">>, maps:get(<<"id">>, lists:nth(1, Channels))),
                ?assertEqual(1, meck:num_calls(channel_logic_common, channel_transfer, 1))
            end)
        end
    }.

%% ===================================================================
%% Internal helpers
%% ===================================================================

setup_mocks(MockConfigs) ->
    lists:foreach(fun({Module, Expectations}) ->
        {ok, _} = meck_helper:setup_mock(Module, [no_link, unstick], Expectations)
    end, MockConfigs),
    ok.

cleanup_mocks(MockConfigs) ->
    lists:foreach(fun({Module, _}) ->
        meck_helper:cleanup_mock(Module)
    end, MockConfigs),
    ok.
