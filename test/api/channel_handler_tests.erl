-module(channel_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% channel_handler 模块的 EUnit 测试
%%%
%%% 目标：验证频道发布消息入口
%%% 覆盖：路径参数 channel_id、body 参数兼容、参数缺失校验、
%%%      mark_read 路径参数优先、remove_admin DELETE/PUT 兼容、
%%%      update_admin_role 路径参数优先
%%%===================================================================

publish_message_uses_path_channel_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"content">> => <<"频道公告"/utf8>>,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => #{<<"k">> => <<"v">>}
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'publish_message', 5, fun(
                    1001, <<"ch_hash_path">>, <<"频道公告"/utf8>>, <<"text">>, #{<<"k">> := <<"v">>}
                ) ->
                    {ok, #{<<"id">> => <<"msg_1">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Msg) -> {ok_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(publish_message, Req, State),
            ?assertMatch({ok_resp, #{<<"id">> := <<"msg_1">>}}, Result)
        end
    ).

publish_message_passes_request_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"content">> => <<"幂等公告"/utf8>>,
                        <<"msg_type">> => <<"text">>,
                        <<"request_id">> => <<"req-1">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'publish_message', 6, fun(
                    1001,
                    <<"ch_hash_path">>,
                    <<"幂等公告"/utf8>>,
                    <<"text">>,
                    #{},
                    <<"req-1">>
                ) ->
                    {ok, #{<<"id">> => <<"msg_1">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Msg) -> {ok_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(publish_message, Req, State),
            ?assertMatch({ok_resp, #{<<"id">> := <<"msg_1">>}}, Result)
        end
    ).

publish_message_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"channel_id">> => <<"ch_hash_body">>,
                        <<"content">> => <<"body优先"/utf8>>,
                        <<"msg_type">> => <<"text">>,
                        <<"payload">> => #{}
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'publish_message', 5, fun(
                    1001, <<"ch_hash_path">>, <<"body优先"/utf8>>, <<"text">>, #{}
                ) ->
                    {ok, #{<<"id">> => <<"msg_2">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Msg) -> {ok_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(publish_message, Req, State),
            ?assertMatch({ok_resp, #{<<"id">> := <<"msg_2">>}}, Result)
        end
    ).

publish_message_without_channel_id_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"content">> => <<"no channel id"/utf8>>}
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> undefined end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(publish_message, Req, State),
            ?assertMatch({error_resp, <<"频道ID不能为空"/utf8>>}, Result)
        end
    ).

publish_message_with_empty_content_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"content">> => <<>>}
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(publish_message, Req, State),
            ?assertMatch({error_resp, <<"消息内容不能为空"/utf8>>}, Result)
        end
    ).

by_custom_id_uses_current_uid_for_role_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(custom_id, _Req) -> <<"tech_daily">> end}
            ]},
            {channel_logic, [
                {'get_channel_by_custom_id', 2, fun(<<"tech_daily">>, 1001) ->
                    {ok, #{
                        <<"id">> => <<"ch_1">>,
                        user_role => 3,
                        is_subscribed => true
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Channel) -> {ok_resp, Channel} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},

            Result = channel_handler:handle_action(by_custom_id, Req, State),
            ?assertMatch({ok_resp, #{user_role := 3, is_subscribed := true}}, Result)
        end
    ).

mark_read_uses_path_channel_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"message_id">> => <<"msg_100">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'mark_as_read', 3, fun(1001, <<"ch_hash_path">>, <<"msg_100">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(mark_read, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

revoke_message_uses_path_params_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        message_id -> <<"msg_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'revoke_message', 3, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(revoke_message, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

revoke_message_returns_error_when_message_id_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        message_id -> undefined;
                        _ -> undefined
                    end
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(revoke_message, Req, State),
            ?assertEqual({error_resp, <<"消息ID不能为空"/utf8>>}, Result)
        end
    ).

messages_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"cursor">>, <<"10">>}, {<<"limit">>, <<"30">>}]
                end}
            ]},
            {channel_logic, [
                {'get_messages', 4, fun(1001, <<"ch_hash_path">>, 10, 30) ->
                    {ok, [#{<<"id">> => <<"msg_100">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(messages, Req, State),
            ?assertMatch({ok_resp, #{list := [#{<<"id">> := <<"msg_100">>}]}}, Result)
        end
    ).

messages_access_denied_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {channel_logic, [
                {'get_messages', 4, fun(1001, <<"ch_hash_path">>, 0, 20) ->
                    {error, <<"付费频道需要先购买"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(messages, Req, State),
            ?assertEqual({error_resp, <<"付费频道需要先购买"/utf8>>}, Result)
        end
    ).

remove_admin_uses_path_params_on_delete_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"DELETE">> end},
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"2002">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'remove_admin', 3, fun(1001, <<"ch_hash_path">>, 2002) -> ok end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(remove_admin, Req, State),
            ?assertEqual({ok_resp, success, #{}}, Result)
        end
    ).

remove_admin_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"DELETE">> end},
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"uid_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'remove_admin', 3, fun(_, _, _) ->
                    erlang:error(should_not_call_remove_admin_when_user_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(remove_admin, Req, State),
            ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, remove_admin, 3))
        end
    ).

remove_admin_put_delegates_to_update_admin_role_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"role">> => 2}
                end}
            ]},
            {cowboy_req, [
                {'method', 1, fun(_Req) -> <<"PUT">> end},
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"2002">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'update_admin_role', 4, fun(1001, <<"ch_hash_path">>, 2002, 2) -> ok end},
                {'remove_admin', 3, fun(_, _, _) -> erlang:error(should_not_call_remove_admin) end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(remove_admin, Req, State),
            ?assertEqual({ok_resp, success, #{}}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, remove_admin, 3))
        end
    ).

update_admin_role_prefers_path_params_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"channel_id">> => <<"ch_hash_body">>,
                        <<"user_id">> => <<"uid_hash_body">>,
                        <<"role">> => <<"3">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"2002">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'update_admin_role', 4, fun(1001, <<"ch_hash_path">>, 2002, 3) -> ok end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(update_admin_role, Req, State),
            ?assertEqual({ok_resp, success, #{}}, Result)
        end
    ).

update_admin_role_without_user_id_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"role">> => <<"2">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> undefined;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'update_admin_role', 4, fun(_, _, _, _) ->
                    erlang:error(should_not_call_update_admin_role)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(update_admin_role, Req, State),
            ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, update_admin_role, 4))
        end
    ).

update_admin_role_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"role">> => <<"2">>}
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"uid_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'update_admin_role', 4, fun(_, _, _, _) ->
                    erlang:error(should_not_call_update_admin_role_when_user_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(update_admin_role, Req, State),
            ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, update_admin_role, 4))
        end
    ).

create_invitation_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"channel_id">> => <<"ch_hash_body">>,
                        <<"invitee_uid">> => <<"2002">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'create_invitation', 3, fun(1001, <<"ch_hash_path">>, 2002) ->
                    {ok, #{<<"id">> => <<"inv_1">>}}
                end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(create_invitation, Req, State),
            ?assertMatch({ok_resp, success, #{<<"id">> := <<"inv_1">>}}, Result)
        end
    ).

create_invitation_without_invitee_uid_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'create_invitation', 3, fun(_, _, _) ->
                    erlang:error(should_not_call_create_invitation)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(create_invitation, Req, State),
            ?assertEqual({error_resp, <<"被邀请人ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, create_invitation, 3))
        end
    ).

create_invitation_returns_error_when_invitee_uid_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitee_uid">> => <<"uid_hash_path">>} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'create_invitation', 3, fun(_, _, _) ->
                    erlang:error(should_not_call_create_invitation_when_invitee_uid_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(create_invitation, Req, State),
            ?assertEqual({error_resp, <<"被邀请人ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, create_invitation, 3))
        end
    ).

create_order_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"channel_id">> => <<"ch_hash_body">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'create_order', 3, fun(1001, <<"ch_hash_path">>, <<"wallet">>) ->
                    {ok, #{<<"order_no">> => <<"ORD001">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Order) -> {ok_resp, Order} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(create_order, Req, State),
            ?assertMatch({ok_resp, #{<<"order_no">> := <<"ORD001">>}}, Result)
        end
    ).

pay_order_without_order_no_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {channel_logic, [
                {'pay_order', 2, fun(_, _) ->
                    erlang:error(should_not_call_pay_order)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(pay_order, Req, State),
            ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, pay_order, 2))
        end
    ).

pay_order_with_whitespace_order_no_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"order_no">> => <<"   ">>} end}
            ]},
            {channel_logic, [
                {'pay_order', 2, fun(_, _) ->
                    erlang:error(should_not_call_pay_order_with_blank_order_no)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(pay_order, Req, State),
            ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, pay_order, 2))
        end
    ).

pay_order_normalizes_integer_order_no_before_logic_call_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"order_no">> => 12345} end}
            ]},
            {channel_logic, [
                {'pay_order', 2, fun(1001, <<"12345">>) -> {ok, #{}} end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(pay_order, Req, State),
            ?assertEqual({ok_resp, #{}}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, pay_order, 2))
        end
    ).

pay_order_normalizes_list_order_no_before_logic_call_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"order_no">> => "  ORD-LIST-01  "} end}
            ]},
            {channel_logic, [
                {'pay_order', 2, fun(1001, <<"ORD-LIST-01">>) -> {ok, #{}} end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(pay_order, Req, State),
            ?assertEqual({ok_resp, #{}}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, pay_order, 2))
        end
    ).

my_orders_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_my_orders', 1, fun(1001) ->
                    {ok, [#{<<"order_no">> => <<"ORD001">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(my_orders, Req, State),
            ?assertMatch({ok_resp, #{list := [#{<<"order_no">> := <<"ORD001">>}]}}, Result)
        end
    ).

my_orders_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_my_orders', 1, fun(1001) ->
                    {error, <<"db_down">>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(my_orders, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

remove_subscriber_uses_path_params_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"2002">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'remove_subscriber', 3, fun(1001, <<"ch_hash_path">>, 2002) -> ok end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(remove_subscriber, Req, State),
            ?assertEqual({ok_resp, success, #{}}, Result)
        end
    ).

remove_subscriber_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        user_id -> <<"uid_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'remove_subscriber', 3, fun(_, _, _) ->
                    erlang:error(should_not_call_remove_subscriber_when_user_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(remove_subscriber, Req, State),
            ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, remove_subscriber, 3))
        end
    ).

add_admin_invalid_role_returns_contract_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"user_id">> => <<"uid_hash_path">>,
                        <<"role">> => <<"0">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'add_admin', 4, fun(_, _, _, _) ->
                    erlang:error(should_not_call_add_admin)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(add_admin, Req, State),
            ?assertEqual({error_resp, <<"角色值必须在1-3之间"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, add_admin, 4))
        end
    ).

add_admin_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"user_id">> => <<"uid_hash_path">>,
                        <<"role">> => <<"2">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'add_admin', 4, fun(_, _, _, _) ->
                    erlang:error(should_not_call_add_admin_when_user_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(add_admin, Req, State),
            ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, add_admin, 4))
        end
    ).

remove_reaction_prefers_path_reaction_type_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"channel_id">> => <<"ch_hash_body">>,
                        <<"message_id">> => <<"msg_hash_body">>,
                        <<"reaction_type">> => <<"angry">>
                    }
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        message_id -> <<"msg_hash_path">>;
                        reaction_type -> <<"like">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'remove_reaction', 4, fun(
                    1001, <<"ch_hash_path">>, <<"msg_hash_path">>, <<"like">>
                ) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(remove_reaction, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

messages_private_channel_access_denied_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {channel_logic, [
                {'get_messages', 4, fun(1001, <<"ch_hash_path">>, 0, 20) ->
                    {error, <<"私有频道仅限订阅用户访问"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(messages, Req, State),
            ?assertEqual({error_resp, <<"私有频道仅限订阅用户访问"/utf8>>}, Result)
        end
    ).

show_uses_path_channel_id_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'get_channel', 2, fun(<<"ch_hash_path">>, 1001) ->
                    {ok, #{<<"id">> => <<"ch_hash_path">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(show, Req, State),
            ?assertEqual({ok_resp, #{<<"id">> => <<"ch_hash_path">>}}, Result)
        end
    ).

subscribe_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"channel_id">> => <<"ch_hash_body">>} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'subscribe', 2, fun(1001, <<"ch_hash_path">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(subscribe, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

unsubscribe_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"channel_id">> => <<"ch_hash_body">>} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'unsubscribe', 2, fun(1001, <<"ch_hash_path">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(unsubscribe, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

subscribed_returns_list_with_cursor_and_limit_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"cursor">>, <<"15">>}, {<<"limit">>, <<"60">>}]
                end}
            ]},
            {channel_logic, [
                {'get_subscribed_channels', 1, fun(1001) ->
                    {ok, [#{<<"id">> => <<"ch_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(subscribed, Req, State),
            ?assertEqual(
                {ok_resp, #{list => [#{<<"id">> => <<"ch_1">>}], cursor => 15, limit => 60}},
                Result
            )
        end
    ).

subscribed_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {channel_logic, [
                {'get_subscribed_channels', 1, fun(1001) ->
                    {error, db_down}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(subscribed, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

managed_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_managed_channels', 1, fun(1001) ->
                    {ok, [#{<<"id">> => <<"ch_admin_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(managed, Req, State),
            ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"ch_admin_1">>}]}}, Result)
        end
    ).

managed_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_managed_channels', 1, fun(1001) ->
                    {error, <<"db_down">>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(managed, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

stats_uses_path_channel_id_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic_subscription, [
                {'is_subscribed', 2, fun(_, _) -> true end}
            ]},
            {channel_logic, [
                {'get_channel_stats', 2, fun(1001, <<"ch_hash_path">>) ->
                    {ok, #{<<"subscriber_count">> => 42}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(stats, Req, State),
            ?assertEqual({ok_resp, #{<<"subscriber_count">> => 42}}, Result)
        end
    ).

stats_daily_parses_days_query_and_passes_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
                {'parse_qs', 1, fun(_Req) -> [{<<"days">>, <<"30">>}] end}
            ]},
            {channel_logic, [
                {'get_daily_stats', 3, fun(1001, <<"ch_hash_path">>, 30) ->
                    {ok, [#{<<"stats_date">> => <<"2026-02-23">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(stats_daily, Req, State),
            ?assertEqual({ok_resp, #{list => [#{<<"stats_date">> => <<"2026-02-23">>}]}}, Result)
        end
    ).

subscribers_parses_cursor_and_limit_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"cursor">>, <<"99">>}, {<<"limit">>, <<"10">>}]
                end}
            ]},
            {channel_logic_subscription, [
                {'is_subscribed', 2, fun(_, _) -> true end}
            ]},
            {channel_logic, [
                {'get_subscribers', 3, fun(<<"ch_hash_path">>, 99, 10) ->
                    {ok, [#{<<"user_id">> => <<"u_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(subscribers, Req, State),
            ?assertEqual(
                {ok_resp, #{list => [#{<<"user_id">> => <<"u_1">>}], cursor => 99, limit => 10}},
                Result
            )
        end
    ).

sync_parses_since_and_passes_current_uid_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"since">>, <<"1700000000">>}] end}
            ]},
            {channel_logic, [
                {'sync_channels', 2, fun(1001, 1700000000) ->
                    {ok, #{channels => [], server_time => 1700000001000}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(sync, Req, State),
            ?assertEqual({ok_resp, #{channels => [], server_time => 1700000001000}}, Result)
        end
    ).

create_without_name_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"name">> => <<>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(create, Req, State),
            ?assertEqual({error_resp, <<"频道名称不能为空"/utf8>>}, Result)
        end
    ).

update_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"channel_id">> => <<"ch_hash_body">>, <<"name">> => <<"new_name">>}
                end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'update_channel', 3, fun(
                    1001, <<"ch_hash_path">>, #{<<"name">> := <<"new_name">>}
                ) ->
                    {ok, #{<<"id">> => <<"ch_hash_path">>, <<"name">> => <<"new_name">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(update, Req, State),
            ?assertMatch({ok_resp, #{<<"name">> := <<"new_name">>}}, Result)
        end
    ).

delete_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"channel_id">> => <<"ch_hash_body">>} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic, [
                {'delete_channel', 2, fun(1001, <<"ch_hash_path">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(delete, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

search_with_empty_keyword_returns_empty_list_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [] end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            Result = channel_handler:handle_action(search, Req, #{}),
            ?assertEqual({ok_resp, #{list => []}}, Result)
        end
    ).

search_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"keyword">>, <<"ops">>}] end}
            ]},
            {channel_logic, [
                {'search_channels', 2, fun(<<"ops">>, 20) ->
                    {error, timeout}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            Result = channel_handler:handle_action(search, Req, #{}),
            ?assertEqual({error_resp, <<"timeout">>}, Result)
        end
    ).

discover_passes_limit_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"limit">>, <<"15">>}] end}
            ]},
            {channel_logic, [
                {'get_discover_channels', 1, fun(15) ->
                    {ok, [#{<<"id">> => <<"ch_discover_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            Result = channel_handler:handle_action(discover, Req, #{}),
            ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"ch_discover_1">>}]}}, Result)
        end
    ).

discover_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"limit">>, <<"15">>}] end}
            ]},
            {channel_logic, [
                {'get_discover_channels', 1, fun(15) ->
                    {error, <<"db_down">>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            Result = channel_handler:handle_action(discover, Req, #{}),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

record_view_uses_path_params_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        message_id -> <<"msg_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'record_message_view', 3, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(record_view, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

add_reaction_uses_path_params_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(Key, _Req) ->
                    case Key of
                        channel_id -> <<"ch_hash_path">>;
                        message_id -> <<"msg_hash_path">>;
                        _ -> undefined
                    end
                end}
            ]},
            {channel_logic, [
                {'add_reaction', 4, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>, <<"like">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(add_reaction, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

pin_message_uses_path_message_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"pinned">> => true} end}
            ]},
            {cowboy_req, [
                {'binding', 2, fun(message_id, _Req) -> <<"msg_hash_path">> end}
            ]},
            {channel_logic, [
                {'pin_message', 3, fun(1001, <<"msg_hash_path">>, true) ->
                    {ok, #{<<"id">> => <<"msg_hash_path">>, <<"pinned">> => true}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(pin_message, Req, State),
            ?assertMatch({ok_resp, #{<<"pinned">> := true}}, Result)
        end
    ).

delete_message_uses_path_message_id_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(message_id, _Req) -> <<"msg_hash_path">> end}
            ]},
            {channel_logic, [
                {'delete_message', 2, fun(1001, <<"msg_hash_path">>) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(delete_message, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

delete_message_returns_error_when_message_id_missing_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(message_id, _Req) -> undefined end}
            ]},
            {channel_logic, [
                {'delete_message', 2, fun(_, _) ->
                    erlang:error(should_not_call_delete_message_when_message_id_missing)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(delete_message, Req, State),
            ?assertEqual({error_resp, <<"消息ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, delete_message, 2))
        end
    ).

delete_message_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(message_id, _Req) -> <<"msg_hash_path">> end}
            ]},
            {channel_logic, [
                {'delete_message', 2, fun(1001, <<"msg_hash_path">>) ->
                    {error, <<"无权限删除此消息"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_message:handle_action(delete_message, Req, State),
            ?assertEqual({error_resp, <<"无权限删除此消息"/utf8>>}, Result)
        end
    ).

get_order_uses_path_order_no_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> <<"ORD001">> end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(1001, <<"ORD001">>) ->
                    {ok, #{<<"order_no">> => <<"ORD001">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({ok_resp, #{<<"order_no">> => <<"ORD001">>}}, Result)
        end
    ).

get_order_returns_error_when_order_no_missing_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> undefined end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(_, _) ->
                    erlang:error(should_not_call_get_order_when_order_no_missing)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, get_order, 2))
        end
    ).

get_order_returns_error_when_order_no_blank_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> <<"   ">> end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(_, _) ->
                    erlang:error(should_not_call_get_order_with_blank_order_no)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, get_order, 2))
        end
    ).

get_order_normalizes_integer_order_no_before_logic_call_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> 12345 end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(1001, <<"12345">>) ->
                    {ok, #{<<"order_no">> => <<"12345">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({ok_resp, #{<<"order_no">> => <<"12345">>}}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
        end
    ).

get_order_normalizes_list_order_no_before_logic_call_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> "  ORD-LIST-GET  " end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(1001, <<"ORD-LIST-GET">>) ->
                    {ok, #{<<"order_no">> => <<"ORD-LIST-GET">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({ok_resp, #{<<"order_no">> => <<"ORD-LIST-GET">>}}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
        end
    ).

get_order_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(order_no, _Req) -> <<"ORD_DB_ERR">> end}
            ]},
            {channel_logic, [
                {'get_order', 2, fun(1001, <<"ORD_DB_ERR">>) ->
                    {error, <<"db_down">>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_order:handle_action(get_order, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
        end
    ).

admins_uses_path_channel_id_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
            ]},
            {channel_logic_subscription, [
                {'is_subscribed', 2, fun(_, _) -> true end}
            ]},
            {channel_logic, [
                {'get_admins', 1, fun(<<"ch_hash_path">>) ->
                    {ok, [#{<<"user_id">> => <<"uid_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            Result = channel_handler_admin:handle_action(admins, Req, #{}),
            ?assertEqual({ok_resp, #{list => [#{<<"user_id">> => <<"uid_1">>}]}}, Result)
        end
    ).

accept_invitation_decodes_invitation_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'accept_invitation', 2, fun(1001, 501) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(accept_invitation, Req, State),
            ?assertEqual({ok_resp, #{}}, Result)
        end
    ).

accept_invitation_returns_error_when_invitation_id_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {channel_logic, [
                {'accept_invitation', 2, fun(_, _) ->
                    erlang:error(should_not_call_accept_invitation)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(accept_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, accept_invitation, 2))
        end
    ).

accept_invitation_returns_error_when_invitation_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
            ]},
            {channel_logic, [
                {'accept_invitation', 2, fun(_, _) ->
                    erlang:error(should_not_call_accept_invitation_when_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(accept_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, accept_invitation, 2))
        end
    ).

accept_invitation_returns_logic_error_message_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'accept_invitation', 2, fun(1001, 501) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(accept_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请不存在或已过期"/utf8>>}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, accept_invitation, 2))
        end
    ).

accept_invitation_retry_is_idempotent_at_handler_boundary_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'accept_invitation', 2, fun(1001, 501) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            First = channel_handler_admin:handle_action(accept_invitation, Req, State),
            Second = channel_handler_admin:handle_action(accept_invitation, Req, State),
            ?assertEqual({ok_resp, #{}}, First),
            ?assertEqual({ok_resp, #{}}, Second),
            ?assertEqual(2, meck:num_calls(channel_logic, accept_invitation, 2))
        end
    ).

reject_invitation_decodes_invitation_id_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'reject_invitation', 2, fun(1001, 501) -> ok end}
            ]},
            {elib_response, meck_helper:full_elib_response_mock(ok_resp)}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(reject_invitation, Req, State),
            ?assertEqual({ok_resp, success, #{}}, Result)
        end
    ).

reject_invitation_returns_error_when_invitation_id_missing_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{} end}
            ]},
            {channel_logic, [
                {'reject_invitation', 2, fun(_, _) ->
                    erlang:error(should_not_call_reject_invitation)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(reject_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, reject_invitation, 2))
        end
    ).

reject_invitation_returns_error_when_invitation_id_decode_unexpected_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
            ]},
            {channel_logic, [
                {'reject_invitation', 2, fun(_, _) ->
                    erlang:error(should_not_call_reject_invitation_when_id_invalid)
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(reject_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
            ?assertEqual(0, meck:num_calls(channel_logic, reject_invitation, 2))
        end
    ).

reject_invitation_returns_logic_error_message_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'reject_invitation', 2, fun(1001, 501) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(reject_invitation, Req, State),
            ?assertEqual({error_resp, <<"邀请不存在或已过期"/utf8>>}, Result),
            ?assertEqual(1, meck:num_calls(channel_logic, reject_invitation, 2))
        end
    ).

reject_invitation_retry_is_idempotent_at_handler_boundary_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"501">>} end}
            ]},
            {channel_logic, [
                {'reject_invitation', 2, fun(1001, 501) -> ok end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            First = channel_handler_admin:handle_action(reject_invitation, Req, State),
            Second = channel_handler_admin:handle_action(reject_invitation, Req, State),
            ?assertEqual({ok_resp, #{}}, First),
            ?assertEqual({ok_resp, #{}}, Second),
            ?assertEqual(2, meck:num_calls(channel_logic, reject_invitation, 2))
        end
    ).

my_invitations_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_my_invitations', 1, fun(1001) ->
                    {ok, [#{<<"id">> => <<"inv_1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(my_invitations, Req, State),
            ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"inv_1">>}]}}, Result)
        end
    ).

my_invitations_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_my_invitations', 1, fun(1001) ->
                    {error, db_down}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(my_invitations, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

sent_invitations_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_sent_invitations', 1, fun(1001) ->
                    {ok, [#{<<"id">> => <<"inv_2">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(sent_invitations, Req, State),
            ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"inv_2">>}]}}, Result)
        end
    ).

sent_invitations_propagates_logic_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_sent_invitations', 1, fun(1001) ->
                    {error, <<"db_down">>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler_admin:handle_action(sent_invitations, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

unread_summary_returns_payload_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_unread_summary', 1, fun(1001) ->
                    {ok, #{
                        <<"total_unread">> => 5,
                        <<"unread_channels">> => 2,
                        <<"channels">> => [
                            #{<<"channel_id">> => <<"ch_1">>, <<"unread_count">> => 3},
                            #{<<"channel_id">> => <<"ch_2">>, <<"unread_count">> => 2}
                        ]
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(unread_summary, Req, State),
            ?assertMatch(
                {ok_resp, #{
                    <<"total_unread">> := 5,
                    <<"unread_channels">> := 2,
                    <<"channels">> := [_, _]
                }},
                Result
            )
        end
    ).

unread_summary_normalizes_non_binary_error_test_() ->
    ?WITH_MECKS(
        [
            {channel_logic, [
                {'get_unread_summary', 1, fun(1001) ->
                    {error, db_down}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
            ]}
        ],
        fun() ->
            Req = req_mock(),
            State = #{current_uid => 1001},
            Result = channel_handler:handle_action(unread_summary, Req, State),
            ?assertEqual({error_resp, <<"db_down">>}, Result)
        end
    ).

handle_action_false_returns_original_req_test() ->
    Req = req_mock(),
    ?assertEqual(Req, channel_handler:handle_action(false, Req, #{})).

req_mock() ->
    #{mock_req => true}.
