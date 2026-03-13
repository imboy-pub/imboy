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
    ?WITH_MECKS([
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
            {'publish_message', 5, fun(1001, <<"ch_hash_path">>, <<"频道公告"/utf8>>, <<"text">>, #{<<"k">> := <<"v">>}) ->
                {ok, #{<<"id">> => <<"msg_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Msg) -> {ok_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},

        Result = channel_handler:handle_action(publish_message, Req, State),
        ?assertMatch({ok_resp, #{<<"id">> := <<"msg_1">>}}, Result)
    end).

publish_message_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
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
            {'publish_message', 5, fun(1001, <<"ch_hash_path">>, <<"body优先"/utf8>>, <<"text">>, #{}) ->
                {ok, #{<<"id">> => <<"msg_2">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Msg) -> {ok_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},

        Result = channel_handler:handle_action(publish_message, Req, State),
        ?assertMatch({ok_resp, #{<<"id">> := <<"msg_2">>}}, Result)
    end).

publish_message_without_channel_id_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},

        Result = channel_handler:handle_action(publish_message, Req, State),
        ?assertMatch({error_resp, <<"频道ID不能为空"/utf8>>}, Result)
    end).

publish_message_with_empty_content_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},

        Result = channel_handler:handle_action(publish_message, Req, State),
        ?assertMatch({error_resp, <<"消息内容不能为空"/utf8>>}, Result)
    end).

by_custom_id_uses_current_uid_for_role_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},

        Result = channel_handler:handle_action(by_custom_id, Req, State),
        ?assertMatch({ok_resp, #{user_role := 3, is_subscribed := true}}, Result)
    end).

mark_read_uses_path_channel_id_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(mark_read, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

revoke_message_uses_path_params_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(revoke_message, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

revoke_message_returns_error_when_message_id_missing_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(revoke_message, Req, State),
        ?assertEqual({error_resp, <<"消息ID不能为空"/utf8>>}, Result)
    end).

messages_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(messages, Req, State),
        ?assertMatch({ok_resp, #{list := [#{<<"id">> := <<"msg_100">>}]}}, Result)
    end).

messages_access_denied_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(messages, Req, State),
        ?assertEqual({error_resp, <<"付费频道需要先购买"/utf8>>}, Result)
    end).

remove_admin_uses_path_params_on_delete_test_() ->
    ?WITH_MECKS([
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'remove_admin', 3, fun(1001, <<"ch_hash_path">>, 2002) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_admin, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

remove_admin_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> invalid_uid_decode end}
        ]},
        {channel_logic, [
            {'remove_admin', 3, fun(_, _, _) -> erlang:error(should_not_call_remove_admin_when_user_id_invalid) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_admin, Req, State),
        ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, remove_admin, 3))
    end).

remove_admin_put_delegates_to_update_admin_role_test_() ->
    ?WITH_MECKS([
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
                    user_id -> <<"uid_hash_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'update_admin_role', 4, fun(1001, <<"ch_hash_path">>, 2002, 2) -> ok end},
            {'remove_admin', 3, fun(_, _, _) -> erlang:error(should_not_call_remove_admin) end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_admin, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, remove_admin, 3))
    end).

update_admin_role_prefers_path_params_over_body_test_() ->
    ?WITH_MECKS([
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
                    user_id -> <<"uid_hash_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'update_admin_role', 4, fun(1001, <<"ch_hash_path">>, 2002, 3) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

update_admin_role_without_user_id_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, update_admin_role, 4))
    end).

update_admin_role_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> invalid_uid_decode end}
        ]},
        {channel_logic, [
            {'update_admin_role', 4, fun(_, _, _, _) ->
                erlang:error(should_not_call_update_admin_role_when_user_id_invalid)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, update_admin_role, 4))
    end).

create_invitation_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"channel_id">> => <<"ch_hash_body">>,
                    <<"invitee_uid">> => <<"uid_hash_path">>
                }
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'create_invitation', 3, fun(1001, <<"ch_hash_path">>, 2002) ->
                {ok, #{<<"id">> => <<"inv_1">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Invitation) -> {ok_resp, Invitation} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertMatch({ok_resp, #{<<"id">> := <<"inv_1">>}}, Result)
    end).

create_invitation_without_invitee_uid_returns_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<>>) -> 0 end}
        ]},
        {channel_logic, [
            {'create_invitation', 3, fun(_, _, _) ->
                erlang:error(should_not_call_create_invitation)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"被邀请人ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, create_invitation, 3))
    end).

create_invitation_returns_error_when_invitee_uid_decode_unexpected_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitee_uid">> => <<"uid_hash_path">>} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> invalid_uid_decode end}
        ]},
        {channel_logic, [
            {'create_invitation', 3, fun(_, _, _) ->
                erlang:error(should_not_call_create_invitation_when_invitee_uid_invalid)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"被邀请人ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, create_invitation, 3))
    end).

create_order_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
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
            {'create_order', 2, fun(1001, <<"ch_hash_path">>) ->
                {ok, #{<<"order_no">> => <<"ORD001">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Order) -> {ok_resp, Order} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertMatch({ok_resp, #{<<"order_no">> := <<"ORD001">>}}, Result)
    end).

pay_order_without_order_no_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, pay_order, 2))
    end).

pay_order_with_whitespace_order_no_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, pay_order, 2))
    end).

pay_order_normalizes_integer_order_no_before_logic_call_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"order_no">> => 12345} end}
        ]},
        {channel_logic, [
            {'pay_order', 2, fun(1001, <<"12345">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, pay_order, 2))
    end).

pay_order_normalizes_list_order_no_before_logic_call_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"order_no">> => "  ORD-LIST-01  "} end}
        ]},
        {channel_logic, [
            {'pay_order', 2, fun(1001, <<"ORD-LIST-01">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, pay_order, 2))
    end).

my_orders_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_my_orders', 1, fun(1001) ->
                {ok, [#{<<"order_no">> => <<"ORD001">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(my_orders, Req, State),
        ?assertMatch({ok_resp, #{list := [#{<<"order_no">> := <<"ORD001">>}]}}, Result)
    end).

my_orders_propagates_logic_error_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_my_orders', 1, fun(1001) ->
                {error, <<"db_down">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(my_orders, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

remove_subscriber_uses_path_params_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"ch_hash_path">>;
                    user_id -> <<"uid_hash_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'remove_subscriber', 3, fun(1001, <<"ch_hash_path">>, 2002) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

remove_subscriber_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"ch_hash_path">>;
                    user_id -> <<"uid_hash_path">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> invalid_uid_decode end}
        ]},
        {channel_logic, [
            {'remove_subscriber', 3, fun(_, _, _) ->
                erlang:error(should_not_call_remove_subscriber_when_user_id_invalid)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, remove_subscriber, 3))
    end).

create_invitation_handler_logic_repo_chain_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{
                    <<"invitee_uid">> => InviteeUidBin
                }
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
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
                ?assertEqual(ChannelIdBin, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertMatch({ok_resp, _}, Result),
        {ok_resp, Invitation} = Result,
        ?assertEqual(11, elib_hashids:decode(maps:get(<<"channel_id">>, Invitation))),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_channel_not_found_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> {error, not_found} end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_call_create_invitation) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"频道不存在"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_channel_disabled_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 0}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_call_create_invitation) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"频道已禁用或删除"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_non_private_channel_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(_, _, _) -> erlang:error(should_not_call_create_invitation) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"只有私有频道支持邀请功能"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_inviter_not_subscribed_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 1, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_invitation', 3, fun(11, 1001, 2002) ->
                {error, <<"您不是频道订阅者，无法邀请他人"/utf8>>}
            end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) -> erlang:error(should_not_call_find_by_id) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"您不是频道订阅者，无法邀请他人"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_ds_binary_error_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
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
            {'find_by_id', 1, fun(_) -> erlang:error(should_not_call_find_by_id) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请创建失败"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_ds_atom_error_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
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
            {'find_by_id', 1, fun(_) -> erlang:error(should_not_call_find_by_id) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertMatch({error_resp, _}, Result),
        {error_resp, ErrMsg} = Result,
        ?assert(is_binary(ErrMsg)),
        ?assert(ErrMsg =/= <<>>),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

create_invitation_handler_logic_repo_chain_invitation_load_failed_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    InviteeUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitee_uid">> => InviteeUidBin}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
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
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_invitation, Req, State),
        ?assertMatch({error_resp, _}, Result),
        {error_resp, ErrMsg} = Result,
        ?assert(is_binary(ErrMsg)),
        ?assert(ErrMsg =/= <<>>),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_invitation, 3)),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

accept_invitation_handler_logic_ds_chain_already_accepted_is_success_and_silent_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitation_id">> => <<"inv_hash_1">>}
            end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(501, 1001) -> {error, already_accepted} end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(_) -> erlang:error(should_not_call_find_by_id_when_already_accepted) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_send_duplicate_invitation_notify) end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
        ?assertEqual(0, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

accept_invitation_handler_logic_ds_chain_notify_crash_still_returns_success_test_() ->
    InvitationIdBin = elib_hashids:encode(513),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"invitation_id">> => InvitationIdBin}
            end}
        ]},
        {channel_subscribe_ds, [
            {'accept_invitation', 2, fun(513, 1001) -> ok end}
        ]},
        {channel_invitation_repo, [
            {'find_by_id', 1, fun(513) ->
                {ok, #{
                    <<"id">> => 513,
                    <<"channel_id">> => 11,
                    <<"inviter_uid">> => 2002,
                    <<"invitee_uid">> => 1001
                }}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, _Uids, _Action, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, accept_invitation, 2)),
        ?assertEqual(1, meck:num_calls(channel_invitation_repo, find_by_id, 1)),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

create_order_handler_logic_repo_chain_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 1001, #{}) -> {ok, <<"ORD001">>} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"id">> => 601,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001,
                    <<"order_no">> => <<"ORD001">>
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertMatch({ok_resp, _}, Result),
        {ok_resp, Order} = Result,
        ?assertEqual(11, elib_hashids:decode(maps:get(<<"channel_id">>, Order))),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_order, 3))
    end).

create_order_handler_logic_repo_chain_channel_not_found_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> {error, not_found} end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_call_create_order) end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertEqual({error_resp, <<"频道不存在"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_channel_lookup_error_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) -> {error, db_down} end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_call_create_order) end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_non_paid_channel_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 0, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_call_create_order) end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertEqual({error_resp, <<"只有付费频道支持购买"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_order_load_failed_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 1001, #{}) -> {ok, <<"ORD404">>} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD404">>) -> {error, not_found} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertMatch({error_resp, _}, Result),
        {error_resp, ErrMsg} = Result,
        ?assert(is_binary(ErrMsg)),
        ?assert(ErrMsg =/= <<>>),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(1, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_channel_disabled_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 0}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(_, _, _) -> erlang:error(should_not_call_create_order) end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertEqual({error_resp, <<"频道已禁用或删除"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_ds_binary_error_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 1001, #{}) -> {error, <<"订单创建失败"/utf8>>} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertEqual({error_resp, <<"订单创建失败"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

create_order_handler_logic_repo_chain_ds_atom_error_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_repo, [
            {'find_by_id', 2, fun(11, <<"id,type,status">>) ->
                #{<<"id">> => 11, <<"type">> => 2, <<"status">> => 1}
            end}
        ]},
        {channel_subscribe_ds, [
            {'create_order', 3, fun(11, 1001, #{}) -> {error, db_timeout} end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(_) -> erlang:error(should_not_call_find_by_order_no) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create_order, Req, State),
        ?assertMatch({error_resp, _}, Result),
        {error_resp, ErrMsg} = Result,
        ?assert(is_binary(ErrMsg)),
        ?assert(ErrMsg =/= <<>>),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, create_order, 3)),
        ?assertEqual(0, meck:num_calls(channel_order_repo, find_by_order_no, 1))
    end).

update_admin_role_handler_logic_repo_chain_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"role">> => <<"2">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end},
            {'update_role', 3, fun(11, 2002, 2) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_admin_repo, update_role, 3))
    end).

pay_order_handler_logic_repo_chain_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD001">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD001">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD001">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD001">>, PaymentData) ->
                ?assertEqual(<<"mock">>, maps:get(payment_method, PaymentData)),
                PaymentNo = maps:get(payment_no, PaymentData),
                ?assert(is_binary(PaymentNo)),
                ?assertEqual(<<"PAY">>, binary:part(PaymentNo, 0, 3)),
                ok
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], Action, <<>>, null, Payload, no_save) ->
                ?assert(lists:member(Action, [<<"channel_order_paid">>, <<"channel_subscribed">>])),
                ?assertEqual(ChannelIdBin, maps:get(<<"channel_id">>, Payload)),
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7))
    end).

remove_subscriber_handler_logic_repo_chain_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
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
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3)),
        ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
    end).

pay_order_handler_logic_repo_chain_owner_mismatch_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD002">>}
            end}
        ]},
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
            {'pay_order', 2, fun(_, _) -> erlang:error(should_not_call_pay_order) end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"无权操作此订单"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

pay_order_handler_logic_repo_chain_order_not_found_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD404">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD404">>) -> {error, not_found} end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) -> erlang:error(should_not_call_pay_order) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"订单不存在"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
    end).

pay_order_handler_logic_repo_chain_lookup_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD_DB_ERR">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD_DB_ERR">>) -> {error, db_down} end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(_, _) -> erlang:error(should_not_call_pay_order) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscribe_ds, pay_order, 2))
    end).

pay_order_handler_logic_repo_chain_ds_atom_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD003">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD003">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD003">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD003">>, _PaymentData) ->
                {error, db_timeout}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertMatch({error_resp, _}, Result),
        {error_resp, ErrMsg} = Result,
        ?assert(is_binary(ErrMsg)),
        ?assert(ErrMsg =/= <<>>),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

pay_order_handler_logic_repo_chain_ds_binary_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD005">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD005">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD005">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD005">>, _PaymentData) ->
                {error, <<"支付失败"/utf8>>}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) -> erlang:error(should_not_call_send) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"支付失败"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

pay_order_handler_logic_repo_chain_already_paid_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD007">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD007">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD007">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD007">>, _PaymentData) ->
                {error, already_paid}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_call_send_for_already_paid)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"订单已支付"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

pay_order_handler_logic_repo_chain_not_found_or_expired_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD008">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD008">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD008">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD008">>, _PaymentData) ->
                {error, not_found_or_expired}
            end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(_, _, _, _, _, _, _) ->
                erlang:error(should_not_call_send_for_not_found_or_expired)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({error_resp, <<"订单不存在或已过期"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(0, meck:num_calls(msg_s2c_ds, send, 7))
    end).

pay_order_handler_logic_repo_chain_notify_failed_still_returns_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD006">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD006">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD006">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD006">>, _PaymentData) -> ok end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], Action, <<>>, null, Payload, no_save) ->
                ?assertEqual(ChannelIdBin, maps:get(<<"channel_id">>, Payload)),
                case Action of
                    <<"channel_order_paid">> -> ok;
                    <<"channel_subscribed">> -> {error, <<"通知发送失败"/utf8>>}
                end
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end},
            {'success', 2, fun(_Req, Data) -> {ok_resp, Data} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

pay_order_handler_logic_repo_chain_notify_crash_still_returns_success_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"order_no">> => <<"ORD006_CRASH">>}
            end}
        ]},
        {channel_order_repo, [
            {'find_by_order_no', 1, fun(<<"ORD006_CRASH">>) ->
                {ok, #{
                    <<"order_no">> => <<"ORD006_CRASH">>,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 1001
                }}
            end}
        ]},
        {channel_subscribe_ds, [
            {'pay_order', 2, fun(<<"ORD006_CRASH">>, _PaymentData) -> ok end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001], _Action, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end},
            {'success', 2, fun(_Req, Data) -> {ok_resp, Data} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pay_order, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscribe_ds, pay_order, 2)),
        ?assertEqual(2, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

remove_subscriber_handler_logic_repo_chain_permission_denied_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 1 end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(_, _, _) -> erlang:error(should_not_call_delete) end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_call_increment) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({error_resp, <<"无权限操作，需要管理员及以上权限"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3))
    end).

remove_subscriber_handler_logic_repo_chain_delete_failed_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
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
            {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_call_increment) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({error_resp, <<"移除订阅者失败"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3))
    end).

remove_subscriber_handler_logic_repo_chain_noop_is_idempotent_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
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
            {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_call_increment) end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(Key) ->
                case Key of
                    {channel_subs, 11} -> ok;
                    {channel, 11} -> ok;
                    _ -> erlang:error({unexpected_cache_key, Key})
                end
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_subscriber, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
        ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1))
    end).

update_admin_role_handler_logic_repo_chain_permission_denied_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"role">> => <<"2">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 2 end},
            {'update_role', 3, fun(_, _, _) -> erlang:error(should_not_call_update_role) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({error_resp, <<"无权限操作，仅创建者可修改角色"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_admin_repo, update_role, 3))
    end).

update_admin_role_handler_logic_repo_chain_update_failed_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    TargetUidBin = elib_hashids:encode(2002),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"role">> => <<"2">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> ChannelIdBin;
                    user_id -> TargetUidBin;
                    _ -> undefined
                end
            end}
        ]},
        {channel_admin_repo, [
            {'get_role', 2, fun(11, 1001) -> 3 end},
            {'update_role', 3, fun(11, 2002, 2) -> {error, db_error} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update_admin_role, Req, State),
        ?assertEqual({error_resp, <<"更新角色失败"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_admin_repo, update_role, 3))
    end).

add_admin_invalid_role_returns_contract_error_test_() ->
    ?WITH_MECKS([
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> 2002 end}
        ]},
        {channel_logic, [
            {'add_admin', 4, fun(_, _, _, _) ->
                erlang:error(should_not_call_add_admin)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(add_admin, Req, State),
        ?assertEqual({error_resp, <<"角色值必须在1-3之间"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, add_admin, 4))
    end).

add_admin_returns_error_when_user_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
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
        {elib_hashids, [
            {'decode', 1, fun(<<"uid_hash_path">>) -> invalid_uid_decode end}
        ]},
        {channel_logic, [
            {'add_admin', 4, fun(_, _, _, _) ->
                erlang:error(should_not_call_add_admin_when_user_id_invalid)
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(add_admin, Req, State),
        ?assertEqual({error_resp, <<"用户ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, add_admin, 4))
    end).

remove_reaction_prefers_path_reaction_type_over_body_test_() ->
    ?WITH_MECKS([
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
            {'remove_reaction', 4, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>, <<"like">>) ->
                ok
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(remove_reaction, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

messages_private_channel_access_denied_returns_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(messages, Req, State),
        ?assertEqual({error_resp, <<"私有频道仅限订阅用户访问"/utf8>>}, Result)
    end).

show_uses_path_channel_id_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(show, Req, State),
        ?assertEqual({ok_resp, #{<<"id">> => <<"ch_hash_path">>}}, Result)
    end).

subscribe_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(subscribe, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

unsubscribe_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(unsubscribe, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

subscribed_returns_list_with_cursor_and_limit_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [{<<"cursor">>, <<"15">>}, {<<"limit">>, <<"60">>}] end}
        ]},
        {channel_logic, [
            {'get_subscribed_channels', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"ch_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(subscribed, Req, State),
        ?assertEqual(
            {ok_resp, #{list => [#{<<"id">> => <<"ch_1">>}], cursor => 15, limit => 60}},
            Result
        )
    end).

subscribed_propagates_logic_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(subscribed, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

managed_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_managed_channels', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"ch_admin_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(managed, Req, State),
        ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"ch_admin_1">>}]}}, Result)
    end).

managed_propagates_logic_error_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_managed_channels', 1, fun(1001) ->
                {error, <<"db_down">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(managed, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

stats_uses_path_channel_id_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {channel_logic, [
            {'get_channel_stats', 1, fun(<<"ch_hash_path">>) ->
                {ok, #{<<"subscriber_count">> => 42}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(stats, Req, State),
        ?assertEqual({ok_resp, #{<<"subscriber_count">> => 42}}, Result)
    end).

stats_daily_parses_days_query_and_passes_to_logic_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
            {'parse_qs', 1, fun(_Req) -> [{<<"days">>, <<"30">>}] end}
        ]},
        {channel_logic, [
            {'get_daily_stats', 2, fun(<<"ch_hash_path">>, 30) ->
                {ok, [#{<<"stats_date">> => <<"2026-02-23">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(stats_daily, Req, State),
        ?assertEqual({ok_resp, #{list => [#{<<"stats_date">> => <<"2026-02-23">>}]}}, Result)
    end).

subscribers_parses_cursor_and_limit_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end},
            {'parse_qs', 1, fun(_Req) -> [{<<"cursor">>, <<"99">>}, {<<"limit">>, <<"10">>}] end}
        ]},
        {channel_logic, [
            {'get_subscribers', 3, fun(<<"ch_hash_path">>, 99, 10) ->
                {ok, [#{<<"user_id">> => <<"u_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(subscribers, Req, State),
        ?assertEqual(
            {ok_resp, #{list => [#{<<"user_id">> => <<"u_1">>}], cursor => 99, limit => 10}},
            Result
        )
    end).

sync_parses_since_and_passes_current_uid_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(sync, Req, State),
        ?assertEqual({ok_resp, #{channels => [], server_time => 1700000001000}}, Result)
    end).

create_without_name_returns_error_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"name">> => <<>>} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(create, Req, State),
        ?assertEqual({error_resp, <<"频道名称不能为空"/utf8>>}, Result)
    end).

update_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"channel_id">> => <<"ch_hash_body">>, <<"name">> => <<"new_name">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {channel_logic, [
            {'update_channel', 3, fun(1001, <<"ch_hash_path">>, #{<<"name">> := <<"new_name">>}) ->
                {ok, #{<<"id">> => <<"ch_hash_path">>, <<"name">> => <<"new_name">>}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update, Req, State),
        ?assertMatch({ok_resp, #{<<"name">> := <<"new_name">>}}, Result)
    end).

update_handler_logic_repo_chain_notify_crash_still_returns_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"name">> => <<"new_name">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_repo, [
            {'update', 2, fun(11, Data) ->
                ?assertEqual(<<"new_name">>, maps:get(<<"name">>, Data)),
                ?assert(maps:is_key(updated_at, Data)),
                {ok, 1}
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{
                    <<"id">> => 11,
                    <<"creator_uid">> => 1001,
                    <<"name">> => <<"new_name">>
                }
            end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_updated">>, <<>>, null, _Payload, no_save) ->
                erlang:error(mock_notify_crash)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update, Req, State),
        ?assertMatch({ok_resp, #{<<"name">> := <<"new_name">>}}, Result),
        ?assertEqual(1, meck:num_calls(channel_repo, update, 2)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

update_handler_logic_repo_chain_notify_failed_still_returns_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) ->
                #{<<"name">> => <<"new_name">>}
            end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 2 end}
        ]},
        {channel_repo, [
            {'update', 2, fun(11, Data) ->
                ?assertEqual(<<"new_name">>, maps:get(<<"name">>, Data)),
                ?assert(maps:is_key(updated_at, Data)),
                {ok, 1}
            end},
            {'find_by_id', 2, fun(11, <<"*">>) ->
                #{
                    <<"id">> => 11,
                    <<"creator_uid">> => 1001,
                    <<"name">> => <<"new_name">>
                }
            end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_updated">>, <<>>, null, _Payload, no_save) ->
                {error, notify_failed}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(update, Req, State),
        ?assertMatch({ok_resp, #{<<"name">> := <<"new_name">>}}, Result),
        ?assertEqual(1, meck:num_calls(channel_repo, update, 2)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

delete_prefers_path_channel_id_over_body_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

delete_handler_logic_repo_chain_notify_crash_still_returns_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 3 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {channel_repo, [
            {'delete', 1, fun(11) -> {ok, 1} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, _Payload, save) ->
                erlang:error(mock_notify_crash)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_repo, delete, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

delete_handler_logic_repo_chain_notify_failed_still_returns_success_test_() ->
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> ChannelIdBin end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 3 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {channel_repo, [
            {'delete', 1, fun(11) -> {ok, 1} end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_deleted">>, <<>>, null, _Payload, save) ->
                {error, notify_failed}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_repo, delete, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

search_with_empty_keyword_returns_empty_list_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'parse_qs', 1, fun(_Req) -> [] end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        Result = channel_handler:handle_action(search, Req, #{}),
        ?assertEqual({ok_resp, #{list => []}}, Result)
    end).

search_propagates_logic_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        Result = channel_handler:handle_action(search, Req, #{}),
        ?assertEqual({error_resp, <<"timeout">>}, Result)
    end).

discover_passes_limit_to_logic_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        Result = channel_handler:handle_action(discover, Req, #{}),
        ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"ch_discover_1">>}]}}, Result)
    end).

discover_propagates_logic_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        Result = channel_handler:handle_action(discover, Req, #{}),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

record_view_uses_path_params_test_() ->
    ?WITH_MECKS([
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
            {'record_message_view', 3, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(record_view, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

add_reaction_uses_path_params_test_() ->
    ?WITH_MECKS([
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
            {'add_reaction', 4, fun(1001, <<"ch_hash_path">>, <<"msg_hash_path">>, <<"like">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(add_reaction, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

pin_message_uses_path_message_id_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(pin_message, Req, State),
        ?assertMatch({ok_resp, #{<<"pinned">> := true}}, Result)
    end).

delete_message_uses_path_message_id_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(message_id, _Req) -> <<"msg_hash_path">> end}
        ]},
        {channel_logic, [
            {'delete_message', 2, fun(1001, <<"msg_hash_path">>) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete_message, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

delete_message_returns_error_when_message_id_missing_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete_message, Req, State),
        ?assertEqual({error_resp, <<"消息ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, delete_message, 2))
    end).

delete_message_propagates_logic_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete_message, Req, State),
        ?assertEqual({error_resp, <<"无权限删除此消息"/utf8>>}, Result)
    end).

delete_message_handler_logic_repo_chain_notify_failed_still_returns_success_test_() ->
    MessageIdBin = elib_hashids:encode(99),
    ChannelIdBin = elib_hashids:encode(11),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(message_id, _Req) -> MessageIdBin end}
        ]},
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
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete_message, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

delete_message_handler_logic_repo_chain_notify_crash_still_returns_success_test_() ->
    MessageIdBin = elib_hashids:encode(100),
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(message_id, _Req) -> MessageIdBin end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(100) ->
                #{
                    <<"id">> => 100,
                    <<"channel_id">> => 11,
                    <<"author_id">> => 1001
                }
            end},
            {'delete', 1, fun(100) -> {ok, 1} end}
        ]},
        {channel_logic_common, [
            {'get_user_role', 2, fun(11, 1001) -> 0 end}
        ]},
        {channel_ds, [
            {'subscriber_uids', 1, fun(11) -> [1001, 2002] end}
        ]},
        {msg_s2c_ds, [
            {'send', 7, fun(0, [1001, 2002], <<"channel_message_deleted">>, <<>>, null, _Payload, save) ->
                erlang:error(mock_notify_crash)
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end},
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(delete_message, Req, State),
        ?assertEqual({ok_resp, #{}}, Result),
        ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
        ?assertEqual(1, meck:num_calls(msg_s2c_ds, send, 7)),
        ?assertEqual(0, meck:num_calls(elib_response, error, 2))
    end).

get_order_uses_path_order_no_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({ok_resp, #{<<"order_no">> => <<"ORD001">>}}, Result)
    end).

get_order_returns_error_when_order_no_missing_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, get_order, 2))
    end).

get_order_returns_error_when_order_no_blank_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({error_resp, <<"订单号不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, get_order, 2))
    end).

get_order_normalizes_integer_order_no_before_logic_call_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({ok_resp, #{<<"order_no">> => <<"12345">>}}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
    end).

get_order_normalizes_list_order_no_before_logic_call_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({ok_resp, #{<<"order_no">> => <<"ORD-LIST-GET">>}}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
    end).

get_order_propagates_logic_error_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(get_order, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, get_order, 2))
    end).

admins_uses_path_channel_id_test_() ->
    ?WITH_MECKS([
        {cowboy_req, [
            {'binding', 2, fun(channel_id, _Req) -> <<"ch_hash_path">> end}
        ]},
        {channel_logic, [
            {'get_admins', 1, fun(<<"ch_hash_path">>) ->
                {ok, [#{<<"user_id">> => <<"uid_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        Result = channel_handler:handle_action(admins, Req, #{}),
        ?assertEqual({ok_resp, #{list => [#{<<"user_id">> => <<"uid_1">>}]}}, Result)
    end).

accept_invitation_decodes_invitation_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'accept_invitation', 2, fun(1001, 501) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

accept_invitation_returns_error_when_invitation_id_missing_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<>>) -> 0 end}
        ]},
        {channel_logic, [
            {'accept_invitation', 2, fun(_, _) -> erlang:error(should_not_call_accept_invitation) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, accept_invitation, 2))
    end).

accept_invitation_returns_error_when_invitation_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> invalid_invitation_id end}
        ]},
        {channel_logic, [
            {'accept_invitation', 2, fun(_, _) -> erlang:error(should_not_call_accept_invitation_when_id_invalid) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, accept_invitation, 2))
    end).

accept_invitation_returns_logic_error_message_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'accept_invitation', 2, fun(1001, 501) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请不存在或已过期"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, accept_invitation, 2))
    end).

accept_invitation_retry_is_idempotent_at_handler_boundary_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'accept_invitation', 2, fun(1001, 501) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        First = channel_handler:handle_action(accept_invitation, Req, State),
        Second = channel_handler:handle_action(accept_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, First),
        ?assertEqual({ok_resp, #{}}, Second),
        ?assertEqual(2, meck:num_calls(channel_logic, accept_invitation, 2))
    end).

reject_invitation_decodes_invitation_id_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'reject_invitation', 2, fun(1001, 501) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(reject_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, Result)
    end).

reject_invitation_returns_error_when_invitation_id_missing_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<>>) -> 0 end}
        ]},
        {channel_logic, [
            {'reject_invitation', 2, fun(_, _) -> erlang:error(should_not_call_reject_invitation) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(reject_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, reject_invitation, 2))
    end).

reject_invitation_returns_error_when_invitation_id_decode_unexpected_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> invalid_invitation_id end}
        ]},
        {channel_logic, [
            {'reject_invitation', 2, fun(_, _) -> erlang:error(should_not_call_reject_invitation_when_id_invalid) end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(reject_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请ID不能为空"/utf8>>}, Result),
        ?assertEqual(0, meck:num_calls(channel_logic, reject_invitation, 2))
    end).

reject_invitation_returns_logic_error_message_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'reject_invitation', 2, fun(1001, 501) -> {error, <<"邀请不存在或已过期"/utf8>>} end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(reject_invitation, Req, State),
        ?assertEqual({error_resp, <<"邀请不存在或已过期"/utf8>>}, Result),
        ?assertEqual(1, meck:num_calls(channel_logic, reject_invitation, 2))
    end).

reject_invitation_retry_is_idempotent_at_handler_boundary_test_() ->
    ?WITH_MECKS([
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"invitation_id">> => <<"inv_hash_1">>} end}
        ]},
        {elib_hashids, [
            {'decode', 1, fun(<<"inv_hash_1">>) -> 501 end}
        ]},
        {channel_logic, [
            {'reject_invitation', 2, fun(1001, 501) -> ok end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        First = channel_handler:handle_action(reject_invitation, Req, State),
        Second = channel_handler:handle_action(reject_invitation, Req, State),
        ?assertEqual({ok_resp, #{}}, First),
        ?assertEqual({ok_resp, #{}}, Second),
        ?assertEqual(2, meck:num_calls(channel_logic, reject_invitation, 2))
    end).

my_invitations_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_my_invitations', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"inv_1">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(my_invitations, Req, State),
        ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"inv_1">>}]}}, Result)
    end).

my_invitations_propagates_logic_error_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_my_invitations', 1, fun(1001) ->
                {error, db_down}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(my_invitations, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

sent_invitations_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_sent_invitations', 1, fun(1001) ->
                {ok, [#{<<"id">> => <<"inv_2">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(_Req, Payload) -> {ok_resp, Payload} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(sent_invitations, Req, State),
        ?assertEqual({ok_resp, #{list => [#{<<"id">> => <<"inv_2">>}]}}, Result)
    end).

sent_invitations_propagates_logic_error_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_sent_invitations', 1, fun(1001) ->
                {error, <<"db_down">>}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(sent_invitations, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

unread_summary_returns_payload_test_() ->
    ?WITH_MECKS([
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
    ], fun() ->
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
    end).

unread_summary_normalizes_non_binary_error_test_() ->
    ?WITH_MECKS([
        {channel_logic, [
            {'get_unread_summary', 1, fun(1001) ->
                {error, db_down}
            end}
        ]},
        {elib_response, [
            {'error', 2, fun(_Req, Msg) -> {error_resp, Msg} end}
        ]}
    ], fun() ->
        Req = req_mock(),
        State = #{current_uid => 1001},
        Result = channel_handler:handle_action(unread_summary, Req, State),
        ?assertEqual({error_resp, <<"db_down">>}, Result)
    end).

handle_action_false_returns_original_req_test() ->
    Req = req_mock(),
    ?assertEqual(Req, channel_handler:handle_action(false, Req, #{})).

req_mock() ->
    #{mock_req => true}.
