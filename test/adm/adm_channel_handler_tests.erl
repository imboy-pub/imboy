-module(adm_channel_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_channel_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖频道消息治理核心路径（列表、置顶、删除校验）
%%%===================================================================

channel_admin_feature_enabled_mocks() ->
    [
        {imboy_feature, [
            {'ensure_enabled', 2, fun(_Req, channel) -> ok;
                                     (_Req, channel_invitation) -> ok;
                                     (_Req, channel_order) -> ok
                                  end}
        ]}
    ].

init_messages_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {channel_message_repo, [
            {'tablename', 0, fun() -> <<"public.channel_message">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, _Col, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"public.channel_message">>, Tb),
                ?assertEqual(#{channel_id => 11}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(10, Size),
                {ok, #{
                    list => [
                        #{
                            <<"id">> => 101,
                            <<"channel_id">> => 11,
                            <<"content">> => <<"hello">>
                        }
                    ],
                    page => 1,
                    size => 10,
                    total => 1,
                    total_pages => 1
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => messages}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        [First | _] = maps:get(list, Payload),
        ?assertEqual(<<"hello">>, maps:get(<<"content">>, First)),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_messages_accepts_legacy_channel_id_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"6q58gm">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {channel_message_repo, [
            {'tablename', 0, fun() -> <<"public.channel_message">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, _Col, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"public.channel_message">>, Tb),
                ?assertEqual(#{channel_id => 11}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(10, Size),
                {ok, #{list => [], page => 1, size => 10, total => 0, total_pages => 0}}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => messages}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(0, maps:get(total, Payload))
    end).

init_pin_message_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    message_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"pinned">> => <<"false">>} end}
        ]},
        {elib_dt, [
            {'now', 0, fun() -> <<"2026-02-21T10:00:00Z">> end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(22) ->
                #{<<"id">> => 22, <<"channel_id">> => 11}
            end},
            {'update', 2, fun(22, Data) ->
                ?assertEqual(false, maps:get(is_pinned, Data)),
                ?assertEqual(<<"2026-02-21T10:00:00Z">>, maps:get(updated_at, Data)),
                {ok, 1}
            end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                assert_audit_governance(
                    Data,
                    0,
                    11,
                    <<"pin_message">>,
                    22,
                    #{<<"pinned">> => false}
                ),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => pin_message}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"消息状态已更新"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(user_log_repo, add, 1))
    end).

init_delete_message_rejects_foreign_message_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    message_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(22) ->
                #{<<"id">> => 22, <<"channel_id">> => 99}
            end},
            {'delete', 1, fun(_) -> erlang:error(should_not_delete) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 400, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => delete_message}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual(<<"消息不属于当前频道"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(channel_message_repo, delete, 1))
    end).

init_subscribers_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 10} end}
        ]},
        {channel_subscription_repo, [
            {'tablename', 0, fun() -> <<"public.channel_subscription">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, _Col, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"public.channel_subscription">>, Tb),
                ?assertEqual(#{channel_id => 11, status => 1}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(10, Size),
                {ok, #{
                    list => [
                        #{
                            <<"id">> => 1,
                            <<"channel_id">> => 11,
                            <<"user_id">> => 2001
                        }
                    ],
                    page => 1,
                    size => 10,
                    total => 1,
                    total_pages => 1
                }}
            end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun([2001], _Column) ->
                {ok, [
                    #{
                        <<"id">> => 2001,
                        <<"account">> => <<"u2001">>,
                        <<"nickname">> => <<"user-2001">>
                    }
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => subscribers}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        [First | _] = maps:get(list, Payload),
        User = maps:get(<<"user">>, First),
        ?assertEqual(2001, maps:get(<<"id">>, User)),
        ?assertEqual(false, maps:is_key(items, Payload))
    end).

init_remove_subscriber_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 22) -> {ok, 1} end}
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
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                assert_audit_governance(
                    Data,
                    0,
                    11,
                    <<"remove_subscriber">>,
                    22,
                    #{<<"scope">> => <<"subscription">>}
                ),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => remove_subscriber}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"订阅者已移除"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3)),
        ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1)),
        ?assertEqual(1, meck:num_calls(user_log_repo, add, 1))
    end).

init_remove_subscriber_accepts_legacy_path_params_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"ch_hash_11">>;
                    user_id -> <<"uid_hash_22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 22) -> {ok, 1} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(fake_conn, 11, -1) -> {ok, 1} end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(_Data) -> {ok, 1} end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => remove_subscriber}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_subscription_repo, delete, 3)),
        ?assertEqual(1, meck:num_calls(channel_repo, increment_subscribers, 3))
    end).

init_remove_subscriber_noop_still_flushes_cache_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_pg, [
            {'with_tx', 1, fun(Fun) -> Fun(fake_conn) end}
        ]},
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 22) -> {ok, 0} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
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
        {user_log_repo, [
            {'add', 1, fun(_) -> erlang:error(should_not_audit) end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => remove_subscriber}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"订阅者已移除"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
        ?assertEqual(2, meck:num_calls(imboy_cache, flush, 1)),
        ?assertEqual(0, meck:num_calls(user_log_repo, add, 1))
    end).

init_remove_subscriber_tx_error_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
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
        {channel_subscription_repo, [
            {'delete', 3, fun(fake_conn, 11, 22) -> {error, db_error} end}
        ]},
        {channel_repo, [
            {'increment_subscribers', 3, fun(_, _, _) -> erlang:error(should_not_increment) end}
        ]},
        {imboy_cache, [
            {'flush', 1, fun(_Key) -> ok end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(_) -> erlang:error(should_not_audit) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 500, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => remove_subscriber}),
        ?assertEqual(500, maps:get(response_status, RespReq)),
        ?assertEqual(<<"移除失败"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(channel_repo, increment_subscribers, 3)),
        ?assertEqual(0, meck:num_calls(imboy_cache, flush, 1)),
        ?assertEqual(0, meck:num_calls(user_log_repo, add, 1))
    end).

init_update_admin_role_rejects_creator_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"role">> => 2} end}
        ]},
        {channel_admin_repo, [
            {'find', 2, fun(11, 22) ->
                #{
                    <<"id">> => 9,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 22,
                    <<"role">> => 3
                }
            end},
            {'update_role', 3, fun(_, _, _) -> erlang:error(should_not_update) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 400, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => update_admin_role}),
        ?assertEqual(400, maps:get(response_status, RespReq)),
        ?assertEqual(<<"创建者角色不可修改"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(0, meck:num_calls(channel_admin_repo, update_role, 3))
    end).

init_update_admin_role_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"role">> => 2} end}
        ]},
        {channel_admin_repo, [
            {'find', 2, fun(11, 22) ->
                #{
                    <<"id">> => 9,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 22,
                    <<"role">> => 1
                }
            end},
            {'update_role', 3, fun(11, 22, 2) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                assert_audit_governance(
                    Data,
                    9001,
                    11,
                    <<"update_admin_role">>,
                    22,
                    #{<<"role">> => 2}
                ),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(
            Req,
            #{action => update_admin_role, adm_user_id => 9001}
        ),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"管理员角色已更新"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_admin_repo, update_role, 3)),
        ?assertEqual(1, meck:num_calls(user_log_repo, add, 1))
    end).

init_update_admin_role_repo_failure_no_audit_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"PUT">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {elib_param, [
            {'post', 1, fun(_Req) -> #{<<"role">> => 2} end}
        ]},
        {channel_admin_repo, [
            {'find', 2, fun(11, 22) ->
                #{
                    <<"id">> => 9,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 22,
                    <<"role">> => 1
                }
            end},
            {'update_role', 3, fun(11, 22, 2) -> {error, <<"db_down">>} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(_) -> erlang:error(should_not_audit) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 500, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(
            Req,
            #{action => update_admin_role, adm_user_id => 9004}
        ),
        ?assertEqual(500, maps:get(response_status, RespReq)),
        ?assertEqual(<<"更新失败"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_admin_repo, update_role, 3)),
        ?assertEqual(0, meck:num_calls(user_log_repo, add, 1))
    end).

init_remove_admin_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    user_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {channel_admin_repo, [
            {'find', 2, fun(11, 22) ->
                #{
                    <<"id">> => 12,
                    <<"channel_id">> => 11,
                    <<"user_id">> => 22,
                    <<"role">> => 2
                }
            end},
            {'delete', 2, fun(11, 22) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                assert_audit_governance(
                    Data,
                    9002,
                    11,
                    <<"remove_admin">>,
                    22,
                    #{<<"scope">> => <<"admin">>}
                ),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(
            Req,
            #{action => remove_admin, adm_user_id => 9002}
        ),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"管理员已移除"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_admin_repo, delete, 2)),
        ?assertEqual(1, meck:num_calls(user_log_repo, add, 1))
    end).

init_invitations_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {channel_invitation_repo, [
            {'tablename', 0, fun() -> <<"channel_invitation">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, _Col, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"channel_invitation">>, Tb),
                ?assertEqual(#{channel_id => 11}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                {ok, #{
                    list => [
                        #{
                            <<"id">> => 301,
                            <<"channel_id">> => 11,
                            <<"inviter_uid">> => 4001,
                            <<"invitee_uid">> => 4002
                        }
                    ],
                    page => 1,
                    size => 20,
                    total => 1,
                    total_pages => 1
                }}
            end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun(UserIds, _Column) ->
                case lists:sort(UserIds) of
                    [4001] ->
                        {ok, [#{<<"id">> => 4001, <<"nickname">> => <<"inviter">>}]};
                    [4002] ->
                        {ok, [#{<<"id">> => 4002, <<"nickname">> => <<"invitee">>}]};
                    _ ->
                        {ok, []}
                end
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => invitations}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        [First | _] = maps:get(list, Payload),
        Inviter = maps:get(<<"inviter_user">>, First),
        Invitee = maps:get(<<"invitee_user">>, First),
        ?assertEqual(4001, maps:get(<<"id">>, Inviter)),
        ?assertEqual(4002, maps:get(<<"id">>, Invitee))
    end).

init_orders_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {elib_param, [
            {'page', 1, fun(_Req) -> {1, 20} end}
        ]},
        {channel_order_repo, [
            {'tablename', 0, fun() -> <<"channel_order">> end}
        ]},
        {elib_pg, [
            {'page_with_total', 6, fun(Tb, _Col, Where, OrderBy, Page, Size) ->
                ?assertEqual(<<"channel_order">>, Tb),
                ?assertEqual(#{channel_id => 11}, Where),
                ?assertEqual(<<"id desc">>, OrderBy),
                ?assertEqual(1, Page),
                ?assertEqual(20, Size),
                {ok, #{
                    list => [
                        #{
                            <<"id">> => 901,
                            <<"channel_id">> => 11,
                            <<"user_id">> => 5001,
                            <<"order_no">> => <<"CH202602220001">>
                        }
                    ],
                    page => 1,
                    size => 20,
                    total => 1,
                    total_pages => 1
                }}
            end}
        ]},
        {user_repo, [
            {'list_by_ids', 2, fun([5001], _Column) ->
                {ok, [#{<<"id">> => 5001, <<"nickname">> => <<"buyer">>}]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => orders}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        [First | _] = maps:get(list, Payload),
        User = maps:get(<<"user">>, First),
        ?assertEqual(5001, maps:get(<<"id">>, User))
    end).

init_stats_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {channel_logic, [
            {'get_channel_stats', 1, fun(<<"ch_hash_11">>) ->
                {ok, #{
                    <<"channel_id">> => <<"ch_hash_11">>,
                    <<"subscriber_count">> => 37,
                    <<"total_messages">> => 120,
                    <<"total_views">> => 899,
                    <<"total_reactions">> => 266
                }}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => stats}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"channel_id">>, Payload)),
        ?assertEqual(37, maps:get(<<"subscriber_count">>, Payload)),
        ?assertEqual(120, maps:get(<<"total_messages">>, Payload)),
        ?assertEqual(899, maps:get(<<"total_views">>, Payload)),
        ?assertEqual(266, maps:get(<<"total_reactions">>, Payload))
    end).

init_stats_not_found_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'binding', 2, fun(channel_id, _Req) -> <<"11">> end}
        ]},
        {channel_logic, [
            {'get_channel_stats', 1, fun(<<"ch_hash_11">>) ->
                {error, <<"频道不存在"/utf8>>}
            end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 404, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => stats}),
        ?assertEqual(404, maps:get(response_status, RespReq)),
        ?assertEqual(<<"频道不存在"/utf8>>, maps:get(error_msg, RespReq))
    end).

init_delete_message_success_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    message_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(22) ->
                #{<<"id">> => 22, <<"channel_id">> => 11}
            end},
            {'delete', 1, fun(22) -> {ok, 1} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(Data) ->
                assert_audit_governance(
                    Data,
                    9003,
                    11,
                    <<"delete_message">>,
                    22,
                    #{<<"scope">> => <<"message">>}
                ),
                {ok, 1}
            end}
        ]},
        {elib_response, [
            {'success', 3, fun(Req, Payload, Msg) ->
                Req#{response_status => 200, payload => Payload, success_msg => Msg}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(
            Req,
            #{action => delete_message, adm_user_id => 9003}
        ),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        ?assertEqual(<<"消息已删除"/utf8>>, maps:get(success_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
        ?assertEqual(1, meck:num_calls(user_log_repo, add, 1))
    end).

init_delete_message_repo_failure_no_audit_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"DELETE">> end},
            {'binding', 2, fun(Key, _Req) ->
                case Key of
                    channel_id -> <<"11">>;
                    message_id -> <<"22">>;
                    _ -> undefined
                end
            end}
        ]},
        {channel_message_repo, [
            {'find_by_id', 1, fun(22) ->
                #{<<"id">> => 22, <<"channel_id">> => 11}
            end},
            {'delete', 1, fun(22) -> {error, <<"db_down">>} end}
        ]},
        {user_log_repo, [
            {'add', 1, fun(_) -> erlang:error(should_not_audit) end}
        ]},
        {elib_response, [
            {'error', 3, fun(Req, Msg, Code) ->
                Req#{response_status => 500, error_msg => Msg, error_code => Code}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(
            Req,
            #{action => delete_message, adm_user_id => 9005}
        ),
        ?assertEqual(500, maps:get(response_status, RespReq)),
        ?assertEqual(<<"删除失败"/utf8>>, maps:get(error_msg, RespReq)),
        ?assertEqual(1, meck:num_calls(channel_message_repo, delete, 1)),
        ?assertEqual(0, meck:num_calls(user_log_repo, add, 1))
    end).

init_search_returns_list_with_normalized_ids_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"keyword">>, <<"tech">>},
                    {<<"limit">>, <<"2">>}
                ]
            end}
        ]},
        {channel_repo, [
            {'search', 3, fun(<<"tech">>, 2, _Column) ->
                {ok, [
                    #{
                        <<"id">> => 11,
                        <<"owner_id">> => 99,
                        <<"name">> => <<"Tech Daily">>
                    }
                ]}
            end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => search}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual(1, maps:get(total, Payload)),
        [First | _] = maps:get(list, Payload),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(<<"ch_hash_11">>, maps:get(<<"id">>, First)),
        ?assertEqual(<<"uid_hash_99">>, maps:get(<<"owner_id">>, First))
    end).

init_search_empty_keyword_returns_empty_list_test_() ->
    ?WITH_MECKS(channel_admin_feature_enabled_mocks() ++ [
        {cowboy_req, [
            {'method', 1, fun(_Req) -> <<"GET">> end},
            {'parse_qs', 1, fun(_Req) ->
                [
                    {<<"keyword">>, <<>>},
                    {<<"limit">>, <<"3">>}
                ]
            end}
        ]},
        {channel_repo, [
            {'search', 3, fun(_, _, _) -> erlang:error(should_not_search) end}
        ]},
        {elib_response, [
            {'success', 2, fun(Req, Payload) ->
                Req#{response_status => 200, payload => Payload}
            end}
        ]}
    ], fun() ->
        Req = #{},
        {ok, RespReq, _State} = adm_channel_handler:init(Req, #{action => search}),
        ?assertEqual(200, maps:get(response_status, RespReq)),
        Payload = maps:get(payload, RespReq),
        ?assertEqual([], maps:get(list, Payload)),
        ?assertEqual(0, maps:get(total, Payload)),
        ?assertEqual(false, maps:is_key(items, Payload)),
        ?assertEqual(0, meck:num_calls(channel_repo, search, 3))
    end).

assert_audit_governance(Data, AdmUserId, ChannelId, Action, TargetId, ExpectedExtra) ->
    ?assertEqual(901, maps:get(type, Data)),
    ?assertEqual(AdmUserId, maps:get(uid, Data)),
    ?assertEqual(<<"adm_channel_governance">>, maps:get(remark, Data)),
    Body = maps:get(body, Data),
    ?assert(is_binary(Body)),
    AuditBody = jsone:decode(Body),
    ?assertEqual(Action, maps:get(<<"action">>, AuditBody)),
    ?assertEqual(AdmUserId, maps:get(<<"operator_uid">>, AuditBody)),
    ?assertEqual(ChannelId, maps:get(<<"channel_id">>, AuditBody)),
    ?assertEqual(TargetId, maps:get(<<"target_id">>, AuditBody)),
    ?assertEqual(ExpectedExtra, maps:get(<<"extra">>, AuditBody)),
    ?assert(maps:is_key(<<"occurred_at">>, AuditBody)).
