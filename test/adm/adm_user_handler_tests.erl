-module(adm_user_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_user_handler 模块的 EUnit 测试
%%%
%%% 目标：覆盖用户分页、详情与禁用/解禁敏感动作
%%%===================================================================

init_list_with_status_and_keyword_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'page', 1, fun(_Req) -> {3, 25} end},
                {'int', 3, fun(status, _Req, _Default) -> {ok, 1} end},
                {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<"alice">>} end}
            ]},
            {user_repo, [
                {'page', 4, fun(Page, Size, Where, OrderBy) ->
                    ?assertEqual(3, Page),
                    ?assertEqual(25, Size),
                    ?assertEqual(<<"created_at DESC">>, OrderBy),
                    ?assertEqual(1, maps:get(status, Where)),
                    AndWhere = maps:get('and', Where),
                    OrClauses = maps:get('or', AndWhere),
                    ?assertEqual(4, length(OrClauses)),
                    {ok, #{list => [#{<<"id">> => 1001}], total => 1, page => 3, size => 25}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => list}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(1, maps:get(total, Payload)),
            ?assertEqual(1, length(maps:get(list, Payload))),
            ?assertEqual(false, maps:is_key(items, Payload))
        end
    ).

init_detail_success_includes_relation_counters_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(1001, Column) ->
                    ?assertNotEqual(nomatch, binary:match(Column, <<"nickname">>)),
                    #{
                        <<"id">> => 1001,
                        <<"account">> => <<"u1001">>,
                        <<"nickname">> => <<"alice">>
                    }
                end}
            ]},
            {user_device_repo, [
                {'count_by_uid', 1, fun(1001) -> 2 end}
            ]},
            {friend_repo, [
                {'count_by_uid', 1, fun(1001) -> 8 end}
            ]},
            {group_member_repo, [
                {'count_by_uid', 1, fun(1001) -> 3 end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => detail}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(2, maps:get(device_count, Payload)),
            ?assertEqual(8, maps:get(friend_count, Payload)),
            ?assertEqual(3, maps:get(group_count, Payload))
        end
    ).

init_detail_accepts_legacy_uid_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(uid, _Req, _Default) -> {ok, 0} end},
                {'binary', 3, fun(uid, _Req, _Default) -> {ok, <<"1001">>} end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(1001, _Column) ->
                    #{
                        <<"id">> => 1001,
                        <<"account">> => <<"u1001">>,
                        <<"nickname">> => <<"alice">>
                    }
                end}
            ]},
            {user_device_repo, [
                {'count_by_uid', 1, fun(1001) -> 2 end}
            ]},
            {friend_repo, [
                {'count_by_uid', 1, fun(1001) -> 8 end}
            ]},
            {group_member_repo, [
                {'count_by_uid', 1, fun(1001) -> 3 end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end},
                {'error', 2, fun(Req, _Msg) ->
                    Req#{response_status => 400}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => detail}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(2, maps:get(device_count, Payload)),
            ?assert(is_binary(maps:get(<<"id">>, Payload)))
        end
    ).

init_ban_updates_status_to_zero_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end},
                {'binary', 3, fun(reason, _Req, _Default) -> {ok, <<"violation">>} end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(1, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 1, <<"role_id">> => 1}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) -> {<<"super_admin">>, [<<"users:update">>], []} end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(1001, <<"status">>) -> #{<<"status">> => 1} end},
                {'update', 2, fun(Uid, Data) ->
                    ?assertEqual(1001, Uid),
                    ?assertEqual(0, maps:get(status, Data)),
                    {ok, 1}
                end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(AdmUid, Action, TargetId, TargetType, Detail, _Ip) ->
                    ?assertEqual(1, AdmUid),
                    ?assertEqual(<<"ban_user">>, Action),
                    ?assertEqual(1001, TargetId),
                    ?assertEqual(<<"user">>, TargetType),
                    ?assertEqual(#{<<"status">> => 1}, maps:get(<<"before">>, Detail)),
                    ?assertEqual(#{<<"status">> => 0}, maps:get(<<"after">>, Detail)),
                    ?assertEqual(<<"violation">>, maps:get(<<"reason">>, Detail)),
                    ok
                end}
            ]},
            {elib_req, [
                {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, Msg) ->
                    Req#{response_status => 200, payload => Payload, msg => Msg}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"POST">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => ban, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual("操作成功", maps:get(msg, RespReq))
        end
    ).

%% 安全回归：无 users:update 权限的管理员不能封禁用户（曾经零权限校验）
init_ban_permission_denied_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_logic, [
                {'find', 3, fun(9, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 9, <<"role_id">> => 3}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(3) -> {<<"viewer">>, [], []} end}
            ]},
            {elib_response, [
                {'error', 3, fun(Req, _Msg, Code) -> Req#{response_status => Code} end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"POST">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => ban, adm_user_id => 9}),
            ?assertEqual(403, maps:get(response_status, RespReq))
        end
    ).

init_unban_updates_status_to_one_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(uid, _Req, _Default) -> {ok, 1001} end},
                {'binary', 3, fun(reason, _Req, _Default) -> {ok, <<>>} end}
            ]},
            {adm_user_logic, [
                {'find', 3, fun(1, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 1, <<"role_id">> => 1}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) -> {<<"super_admin">>, [<<"users:update">>], []} end}
            ]},
            {user_repo, [
                {'find_by_id', 2, fun(1001, <<"status">>) -> #{<<"status">> => 0} end},
                {'update', 2, fun(Uid, Data) ->
                    ?assertEqual(1001, Uid),
                    ?assertEqual(1, maps:get(status, Data)),
                    {ok, 1}
                end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(_AdmUid, Action, 1001, <<"user">>, Detail, _Ip) ->
                    ?assertEqual(<<"unban_user">>, Action),
                    ?assertEqual(#{<<"status">> => 0}, maps:get(<<"before">>, Detail)),
                    ?assertEqual(#{<<"status">> => 1}, maps:get(<<"after">>, Detail)),
                    ok
                end}
            ]},
            {elib_req, [
                {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, Payload, Msg) ->
                    Req#{response_status => 200, payload => Payload, msg => Msg}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"POST">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => unban, adm_user_id => 1}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual("操作成功", maps:get(msg, RespReq))
        end
    ).

init_search_requires_keyword_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'binary', 3, fun(keyword, _Req, _Default) -> {ok, <<>>} end},
                {'page', 1, fun(_Req) -> {1, 10} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) ->
                    Req#{response_status => 400, error_msg => Msg}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => search}),
            ?assertEqual(400, maps:get(response_status, RespReq)),
            ?assertEqual("请输入搜索关键词", maps:get(error_msg, RespReq))
        end
    ).

%% ===================================================================
%% 用户设备管理（devices / device_kick action）
%% ===================================================================

init_devices_lists_user_devices_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(user_id, _Req, _Default) -> {ok, 1001} end},
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {user_device_logic, [
                {'page', 3, fun(1001, 1, 20) ->
                    #{
                        total => 1,
                        page => 1,
                        size => 20,
                        list => [
                            #{
                                <<"device_id">> => <<"dev-abc">>,
                                <<"device_name">> => <<"iPhone">>,
                                <<"online">> => true
                            }
                        ]
                    }
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => devices}),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(1, maps:get(total, Payload)),
            [Dev] = maps:get(list, Payload),
            ?assertEqual(<<"dev-abc">>, maps:get(<<"device_id">>, Dev))
        end
    ).

init_devices_invalid_uid_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(_Key, _Req, _Default) -> {ok, 0} end},
                {'binary', 3, fun(_Key, _Req, _Default) -> {ok, <<>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) -> Req#{response_status => 400, error_msg => Msg} end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"GET">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{action => devices}),
            ?assertEqual(400, maps:get(response_status, RespReq)),
            ?assertEqual("参数错误", maps:get(error_msg, RespReq))
        end
    ).

init_device_kick_success_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_logic, [
                {'find', 3, fun(1, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 1, <<"role_id">> => 1}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) -> {<<"super_admin">>, [<<"users:update">>], []} end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"user_id">> => <<"1001">>, <<"did">> => <<"dev-abc">>}
                end}
            ]},
            {user_device_logic, [
                {'kick_device', 3, fun(1001, <<>>, <<"dev-abc">>) -> ok end}
            ]},
            {adm_operation_log_ds, [
                {'insert', 6, fun(AdmUid, Action, TargetId, TargetType, Detail, _Ip) ->
                    ?assertEqual(1, AdmUid),
                    ?assertEqual(<<"kick_device">>, Action),
                    ?assertEqual(1001, TargetId),
                    ?assertEqual(<<"user">>, TargetType),
                    ?assertEqual(<<"dev-abc">>, maps:get(<<"did">>, Detail)),
                    ok
                end}
            ]},
            {elib_req, [
                {'peer_ip', 1, fun(_Req) -> <<"127.0.0.1">> end}
            ]},
            {elib_response, [
                {'success', 3, fun(Req, _Payload, Msg) ->
                    Req#{response_status => 200, msg => Msg}
                end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"POST">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{
                action => device_kick, adm_user_id => 1
            }),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual("操作成功", maps:get(msg, RespReq))
        end
    ).

init_device_kick_missing_did_returns_error_test_() ->
    ?WITH_MECKS(
        [
            {adm_user_logic, [
                {'find', 3, fun(1, <<"id,role_id">>, _Key) ->
                    #{<<"id">> => 1, <<"role_id">> => 1}
                end}
            ]},
            {adm_index_handler, [
                {'role_acl', 1, fun(1) -> {<<"super_admin">>, [<<"users:update">>], []} end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) -> #{<<"user_id">> => <<"1001">>, <<"did">> => <<>>} end}
            ]},
            {elib_response, [
                {'error', 2, fun(Req, Msg) -> Req#{response_status => 400, error_msg => Msg} end}
            ]}
        ],
        fun() ->
            Req = #{method => <<"POST">>},
            {ok, RespReq, _State} = adm_user_handler:init(Req, #{
                action => device_kick, adm_user_id => 1
            }),
            ?assertEqual(400, maps:get(response_status, RespReq)),
            ?assertEqual("did不能为空", maps:get(error_msg, RespReq))
        end
    ).
