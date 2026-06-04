-module(e2ee_transfer_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc e2ee_transfer_handler 基础行为测试
%%%===================================================================

module_loaded_test_() ->
    ?TEST_SIMPLE(fun() ->
        code:ensure_loaded(e2ee_transfer_handler),
        ?assertMatch({file, _}, code:is_loaded(e2ee_transfer_handler))
    end).

handle_action_false_returns_original_req_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{uri => <<"/v1/e2ee/transfer/unknown">>}),
        Result = e2ee_transfer_handler:handle_action(false, Req0, #{}),
        ?assertEqual(Req0, Result)
    end).

init_with_false_action_removes_action_from_state_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req0 = cowboy_req_h:new(#{}),
        {ok, Req1, State1} = e2ee_transfer_handler:init(Req0, #{action => false, keep => 2}),
        ?assertEqual(Req0, Req1),
        ?assertEqual(#{keep => 2}, State1)
    end).

create_transfer_accepts_legacy_to_uid_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"to_uid\":\"12345\"}">>, req_after_body}
                end}
            ]},
            {user_ds, [
                {'may_exist', 1, fun(12345) -> true end}
            ]},
            {user_device_ds, [
                {'get_public_by_uid', 1, fun(Uid) ->
                    case Uid of
                        100 ->
                            {ok, [
                                #{
                                    <<"device_id">> => <<"sender_device_1">>,
                                    <<"public_key">> => <<"sender_public">>
                                }
                            ]};
                        12345 ->
                            {ok, [
                                #{
                                    <<"device_id">> => <<"receiver_device_1">>,
                                    <<"public_key">> => <<"receiver_public">>
                                }
                            ]}
                    end
                end},
                {'get_private_key', 2, fun(100, <<"sender_device_1">>) ->
                    {ok, <<"sender_private">>}
                end}
            ]},
            {e2ee_transfer_logic, [
                {'create_transfer', 5, fun(
                    100, <<"sender_device_1">>, 12345, <<"sender_private">>, <<"receiver_public">>
                ) ->
                    {ok, #{
                        <<"session_id">> => <<"session-1">>,
                        <<"expires_at">> => <<"2026-02-25T00:00:00Z">>
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    self() ! {resp_data, Data},
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_transfer_handler:create_transfer(Req0, #{current_uid => 100}),
            ?assertEqual(req_ok, Result),
            ?assertEqual(
                #{
                    <<"session_id">> => <<"session-1">>,
                    <<"expires_at">> => <<"2026-02-25T00:00:00Z">>
                },
                receive_resp_data()
            )
        end
    ).

%%%===================================================================
%%% TDD 修复验证测试 — C2: 死值赋值修复（session_id 缺失不再崩溃）
%%%===================================================================

%% 修复前：缺少 session_id 时 SessionId = elib_response:error(...)，
%%         随即传入 logic 层导致 badarg 崩溃。
%% 修复后：返回 400 Bad Request，不触发 logic 层。
accept_transfer_missing_session_id_returns_400_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{\"device_id\":\"dev-1\"}">>, req_after_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {error_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_transfer_handler:accept_transfer(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(400, receive_msg(error_code))
        end
    ).

confirm_transfer_missing_session_id_returns_400_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{}">>, req_after_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {error_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_transfer_handler:confirm_transfer(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(400, receive_msg(error_code))
        end
    ).

cancel_transfer_missing_session_id_returns_400_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"{}">>, req_after_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {error_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            Result = e2ee_transfer_handler:cancel_transfer(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(400, receive_msg(error_code))
        end
    ).

%%%===================================================================
%%% TDD 修复验证测试 — H2: 畸形 JSON 不崩溃进程
%%%===================================================================

%% 修复前：jsx:decode(<<"not json">>) 直接抛异常使 handler 进程崩溃。
%% 修复后：decode_json_body 捕获异常，返回 400。
accept_transfer_invalid_json_body_returns_400_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'read_body', 1, fun(_Req) ->
                    {ok, <<"not-valid-json!!!">>, req_after_body}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {error_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            %% 不应该抛出异常
            Result = e2ee_transfer_handler:accept_transfer(Req0, #{current_uid => 100}),
            ?assertEqual(req_error, Result),
            ?assertEqual(400, receive_msg(error_code))
        end
    ).

%%%===================================================================
%%% TDD 修复验证测试 — C3: get_transfer_info 授权校验
%%%===================================================================

%% 修复前：_State 忽略认证，任意用户可查任意会话。
%% 修复后：CurrentUid 不是 from_uid 或 to_uid 时返回 403。
get_transfer_info_rejects_unauthorized_uid_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"session_id">>, <<"session-abc">>}]
                end}
            ]},
            {e2ee_transfer_logic, [
                {'get_transfer_info', 1, fun(<<"session-abc">>) ->
                    {ok, #{
                        <<"session_id">> => <<"session-abc">>,
                        <<"from_uid">> => 111,
                        <<"to_uid">> => 222,
                        <<"from_device_id">> => <<"dev-1">>,
                        <<"status">> => <<"pending">>,
                        <<"expires_at">> => <<"2026-12-31">>
                    }}
                end}
            ]},
            {elib_response, [
                {'error', 3, fun(_Req, _Msg, Code) ->
                    self() ! {error_code, Code},
                    req_error
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            %% uid=999 不是 from_uid(111) 也不是 to_uid(222)
            Result = e2ee_transfer_handler:get_transfer_info(Req0, #{current_uid => 999}),
            ?assertEqual(req_error, Result),
            ?assertEqual(403, receive_msg(error_code))
        end
    ).

get_transfer_info_allows_from_uid_test_() ->
    ?WITH_MECKS(
        [
            {imboy_policy, [
                {'e2ee_enabled', 0, fun() -> true end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"session_id">>, <<"session-abc">>}]
                end}
            ]},
            {e2ee_transfer_logic, [
                {'get_transfer_info', 1, fun(<<"session-abc">>) ->
                    {ok, #{
                        <<"session_id">> => <<"session-abc">>,
                        <<"from_uid">> => 111,
                        <<"to_uid">> => 222,
                        <<"from_device_id">> => <<"dev-1">>,
                        <<"status">> => <<"pending">>,
                        <<"expires_at">> => <<"2026-12-31">>
                    }}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, _Data) ->
                    self() ! authorized_ok,
                    req_ok
                end}
            ]}
        ],
        fun() ->
            Req0 = cowboy_req_h:new(#{}),
            %% uid=111 是 from_uid，应该允许访问
            Result = e2ee_transfer_handler:get_transfer_info(Req0, #{current_uid => 111}),
            ?assertEqual(req_ok, Result),
            ?assertEqual(authorized_ok, receive_msg(authorized_ok))
        end
    ).

%%%===================================================================
%%% TDD 修复验证测试 — metrics_handler: 内网 IP 限制
%%%===================================================================

%% 修复前：/metrics 在 open() 白名单，外网可访问。
%% 修复后：is_internal_ip/1 函数正确识别内外网 IP。
metrics_internal_ip_loopback_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(metrics_handler:is_internal_ip({127, 0, 0, 1})),
        ?assert(metrics_handler:is_internal_ip({127, 255, 255, 255}))
    end).

metrics_internal_ip_rfc1918_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(metrics_handler:is_internal_ip({10, 0, 0, 1})),
        ?assert(metrics_handler:is_internal_ip({10, 255, 255, 255})),
        ?assert(metrics_handler:is_internal_ip({172, 16, 0, 1})),
        ?assert(metrics_handler:is_internal_ip({172, 31, 255, 255})),
        ?assert(metrics_handler:is_internal_ip({192, 168, 1, 100}))
    end).

metrics_external_ip_blocked_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assertNot(metrics_handler:is_internal_ip({8, 8, 8, 8})),
        ?assertNot(metrics_handler:is_internal_ip({1, 1, 1, 1})),
        ?assertNot(metrics_handler:is_internal_ip({203, 0, 113, 1})),
        %% 172.15.x.x 不在 RFC-1918 范围
        ?assertNot(metrics_handler:is_internal_ip({172, 15, 255, 255})),
        %% 172.32.x.x 不在 RFC-1918 范围
        ?assertNot(metrics_handler:is_internal_ip({172, 32, 0, 0}))
    end).

metrics_ipv6_loopback_allowed_test_() ->
    ?TEST_SIMPLE(fun() ->
        ?assert(metrics_handler:is_internal_ip({0, 0, 0, 0, 0, 0, 0, 1}))
    end).

%%%===================================================================
%%% 内部辅助函数
%%%===================================================================

receive_resp_data() ->
    receive_msg(resp_data).

receive_msg(Tag) ->
    receive
        {Tag, Value} -> Value;
        Tag -> Tag
    after 1000 ->
        timeout
    end.
