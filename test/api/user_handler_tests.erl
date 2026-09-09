-module(user_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% user_handler 当前接口回归测试
%%%
%%% 这些用例贴合当前 handler/logic 签名，验证：
%%% - search 走 elib_type 与统一 payload
%%% - 密码、状态、设置等写操作走当前 response 形态
%%% - credential / qrcode 走当前 domain facade
%%%===================================================================

search_by_email_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'page', 1, fun(_Req) -> {1, 20} end}
            ]},
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"keyword">>, <<"test@example.com">>}]
                end}
            ]},
            {elib_type, [
                {'is_email', 1, fun(<<"test@example.com">>) -> true end},
                {'is_mobile', 1, fun(_Any) -> false end}
            ]},
            {user_repo, [
                {'find_by_email', 2, fun(_Email, _Columns) ->
                    #{
                        <<"id">> => 12345,
                        <<"nickname">> => <<"Test User">>,
                        <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                        <<"gender">> => 1,
                        <<"sign">> => <<"Hello World">>,
                        <<"region">> => <<"Beijing">>
                    }
                end}
            ]},
            {fts_user_repo, [
                {'allow_search', 1, fun(12345) -> true end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(67890, 12345, <<"remark">>) ->
                    {false, <<>>}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{action => search, current_uid => 67890}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertMatch(#{total := 1, page := 1, size := 20, list := [_]}, Body)
        end
    ).

change_password_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {user_logic, [
                {'change_password', 2, fun(12345, _Req) ->
                    {ok, <<"success">>}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => #{status => success}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{
                action => change_password, current_uid => 12345
            }),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode)
        end
    ).

set_password_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {user_logic, [
                {'set_password', 2, fun(12345, _Req) ->
                    {ok, <<"success">>}
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(_Req) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => #{status => success}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{
                action => set_password, current_uid => 12345
            }),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode)
        end
    ).

credential_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {user_ds, [
                {'webrtc_credential', 1, fun(12345) ->
                    #{<<"username">> => <<"user123">>, <<"credential">> => <<"base64_credential">>}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{
                action => credential, current_uid => 12345
            }),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(<<"user123">>, maps:get(<<"username">>, Body))
        end
    ).

qrcode_user_exists_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) ->
                    [{<<"id">>, <<"12345">>}]
                end}
            ]},
            {user_logic, [
                {'find_by_id', 2, fun(12345, _Columns) ->
                    #{
                        <<"id">> => 12345,
                        <<"nickname">> => <<"Test User">>,
                        <<"avatar">> => <<"https://example.com/avatar.jpg">>,
                        <<"gender">> => 1,
                        <<"sign">> => <<"Hello World">>,
                        <<"region">> => <<"Beijing">>,
                        <<"status">> => 1
                    }
                end}
            ]},
            {friend_ds, [
                {'is_friend', 3, fun(67890, 12345, <<"remark">>) ->
                    {true, <<"My Friend">>}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{action => qrcode, current_uid => 67890}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            ?assertEqual(12345, maps:get(<<"id">>, Body)),
            ?assertEqual(true, maps:get(<<"isfriend">>, Body))
        end
    ).

change_state_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"state">> => <<"online">>}
                end}
            ]},
            {user_setting_ds, [
                {'save', 3, fun(12345, <<"chat_state">>, <<"online">>) ->
                    ok
                end}
            ]},
            {user_server, [
                {'cast_notice_friend', 2, fun(12345, <<"online">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Payload, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => #{status => success}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{
                action => change_state, current_uid => 12345
            }),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode)
        end
    ).

setting_test_() ->
    ?WITH_MECKS(
        [
            {auth_ds, [
                {'current_uid', 1, fun(_State) -> 12345 end}
            ]},
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{
                        <<"setting">> => [
                            [{<<"theme">>, <<"dark">>}],
                            [{<<"language">>, <<"zh-CN">>}]
                        ]
                    }
                end}
            ]},
            {user_setting_ds, [
                {'save', 3, fun
                    (12345, <<"theme">>, <<"dark">>) -> ok;
                    (12345, <<"language">>, <<"zh-CN">>) -> ok
                end}
            ]},
            {elib_response, [
                {'success', 3, fun(_Req, _Payload, _Msg) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => #{status => success}
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{
                action => setting, current_uid => 12345
            }),
            {StatusCode, _, _Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode)
        end
    ).

%% ===================================================================
%% LT02-SEC-01（AI-ID=B）：user/show 只做数据投影、不构成授权源
%% ===================================================================
%% show 端点在 imboy_router:open/0 免鉴权白名单，keep_agent_account_type/1
%% 仅对 AI 助手（account_type=1）回吐徽章字段、真人一律剥除、未知 id 回空。
%% 本组锁定该 fail-closed 形态：客户端明文授权只能来自本地用户确认记录
%% （AiPlaintextGate 四元组），show 响应中的 account_type 仅可用于 UI 徽章；
%% 伪造/污染的 show 数据不能在服务端或客户端单独触发明文豁免。
show_publishes_agent_badge_for_ai_account_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"id">>, <<"456">>}] end}
            ]},
            {imboy_error, [
                {'validate_id', 2, fun(_Req, <<"456">>) -> {ok, 456} end}
            ]},
            {user_logic, [
                {'find_by_id', 2, fun(456, _Columns) ->
                    #{
                        <<"id">> => 456,
                        <<"nickname">> => <<"AI Assistant">>,
                        <<"avatar">> => <<"">>,
                        <<"sign">> => <<"">>,
                        <<"account_type">> => 1
                    }
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{action => show}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            %% AI 助手徽章按产品设计公开（广场即公开展示），数据投影保留
            ?assertEqual(1, maps:get(<<"account_type">>, Body)),
            %% TSID 必须转 binary 字符串
            ?assertEqual(<<"456">>, maps:get(<<"id">>, Body))
        end
    ).

show_strips_account_type_for_human_account_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"id">>, <<"456">>}] end}
            ]},
            {imboy_error, [
                {'validate_id', 2, fun(_Req, <<"456">>) -> {ok, 456} end}
            ]},
            {user_logic, [
                {'find_by_id', 2, fun(456, _Columns) ->
                    #{
                        <<"id">> => 456,
                        <<"nickname">> => <<"Human">>,
                        <<"avatar">> => <<"">>,
                        <<"sign">> => <<"">>,
                        %% 即使服务端数据异常携带非 1 值也必须剥除
                        <<"account_type">> => 0
                    }
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{action => show}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            %% 真人账号类型不属公开资料：字段必须被剥除（show 不作授权源）
            ?assertNot(maps:is_key(<<"account_type">>, Body))
        end
    ).

show_unknown_id_returns_empty_object_fail_closed_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'parse_qs', 1, fun(_Req) -> [{<<"id">>, <<"999">>}] end}
            ]},
            {imboy_error, [
                {'validate_id', 2, fun(_Req, <<"999">>) -> {ok, 999} end}
            ]},
            {user_logic, [
                {'find_by_id', 2, fun(999, _Columns) -> undefined end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Payload) ->
                    cowboy_req_h:new(#{
                        response_status => 200,
                        response_body => Payload
                    })
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            {ok, Req, _State} = user_handler:init(MockReq, #{action => show}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?assertEqual(200, StatusCode),
            %% 未知 id 免鉴权端点 fail-closed：回空对象，不打崩也不泄露
            ?assertEqual(#{}, Body)
        end
    ).
