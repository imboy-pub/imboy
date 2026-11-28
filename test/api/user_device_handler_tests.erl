-module(user_device_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

mock_request() ->
    #{method => <<"POST">>, path => <<"/api/v1/user/device">>}.

init_false_action_passthrough_test_() ->
    ?TEST_SIMPLE(fun() ->
        Req = mock_request(),
        {ok, RespReq, State} = user_device_handler:init(Req, #{
            action => false, current_uid => 12345
        }),
        ?assertEqual(Req, RespReq),
        ?assertEqual(#{current_uid => 12345}, State)
    end).

init_page_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'page', 1, fun(_Req) ->
                    {1, 20}
                end}
            ]},
            {user_device_logic, [
                {'page', 3, fun(12345, 1, 20) ->
                    #{total => 1, page => 1, size => 20, list => [#{<<"did">> => <<"device-1">>}]}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => page, current_uid => 12345
            }),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(1, maps:get(total, Payload))
        end
    ).

init_change_name_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"did">> => <<"device-1">>, <<"name">> => <<"iPhone 15">>}
                end}
            ]},
            {user_device_logic, [
                {'change_name', 3, fun(12345, <<"device-1">>, <<"iPhone 15">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) ->
                    Req#{response_status => 200}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => change_name, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

init_delete_success_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"did">> => <<"device-1">>}
                end}
            ]},
            {user_device_logic, [
                {'delete', 2, fun(12345, <<"device-1">>) ->
                    ok
                end}
            ]},
            {elib_response, [
                {'success', 1, fun(Req) ->
                    Req#{response_status => 200}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => delete, current_uid => 12345
            }),
            ?assertEqual(200, maps:get(response_status, RespReq))
        end
    ).

init_check_login_no_conflict_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'post', 1, fun(_Req) ->
                    #{<<"device_type">> => <<"ios">>}
                end}
            ]},
            {user_device_logic, [
                {'validate_device_type', 1, fun(<<"ios">>) ->
                    true
                end},
                {'check_login_conflict', 2, fun(12345, <<"ios">>) ->
                    {ok, no_conflict}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(Req, Payload) ->
                    Req#{response_status => 200, payload => Payload}
                end}
            ]}
        ],
        fun() ->
            Req = mock_request(),
            {ok, RespReq, _State} = user_device_handler:init(Req, #{
                action => check_login, current_uid => 12345
            }),
            Payload = maps:get(payload, RespReq),
            ?assertEqual(200, maps:get(response_status, RespReq)),
            ?assertEqual(false, maps:get(<<"conflict">>, Payload))
        end
    ).

%% G-01 合同测试：三个原裸解码 action（check_login/kick/kick_others）在请求体
%% 解析失败（elib_param:post 对坏 JSON/非对象 JSON 的兜底形状 #{}）时必须返回
%% 400，不得 crash。
%% RED 证据（修复前实测）：真实 jsone:decode 在 src/api/user_device_handler.erl:145
%% 对 <<"this is { not json">> 抛 function_clause（jsone_decode:number_integer_part）
%% → cowboy 进程 500。修复后 handler 走 elib_param:post seam，坏输入经
%% validate_device_type 判 false → 400。
%% 有效请求的解析行为由 elib_param 自己的契约保证（同文件其余 5 个 action
%% 已走同一 seam）；探针为纯 mock 边界测试，不启 app（对齐 web_delete_probe）。
malformed_body_returns_400_test_() ->
    [?_test(malformed_body_probe(Action)) || Action <- [check_login, kick, kick_others]].

malformed_body_probe(Action) ->
    Modules = [auth_ds, elib_param, elib_response],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(auth_ds, current_uid, fun(S) -> maps:get(current_uid, S) end),
        meck:expect(elib_param, post, fun(_) ->
            %% elib_param:post 对坏 JSON/非对象 JSON 的兜底返回
            #{}
        end),
        meck:expect(elib_response, error, fun(R, _Msg, Code) ->
            R#{result => {error, Code}}
        end),
        meck:expect(elib_response, success, fun(R, _Payload) ->
            R#{result => success}
        end),
        Result = user_device_handler:handle_action(Action, #{}, #{current_uid => 12345}),
        ?assertEqual({error, 400}, maps:get(result, Result))
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.

%% Pure boundary tests: deliberately avoid WITH_MECKS, which boots the app.
web_delete_bound_device_test_() ->
    [
        ?_test(web_delete_probe(Body, State, Expected))
     || {Body, State, Expected} <- [
            {
                #{<<"did">> => <<"web-self">>},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                ok
            },
            {
                #{<<"did">> => <<>>},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                denied
            },
            {
                #{},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                denied
            },
            {
                #{<<"did">> => 123},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                denied
            },
            {
                #{<<"did">> => <<"other-device">>},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                denied
            },
            {#{<<"did">> => <<"web-self">>}, #{surface => web_human, current_uid => 12345}, denied},
            {
                #{<<"did">> => <<"web-self">>, <<"uid">> => 99999},
                #{surface => web_human, current_uid => 12345, current_did => <<"web-self">>},
                ok
            },
            {#{<<"did">> => <<"native-other">>}, #{current_uid => 12345}, ok}
        ]
    ].

web_delete_probe(Body, State, Expected) ->
    Modules = [auth_ds, elib_param, user_device_logic, elib_response],
    lists:foreach(fun(M) -> meck:new(M, [non_strict, no_link]) end, Modules),
    try
        meck:expect(auth_ds, current_uid, fun(S) -> maps:get(current_uid, S) end),
        meck:expect(elib_param, post, fun(_) -> Body end),
        meck:expect(user_device_logic, delete, fun(12345, _) -> ok end),
        meck:expect(elib_response, success, fun(R) -> R#{result => ok} end),
        meck:expect(elib_response, error, fun(R, _, 400) -> R#{result => denied} end),
        Result = user_device_handler:handle_action(delete, #{}, State),
        ?assertEqual(Expected, maps:get(result, Result)),
        case Expected of
            ok ->
                ?assert(
                    meck:called(
                        user_device_logic,
                        delete,
                        [12345, maps:get(<<"did">>, Body)]
                    )
                ),
                ?assertEqual(1, meck:num_calls(user_device_logic, delete, '_'));
            denied ->
                ?assertEqual(0, meck:num_calls(user_device_logic, delete, '_'))
        end
    after
        lists:foreach(fun meck:unload/1, lists:reverse(Modules))
    end.
