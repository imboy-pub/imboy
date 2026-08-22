-module(bot_handler_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc bot_handler 的 API 层单元测试
%%% 覆盖：register 身份覆盖（H1 回归）、list_mine map 读取（H3 回归）、
%%%       update/disable 非属主拒绝（H2 回归）
%%%===================================================================

%% ===================================================================
%% register：owner_uid 一律取自 JWT 身份，忽略请求体
%% ===================================================================

register_ignores_body_owner_uid_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"name">> => <<"EvilBot">>,
                            <<"username">> => <<"evil">>,
                            <<"owner_uid">> => 999
                        },
                        fake_req}
                end}
            ]},
            {bot_logic, [
                {'register', 1, fun(Data) ->
                    %% 关键断言：owner_uid 必须是 JWT 身份 100，而非请求体的 999
                    ?assertEqual(100, maps:get(owner_uid, Data)),
                    ?assertEqual(<<"EvilBot">>, maps:get(name, Data)),
                    {ok, #{<<"user_id">> => 1}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            State = #{action => register, current_uid => 100},
            {ok, Req, _} = bot_handler:init(MockReq, State),
            {StatusCode, _, _} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode)
        end
    ).

%% ===================================================================
%% list_mine：从 map State 读 current_uid（H3 回归，proplists 读 map 必崩）
%% ===================================================================

list_mine_reads_current_uid_from_map_state_test_() ->
    ?WITH_MECKS(
        [
            {elib_param, [
                {'int', 3, fun(page, _Req, 1) -> {ok, 1} end}
            ]},
            {bot_logic, [
                {'list_mine', 2, fun(100, 1) ->
                    {ok, #{total => 0, page => 1, size => 20, list => []}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"GET">>}),
            State = #{action => list_mine, current_uid => 100},
            {ok, Req, _} = bot_handler:init(MockReq, State),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(0, maps:get(total, Body))
        end
    ).

%% ===================================================================
%% update：非属主请求被拒绝（H2 回归）
%% ===================================================================

update_rejects_non_owner_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"bot_id">> => 1,
                            <<"webhook_url">> => <<"https://attacker.example.com/hook">>
                        },
                        fake_req}
                end}
            ]},
            {bot_logic, [
                {'update', 3, fun(1, _Data, 999) ->
                    {error, <<"无权操作此 Bot"/utf8>>}
                end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Reason) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => #{errcode => 1}})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            State = #{action => update, current_uid => 999},
            {ok, Req, _} = bot_handler:init(MockReq, State),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            %% 业务错误经 errcode 表达（elib_response:error 返回 200 + errcode）
            ?ASSERT_EQUAL(1, maps:get(errcode, Body))
        end
    ).

%% ===================================================================
%% disable：属主身份透传到 logic（H2 回归）
%% ===================================================================

disable_passes_current_uid_to_logic_test_() ->
    ?WITH_MECKS(
        [
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"bot_id">> => 1}, fake_req}
                end}
            ]},
            {bot_logic, [
                {'set_status', 3, fun(1, 0, 100) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 0}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            State = #{action => disable, current_uid => 100},
            {ok, Req, _} = bot_handler:init(MockReq, State),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(0, maps:get(<<"status">>, Body))
        end
    ).

%% ===================================================================
%% send_message：api_token 认证 + 防骚扰前置校验（H4 回归）
%% ===================================================================

send_message_with_valid_token_sends_message_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"authorization">>, _Req, _Default) ->
                    <<"Bearer valid_token_48hex">>
                end}
            ]},
            {bot_ds, [
                {'find_by_token', 1, fun(<<"valid_token_48hex">>) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end}
            ]},
            {agent_rate_limiter, [
                {'allow', 2, fun(1, 1) -> allow end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok,
                        #{
                            <<"to_uid">> => 200,
                            <<"msg_type">> => <<"text">>,
                            <<"payload">> => #{<<"text">> => <<"Hi">>}
                        },
                        fake_req}
                end}
            ]},
            {bot_repo, [
                {'has_exchange', 2, fun(1, 200) -> true end}
            ]},
            {bot_logic, [
                {'send_message', 3, fun(1, 200, #{<<"msg_type">> := <<"text">>}) ->
                    {ok, #{<<"msg_id">> => <<"m123">>}}
                end}
            ]},
            {elib_response, [
                {'success', 2, fun(_Req, Data) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => Data})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _} = bot_handler:init(MockReq, #{action => send_message}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(<<"m123">>, maps:get(<<"msg_id">>, Body))
        end
    ).

send_message_rejects_invalid_token_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"authorization">>, _Req, _Default) ->
                    <<"Bearer wrong_token">>
                end}
            ]},
            {bot_ds, [
                {'find_by_token', 1, fun(<<"wrong_token">>) -> {error, not_found} end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Reason) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => #{errcode => 1}})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _} = bot_handler:init(MockReq, #{action => send_message}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(1, maps:get(errcode, Body))
        end
    ).

send_message_rejects_user_without_prior_exchange_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'header', 3, fun(<<"authorization">>, _Req, _Default) ->
                    <<"Bearer valid_token_48hex">>
                end}
            ]},
            {bot_ds, [
                {'find_by_token', 1, fun(<<"valid_token_48hex">>) ->
                    {ok, #{<<"user_id">> => 1, <<"status">> => 1}}
                end}
            ]},
            {agent_rate_limiter, [
                {'allow', 2, fun(1, 1) -> allow end}
            ]},
            {elib_req, [
                {'body', 2, fun(_Req, _Opts) ->
                    {ok, #{<<"to_uid">> => 999}, fake_req}
                end}
            ]},
            {bot_repo, [
                {'has_exchange', 2, fun(1, 999) -> false end},
                %% 无历史会话时绝不能触达发送逻辑
                {'find', 1, fun(_) -> exit(find_should_not_be_called) end}
            ]},
            {bot_logic, [
                {'send_message', 3, fun(_, _, _) -> exit(send_should_not_be_called) end}
            ]},
            {elib_response, [
                {'error', 2, fun(_Req, _Reason) ->
                    cowboy_req_h:new(#{response_status => 200, response_body => #{errcode => 1}})
                end}
            ]}
        ],
        fun() ->
            MockReq = cowboy_req_h:new(#{method => <<"POST">>}),
            {ok, Req, _} = bot_handler:init(MockReq, #{action => send_message}),
            {StatusCode, _, Body} = cowboy_req_h:response(Req),
            ?ASSERT_EQUAL(200, StatusCode),
            ?ASSERT_EQUAL(1, maps:get(errcode, Body))
        end
    ).
