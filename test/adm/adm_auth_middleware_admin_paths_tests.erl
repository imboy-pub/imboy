-module(adm_auth_middleware_admin_paths_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc
%%% adm_auth_middleware 管理端新增路径回归测试
%%%
%%% 目标：确认 Console-6 新增 handler 场景下认证行为稳定
%%%===================================================================

execute_allows_passport_path_without_auth_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/adm/passport/login">> end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{handler_opts => #{trace => true}, has_sent_resp => false},
            {ok, _ReqOut, EnvOut} = adm_auth_middleware:execute(Req, Env),
            ?assertEqual(#{handler_opts => #{trace => true}, has_sent_resp => false}, EnvOut)
        end
    ).

execute_rejects_new_admin_api_path_without_cookie_test_() ->
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/adm/stats/overview">> end},
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
                {'reply', 4, fun(Code, Headers, Body, Req) ->
                    Req#{
                        response_status => Code, response_headers => Headers, response_body => Body
                    }
                end}
            ]},
            {config_ds, [
                {'env', 2, fun
                    (start_mode, _Default) -> http;
                    (_, Default) -> Default
                end}
            ]},
            {elib_req, [
                {'cookie', 2, fun(_Name, _Req) -> undefined end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000000000 end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{handler_opts => #{}},
            {stop, RespReq} = adm_auth_middleware:execute(Req, Env),
            ?assertEqual(401, maps:get(response_status, RespReq)),
            Headers = maps:get(response_headers, RespReq),
            ?assertEqual(
                <<"application/json; charset=utf-8">>,
                maps:get(<<"content-type">>, Headers)
            )
        end
    ).

execute_api_adm_root_unauthorized_json_test_() ->
    %% CI-00 修桩：/api/adm/* 未授权现语义为 401 JSON（"仅页面入口 302，API
    %% 统一 401"，见 unauthorized_api_response/should_redirect_to_login）；
    %% 原用例假设全部 302 且只 mock reply/3 -> passthrough function_clause。
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/api/adm/">> end},
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
                {'reply', 4, fun(Code, Headers, Body, Req) ->
                    Req#{
                        response_status => Code,
                        response_headers => Headers,
                        response_body => Body
                    }
                end}
            ]},
            {config_ds, [
                {'env', 2, fun
                    (start_mode, _Default) -> http;
                    (_, Default) -> Default
                end}
            ]},
            {elib_req, [
                {'cookie', 2, fun(_Name, _Req) -> undefined end}
            ]},
            {elib_dt, [
                {'millisecond', 0, fun() -> 1700000000000 end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{handler_opts => #{}},
            {stop, RespReq} = adm_auth_middleware:execute(Req, Env),
            ?assertEqual(401, maps:get(response_status, RespReq)),
            Headers = maps:get(response_headers, RespReq),
            ?assertEqual(
                <<"application/json; charset=utf-8">>,
                maps:get(<<"content-type">>, Headers)
            )
        end
    ).

execute_keeps_redirect_for_page_entry_without_cookie_test_() ->
    %% CI-00 补充：页面入口 /adm/index 未授权保持 302（原语义保留覆盖）。
    ?WITH_MECKS(
        [
            {cowboy_req, [
                {'path', 1, fun(_Req) -> <<"/adm/index">> end},
                {'method', 1, fun(_Req) -> <<"GET">> end},
                {'uri', 1, fun(_Req) -> <<"https://example.com/adm/index">> end},
                {'set_resp_cookie', 4, fun(_Name, _Value, Req, _Opts) -> Req end},
                {'reply', 3, fun(Code, Headers, Req) ->
                    Req#{response_status => Code, response_headers => Headers}
                end}
            ]},
            {config_ds, [
                {'env', 2, fun
                    (start_mode, _Default) -> http;
                    (_, Default) -> Default
                end}
            ]},
            {elib_req, [
                {'cookie', 2, fun(_Name, _Req) -> undefined end}
            ]}
        ],
        fun() ->
            Req = #{},
            Env = #{handler_opts => #{}},
            {stop, RespReq} = adm_auth_middleware:execute(Req, Env),
            ?assertEqual(302, maps:get(response_status, RespReq)),
            Headers = maps:get(response_headers, RespReq),
            ?assertEqual("/adm/passport/login", maps:get(<<"Location">>, Headers))
        end
    ).
