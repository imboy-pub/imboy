%%% @doc BE-W01 A05：动态 frame HTML 端点套件（真 cowboy 监听器 + meck 的
%%% facade；零 DB）。路由注册不在本卡（wiring manifest），监听器用测试本地
%%% Dispatch 直挂 handler + 真 route Opts 形状。
%%%
%%% 冻结合同（cors-auth-matrix.json frame_policy / api-surface-freeze）：
%%%   * 按公开 installation 返回 HTML，含**精确** frame-ancestors CSP
%%%     （installation allowed_origins 逐个列出；空 = 'none'）；
%%%   * 引用版本化静态 JS（路径常量，部署属 A6）；
%%%   * 不继承 X-Frame-Options DENY/SAMEORIGIN（该面豁免 XFO、保留其他
%%%     安全头——security_headers_middleware/cors_middleware 双侧豁免）；
%%%   * installation 不存在 / revoked → 404；方法非 GET → 405；
%%%   * 凭证不进 URL（查询串凭证样式键 → 400）；响应零 secret。
-module(cs_widget_frame_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(ORG, 7001001).
-define(INSTALL, 810001).
-define(FRAME_PATH, <<"/api/v1/cs/widget/frame/">>).
-define(FRAME_JS, <<"/widget-assets/cs-widget.v1.js">>).

frame_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            ok
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            ok
        end,
        [
            fun frame_html_tests/1,
            fun frame_error_tests/1,
            fun frame_pure_functions_test/1
        ]}.

with_frame_listener(Fun) ->
    Name = list_to_atom(
        "cswwframe_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Dispatch = cowboy_router:compile([
        {'_', [
            {binary_to_list(<<?FRAME_PATH/binary, ":installation_id">>), cs_widget_frame_handler, #{
                surface => widget,
                feature => customer_service,
                auth_context => cs_visit
            }}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(Name, [{port, 0}], #{
        env => #{dispatch => Dispatch},
        middlewares => [
            cowboy_router,
            cors_middleware,
            security_headers_middleware,
            cowboy_handler
        ]
    }),
    Port = ranch:get_port(Name),
    try
        Fun(Port)
    after
        _ = cowboy:stop_listener(Name)
    end.

frame_path() ->
    <<?FRAME_PATH/binary, (integer_to_binary(?INSTALL))/binary>>.

frame_url() ->
    <<(frame_path())/binary, "?organization_id=", (integer_to_binary(?ORG))/binary>>.

installation_view(Origins) ->
    #{
        id => ?INSTALL,
        public_widget_id => <<"wgt_pub_frame">>,
        allowed_origins => Origins
    }.

%% ===================================================================
%% 200 面：HTML + CSP + XFO 豁免 + 版本化 JS
%% ===================================================================

frame_html_tests(_) ->
    [
        {"A05 frame HTML echoes allowed_origins one by one in frame-ancestors CSP", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(Org, Params) ->
                ?assertEqual(?ORG, Org),
                ?assertEqual(?INSTALL, maps:get(installation_id, Params)),
                {ok,
                    installation_view([
                        <<"https://shop.example.com">>, <<"https://other.example.com">>
                    ])}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                H = maps:get(headers, Resp),
                ?assertEqual(
                    <<"frame-ancestors https://shop.example.com https://other.example.com">>,
                    maps:get(<<"content-security-policy">>, H)
                ),
                %% XFO 绝不出现在该面（继承 DENY 会让一切嵌入失败）。
                ?assertNot(is_map_key(<<"x-frame-options">>, H)),
                %% 其余安全头保留。
                ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, H)),
                ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, H)),
                %% 版本化静态 JS 引用（产物部署属 A6）。
                ?assertEqual(
                    nomatch,
                    binary:match(maps:get(body, Resp), <<"src=\"x\"">>)
                ),
                ?assertMatch(
                    {_, _}, binary:match(maps:get(body, Resp), <<"src=\"", ?FRAME_JS/binary, "\"">>)
                )
            end)
        end},

        {"A05 empty allowlist yields frame-ancestors 'none'", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(_O, _P) ->
                {ok, installation_view([])}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                ?assertEqual(
                    <<"frame-ancestors 'none'">>,
                    maps:get(<<"content-security-policy">>, maps:get(headers, Resp))
                )
            end)
        end},

        {"A05 frame document carries public ids only (no secret/token in body)", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(_O, _P) ->
                {ok, installation_view([<<"https://shop.example.com">>])}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                Body = maps:get(body, Resp),
                ?assertMatch({_, _}, binary:match(Body, <<"data-installation-id=\"810001\"">>)),
                ?assertMatch(
                    {_, _}, binary:match(Body, <<"data-public-widget-id=\"wgt_pub_frame\"">>)
                ),
                ?assertEqual(nomatch, binary:match(Body, <<"secret">>)),
                ?assertEqual(nomatch, binary:match(Body, <<"token">>))
            end)
        end}
    ].

%% ===================================================================
%% 错误面：404（不存在/revoked）、405、400（缺参/凭证进 URL）
%% ===================================================================

frame_error_tests(_) ->
    [
        {"A05 missing installation is 404 (no enumeration)", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(_O, _P) ->
                {error, not_found}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                ?assertEqual(404, ?S:status(Resp))
            end)
        end},

        {"A05 revoked installation is 404 (kill switch does not show shape)", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(_O, _P) ->
                {error, installation_revoked}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                ?assertEqual(404, ?S:status(Resp))
            end)
        end},

        {"A05 wrong org hits not_found in store (no row crosses org)", fun() ->
            meck:expect(customer_service_facade, widget_frame_html, fun(Org, _P) ->
                ?assertEqual(?ORG, Org),
                {error, not_found}
            end),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_url(), <<>>, #{}),
                ?assertEqual(404, ?S:status(Resp))
            end)
        end},

        {"A05 non-GET method is 405", fun() ->
            meck:reset(customer_service_facade),
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"POST">>, frame_url(), <<>>, #{}),
                ?assertEqual(405, ?S:status(Resp)),
                ?assertNot(meck:called(customer_service_facade, widget_frame_html, '_'))
            end)
        end},

        {"A05 missing organization_id is a structured 400", fun() ->
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, frame_path(), <<>>, #{}),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"missing_org_id">>, ?S:msg(Resp))
            end)
        end},

        {"A05 invalid installation_id tsid is a structured 400", fun() ->
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<?FRAME_PATH/binary, "not-a-tsid?organization_id=",
                        (integer_to_binary(?ORG))/binary>>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp))
            end)
        end},

        {"A05 credential-style query keys are rejected 400 (token never in URL)", fun() ->
            with_frame_listener(fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<
                        (frame_path())/binary,
                        "?organization_id=",
                        (integer_to_binary(?ORG))/binary,
                        "&token=wtok-x"
                    >>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"credential_in_query_string">>, ?S:msg(Resp))
            end)
        end}
    ].

%% ===================================================================
%% 纯函数（零 socket）
%% ===================================================================

frame_pure_functions_test(_) ->
    [
        fun() ->
            %% CSP 逐 origin 列出 / 空名单 'none'。
            ?assertEqual(
                <<"frame-ancestors https://a.example.com https://b.example.com">>,
                cs_widget_frame_handler:frame_ancestors_csp([
                    <<"https://a.example.com">>, <<"https://b.example.com">>
                ])
            ),
            ?assertEqual(
                <<"frame-ancestors 'none'">>,
                cs_widget_frame_handler:frame_ancestors_csp([])
            ),
            %% 文档 HTML 属性转义（公开 id 仍是文本安全的一等公民）。
            Doc = cs_widget_frame_handler:frame_document(810001, <<"w&quot;x">>),
            ?assertEqual(nomatch, binary:match(Doc, <<"&quot;w&quot;quot;x">>)),
            ?assertMatch({_, _}, binary:match(Doc, <<"w&amp;quot;x">>)),
            %% 版本化 JS 常量在文档内。
            ?assertMatch({_, _}, binary:match(Doc, <<"src=\"", ?FRAME_JS/binary, "\"">>))
        end,
        fun ban_non_http_scheme_not_in_csp/0,
        fun ban_wildcard_not_in_csp/0,
        fun ban_space_control_char_not_in_csp/0,
        fun ban_path_not_in_csp/0,
        fun ban_userinfo_not_in_csp/0,
        fun ban_crlf_not_in_csp/0
    ].

%% ===================================================================
%% CSD-BE-01T（合同 S4 六禁端到端，SEC-1 修复）：每个被禁形状
%% `cs_widget:normalize_origin/1` 必须 error，且经归一门出口过滤
%% （对齐 `cs_widget_app:normalized_allowed_origins/1` 的 filtermap 语义）
%% 后进 `frame_ancestors_csp/1` 的 CSP 值里裸值不出现——写入口 fail-closed
%% 与读出口过滤双重保证，形状非法值绝不上 frame-ancestors 头。
%% ===================================================================

assert_ban_not_in_csp(Raw) ->
    ?assertMatch({error, {invalid_origin, _}}, cs_widget:normalize_origin(Raw)),
    Filtered =
        lists:filtermap(
            fun(O) ->
                case cs_widget:normalize_origin(O) of
                    {ok, Norm} -> {true, Norm};
                    {error, _} -> false
                end
            end,
            [<<"https://good.example.com">>, Raw]
        ),
    Csp = cs_widget_frame_handler:frame_ancestors_csp(Filtered),
    ?assertEqual(<<"frame-ancestors https://good.example.com">>, Csp).

ban_non_http_scheme_not_in_csp() ->
    assert_ban_not_in_csp(<<"ftp://h.com:21">>).

ban_wildcard_not_in_csp() ->
    assert_ban_not_in_csp(<<"https://*.evil.com">>).

ban_space_control_char_not_in_csp() ->
    assert_ban_not_in_csp(<<"https://a.com X">>).

ban_path_not_in_csp() ->
    assert_ban_not_in_csp(<<"https://a.com/x">>).

ban_userinfo_not_in_csp() ->
    assert_ban_not_in_csp(<<"https://u@a.com">>).

ban_crlf_not_in_csp() ->
    assert_ban_not_in_csp(<<"https://a.com\r\nEvil">>).
