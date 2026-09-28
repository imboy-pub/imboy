%%% @doc `/seat/:public_seat_console_id` 嵌入面套件（seat-console-embed SC-BE；
%%% 零 DB）。
%%%
%%% 分层夹具（cs_widget_public_frame_tests 先例）：
%%%   * HTTP 面 = 真 cowboy 监听器（真路由表 Opts + cors/security_headers
%%%     中间件链）+ meck 的 facade——锁 200/404/405/400 线格式与响应头；
%%%   * application 面 = `cs_fake_store` 直驱写入门（CRLF/通配/userinfo 在
%%%     写入侧拒绝）与 `cs_seat_console:frame_view` 投影；
%%%   * 静态面 = 纯函数 + 源码机械断言——锁 HTML 形状（零租户键）、
%%%     /seat/ 形状谓词单源登记、SEAT_FRAME_ASSET_JS 常量与配对表一致。
-module(cs_seat_console_handler_tests).

-include_lib("eunit/include/eunit.hrl").

-define(S, cs_test_support).
-define(FAKE, cs_fake_store).
-define(ORG, 7201001).
-define(PUBID, <<"sc_pub_http1">>).
-define(SEAT_JS, <<"/seat-assets/cs-seat.v1.js">>).
-define(SEAT_CSS, <<"/seat-assets/cs-seat.v1.css">>).
-define(SEAT_PATH, <<"/seat/">>).

seat_console_handler_test_() ->
    {foreach,
        fun() ->
            {ok, _} = application:ensure_all_started(cowboy),
            meck:new(customer_service_facade, [passthrough]),
            ok = ?FAKE:init(),
            ok = cs_fake_id:reset()
        end,
        fun(_) ->
            meck:unload(customer_service_facade),
            ok = ?FAKE:destroy(),
            ok = cs_fake_id:reset()
        end,
        [
            fun frame_http_tests/1,
            fun frame_app_gate_tests/1,
            fun frame_static_tests/1
        ]}.

%% 真 /seat/ 路由 Opts（从真路由表取）+ 完整中间件链（cors/security_headers
%% 在位——XFO 豁免是响应头合同的一部分，必须在链上断言）。
with_seat_listener(Fun) ->
    {Pattern, Opts} = ?S:route_opt(widget, seat_console_frame_html),
    Name = list_to_atom(
        "csseatpublic_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Dispatch = cowboy_router:compile([
        {'_', [{binary_to_list(Pattern), cs_seat_console_handler, Opts}]}
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

seat_url(PubId) ->
    <<?SEAT_PATH/binary, PubId/binary>>.

frame_view(Origins) ->
    #{public_seat_console_id => ?PUBID, allowed_origins => Origins}.

%% ===================================================================
%% A05/A06/A07：HTTP 线格式合同
%% ===================================================================

frame_http_tests(_) ->
    [
        {"200 frame doc: exact CSP per origin, no-store, no-referrer, no XFO", fun() ->
            meck:expect(customer_service_facade, seat_console_frame_html, fun(
                OrgId, Params
            ) ->
                %% OrgId=0 占位：反查面无 Org 输入（租户由命中行派生）。
                ?assertEqual(0, OrgId),
                ?assertEqual(#{public_seat_console_id => ?PUBID}, Params),
                {ok,
                    frame_view([
                        <<"https://shop.example.com">>, <<"https://other.example.com">>
                    ])}
            end),
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, seat_url(?PUBID), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                H = maps:get(headers, Resp),
                %% A06：allowed_origins 逐项 frame-ancestors。
                ?assertEqual(
                    <<"frame-ancestors https://shop.example.com https://other.example.com">>,
                    maps:get(<<"content-security-policy">>, H)
                ),
                ?assertEqual(<<"no-store">>, maps:get(<<"cache-control">>, H)),
                ?assertEqual(<<"no-referrer">>, maps:get(<<"referrer-policy">>, H)),
                ?assertEqual(
                    <<"text/html; charset=utf-8">>, maps:get(<<"content-type">>, H)
                ),
                %% A08：XFO 豁免面（/seat/* 经 imboy_route_shape 单源登记）；
                %% 其余安全头照常。
                ?assertNot(is_map_key(<<"x-frame-options">>, H)),
                ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, H)),
                Body = maps:get(body, Resp),
                %% A07：合同冻结形状——title + 挂载点 + 公开 id + 稳定资产。
                ?assertMatch(
                    {_, _}, binary:match(Body, <<"<title>IMBoy 客服工作台</title>"/utf8>>)
                ),
                ?assertMatch({_, _}, binary:match(Body, <<"robots\" content=\"noindex">>)),
                ?assertMatch({_, _}, binary:match(Body, <<"referrer\" content=\"no-referrer">>)),
                ?assertMatch(
                    {_, _},
                    binary:match(
                        Body, <<"id=\"cs-seat-root\" data-public-seat-console-id=\"sc_pub_http1\"">>
                    )
                ),
                ?assertMatch(
                    {_, _}, binary:match(Body, <<"href=\"/seat-assets/cs-seat.v1.css\"">>)
                ),
                ?assertMatch(
                    {_, _}, binary:match(Body, <<"src=\"", ?SEAT_JS/binary, "\"">>)
                ),
                %% 合同级禁令：壳内零租户键 / 零凭证 / 零 api-base。
                Forbidden = [
                    <<"organization">>,
                    <<"workspace">>,
                    <<"secret">>,
                    <<"token">>,
                    <<"jwt">>,
                    <<"api-base">>,
                    <<"Bearer">>,
                    <<"data-installation-id">>
                ],
                lists:foreach(
                    fun(F) -> ?assertEqual(nomatch, binary:match(Body, F)) end,
                    Forbidden
                )
            end)
        end},
        {"A06 empty allowlist renders frame-ancestors 'none'", fun() ->
            meck:expect(customer_service_facade, seat_console_frame_html, fun(_OrgId, _Params) ->
                {ok, frame_view([])}
            end),
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, seat_url(?PUBID), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                H = maps:get(headers, Resp),
                ?assertEqual(
                    <<"frame-ancestors 'none'">>, maps:get(<<"content-security-policy">>, H)
                )
            end)
        end},
        {"A06 pre-emission guard drops non-renderable origins (CRLF defense)", fun() ->
            meck:expect(customer_service_facade, seat_console_frame_html, fun(_OrgId, _Params) ->
                {ok,
                    frame_view([
                        <<"https://good.example.com">>,
                        <<"https://evil.example.com\r\nX-Injected: 1">>,
                        <<"https://*.wildcard.example.com">>
                    ])}
            end),
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, seat_url(?PUBID), <<>>, #{}),
                ?assertEqual(200, ?S:status(Resp)),
                H = maps:get(headers, Resp),
                CSP = maps:get(<<"content-security-policy">>, H),
                ?assertEqual(
                    <<"frame-ancestors https://good.example.com">>, CSP
                ),
                ?assertEqual(nomatch, binary:match(CSP, <<"\r">>)),
                ?assertEqual(nomatch, binary:match(CSP, <<"\n">>)),
                ?assertEqual(nomatch, binary:match(CSP, <<"*">>))
            end)
        end},
        {"A05 unknown id is 404 seat_console_unavailable (no enumeration)", fun() ->
            meck:expect(customer_service_facade, seat_console_frame_html, fun(_OrgId, _Params) ->
                {error, seat_console_unavailable}
            end),
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"GET">>, seat_url(?PUBID), <<>>, #{}),
                ?assertEqual(404, ?S:status(Resp)),
                ?assertEqual(<<"seat_console_unavailable">>, ?S:msg(Resp))
            end)
        end},
        {"A05 non-GET is 405", fun() ->
            meck:expect(customer_service_facade, seat_console_frame_html, fun(_OrgId, _P) ->
                meck:exception(error, must_not_be_called)
            end),
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(Port, <<"POST">>, seat_url(?PUBID), <<>>, #{}),
                ?assertEqual(405, ?S:status(Resp)),
                ?assertEqual(<<"method_not_allowed">>, ?S:msg(Resp))
            end)
        end},
        {"A05 credential style keys in query string is 400", fun() ->
            with_seat_listener(fun(Port) ->
                Resp = ?S:request(
                    Port,
                    <<"GET">>,
                    <<(seat_url(?PUBID))/binary, "?x-cs-visit-token=abc">>,
                    <<>>,
                    #{}
                ),
                ?assertEqual(400, ?S:status(Resp)),
                ?assertEqual(<<"credential_in_query_string">>, ?S:msg(Resp))
            end)
        end},
        {"A05 invalid binding shape is 400 (space / oversize)", fun() ->
            with_seat_listener(fun(Port) ->
                Bad1 = ?S:request(Port, <<"GET">>, <<"/seat/bad%20id">>, <<>>, #{}),
                ?assertEqual(400, ?S:status(Bad1)),
                ?assertEqual(<<"invalid_public_seat_console_id">>, ?S:msg(Bad1)),
                Bad2 = ?S:request(
                    Port, <<"GET">>, seat_url(binary:copy(<<"a">>, 129)), <<>>, #{}
                ),
                ?assertEqual(400, ?S:status(Bad2))
            end)
        end}
    ].

%% ===================================================================
%% 写入门：CRLF / 通配 / userinfo 在写入侧拒绝（domain 六禁形状门）
%% ===================================================================

frame_app_gate_tests(_) ->
    {"invalid origin shapes are rejected at write time (CRLF/wildcard/userinfo)", fun() ->
        Classes = [
            <<"https://shop.example.com\r\nEvil: 1">>,
            <<"https://*.example.com">>,
            <<"https://user@shop.example.com">>
        ],
        lists:foreach(
            fun(Origin) ->
                ?assertMatch(
                    {error, {invalid_origin, _}},
                    cs_seat_console_app:create_console(?ORG, #{
                        workspace_id => 92001,
                        allowed_origins => [Origin],
                        at => 1700000200,
                        store => ?FAKE,
                        id => cs_fake_id,
                        new_public_seat_console_id => fun() -> ?PUBID end
                    })
                )
            end,
            Classes
        ),
        %% 零残留：非法形状未触 store。
        ?assertEqual(
            {ok, #{seat_consoles => [], next_after_id => undefined}},
            cs_seat_console_app:list_consoles(?ORG, #{
                workspace_id => 92001, store => ?FAKE
            })
        )
    end}.

%% ===================================================================
%% 静态面：形状谓词单源 + 资产配对 + HTML 合同机械断言
%% ===================================================================

frame_static_tests(_) ->
    {"route shape predicate + pairing contract + doc purity", fun() ->
        %% A08：/seat/* 形状谓词真值表（恰两段、首段字面 seat）。
        ?assert(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat/sc_pub_x">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seat/a/b">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/seats/sc_pub_x">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/search/x">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(<<"/w/sc_pub_x">>)),
        ?assertNot(imboy_route_shape:is_cs_seat_console_frame_path(not_a_binary)),
        %% widget 谓词不吞 /seat/（两面互斥，无放宽）。
        ?assertNot(imboy_route_shape:is_cs_widget_frame_path(<<"/seat/sc_pub_x">>)),
        %% 免签直通面：/seat/* 经 cs_http 单一判定入口登记。
        ?assert(cs_http:is_credential_surface_path(<<"/seat/sc_pub_x">>)),
        %% 资产配对合同：JSON 的 seat_console 面与本模块宏逐字一致。
        {ok, PairBin} = file:read_file("priv/cs_widget_asset_pairing.json"),
        Pairs = maps:get(<<"pairs">>, jsone:decode(PairBin)),
        SeatPair = hd([P || P <- Pairs, maps:get(<<"face">>, P) =:= <<"seat_console">>]),
        ?assertEqual(
            <<"seat-assets/cs-seat.v1.js">>,
            maps:get(<<"asset_path">>, SeatPair)
        ),
        Backend = maps:get(<<"backend">>, SeatPair),
        ?assertEqual(<<"SEAT_FRAME_ASSET_JS">>, maps:get(<<"macro">>, Backend)),
        {ok, HandlerSrc} = file:read_file(
            "src/features/customer_service/interfaces/cs_seat_console_handler.erl"
        ),
        ?assertMatch(
            {match, _},
            re:run(
                HandlerSrc,
                <<"-define\\(SEAT_FRAME_ASSET_JS, <<\"/seat-assets/cs-seat.v1.js\">>\\)">>
            )
        ),
        %% 帧文档纯函数：属性值 HTML 转义（引号/尖括号/与符号）——转义后值
        %% 无法越过引号边界（"onclick" 一词仍在，但只能在属性值**内部**出现）。
        Doc = cs_seat_console_handler:frame_document(<<"x\" onclick=\"evil">>),
        ?assertEqual(
            nomatch, binary:match(Doc, <<"data-public-seat-console-id=\"x\"">>)
        ),
        ?assertMatch(
            {_, _},
            binary:match(Doc, <<"data-public-seat-console-id=\"x&quot; onclick=&quot;evil\"">>)
        ),
        %% CSP 纯函数：空名单 'none'；非列表输入 fail-closed 归 'none'。
        ?assertEqual(
            <<"frame-ancestors 'none'">>, cs_seat_console_handler:frame_csp([])
        ),
        ?assertEqual(
            <<"frame-ancestors 'none'">>, cs_seat_console_handler:frame_csp(not_a_list)
        )
    end}.
