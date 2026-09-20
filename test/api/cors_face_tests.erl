%%% @doc BE-W01 A03：CORS 三面（Widget / Admin / Seat）合同测试。
%%%
%%% 冻结依据 contracts/cors-auth-matrix.json：
%%%   * Widget 面 = cs 应用 origin + credentials 禁 + Content-Type/x-cs-visit-token；
%%%     预检只精确校验 Widget 应用 Origin（宿主页 page_origin 不在 CORS 层换权）；
%%%   * Admin 面（/api/adm）= admin origin + credentials true（Cookie）；
%%%   * Seat 面（cs_seat/enterprise_owner_admin 路由 + /api/v1/cs/organizations）=
%%%     admin origin + credentials false + Authorization/Last-Event-ID +
%%%     expose X-CS-Event-Retention-Seconds；
%%%   * 未标注面保持既有全局白名单行为（credentials true）——不回归；
%%%   * 三面凭据/origin 交叉 fail-closed：跨面 origin 预检 403；
%%%   * 所有带 ACAO 的响应都带 Vary: Origin。
%%%
%%% 双层验证：classify_face/2 纯函数 + 真 cowboy 监听器全中间件链
%%% （cowboy_router → cors_middleware → security_headers_middleware → handler）。
-module(cors_face_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CS_ORIGIN, <<"https://cs.example.com">>).
-define(ADMIN_ORIGIN, <<"https://adm.example.com">>).
-define(PAGE_ORIGIN, <<"https://shop.example.com">>).
-define(EVIL_ORIGIN, <<"https://evil.example.com">>).

widget_opts() ->
    #{surface => widget, auth_context => cs_visit, feature => customer_service}.

seat_opts() ->
    #{surface => tenant, auth_context => cs_seat, feature => customer_service}.

owner_opts() ->
    #{surface => tenant, auth_context => enterprise_owner_admin, feature => customer_service}.

%% ===================================================================
%% 纯函数：面判定
%% ===================================================================

classify_face_by_metadata_test() ->
    %% Widget：route metadata surface=widget（cs_widget_handler 全部路由）。
    ?assertEqual(
        widget, cors_middleware:classify_face(<<"/api/v1/cs/widget/bootstrap">>, widget_opts())
    ),
    %% Seat：cs_seat / enterprise_owner_admin 的 tenant 面路由。
    ?assertEqual(
        seat, cors_middleware:classify_face(<<"/api/v1/cs/sessions/123/claim">>, seat_opts())
    ),
    ?assertEqual(
        seat,
        cors_middleware:classify_face(
            <<"/api/v1/cs/organizations/9001/seats">>, owner_opts()
        )
    ),
    %% Admin：/api/adm 前缀（含 cs_platform_handler 的平台治理面）。
    ?assertEqual(
        admin,
        cors_middleware:classify_face(
            <<"/api/adm/customer-service/widget-installations">>,
            #{surface => platform, auth_context => platform_admin}
        )
    ),
    ?assertEqual(admin, cors_middleware:classify_face(<<"/api/adm/users">>, #{})),
    %% 未标注面：保持既有全局行为。
    ?assertEqual(
        undefined,
        cors_middleware:classify_face(<<"/api/v1/passport/login">>, #{})
    ),
    ?assertEqual(
        undefined,
        cors_middleware:classify_face(
            <<"/api/v1/cs/sessions">>, #{surface => tenant, auth_context => cs_visit}
        )
    ),
    ?assertEqual(
        undefined,
        cors_middleware:classify_face(
            <<"/api/v1/cs/sessions/queue">>, #{surface => tenant, auth_context => cs_shop_key}
        )
    ).

%% frame HTML 路径：按路径段识别（路由注册由 wiring manifest 后续应用，
%% 判定不能依赖 route metadata 先存在）。
classify_face_frame_path_test() ->
    ?assertEqual(
        widget, cors_middleware:classify_face(<<"/api/v1/cs/widget/frame/810001">>, #{})
    ),
    %% 相似但不完全一致的路径不是 frame 面（不放宽）。
    ?assertEqual(
        undefined, cors_middleware:classify_face(<<"/api/v1/cs/widget/frames">>, #{})
    ),
    ?assertEqual(
        undefined, cors_middleware:classify_face(<<"/api/v1/cs/widget/frame">>, #{})
    ).

%% ===================================================================
%% 全中间件链（真 cowboy 监听器）
%% ===================================================================

with_face_listener(Routes, Fun) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Name = list_to_atom(
        "corsface_" ++ integer_to_list(erlang:unique_integer([positive]))
    ),
    Dispatch = cowboy_router:compile([{'_', Routes}]),
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
        _ = cowboy:stop_listener(Name),
        ok = restore_cors_config()
    end.

request(Port, Method, Path, Headers) ->
    Req = iolist_to_binary([
        Method,
        <<" ">>,
        Path,
        <<" HTTP/1.1\r\n">>,
        <<"host: localhost\r\nconnection: close\r\n">>,
        [[K, <<": ">>, V, <<"\r\n">>] || {K, V} <- maps:to_list(Headers)],
        <<"\r\n">>
    ]),
    {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}], 10000),
    ok = gen_tcp:send(Socket, Req),
    Raw = recv_all(Socket),
    ok = gen_tcp:close(Socket),
    cs_test_support:parse(Raw).

recv_all(Socket) ->
    case gen_tcp:recv(Socket, 0, 10000) of
        {ok, Data} ->
            case recv_all(Socket) of
                <<>> -> Data;
                Rest -> <<Data/binary, Rest/binary>>
            end;
        {error, _} ->
            <<>>
    end.

%% 面配置注入 + 保存/恢复（cors_middleware 读 config_ds → application env）。
set_face_origins() ->
    Saved = [
        {K, application:get_env(imboy, K)}
     || K <- [cors_widget_origins, cors_admin_origins, cors_allowed_origins]
    ],
    ok = application:set_env(imboy, cors_widget_origins, [?CS_ORIGIN]),
    ok = application:set_env(imboy, cors_admin_origins, [?ADMIN_ORIGIN]),
    ok = application:set_env(imboy, cors_allowed_origins, [?ADMIN_ORIGIN]),
    Saved.

restore_cors_config() ->
    case persistent_term:get({cors_face_tests, saved}, undefined) of
        undefined ->
            ok;
        Saved ->
            lists:foreach(
                fun
                    ({K, undefined}) -> _ = application:unset_env(imboy, K);
                    ({K, {ok, V}}) -> ok = application:set_env(imboy, K, V)
                end,
                Saved
            ),
            persistent_term:erase({cors_face_tests, saved}),
            ok
    end.

preflight(Port, Path, Origin) ->
    request(Port, <<"OPTIONS">>, Path, #{
        <<"origin">> => Origin,
        <<"access-control-request-method">> => <<"POST">>,
        <<"access-control-request-headers">> => <<"content-type, x-cs-visit-token">>
    }).

widget_face_preflight_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [{"/api/v1/cs/widget/bootstrap", cors_face_echo_handler, widget_opts()}],
    with_face_listener(Routes, fun(Port) ->
        %% 合法 cs 应用 origin：204 + ACAO 精确 echo + Vary + 绝不 credentials。
        Ok = preflight(Port, <<"/api/v1/cs/widget/bootstrap">>, ?CS_ORIGIN),
        ?assertEqual(204, maps:get(status, Ok)),
        H = maps:get(headers, Ok),
        ?assertEqual(?CS_ORIGIN, maps:get(<<"access-control-allow-origin">>, H)),
        ?assert(string:find(maps:get(<<"vary">>, H, <<>>), <<"Origin">>) =/= nomatch),
        ?assertNot(is_map_key(<<"access-control-allow-credentials">>, H)),
        AllowHeaders = maps:get(<<"access-control-allow-headers">>, H),
        ?assert(string:find(AllowHeaders, <<"x-cs-visit-token">>) =/= nomatch),
        ?assert(string:find(AllowHeaders, <<"Content-Type">>) =/= nomatch),
        %% 宿主页 origin（installation 允许的是它，但 CORS 层只认 cs 应用
        %% origin——page_origin 不是认证凭据，不换 CORS 权）→ 403。
        Page = preflight(Port, <<"/api/v1/cs/widget/bootstrap">>, ?PAGE_ORIGIN),
        ?assertEqual(403, maps:get(status, Page)),
        %% evil origin → 403。
        Evil = preflight(Port, <<"/api/v1/cs/widget/bootstrap">>, ?EVIL_ORIGIN),
        ?assertEqual(403, maps:get(status, Evil)),
        %% null origin（沙箱/窜改 iframe）→ 403。
        Null = preflight(Port, <<"/api/v1/cs/widget/bootstrap">>, <<"null">>),
        ?assertEqual(403, maps:get(status, Null))
    end).

admin_face_preflight_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [{"/api/adm/[...]", cors_face_echo_handler, #{}}],
    with_face_listener(Routes, fun(Port) ->
        Ok = preflight(Port, <<"/api/adm/customer-service/widget-installations">>, ?ADMIN_ORIGIN),
        ?assertEqual(204, maps:get(status, Ok)),
        H = maps:get(headers, Ok),
        ?assertEqual(?ADMIN_ORIGIN, maps:get(<<"access-control-allow-origin">>, H)),
        ?assertEqual(<<"true">>, maps:get(<<"access-control-allow-credentials">>, H)),
        ?assert(string:find(maps:get(<<"vary">>, H, <<>>), <<"Origin">>) =/= nomatch),
        %% cs 应用 origin 打 Admin 面 = 跨面凭据交叉 → fail-closed 403。
        Cross = preflight(
            Port, <<"/api/adm/customer-service/widget-installations">>, ?CS_ORIGIN
        ),
        ?assertEqual(403, maps:get(status, Cross))
    end).

seat_face_preflight_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [{"/api/v1/cs/organizations/[...]", cors_face_echo_handler, seat_opts()}],
    with_face_listener(Routes, fun(Port) ->
        %% Seat 面：admin origin 放行；credentials 绝不开启；头/暴露按合同。
        Ok = preflight(Port, <<"/api/v1/cs/organizations/9001/seats">>, ?ADMIN_ORIGIN),
        ?assertEqual(204, maps:get(status, Ok)),
        H = maps:get(headers, Ok),
        ?assertEqual(?ADMIN_ORIGIN, maps:get(<<"access-control-allow-origin">>, H)),
        ?assertNot(is_map_key(<<"access-control-allow-credentials">>, H)),
        AllowHeaders = maps:get(<<"access-control-allow-headers">>, H),
        ?assert(string:find(AllowHeaders, <<"Authorization">>) =/= nomatch),
        ?assert(string:find(AllowHeaders, <<"Last-Event-ID">>) =/= nomatch),
        Expose = maps:get(<<"access-control-expose-headers">>, H),
        ?assert(string:find(Expose, <<"X-CS-Event-Retention-Seconds">>) =/= nomatch),
        %% widget cs origin 打 Seat 面 → 403（Widget origin 永不获得 seat 面）。
        Cross = preflight(Port, <<"/api/v1/cs/organizations/9001/seats">>, ?CS_ORIGIN),
        ?assertEqual(403, maps:get(status, Cross))
    end).

unmarked_route_keeps_legacy_behavior_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [{"/api/v1/passport/[...]", cors_face_echo_handler, #{}}],
    with_face_listener(Routes, fun(Port) ->
        %% 全局白名单 origin（配置为 admin origin）：204 + credentials true（旧行为）。
        Ok = preflight(Port, <<"/api/v1/passport/login">>, ?ADMIN_ORIGIN),
        ?assertEqual(204, maps:get(status, Ok)),
        ?assertEqual(
            <<"true">>, maps:get(<<"access-control-allow-credentials">>, maps:get(headers, Ok))
        ),
        %% 非白名单：403（旧行为）。
        Bad = preflight(Port, <<"/api/v1/passport/login">>, ?EVIL_ORIGIN),
        ?assertEqual(403, maps:get(status, Bad))
    end).

%% BE-W01 A04：OPTIONS 与 POST 的分离语义——预检只精确校验 Widget 应用
%% Origin；非预检（POST）不被中间件 403 截停（installation+page_origin 的
%% 裁决在 handler/application 层，见 cs_widget_handler_tests A01）。
widget_non_preflight_is_not_blocked_by_middleware_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [{"/api/v1/cs/widget/bootstrap", cors_face_echo_handler, widget_opts()}],
    with_face_listener(Routes, fun(Port) ->
        %% 宿主页 origin 的 POST：不被 CORS 层截停（200 抵达 handler），
        %% 但响应不带 ACAO（不在 widget 应用 origin 名单）。
        Page = request(Port, <<"POST">>, <<"/api/v1/cs/widget/bootstrap">>, #{
            <<"origin">> => ?PAGE_ORIGIN
        }),
        ?assertEqual(200, maps:get(status, Page)),
        ?assertNot(
            is_map_key(<<"access-control-allow-origin">>, maps:get(headers, Page))
        ),
        %% cs 应用 origin 的非预检：ACAO echo + vary（credentials 仍禁）。
        Cs = request(Port, <<"POST">>, <<"/api/v1/cs/widget/bootstrap">>, #{
            <<"origin">> => ?CS_ORIGIN
        }),
        ?assertEqual(200, maps:get(status, Cs)),
        H = maps:get(headers, Cs),
        ?assertEqual(?CS_ORIGIN, maps:get(<<"access-control-allow-origin">>, H)),
        ?assertNot(is_map_key(<<"access-control-allow-credentials">>, H))
    end).

frame_path_has_no_xfo_but_keeps_other_security_headers_test() ->
    Saved = set_face_origins(),
    persistent_term:put({cors_face_tests, saved}, Saved),
    Routes = [
        {"/api/v1/cs/widget/frame/[...]", cors_face_echo_handler, widget_opts()},
        {"/api/v1/other/[...]", cors_face_echo_handler, #{}}
    ],
    with_face_listener(Routes, fun(Port) ->
        Resp = request(Port, <<"GET">>, <<"/api/v1/cs/widget/frame/810001">>, #{}),
        ?assertEqual(200, maps:get(status, Resp)),
        H = maps:get(headers, Resp),
        %% frame HTML 面：精确 frame-ancestors CSP 由 handler 出，绝不继承 XFO。
        ?assertNot(is_map_key(<<"x-frame-options">>, H)),
        %% 其余安全头保留。
        ?assertEqual(<<"nosniff">>, maps:get(<<"x-content-type-options">>, H)),
        ?assertEqual(
            <<"strict-origin-when-cross-origin">>, maps:get(<<"referrer-policy">>, H)
        ),
        %% 对照组：非 frame 路径仍带 XFO DENY（豁免只作用于 frame 面）。
        Other = request(Port, <<"GET">>, <<"/api/v1/other/x">>, #{}),
        HO = maps:get(headers, Other),
        ?assertEqual(<<"DENY">>, maps:get(<<"x-frame-options">>, HO))
    end).
