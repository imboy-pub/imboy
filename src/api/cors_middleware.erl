-module(cors_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% BE-W01 A03：三面 CORS 判定（纯函数，测试直调）。
-export([classify_face/2]).

%% @doc CORS 中间件
%% 处理跨域资源共享 (CORS) 预检请求和响应头
%%
%% 支持前端从 localhost 或其他域名访问 API
%%
%% BE-W01 三面改造（contracts/cors-auth-matrix.json）：
%%   * Widget 面（surface=widget 或 frame HTML 路径）：cs 应用 origin 精确
%%     校验，credentials 禁，allow-headers = Content-Type/x-cs-visit-token；
%%     预检非精确命中即 403 fail-closed（宿主页 page_origin 不在本层换权）；
%%   * Admin 面（/api/adm 前缀）：admin origin + credentials true（Cookie）；
%%   * Seat 面（cs_seat/enterprise_owner_admin 路由或 /api/v1/cs/organizations
%%     前缀）：admin origin + credentials 禁 + Authorization/Last-Event-ID +
%%     expose X-CS-Event-Retention-Seconds；
%%   * 未标注面：保持既有全局白名单行为（不回归）。
%%   * frame HTML 路径豁免 X-Frame-Options（精确 frame-ancestors CSP 由
%%     handler 按 installation allowed_origins 出）；其余安全头保留。
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
%% @end
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req0, Env) ->
    Method = cowboy_req:method(Req0),
    Origin = cowboy_req:header(<<"origin">>, Req0),
    Path = cowboy_req:path(Req0),
    HandlerOpts = maps:get(handler_opts, Env, #{}),
    case classify_face(Path, HandlerOpts) of
        undefined ->
            legacy_execute(Req0, Env, Method, Origin);
        Face ->
            face_execute(Req0, Env, Method, Origin, Face)
    end.

%% ===================================================================
%% 面判定（route metadata 优先，路径前缀兜底；frame 路径按段精确识别）
%% ===================================================================

%% @doc 按 route metadata（surface/auth_context，cowboy_router 已先于本中间件
%% 解析进 Env）+ 路径前缀判定 CORS 面。未标注返回 undefined（既有全局行为）。
-spec classify_face(binary(), map()) -> widget | admin | seat | undefined.
classify_face(Path, HandlerOpts) when is_binary(Path) ->
    case widget_frame_path(segments(Path)) of
        true ->
            %% frame HTML 端点（iframe src 导航）：归属 widget 面；路由注册由
            %% wiring manifest 应用，判定不依赖 metadata 先存在。
            widget;
        false ->
            classify_by_metadata(HandlerOpts, Path)
    end;
classify_face(_Path, _HandlerOpts) ->
    undefined.

classify_by_metadata(#{surface := Surface} = Opts, Path) ->
    case Surface of
        widget ->
            widget;
        tenant ->
            case maps:get(auth_context, Opts, undefined) of
                Ctx when Ctx =:= cs_seat; Ctx =:= enterprise_owner_admin ->
                    seat;
                _ ->
                    classify_by_prefix(Path)
            end;
        _ ->
            classify_by_prefix(Path)
    end;
classify_by_metadata(_Opts, Path) ->
    classify_by_prefix(Path).

classify_by_prefix(Path) ->
    case Path of
        <<"/api/adm", _/binary>> ->
            admin;
        <<"/api/v1/cs/organizations/", _/binary>> ->
            %% 坐席/治理面路径前缀兜底（未来 seat SSE 等新路由未带 metadata
            %% 时仍按 seat 面收口；凭据校验照旧在 auth 层 fail-closed）。
            seat;
        _ ->
            undefined
    end.

%% frame HTML 路径段形状：[api, v1, cs, widget, frame, :installation_id]。
widget_frame_path([<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"frame">>, _Id]) ->
    true;
widget_frame_path(_Other) ->
    false.

segments(Path) ->
    [S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>].

%% ===================================================================
%% 三面执行（Widget / Admin / Seat）
%% ===================================================================

face_execute(Req0, Env, Method, Origin, Face) ->
    Policy = face_policy(Face),
    IsAllowed = is_face_origin_allowed(Origin, face_origins(Face)),
    Req1 =
        case {Origin, IsAllowed} of
            {undefined, _} ->
                Req0;
            {_O, true} ->
                ReqA = cowboy_req:set_resp_header(
                    <<"access-control-allow-origin">>, Origin, Req0
                ),
                ReqB = cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, ReqA),
                maybe_credentials(Policy, ReqB);
            {_O, false} ->
                Req0
        end,
    Req2 = set_resp_header_if_defined(
        maps:get(allow_methods, Policy), <<"access-control-allow-methods">>, Req1
    ),
    Req3 = set_resp_header_if_defined(
        maps:get(allow_headers, Policy), <<"access-control-allow-headers">>, Req2
    ),
    Req4 = set_resp_header_if_defined(
        maps:get(expose_headers, Policy), <<"access-control-expose-headers">>, Req3
    ),
    Req5 = cowboy_req:set_resp_header(<<"access-control-max-age">>, <<"3600">>, Req4),
    Req6 = face_security_headers(Req0, Req5),
    case Method of
        <<"OPTIONS">> when Origin =/= undefined, IsAllowed =:= false ->
            %% 三面预检 fail-closed：非本面 origin 一律 403（跨面凭据交叉在此拦）。
            ReqDenied = cowboy_req:reply(
                403,
                #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                <<"{\"msg\":\"CORS origin not allowed\"}">>,
                Req6
            ),
            {stop, ReqDenied};
        <<"OPTIONS">> ->
            ReqFinal = cowboy_req:reply(204, Req6),
            {stop, ReqFinal};
        _ ->
            {ok, Req6, Env}
    end.

%% 面的安全头：与既有全局口径一致（nosniff/XFO/XSS），唯 frame HTML 路径
%% 豁免 XFO——精确 frame-ancestors CSP 由 handler 按 installation 出。
face_security_headers(ReqOriginal, Req0) ->
    Req1 = cowboy_req:set_resp_header(
        <<"x-content-type-options">>, <<"nosniff">>, Req0
    ),
    Req2 =
        case widget_frame_path(segments(cowboy_req:path(ReqOriginal))) of
            true -> Req1;
            false -> cowboy_req:set_resp_header(<<"x-frame-options">>, <<"DENY">>, Req1)
        end,
    cowboy_req:set_resp_header(<<"x-xss-protection">>, <<"1; mode=block">>, Req2).

maybe_credentials(#{credentials := true}, Req) ->
    cowboy_req:set_resp_header(<<"access-control-allow-credentials">>, <<"true">>, Req);
maybe_credentials(_Policy, Req) ->
    %% Widget/Seat 面绝不开启 credentials（冻结合同：Widget Origin 永不获得
    %% credentialed CORS；Seat 走 Authorization 头，不需要 Cookie）。
    Req.

set_resp_header_if_defined(undefined, _Header, Req) ->
    Req;
set_resp_header_if_defined(Value, Header, Req) ->
    cowboy_req:set_resp_header(Header, Value, Req).

%% 面策略：headers 大小写按浏览器实际回显习惯（access-control-* 头名
%% 不区分大小写，值内保持冻结合同列出的形态）。
face_policy(widget) ->
    #{
        credentials => false,
        allow_methods => <<"GET, POST, OPTIONS">>,
        allow_headers => <<"Content-Type, x-cs-visit-token">>,
        expose_headers => <<"content-type, content-length">>
    };
face_policy(admin) ->
    #{
        credentials => true,
        allow_methods => <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>,
        allow_headers =>
            <<
                "Content-Type, Authorization, Accept, Origin, X-Requested-With, Cookie, "
                "cos, vsn, pkg, terminology-profile, did, tz_offset, method, sk, sign, token, x-refresh-token, "
                "imboy-refreshtoken, "
                "device-type, device-type-vsn, device-id, device-name, device-name-vsn, "
                "platform, user-agent, content-length, X-Auth-Token, referer, Referrer-Policy, "
                "x-cs-visit-token, x-cs-shop-key, Last-Event-ID"
            >>,
        expose_headers => <<"content-type, content-length, authorization">>
    };
face_policy(seat) ->
    #{
        credentials => false,
        allow_methods => <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>,
        allow_headers => <<"Content-Type, Authorization, Accept, Last-Event-ID">>,
        expose_headers =>
            <<"content-type, content-length, authorization, X-CS-Event-Retention-Seconds">>
    }.

%% 面的 origin 名单：
%%   * Widget：只认 cors_widget_origins（无全局白名单回退——admin origin
%%     不得经全局名单混入 widget 面预检）；
%%   * Admin/Seat：cors_admin_origins，未配置回退全局白名单（既有部署行为
%%     保持，不回归）。
face_origins(widget) ->
    config_list(cors_widget_origins);
face_origins(admin) ->
    face_origins_with_global_fallback(cors_admin_origins);
face_origins(seat) ->
    face_origins_with_global_fallback(cors_admin_origins).

face_origins_with_global_fallback(Key) ->
    case config_list(Key) of
        [] -> allowed_origins();
        Configured -> Configured
    end.

config_list(Key) ->
    case config_ds:env(Key, []) of
        L when is_list(L) -> [ec_cnv:to_binary(O) || O <- L];
        Single -> [ec_cnv:to_binary(Single)]
    end.

%% ===================================================================
%% 既有全局行为（未标注面；原样保留，不回归）
%% ===================================================================

legacy_execute(Req0, Env, Method, Origin) ->
    AllowedOrigins = allowed_origins(),
    IsAllowedOrigin = is_origin_allowed(Origin, AllowedOrigins),

    % 设置 CORS 响应头
    Req1 =
        case {Origin, IsAllowedOrigin} of
            {undefined, _} ->
                % 没有 Origin 头，可能是同源请求或非浏览器请求
                Req0;
            {_Origin, true} ->
                % 只允许白名单中的来源
                ReqA = cowboy_req:set_resp_header(
                    <<"access-control-allow-origin">>,
                    Origin,
                    Req0
                ),
                cowboy_req:set_resp_header(<<"vary">>, <<"Origin">>, ReqA);
            {_Origin, false} ->
                Req0
        end,

    Req2 = cowboy_req:set_resp_header(
        <<"access-control-allow-methods">>,
        <<"GET, POST, PUT, DELETE, OPTIONS, PATCH">>,
        Req1
    ),

    Req3 = cowboy_req:set_resp_header(
        <<"access-control-allow-headers">>,
        <<
            "Content-Type, Authorization, Accept, Origin, X-Requested-With, "
            "cos, vsn, pkg, terminology-profile, did, tz_offset, method, sk, sign, token, x-refresh-token, "
            "imboy-refreshtoken, "
            "device-type, device-type-vsn, device-id, device-name, device-name-vsn, "
            "platform, user-agent, content-length, X-Auth-Token, referer, Referrer-Policy, "
            "x-cs-visit-token, x-cs-shop-key, Last-Event-ID"
        >>,
        Req2
    ),

    Req4 = cowboy_req:set_resp_header(
        <<"access-control-expose-headers">>,
        <<"content-type, content-length, authorization">>,
        Req3
    ),

    Req5 = cowboy_req:set_resp_header(
        <<"access-control-max-age">>,
        <<"3600">>,
        Req4
    ),

    Req6 =
        case IsAllowedOrigin of
            true ->
                cowboy_req:set_resp_header(
                    <<"access-control-allow-credentials">>,
                    <<"true">>,
                    Req5
                );
            false ->
                Req5
        end,

    % 安全响应头（防止 MIME 嗅探、点击劫持、XSS）
    % 注意：Strict-Transport-Security (HSTS) 应在 nginx 层配置，不在此处设置
    Req7 = cowboy_req:set_resp_header(
        <<"x-content-type-options">>,
        <<"nosniff">>,
        Req6
    ),
    Req8 = cowboy_req:set_resp_header(
        <<"x-frame-options">>,
        <<"DENY">>,
        Req7
    ),
    Req9 = cowboy_req:set_resp_header(
        <<"x-xss-protection">>,
        <<"1; mode=block">>,
        Req8
    ),

    % 处理 OPTIONS 预检请求
    case Method of
        <<"OPTIONS">> when Origin =/= undefined, IsAllowedOrigin =:= false ->
            ReqDenied = cowboy_req:reply(
                403,
                #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                <<"{\"msg\":\"CORS origin not allowed\"}">>,
                Req9
            ),
            {stop, ReqDenied};
        <<"OPTIONS">> ->
            % 预检请求直接返回 204 No Content
            ReqFinal = cowboy_req:reply(204, Req9),
            {stop, ReqFinal};
        _ ->
            % 其他请求继续处理
            {ok, Req9, Env}
    end.

%% @doc 获取允许的 CORS 来源列表
-spec allowed_origins() -> [binary()].
allowed_origins() ->
    case config_ds:env(cors_allowed_origins, undefined) of
        undefined ->
            BaseUrl = ec_cnv:to_binary(config_ds:env(base_url, <<>>)),
            case BaseUrl of
                <<>> -> [];
                _ -> [BaseUrl]
            end;
        Origins when is_list(Origins) ->
            [ec_cnv:to_binary(O) || O <- Origins];
        Origin ->
            [ec_cnv:to_binary(Origin)]
    end.

%% @doc 判断来源是否在白名单中（既有全局口径：精确匹配 + 可选 localhost）
-spec is_origin_allowed(binary() | undefined, [binary()]) -> boolean().
is_origin_allowed(undefined, _AllowedOrigins) ->
    false;
is_origin_allowed(Origin, AllowedOrigins) ->
    case lists:member(Origin, AllowedOrigins) of
        true ->
            true;
        false ->
            % 检查是否允许 localhost 任意端口（开发环境）
            AllowLocalhost = config_ds:env(cors_allow_localhost, false),
            case AllowLocalhost of
                true -> is_localhost_origin(Origin);
                false -> false
            end
    end.

%% @doc 面 origin 匹配（BE-W01）：精确 + scheme/host 大小写折叠（配置值写
%% 大写也能命中）；端口/路径不做容错——配置值必须是归一化 origin。
-spec is_face_origin_allowed(binary() | undefined, [binary()]) -> boolean().
is_face_origin_allowed(undefined, _AllowedOrigins) ->
    false;
is_face_origin_allowed(Origin, AllowedOrigins) ->
    lists:member(Origin, AllowedOrigins) orelse
        lists:member(
            string:lowercase(Origin), [string:lowercase(O) || O <- AllowedOrigins]
        ).

%% @doc 判断是否为 localhost 来源（支持任意端口）
-spec is_localhost_origin(binary()) -> boolean().
is_localhost_origin(Origin) ->
    OriginStr = ec_cnv:to_list(Origin),
    % 匹配 http://localhost:端口 或 https://localhost:端口
    case re:run(OriginStr, "^https?://(localhost|127\\.0\\.0\\.1)(:[0-9]+)?$", [{capture, none}]) of
        match -> true;
        _ -> false
    end.
