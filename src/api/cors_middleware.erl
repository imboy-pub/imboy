-module(cors_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @doc CORS 中间件
%% 处理跨域资源共享 (CORS) 预检请求和响应头
%%
%% 支持前端从 localhost 或其他域名访问 API
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
            "cos, vsn, pkg, did, tz_offset, method, sk, sign, token, x-refresh-token, "
            "imboy-refreshtoken, "
            "device-type, device-type-vsn, device-id, device-name, device-name-vsn, "
            "platform, user-agent, content-length, X-Auth-Token, referer, Referrer-Policy"
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

%% @doc 判断来源是否在白名单中
-spec is_origin_allowed(binary() | undefined, [binary()]) -> boolean().
is_origin_allowed(undefined, _AllowedOrigins) ->
    false;
is_origin_allowed(Origin, AllowedOrigins) ->
    % 首先检查精确匹配
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

%% @doc 判断是否为 localhost 来源（支持任意端口）
-spec is_localhost_origin(binary()) -> boolean().
is_localhost_origin(Origin) ->
    OriginStr = ec_cnv:to_list(Origin),
    % 匹配 http://localhost:端口 或 https://localhost:端口
    case re:run(OriginStr, "^https?://(localhost|127\\.0\\.0\\.1)(:[0-9]+)?$", [{capture, none}]) of
        match -> true;
        _ -> false
    end.
