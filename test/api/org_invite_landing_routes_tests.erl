-module(org_invite_landing_routes_tests).
-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% APP-M02：/invite/org/:code 路由契约 + 真实 HTTP 矩阵
%%
%% 自带专用 Cowboy listener（端口 0 随机）：真实路由表（imboy_router:
%% get_routes）+ 真实中间件链（与 intbe02_http_support 同款 ?MIDDLEWARES，
%% HTTP 层零 mock）。单跑 handler 纯函数覆盖不了的三件事在这里验证：
%%   1. 路由确实注册进了 cowboy 调度表；
%%   2. auth_middleware 对 /invite/ 前缀直通（若落入兜底 verify_sign
%%      分支会 403/902，拿不到 HTML）；
%%   3. 契约头（content-type / cache-control / CSP）真实下发给浏览器。
%% ===================================================================

-define(LISTENER, org_invite_landing_http_listener).

-define(MIDDLEWARES, [
    cowboy_router,
    cors_middleware,
    security_headers_middleware,
    auth_middleware,
    feature_gate_middleware,
    throttle_middleware,
    cowboy_handler
]).

%% 路由表契约：/invite/org/:code 挂 org_invite_landing_handler、零 Opts
%%（鉴权不走 opts，由 auth_middleware 前缀分支直通）。
route_registered_test() ->
    [{_, All}] = imboy_router:get_routes(),
    [{"/invite/org/:code", org_invite_landing_handler, Opts}] =
        [R || {P, _, _} = R <- All, P =:= "/invite/org/:code"],
    ?assertEqual(#{}, Opts).

landing_http_matrix_test_() ->
    {setup, fun start_listener/0, fun stop_listener/1, fun(Port) ->
        fun() -> matrix(Port) end
    end}.

start_listener() ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(inets),
    %% throttle 中间件在链上：api_per_ip 桶初始化为高配额（镜像
    %% intbe02_http_support:setup_all 的注入口径），避免 rate_not_set
    %% 或测试期限流引入噪声；本套件只发个位数请求。
    {ok, _} = application:ensure_all_started(throttle),
    application:set_env(throttle, rates, [{api_per_ip, 100000, per_minute}]),
    catch throttle:setup(api_per_ip, 100000, per_minute),
    Dispatch = cowboy_router:compile(imboy_router:get_routes()),
    {ok, _} = cowboy:start_clear(
        ?LISTENER,
        [{port, 0}],
        #{env => #{dispatch => Dispatch}, middlewares => ?MIDDLEWARES}
    ),
    ranch:get_port(?LISTENER).

stop_listener(_Port) ->
    ok = cowboy:stop_listener(?LISTENER).

matrix(Port) ->
    Base = "http://127.0.0.1:" ++ integer_to_list(Port),

    %% 合法码 → 200：契约头齐 + 正文含码与深链 payload
    {ok, {{_, 200, _}, H200, Body200}} =
        http_get(Base ++ "/invite/org/AB2C3D4E"),
    ?assertEqual("text/html; charset=utf-8", ct(H200)),
    ?assertEqual("no-store", proplists:get_value("cache-control", H200)),
    ?assert(
        string:find(
            proplists:get_value("content-security-policy", H200, ""),
            "default-src 'none'"
        ) =/= nomatch
    ),
    ?assert(nomatch =/= binary:match(Body200, <<"AB2C3D4E">>)),
    ?assert(
        nomatch =/= binary:match(Body200, <<"imboy://org/join?c=AB2C3D4E">>)
    ),

    %% 字符集外（0）与形状外（短码）→ 404 静态无效页
    {ok, {{_, 404, _}, _H404, Body404}} =
        http_get(Base ++ "/invite/org/AB2C3D40"),
    ?assert(
        nomatch =/= binary:match(Body404, <<"邀请链接无效"/utf8>>)
    ),
    {ok, {{_, 404, _}, _, _}} = http_get(Base ++ "/invite/org/SHORT"),

    %% HEAD → 200（无正文由 cowboy 保证）
    {ok, {{_, 200, _}, _, _}} =
        httpc:request(
            head,
            {Base ++ "/invite/org/AB2C3D4E", []},
            [{autoredirect, false}],
            []
        ),

    %% 写方法 → 405 + allow 头
    {ok, {{_, 405, _}, H405, _}} =
        httpc:request(
            post,
            {Base ++ "/invite/org/AB2C3D4E", [], [], <<>>},
            [{autoredirect, false}],
            [{body_format, binary}]
        ),
    ?assertEqual("GET, HEAD", proplists:get_value("allow", H405)),

    ok.

http_get(Url) ->
    httpc:request(
        get,
        {Url, []},
        [{autoredirect, false}, {timeout, 10000}],
        [{body_format, binary}]
    ).

ct(Headers) ->
    proplists:get_value("content-type", Headers).
