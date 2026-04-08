-module(security_headers_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @doc 安全响应头中间件
%% 为所有 HTTP 响应添加标准安全头，防范常见 Web 攻击
%%
%% 添加的安全头:
%% - X-Content-Type-Options: nosniff — 防止 MIME 类型嗅探
%% - X-Frame-Options: DENY — 防止点击劫持
%% - Referrer-Policy: strict-origin-when-cross-origin — 控制 Referer 泄露
%% - Cache-Control: no-store — 防止敏感数据缓存
%% - Permissions-Policy: camera=(), microphone=(), geolocation=() — 限制浏览器 API
%%
%% 注意: Strict-Transport-Security (HSTS) 仅在 TLS 模式下添加
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return {ok, Req, Env} 始终放行请求
%% @end
-spec execute(cowboy_req:req(), map()) ->
                 {ok, cowboy_req:req(), map()}.
execute(Req0, Env) ->
    Req1 = cowboy_req:set_resp_header(
        <<"x-content-type-options">>, <<"nosniff">>, Req0),
    Req2 = cowboy_req:set_resp_header(
        <<"x-frame-options">>, <<"DENY">>, Req1),
    Req3 = cowboy_req:set_resp_header(
        <<"referrer-policy">>, <<"strict-origin-when-cross-origin">>, Req2),
    Req4 = cowboy_req:set_resp_header(
        <<"cache-control">>, <<"no-store">>, Req3),
    Req5 = cowboy_req:set_resp_header(
        <<"permissions-policy">>, <<"camera=(), microphone=(), geolocation=()">>, Req4),

    %% HSTS 仅在 TLS 启动模式下添加
    Req6 = case config_ds:env(start_mode, http) of
        https ->
            cowboy_req:set_resp_header(
                <<"strict-transport-security">>,
                <<"max-age=31536000; includeSubDomains">>, Req5);
        _ ->
            Req5
    end,

    {ok, Req6, Env}.
