-module(security_headers_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

%% @doc 安全响应头中间件
%% 为所有 HTTP 响应添加标准安全头，防范常见 Web 攻击
%%
%% 添加的安全头:
%% - X-Content-Type-Options: nosniff — 防止 MIME 类型嗅探
%% - X-Frame-Options: DENY — 防止点击劫持
%%   （BE-W01 例外：widget frame HTML 路径豁免——该面由 handler 按
%%    installation allowed_origins 出精确 frame-ancestors CSP，XFO
%%    DENY/SAMEORIGIN 会让任何嵌入都失败；其余安全头照常添加）
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
        <<"x-content-type-options">>, <<"nosniff">>, Req0
    ),
    Req2 =
        case is_widget_frame_path(cowboy_req:path(Req0)) of
            true ->
                %% frame HTML 面：CSP 由 handler 精确出，不叠加 XFO（叠加即全拒）。
                Req1;
            false ->
                cowboy_req:set_resp_header(
                    <<"x-frame-options">>, <<"DENY">>, Req1
                )
        end,
    Req3 = cowboy_req:set_resp_header(
        <<"referrer-policy">>, <<"strict-origin-when-cross-origin">>, Req2
    ),
    Req4 = cowboy_req:set_resp_header(
        <<"cache-control">>, <<"no-store">>, Req3
    ),
    Req5 = cowboy_req:set_resp_header(
        <<"permissions-policy">>, <<"camera=(), microphone=(), geolocation=()">>, Req4
    ),

    %% HSTS 仅在 TLS 启动模式下添加
    Req6 =
        case config_ds:env(start_mode, http) of
            https ->
                cowboy_req:set_resp_header(
                    <<"strict-transport-security">>,
                    <<"max-age=31536000; includeSubDomains">>,
                    Req5
                );
            _ ->
                Req5
        end,

    {ok, Req6, Env}.

%% frame HTML 路径段形状：[api, v1, cs, widget, frame, :installation_id]
%% （与 cors_middleware:widget_frame_path/1 同一冻结形状；路由注册由
%% BE-W01 wiring manifest 应用，判定按路径段独立成立）。
is_widget_frame_path(Path) when is_binary(Path) ->
    Segments = [S || S <- binary:split(Path, <<"/">>, [global]), S =/= <<>>],
    case Segments of
        [<<"api">>, <<"v1">>, <<"cs">>, <<"widget">>, <<"frame">>, _Id] -> true;
        _Other -> false
    end;
is_widget_frame_path(_Path) ->
    false.
