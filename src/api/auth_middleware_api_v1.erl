-module(auth_middleware_api_v1).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("log.hrl").
-include("error_code.hrl").

%% @doc Cowboy中间件执行函数
%% 处理 /v1 路由的认证和授权验证
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Path = auth_ds:remove_last_forward_slash(cowboy_req:path(Req)),

    OpenLi = imboy_router:open(),
    OptionLi = imboy_router:option(),
    %% 支付回调来自第三方服务器无 JWT，/v1/payment/callback/:gateway 整族免认证。
    %% :gateway 为变量段无法在 open/0 精确枚举，沿用 /v1/passport/ 的前缀放行范式，
    %% 并把前缀命中折叠进 InOpenLi —— 这样既跳过 verify_sign，又能让下游
    %% auth_ds:condition/5 以「开放路由」放行（否则 condition 仍会因无 token 而 stop）。
    IsPaymentCallback =
        string:sub_string(binary_to_list(Path), 1, 25) == "/api/v1/payment/callback/",
    %% 频道 incoming webhook 同款范式：token 即凭证（:token 变量段无法在 open/0
    %% 精确枚举），限流/token 校验在 channel_webhook_logic:incoming/2 完成。
    IsChannelWebhook =
        string:sub_string(binary_to_list(Path), 1, 24) == "/api/v1/webhook/channel/",
    InOpenLi = IsPaymentCallback orelse IsChannelWebhook orelse lists:member(Path, OpenLi),
    InOptionLi = lists:member(Path, OptionLi),
    Switch = ec_cnv:to_binary(config_ds:env(api_auth_switch, <<"on">>)),
    %% ws/init/refreshtoken/passport 是 JWT-open 但仍需设备签名校验的端点
    %% （open() 命中会让 InOpenLi=true，若不在此显式拦截会被直接放行，
    %% 丢失签名防篡改校验）。2026-07-08 v0 裸 /api/* 路由已下架，
    %% 只保留 /api/v1/* 形态。
    IsPassportPath =
        string:sub_string(binary_to_list(Path), 1, 17) == "/api/v1/passport/",
    Res1 =
        if
            Path == <<"/api/v1/ws">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/api/v1/init">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/api/v1/refreshtoken">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            IsPassportPath, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            InOpenLi == false, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            true ->
                {ok, Req, Env}
        end,
    case Res1 of
        {ok, Req, Env} ->
            Authorization = cowboy_req:header(<<"authorization">>, Req),
            auth_ds:condition(InOptionLi, InOpenLi, Authorization, Req, Env);
        Res2 ->
            Res2
    end.
