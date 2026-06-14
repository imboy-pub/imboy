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
        string:sub_string(binary_to_list(Path), 1, 21) == "/v1/payment/callback/",
    InOpenLi = IsPaymentCallback orelse lists:member(Path, OpenLi),
    InOptionLi = lists:member(Path, OptionLi),
    Switch = ec_cnv:to_binary(config_ds:env(api_auth_switch, <<"on">>)),
    Passport = string:sub_string(binary_to_list(Path), 1, 10),
    Res1 =
        if
            Path == <<"/v1/ws">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/v1/init">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Path == <<"/v1/refreshtoken">>, Switch == <<"on">> ->
                auth_ds:verify_sign(Req, Env);
            Passport == "/v1/passport/", Switch == <<"on">> ->
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
