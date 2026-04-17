-module(auth_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("log.hrl").
-include("error_code.hrl").

%% @doc Cowboy中间件执行函数
%% 处理请求的认证和授权验证
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
%% @end
-spec execute(cowboy_req:req(), map()) ->
                 {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Path = auth_ds:remove_last_forward_slash(cowboy_req:path(Req)),
    case Path of
        <<"/static/", _Tail/binary>> ->
            {ok, Req, Env};
        <<"/static/admin/", _Tail/binary>> ->
            % Admin 静态资源直接放行
            {ok, Req, Env};
        <<"/adm/", _Tail/binary>> ->
            % Admin 路由委托给 adm_auth_middleware
            adm_auth_middleware:execute(Req, Env);
        <<"/v1/", _Tail/binary>> ->
            % API v1 路由委托给 auth_middleware_api_v1
            auth_middleware_api_v1:execute(Req, Env);
        <<"/webrtc/", _Tail/binary>> ->
            {ok, Req, Env};
        _ ->
            OpenLi = imboy_router:open(),
            OptionLi = imboy_router:option(),
            InOpenLi = lists:member(Path, OpenLi),
            InOptionLi = lists:member(Path, OptionLi),
            Switch =
                ec_cnv:to_binary(
                    config_ds:env(api_auth_switch, <<"off">>)),
            Res1 =
                if InOpenLi == false, Switch == <<"on">> ->
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
            end
    end.
