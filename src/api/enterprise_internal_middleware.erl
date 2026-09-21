-module(enterprise_internal_middleware).

%%%
% enterprise_internal_middleware 是 /api/internal/v1/* 的 Cowboy 中间件壳
% （EPGZ-02）。决策逻辑全部在 enterprise_internal_auth:decide/4（可注入
% AuthFun 纯测试）；本模块只做两件事：
%   1. 从 cowboy 请求取 method/path/headers，注入池化认证闭包；
%   2. 把 decide 的结果映射为 Env 注入（成功）或错误信封应答（失败）。
%
% W4 接线（A0）：auth_middleware 对 <<"/api/internal/v1/", _/binary>> 前缀
% 委托本模块 execute/2（不得进入人类 JWT/签名门，也不得被 open() 放行）；
% is_internal_path/1 供前缀判定复用。
%
% 成功时认证产物写入 Env 两处（handler 侧任取其一）：
%   env.enterprise_internal_ctx  —— 中间件层
%   env.handler_opts.enterprise_internal —— 当 handler_opts 是 map 时合并
%%%

-behaviour(cowboy_middleware).

-export([execute/2, is_internal_path/1]).

-include("log.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%% @doc internal 面前缀判定（manifest allowed_prefix）。
-spec is_internal_path(binary()) -> boolean().
is_internal_path(<<"/api/internal/v1/", _/binary>>) ->
    true;
is_internal_path(_Path) ->
    false.

%% @doc Cowboy 中间件入口：认证链编排（见 enterprise_internal_auth:decide/4）。
%% 认证失败/任一前置失败 → 稳定错误信封 + {stop, Req}。
-spec execute(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Headers = cowboy_req:headers(Req),
    AuthFun = pool_auth_fun(Headers),
    case enterprise_internal_auth:decide(Method, Path, Headers, AuthFun) of
        {ok, Ctx} ->
            {ok, Req, inject_ctx(Env, Ctx)};
        {error, Code} ->
            {stop, enterprise_internal_error:reply(Req, Code)}
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

%% 池化认证闭包：parse + 单事务认证链（格式错误在此归一 invalid_credential）。
-spec pool_auth_fun(map()) -> fun(() -> {ok, map()} | {error, atom()}).
pool_auth_fun(Headers) ->
    fun() ->
        case
            enterprise_internal_auth:parse_bearer(
                maps:get(<<"authorization">>, Headers, undefined)
            )
        of
            {error, _Code} ->
                {error, invalid_credential};
            {ok, Prefix, Secret} ->
                enterprise_internal_auth:authenticate(Prefix, Secret)
        end
    end.

-spec inject_ctx(map(), map()) -> map().
inject_ctx(Env, Ctx) ->
    Env1 = Env#{enterprise_internal_ctx => Ctx},
    case maps:find(handler_opts, Env) of
        {ok, Opts} when is_map(Opts) ->
            Env1#{handler_opts := Opts#{enterprise_internal => Ctx}};
        _ ->
            Env1
    end.
