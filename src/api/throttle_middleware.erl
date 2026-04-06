-module(throttle_middleware).

-behaviour(cowboy_middleware).

-export([execute/2]).

-include("log.hrl").

%% @doc 限流中间件
%% 在 auth_middleware 之后、cowboy_handler 之前执行
%% 已认证请求基于 UID 限流，未认证请求基于 IP 限流
%%
%% @param Req Cowboy请求对象
%% @param Env 环境变量映射
%% @return 中间件执行结果
%% @end
-spec execute(cowboy_req:req(), map()) ->
                 {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    case is_whitelisted(Path) of
        true ->
            {ok, Req, Env};
        false ->
            do_throttle(Req, Env)
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc 执行限流检查
%% @private
-spec do_throttle(cowboy_req:req(), map()) ->
                     {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
do_throttle(Req, Env) ->
    case get_current_uid(Env) of
        0 ->
            %% 未认证请求，基于 IP 限流
            Ip = elib_req:get_client_ip(Req),
            case throttle:check(api_per_ip, Ip) of
                {ok, _Remaining, _RetryAfter} ->
                    {ok, Req, Env};
                {limit_exceeded, _, _} ->
                    ?WARN_LOG([rate_limited, #{key_type => ip, key => Ip, path => cowboy_req:path(Req)}]),
                    reply_429(Req)
            end;
        Uid ->
            %% 已认证请求，基于 UID 限流
            Key = integer_to_binary(Uid),
            case throttle:check(api_per_user, Key) of
                {ok, _Remaining, _RetryAfter} ->
                    {ok, Req, Env};
                {limit_exceeded, _, _} ->
                    ?WARN_LOG([rate_limited, #{key_type => uid, key => Uid, path => cowboy_req:path(Req)}]),
                    reply_429(Req)
            end
    end.

%% @doc 从 Env 中获取当前用户 ID
%% @private
-spec get_current_uid(map()) -> integer().
get_current_uid(Env) ->
    case maps:find(handler_opts, Env) of
        {ok, HandlerOpts} when is_map(HandlerOpts) ->
            maps:get(current_uid, HandlerOpts, 0);
        _ ->
            0
    end.

%% @doc 返回 HTTP 429 JSON 响应
%% @private
-spec reply_429(cowboy_req:req()) -> {stop, cowboy_req:req()}.
reply_429(Req) ->
    Body = jsone:encode(#{
        <<"code">> => 429,
        <<"msg">> => <<"rate_limit_exceeded">>
    }),
    Req1 = cowboy_req:reply(
        429,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        Body,
        Req
    ),
    {stop, Req1}.

%% @doc 判断路径是否在白名单中（跳过限流）
%% @private
-spec is_whitelisted(binary()) -> boolean().
is_whitelisted(<<"/v1/passport/", _/binary>>) -> true;
is_whitelisted(<<"/v1/init">>) -> true;
is_whitelisted(<<"/v1/ws">>) -> true;
is_whitelisted(<<"/health">>) -> true;
is_whitelisted(<<"/healthz">>) -> true;
is_whitelisted(<<"/static/", _/binary>>) -> true;
is_whitelisted(<<"/webrtc/", _/binary>>) -> true;
is_whitelisted(_) -> false.
