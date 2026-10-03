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
        passport ->
            % GAP-09: passport 路径使用专用宽松限流（5 req/min/IP），不再完全豁免
            do_throttle_passport(Req, Env);
        cs_widget ->
            % CSB-03: widget 接入面专用限流（访客无 UID，恒按 IP；bootstrap/
            % 消息/SSE 共享一只桶），不再混入通用 api_per_ip 桶
            do_throttle_cs_widget(Req, Env);
        false ->
            do_throttle(Req, Env)
    end.

%%%===================================================================
%%% 内部函数
%%%===================================================================

%% @doc passport 路径专用宽松限流（登录/注册接口）
%% 基于 IP，使用 passport_per_ip 规则（宽松于通用规则）
%% GAP-09: 修复原有完全豁免导致暴力破解无 HTTP 层防护的问题
%% @private
-spec do_throttle_passport(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
do_throttle_passport(Req, Env) ->
    Ip = elib_req:get_client_ip(Req),
    case throttle:check(passport_per_ip, Ip) of
        {ok, _Remaining, _RetryAfter} ->
            {ok, Req, Env};
        {limit_exceeded, _, _} ->
            ?WARN_LOG([
                passport_rate_limited, #{key_type => ip, key => Ip, path => cowboy_req:path(Req)}
            ]),
            reply_429(Req);
        rate_not_set ->
            %% passport_per_ip 规则未初始化时 fail-open（不阻断登录）
            %% 此时 login_attempt_ds 暴力破解保护仍有效
            ?WARN_LOG([
                throttle_rate_not_set, #{scope => passport_per_ip, path => cowboy_req:path(Req)}
            ]),
            {ok, Req, Env}
    end.

%% @doc widget 接入面专用限流（CSB-03）：cs_widget_per_ip 规则，按 IP。
%% 与 passport 同款纪律：规则未初始化时 fail-open 并告警（代码级缺省在
%% imboy_app:init_throttle_rates/0 注册，正常不会 rate_not_set）。
%% @private
-spec do_throttle_cs_widget(cowboy_req:req(), map()) ->
    {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
do_throttle_cs_widget(Req, Env) ->
    Ip = elib_req:get_client_ip(Req),
    case throttle:check(cs_widget_per_ip, Ip) of
        {ok, _Remaining, _RetryAfter} ->
            {ok, Req, Env};
        {limit_exceeded, _, _} ->
            ?WARN_LOG([
                rate_limited,
                #{
                    key_type => ip,
                    key => Ip,
                    scope => cs_widget_per_ip,
                    path => cowboy_req:path(Req)
                }
            ]),
            reply_429(Req);
        rate_not_set ->
            ?WARN_LOG([
                throttle_rate_not_set, #{scope => cs_widget_per_ip, path => cowboy_req:path(Req)}
            ]),
            {ok, Req, Env}
    end.

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
                    ?WARN_LOG([
                        rate_limited, #{key_type => ip, key => Ip, path => cowboy_req:path(Req)}
                    ]),
                    reply_429(Req);
                rate_not_set ->
                    %% 速率规则未初始化，放行并告警（fail-open）
                    ?WARN_LOG([
                        throttle_rate_not_set, #{scope => api_per_ip, path => cowboy_req:path(Req)}
                    ]),
                    {ok, Req, Env}
            end;
        Uid ->
            %% 已认证请求，基于 UID 限流：双桶串联——
            %% 秒级突发桶先挡固定窗口边界突刺，分钟桶控制总量；
            %% 任一超限即 429。单桶 120/min 对活跃 IM 客户端过紧
            %% （列表/历史/离线分页一轮就是十几个请求），2026-10 放宽。
            Key = integer_to_binary(Uid),
            case
                check_uid_scopes(
                    [{api_per_user_burst, Key}, {api_per_user, Key}],
                    cowboy_req:path(Req)
                )
            of
                ok ->
                    {ok, Req, Env};
                {limit_exceeded, Scope} ->
                    ?WARN_LOG([
                        rate_limited,
                        #{
                            key_type => uid,
                            key => Key,
                            scope => Scope,
                            uid => Uid,
                            path => cowboy_req:path(Req)
                        }
                    ]),
                    reply_429(Req)
            end
    end.

%% @doc 依次检查多只桶；全部通过返回 ok，任一超限返回 {limit_exceeded, Scope}。
%% 某桶规则未初始化时 fail-open（跳过该桶继续检查剩余桶）并告警。
%% @private
-spec check_uid_scopes([{atom(), binary()}], binary()) ->
    ok | {limit_exceeded, atom()}.
check_uid_scopes([], _Path) ->
    ok;
check_uid_scopes([{Scope, Key} | Rest], Path) ->
    case throttle:check(Scope, Key) of
        {ok, _Remaining, _RetryAfter} ->
            check_uid_scopes(Rest, Path);
        {limit_exceeded, _, _} ->
            {limit_exceeded, Scope};
        rate_not_set ->
            %% 速率规则未初始化，放行并告警（fail-open）
            ?WARN_LOG([
                throttle_rate_not_set,
                #{scope => Scope, uid => Key, path => Path}
            ]),
            check_uid_scopes(Rest, Path)
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
%% P2-8f: 统一走 elib_response:error_with_status/4，保证 envelope 完整
%% （code/msg/sv_ts/payload），并使用中文 msg 与全站其它错误响应保持一致
%% @private
-spec reply_429(cowboy_req:req()) -> {stop, cowboy_req:req()}.
reply_429(Req) ->
    Req1 = elib_response:error_with_status(
        Req,
        429,
        <<"请求过于频繁，请稍后重试"/utf8>>,
        429
    ),
    {stop, Req1}.

%% @doc 判断路径的限流策略
%% 返回 true（完全豁免）| passport（专用宽松限流）| cs_widget（widget 接入面
%% 专用限流）| false（通用限流）
%% GAP-09: /v1/passport/ 从完全豁免改为专用宽松限流
%% CSB-03: /v1/cs/widget/ 专用限流（访客无 UID，恒按 IP）
%% @private
-spec is_whitelisted(binary()) -> true | passport | cs_widget | false.
%% 2026-07-08：v0 裸 /api/* 业务路由已下架，只保留 /api/v1/* 形态。
is_whitelisted(<<"/api/v1/passport/", _/binary>>) -> passport;
is_whitelisted(<<"/api/v1/cs/widget/", _/binary>>) -> cs_widget;
is_whitelisted(<<"/api/v1/cs/widget">>) -> cs_widget;
%% CSD-BE-01（hosted-widget-contract S4）：/w/:public_widget_id 动态 frame HTML
%% 归 widget 接入面专用桶（访客无 UID，恒按 IP——与 bootstrap/消息/SSE 同一只
%% 桶，不混入通用 api_per_ip 桶）。/w 根段全站唯一（CS widget 专属命名空间）。
is_whitelisted(<<"/w/", _/binary>>) -> cs_widget;
is_whitelisted(<<"/api/v1/init">>) -> true;
is_whitelisted(<<"/api/v1/ws">>) -> true;
is_whitelisted(<<"/health">>) -> true;
is_whitelisted(<<"/healthz">>) -> true;
is_whitelisted(<<"/static/", _/binary>>) -> true;
is_whitelisted(<<"/webrtc/", _/binary>>) -> true;
is_whitelisted(_) -> false.
