-module(lifecycle_middleware).
-behaviour(cowboy_middleware).
-export([execute/2]).

%% 2026-11-13（R2 review）：入口闸门只读 lifecycle env（原子判断），
%% 不做 degraded 聚合——聚合里的 limiter_ready()/group_event_subscription:ready()
%% 是 gen_server:call，每请求翻倍热点进程邮箱，拥塞时超时判 degraded 会把
%% 故障放大为全量 503（越忙越摘流）。degraded 聚合仅由 /readyz 探针侧承担。
-spec execute(cowboy_req:req(), map()) -> {ok, cowboy_req:req(), map()} | {stop, cowboy_req:req()}.
execute(Req, #{handler := Handler} = Env) when
    Handler =:= healthz_handler; Handler =:= metrics_handler
->
    {ok, Req, Env};
execute(Req, Env) ->
    case imboy_app:accepting_requests() of
        true ->
            {ok, Req, Env};
        false ->
            {stop,
                elib_response:error_with_status(
                    Req,
                    503,
                    <<"服务暂不可用，请稍后重试"/utf8>>,
                    503
                )}
    end.
