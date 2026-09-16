%%% @doc 注入时钟的生产实现（`eb_clock_port` 的真实现）。
%%%
%%% 依据：铁律 4（domain 不得依赖隐式时间源）+ EB-03-A06（purge 必须使用注入时钟）。
%%%
%%% 这是**全链唯一**允许读系统时间的地方：application 层取一次 `now/0` 后，
%%% 时间作为参数逐层传入 domain 与 purge worker，使 `retain_until` 与 purge 资格
%%% 在固定时钟下可逐字复现。
-module(eb_system_clock).

-behaviour(eb_clock_port).

-export([now/0]).

%% @doc 当前时间（Unix 秒，UTC）。
-spec now() -> integer().
now() ->
    erlang:system_time(second).
