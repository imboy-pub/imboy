%% IMBOY_ERLANG_QUALITY P2-01：合成性质/见证/坏例模块（仅机制自测用）。
%% 真实 12 项业务性质由 P2-03..P2-08 落地，不在此文件。
-module(imboy_quality_pbt_demo).
-export([
    prop_sort_idempotent/0,
    prop_always_false/0,
    prop_crash/0,
    witness_ok/0,
    witness_bad/0,
    undef_target/0
]).

-include_lib("proper/include/proper.hrl").

%% 恒真性质：排序幂等（真实 PropEr 生成器，验证依赖真实加载与计数）。
prop_sort_idempotent() ->
    ?FORALL(
        L,
        list(integer()),
        lists:sort(L) =:= lists:sort(lists:sort(L))
    ).

%% 恒假性质：快速产生真实反例（缩减后输入非空，供 REPLAY 材料断言）。
prop_always_false() ->
    ?FORALL(
        X,
        integer(1, 9999),
        X < 0
    ).

%% 执行期崩溃性质：引擎路径必须非零/失败（PBT04）。
prop_crash() ->
    ?FORALL(
        X,
        integer(),
        erlang:error({demo_crash, X}) =:= never
    ).

%% 通过见证（合同：返回 ok）。
witness_ok() -> ok.

%% 失败见证（PBT05：random_started=0 拒绝）。
witness_bad() -> {error, demo_witness_failed}.

%% 未定义 MFA 目标（PBT04：undef 构造）。
undef_target() ->
    imboy_quality_pbt_demo:no_such_function().
