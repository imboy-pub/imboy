%% IMBOY_ERLANG_QUALITY P2-01/P2-02：合成性质/见证/坏例模块（仅机制自测用）。
%% 真实 12 项业务性质由 P2-03..P2-08 落地，不在此文件。
-module(imboy_quality_pbt_demo).
-export([
    prop_sort_idempotent/0,
    prop_always_false/0,
    prop_crash/0,
    prop_shrink_witness/0,
    prop_repair_before/0,
    prop_repair_after/0,
    witness_ok/0,
    witness_bad/0,
    witness_bad_with_input/0,
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

%% 可控非最小失败（REPLAY01）：失败域 = 非空且长度≤5 且元素≥2——任何
%% 失败输入的元素值恒可缩（向 0），长度>1 亦可缩，故 NumShrinks>0 确定，
%% 不依赖随机首例恰好最小。
prop_shrink_witness() ->
    ?FORALL(
        L,
        non_empty(list(integer(2, 100))),
        length(L) =< 5
    ).

%% 修复前（缺陷实现：off-by-one）。REPLAY03：修复前失败、修复后通过。
broken_double(X) -> 2 * X + 1.
fixed_double(X) -> 2 * X.

prop_repair_before() ->
    ?FORALL(X, integer(), broken_double(X) =:= 2 * X).

prop_repair_after() ->
    ?FORALL(X, integer(), fixed_double(X) =:= 2 * X).

%% 通过见证（合同：返回 ok）。
witness_ok() -> ok.

%% 失败见证（PBT05：random_started=0 拒绝）。
witness_bad() -> {error, demo_witness_failed}.

%% 带固定输入的失败见证（REPLAY02：保存 WitnessId 与原 input——
%% 输入内蕴于见证，由框架记录其失败返回中的原始输入值）。
witness_bad_with_input() ->
    {error, {demo_witness_failed, 42}}.

%% 未定义 MFA 目标（PBT04：undef 构造）。
undef_target() ->
    imboy_quality_pbt_demo:no_such_function().
