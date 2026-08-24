%%% E2EE-062：**限流 scope 的配置守护**。
%%%
%%% == 为什么需要它 ==
%%%
%%% 第二刀实证过一件事（`evidence/E2EE-062-per-target-throttle.md` §4.1）：
%%% `throttle:check/2` 遇到**未注册的 scope 返回原子 `rate_not_set`，不崩**。
%%% 生产代码 `olm_handler:scope_limited/2` 因此显式识别它并打 ERROR 日志——
%%% 但**仍然放行**（scope 缺失是配置错误而非攻击，拒掉全部 claim 会让 E2EE 建会话
%%% 不可用）。
%%%
%%% 净效果：**从 `config/sys.config` 里删掉一行，整道 OTK 限流就消失了**，
%%% 而唯一的信号是一条运行时 ERROR 日志——没有任何测试会红。
%%%
%%% E2EE-062 前八刀的抗耗尽工作全部建立在这两个 scope 之上。本文件把它们钉死。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. 【对照组】未注册 scope 确实返回 `rate_not_set`（这正是本守护存在的前提；
%%%    它红说明前提变了，整个守护的理由需要重估）；
%%% 2. `config/sys.config` 里必须同时存在 `olm_claim` 与 `olm_claim_target`，
%%%    且速率为预期值；
%%% 3. 【正向可用性 / 单位正确性】声明的数字确实变成**被强制执行**的次数——
%%%    第 N 次仍放行、第 N+1 次才拒。这条否掉「把 per_minute 写成 per_second」
%%%    之类的单位错误：那样 scope 仍在、日志也不报，但实际配额完全不是声明的那个。
-module(e2ee_throttle_scope_config_tests).

-include_lib("eunit/include/eunit.hrl").

%% 与 config/sys.config 中的声明一致；改动任一侧都必须同步改另一侧。
-define(EXPECTED_CLAIM, {olm_claim, 30, per_minute}).
-define(EXPECTED_CLAIM_TARGET, {olm_claim_target, 60, per_minute}).

ensure_throttle() ->
    {ok, _} = application:ensure_all_started(throttle),
    ok.

%% 优先读部署时实际使用的 config/sys.config；该文件不入仓时，读随发布走的
%% sys.config.example。测试环境的 sys.local.config 被 gitignore，不应作为守护对象。
shipped_rates() ->
    ConfigPath =
        case filelib:is_regular("config/sys.config") of
            true -> "config/sys.config";
            false -> "config/sys.config.example"
        end,
    {ok, [Config]} = file:consult(ConfigPath),
    Throttle = proplists:get_value(throttle, Config, []),
    proplists:get_value(rates, Throttle, []).

%% ===================================================================
%% 1. 对照组：未注册 scope 返回 rate_not_set（本守护存在的前提）
%% ===================================================================

unregistered_scope_returns_rate_not_set_test() ->
    ok = ensure_throttle(),
    ?assertEqual(
        rate_not_set,
        throttle:check(scope_that_was_never_registered_zzz, 1),
        "本守护的全部理由就是这个返回值——它一旦不再成立，"
        "「删掉一行配置就静默关掉限流」的风险模型需要重估"
    ).

%% ===================================================================
%% 2. 两个 OTK scope 必须在随发布的 sys.config 里，且速率为预期值
%% ===================================================================

otk_scopes_present_in_shipped_config_test() ->
    Rates = shipped_rates(),
    ?assert(
        lists:member(?EXPECTED_CLAIM, Rates),
        "olm_claim 从 sys.config 消失 → per-claimant 限流静默失效，"
        "只留一条 ERROR 日志"
    ),
    ?assert(
        lists:member(?EXPECTED_CLAIM_TARGET, Rates),
        "olm_claim_target 从 sys.config 消失 → 目标级抗耗尽静默失效；"
        "E2EE-062 第二刀整刀的效果归零"
    ).

%% 顺带钉住：这两个 scope 不得被声明成 per_second 之类（见下方单位用例的理由）
otk_scopes_use_per_minute_test() ->
    Rates = shipped_rates(),
    Periods = [P || {N, _R, P} <- Rates, N =:= olm_claim orelse N =:= olm_claim_target],
    ?assertEqual([per_minute, per_minute], lists:sort(Periods)).

%% ===================================================================
%% 3. 正向可用性 / 单位正确性：声明的数字确实被强制执行
%% ===================================================================

%% 一个「scope 在、但配额根本不是声明的那个」的配置（例如把 per_minute 误写成
%% per_second），既不会触发 rate_not_set、也不会有日志——上面两条全绿而实际无防护。
%% 这条用一个独立的小 scope 验证「第 N 次放行、第 N+1 次拒」。
declared_rate_is_actually_enforced_test() ->
    ok = ensure_throttle(),
    Scope = otk_config_probe_scope,
    Limit = 3,
    ok = throttle:setup(Scope, Limit, per_minute),
    Key = 4242,
    Results = [throttle:check(Scope, Key) || _ <- lists:seq(1, Limit)],
    ?assertEqual(
        [],
        [R || R <- Results, element(1, R) =/= ok],
        "配额内的调用必须全部放行——否则这是个「一律拒绝」的实现"
    ),
    ?assertMatch({limit_exceeded, _, _}, throttle:check(Scope, Key)).
