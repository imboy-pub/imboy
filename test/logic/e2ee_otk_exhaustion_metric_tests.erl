%%% E2EE-062 残留 3 第一刀：**OTK 耗尽的可观测性**。
%%%
%%% == 缺口 ==
%%%
%%% 前七刀把幂等租约、目标级限流、batch 幂等、客户端补传都做了，但残留里始终有
%%% 一条：**「耗尽告警 / 运维指标缺失 —— 补传是客户端自愈，运维侧对耗尽攻击仍然盲」**
%%% （见 evidence/E2EE-062-client-refill-wiring.md §5.1 起）。
%%%
%%% 服务端**确实**知道每一次耗尽：`claim_with_identity` 在
%%% `olm_identity_ds:claim_one_time_key` 返回 `{error, exhausted}` 时回退到
%%% fallback prekey——那一刻就是前向保密降级的瞬间。但这个事实**没有被记录到任何
%%% 地方**：没有计数、没有日志、没有指标。运维只能等用户报障。
%%%
%%% 项目已有 `elib_metric` 计数器facility 并在生产使用
%%% （`message_ds:msg_sent_total`、`msg_ack_logic:msg_delivered_total`），
%%% 经 `/metrics` 以 Prometheus 格式导出。本刀把耗尽事件接上去。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. OTK 耗尽回退 fallback → 必须计一次 `olm_otk_exhausted_total`；
%%% 2. 连 fallback 都没有 → 必须计一次 `olm_prekey_unavailable_total`；
%%% 3. 【对照组】OTK 正常可领时**不得**计任何耗尽指标——
%%%    否则指标恒为噪音，告警起不到作用；
%%% 4. 【正向可用性】加了埋点**不得改变返回值**：耗尽时仍必须成功返回 fallback key。
%%%    一个「耗尽就报错」的实现在"能观测到耗尽"这个指标上也满分，必须被这条否掉；
%%% 5. 【安全】指标**不得携带 uid**。原因见文件末注释。
-module(e2ee_otk_exhaustion_metric_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TARGET, 4001).
-define(CLAIMER, 4002).
-define(DID, <<"dev-metric-A">>).

%% 记录 elib_metric 的全部调用，供断言「计了什么、带没带 uid」
-define(MK, {?MODULE, metrics}).

record_metric(Call) ->
    Prev =
        case persistent_term:get(?MK, undefined) of
            undefined -> [];
            L -> L
        end,
    persistent_term:put(?MK, Prev ++ [Call]).

metrics() ->
    case persistent_term:get(?MK, undefined) of
        undefined -> [];
        L -> L
    end.

names() ->
    [N || {N, _Args} <- metrics()].

setup(OtkResult, FbResult) ->
    persistent_term:erase(?MK),
    meck:new(olm_identity_ds, [passthrough, no_link]),
    meck:new(elib_metric, [passthrough, no_link]),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) ->
        {ok, #{<<"identity_key">> => <<"ik">>}}
    end),
    meck:expect(olm_identity_ds, claim_one_time_key, fun(_U, _D, _C) -> OtkResult end),
    meck:expect(olm_identity_ds, claim_one_time_key, fun(_U, _D, _C, _R) -> OtkResult end),
    meck:expect(olm_identity_ds, claim_fallback_key, fun(_U, _D) -> FbResult end),
    meck:expect(elib_metric, increment, fun(Name) ->
        record_metric({Name, []}),
        ok
    end),
    meck:expect(elib_metric, increment, fun(Name, Delta) ->
        record_metric({Name, [Delta]}),
        ok
    end),
    meck:expect(elib_metric, increment, fun(Name, Delta, Labels) ->
        record_metric({Name, [Delta, Labels]}),
        ok
    end),
    ok.

cleanup(_) ->
    _ = (catch meck:unload(elib_metric)),
    _ = (catch meck:unload(olm_identity_ds)),
    persistent_term:erase(?MK),
    ok.

%% ⚠️ instantiator 必须是 **1 元** fun。写成 0 元时 eunit 会把它当成一个普通
%% 测试直接执行——它只是返回一个列表，**内部断言一次都不会运行**，
%% 于是整个文件"全绿"却什么都没验。本文件初版即栽在这里，见 evidence §2.1。
with(OtkResult, FbResult, Body) ->
    {setup, fun() -> setup(OtkResult, FbResult) end, fun cleanup/1, fun(_) -> Body() end}.

otk_row() ->
    {ok, #{<<"key_id">> => <<"k1">>, <<"key_base64">> => <<"b1">>}}.

fb_row() ->
    {ok, #{<<"key_id">> => <<"fb1">>, <<"key_base64">> => <<"fbb1">>}}.

%% ===================================================================
%% 1. 对照组：OTK 正常可领 → 不得计任何耗尽指标
%% ===================================================================

healthy_claim_emits_no_exhaustion_metric_test_() ->
    with(otk_row(), fb_row(), fun() ->
        [
            {"OTK 可领时不得计耗尽指标", fun() ->
                {ok, P} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
                ?assertEqual(<<"one_time">>, maps:get(<<"type">>, P)),
                ?assertEqual(
                    [],
                    names(),
                    "正常路径打耗尽指标 = 指标恒为噪音，告警起不到作用"
                )
            end}
        ]
    end).

%% ===================================================================
%% 2. 耗尽回退 fallback → 计一次 olm_otk_exhausted_total
%% ===================================================================

exhausted_claim_emits_metric_test_() ->
    with({error, exhausted}, fb_row(), fun() ->
        [
            {"OTK 耗尽回退 fallback 必须被计数", fun() ->
                {ok, P} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
                %% 【正向可用性】埋点不得改变返回值：仍须成功拿到 fallback
                ?assertEqual(
                    <<"fallback">>,
                    maps:get(<<"type">>, P),
                    "「耗尽就报错」的实现在可观测性上也满分，必须被这条否掉"
                ),
                ?assert(
                    lists:member(olm_otk_exhausted_total, names()),
                    "耗尽即前向保密降级的瞬间；不计数则运维侧对耗尽攻击全盲"
                )
            end}
        ]
    end).

%% 带 request_id 的幂等路径（claim_keys/4）走的是另一个函数子句，同样必须计数
exhausted_claim_with_request_id_emits_metric_test_() ->
    with({error, exhausted}, fb_row(), fun() ->
        [
            {"幂等路径耗尽同样必须被计数", fun() ->
                {ok, P} = olm_identity_logic:claim_keys(
                    ?CLAIMER, ?TARGET, ?DID, <<"req-metric-1">>
                ),
                ?assertEqual(<<"fallback">>, maps:get(<<"type">>, P)),
                ?assert(lists:member(olm_otk_exhausted_total, names()))
            end}
        ]
    end).

%% ===================================================================
%% 3. 连 fallback 都没有 → 计 olm_prekey_unavailable_total
%% ===================================================================

no_prekey_emits_unavailable_metric_test_() ->
    with({error, exhausted}, {error, exhausted}, fun() ->
        [
            {"fallback 也缺失必须单独计数", fun() ->
                R = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
                ?assertEqual({error, <<"no_prekey_available">>}, R),
                Names = names(),
                ?assert(lists:member(olm_otk_exhausted_total, Names)),
                ?assert(
                    lists:member(olm_prekey_unavailable_total, Names),
                    "「池空但有 fallback」与「连 fallback 都没有」严重程度不同，"
                    "必须分开计数，否则告警无法分级"
                )
            end}
        ]
    end).

%% ===================================================================
%% 4. 安全：指标不得携带 uid
%% ===================================================================

%% Prometheus 指标会被抓取并长期留存。把 uid 放进标签有两个问题：
%%   1. 基数无上限（每个用户一条时间序列），会拖垮存储；
%%   2. **「谁的池快空了」正是耗尽攻击要的择时信号** —— 第五刀的
%%      prekey_count 端点为此刻意不接受 uid 入参
%%      （见 evidence/E2EE-062-prekey-count-endpoint.md §1.1）。
%%      指标端把同一信息漏出去，等于从后门把那道设计推翻。
%% 因此只计聚合量：运维能知道「耗尽正在以多高的速率发生」，足够告警；
%% 定位具体目标属事件响应，走别的途径。
metric_carries_no_uid_test_() ->
    with({error, exhausted}, fb_row(), fun() ->
        [
            {"耗尽指标不得携带 uid（基数 + 择时信号泄漏）", fun() ->
                {ok, _} = olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
                Flat = lists:flatten(io_lib:format("~p", [metrics()])),
                ?assertEqual(
                    nomatch,
                    string:find(Flat, integer_to_list(?TARGET)),
                    "指标里出现目标 uid = 把「谁的池快空了」从后门漏出去"
                ),
                ?assertEqual(
                    nomatch,
                    string:find(Flat, integer_to_list(?CLAIMER))
                )
            end}
        ]
    end).
