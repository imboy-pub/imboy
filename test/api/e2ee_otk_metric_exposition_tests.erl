%%% E2EE-062：**耗尽计数最终出现在 Prometheus 导出里**。
%%%
%%% == 缺口 ==
%%%
%%% 耗尽埋点那一刀（evidence/E2EE-062-otk-exhaustion-metric.md）把
%%% 「计数最终出现在 `/metrics` 输出」明确标为
%%% **「文件级阅读结论，未实证」**——埋点被调用已实证，但
%%% `elib_metric` → `metrics_handler` 这一段是照文件阅读认定的。
%%%
%%% 一个只增不导出的计数器等于没有计数器：运维那边永远是零。
%%%
%%% == 本文件守护 ==
%%%
%%% 1. 【对照组】`format_prometheus/1` 拿到什么就导出什么——
%%%    它红说明导出器本身坏了，后面的结论都不成立；
%%% 2. 【对照组】未被计数的指标**不得**出现在导出里——
%%%    否则"出现了"这件事毫无信息量（一个把所有已知名字都打印一遍的导出器
%%%    在该指标上恒得满分）；
%%% 3. **走生产耗尽路径**（`olm_identity_logic:claim_keys`，OTK 耗尽回退 fallback）
%%%    之后，`olm_otk_exhausted_total` 必须真的出现在导出文本里。
%%%
%%% ⚠️ 本文件**不 mock `elib_metric`**——那正是被测链路的一环。
%%% 用真的 gen_server。
-module(e2ee_otk_metric_exposition_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TARGET, 8001).
-define(CLAIMER, 8002).
-define(DID, <<"dev-metric-exp">>).

ensure_metric_server() ->
    case whereis(elib_metric) of
        undefined ->
            {ok, _} = elib_metric:start_link(),
            ok;
        _ ->
            ok
    end.

setup() ->
    ok = ensure_metric_server(),
    %% 只 mock DS 层制造"耗尽"，**不碰 elib_metric**
    meck:new(olm_identity_ds, [passthrough, no_link]),
    meck:expect(olm_identity_ds, find_identity, fun(_U, _D) ->
        {ok, #{<<"identity_key">> => <<"ik">>}}
    end),
    meck:expect(olm_identity_ds, claim_one_time_key, fun(_U, _D, _C) -> {error, exhausted} end),
    meck:expect(olm_identity_ds, claim_one_time_key, fun(_U, _D, _C, _R) -> {error, exhausted} end),
    meck:expect(olm_identity_ds, claim_fallback_key, fun(_U, _D) ->
        {ok, #{<<"key_id">> => <<"fb1">>, <<"key_base64">> => <<"b1">>}}
    end),
    ok.

cleanup(_) ->
    _ = (catch meck:unload(olm_identity_ds)),
    ok.

exposition_text() ->
    Metrics = elib_metric:get_all_metrics(),
    iolist_to_binary(metrics_handler:format_prometheus(Metrics)).

counter_value(Name) ->
    maps:get(Name, maps:get(counters, elib_metric:get_all_metrics(), #{}), 0).

contains(Text, Needle) ->
    binary:match(Text, Needle) =/= nomatch.

metric_exposition_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) ->
        [
            {"对照组：导出器把拿到的计数原样导出", fun formatter_emits_given_counter/0},
            {"对照组：未被计数的指标不得出现在导出里", fun unknown_counter_absent/0},
            {"生产耗尽路径的计数确实出现在 Prometheus 导出里", fun exhaustion_metric_exposed/0}
        ]
    end}.

%% 若这条红，说明导出器本身坏了（或我对它的调用方式不对），
%% 后面"耗尽计数出现了"的绿也说明不了任何事。
formatter_emits_given_counter() ->
    Text = iolist_to_binary(
        metrics_handler:format_prometheus(#{counters => #{probe_counter_xyz => 7}})
    ),
    ?assert(contains(Text, <<"probe_counter_xyz">>)),
    ?assert(contains(Text, <<"7">>)).

%% 一个"把所有已知名字都打印一遍"的导出器，在"指标出现了"这个断言上恒得满分。
%% 这条把它否掉。
unknown_counter_absent() ->
    Text = iolist_to_binary(
        metrics_handler:format_prometheus(#{counters => #{probe_counter_xyz => 1}})
    ),
    ?assertNot(
        contains(Text, <<"never_incremented_counter_qqq">>),
        "导出器若无中生有，'指标出现了'就没有信息量"
    ).

exhaustion_metric_exposed() ->
    %% 用**计数值**而非"文本里有没有出现"来断言递增：同一个 VM 里若别的路径
    %% 早已把该计数器加过，"出现了"会恒真，测不出任何东西。
    Before = counter_value(olm_otk_exhausted_total),
    %% 走生产入口：OTK 耗尽 → 回退 fallback → 该路径内埋点
    {ok, #{<<"type">> := <<"fallback">>}} =
        olm_identity_logic:claim_keys(?CLAIMER, ?TARGET, ?DID),
    ?assertEqual(
        Before + 1,
        counter_value(olm_otk_exhausted_total),
        "生产耗尽路径必须恰好计一次"
    ),
    ?assert(
        contains(exposition_text(), <<"olm_otk_exhausted_total">>),
        "埋点被调用已实证过；本条补的是它到 Prometheus 文本这一段——"
        "只增不导出的计数器等于没有计数器"
    ).
