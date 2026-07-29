-module(payment_callback_logic_tests).

%%%===================================================================
%%% @doc payment_callback_logic 回调字段提取/业务类型推导单测
%%%
%%% 验证 biz_type 由订单号前缀推导、各网关字段名兼容(pick)、频道金额元→分。
%%% @end
%%%===================================================================

-include_lib("eunit/include/eunit.hrl").

%% biz_type 由订单号前缀推导：RCH=充值(1) / CH=频道(2)；显式值作兜底
biz_type_of_test() ->
    ?assertEqual(1, payment_callback_logic:biz_type_of(<<"RCH1781400000123">>, 0)),
    ?assertEqual(2, payment_callback_logic:biz_type_of(<<"CH1781400000456">>, 0)),
    %% 无法识别前缀时用回调显式 biz_type 兜底
    ?assertEqual(2, payment_callback_logic:biz_type_of(<<"UNKNOWN">>, 2)),
    ?assertEqual(0, payment_callback_logic:biz_type_of(<<"UNKNOWN">>, 0)),
    %% RCH 优先于 CH（不被 CH 子句误匹配）
    ?assertEqual(1, payment_callback_logic:biz_type_of(<<"RCH999">>, 0)).

%% pick：按候选键列表取第一个非空值（适配各网关字段名）
pick_test() ->
    %% 命中第二候选键（微信 transaction_id）
    ?assertEqual(
        <<"4200001">>,
        payment_callback_logic:pick(
            #{<<"transaction_id">> => <<"4200001">>},
            [<<"gateway_payment_no">>, <<"transaction_id">>, <<"trade_no">>]
        )
    ),
    %% 命中第一候选键（统一字段优先）
    ?assertEqual(
        <<"UNI">>,
        payment_callback_logic:pick(
            #{<<"gateway_payment_no">> => <<"UNI">>, <<"trade_no">> => <<"TN">>},
            [<<"gateway_payment_no">>, <<"trade_no">>]
        )
    ),
    %% 全部缺失返回空
    ?assertEqual(<<>>, payment_callback_logic:pick(#{}, [<<"a">>, <<"b">>])),
    %% 空值跳过继续找
    ?assertEqual(
        <<"X">>,
        payment_callback_logic:pick(
            #{<<"a">> => <<>>, <<"b">> => <<"X">>}, [<<"a">>, <<"b">>]
        )
    ).

%% 支付回调结果指标必须在唯一出入口产出（C0-OPS-01）。
%% deploy/prometheus/rules/imboy-alerts.yml 的 imboy.payment 告警组依赖这两个
%% 计数器；若未来重构把 increment 丢了，告警会变成永远不触发的死规则。
callback_result_metric_test_() ->
    {setup, fun metric_setup/0, fun metric_cleanup/1, [
        fun sign_failure_emits_both_counters/0
    ]}.

metric_setup() ->
    meck:new(elib_metric, [passthrough, non_strict]),
    meck:expect(elib_metric, increment, fun(_N, _D, _L) -> ok end),
    meck:new(payment_sign, [passthrough, non_strict]),
    meck:expect(payment_sign, verify, fun(_G, _R, _H) -> {error, bad_sign} end),
    ok.

metric_cleanup(_) ->
    catch meck:unload(payment_sign),
    catch meck:unload(elib_metric),
    ok.

sign_failure_emits_both_counters() ->
    ?assertEqual(
        {error, <<"验签失败"/utf8>>},
        payment_callback_logic:handle(<<"alipay">>, #{}, #{raw => <<>>, headers => #{}})
    ),
    Calls = [Args || {_Pid, {elib_metric, increment, Args}, _Ret} <- meck:history(elib_metric)],
    Names = [N || [N | _] <- Calls],
    %% 验签失败要同时计入「安全信号」与「总结果」两个维度
    ?assert(lists:member(payment_callback_sign_failed_total, Names)),
    ?assert(lists:member(payment_callback_total, Names)),
    %% 总结果维度必须带 result="error" 标签，否则告警的 result 过滤失效
    ?assert(
        lists:any(
            fun
                ([payment_callback_total, _D, #{result := <<"error">>}]) -> true;
                (_) -> false
            end,
            Calls
        )
    ).

%% 频道订单金额 元 → 分
yuan_to_fen_test() ->
    ?assertEqual(900, payment_callback_logic:yuan_to_fen(9)),
    ?assertEqual(990, payment_callback_logic:yuan_to_fen(<<"9.90">>)),
    ?assertEqual(10001, payment_callback_logic:yuan_to_fen(<<"100.01">>)),
    ?assertEqual(0, payment_callback_logic:yuan_to_fen(<<"bad">>)).
