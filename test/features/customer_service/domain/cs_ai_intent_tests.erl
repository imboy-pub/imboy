%%% @doc `cs_ai_intent` 的纯函数单测（铁律 4：domain 测试零 mock，不引入 meck）。
-module(cs_ai_intent_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 值域
%% ===================================================================

intents_value_set_test() ->
    ?assertEqual([refund, order, tech, other], cs_ai_intent:intents()),
    ?assert(cs_ai_intent:is_intent(refund)),
    ?assertNot(cs_ai_intent:is_intent(billing)).

%% ===================================================================
%% 关键词层
%% ===================================================================

classify_refund_test() ->
    %% 单命中：权重 1.0 / 饱和 2.0
    ?assertEqual({refund, 0.5}, cs_ai_intent:classify_keywords(<<"我要退款"/utf8>>)),
    %% 多命中叠加（「申请退」是「申请退款」的子串，两者都算）
    {Intent, Score} = cs_ai_intent:classify_keywords(<<"我要申请退款，怎么退货"/utf8>>),
    ?assertEqual(refund, Intent),
    ?assertEqual(1.0, Score).

classify_order_test() ->
    {Intent, Score} = cs_ai_intent:classify_keywords(<<"订单物流到哪了，运单号是什么"/utf8>>),
    ?assertEqual(order, Intent),
    ?assert(Score > 0.0).

classify_tech_test() ->
    {Intent, Score} = cs_ai_intent:classify_keywords(<<"App 一直闪退报错，打不开"/utf8>>),
    ?assertEqual(tech, Intent),
    ?assertEqual(1.0, Score).

classify_saturation_clamped_test() ->
    %% 饱和后不超过 1.0（多命中不许突破上限）
    {_Intent, Score} = cs_ai_intent:classify_keywords(
        <<"退款 退货 退钱 退单 取消订单 申请退 售后 拒收"/utf8>>
    ),
    ?assertEqual(1.0, Score).

classify_no_hit_is_other_test() ->
    %% 无命中不是失败，是「关键词层无信息」
    ?assertEqual({other, 0.0}, cs_ai_intent:classify_keywords(<<"你们家店几点开门"/utf8>>)).

classify_non_binary_is_other_test() ->
    ?assertEqual({other, 0.0}, cs_ai_intent:classify_keywords(undefined)).

%% ===================================================================
%% 人工/负面关键词
%% ===================================================================

human_request_detected_test() ->
    ?assert(cs_ai_intent:contains_any(<<"能不能转人工"/utf8>>, cs_ai_intent:human_request_keywords())),
    ?assertNot(cs_ai_intent:contains_any(<<"谢谢"/utf8>>, cs_ai_intent:human_request_keywords())).

negative_detected_test() ->
    ?assert(cs_ai_intent:contains_any(<<"我要投诉你们"/utf8>>, cs_ai_intent:negative_keywords())),
    ?assertNot(cs_ai_intent:contains_any(<<"很满意"/utf8>>, cs_ai_intent:negative_keywords())).

%% ===================================================================
%% LLM 输出解析（只解析，不调用模型）
%% ===================================================================

parse_llm_plain_json_test() ->
    {ok, R} = cs_ai_intent:parse_llm(
        <<"{\"intent\":\"refund\",\"confidence\":0.82,\"resolved\":true}">>
    ),
    ?assertEqual(refund, maps:get(intent, R)),
    ?assertEqual(0.82, maps:get(confidence, R)),
    ?assert(maps:get(resolved, R)).

parse_llm_fenced_json_test() ->
    %% 模型最常见的输出形态：markdown 围栏 + 前后噪声
    Raw = <<"好的，判定如下：\n```json\n{\"intent\":\"tech\",\"score\":0.7}\n```\n以上。"/utf8>>,
    {ok, R} = cs_ai_intent:parse_llm(Raw),
    ?assertEqual(tech, maps:get(intent, R)),
    ?assertEqual(0.7, maps:get(confidence, R)),
    ?assertNot(maps:get(resolved, R)).

parse_llm_confidence_clamped_test() ->
    {ok, R} = cs_ai_intent:parse_llm(<<"{\"intent\":\"order\",\"confidence\":1.4}">>),
    ?assertEqual(1.0, maps:get(confidence, R)).

parse_llm_rejects_out_of_range_intent_test() ->
    %% 值域越界绝不猜：宁可回落关键词层
    ?assertMatch(
        {error, {invalid_intent, _}}, cs_ai_intent:parse_llm(<<"{\"intent\":\"billing\"}">>)
    ),
    ?assertMatch(
        {error, {invalid_intent, _}}, cs_ai_intent:parse_llm(<<"{\"intent\":\"refundx\"}">>)
    ).

parse_llm_rejects_non_json_test() ->
    ?assertMatch({error, _}, cs_ai_intent:parse_llm(<<"我不知道你在说什么"/utf8>>)),
    ?assertMatch({error, _}, cs_ai_intent:parse_llm(<<"[1,2,3]">>)),
    ?assertMatch({error, _}, cs_ai_intent:parse_llm(undefined)).

parse_llm_alias_and_truthy_test() ->
    {ok, R} = cs_ai_intent:parse_llm(
        <<"{\"intent\":\"SUPPORT\",\"confidence\":\"0.9\",\"resolved\":\"true\"}">>
    ),
    ?assertEqual(tech, maps:get(intent, R)),
    %% 数字字符串容忍（模型常见形态）
    ?assertEqual(0.9, maps:get(confidence, R)),
    ?assert(maps:get(resolved, R)).

parse_llm_non_numeric_confidence_is_zero_test() ->
    %% 非数字不猜 —— 猜出来的置信度会直接喂给转人工阈值
    {ok, R} = cs_ai_intent:parse_llm(<<"{\"intent\":\"refund\",\"confidence\":\"high\"}">>),
    ?assertEqual(0.0, maps:get(confidence, R)).

%% ===================================================================
%% 融合层
%% ===================================================================

merge_trusts_confident_llm_test() ->
    %% LLM 与关键词同意 ⇒ 置信度加成（max + 0.1，上限 1.0）
    ?assertEqual(
        {refund, 0.95},
        cs_ai_intent:merge(
            {refund, 0.8}, {ok, #{intent => refund, confidence => 0.85, resolved => true}}
        )
    ),
    %% 加成不得突破 1.0
    ?assertEqual(
        {refund, 1.0},
        cs_ai_intent:merge(
            {refund, 0.9}, {ok, #{intent => refund, confidence => 0.95, resolved => true}}
        )
    ).

merge_llm_disagrees_uses_llm_test() ->
    %% LLM 置信度够但意图不同 ⇒ 采信 LLM（不加成）
    ?assertEqual(
        {order, 0.7},
        cs_ai_intent:merge(
            {refund, 0.4}, {ok, #{intent => order, confidence => 0.7, resolved => true}}
        )
    ).

merge_low_llm_confidence_falls_back_test() ->
    ?assertEqual(
        {refund, 0.5},
        cs_ai_intent:merge(
            {refund, 0.5}, {ok, #{intent => tech, confidence => 0.2, resolved => false}}
        )
    ).

merge_error_falls_back_test() ->
    ?assertEqual({refund, 0.5}, cs_ai_intent:merge({refund, 0.5}, {error, not_json})),
    ?assertEqual({other, 0.0}, cs_ai_intent:merge({other, 0.0}, {error, not_json})).

merge_llm_other_not_boosted_test() ->
    %% 「other == other」不是有意义的共识，不应加成（0.8 仍为 0.8）
    ?assertEqual(
        {other, 0.8},
        cs_ai_intent:merge(
            {other, 0.3}, {ok, #{intent => other, confidence => 0.8, resolved => false}}
        )
    ),
    %% LLM 置信度不足时回落关键词层
    ?assertEqual(
        {other, 0.0},
        cs_ai_intent:merge(
            {other, 0.3}, {ok, #{intent => other, confidence => 0.3, resolved => false}}
        )
    ).

clamp01_test() ->
    ?assertEqual(0.0, cs_ai_intent:clamp01(-1)),
    ?assertEqual(1.0, cs_ai_intent:clamp01(2)),
    ?assertEqual(0.5, cs_ai_intent:clamp01(0.5)),
    ?assertEqual(0.0, cs_ai_intent:clamp01(not_a_number)).
