%%% @doc `cs_ai_decision` 的纯函数单测（铁律 4：零 mock）。
%%%
%%% 重点不是"逐条规则走一遍"，而是把**顺序即优先级**这一产品事实钉死：
%%% 规则表一旦被重排，下面几条跨规则断言必须变红。
-module(cs_ai_decision_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CFG, #{
    threshold => 0.6,
    max_unresolved_turns => 2,
    faq_min_score => 0.2
}).

base() ->
    #{
        intent => refund,
        confidence => 0.9,
        resolved => true,
        faq_score => 0.8,
        unresolved_turns => 0,
        explicit_human => false,
        negative => false,
        config => ?CFG
    }.

facts(Overrides) -> maps:merge(base(), Overrides).

%% ===================================================================
%% 规则 6：兜底 —— 有依据、有信心 ⇒ 自动回复
%% ===================================================================

confident_reply_test() ->
    D = cs_ai_decision:decide(facts(#{})),
    ?assertEqual(auto_reply, maps:get(action, D)),
    ?assertEqual(<<"confident_reply">>, maps:get(reason, D)).

intent_passthrough_test() ->
    ?assertEqual(tech, maps:get(intent, cs_ai_decision:decide(facts(#{intent => tech})))).

%% ===================================================================
%% 规则 1/2：显式要人工 / 负面情绪 —— 不看置信度
%% ===================================================================

explicit_human_beats_everything_test() ->
    %% 置信度拉满、FAQ 命中最优，客户说「转人工」仍必须转
    D = cs_ai_decision:decide(
        facts(#{
            explicit_human => true, negative => true, confidence => 1.0, faq_score => 1.0
        })
    ),
    ?assertEqual(handoff, maps:get(action, D)),
    ?assertEqual(<<"customer_requested_human">>, maps:get(reason, D)).

negative_beats_confidence_test() ->
    D = cs_ai_decision:decide(facts(#{negative => true, confidence => 1.0})),
    ?assertEqual(handoff, maps:get(action, D)),
    ?assertEqual(<<"negative_sentiment">>, maps:get(reason, D)).

negative_beats_unresolved_test() ->
    %% 负面情绪优先于「轮数超限」——合规风险的排序不能靠轮数
    D = cs_ai_decision:decide(facts(#{negative => true, unresolved_turns => 9})),
    ?assertEqual(<<"negative_sentiment">>, maps:get(reason, D)).

%% ===================================================================
%% 规则 3：连续未解决轮数
%% ===================================================================

unresolved_turns_threshold_test() ->
    D = cs_ai_decision:decide(facts(#{unresolved_turns => 2})),
    ?assertEqual(handoff, maps:get(action, D)),
    ?assertEqual(<<"unresolved_turns_exceeded">>, maps:get(reason, D)).

unresolved_below_threshold_test() ->
    D = cs_ai_decision:decide(facts(#{unresolved_turns => 1})),
    ?assertEqual(auto_reply, maps:get(action, D)).

unresolved_rule_disabled_when_zero_test() ->
    %% max_unresolved_turns = 0 ⇒ 关闭该规则，轮数再大也不据此升级
    Cfg = maps:put(max_unresolved_turns, 0, ?CFG),
    D = cs_ai_decision:decide(facts(#{unresolved_turns => 5, config => Cfg})),
    ?assertEqual(auto_reply, maps:get(action, D)).

unresolved_beats_low_confidence_test() ->
    %% 两条都会触发转人工时，原因取优先级更高的一条（后台统计看原因分布）
    D = cs_ai_decision:decide(facts(#{unresolved_turns => 3, confidence => 0.1})),
    ?assertEqual(<<"unresolved_turns_exceeded">>, maps:get(reason, D)).

%% ===================================================================
%% 规则 4：置信度
%% ===================================================================

low_confidence_handoff_test() ->
    D = cs_ai_decision:decide(facts(#{confidence => 0.59})),
    ?assertEqual(handoff, maps:get(action, D)),
    ?assertEqual(<<"low_confidence">>, maps:get(reason, D)).

confidence_at_threshold_replies_test() ->
    %% 边界：等于阈值不放行转人工（>= 阈值即放行），避免阈值含义漂移
    D = cs_ai_decision:decide(facts(#{confidence => 0.6})),
    ?assertEqual(auto_reply, maps:get(action, D)).

%% ===================================================================
%% 规则 5：无依据
%% ===================================================================

no_grounding_when_unresolved_and_no_faq_test() ->
    D = cs_ai_decision:decide(facts(#{resolved => false, faq_score => 0.0})),
    ?assertEqual(no_match, maps:get(action, D)),
    ?assertEqual(<<"no_grounding">>, maps:get(reason, D)).

grounded_even_if_model_unsure_still_replies_test() ->
    %% 有 FAQ 依据 ⇒ 即使模型自述未解决也让 AI 先答（人不必代替知识库）
    D = cs_ai_decision:decide(facts(#{resolved => false, faq_score => 0.5})),
    ?assertEqual(auto_reply, maps:get(action, D)).

low_confidence_beats_no_grounding_test() ->
    D = cs_ai_decision:decide(facts(#{confidence => 0.1, resolved => false, faq_score => 0.0})),
    ?assertEqual(<<"low_confidence">>, maps:get(reason, D)).

faq_threshold_boundary_test() ->
    %% 等于 FAQ 下限视为「有依据」（与置信度同口径）
    D = cs_ai_decision:decide(facts(#{resolved => false, faq_score => 0.2})),
    ?assertEqual(auto_reply, maps:get(action, D)).

%% ===================================================================
%% 空缺省值：事实缺失时 fail-safe（不猜成"能自动答"）
%% ===================================================================

missing_facts_default_to_handoff_test() ->
    %% 只给 config：置信度 0、未解决、无 FAQ ⇒ 必须转人工，绝不能默认自动回复
    D = cs_ai_decision:decide(#{config => ?CFG}),
    ?assert(cs_ai_decision:is_handoff(D)),
    ?assertEqual(<<"low_confidence">>, maps:get(reason, D)).

non_map_facts_is_not_crash_free_test() ->
    %% 非 map 不是被支持入参（application 保证类型）；此处只锁定会显式失败而非静默
    ?assertError(function_clause, cs_ai_decision:decide(not_a_map)).

%% ===================================================================
%% 值域与不变量
%% ===================================================================

reason_codes_cover_all_emittable_reasons_test() ->
    Declared = cs_ai_decision:reason_codes(),
    Matrix = [
        facts(#{}),
        facts(#{explicit_human => true}),
        facts(#{negative => true}),
        facts(#{unresolved_turns => 5}),
        facts(#{confidence => 0.1}),
        facts(#{resolved => false, faq_score => 0.0}),
        #{config => ?CFG}
    ],
    Emitted = [maps:get(reason, cs_ai_decision:decide(F)) || F <- Matrix],
    lists:foreach(
        fun(R) -> ?assert(lists:member(R, Declared)) end,
        Emitted
    ),
    %% 决策路径确实把每一条都走到过（非真空：否则上面的 ⊂ 断言毫无约束力）
    lists:foreach(
        fun(R) -> ?assert(lists:member(R, Emitted)) end,
        [
            <<"confident_reply">>,
            <<"customer_requested_human">>,
            <<"negative_sentiment">>,
            <<"unresolved_turns_exceeded">>,
            <<"low_confidence">>,
            <<"no_grounding">>
        ]
    ).

is_handoff_test() ->
    ?assert(cs_ai_decision:is_handoff(handoff)),
    ?assert(cs_ai_decision:is_handoff(no_match)),
    ?assertNot(cs_ai_decision:is_handoff(auto_reply)),
    ?assert(cs_ai_decision:is_handoff(#{action => handoff})),
    ?assertNot(cs_ai_decision:is_handoff(#{action => auto_reply})).
