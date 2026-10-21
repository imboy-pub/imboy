%%% @doc 自动回复 / 自动转人工的裁决规则（铁律 4：纯函数）。
%%%
%%% 本模块是「什么时候 AI 自己答、什么时候必须交给人」的**唯一语义真源**。
%%% application 只负责取事实（意图、置信度、FAQ 分、累计轮数、配置阈值）并把
%%% 结论落库；domain 不做任何 I/O，因此这里的每条规则都能零 mock 单测。
%%%
%%% 规则**有序、首个命中即裁决**（顺序即优先级，改顺序就是改产品行为）：
%%%
%%%   1. `explicit_human`      客户明确要人工（「转人工」）—— 任何置信度都不该
%%%                            拦着客户找人；
%%%   2. `negative_sentiment`  投诉/差评/举报等高风险信号 —— 让模型继续自动
%%%                            答复是火上浇油，且合规风险最高；
%%%   3. `unresolved_turns`    连续未解决轮数达阈值 —— 「答了但没解决」比
%%%                            「答不上」更需要人；
%%%   4. `low_confidence`      意图置信度低于阈值 —— 不知道客户在说什么；
%%%   5. `no_grounding`        模型自认未解决 **且** 无 FAQ 可依据 —— 无据作答；
%%%   6. 其余 → `auto_reply`。
%%%
%%% 「超出范围」而不是「转人工」的分支：`no_match`（有意图、有置信度，但知识库
%%% 没有对应条目且模型说未解决）—— 它同样触发转人工，但**原因不同**，后台统计
%%% 要能把「答不上」与「客户要找人」分开看。
-module(cs_ai_decision).

-moduledoc "自动回复 / 自动转人工裁决规则（纯函数）。".

-export([
    decide/1,
    auto_reply/0,
    handoff/0,
    no_match/0,
    reason_codes/0,
    is_handoff/1
]).

-type action() :: auto_reply | handoff | no_match.
-type decision() :: #{action := action(), reason := binary(), intent := term()}.
-export_type([action/0, decision/0]).

%% ===================================================================
%% 裁决
%% ===================================================================

%% @doc 裁决一次 AI 应答。
%%
%% Facts（全部由 application 取事实后传入，本函数不查任何东西）：
%%   * `intent`            —— `cs_ai_intent` 融合后的意图；
%%   * `confidence`        —— 0..1；
%%   * `resolved`          —— 模型自述是否已解决（无模型结论时为 `false`）；
%%   * `faq_score`         —— FAQ 检索首条融合分（0..1，无命中为 0.0）；
%%   * `unresolved_turns`  —— 本会话连续未解决轮数（含本轮之前）；
%%   * `explicit_human`    —— 客户文本含显式要人工的措辞；
%%   * `negative`          —— 客户文本含负面/高风险措辞；
%%   * `config`            —— 阈值：`threshold` / `max_unresolved_turns` /
%%                            `faq_min_score`。
-spec decide(map()) -> decision().
decide(Facts) when is_map(Facts) ->
    Intent = maps:get(intent, Facts, other),
    Confidence = num(Facts, confidence, 0.0),
    Resolved = maps:get(resolved, Facts, false) =:= true,
    FaqScore = num(Facts, faq_score, 0.0),
    Unresolved = int(Facts, unresolved_turns, 0),
    Config = maps:get(config, Facts, #{}),
    Threshold = num(Config, threshold, 0.6),
    MaxUnresolved = int(Config, max_unresolved_turns, 2),
    FaqMin = num(Config, faq_min_score, 0.2),
    case
        classify(
            maps:get(explicit_human, Facts, false),
            maps:get(negative, Facts, false),
            MaxUnresolved,
            Unresolved,
            Confidence,
            Threshold,
            Resolved,
            FaqScore,
            FaqMin
        )
    of
        {Action, Reason} ->
            #{action => Action, reason => Reason, intent => Intent};
        undefined ->
            #{action => auto_reply(), reason => <<"confident_reply">>, intent => Intent}
    end.

%% 有序规则表（首个命中即返回；顺序 = 优先级）。
classify(true, _, _, _, _, _, _, _, _) ->
    {handoff(), <<"customer_requested_human">>};
classify(_, true, _, _, _, _, _, _, _) ->
    {handoff(), <<"negative_sentiment">>};
classify(_, _, Max, Unresolved, _, _, _, _, _) when Max > 0, Unresolved >= Max ->
    {handoff(), <<"unresolved_turns_exceeded">>};
classify(_, _, _, _, Confidence, Threshold, _, _, _) when Confidence < Threshold ->
    {handoff(), <<"low_confidence">>};
classify(_, _, _, _, _, _, false, FaqScore, FaqMin) when FaqScore < FaqMin ->
    {no_match(), <<"no_grounding">>};
classify(_, _, _, _, _, _, _, _, _) ->
    undefined.

%% ===================================================================
%% 值域
%% ===================================================================

-spec auto_reply() -> action().
auto_reply() -> auto_reply.

-spec handoff() -> action().
handoff() -> handoff.

-spec no_match() -> action().
no_match() -> no_match.

%% @doc 全部原因码（管理后台按此枚举出统计维度；新增规则必须同步登记）。
-spec reason_codes() -> [binary()].
reason_codes() ->
    [
        <<"confident_reply">>,
        <<"customer_requested_human">>,
        <<"negative_sentiment">>,
        <<"unresolved_turns_exceeded">>,
        <<"low_confidence">>,
        <<"no_grounding">>,
        <<"ai_disabled">>,
        <<"ai_seat_unavailable">>,
        <<"llm_unavailable">>
    ].

%% @doc 该裁决是否意味着「会话应交给人」。
%% contract 用 map()（实现只消费 action 键）：dialyzer 对调用方
%% decide/1 输出的推断窄于 decision()，2026-10-15 拆分后推断路径
%% 变化暴露了原 contract 过窄（cs_ai_app:167 假阳性连锁）。
-spec is_handoff(map() | action()) -> boolean().
is_handoff(#{action := Action}) -> is_handoff(Action);
is_handoff(handoff) -> true;
is_handoff(no_match) -> true;
is_handoff(_) -> false.

%% ===================================================================
%% 数值工具
%% ===================================================================

num(Map, Key, Default) ->
    case maps:get(Key, Map, Default) of
        V when is_number(V) -> float(V);
        _ -> Default
    end.

int(Map, Key, Default) ->
    case maps:get(Key, Map, Default) of
        V when is_integer(V) -> V;
        _ -> Default
    end.
