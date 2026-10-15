%%% @doc 智能客服意图识别（铁律 4：纯函数，零 I/O、零隐式环境依赖）。
%%%
%%% 两层判定，本模块是**语义真源**（HTTP/DB/LLM 都不重复这些规则）：
%%%
%%%   * 关键词层：`classify_keywords/1` —— 无外部依赖的兜底（LLM 不可用/超时/答不
%%%     合规时仍在工作）。权重归一化到 0..1，纯函数可测。
%%%   * LLM 层：`parse_llm/1` 只负责**解析与校验**模型的结构化输出（不调模型）；
%%%     模型不给合规 JSON 即 `{error, _}`，绝不猜。
%%%   * 融合层：`merge/2` —— LLM 有效且置信度够即采信；否则回落关键词；两层一
%%%     致时给置信度加成（一致的独立证据应更可信）。
%%%
%%% 值域冻结：`refund | order | tech | other`。出现第五个意图必须改本模块 +
%%% 迁移的 CHECK 注释 + 管理后台文案，三处同源。
-module(cs_ai_intent).

-moduledoc "智能客服意图识别（纯函数）：值域、关键词兜底、LLM 输出解析与融合。".

-export([
    intents/0,
    is_intent/1,
    parse_intent_bin/1,
    keywords/1,
    human_request_keywords/0,
    negative_keywords/0,
    contains_any/2,
    classify_keywords/1,
    parse_llm/1,
    merge/2,
    clamp01/1
]).

%% 单意图关键词饱和点：命中权重累计到此值即算「非常确定」。
-define(SATURATION, 2.0).
%% LLM 置信度采信门槛（低于此值视为模型没把握，退回关键词层）。
-define(LLM_TRUST, 0.5).

-type intent() :: refund | order | tech | other.
-export_type([intent/0]).

%% ===================================================================
%% 值域
%% ===================================================================

-spec intents() -> [intent()].
intents() -> [refund, order, tech, other].

-spec is_intent(term()) -> boolean().
is_intent(I) -> lists:member(I, intents()).

%% @doc 外部（HTTP / DB）传入的意图标签 → 值域原子。
%% 管理面的 FAQ 写入与统计过滤都走这里，避免各处各写一份枚举比较。
-spec parse_intent_bin(term()) -> {ok, intent()} | error.
parse_intent_bin(V) -> normalize_intent(V).

%% 关键词表（权重 = 该词的区分度；「退款」比「钱」更能指向 refund）。
-spec keywords(intent()) -> [{binary(), number()}].
keywords(refund) ->
    [
        {<<"退款"/utf8>>, 1.0},
        {<<"退货"/utf8>>, 0.8},
        {<<"退钱"/utf8>>, 1.0},
        {<<"退单"/utf8>>, 0.8},
        {<<"取消订单"/utf8>>, 0.7},
        {<<"申请退"/utf8>>, 0.8},
        {<<"售后"/utf8>>, 0.6},
        {<<"拒收"/utf8>>, 0.6}
    ];
keywords(order) ->
    [
        {<<"订单"/utf8>>, 0.8},
        {<<"物流"/utf8>>, 0.9},
        {<<"快递"/utf8>>, 0.9},
        {<<"发货"/utf8>>, 0.8},
        {<<"收货"/utf8>>, 0.5},
        {<<"运单"/utf8>>, 0.9},
        {<<"什么时候到"/utf8>>, 0.7},
        {<<"查订单"/utf8>>, 0.9}
    ];
keywords(tech) ->
    [
        {<<"报错"/utf8>>, 1.0},
        {<<"登录不了"/utf8>>, 1.0},
        {<<"打不开"/utf8>>, 0.8},
        {<<"闪退"/utf8>>, 1.0},
        {<<"用不了"/utf8>>, 0.7},
        {<<"卡住"/utf8>>, 0.7},
        {<<"技术支持"/utf8>>, 0.9},
        {<<"故障"/utf8>>, 0.9},
        {<<"异常"/utf8>>, 0.6}
    ];
keywords(other) ->
    [].

%% 客户**显式**要人工（最高优先级转人工信号，不受置信度影响）。
-spec human_request_keywords() -> [binary()].
human_request_keywords() ->
    [
        <<"转人工"/utf8>>,
        <<"人工客服"/utf8>>,
        <<"找人工"/utf8>>,
        <<"真人"/utf8>>,
        <<"人工"/utf8>>,
        <<"客服人员"/utf8>>,
        <<"叫你们客服"/utf8>>
    ].

%% 负面情绪/高风险信号（同样直接转人工，不让模型继续自动答复）。
-spec negative_keywords() -> [binary()].
negative_keywords() ->
    [
        <<"投诉"/utf8>>,
        <<"举报"/utf8>>,
        <<"差评"/utf8>>,
        <<"骗子"/utf8>>,
        <<"欺诈"/utf8>>,
        <<"报警"/utf8>>,
        <<"315">>,
        <<"曝光"/utf8>>,
        <<"起诉"/utf8>>,
        <<"太差"/utf8>>
    ].

%% ===================================================================
%% 关键词层
%% ===================================================================

-spec contains_any(binary(), [binary()]) -> boolean().
contains_any(Text, Words) when is_binary(Text) ->
    lists:any(fun(W) -> binary:match(Text, W) =/= nomatch end, Words);
contains_any(_, _) ->
    false.

%% @doc 关键词打分：对全部意图逐词匹配，返回得分最高的意图与归一化置信度。
%% 命中权重累计后除以饱和常数并截断到 1.0 —— 单命中即 0.5 级、双命中即 1.0 级。
%% 无命中返回 `{other, 0.0}`（不是失败，是「关键词层无信息」）。
-spec classify_keywords(binary()) -> {intent(), float()}.
classify_keywords(Text) when is_binary(Text) ->
    Scored = [{I, kw_score(Text, keywords(I))} || I <- [refund, order, tech]],
    case lists:filter(fun({_I, S}) -> S > 0 end, Scored) of
        [] ->
            {other, 0.0};
        Hits ->
            {Best, BestScore} = lists:foldl(
                fun
                    ({I, S}, {_BI, BS}) when S > BS -> {I, S};
                    (_, Acc) -> Acc
                end,
                hd(Hits),
                tl(Hits)
            ),
            {Best, clamp01(BestScore / ?SATURATION)}
    end;
classify_keywords(_) ->
    {other, 0.0}.

kw_score(Text, Words) ->
    lists:foldl(
        fun({W, Weight}, Acc) ->
            case binary:match(Text, W) of
                nomatch -> Acc;
                _ -> Acc + Weight
            end
        end,
        0.0,
        Words
    ).

%% ===================================================================
%% LLM 层：只解析与校验，不调用模型
%% ===================================================================

%% @doc 解析模型返回的结构化意图判定。
%%
%% 容忍 markdown 代码围栏与前后噪声（模型常见输出形态），但**不容忍值域越界**：
%% intent 不在值域、confidence 非法、JSON 语法错，一律 `{error, _}`，由调用方
%% 回落关键词层——宁可退化也不采信不可验证的输出。
-spec parse_llm(binary()) ->
    {ok, #{intent := intent(), confidence := float(), resolved := boolean()}}
    | {error, term()}.
parse_llm(Raw) when is_binary(Raw) ->
    case jsone:try_decode(strip_fence(Raw)) of
        {ok, Map, _Rest} when is_map(Map) -> validate_llm(Map);
        {error, Reason} -> {error, {not_json, Reason}};
        _ -> {error, not_object}
    end;
parse_llm(_) ->
    {error, not_binary}.

validate_llm(Map) ->
    case intent_from(Map) of
        {ok, Intent} ->
            {ok, #{
                intent => Intent,
                confidence => clamp01(num(Map, [<<"confidence">>, <<"score">>], 0.0)),
                resolved => truthy(maps:get(<<"resolved">>, Map, false))
            }};
        {error, _} = Err ->
            Err
    end.

intent_from(Map) ->
    Raw = maps:get(<<"intent">>, Map, undefined),
    case normalize_intent(Raw) of
        {ok, I} -> {ok, I};
        error -> {error, {invalid_intent, Raw}}
    end.

normalize_intent(I) when is_atom(I) ->
    case is_intent(I) of
        true -> {ok, I};
        false -> error
    end;
normalize_intent(B) when is_binary(B) ->
    case string:lowercase(binary_to_list(B)) of
        "refund" -> {ok, refund};
        "order" -> {ok, order};
        "tech" -> {ok, tech};
        "technical" -> {ok, tech};
        "support" -> {ok, tech};
        "other" -> {ok, other};
        "unknown" -> {ok, other};
        _ -> error
    end;
normalize_intent(_) ->
    error.

%% 去掉 ```json ... ``` 围栏：取第一条 { 到最后一个 }。
strip_fence(Raw) ->
    case {binary:match(Raw, <<"{">>), binary:matches(Raw, <<"}">>)} of
        {{Start, _}, Matches} when Matches =/= [] ->
            {LastStart, _} = lists:last(Matches),
            binary:part(Raw, Start, LastStart - Start + 1);
        _ ->
            Raw
    end.

%% ===================================================================
%% 融合层
%% ===================================================================

%% @doc 两层证据融合。
%%
%%   * LLM 合规且置信度 ≥ 0.5 → 采信 LLM；若与关键词层同向，置信度取
%%     `max(LLM, 关键词)` 再抬 0.1 上限 1.0（独立证据一致 ⇒ 更可信）；
%%   * LLM 不可用/低置信 → 回落关键词层；
%%   * 两层都无信息 → `{other, 0.0}`。
-spec merge({intent(), float()}, {ok, map()} | {error, term()}) -> {intent(), float()}.
merge({KwIntent, KwScore}, {error, _Err}) ->
    fallback_keywords({KwIntent, KwScore});
merge({KwIntent, KwScore}, {ok, #{intent := LlmIntent, confidence := LlmConf}}) ->
    case LlmConf >= ?LLM_TRUST of
        true when LlmIntent =:= KwIntent, KwIntent =/= other ->
            {LlmIntent, clamp01(max(LlmConf, KwScore) + 0.1)};
        true ->
            {LlmIntent, clamp01(LlmConf)};
        false ->
            fallback_keywords({KwIntent, KwScore})
    end.

fallback_keywords({other, _}) -> {other, 0.0};
fallback_keywords({Intent, Score}) -> {Intent, clamp01(Score)}.

%% ===================================================================
%% 数值工具
%% ===================================================================

-spec clamp01(number()) -> float().
clamp01(V) when is_number(V), V < 0.0 -> 0.0;
clamp01(V) when is_number(V), V > 1.0 -> 1.0;
clamp01(V) when is_number(V) -> float(V);
clamp01(_) -> 0.0.

num(Map, Keys, Default) ->
    case first_defined([maps:get(K, Map, undefined) || K <- Keys]) of
        undefined -> Default;
        V -> to_float(V, Default)
    end.

first_defined([undefined | Rest]) -> first_defined(Rest);
first_defined([H | _]) -> H;
first_defined([]) -> undefined.

%% 数字字符串容忍（模型常把 confidence 写成 "0.8"）；非数字一律取默认，
%% **不猜**——猜出来的置信度会直接喂给转人工阈值，比没有更危险。
to_float(V, _Default) when is_number(V) -> float(V);
to_float(V, Default) when is_binary(V) ->
    case string:to_float(binary_to_list(V)) of
        {F, []} -> float(F);
        _ -> Default
    end;
to_float(_, Default) ->
    Default.

truthy(true) -> true;
truthy(<<"true">>) -> true;
truthy(1) -> true;
truthy(_) -> false.
