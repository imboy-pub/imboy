%%% @doc FAQ 检索结果融合（铁律 4：纯函数）。
%%%
%%% 两路证据：
%%%   * **词法路** —— PG `ts_rank`（pg_jieba `jiebacfg` 中文分词 + GIN 索引）；
%%%   * **语义路** —— pgvector 余弦相似（仅在配置了 embedding provider 时非空）。
%%%
%%% 融合用 **RRF（Reciprocal Rank Fusion）**，而不是加权和：
%%%   * 两条路的分数**量纲不同**（`ts_rank` 是 0.0x 量级，余弦是 0..1），加权和
%%%     必须先做不可靠的尺度对齐；
%%%   * RRF 只吃**排名**，对量纲免疫，且天然处理「某一行只被一路召回」。
%%%
%%% 输出分数归一化到 0..1（除以理论最大 RRF 值），使 `faq_min_score` 这类阈值在
%%% 配置里有稳定含义——否则阈值会随路数/常量漂移。
-module(cs_ai_retrieval).

-moduledoc "FAQ 检索结果融合（纯函数）：归一化 + RRF + 稳定排序。".

-export([
    normalize/1,
    fuse/2,
    rank/1,
    top/2,
    rrf_constant/0
]).

%% RRF 标准常数（Cormack et al. 2009 的 k=60）：压制头部单一排名的支配力。
-define(RRF_K, 60).

-type scored() :: [{integer(), number()}].
-type fused() :: [{integer(), float()}].
-export_type([scored/0, fused/0]).

-spec rrf_constant() -> pos_integer().
rrf_constant() -> ?RRF_K.

%% ===================================================================
%% 归一化：把原始分数按「本次结果集最大值」相对归一化到 0..1
%% ===================================================================

%% @doc 相对归一化。空集或全零分数一律归零（不产生 NaN/除零）。
-spec normalize(scored()) -> fused().
normalize([]) ->
    [];
normalize(Pairs) ->
    Max = lists:max([S || {_, S} <- Pairs]),
    case Max =< 0 of
        true -> [{Id, 0.0} || {Id, _} <- Pairs];
        false -> [{Id, float(S) / float(Max)} || {Id, S} <- Pairs]
    end.

%% ===================================================================
%% RRF 融合
%% ===================================================================

%% @doc RRF 融合。
%%
%% 入参 `Channels`：`#{lexical => scored(), vector => scored()}`（键可缺省或为空）。
%% 每个通道内部先按分数降序、再按 id 升序定序（**确定性**：同分不因列表顺序抖动，
%% 否则同一问题两次检索可能给出不同首答）。
%%
%% 返回按融合分降序的 `[{FaqId, Score01}]`，Score01 已归一化到 0..1：
%% `Score01 = RRF / (ChannelCount / (K+1))`，即「每路都排第一」恰为 1.0。
-spec fuse(map(), map()) -> fused().
fuse(Channels, _Opts) when is_map(Channels) ->
    NonEmpty = [{N, S} || {N, S} <- maps:to_list(Channels), S =/= []],
    case NonEmpty of
        [] ->
            [];
        _ ->
            Ranks = lists:foldl(
                fun({_Name, Scored}, Acc) -> accumulate_ranks(Scored, Acc) end,
                #{},
                NonEmpty
            ),
            Max = length(NonEmpty) / (?RRF_K + 1),
            [
                {Id, clamp01(RRF / Max)}
             || {Id, RRF} <- maps:to_list(Ranks)
            ]
    end;
fuse(_, _) ->
    [].

%% 用「分数降序 + id 升序」的确定序把排名累加进 RRF 账本。
accumulate_ranks(Scored, Acc0) ->
    {_Next, Acc} =
        lists:foldl(
            fun(Id, {Rank, A}) ->
                Weight = 1 / (?RRF_K + Rank),
                {Rank + 1, maps:update_with(Id, fun(V) -> V + Weight end, Weight, A)}
            end,
            {1, Acc0},
            [Id || {Id, _S} <- rank(Scored)]
        ),
    Acc.

%% ===================================================================
%% 排序与截断
%% ===================================================================

%% @doc 稳定降序排序：分数相同时 id 小者在前（跨调用可复现）。
-spec rank([{integer(), number()}]) -> fused().
rank(Pairs) ->
    lists:sort(
        fun({IdA, SA}, {IdB, SB}) ->
            case SA == SB of
                true -> IdA < IdB;
                false -> SA > SB
            end
        end,
        [{Id, float(S)} || {Id, S} <- Pairs]
    ).

-spec top(fused(), non_neg_integer()) -> fused().
top(Ranked, Limit) when is_integer(Limit), Limit >= 0 ->
    lists:sublist(Ranked, Limit);
top(Ranked, _) ->
    Ranked.

clamp01(V) when is_number(V), V < 0.0 -> 0.0;
clamp01(V) when is_number(V), V > 1.0 -> 1.0;
clamp01(V) when is_number(V) -> float(V);
clamp01(_) -> 0.0.
