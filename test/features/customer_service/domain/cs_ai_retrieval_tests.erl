%%% @doc `cs_ai_retrieval` 的纯函数单测（铁律 4：零 mock）。
-module(cs_ai_retrieval_tests).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% 归一化
%% ===================================================================

normalize_empty_test() ->
    ?assertEqual([], cs_ai_retrieval:normalize([])).

normalize_relative_to_max_test() ->
    %% 相对归一化：最大值 → 1.0，其余按比例（浮点用容差比较）
    [{1, Top}, {2, Half}] = cs_ai_retrieval:normalize([{1, 0.04}, {2, 0.02}]),
    ?assertEqual(1.0, Top),
    ?assert(abs(Half - 0.5) < 1.0e-9).

normalize_all_zero_no_nan_test() ->
    %% 全零不得产生 NaN/除零
    ?assertEqual([{1, 0.0}, {2, 0.0}], cs_ai_retrieval:normalize([{1, 0}, {2, 0}])).

%% ===================================================================
%% 排序确定性
%% ===================================================================

rank_desc_and_tie_break_by_id_test() ->
    %% 同分时 id 升序 —— 同一问题两次检索必须给出同一首答
    ?assertEqual(
        [{2, 0.9}, {1, 0.5}, {3, 0.5}],
        cs_ai_retrieval:rank([{3, 0.5}, {1, 0.5}, {2, 0.9}])
    ).

rank_idempotent_test() ->
    In = [{5, 0.1}, {4, 0.1}, {9, 0.7}],
    Once = cs_ai_retrieval:rank(In),
    ?assertEqual(Once, cs_ai_retrieval:rank(lists:reverse(In))),
    ?assertEqual(Once, cs_ai_retrieval:rank(Once)).

%% ===================================================================
%% RRF 融合
%% ===================================================================

fuse_empty_channels_test() ->
    ?assertEqual([], cs_ai_retrieval:fuse(#{}, #{})),
    ?assertEqual([], cs_ai_retrieval:fuse(#{lexical => []}, #{})).

fuse_single_channel_scores_one_at_top_test() ->
    %% 单通道时「排第一」必须恰好是 1.0 —— 阈值 faq_min_score 才有稳定含义
    Fused = cs_ai_retrieval:fuse(#{lexical => [{7, 0.4}, {8, 0.1}]}, #{}),
    ?assertEqual(2, length(Fused)),
    [{7, Top}, {8, Second}] = Fused,
    ?assertEqual(1.0, Top),
    %% 名次 2 的 RRF = (1/62)/(1/61)
    ?assert(abs(Second - 61 / 62) < 1.0e-12),
    ?assert(Second > 0.0 andalso Second < 1.0).

fuse_two_channels_agree_ranks_first_test() ->
    Fused = cs_ai_retrieval:fuse(
        #{lexical => [{1, 0.5}, {2, 0.2}], vector => [{1, 0.9}, {3, 0.8}]},
        #{}
    ),
    ?assertEqual(1, element(1, hd(Fused))),
    ?assertEqual(1.0, element(2, hd(Fused))),
    %% 融合分单调不增（已排序）
    Scores = [S || {_, S} <- Fused],
    ?assertEqual(Scores, lists:reverse(lists:sort(Scores))).

fuse_recalls_union_of_channels_test() ->
    %% 只被一路召回的条目也要出现在结果里（RRF 的价值所在）
    Fused = cs_ai_retrieval:fuse(
        #{lexical => [{1, 0.5}], vector => [{2, 0.9}]},
        #{}
    ),
    Ids = [Id || {Id, _} <- Fused],
    ?assertEqual([1, 2], lists:sort(Ids)).

fuse_scores_within_unit_interval_test() ->
    Fused = cs_ai_retrieval:fuse(
        #{lexical => [{1, 0.5}, {2, 0.4}, {3, 0.3}], vector => [{3, 0.9}, {1, 0.7}]},
        #{}
    ),
    ?assert(lists:all(fun({_, S}) -> S >= 0.0 andalso S =< 1.0 end, Fused)).

fuse_deterministic_under_input_permutation_test() ->
    A = cs_ai_retrieval:fuse(
        #{lexical => [{1, 0.5}, {2, 0.5}], vector => [{2, 0.4}, {1, 0.4}]}, #{}
    ),
    B = cs_ai_retrieval:fuse(
        #{lexical => [{2, 0.5}, {1, 0.5}], vector => [{1, 0.4}, {2, 0.4}]}, #{}
    ),
    ?assertEqual(A, B).

%% ===================================================================
%% 截断
%% ===================================================================

top_test() ->
    Ranked = [{1, 0.9}, {2, 0.5}, {3, 0.1}],
    ?assertEqual([{1, 0.9}, {2, 0.5}], cs_ai_retrieval:top(Ranked, 2)),
    ?assertEqual([], cs_ai_retrieval:top(Ranked, 0)),
    %% 超过长度不报错
    ?assertEqual(Ranked, cs_ai_retrieval:top(Ranked, 99)).

rrf_constant_test() ->
    ?assertEqual(60, cs_ai_retrieval:rrf_constant()).
