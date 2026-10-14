%%% @doc P0-5（v4 02-D.5）：中文 OR 检索 + workspace 约束 focused 套件（真库）。
%%%
%%% 覆盖 D-4（多词 OR 命中，AND 对照零）+ D-7（跨 workspace 命中 0）+
%%% status/有效期门（published 才可被检索；paused/expired 排除）。
%%%
%%% 隔离：cs_pg_test_fixture 随机 TSID scope；环境不可用 ⇒ erlang:error。
-module(cs_ai_retrieval_workspace_tests).

-include_lib("eunit/include/eunit.hrl").

-define(FIX, cs_pg_test_fixture).

cs_ai_retrieval_workspace_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun cases/1}.

setup() ->
    case eunit_runner:eunit_setup_with_db() of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

cleanup({ok, Conn}) ->
    eunit_runner:eunit_cleanup_db(Conn);
cleanup(Other) ->
    Other.

cases({ok, _Conn}) ->
    [
        {timeout, 60, fun d4_multiterm_or_hits/0},
        {timeout, 60, fun d7_workspace_constraint/0},
        {timeout, 60, fun status_and_validity_gate/0},
        {timeout, 60, fun d6_false_confidence_buckets_structure/0}
    ];
cases({error, Reason}) ->
    erlang:error({csai_d_retrieval_suite_db_unavailable, Reason}).

%% ===================================================================
%% D-4：中文多词查询有命中（OR 组；AND 对照 = 0）
%% ===================================================================

d4_multiterm_or_hits() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        %% 多词元 FAQ：「我想退款」含「退款」词元。
        _ = seed_faq(Org, Ws, <<"我想退款怎么办"/utf8>>, 1),
        _ = seed_faq(Org, Ws, <<"发货时间说明"/utf8>>, 2),
        %% OR 组（cs_pg_ai 实现）：多词查询「退款 发票」命中含「退款」的 FAQ。
        {ok, OrHits} = cs_pg_ai:search_faq_lexical(Org, Ws, <<"退款 发票"/utf8>>, 10),
        ?assert(length(OrHits) >= 1),
        %% AND 对照（plainto_tsquery 语义）：多词 AND 在「发票」词元上零命中。
        AndZero =
            ?FIX:scalar(
                <<
                    "SELECT count(*) FROM customer_service_faq f"
                    " WHERE f.organization_id=$1"
                    "   AND f.search_tsv @@ plainto_tsquery('jiebacfg', '退款 发票')"/utf8
                >>,
                [Org]
            ),
        ?assertEqual(0, AndZero)
    after
        cleanup_ai(Scope)
    end.

%% ===================================================================
%% D-7：workspace 约束生效（跨 workspace 命中 0）
%% ===================================================================

d7_workspace_constraint() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        OtherWs = maps:get(other_workspace_id, Scope),
        %% scope 限定到本 ws 的 FAQ。
        _ = seed_faq_scoped(Org, Ws, [Ws], <<"退货流程"/utf8>>, 1),
        %% 本 ws 命中。
        {ok, Own} = cs_pg_ai:search_faq_lexical(Org, Ws, <<"退货"/utf8>>, 10),
        ?assert(length(Own) >= 1),
        %% 跨 ws（scope 不含）命中 0。
        {ok, Cross} = cs_pg_ai:search_faq_lexical(Org, OtherWs, <<"退货"/utf8>>, 10),
        ?assertEqual([], Cross),
        %% 空数组 scope = 全 workspace：另一条空 scope FAQ 对 OtherWs 可见。
        _ = seed_faq_scoped(Org, Ws, [], <<"发票抬头"/utf8>>, 2),
        {ok, Other} = cs_pg_ai:search_faq_lexical(Org, OtherWs, <<"发票"/utf8>>, 10),
        ?assert(length(Other) >= 1)
    after
        cleanup_ai(Scope)
    end.

%% ===================================================================
%% status / 有效期门
%% ===================================================================

status_and_validity_gate() ->
    Scope = ?FIX:new_scope(),
    try
        Org = maps:get(org_id, Scope),
        Ws = maps:get(workspace_id, Scope),
        Id = seed_faq(Org, Ws, <<"退款政策"/utf8>>, 1),
        %% published ⇒ 可检索。
        {ok, Hits0} = cs_pg_ai:search_faq_lexical(Org, Ws, <<"退款"/utf8>>, 10),
        ?assert(length(Hits0) >= 1),
        %% paused ⇒ 排除。
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_faq SET status='paused'"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, Id]
        ),
        {ok, Hits1} = cs_pg_ai:search_faq_lexical(Org, Ws, <<"退款"/utf8>>, 10),
        ?assertEqual([], Hits1),
        %% 恢复 published 但已过期 ⇒ 排除。
        ok = ?FIX:exec(
            <<
                "UPDATE customer_service_faq SET status='published',"
                " valid_from = now() - interval '10 days',"
                " valid_until = now() - interval '1 day'"
                " WHERE organization_id=$1 AND id=$2"
            >>,
            [Org, Id]
        ),
        {ok, Hits2} = cs_pg_ai:search_faq_lexical(Org, Ws, <<"退款"/utf8>>, 10),
        ?assertEqual([], Hits2)
    after
        cleanup_ai(Scope)
    end.

%% ===================================================================
%% D-6：false_confidence_rate 分桶输出结构（纯函数）
%% ===================================================================

d6_false_confidence_buckets_structure() ->
    Samples = [
        #{intent => <<"refund">>, faq_hit => true, confidence => 0.9, support => true},
        #{intent => <<"refund">>, faq_hit => true, confidence => 0.8, support => false},
        #{intent => <<"refund">>, faq_hit => true, confidence => 0.7, support => true},
        #{intent => <<"shipping">>, faq_hit => false, confidence => 0.6, support => false},
        %% 低于 MIN_SAMPLES=3 的桶。
        #{intent => <<"other">>, faq_hit => true, confidence => 0.5, support => false}
    ],
    Buckets = cs_ai_app:false_confidence_buckets(Samples, 3),
    %% D.4.2：n < MIN_SAMPLES 的桶**不输出**——refund/true(3) 之外的
    %% shipping/false(1) 与 other/true(1) 全部被滤掉，只出 1 个桶。
    ?assertEqual(1, length(Buckets)),
    [Refund] = Buckets,
    ?assertEqual(<<"refund">>, maps:get(intent, Refund)),
    ?assertEqual(3, maps:get(n, Refund)),
    ?assertEqual(true, maps:get(faq_hit, Refund)),
    ?assertEqual(2 / 3, maps:get(support_rate, Refund)).

%% ===================================================================
%% 辅助
%% ===================================================================

seed_faq(Org, Ws, Question, Seq) ->
    seed_faq_scoped(Org, Ws, [Ws], Question, Seq).

seed_faq_scoped(Org, Ws, ScopeWs, Question, Seq) ->
    Id = ?FIX:id() + Seq,
    ok = ?FIX:exec(
        <<
            "INSERT INTO customer_service_faq"
            " (organization_id, id, question, answer, status, scope_workspace_ids)"
            " VALUES ($1,$2,$3,'答案','published',$4::bigint[])"/utf8
        >>,
        [Org, Id, Question, ScopeWs]
    ),
    Id.

cleanup_ai(Scope) ->
    O = maps:get(org_id, Scope),
    lists:foreach(
        fun(Table) ->
            _ = ?FIX:exec(
                <<"DELETE FROM ", Table/binary, " WHERE organization_id=$1">>, [O]
            )
        end,
        [
            <<"customer_service_faq">>,
            <<"customer_service_ai_session">>,
            <<"customer_service_ai_job">>
        ]
    ),
    ?FIX:cleanup(Scope).
