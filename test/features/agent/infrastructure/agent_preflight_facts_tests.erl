%% @doc D6：agent 域 DeletionPreflightFacts provider 的单元契约（meck）。
%%
%% 覆盖守卫/错误映射/SQL 形状/行映射四面；真库行为（bot 表 schema 真实
%% 列名/FK/status 过滤）见 agent_preflight_facts_pg_tests——两层合起来
%% 才是完整契约证据（单测 mock 抓不到字段缺口，真库抓不到错误分支）。
-module(agent_preflight_facts_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MOD, agent_preflight_facts_pg).

agent_preflight_facts_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        {"guard: non-positive/non-integer subject -> unavailable, zero DB contact",
            fun guard_rejects_bad_subject/0},
        {"error mapping: query failure -> unavailable (no local fallback)",
            fun query_error_maps_to_unavailable/0},
        {"row mapping: two owned active bots -> two frozen four-field blockers",
            fun rows_map_to_frozen_blockers/0},
        {"empty rows -> frozen five-key fact with zero blockers", fun empty_rows_zero_blockers/0},
        {"sql shape: owner_uid = $1 AND status = 1 (active-only, ownership keyed)",
            fun sql_filters_owner_and_active/0},
        {"fact_version/0 -> 1", fun fact_version_is_one/0}
    ]}.

setup() ->
    meck:new(elib_pg, [no_link]).

cleanup(_) ->
    meck:unload(elib_pg).

%% -------------------------------------------------------------------

guard_rejects_bad_subject() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {error, should_not_be_called} end),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(0)),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(-1)),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(<<"992010">>)),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(1.5)),
    %% 守卫先于任何 DB 接触
    ?assertEqual(0, meck:num_calls(elib_pg, query, 2)).

query_error_maps_to_unavailable() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {error, connection_closed} end),
    ?assertEqual({error, unavailable}, ?MOD:facts_agent(992010)).

rows_map_to_frozen_blockers() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) ->
        {ok, [
            #{<<"bot_user_id">> => 992011},
            #{<<"bot_user_id">> => 992012}
        ]}
    end),
    {ok, Fact} = ?MOD:facts_agent(992010),
    %% §1.6 冻结五键
    ?assertEqual(992010, maps:get(subject_user_id, Fact)),
    ?assertEqual(agent, maps:get(domain, Fact)),
    ?assert(is_integer(maps:get(observed_at, Fact))),
    ?assertEqual(1, maps:get(fact_version, Fact)),
    %% 冻结四字段 blocker（opaque + organization_id=null）
    ?assertEqual(
        [
            #{
                code => <<"AGENT_OWNER_ACTIVE">>,
                resource_type => <<"bot">>,
                resource_id => <<"992011">>,
                organization_id => null
            },
            #{
                code => <<"AGENT_OWNER_ACTIVE">>,
                resource_type => <<"bot">>,
                resource_id => <<"992012">>,
                organization_id => null
            }
        ],
        maps:get(blockers, Fact)
    ).

empty_rows_zero_blockers() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, []} end),
    {ok, Fact} = ?MOD:facts_agent(992030),
    ?assertEqual([], maps:get(blockers, Fact)),
    ?assertEqual(agent, maps:get(domain, Fact)),
    ?assertEqual(1, maps:get(fact_version, Fact)).

sql_filters_owner_and_active() ->
    meck:expect(elib_pg, query, fun(_Sql, _Args) -> {ok, []} end),
    _ = ?MOD:facts_agent(992010),
    %% history/1 形状经探针实证：{Pid, {M, F, [Args]}, Result}——本用例恰一次
    %% 调用，模式匹配直接取 SQL 与参数（capture/5 的 first 语义在多次
    %% expect 场景不可靠，弃用）
    [{_Pid, {elib_pg, query, [Sql, Args]}, {ok, []}}] = meck:history(elib_pg),
    SqlBin = iolist_to_binary(Sql),
    %% binary:match/2 成功返回 {Start, Length}（非 re:run 的 {match,_}）；
    %% nomatch 返回原子 nomatch，元组断言即区分
    ?assertMatch(
        {_, _},
        binary:match(SqlBin, <<"owner_uid = $1">>)
    ),
    ?assertMatch(
        {_, _},
        binary:match(SqlBin, <<"status = 1">>)
    ),
    %% 参数只携带 subject（只读单参查询）
    ?assertEqual([992010], Args).

fact_version_is_one() ->
    ?assertEqual(1, ?MOD:fact_version()).
