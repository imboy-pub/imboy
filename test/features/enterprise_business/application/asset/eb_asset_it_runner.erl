%%% @doc EB-07 企业附件闭环集成门的 **Erlang 侧入口**（test-only）。
%%%
%%% `scripts/enterprise_business_asset_it.sh` 调 `main/0`：逐条跑 Acceptance 场景，
%%% 打印 `[ASSERT <ID>]` / `[ASSERT-FAIL <ID>]`，并在末尾给汇总与退出码。
%%%
%%% 退出码：0 = 全部通过；1 = 有断言失败。脚本据此决定整门红/绿。
%%%
%%% `[FAIL]` 逐字令牌**只在整门失败时**打印一次 —— `control/verify_evidence.py` 用
%%% `^\s*\[FAIL\]` 判定「GREEN 里是否混进失败」，故通过时不得出现该字符串。
-module(eb_asset_it_runner).

-export([main/0]).

-spec main() -> no_return().
main() ->
    _ = eunit_runner:eunit_setup_with_db(),
    io:format("== EB-07 企业附件闭环集成门（A01..A06；合成租户 + 本地替身对象存储）==~n"),
    io:format("-- 口径: adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN --~n"),
    Results = [run_one(Id) || Id <- eb_asset_it_scenarios:scenarios()],
    Passed = length([Id || {Id, ok, _N} <- Results]),
    Failed = length(Results) - Passed,
    io:format("~n-- 汇总 --~n"),
    lists:foreach(
        fun({Id, Outcome, N}) ->
            io:format("  ~s 子断言=~p 结果=~s~n", [Id, N, outcome(Outcome)])
        end,
        Results
    ),
    io:format("ASSET_IT: ASSERT_PASS=~p ASSERT_FAIL=~p~n", [Passed, Failed]),
    case Failed of
        0 ->
            io:format("[OK] EB-07 企业附件闭环集成门全绿~n"),
            halt(0);
        _ ->
            io:format("[FAIL] EB-07 企业附件集成门: ~p 个 Acceptance 有失败断言~n", [Failed]),
            halt(1)
    end.

run_one(Id) ->
    _ = eb_asset_it_scenarios:run(Id),
    {Passed, Failed, Failures} = eb_asset_it_scenarios:summary(),
    case Failed of
        0 ->
            io:format("[ASSERT ~s] 全部 ~p 条子断言通过（失败 0）~n", [Id, Passed]),
            {Id, ok, Passed};
        _ ->
            io:format(
                "[ASSERT-FAIL ~s] 子断言失败 ~p/~p；首条: ~ts~n",
                [Id, Failed, Passed + Failed, first_failure(Failures)]
            ),
            {Id, failed, Passed}
    end.

first_failure([]) ->
    <<"unknown">>;
first_failure([{SubId, Desc, RawReason} | _]) ->
    iolist_to_binary([SubId, " :: ", Desc, " :: ", eb_asset_it_lib:reason(RawReason)]).

outcome(ok) -> "PASS";
outcome(failed) -> "FAIL".
