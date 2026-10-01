%%% @doc EB-11 Foundation 独立 E2E 的 **Erlang 侧入口**（test-only）。
%%%
%%% `scripts/enterprise_business_e2e.sh` 调 `main/0`：
%%%   * 启动真 imboy app（`eunit_runner:eunit_setup_with_db/0`：真连接池 + 真迁移 +
%%%     真监听器，端口由 `HTTP_PORT` 环境变量给出 ⇒ **隔离端口**）；
%%%   * 记录真路由表（`imboy_router:get_routes/0`），在 `probe` 模式下只替换企业租户
%%%     路由的 `auth_facts` 装配键（F1/F2 的测试侧补偿，见 `eb_e2e_facts_probe`）；
%%%   * 依次跑 A01 → A02 → A03 → A04 → A06 → A05，逐条打印 `[ASSERT EB-11-Axx.n]`；
%%%   * 末尾**恢复生产路由装配**、best-effort 清场、打印汇总并给出退出码。
%%%
%%% 退出码：0 = 全部断言通过；1 = 有断言失败。`[FAIL]` 令牌只在整门失败时打印一次
%%% （`control/verify_evidence.py` 用 `^\s*\[FAIL\]` 判定 GREEN 里是否混进失败令牌）。
%%%
%%% **不得假绿**：本入口不做「失败即跳过」的降级；每条断言都落到日志里。
-module(eb_e2e_runner).

-export([main/0, run/0]).

-spec main() -> no_return().
main() ->
    halt(run()).

-spec run() -> non_neg_integer().
run() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    try
        run_stub()
    after
        eb_pg_test_fixture:restore_asset_store(Previous)
    end.

run_stub() ->
    eb_e2e_lib:reset(),
    io:format("== EB-11 Foundation 独立 E2E（两 Org + 隔离端口 + 私有测试对象前缀）==~n"),
    io:format(
        "-- 口径: LOCAL_FOUNDATION_PASS 候选；对象存储 = 本地替身 "
        "(adapter_contract_verified_locally; real_garage_acceptance=NOT_RUN) --~n"
    ),
    io:format("-- run_token: ~s~n", [eb_e2e_lib:run_token()]),
    case eunit_runner:eunit_setup_with_db() of
        {ok, _Conn} ->
            ok;
        {error, Reason} ->
            io:format("[E2E-FATAL] app/PG 启动失败: ~p~n", [Reason]),
            halt(2)
    end,
    Port = list_to_integer(os:getenv("HTTP_PORT")),
    eb_e2e_lib:set_port(Port),
    ok = eb_e2e_lib:install_dispatch(),
    io:format("-- 隔离端口: ~p（监听器 imboy_listener；路由表 = 生产过程表）~n", [Port]),
    Scope = eb_e2e_fixture:seed(),
    log_scope(Scope),
    Ctx0 = #{scope => Scope, canaries => []},
    Try = fun(Mod, Ctx) ->
        try Mod:run(Ctx) of
            CtxNext when is_map(CtxNext) -> CtxNext;
            _NotCtx -> Ctx
        catch
            Class:CrashReason:Stack ->
                eb_e2e_lib:assert(
                    <<"EB-11-scenario-internal">>,
                    io_lib:format("场景 ~p 崩溃: ~p:~p @ ~p", [Mod, Class, CrashReason, Stack]),
                    false
                ),
                Ctx
        end
    end,
    Ctx1 = Try(eb_e2e_a01, Ctx0),
    Ctx2 = Try(eb_e2e_a02, Ctx1),
    Ctx3 = Try(eb_e2e_a03, Ctx2),
    Ctx4 = Try(eb_e2e_a04, Ctx3),
    Ctx5 = Try(eb_e2e_a06, Ctx4),
    _Ctx6 = Try(eb_e2e_a05, Ctx5),
    %% 恢复生产装配（不留「探针装配」在运行中的监听器上）
    ok = eb_e2e_lib:set_facts_mode(real),
    eb_e2e_lib:assert(
        <<"EB-11-harness.1">>,
        "末尾恢复生产 auth_facts 装配（listener dispatch 回到未改写路由表）",
        eb_e2e_lib:facts_mode() =:= real
    ),
    summary().

log_scope(Scope) ->
    io:format(
        "-- 合成作用域: org1=~p ws1=~p org2=~p ws2=~p owner1=~p A=~p B=~p X=~p Y=~p~n",
        [
            maps:get(org1, Scope),
            maps:get(ws1, Scope),
            maps:get(org2, Scope),
            maps:get(ws2, Scope),
            maps:get(owner1, Scope),
            maps:get(a_user, Scope),
            maps:get(b_user, Scope),
            maps:get(x_user, Scope),
            maps:get(y_user, Scope)
        ]
    ),
    ok.

summary() ->
    {Passed, Failed} = eb_e2e_lib:tally(),
    io:format("~n-- 汇总 --~n"),
    io:format("E2E: ASSERT_PASS=~p ASSERT_FAIL=~p~n", [Passed, Failed]),
    case Failed of
        0 ->
            io:format("[OK] EB-11 Foundation 独立 E2E 全绿（断言 ~p 条）~n", [Passed]),
            0;
        _ ->
            [
                io:format("[ASSERT-FAIL] ~s :: ~ts~n", [Id, Desc])
             || {Id, Desc} <- eb_e2e_lib:failures()
            ],
            io:format("[FAIL] EB-11 E2E: ~p 条断言失败~n", [Failed]),
            1
    end.
