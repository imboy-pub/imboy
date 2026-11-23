-module(elib_pg_named_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%%===================================================================
%%% @doc Q-03 v5 修复聚焦验证：elib_pg:query_named/2,3 命名 prepared 快路径。
%%%
%%% 合同锚点（v5 FINDINGS 数量级判断第 2 条）：
%%%   - 结果与 elib_pg:query/3 同形（SELECT 多行 map）；
%%%   - 同一连接同一 SQL 第二次调用命中按连接共享的缓存（无再 Parse；另一进程同样命中）；
%%%   - 参数编码正确（不同参数不同结果）；
%%%   - 服务端语句丢失（人工 DEALLOCATE 模拟 26000）后自愈重建；
%%%   - 策略切换=新 SQL 文本=新名字，两变体并存；
%%%   - 事务回滚后命名语句仍可复用（非事务性）；
%%%   - 事务作用域内不走命名语句（失败的 Parse/Bind 会毒化整个事务，25P02）。
%%% DB 不可用时按 TEST_WITH_CONN 惯例 skip。
%%%===================================================================

named_pg_test_() ->
    ?TEST_WITH_CONN(fun(Conn) -> run(Conn) end).

run(Conn) ->
    ok = ok_val(
        elib_pg:execute(
            Conn,
            <<"CREATE TABLE IF NOT EXISTS elib_pg_named_probe(id integer PRIMARY KEY, v text)">>,
            []
        )
    ),
    ok = ok_val(elib_pg:execute(Conn, <<"TRUNCATE elib_pg_named_probe">>, [])),
    ok = ok_val(
        elib_pg:execute(Conn, <<"INSERT INTO elib_pg_named_probe VALUES (1,'a'),(2,'b')">>, [])
    ),
    parity_and_cache(Conn),
    params_encoded(Conn),
    invalidation_selfheal(Conn),
    variant_switch(Conn),
    survives_rollback(Conn),
    cache_shared_across_processes(Conn),
    tx_scope_uses_anonymous_path(Conn),
    ok.

parity_and_cache(Conn) ->
    Sql = <<"SELECT id, v FROM elib_pg_named_probe WHERE id >= $1 ORDER BY id">>,
    Expected = elib_pg:query(Conn, Sql, [1]),
    First = elib_pg:query_named(Conn, Sql, [1]),
    ?assertEqual(
        {ok, [#{<<"id">> => 1, <<"v">> => <<"a">>}, #{<<"id">> => 2, <<"v">> => <<"b">>}]}, First
    ),
    ?assertEqual(Expected, First),
    %% 第二次调用必须命中 pdict 缓存（不再走 prepare_named 的 parse）。
    meck:new(epgsql, [unstick, passthrough, no_link]),
    meck:expect(epgsql, parse, fun(_, _, _, _) -> error(unexpected_parse_on_cache_hit) end),
    Second = elib_pg:query_named(Conn, Sql, [1]),
    meck:unload(epgsql),
    ?assertEqual(First, Second).

params_encoded(Conn) ->
    Sql = <<"SELECT id FROM elib_pg_named_probe WHERE id = $1">>,
    ?assertEqual({ok, [#{<<"id">> => 2}]}, elib_pg:query_named(Conn, Sql, [2])),
    ?assertEqual({ok, []}, elib_pg:query_named(Conn, Sql, [99])).

invalidation_selfheal(Conn) ->
    Sql = <<"SELECT count(*) AS n FROM elib_pg_named_probe">>,
    {ok, [#{<<"n">> := _}]} = elib_pg:query_named(Conn, Sql, []),
    #statement{name = Name} = named_cached(Conn, Sql),
    %% 服务端确有该命名语句（防“从未建名”的假阳性路径）。
    ?assertMatch(
        {ok, [#{<<"n">> := 1}]},
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n FROM pg_prepared_statements WHERE name = $1">>,
            [list_to_binary(Name)]
        )
    ),
    %% 模拟服务端语句丢失（连接换代/人工 DEALLOCATE）。DEALLOCATE 走简单协议
    %% squery（extended 协议 Parse 对 DEALLOCATE 的支持随 PG 版本/中继而异，
    %% 探针已证 squery 稳定；本测试焦点是 query_named 的 26000 自愈路径）。
    %% 名字必须加引号：PG 会把未加引号的 DEALLOCATE 名按标识符折叠成小写，
    %% 而 Parse 协议字段保留原大小写（binary:encode_hex 为大写）。
    ?assertMatch(
        {ok, [], []},
        epgsql:squery(Conn, <<"DEALLOCATE \"", (list_to_binary(Name))/binary, "\"">>)
    ),
    ?assertEqual({ok, [#{<<"n">> => 2}]}, elib_pg:query_named(Conn, Sql, [])),
    %% 自愈后缓存已重建。
    ?assertMatch(#statement{}, named_cached(Conn, Sql)).

variant_switch(Conn) ->
    %% 策略切换产生新 SQL 文本：两变体并存，各自缓存各自名字。
    S1 = <<"SELECT id FROM elib_pg_named_probe WHERE v = $1">>,
    S2 = <<"SELECT id FROM elib_pg_named_probe WHERE v = $1 ORDER BY id DESC">>,
    ?assertEqual({ok, [#{<<"id">> => 1}]}, elib_pg:query_named(Conn, S1, [<<"a">>])),
    ?assertEqual({ok, [#{<<"id">> => 2}]}, elib_pg:query_named(Conn, S2, [<<"b">>])),
    ?assertMatch(#statement{}, named_cached(Conn, S1)),
    ?assertMatch(#statement{}, named_cached(Conn, S2)),
    #statement{name = N1} = named_cached(Conn, S1),
    #statement{name = N2} = named_cached(Conn, S2),
    ?assertNotEqual(N1, N2).

ok_val({ok, _}) -> ok;
ok_val(Other) -> erlang:error({execute_failed, Other}).

survives_rollback(Conn) ->
    Sql = <<"SELECT 1 AS one">>,
    ?assertEqual({ok, [#{<<"one">> => 1}]}, elib_pg:query_named(Conn, Sql, [])),
    {ok, _} = elib_pg:execute(Conn, <<"BEGIN">>, []),
    {ok, _} = elib_pg:execute(Conn, <<"ROLLBACK">>, []),
    %% 命名语句非事务性：回滚后继续复用同一条。
    ?assertEqual({ok, [#{<<"one">> => 1}]}, elib_pg:query_named(Conn, Sql, [])),
    ?assertMatch(#statement{}, named_cached(Conn, Sql)).

%% 缓存按连接共享：另一进程（进程字典为空）不得再 Parse/Describe。
cache_shared_across_processes(Conn) ->
    Sql = <<"SELECT count(*) AS n FROM elib_pg_named_probe WHERE id >= $1">>,
    {ok, _} = elib_pg:query_named(Conn, Sql, [1]),
    ?assertMatch(#statement{}, named_cached(Conn, Sql)),
    meck:new(epgsql, [unstick, passthrough, no_link]),
    meck:expect(epgsql, parse, fun(_, _, _, _) -> error(unexpected_parse_from_another_process) end),
    meck:expect(epgsql, describe, fun(_, _, _) ->
        error(unexpected_describe_from_another_process)
    end),
    Parent = self(),
    _ = spawn(fun() -> Parent ! {other, elib_pg:query_named(Conn, Sql, [2])} end),
    Other =
        receive
            {other, Result} -> Result
        after 5000 -> error(no_reply_from_other_process)
        end,
    meck:unload(epgsql),
    ?assertEqual({ok, [#{<<"n">> => 1}]}, Other).

%% 事务作用域内走匿名往返：服务端已有同名语句而本进程缓存为空时，旧实现会在
%% 事务内再次 Parse（42P05）并把事务毒成 25P02（msg_safe_boundary_pg_tests 的
%% BE-05 两例即由此失败）。
tx_scope_uses_anonymous_path(Conn) ->
    Sql = <<"SELECT id, v FROM elib_pg_named_probe WHERE id >= $1 ORDER BY id">>,
    Expected = {ok, [#{<<"id">> => 1, <<"v">> => <<"a">>}, #{<<"id">> => 2, <<"v">> => <<"b">>}]},
    ?assertEqual(Expected, elib_pg:query_named(Conn, Sql, [1])),
    ?assertMatch(#statement{}, named_cached(Conn, Sql)),
    #statement{name = Name} = named_cached(Conn, Sql),
    %% 模拟“语句已在、缓存不在”，并确认服务端语句确实存在。
    true = ets:delete(elib_pg_named_stmt, {Conn, Sql}),
    ?assertEqual(undefined, named_cached(Conn, Sql)),
    ?assertMatch(
        {ok, [#{<<"n">> := 1}]},
        elib_pg:query(
            Conn,
            <<"SELECT count(*) AS n FROM pg_prepared_statements WHERE name = $1">>,
            [list_to_binary(Name)]
        )
    ),
    {ok, _} = elib_pg:execute(Conn, <<"BEGIN">>, []),
    %% 与 transaction_with_actions/4 相同的事务作用域标记。
    put(elib_pg_commit_actions, {Conn, #{}}),
    try
        ?assertEqual(Expected, elib_pg:query_named(Conn, Sql, [1])),
        %% 同一事务内的后续语句仍可执行：没有被失败的 Parse 毒成 25P02。
        ?assertEqual({ok, [#{<<"one">> => 1}]}, elib_pg:query(Conn, <<"SELECT 1 AS one">>, []))
    after
        erase(elib_pg_commit_actions),
        {ok, _} = elib_pg:execute(Conn, <<"COMMIT">>, [])
    end,
    %% 事务内不建缓存条目。
    ?assertEqual(undefined, named_cached(Conn, Sql)).

named_cached(Conn, Sql) ->
    case ets:lookup(elib_pg_named_stmt, {Conn, Sql}) of
        [{_, #statement{} = S}] -> S;
        [] -> undefined
    end.
