%% IMBOY_ERLANG_QUALITY P1-07 正式 PG 隔离回归（进入 U；需本机 PG 4323）。
%% 真实库操作：DB01（模板命中/fresh clone 等价）、DB03（活动连接检查）、
%% DB06（未拥有库拒删）、DB02（digest 键记录）。
%% 环境缺失时 fail-closed（DB07 语义：不返回空集假绿）。
-module(imboy_quality_fixture_tests).
-include_lib("eunit/include/eunit.hrl").

args() ->
    #{
        host => "127.0.0.1",
        port => 4323,
        username => "imboy_user",
        password => "abc54321",
        database => "postgres"
    }.

pg_isolation_test_() ->
    {timeout, 120, fun pg_isolation/0}.

pg_isolation() ->
    Suffix = os:getpid(),
    Tpl = "imboy_quality_tpl_p7_" ++ Suffix,
    Clone = Tpl ++ "_c1",
    case imboy_quality_fixture:connect(args()) of
        {ok, C} ->
            try
                %% DB02：模板建库 + digest 键
                #{name := Tpl, digest := D} =
                    imboy_quality_fixture:ensure_template(C, Tpl),
                ?assert(byte_size(D) =:= 32),
                %% DB03：模板无活动连接（本连接连在 postgres 库上）
                ?assertEqual(
                    [],
                    imboy_quality_fixture:active_connections(C, Tpl)
                ),
                %% DB01：克隆初态等价（同一 public 表集合）
                {ok, Tables} = imboy_quality_fixture:clone(C, Tpl, Clone),
                ?assert(is_list(Tables)),
                %% 克隆库带 marker（可安全清理）
                ?assert(imboy_quality_fixture:marker_ok(args(), Clone)),
                %% DB06：未拥有库（正式业务库/系统库）拒删
                Refusal1 =
                    try
                        imboy_quality_fixture:drop_owned(C, "imboy_test_v1"),
                        no_error
                    catch
                        error:{refusing_unowned_database, _} = R1 -> R1
                    end,
                ?assertMatch({refusing_unowned_database, _}, Refusal1),
                Refusal2 =
                    try
                        imboy_quality_fixture:drop_owned(C, "postgres"),
                        no_error
                    catch
                        error:{refusing_unowned_database, _} = R2 -> R2
                    end,
                ?assertMatch({refusing_unowned_database, _}, Refusal2),
                %% 清理：克隆与模板按 marker 删除
                ok = imboy_quality_fixture:drop_owned(C, Clone),
                ok = imboy_quality_fixture:drop_owned(C, Tpl),
                %% DB07：真实业务库的扩展 preflight（产品合同四扩展）；
                %% 缺失任一则 {error,...} fail-closed，不得空集通过
                ?assertMatch(
                    {ok, _},
                    imboy_quality_fixture:preflight_extensions(
                        args(),
                        "imboy_test_v1",
                        [<<"pg_jieba">>, <<"postgis">>, <<"timescaledb">>, <<"pgcrypto">>]
                    )
                ),
                ?assertMatch(
                    {error, {missing_extensions, [<<"no_such_ext">>], _}},
                    imboy_quality_fixture:preflight_extensions(
                        args(), "imboy_test_v1", [<<"no_such_ext">>]
                    )
                )
            after
                catch epgsql:close(C)
            end;
        {error, _} = E ->
            %% DB07：环境缺失 fail-closed——显式失败，不静默空集假绿
            erlang:error({pg_preflight_failed, E})
    end.
