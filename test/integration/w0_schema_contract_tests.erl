-module(w0_schema_contract_tests).
-include_lib("eunit/include/eunit.hrl").
-include("eunit_setup.hrl").

%%% 双体验 v2.5.2 WP8/T13 — W0 Schema Contract 断言套件（PG 集成测试，真库不可用自动 skip）
%%%
%%% 契约来源：Gate W = W0（Scope Contract），计划 §七 T13 VALIDATE：
%%%   * defer/reject 项以"无 schema"为通过条件：
%%%       - project_member        不存在（W0 禁止落表；Project 对 active workspace_member 可见）
%%%       - project_milestone     不存在（Milestones defer）
%%%       - project_channel_rel   不存在（关联 Channel defer）
%%%       - project_participant / resource_participant 不存在（禁多态万能表）
%%%       - project 无 links 列（Resources 聚合 defer）
%%%   * now 项 schema 就位：
%%%       - channel/"group" 有 scope 列且默认 'personal'、workspace_id 可空列
%%%       - scope XOR CHECK 约束存在（chk_channel_scope_xor / chk_group_scope_xor）
%%%       - 存量行全部回填 personal 且 workspace_id IS NULL
%%%
%%% 注意：本套件断言的是 schema 形态与数据回填形态，与具体测试运行顺序无关
%%% （WP3-WP7 的关系矩阵/scope 单测均为 mock 不落库；真库写侧集成测试自清理）。
%%% 若此断言在干净对账库失败，说明 defer 能力落表或存量回填被破坏，即为缺口。

-define(DEFERRED_TABLES, [
    <<"project_member">>,
    <<"project_milestone">>,
    <<"project_channel_rel">>,
    %% 禁多态万能表/万能参与关系（计划 T13 IMPLEMENT 原文）
    <<"project_participant">>,
    <<"resource_participant">>
]).

-define(SCOPE_TABLES, [<<"channel">>, <<"group">>]).

%%% ===================================================================
%%% defer/reject 项：相关表不存在
%%% ===================================================================

deferred_tables_absent_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        lists:foreach(
            fun(Tb) ->
                {ok, _, [{Exists}]} = epgsql:equery(
                    Conn,
                    <<"SELECT to_regclass('public.' || $1)::text IS NOT NULL">>,
                    [Tb]
                ),
                ?assertEqual(
                    false,
                    Exists,
                    io_lib:format(
                        "W0 Scope Contract 违约：表 ~s 不应存在（defer/reject 项必须有"
                        "无 schema）；若为刻意启用需先过 Gate W 换档",
                        [Tb]
                    )
                )
            end,
            ?DEFERRED_TABLES
        ),
        ok
    end).

%%% ===================================================================
%%% defer 项：project 无 links 列
%%% ===================================================================

project_links_column_absent_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        {ok, _, [{Cnt}]} = epgsql:equery(
            Conn,
            <<"SELECT count(*)::bigint FROM information_schema.columns",
                " WHERE table_schema = 'public' AND table_name = 'project'",
                "   AND column_name = 'links'">>,
            []
        ),
        ?assertEqual(0, Cnt, "W0 Scope Contract 违约：project.links 列不应存在（Resources 聚合 defer）"),
        ok
    end).

%%% ===================================================================
%%% now 项：scope/workspace_id 列就位 + 默认值 + XOR CHECK
%%% ===================================================================

scope_columns_and_xor_constraints_present_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        lists:foreach(
            fun(Tb) ->
                %% scope 列存在且默认 'personal'（NULL 以 '-' 哨兵表示，屏蔽驱动差异）
                {ok, _, Rows} = epgsql:equery(
                    Conn,
                    <<"SELECT column_name, COALESCE(column_default, '-') FROM information_schema.columns",
                        " WHERE table_schema = 'public' AND table_name = $1",
                        "   AND column_name IN ('scope','workspace_id') ORDER BY column_name">>,
                    [Tb]
                ),
                Cols = lists:sort([{C, D} || {C, D} <- Rows]),
                ?assertEqual(
                    [
                        {<<"scope">>, <<"'personal'::text">>},
                        {<<"workspace_id">>, <<"-">>}
                    ],
                    Cols,
                    io_lib:format(
                        "~s 必须有 scope 列（默认 'personal'）与可空 workspace_id 列",
                        [Tb]
                    )
                ),
                %% XOR CHECK 约束存在
                {ok, _, [{XorCnt}]} = epgsql:equery(
                    Conn,
                    <<"SELECT count(*)::bigint FROM pg_constraint",
                        " WHERE conrelid = to_regclass('public.' || $1)",
                        "   AND conname = 'chk_' || $1 || '_scope_xor'">>,
                    [Tb]
                ),
                ?assertEqual(
                    1,
                    XorCnt,
                    io_lib:format("~s 缺少 chk_~s_scope_xor 约束", [Tb, Tb])
                )
            end,
            ?SCOPE_TABLES
        ),
        ok
    end).

%%% ===================================================================
%%% now 项：存量行回填形态 = 全 personal 且 workspace_id IS NULL
%%% ===================================================================

legacy_rows_all_personal_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        lists:foreach(
            fun(Tb) ->
                Q = iolist_to_binary([
                    <<"SELECT count(*)::bigint FROM ">>,
                    quoted_id(Tb),
                    <<" WHERE scope IS DISTINCT FROM 'personal' OR workspace_id IS NOT NULL">>
                ]),
                {ok, _, [{Violations}]} = epgsql:equery(Conn, Q, []),
                ?assertEqual(
                    0,
                    Violations,
                    io_lib:format(
                        "~s 存量回填形态被破坏：存在非 personal 或带 workspace_id 的行"
                        "（I2 回填=零回填契约要求基线库全 personal）",
                        [Tb]
                    )
                )
            end,
            ?SCOPE_TABLES
        ),
        ok
    end).

%%% ===================================================================
%%% W0 now 项补强：project/task/event 三表确实存在（避免"删 defer 项误伤 now 项"回归）
%%% ===================================================================

w0_now_tables_present_test_() ->
    ?TEST_WITH_CONN(fun(Conn) ->
        lists:foreach(
            fun(Tb) ->
                {ok, _, [{Exists}]} = epgsql:equery(
                    Conn,
                    <<"SELECT to_regclass('public.' || $1)::text IS NOT NULL">>,
                    [Tb]
                ),
                ?assertEqual(
                    true,
                    Exists,
                    io_lib:format("W0 now 项缺表：~s 应存在（T3 迁移 78）", [Tb])
                )
            end,
            [
                <<"workspace">>,
                <<"workspace_member">>,
                <<"project">>,
                <<"project_task">>,
                <<"project_event">>
            ]
        ),
        ok
    end).

%%% ===================================================================
%%% Internal
%%% ===================================================================

%% 表名白名单已硬编码于常量，此处仅防注入习惯化处理
quoted_id(Tb) when is_binary(Tb) ->
    Q = <<34>>,
    <<Q/binary, Tb/binary, Q/binary>>;
quoted_id(Tb) ->
    erlang:error({bad_table_name, Tb}).
