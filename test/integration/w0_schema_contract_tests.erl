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
%%%       - 迁移以 DEFAULT 'personal'、nullable workspace_id 保证存量行回填形态
%%%
%%% 注意：schema 形态由真库断言，迁移时存量回填由迁移 SQL 契约断言；不把持续产生
%%% Workspace 数据的共享开发库误当成迁移前快照。
%%%
%%% ⚠️ Gate 换档记录（2026-08-29）：channel-firstclass W2 Scope Contract 经 H1 放行
%%% （execution ledger §4），project_member / project_milestone / project_channel_rel /
%%% project.links 从 defer 清单移入 now，其 schema 断言由 w2_schema_contract_tests 承接；
%%% 本文件保留"仍禁止"项（多态参与表）、W0 now 项与回填形态断言。

-define(DEFERRED_TABLES, [
    %% 禁多态万能表/万能参与关系（计划 T13 IMPLEMENT 原文，W2 仍然禁止）
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
%%% （Gate 换档 2026-08-29：links 已随 W2 启用，存在性断言移至
%%%   w2_schema_contract_tests，本组断言退役）
%%% ===================================================================

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
%%% now 项：迁移存量行默认回填为 personal 且 workspace_id IS NULL
%%% ===================================================================

legacy_rows_default_to_personal_test() ->
    {ok, Migration} = file:read_file("priv/migrations/00000077_resource_scope.up.sql"),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Migration,
            <<"ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope text NOT NULL DEFAULT 'personal';">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Migration,
            <<"ALTER TABLE \"group\" ADD COLUMN IF NOT EXISTS scope text NOT NULL DEFAULT 'personal';">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Migration, <<"ALTER TABLE channel ADD COLUMN IF NOT EXISTS workspace_id bigint;">>
        )
    ),
    ?assertNotEqual(
        nomatch,
        binary:match(
            Migration, <<"ALTER TABLE \"group\" ADD COLUMN IF NOT EXISTS workspace_id bigint;">>
        )
    ).

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
