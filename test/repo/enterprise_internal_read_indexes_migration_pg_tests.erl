%% enterprise_internal_read_indexes_migration_pg_tests
%% V2.1 A2-REPAIR — 迁移 00000145（enterprise internal 只读面 3 个热点索引，
%% F2 EXPLAIN 证据裁决）真库往返回归。
%%
%% 一次性 marker 库（inttest_marker_db 配方，env 前缀 EOV21A2IX_INTTEST）：
%%   空库全量迁移 up（erlang_migrate strict，含 00000145）→ head schema
%%   → down 145（回到 144）→ 3 索引残留归零 → up 回 head → 索引复现。
%%
%% 防棘轮约定：**不写死 head 绝对号**——head 断言一律用动态
%% migration_head()（目录内最大版本号），本迁移只断言 head >= 145 与
%% 「down 到 144 后本迁移对象消失、up 回 head 后复现」这个相对关系；
%% 后续新迁移落地后本套件保持绿。
%%
%% oracle（F2 verifiers/index-decisions.json，verdict=ADD_INDEX 3 条）：
%%    ① i_ws_org_created      ON workspace (organization_id, created_at DESC, id DESC)
%%    ② i_group_ws_created    ON "group" (created_at DESC, id DESC)
%%                            WHERE scope='workspace' AND status=1
%%    ③ i_gm_grp_created      ON group_member (group_id, created_at, id) WHERE status=1
%%
%% marker 库供给失败（环境/配置/迁移任一不可用）显式 FAIL，无静默 skip。

-module(enterprise_internal_read_indexes_migration_pg_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% 本套件被测迁移的版本号与回滚目标（回滚目标 144 是历史，不可变）
-define(THIS_MIGRATION, 145).
-define(DOWN_TARGET, 144).

-define(IDX_WS, <<"i_ws_org_created">>).
-define(IDX_GROUP, <<"i_group_ws_created">>).
-define(IDX_GM, <<"i_gm_grp_created">>).

%%%===================================================================
%%% Fixture
%%%===================================================================

setup_state() ->
    _ = code:add_patha("deps/erlang_migrate/ebin"),
    inttest_marker_db:provision(#{
        env_prefix => <<"EOV21A2IX_INTTEST">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }).

close_state(State) ->
    inttest_marker_db:release(State),
    ok.

%% 迁移版本变更（down/up）用独立连接（erlang_migrate 需事务自治）。
connect_marker(State) ->
    #{host := Host, port := Port, username := User, password := Pass} = maps:get(server, State),
    {ok, Conn} =
        inttest_marker_db:safe_connect(#{
            host => Host,
            port => Port,
            username => User,
            password => Pass,
            database => maps:get(db, State),
            timeout => 10000,
            codecs => [{epgsql_codec_rfc3339_bin, []}]
        }),
    Conn.

migration_head() ->
    {ok, Files} = file:list_dir("priv/migrations"),
    Versions = [
        list_to_integer(Ver)
     || F <- Files,
        {match, [Ver]} <- [re:run(F, "^(\\d{8})_.*\\.up\\.sql$", [{capture, all_but_first, list}])]
    ],
    ?assertNotEqual([], Versions),
    lists:max(Versions).

%%%===================================================================
%%% Tests
%%%===================================================================

enterprise_internal_read_indexes_migration_test_() ->
    {timeout, 900,
        {setup, fun setup_state/0, fun close_state/1, fun(State) ->
            C = maps:get(conn, State),
            %% inorder：down/up 会整库改 schema，与其余用例并发会互相踩。
            {inorder, [
                {"head_schema_three_indexes_present", {timeout, 60, head_schema_test(C)}},
                {"down_up_roundtrip_ratchet_safe",
                    {timeout, 300, fun() -> down_up_cycle_test(State) end}}
            ]}
        end}}.

%% ⓪ head schema：动态 head + 3 索引定义逐条核对
head_schema_test(C) ->
    ?_test(begin
        {ok, Version, Dirty} = erlang_migrate:version(#{conn => C, dir => "priv/migrations"}),
        ?assertEqual(false, Dirty),
        %% 相对头断言（防棘轮）：DB 版本 = 目录 head，且 >= 本迁移
        ?assertEqual(migration_head(), Version),
        ?assert(Version >= ?THIS_MIGRATION),
        ?assert(has_index(C, ?IDX_WS)),
        ?assert(has_index(C, ?IDX_GROUP)),
        ?assert(has_index(C, ?IDX_GM)),
        %% 定义逐条核对（列集 + partial 谓词）
        WsDef = index_def(C, ?IDX_WS),
        ?assert(string:find(WsDef, "(organization_id, created_at DESC, id DESC)") =/= nomatch),
        GroupDef = index_def(C, ?IDX_GROUP),
        ?assert(string:find(GroupDef, "(created_at DESC, id DESC)") =/= nomatch),
        ?assert(string:find(GroupDef, "scope") =/= nomatch),
        ?assert(string:find(GroupDef, "status = 1") =/= nomatch),
        GmDef = index_def(C, ?IDX_GM),
        ?assert(string:find(GmDef, "(group_id, created_at, id)") =/= nomatch),
        ?assert(string:find(GmDef, "status = 1") =/= nomatch)
    end).

%% ⑤ down/up 往返：down 145 → 残留归零 → up 回 head → 复现
down_up_cycle_test(State) ->
    C = maps:get(conn, State),
    MigConn = connect_marker(State),
    MigConfig = #{conn => MigConn, dir => "priv/migrations"},
    try
        %% --- down 到 144 ---
        ok = erlang_migrate:goto(MigConfig, ?DOWN_TARGET),
        ?assertEqual({ok, ?DOWN_TARGET, false}, erlang_migrate:version(MigConfig)),
        %% 残留归零
        ?assertNot(has_index(C, ?IDX_WS)),
        ?assertNot(has_index(C, ?IDX_GROUP)),
        ?assertNot(has_index(C, ?IDX_GM)),
        %% --- up 回 head ---
        ok = erlang_migrate:up(MigConfig),
        Head = migration_head(),
        ?assertEqual({ok, Head, false}, erlang_migrate:version(MigConfig)),
        ?assert(Head >= ?THIS_MIGRATION),
        %% 重建后对象复现
        ?assert(has_index(C, ?IDX_WS)),
        ?assert(has_index(C, ?IDX_GROUP)),
        ?assert(has_index(C, ?IDX_GM))
    after
        epgsql:close(MigConn)
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

one(C, Sql, Params) ->
    case elib_pg:query(C, Sql, Params) of
        {ok, [Row | _]} -> Row;
        {ok, []} -> #{};
        {error, Reason} -> erlang:error({sql_error, Reason, Sql})
    end.

scalar(C, Sql, Params) ->
    Row = one(C, Sql, Params),
    case maps:values(Row) of
        [V] -> V;
        _ -> erlang:error({scalar_failed, Sql})
    end.

has_index(C, Name) when is_binary(Name) ->
    scalar(
        C,
        <<"SELECT count(*) FROM pg_indexes WHERE schemaname = 'public' AND indexname = $1">>,
        [Name]
    ) > 0.

index_def(C, Name) when is_binary(Name) ->
    scalar(
        C,
        <<"SELECT indexdef FROM pg_indexes WHERE schemaname = 'public' AND indexname = $1">>,
        [Name]
    ).
