-module(workspace_admin_archive_fk_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("eunit_setup.hrl").

%%% admin 归档 FK 真库集成测试（PG 不可用自动 skip）
%%%
%%% 背景（auto_test 批次W2R1 bug）：workspace.archived_by 带 FK → "user"(id)
%%% （迁移 00000076），而运营归档写入的 archived_by 是 adm_user.id——不在
%%% user 表，UPDATE 必 23503，事务回滚后对外表现为 200 + code:500「归档失败」。
%%% 修复：admin 路径 archived_by 固定写 NULL（操作者审计由 handler 层
%%% audit_workspace_governance → admin_operation_logs 承担），本套件在真库上
%%% 断言归档成功且 archived_by IS NULL、archived_at 落值、恢复清空。
%%% 修复前本套件必红（23503）。

admin_archive_fk_end_to_end_test_() ->
    ?TEST_WITH_DB_TIMEOUT(30, fun() ->
        {ok, Conn} = take_conn(),
        {ok, _, [{OwnerId}]} = epgsql:equery(
            Conn, <<"SELECT id FROM \"user\" ORDER BY id ASC LIMIT 1">>, []
        ),
        WsId = elib_tsid:generate(workspace),
        {ok, 1} = epgsql:equery(
            Conn,
            <<"INSERT INTO workspace (id, name, owner_id, status)",
                " VALUES ($1, $2, $3, 'active')">>,
            [WsId, <<"W2R2FIX-archfk">>, OwnerId]
        ),
        try
            %% 修复前：此处 UPDATE 撞 fk_workspace_archived_by → 23503 → 500
            ?assertMatch(
                {ok, #{workspace_id := WsId, status := <<"archived">>, archived_by := null}},
                workspace_logic:admin_archive(103119732858947584, WsId)
            ),
            {ok, _, [{Status, ArchivedBy, ArchivedAt}]} = epgsql:equery(
                Conn,
                <<"SELECT status, archived_by, archived_at FROM workspace WHERE id = $1">>,
                [WsId]
            ),
            ?assertEqual(<<"archived">>, Status),
            ?assertEqual(null, ArchivedBy, "admin 归档 archived_by 必须 NULL（FK 完整性）"),
            ?assert(ArchivedAt =/= null, "archived_at 应落值"),
            %% 重复归档 409
            ?assertMatch(
                {error, {409, _}},
                workspace_logic:admin_archive(103119732858947584, WsId)
            ),
            %% 恢复清空审计列
            ?assertMatch(
                {ok, #{status := <<"active">>}},
                workspace_logic:admin_restore(103119732858947584, WsId)
            ),
            {ok, _, [{RestoredBy, RestoredAt}]} = epgsql:equery(
                Conn,
                <<"SELECT archived_by, archived_at FROM workspace WHERE id = $1">>,
                [WsId]
            ),
            ?assertEqual(null, RestoredBy),
            ?assertEqual(null, RestoredAt)
        after
            epgsql:equery(Conn, <<"DELETE FROM workspace WHERE id = $1">>, [WsId]),
            pooler:return_member(pgsql, Conn),
            ok
        end
    end).

-spec take_conn() -> {ok, pid()} | {error, term()}.
take_conn() ->
    case pooler:take_member(pgsql) of
        error_no_members ->
            timer:sleep(200),
            case pooler:take_member(pgsql) of
                error_no_members -> {error, no_connection};
                Conn -> {ok, Conn}
            end;
        Conn ->
            {ok, Conn}
    end.
