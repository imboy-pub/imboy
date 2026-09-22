-module(organization_default_workspace_pg).

%% Organization Default Workspace 读写 SQL（infrastructure，事务内使用）。
%%
%% 只封装语句与行集；锁序与业务裁决在 organization_default_workspace_app。
%% 变更路径显式贯穿 organization_id（org 作用域），锁 organization 行复用
%% organization_owner_store:lock_organization_tx/2（与 owner transfer 保持
%% 「组织行先」的同一锁顺序）。
%% 读取路径不回落 min-ID 推导：显式关系是唯一读取真源（C05 TRANSITION 完成态）。

-export([
    find_tx/2,
    find/1,
    target_row_tx/2,
    upsert_tx/3,
    delete_tx/2,
    ensure_first_workspace_tx/3,
    set_replacement_on_archive_tx/4,
    remaining_active_ids_tx/3
]).

%%--------------------------------------------------------------------
%% 读
%%--------------------------------------------------------------------

%% @doc 事务内读取默认 workspace_id；无行返回 not_found。
-spec find_tx(any(), integer()) -> {ok, integer()} | {error, not_found | term()}.
find_tx(Conn, OrgId) ->
    case elib_pg:query(Conn, select_sql(), [OrgId]) of
        {ok, [#{<<"workspace_id">> := WsId} | _]} -> {ok, WsId};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 自带连接读取默认 workspace_id（应用层 get/1）。
-spec find(integer()) -> {ok, integer()} | {error, not_found | term()}.
find(OrgId) ->
    case elib_pg:query(select_sql(), [OrgId]) of
        {ok, [#{<<"workspace_id">> := WsId} | _]} -> {ok, WsId};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内读取 set 目标行（organization_id/status/owner_id 三列：前两列用于
%% 同 Org/active 预检，owner_id 用于调用者鉴权——「目标 Workspace 的 owner」
%% 是合法调用方，见 organization_default_workspace_app:set_tx/4）。
%% 按 id 读取、**不带 org 过滤**：跨 Org 判定交给 domain
%% （organization_default_workspace:ensure_settable_target/3 裁决 cross_org），
%% 调用方不得在本层吞掉归属差异。
-spec target_row_tx(any(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
target_row_tx(Conn, WsId) ->
    Sql =
        <<"SELECT organization_id, status, owner_id FROM ", (workspace_table())/binary,
            " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [WsId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 同 Org 剩余 active workspace id 升序（不含被归档者）；
%% archive 交接（replace_with_min_active）取头元素。
-spec remaining_active_ids_tx(any(), integer(), integer()) -> {ok, [integer()]} | {error, term()}.
remaining_active_ids_tx(Conn, OrgId, ExcludeWsId) ->
    Sql =
        <<"SELECT id FROM ", (workspace_table())/binary,
            " WHERE organization_id = $1 AND id <> $2 AND status = 'active'", " ORDER BY id ASC">>,
    case elib_pg:query(Conn, Sql, [OrgId, ExcludeWsId]) of
        {ok, Rows} ->
            {ok, [elib_cnv:safe_to_integer(maps:get(<<"id">>, R)) || R <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% 写（事务内；调用方负责先锁 organization 行）
%%--------------------------------------------------------------------

%% @doc 设置/改设默认（同值幂等：WHERE 排除同值，0 行更新 = unchanged）。
%% 跨 Org / 非 active 目标由组合 FK（23503）与守卫触发器（23514）fail-closed。
%% 时间戳由 DB now() 供给（created_at 首设稳定，updated_at 仅真变更时刷新）。
-spec upsert_tx(any(), integer(), integer()) ->
    {ok, changed | unchanged} | {error, term()}.
upsert_tx(Conn, OrgId, WsId) ->
    Sql =
        <<"INSERT INTO ", (table())/binary,
            " (organization_id, workspace_id, created_at, updated_at)",
            " VALUES ($1, $2, now(), now())", " ON CONFLICT (organization_id) DO UPDATE",
            " SET workspace_id = EXCLUDED.workspace_id, updated_at = now()", " WHERE ",
            (table())/binary, ".workspace_id <> EXCLUDED.workspace_id">>,
    case elib_pg:execute(Conn, Sql, [OrgId, WsId]) of
        {ok, 1} -> {ok, changed};
        {ok, 0} -> {ok, unchanged};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 清空默认（幂等：无行删除 = already_empty）。
-spec delete_tx(any(), integer()) -> {ok, cleared | already_empty} | {error, term()}.
delete_tx(Conn, OrgId) ->
    Sql = <<"DELETE FROM ", (table())/binary, " WHERE organization_id = $1">>,
    case elib_pg:execute(Conn, Sql, [OrgId]) of
        {ok, 1} -> {ok, cleared};
        {ok, 0} -> {ok, already_empty};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 首个 Org Workspace 创建时同事务设默认（workspace_ds:create_template 调用）。
%% 守卫条件（同事务可见性）：
%%   1. 该 Org 尚无默认行；且
%%   2. 该 Org 除本工作区外无其他工作区行（= 首个）。
%% 并发同 Org 首建竞态由 PK 冲突仲裁（恰一行，ON CONFLICT DO NOTHING）。
%% 既有库存 Org（回填覆盖 / 非首个）不会由此自动改设——首个语义可证。
-spec ensure_first_workspace_tx(any(), integer(), integer()) -> ok | {error, term()}.
ensure_first_workspace_tx(Conn, OrgId, WsId) ->
    Sql =
        <<"INSERT INTO ", (table())/binary, " (organization_id, workspace_id)", " SELECT $1, $2",
            " WHERE NOT EXISTS (SELECT 1 FROM ", (table())/binary, " WHERE organization_id = $1)",
            " AND NOT EXISTS (SELECT 1 FROM ", (workspace_table())/binary,
            " WHERE organization_id = $1 AND id <> $2)",
            " ON CONFLICT (organization_id) DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, [OrgId, WsId]) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 归档默认 Workspace 时的**显式替代**交接（计划 §105：归档默认前必须
%% 先指定替代项）。仅当被归档者是该 Org 当前默认时动作；调用方必须给出
%% 替代项，且替代项须存在 / 同 Org / active（复用 domain 的单一裁决真源）。
%% 未指定 → {error, replacement_not_specified}（不再回落到 min-active 自动改指）。
-spec set_replacement_on_archive_tx(any(), integer(), integer(), integer() | undefined) ->
    ok | {error, term()}.
set_replacement_on_archive_tx(Conn, OrgId, ArchivedWsId, ReplacementWsId) ->
    case find_tx(Conn, OrgId) of
        {ok, ArchivedWsId} ->
            apply_replacement_tx(Conn, OrgId, ReplacementWsId);
        {ok, _Other} ->
            ok;
        {error, not_found} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

apply_replacement_tx(_Conn, _OrgId, undefined) ->
    {error, replacement_not_specified};
apply_replacement_tx(_Conn, _OrgId, WsId) when not is_integer(WsId); WsId =< 0 ->
    {error, replacement_not_specified};
apply_replacement_tx(Conn, OrgId, ReplacementWsId) ->
    case target_row_tx(Conn, ReplacementWsId) of
        {ok, #{<<"organization_id">> := RowOrg, <<"status">> := Status}} ->
            case
                organization_default_workspace:ensure_settable_target(
                    OrgId, RowOrg, Status
                )
            of
                ok ->
                    upsert_tx(Conn, OrgId, ReplacementWsId);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% 说明：原 `archive_handover_tx/3`（自动改指剩余最小 active）已随
%% 计划 §105 的落地删除——归档默认工作区现在必须由调用方显式给出替代项
%% （见 set_replacement_on_archive_tx/4），不再有"自动挑一个"的分支。
%% `remaining_active_ids_tx/3` 与 domain 的 `archive_decision/2` 保留，
%% 供测试与既有 domain 决策复用。

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

%% 默认行读取（单行单列；org 作用域）
-spec select_sql() -> binary().
select_sql() ->
    <<"SELECT workspace_id FROM ", (table())/binary, " WHERE organization_id = $1">>.

-spec table() -> binary().
table() ->
    elib_pg_sql:public_tablename(<<"organization_default_workspace">>).

-spec workspace_table() -> binary().
workspace_table() ->
    elib_pg_sql:public_tablename(<<"workspace">>).
