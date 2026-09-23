-module(workspace_repo).
%%%
% workspace_repo 是 workspace repository 缩写
% 工作区数据仓库层（迁移 00000076，双体验 v2.5.2 WP3/T4）
%
% 表结构（priv/migrations/00000076_workspace_foundation.up.sql）：
%   workspace(id TSID, name, logo, owner_id, status active|archived,
%             archived_at, archived_by, type, branding jsonb, timestamps)
% 物理删除不存在（计划 §1.4.2：Workspace 本期无物理删除 API）。
%%%

-export([tablename/0]).
-export([add/2]).
-export([find_by_id/2]).
-export([find_by_owner_and_name/4]).
-export([find_by_request_id/4]).
-export([update_by_id/2]).
-export([update_owner_tx/3]).
-export([page_by_member/4]).
-export([count_by_owner/1]).
-export([ids_by_organization/1]).
%% V2.1 Internal 只读面（INT-24/25 adapter；plan §6.1 "workspace_repo adapter"）
-export([internal_find_tx/3]).
-export([internal_covered_page_tx/5]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 输出白名单列：branding 内部键（如 _request_id 幂等标记）由 DS 层过滤，
%% repo 层恒返回原始行。
-define(WS_SAFE_COLUMNS,
    <<"id,name,logo,owner_id,organization_id,status,archived_at,archived_by,type,branding,created_at,updated_at">>
).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"workspace">>).

%% @doc 插入工作区（事务连接版；Template 原子初始化专用）
%% TSID 生成器 workspace 已注册（imboy_app:tsid_generator_names/0，
%% 由 elib_tsid_registration_guard_tests 守护）。
-spec add(any(), map()) -> {ok, integer()} | {error, term()}.
add(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(workspace),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 ID 查询工作区
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(WsId, Column) when is_binary(WsId); is_list(WsId) ->
    find_by_id(ec_cnv:to_integer(WsId), Column);
find_by_id(WsId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => WsId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 幂等语义键查询：同一 Organization + Owner + 同名 active 工作区
%% "先查后插于同事务"幂等模式（workspace 表无 request_id 列，
%% 见 workspace_ds:create_template/4 注释）。
-spec find_by_owner_and_name(integer() | undefined, integer(), binary(), any()) -> map().
find_by_owner_and_name(undefined, OwnerUid, Name, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE organization_id IS NULL AND owner_id = $1",
            " AND name = $2 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OwnerUid, Name]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end;
find_by_owner_and_name(OrgId, OwnerUid, Name, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE organization_id = $1 AND owner_id = $2",
            " AND name = $3 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, OwnerUid, Name]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc request_id 幂等查询：branding->>'_request_id' 内部标记
%% 仅匹配同 Organization + Owner + active 的工作区；空 map 表示未命中。
-spec find_by_request_id(integer() | undefined, integer(), binary(), any()) -> map().
find_by_request_id(undefined, OwnerUid, RequestId, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE organization_id IS NULL AND owner_id = $1 AND status = 'active'",
            " AND branding->>'_request_id' = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OwnerUid, RequestId]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end;
find_by_request_id(OrgId, OwnerUid, RequestId, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE organization_id = $1 AND owner_id = $2 AND status = 'active'",
            " AND branding->>'_request_id' = $3 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, OwnerUid, RequestId]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 更新工作区（白名单字段由 workspace_logic 构造）
-spec update_by_id(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_by_id(WsId, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:update(Tb, Data, <<"id = $1">>, [WsId]),
    elib_pg:query(Sql, Params).

%% @doc 事务内转移主 Owner（workspace.owner_id，计费锚点只读锚的治理面）
-spec update_owner_tx(any(), integer(), integer()) -> ok | {error, term()}.
update_owner_tx(Conn, WsId, NewOwnerUid) ->
    Tb = tablename(),
    Now = elib_dt:now(),
    %% 列名是 owner_id（非 owner_uid，见迁移 00000076）
    Sql = <<"UPDATE ", Tb/binary, " SET owner_id = $1, updated_at = $2 WHERE id = $3">>,
    case elib_pg:execute(Conn, Sql, [NewOwnerUid, Now, WsId]) of
        {ok, 1} -> ok;
        {ok, _} -> {error, workspace_not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 我的工作区列表（JOIN workspace_member，仅 active 成员身份）
%% 稳定排序 created_at DESC, id DESC；limit 上限由 DS 层钳制。
-spec page_by_member(integer(), integer(), integer(), binary()) -> {ok, map()} | {error, term()}.
page_by_member(Uid, Page, Size, Column) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    CountSql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " w JOIN workspace_member wm ON wm.workspace_id = w.id",
            " WHERE wm.user_id = $1 AND wm.status = 'active'">>,
    Total =
        case elib_pg:one(CountSql, [Uid]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    DataSql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary,
            " w JOIN workspace_member wm ON wm.workspace_id = w.id",
            " WHERE wm.user_id = $1 AND wm.status = 'active'",
            " ORDER BY w.created_at DESC, w.id DESC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [Uid, Size, Offset]) of
        {ok, Items} ->
            TotalPage =
                case Total > 0 of
                    true -> ((Total - 1) div Size) + 1;
                    false -> 0
                end,
            {ok, #{
                list => Items, page => Page, size => Size, total => Total, total_page => TotalPage
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 用户创建的 active 工作区数量（防滥建上限用）
-spec count_by_owner(integer()) -> non_neg_integer().
count_by_owner(OwnerUid) ->
    Tb = tablename(),
    Sql =
        <<"SELECT COUNT(*) AS count FROM ", Tb/binary,
            " WHERE owner_id = $1 AND status = 'active'">>,
    case elib_pg:one(Sql, [OwnerUid]) of
        {ok, #{<<"count">> := Count}} -> Count;
        _ -> 0
    end.

%% @doc Organization 名下全部 workspace id（Admin 企业入口 O 维度过滤真源；
%% adm_enterprise_filter 把结果以 IN 谓词下推给 group/channel 列表）。
%% 查询失败返回 {error, _}（调用方 fail-closed 处理，不兜底全量）。
-spec ids_by_organization(integer()) -> {ok, [integer()]} | {error, term()}.
ids_by_organization(OrgId) when OrgId > 0 ->
    Tb = tablename(),
    Sql =
        <<"SELECT id FROM ", Tb/binary, " WHERE organization_id = $1 ORDER BY id ASC">>,
    case elib_pg:query(Sql, [OrgId]) of
        {ok, Rows} ->
            {ok, [maps:get(<<"id">>, Row) || Row <- Rows]};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% V2.1 Internal 只读面（INT-24/25 adapter）
%% ===================================================================

%% @doc INT-25 详情定位（Org 边界 + active）：返回最小投影列
%% （id, name, owner_id, created_at）；跨 Org / 不存在 / 已归档 → {error, not_found}
%% （IDOR 同不存在同体，不泄露存在性）。
-spec internal_find_tx(any(), integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
internal_find_tx(Conn, OrgId, WsId) when
    is_integer(OrgId), is_integer(WsId), WsId > 0
->
    Sql =
        <<"SELECT id, name, owner_id, created_at FROM ", (tablename())/binary,
            " WHERE id = $1 AND organization_id = $2 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [WsId, OrgId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc INT-24 keyset 列表：active + **仅 Grant 覆盖的 W**（kind=list 的
%% 行级收窄是 SQL 义务，plan §6.2/边界 moduledoc：覆盖谓词 = 存在同一生效
%% Grant 覆盖 scope 且 kind='none'（Org 全域）或显式命中该 W）。
%% 排序 created_at DESC, id DESC；Pivot 为续页 keyset 元组（首页 undefined）；
%% 调用方传 Limit（已按 §10.1 校验），本查询不加 1——has_more 判定在
%% logic 层（以 Limit+1 调用）。
-spec internal_covered_page_tx(
    any(), integer(), integer(), undefined | {binary(), integer()}, pos_integer()
) -> {ok, [map()]} | {error, term()}.
internal_covered_page_tx(Conn, OrgId, AppId, Pivot, Limit) when
    is_integer(OrgId), is_integer(AppId), is_integer(Limit), Limit > 0
->
    Scope = <<"workspaces:read">>,
    GrantView = enterprise_application_grant_repo:effective_view(),
    ScopeTb = enterprise_application_grant_repo:scope_tablename(),
    WsTb = enterprise_application_grant_repo:workspace_tablename(),
    CoveredExists =
        <<
            " AND EXISTS (SELECT 1 FROM ",
            GrantView/binary,
            " g",
            " JOIN ",
            ScopeTb/binary,
            " s ON s.grant_id = g.grant_id",
            " LEFT JOIN ",
            WsTb/binary,
            " gw ON gw.grant_id = g.grant_id",
            "   AND gw.workspace_id = w.id",
            " WHERE g.organization_id = $1 AND g.application_id = $2",
            "   AND s.scope = $3",
            "   AND (g.workspace_scope_kind = 'none' OR gw.grant_id IS NOT NULL))"
        >>,
    {KeysetClause, Params0} =
        case Pivot of
            undefined ->
                {<<>>, []};
            {CreatedAt, Id} ->
                {<<" AND (w.created_at, w.id) < ($4, $5)">>, [CreatedAt, Id]}
        end,
    Sql =
        <<"SELECT w.id, w.name, w.owner_id, w.created_at FROM ", (tablename())/binary, " w",
            " WHERE w.organization_id = $1 AND w.status = 'active'", CoveredExists/binary,
            KeysetClause/binary, " ORDER BY w.created_at DESC, w.id DESC", " LIMIT $",
            (integer_to_binary(4 + length(Params0)))/binary>>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, Scope] ++ Params0 ++ [Limit]) of
        {ok, Rows} -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.
