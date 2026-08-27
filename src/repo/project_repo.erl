-module(project_repo).
%%%
% project_repo 是 project repository 缩写
% 项目数据仓库层（迁移 00000078，双体验 v2.5.2 WP4/T6a）
%
% 表结构（priv/migrations/00000078_project_foundation.up.sql，W0 版）：
%   project(id TSID, workspace_id 非空FK, name, description, owner_id, status
%           active|done, timestamps)
%   无 links 列（Resources 聚合 defer）；无物理删除（仅 status 流转）。
% W0 硬约束：不建/不读/不写 project_member；Project 对 active
% workspace_member 可见；owner 的 active membership 由复合 FK
% fk_project_owner_membership + 触发器 trg_project_owner_membership_active
% 双兜底（均 DEFERRABLE，允许同事务先建 project 再补 membership）。
%%%

-export([tablename/0]).
-export([add_tx/2]).
-export([find_by_id/2]).
-export([find_tx/3]).
-export([page_by_workspace/4]).
-export([update_by_id/2]).
-export([update_fields_tx/3]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"project">>).

%% @doc 事务内插入项目（creator 同事务成为 Project Owner：owner_id=creator；
%% DB 复合 FK+可延迟触发器在 COMMIT 校验 owner 为 active workspace_member）
-spec add_tx(any(), map()) -> {ok, integer()} | {error, term()}.
add_tx(Conn, Data) ->
    Tb = tablename(),
    Id = elib_tsid:generate(project),
    Data2 = Data#{<<"id">> => Id},
    {Sql, Params} = elib_pg_sql:insert(Tb, Data2),
    case elib_pg:execute(Conn, Sql, Params) of
        {ok, _Count} -> {ok, Id};
        {error, _} = Err -> Err
    end.

%% @doc 按 ID 查询项目（自动提交；空 map = 无记录）
-spec find_by_id(integer() | binary(), binary()) -> map() | {error, term()}.
find_by_id(ProjectId, Column) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:build_select(Tb, Column, #{id => ProjectId}, #{limit => 1}),
    case elib_pg:one(Sql, Params) of
        {ok, Row} -> Row;
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内查询项目行
-spec find_tx(any(), integer(), binary()) -> map().
find_tx(Conn, ProjectId, Column) ->
    Tb = tablename(),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [ProjectId]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 工作区项目分页列表（稳定排序 created_at DESC, id DESC；走 i_project_workspace_id_id）
-spec page_by_workspace(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
page_by_workspace(WsId, Page, Size, Column) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    CountSql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE workspace_id = $1">>,
    Total =
        case elib_pg:one(CountSql, [WsId]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    DataSql =
        <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE workspace_id = $1",
            " ORDER BY created_at DESC, id DESC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [WsId, Size, Offset]) of
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

%% @doc 更新项目（自动提交；白名单字段由 logic 层构造）
-spec update_by_id(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_by_id(ProjectId, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:update(Tb, Data, #{id => ProjectId}),
    elib_pg:query(Sql, Params).

%% @doc 事务内更新项目字段（改名/描述/状态；守卫在同事务由 DS 层前置）
-spec update_fields_tx(any(), integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_fields_tx(Conn, ProjectId, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:update(Tb, Data, #{id => ProjectId}),
    elib_pg:execute(Conn, Sql, Params).
