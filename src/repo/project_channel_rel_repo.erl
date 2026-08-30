-module(project_channel_rel_repo).
%%%
% project_channel_rel_repo 是 project_channel_rel repository 缩写
% 项目↔频道关联数据仓库层（迁移 00000081，channel-firstclass W2 ZC-04）
%
% 表结构：project_channel_rel(workspace_id 冗余, project_id, channel_id,
%   created_by 可空审计, created_at)；PK (project_id, channel_id) 即
%   "重复关联只一行"；两侧复合 FK 强制 project 与 channel 同 workspace
%   （personal 频道 workspace_id IS NULL 在 FK 层即被拒）。
%
% 幂等 UPSERT 语义由应用层实现：INSERT ... ON CONFLICT DO NOTHING，
% 返回实际插入行数（1=新建 / 0=已存在）。
%
% ⚠️ 整合点（ZC-05）：find_project_member/2 是 project_member 的最小只读
% 查询（project_member_repo 由 ZC-02 产出，本卡不得依赖其模块）；
% ZC-05 统一注册路由时应将本函数迁至 project_member_repo 并改调用方。
%%%

-export([tablename/0]).
-export([insert_on_conflict_tx/5]).
-export([delete_tx/3]).
-export([find_channel_tx/3]).
-export([find_project_member/2]).
-export([page_channels_by_project/3]).

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
    elib_pg_sql:public_tablename(<<"project_channel_rel">>).

%% @doc 事务内幂等插入关联（PK (project_id, channel_id) 冲突 DO NOTHING）
%% 返回 {ok, 1}=新建 / {ok, 0}=已存在（调用方据此决定是否写 channel_linked 事件）；
%% 复合 FK 违例（23503：跨 Workspace / personal 频道）原样上抛由 DS 归一 400。
-spec insert_on_conflict_tx(any(), integer(), integer(), integer(), integer()) ->
    {ok, 0 | 1} | {error, term()}.
insert_on_conflict_tx(Conn, WsId, ProjectId, ChannelId, CreatedBy) ->
    Tb = tablename(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (workspace_id, project_id, channel_id, created_by, created_at)",
            " VALUES ($1, $2, $3, $4, CURRENT_TIMESTAMP)",
            " ON CONFLICT (project_id, channel_id) DO NOTHING">>,
    case elib_pg:execute(Conn, Sql, [WsId, ProjectId, ChannelId, CreatedBy]) of
        {ok, Count} when Count =:= 0; Count =:= 1 -> {ok, Count};
        {error, _} = Err -> Err
    end.

%% @doc 事务内删除关联，返回实际删除行数（0=关联不存在）
-spec delete_tx(any(), integer(), integer()) -> {ok, 0 | 1} | {error, term()}.
delete_tx(Conn, ProjectId, ChannelId) ->
    Tb = tablename(),
    Sql =
        <<"DELETE FROM ", Tb/binary, " WHERE project_id = $1 AND channel_id = $2">>,
    case elib_pg:execute(Conn, Sql, [ProjectId, ChannelId]) of
        {ok, Count} when Count =:= 0; Count =:= 1 -> {ok, Count};
        {error, _} = Err -> Err
    end.

%% @doc 事务内查询频道行（关联前置校验用；空 map = 频道不存在）
-spec find_channel_tx(any(), integer(), binary()) -> map().
find_channel_tx(Conn, ChannelId, Column) ->
    Tb = elib_pg_sql:public_tablename(<<"channel">>),
    Sql = <<"SELECT ", Column/binary, " FROM ", Tb/binary, " WHERE id = $1">>,
    case elib_pg:query(Conn, Sql, [ChannelId]) of
        {ok, [Row | _]} ->
            Row;
        {error, Reason} ->
            _ = ?ERROR_LOG([project_channel_find_tx_failed, ChannelId, Reason]),
            #{};
        _ ->
            #{}
    end.

%% @doc 查询用户在项目下的成员关系行（空 map = 无关系）
%% ZC-05 收敛：转发 project_member_repo:find_row/2 单点实现；
%% 函数名保留以稳定既有调用点与 mock。
-spec find_project_member(integer(), integer()) -> map().
find_project_member(ProjectId, Uid) ->
    project_member_repo:find_row(ProjectId, Uid).

%% @doc 项目关联频道列表（JOIN channel 元数据；稳定排序 created_at DESC,
%% channel_id DESC；固定 2 条 SQL：count + data，无 N+1）
-spec page_channels_by_project(integer(), integer(), integer()) ->
    {ok, map()} | {error, term()}.
page_channels_by_project(ProjectId, Page, Size) ->
    Tb = tablename(),
    Offset = (Page - 1) * Size,
    CountSql = <<"SELECT COUNT(*) AS count FROM ", Tb/binary, " WHERE project_id = $1">>,
    Total =
        case elib_pg:one(CountSql, [ProjectId]) of
            {ok, #{<<"count">> := C}} -> C;
            _ -> 0
        end,
    ChTb = elib_pg_sql:public_tablename(<<"channel">>),
    DataSql =
        <<"SELECT r.channel_id, r.workspace_id, r.created_at AS linked_at,",
            " c.name, c.avatar, c.status AS channel_status", " FROM ", Tb/binary, " r JOIN ",
            ChTb/binary, " c ON c.id = r.channel_id", " WHERE r.project_id = $1",
            " ORDER BY r.created_at DESC, r.channel_id DESC LIMIT $2 OFFSET $3">>,
    case elib_pg:query(DataSql, [ProjectId, Size, Offset]) of
        {ok, Items} ->
            {ok, page_map(Items, Page, Size, Total)};
        {error, Reason} ->
            {error, Reason}
    end.

%% ===================================================================
%% Internal Function Definitions
%% ===================================================================

page_map(Items, Page, Size, Total) ->
    TotalPage =
        case Total > 0 of
            true -> ((Total - 1) div Size) + 1;
            false -> 0
        end,
    #{
        list => Items,
        page => Page,
        size => Size,
        total => Total,
        total_page => TotalPage
    }.
