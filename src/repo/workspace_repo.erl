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
-export([find_by_owner_and_name/3]).
-export([find_by_request_id/3]).
-export([update_by_id/2]).
-export([update_owner_tx/3]).
-export([page_by_member/4]).
-export([count_by_owner/1]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
-include_lib("kernel/include/logger.hrl").
-include("log.hrl").

%% 输出白名单列：branding 内部键（如 _request_id 幂等标记）由 DS 层过滤，
%% repo 层恒返回原始行。
-define(WS_SAFE_COLUMNS,
    <<"id,name,logo,owner_id,status,archived_at,archived_by,type,branding,created_at,updated_at">>
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

%% @doc 幂等语义键查询：同一 Owner + 同名 active 工作区
%% 镜像 group_ds:find_by_creator_and_sum/2 的"先查后插于同事务"幂等模式
%% （workspace 表无 request_id 列，见 workspace_ds:create_template/3 注释）。
-spec find_by_owner_and_name(integer(), binary(), binary()) -> map().
find_by_owner_and_name(OwnerUid, Name, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE owner_id = $1 AND name = $2 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OwnerUid, Name]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc request_id 幂等查询：branding->>'_request_id' 内部标记
%% 仅匹配同 Owner + active 的工作区；空 map 表示未命中。
-spec find_by_request_id(integer(), binary(), binary()) -> map().
find_by_request_id(OwnerUid, RequestId, Conn) ->
    Tb = tablename(),
    Sql =
        <<"SELECT ", ?WS_SAFE_COLUMNS/binary, " FROM ", Tb/binary,
            " WHERE owner_id = $1 AND status = 'active'",
            " AND branding->>'_request_id' = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OwnerUid, RequestId]) of
        {ok, [Row | _]} -> Row;
        _ -> #{}
    end.

%% @doc 更新工作区（白名单字段由 workspace_logic 构造）
-spec update_by_id(integer(), map()) -> {ok, non_neg_integer()} | {error, term()}.
update_by_id(WsId, Data) ->
    Tb = tablename(),
    {Sql, Params} = elib_pg_sql:update(Tb, Data, #{id => WsId}),
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
