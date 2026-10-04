-module(enterprise_group_origin_repo).

-moduledoc "企业群 Application 归属仓储层。".
%%%
% enterprise_group_origin_repo 是企业群 **Application 归属** 仓储
% （FULL-02 / plan-full §3.1「企业群生命周期、成员角色、Application membership」；
% 表由 migration 00000140 建立）。
%
% Application membership 的语义：每一行记录「哪一份 Application 在哪个
% Org/Workspace 建立了该企业群」——INT-04 建群时同事务写入；群详情读取它；
% 归档把 status 推到 archived（单向，DB 触发器 23514 拒绝回退）。行禁止物理删除
% （证据只增不减）。
%
% 归属性判定（owns_tx/4）用于新路由的生命周期操作：OA 只能归档/改角色自己
% 建立的企业群（workspace 级边界仍由既有 find_group_in_org_tx 判定）。
%%%

-export([
    tablename/0,
    insert_tx/5,
    find_tx/2,
    owns_tx/4,
    archive_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "group_id, organization_id, application_id, workspace_id, status, archived_at,"
    " created_at, updated_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_group_origin">>).

%% @doc 事务内登记企业群归属（INT-04 建群同事务）。重复登记同 group_id →
%% {error, already_exists}（PK 冲突归一；调用方按内部错误处理，属程序错误）。
-spec insert_tx(any(), pos_integer(), integer(), integer(), integer()) ->
    {ok, map()} | {error, already_exists | term()}.
insert_tx(Conn, GroupId, OrgId, AppId, WsId) when
    is_integer(GroupId), GroupId > 0, is_integer(OrgId), is_integer(AppId), is_integer(WsId)
->
    Sql =
        <<"INSERT INTO ", (tablename())/binary,
            " (group_id, organization_id, application_id, workspace_id, status, created_at)",
            " VALUES ($1, $2, $3, $4, 'active', NOW())", " RETURNING ", ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [GroupId, OrgId, AppId, WsId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, #error{code = <<"23505">>}} ->
            {error, already_exists};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按 group_id 取归属行（任意状态）。
-spec find_tx(any(), pos_integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, GroupId) when is_integer(GroupId), GroupId > 0 ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE group_id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [GroupId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 归属判定：该群是否由**本 (org, app)** 建立（跨 Org/跨 App 一律 false）。
-spec owns_tx(any(), pos_integer(), integer(), integer()) ->
    {ok, boolean()} | {error, term()}.
owns_tx(Conn, GroupId, OrgId, AppId) when is_integer(GroupId), GroupId > 0 ->
    Sql =
        <<"SELECT EXISTS (SELECT 1 FROM ", (tablename())/binary,
            " WHERE group_id = $1 AND organization_id = $2 AND application_id = $3) AS owned">>,
    case elib_pg:query(Conn, Sql, [GroupId, OrgId, AppId]) of
        {ok, [Row | _]} -> {ok, maps:get(<<"owned">>, Row)};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内归档归属行（单向；幂等：已 archived 返回 {ok, false} 且不改行）。
%% DB 触发器保证 archived 不可回退（23514）。
-spec archive_tx(any(), pos_integer()) -> {ok, boolean()} | {error, term()}.
archive_tx(Conn, GroupId) when is_integer(GroupId), GroupId > 0 ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 'archived', archived_at = NOW(), updated_at = NOW()",
            " WHERE group_id = $1 AND status = 'active'", " RETURNING group_id">>,
    case elib_pg:query(Conn, Sql, [GroupId]) of
        {ok, [_ | _]} -> {ok, true};
        {ok, []} -> {ok, false};
        {error, Reason} -> {error, Reason}
    end.
