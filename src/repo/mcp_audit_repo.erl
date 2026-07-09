-module(mcp_audit_repo).

%%%
% MCP 治理审计仓库 / MCP governance audit repository
% 表 mcp_audit_log：记 client tool_call 行为 + 管理员 approve/reject/revoke 动作。
% tool_call 高频，单列本表（不塞 admin_operation_logs，避免污染管理员操作日志语义/体量）；
% 管理员审批动作在 logic 层同时复用 adm_operation_log_ds:insert（双写）。
%%%

-export([tablename/0]).
-export([insert/6]).
-export([page/4]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"mcp_audit_log">>).

%% @doc 写一条审计。Detail 为已编码 JSON binary。
-spec insert(integer(), integer(), binary(), binary(), integer(), binary()) ->
    {ok, [map()]} | {error, term()}.
insert(ClientId, OwnerUid, Action, Tool, ActorUid, Detail) ->
    Tb = tablename(),
    Id = elib_tsid:generate(mcp_audit_log),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, client_id, owner_uid, action, tool, actor_uid, detail, created_at)"
            " VALUES ($1,$2,$3,$4,$5,$6,$7::jsonb,NOW()) RETURNING id">>,
    case elib_pg:query(Sql, [Id, ClientId, OwnerUid, Action, Tool, ActorUid, Detail]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("mcp_audit_repo:insert action=~p error ~p~n", [Action, Reason]),
            {error, Reason}
    end.

%% @doc 分页审计（可按 client_id/action 过滤）
-spec page(pos_integer(), pos_integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
page(Page, Size, ClientId, Action) ->
    Tb = tablename(),
    Column = <<"id, client_id, owner_uid, action, tool, actor_uid, detail, created_at">>,
    Where = build_where(ClientId, Action),
    elib_pg:page_with_total(Tb, Column, Where, <<"created_at DESC">>, Page, Size).

build_where(0, <<>>) -> #{};
build_where(0, Action) -> #{action => Action};
build_where(ClientId, <<>>) -> #{client_id => ClientId};
build_where(ClientId, Action) -> #{client_id => ClientId, action => Action}.
