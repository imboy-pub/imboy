-module(mcp_client_grant_repo).

%%%
% MCP 客户端按 tool 授权仓库 / MCP per-tool grant repository
% 表 mcp_client_grant：UNIQUE(client_id, tool_name)，enforce=true 时授权判定读它。
%%%

-export([tablename/0]).
-export([upsert/3]).
-export([set_enabled/3]).
-export([list_by_client/1]).
-export([is_enabled/2]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"mcp_client_grant">>).

%% @doc upsert 一条 grant（approve 时批量授予各 tool 用）
-spec upsert(integer(), binary(), boolean()) -> {ok, [map()]} | {error, term()}.
upsert(ClientId, ToolName, Enabled) ->
    Tb = tablename(),
    Id = elib_tsid:generate(mcp_client_grant),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, client_id, tool_name, enabled, created_at)"
            " VALUES ($1,$2,$3,$4,NOW())"
            " ON CONFLICT (client_id, tool_name)"
            " DO UPDATE SET enabled = EXCLUDED.enabled RETURNING id">>,
    case elib_pg:query(Sql, [Id, ClientId, ToolName, Enabled]) of
        {ok, Rows} ->
            {ok, Rows};
        {error, Reason} ->
            ?ERROR_LOG("mcp_client_grant_repo:upsert client=~p tool=~p error ~p~n", [
                ClientId, ToolName, Reason
            ]),
            {error, Reason}
    end.

%% @doc 单独开关某 tool 授权
-spec set_enabled(integer(), binary(), boolean()) -> {ok, [map()]} | {error, term()}.
set_enabled(ClientId, ToolName, Enabled) ->
    upsert(ClientId, ToolName, Enabled).

%% @doc 列出某 client 的全部 grant
-spec list_by_client(integer()) -> {ok, [map()]} | {error, term()}.
list_by_client(ClientId) ->
    Tb = tablename(),
    Sql =
        <<"SELECT tool_name, enabled FROM ", Tb/binary,
            " WHERE client_id = $1 ORDER BY tool_name">>,
    elib_pg:query(Sql, [ClientId]).

%% @doc 某 tool 是否被授权（无记录=未授权=false）
-spec is_enabled(integer(), binary()) -> boolean().
is_enabled(ClientId, ToolName) ->
    Tb = tablename(),
    Sql =
        <<"SELECT enabled FROM ", Tb/binary, " WHERE client_id = $1 AND tool_name = $2">>,
    case elib_pg:query(Sql, [ClientId, ToolName]) of
        {ok, [#{<<"enabled">> := true} | _]} -> true;
        _ -> false
    end.
