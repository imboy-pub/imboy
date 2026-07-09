-module(mcp_client_repo).

%%%
% MCP 客户端登记/审批仓库 / MCP client registry & approval repository
% 表 mcp_client（见 priv/migrations/00000028_mcp_governance）：
%   client_id TSID 主键，owner_uid 唯一（一个用户一条 client 记录）。
% 所有 SQL 经 elib_pg 参数化。
%%%

-export([tablename/0]).
-export([find_by_owner/1]).
-export([find/1]).
-export([ensure/2]).
-export([set_status/4]).
-export([page/4]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"mcp_client">>).

%% @doc 按 owner_uid 查 client（唯一）
-spec find_by_owner(integer()) -> {ok, map()} | {error, notfound | term()}.
find_by_owner(OwnerUid) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at FROM ",
            Tb/binary,
            " WHERE owner_uid = $1"
        >>,
    case elib_pg:query(Sql, [OwnerUid]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 client_id 查 client
-spec find(integer()) -> {ok, map()} | {error, notfound | term()}.
find(ClientId) ->
    Tb = tablename(),
    Sql =
        <<
            "SELECT client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at FROM ",
            Tb/binary,
            " WHERE client_id = $1"
        >>,
    case elib_pg:query(Sql, [ClientId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, notfound};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 惰性登记：owner_uid 无记录则插入 pending，返回 client_id。
%% 并发下靠 uniq_mcp_client_owner + ON CONFLICT DO NOTHING 幂等。
-spec ensure(integer(), binary()) -> {ok, integer()} | {error, term()}.
ensure(OwnerUid, Name) ->
    case find_by_owner(OwnerUid) of
        {ok, #{<<"client_id">> := ClientId}} ->
            {ok, ClientId};
        {error, notfound} ->
            ClientId = elib_tsid:generate(mcp_client),
            Tb = tablename(),
            Sql =
                <<"INSERT INTO ", Tb/binary,
                    " (client_id, owner_uid, name, status, created_at, updated_at)"
                    " VALUES ($1,$2,$3,'pending',NOW(),NOW())"
                    " ON CONFLICT (owner_uid) DO NOTHING">>,
            case elib_pg:query(Sql, [ClientId, OwnerUid, Name]) of
                {ok, _} ->
                    %% 若刚好被并发抢先插入，重查取权威 client_id
                    case find_by_owner(OwnerUid) of
                        {ok, #{<<"client_id">> := Cid}} -> {ok, Cid};
                        Other -> Other
                    end;
                {error, Reason} ->
                    ?ERROR_LOG("mcp_client_repo:ensure owner=~p error ~p~n", [OwnerUid, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 改状态（approved 记 approved_at；reject/revoke 记 reason）
-spec set_status(integer(), binary(), binary(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
set_status(ClientId, <<"approved">>, _Reason, _ActorUid) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{status => <<"approved">>, approved_at => elib_dt:now(), updated_at => elib_dt:now()},
        <<"client_id = $1">>,
        [ClientId]
    );
set_status(ClientId, Status, Reason, _ActorUid) ->
    Tb = tablename(),
    elib_pg:update(
        Tb,
        #{status => Status, reason => Reason, updated_at => elib_dt:now()},
        <<"client_id = $1">>,
        [ClientId]
    ).

%% @doc 分页列出 client（可按 status 过滤；keyword 模糊 MVP 从简，仅 status 等值过滤）
-spec page(pos_integer(), pos_integer(), binary(), binary()) ->
    {ok, map()} | {error, term()}.
page(Page, Size, Status, _Keyword) ->
    Tb = tablename(),
    Column =
        <<
            "client_id, owner_uid, name, description, status, reason,"
            " created_at, approved_at"
        >>,
    Where = build_where(Status),
    elib_pg:page_with_total(Tb, Column, Where, <<"created_at DESC">>, Page, Size).

%% ponytail: keyword 模糊匹配后续真需要再下推 SQL，当前仅 status 等值过滤。
build_where(<<>>) -> #{};
build_where(Status) -> #{status => Status}.
