-module(organization_agent_facts_pg).

%% Agent Organization Boundary 事实读取 SQL（infrastructure，只读，ORG-06）。
%%
%% Agent V3.1 的稳定读合同（Agent Organization Contract §8）底层代查：
%%   resolve membership / resolve workspace membership /
%%   resolve organization state / validate workspace ownership。
%% 只封装语句与行集；允许裁决（fail closed）在 organization_agent_boundary
%% （domain），编排与事实形状在 organization_agent_facts_app（application）。
%%
%% 不新增 DDL（ORG-06 无迁移）：fact_version 锚定各行 updated_at 的
%% 单调投影（epoch 微秒，NULL 视为 0）；重复读取零副作用（纯 SELECT）。
%% 返回不含 PII/credential/内部 SQL（计划 §1.6 口径）。

-export([
    account_type/1,
    organization_row/1,
    membership_row/2,
    workspace_row/1,
    workspace_membership_row/2
]).

%% fact_version 统一投影：updated_at → epoch 微秒整数（可空列兜底 0）。
-define(FACT_VERSION_EXPR,
    <<"COALESCE((EXTRACT(EPOCH FROM updated_at) * 1000000)::bigint, 0) AS fact_version">>
).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc 账号身份类型（user.account_type；1=Agent，0=Human）。
-spec account_type(integer()) -> {ok, integer()} | {error, not_found | term()}.
account_type(Uid) ->
    Sql = <<"SELECT account_type FROM ", (user_table())/binary, " WHERE id = $1">>,
    case elib_pg:query(Sql, [Uid]) of
        {ok, [Row | _]} -> {ok, elib_cnv:safe_to_integer(maps:get(<<"account_type">>, Row))};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Organization 生命周期行（id/status/fact_version）。
-spec organization_row(integer()) -> {ok, map()} | {error, not_found | term()}.
organization_row(OrgId) ->
    Sql =
        <<"SELECT id, status, ", (?FACT_VERSION_EXPR)/binary, " FROM ",
            (organization_table())/binary, " WHERE id = $1">>,
    one(Sql, [OrgId]).

%% @doc Organization 成员行（**不带 status 过滤**：
%% suspended/removed 是必须如实上报的边界事实，fail closed 由 domain 裁决）。
-spec membership_row(integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
membership_row(OrgId, Uid) ->
    Sql =
        <<"SELECT role, status, ", (?FACT_VERSION_EXPR)/binary, " FROM ", (member_table())/binary,
            " WHERE organization_id = $1 AND user_id = $2">>,
    one(Sql, [OrgId, Uid]).

%% @doc Workspace 行（归属 Org / 生命周期 / fact_version）。
-spec workspace_row(integer()) -> {ok, map()} | {error, not_found | term()}.
workspace_row(WsId) ->
    Sql =
        <<"SELECT id, organization_id, status, ", (?FACT_VERSION_EXPR)/binary, " FROM ",
            (workspace_table())/binary, " WHERE id = $1">>,
    one(Sql, [WsId]).

%% @doc Workspace 成员行（不带 status 过滤，理由同 membership_row/2）。
-spec workspace_membership_row(integer(), integer()) ->
    {ok, map()} | {error, not_found | term()}.
workspace_membership_row(WsId, Uid) ->
    Sql =
        <<"SELECT role, status, ", (?FACT_VERSION_EXPR)/binary, " FROM ",
            (workspace_member_table())/binary, " WHERE workspace_id = $1 AND user_id = $2">>,
    one(Sql, [WsId, Uid]).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

-spec one(binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one(Sql, Params) ->
    case elib_pg:query(Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec user_table() -> binary().
user_table() ->
    elib_pg_sql:public_tablename(<<"user">>).

-spec organization_table() -> binary().
organization_table() ->
    elib_pg_sql:public_tablename(<<"organization">>).

-spec member_table() -> binary().
member_table() ->
    elib_pg_sql:public_tablename(<<"organization_member">>).

-spec workspace_table() -> binary().
workspace_table() ->
    elib_pg_sql:public_tablename(<<"workspace">>).

-spec workspace_member_table() -> binary().
workspace_member_table() ->
    elib_pg_sql:public_tablename(<<"workspace_member">>).
