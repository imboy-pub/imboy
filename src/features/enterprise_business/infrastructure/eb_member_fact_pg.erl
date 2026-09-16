%%% @doc EB-03R P10：最小只读事实的 PG 实现（成员状态 / 默认 Workspace）。
%%%
%%% 依据：用户裁决 §三——成员**只读事实**走最小 Port，**不得伪造 eb_member_app**；
%%% `suspend_member` 属 EB-08 的用例职责，本模块**不含任何写语句**。
%%%
%%% ## 为什么逐请求直读、不缓存
%%%
%%% 与 `eb_auth_port:load_request_facts/1` 同规：授权相关事实一旦跨请求缓存，
%%% 「suspended 后旧 JWT 立即失效」就退化为「等 token 过期」。本模块只输出事实，
%%% 但必须保证事实是**此刻**的。
%%%
%%% ## 事实 ≠ 授权结论
%%%
%%% `member_status/2` 回答「关系状态是什么」，不回答「这个请求能不能做 X」。
%%% 后者必须走 `eb_auth_port` + `eb_auth_app` 的逐请求判定。
%%%
%%% ## 作用域
%%%
%%% 成员事实是 **Org 域**事实（`organization_member` 的主键就是 Org+User），因此
%%% 只带 `organization_id`；默认 Workspace 解析额外要求「同 Org」+「成员 active」
%%% +「Workspace active」，三者缺一即 fail-closed。
-module(eb_member_fact_pg).

-behaviour(eb_member_fact_port).

-export([member_status/2, default_workspace/2, sql_statements/0]).

-define(SQL_MEMBER_STATUS, <<
    "SELECT m.organization_id, m.user_id, m.role, m.status"
    "  FROM organization_member m"
    " WHERE m.organization_id = $1 AND m.user_id = $2"
>>).

%% 默认 Workspace 解析（V1）：该 Org 下**状态 active 且 id 最小**的 Workspace，
%% 且成员必须 active。规则确定（同输入同输出），不接受调用方任意指定或切换。
-define(SQL_DEFAULT_WORKSPACE, <<
    "SELECT w.id AS workspace_id, w.organization_id, w.status"
    "  FROM workspace w"
    "  JOIN organization_member m"
    "    ON m.organization_id = w.organization_id AND m.user_id = $2"
    "   AND m.status = 'active'"
    " WHERE w.organization_id = $1 AND w.status = 'active'"
    " ORDER BY w.id"
    " LIMIT 1"
>>).

%% @doc 冻结语句（**只读**：无 INSERT/UPDATE/DELETE，A10 的机械判据之一）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_MEMBER_STATUS, ?SQL_DEFAULT_WORKSPACE].

%% @doc 读取成员在该 Org 内的关系状态；无成员关系 → `{error, no_member}`。
-spec member_status(integer(), integer()) -> {ok, atom()} | {error, term()}.
member_status(OrgId, UserId) when is_integer(OrgId), is_integer(UserId) ->
    case elib_pg:query(?SQL_MEMBER_STATUS, [OrgId, UserId]) of
        {ok, [Row | _]} ->
            {ok, status_atom(maps:get(<<"status">>, Row))};
        {ok, []} ->
            {error, no_member};
        {error, Reason} ->
            {error, eb_pg_store_sql:normalize_error(Reason)}
    end;
member_status(_OrgId, _UserId) ->
    {error, no_member}.

%% @doc 解析默认 Workspace（fail-closed：无 active 成员关系或无 active Workspace ⇒ error）。
-spec default_workspace(integer(), integer()) -> {ok, integer()} | {error, term()}.
default_workspace(OrgId, UserId) when is_integer(OrgId), is_integer(UserId) ->
    case elib_pg:query(?SQL_DEFAULT_WORKSPACE, [OrgId, UserId]) of
        {ok, [Row | _]} ->
            {ok, maps:get(<<"workspace_id">>, Row)};
        {ok, []} ->
            {error, no_default_workspace};
        {error, Reason} ->
            {error, eb_pg_store_sql:normalize_error(Reason)}
    end;
default_workspace(_OrgId, _UserId) ->
    {error, no_default_workspace}.

status_atom(<<"active">>) -> active;
status_atom(<<"suspended">>) -> suspended;
status_atom(<<"removed">>) -> removed;
status_atom(Other) -> Other.
