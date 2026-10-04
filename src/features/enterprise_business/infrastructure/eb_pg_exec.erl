%%% @doc 企业业务持久化的**共享执行辅助**（EB-03R 新增能力的公共底座）。
%%%
%%% 为什么单独存在（F1/F4）：`eb_pg_store.erl` 拆分后，新的正向能力模块
%%%（contact / identity / message / offboarding / asset metadata）需要同一套
%%% 「租户门 + 参数化执行 + 行归一化 + 冲突翻译」，但不应该各自复制一份。
%%%
%%% 本模块只做机械动作，**不含业务规则**：
%%%   * 租户门：OrgId/WorkspaceId 必须是整数，否则 `{error, {invalid_tenant, _}}`（不触库）；
%%%   * 语句一律**参数化**（`$N`），绝不字符串拼接；
%%%   * 0 行写入 → 交由调用方按语义翻译（`no_row` / `conflict` / `not_found`）；
%%%   * 行归一化复用 `eb_pg_store_sql:normalize/2`（二进制列名 → 原子键、
%%%     timestamptz → Unix 秒、NULL → `undefined`）。
-module(eb_pg_exec).

-moduledoc "企业业务持久化共享执行辅助（EB-03R 新增能力的公共底座）。".
-export([
    tenant_error/2,
    with_tenant/3,
    fetch_one/3,
    fetch_many/3,
    execute_returning/2,
    insert_returning/2,
    scope_ok/2,
    conflict_or/3,
    error_of/1
]).

%% @doc 租户门：两个业务参数必须都是整数。
-spec tenant_error(term(), term()) -> ok | {error, {invalid_tenant, term()}}.
tenant_error(OrgId, WorkspaceId) when is_integer(OrgId), is_integer(WorkspaceId) ->
    ok;
tenant_error(OrgId, WorkspaceId) ->
    {error, {invalid_tenant, {OrgId, WorkspaceId}}}.

%% @doc 租户门 + 执行。
-spec with_tenant(term(), term(), fun(() -> T)) -> T | {error, term()}.
with_tenant(OrgId, WorkspaceId, Fun) ->
    case tenant_error(OrgId, WorkspaceId) of
        ok -> Fun();
        {error, _} = Err -> Err
    end.

%% @doc 单行读取（归一化）；0 行 → `{error, not_found}`。
-spec fetch_one(binary(), [term()], list()) -> {ok, map()} | {error, term()}.
fetch_one(Sql, Params, Fields) ->
    case fetch_many(Sql, Params, Fields) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} = Err -> Err
    end.

%% @doc 多行读取（归一化）。
-spec fetch_many(binary(), [term()], list()) -> {ok, [map()]} | {error, term()}.
fetch_many(Sql, Params, Fields) ->
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, [eb_pg_store_sql:normalize(Row, Fields) || Row <- Rows]};
        {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

%% @doc `INSERT/UPDATE ... RETURNING`：返回原始行列表（调用方自行决定归一化）。
%% 0 行 → `{ok, []}`（交由调用方翻译语义）。
-spec execute_returning(binary(), [term()]) -> {ok, [map()]} | {error, term()}.
execute_returning(Sql, Params) ->
    case elib_pg:execute(Sql, Params) of
        {ok, 1, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, 1} -> {ok, []};
        {ok, 0, _Rows} -> {ok, []};
        {ok, 0} -> {ok, []};
        {ok, _Other, Rows} when is_list(Rows) -> {ok, Rows};
        {ok, _Other} -> {ok, []};
        {error, Reason} -> {error, eb_pg_store_sql:normalize_error(Reason)}
    end.

%% @doc 写入必须落行：0 行 → `{error, no_row}`，交由调用方按语义翻译。
-spec insert_returning(binary(), [term()]) -> {ok, [map()]} | {error, term()}.
insert_returning(Sql, Params) ->
    case execute_returning(Sql, Params) of
        {ok, []} -> {error, no_row};
        Other -> Other
    end.

%% @doc (Org, Workspace) 自检：只在两者同时成立时返回 `true`。
-spec scope_ok(term(), term()) -> boolean().
scope_ok(OrgId, WorkspaceId) ->
    case elib_pg:query(eb_pg_store_sql:sql(scope_ok), [OrgId, WorkspaceId]) of
        {ok, [_ | _]} -> true;
        _ -> false
    end.

%% @doc 0 行且 scope 正常 ⇒ 唯一键冲突；否则是 workspace 归属错（不是 conflict）。
-spec conflict_or(term(), term(), term()) ->
    {error, conflict} | {error, {workspace_not_in_org, term()}}.
conflict_or(OrgId, WorkspaceId, WorkspaceId) ->
    case scope_ok(OrgId, WorkspaceId) of
        true -> {error, conflict};
        false -> {error, {workspace_not_in_org, WorkspaceId}}
    end.

%% @doc 从 epgsql 错误结构里取 SQLSTATE（`{error, {sql, Code, Constraint}}` 的中间元素）。
-spec error_of(term()) -> binary() | undefined.
error_of(Reason) ->
    case eb_pg_store_sql:normalize_error(Reason) of
        {sql, Code, _Constraint} -> Code;
        _ -> undefined
    end.
