%%% @doc 客服 PG 层共享机械辅助（行归一化 / 错误归一化 / 参数处理）。
%%%
%%% 镜像 `eb_pg_exec` / `eb_pg_store_sql` 的职责但**自包含**：跨 Feature 只能引用
%%% facade（铁律 5），因此本模块不引用任何 `eb_pg_*`，只依赖 core（`elib_pg`）。
%%%
%%% 只做机械动作，不含业务规则：
%%%   * 语句一律参数化（`$N`），绝不字符串拼接；
%%%   * 时间列在 SQL 里 `extract(epoch ...)::bigint` 取出（Unix 秒），归一化零转换；
%%%   * NULL → `undefined`；状态列由调用方 `to_status/1` 转 atom；
%%%   * DB 错误归一化为 `{sql, SQLSTATE, Constraint}` / `{db, Other}`。
-module(cs_pg_common).

-include_lib("epgsql/include/epgsql.hrl").

-export([
    fetch_one/3,
    fetch_many/3,
    normalize_row/2,
    normalize_error/1,
    error_constraint/1,
    error_code/1,
    nullify/1,
    jsonb/1,
    to_status/1
]).

%% @doc 单行读取（归一化）；0 行 → `{error, not_found}`。
-spec fetch_one(binary(), [term()], [atom()]) -> {ok, map()} | {error, term()}.
fetch_one(Sql, Params, Keys) ->
    case fetch_many(Sql, Params, Keys) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, _} = Err -> Err
    end.

%% @doc 多行读取（归一化）。
-spec fetch_many(binary(), [term()], [atom()]) -> {ok, [map()]} | {error, term()}.
fetch_many(Sql, Params, Keys) ->
    case elib_pg:query(Sql, Params) of
        {ok, Rows} -> {ok, [normalize_row(Row, Keys) || Row <- Rows]};
        {error, Reason} -> {error, normalize_error(Reason)}
    end.

%% @doc 行归一化：binary 列名 → atom 键；`null` → `undefined`；其余原样。
-spec normalize_row(map(), [atom()]) -> map().
normalize_row(Row, Keys) when is_map(Row) ->
    maps:from_list([
        {Key, null_to_undefined(maps:get(atom_to_binary(Key, utf8), Row, undefined))}
     || Key <- Keys
    ]);
normalize_row(_Row, _Keys) ->
    #{}.

null_to_undefined(null) -> undefined;
null_to_undefined(undefined) -> undefined;
null_to_undefined(Value) -> Value.

%% @doc 状态列二进制 → atom（未知值 fail-closed 原样返回 binary，调用方判型）。
-spec to_status(binary() | atom()) -> atom() | binary().
to_status(<<"queued">>) -> queued;
to_status(<<"active">>) -> active;
to_status(<<"closed">>) -> closed;
to_status(Other) -> Other.

%% @doc DB 错误归一化（与 `eb_pg_store_sql` 同口径）。
-spec normalize_error(term()) -> {sql, binary(), binary() | undefined} | {db, term()}.
normalize_error(Reason) ->
    case Reason of
        #error{} = Err -> {sql, Err#error.code, error_constraint(Err#error.extra)};
        Other -> {db, Other}
    end.

-spec error_constraint(term()) -> binary() | undefined.
error_constraint(Extra) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
error_constraint(_Other) ->
    undefined.

%% @doc 从归一化错误取 SQLSTATE。
-spec error_code(term()) -> binary() | undefined.
error_code({sql, Code, _Constraint}) -> Code;
error_code(_Other) -> undefined.

%% @doc undefined → `null`（epgsql 的 NULL 参数）。
-spec nullify(term()) -> term().
nullify(undefined) -> null;
nullify(null) -> null;
nullify(Value) -> Value.

%% @doc map / list → jsonb 参数（永不落明文正文/secret）。
%% list（如 allowed_origins）必须编码为 JSON 数组；此前的 catch-all 会把
%% 列表吞成 <<"{}">> 字符串，导致 create 落库后 origin 白名单永不命中
%% （CSX-01 W4 真实 HTTP 验证抓出，2026-09-17）。
-spec jsonb(term()) -> binary().
jsonb(Map) when is_map(Map) ->
    jsone:encode(Map);
jsonb(List) when is_list(List) ->
    jsone:encode(List);
jsonb(Bin) when is_binary(Bin) ->
    Bin;
jsonb(_Other) ->
    <<"{}">>.
