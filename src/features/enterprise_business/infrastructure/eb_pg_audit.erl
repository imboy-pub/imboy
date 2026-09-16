%%% @doc append-only 审计写入的 PG 实现（`eb_audit_port` 的真实现）。
%%%
%%% 依据：plan v4.1 EB-D05、§4.1（enterprise_audit_event）、§2.1 #18、EB-03 §3.1。
%%%
%%% 契约要点（EB-02 冻结）：
%%%   * 只有 `append/2`，没有 update / delete / rewrite —— 审计事实一旦写入不可变更；
%%%   * 首个业务参数是 `organization_id`（审计同样 tenant-scoped）；
%%%   * `detail` 不得含 secret / 密文 / 明文 / Authorization。本模块只接受调用方
%%%     给的 jsonb map，且**不落任何原始正文**：调用方（canonical 事务）只放
%%%     结构化字段（sender_type / policy 版本 / retain_until / aad_hash 等摘要）。
%%%   * 服务端「接受」语义要求 canonical message + 审计同事务提交，故本模块提供
%%%     `append_in/3`（显式连接，纳入调用方事务）；`append/2` 走连接池独立事务。
%%%
%%% 实现细节：`enterprise_audit_event` 的 append-only 由 DB 触发器强制
%%% （`trg_enterprise_audit_event_append_only`，UPDATE/DELETE 一律 23514），
%%% 实现层因此只允许 INSERT。
-module(eb_pg_audit).

-behaviour(eb_audit_port).

-include_lib("epgsql/include/epgsql.hrl").

-export([append/2, append_in/3, sql_statements/0]).

%% 所有语句都带 organization_id（审计表是 Org 级事实，无 workspace 列）。
-define(SQL_INSERT_AUDIT, <<
    "INSERT INTO enterprise_audit_event"
    " (id, organization_id, resource_type, resource_id, action, business_identity_id,"
    "  actor_user_id, actor_role, detail)"
    " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)"
    " RETURNING id"
>>).

%% @doc 独立事务追加一条审计事实（组织级租户参数显式传入）。
-spec append(integer(), map()) -> {ok, integer()} | {error, term()}.
append(OrgId, Event) when is_integer(OrgId), is_map(Event) ->
    case elib_pg:execute(?SQL_INSERT_AUDIT, audit_params(OrgId, Event)) of
        {ok, 1, Rows} -> {ok, first_id(Rows)};
        {ok, 0, _Rows} -> {error, audit_not_inserted};
        {error, Reason} -> {error, normalize_error(Reason)}
    end;
append(OrgId, _Event) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc 在调用方事务内追加审计（canonical message + policy snapshot + audit 原子提交）。
-spec append_in(term(), integer(), map()) -> {ok, integer()} | {error, term()}.
append_in(Conn, OrgId, Event) when is_integer(OrgId), is_map(Event) ->
    case elib_pg:execute(Conn, ?SQL_INSERT_AUDIT, audit_params(OrgId, Event)) of
        {ok, 1, Rows} -> {ok, first_id(Rows)};
        {ok, 0, _Rows} -> {error, audit_not_inserted};
        {error, Reason} -> {error, normalize_error(Reason)}
    end;
append_in(_Conn, OrgId, _Event) ->
    {error, {invalid_organization_id, OrgId}}.

%% @doc 冻结的语句列表（供静态租户/形态断言）。
-spec sql_statements() -> [binary()].
sql_statements() ->
    [?SQL_INSERT_AUDIT].

%% ===================================================================
%% 参数 / 结果
%% ===================================================================

audit_params(OrgId, Event) ->
    [
        maps:get(id, Event, eb_tsid:new_id(audit)),
        OrgId,
        maps:get(resource_type, Event, <<"enterprise">>),
        nullify(maps:get(resource_id, Event, undefined)),
        maps:get(action, Event, <<"unknown">>),
        nullify(maps:get(business_identity_id, Event, undefined)),
        nullify(maps:get(actor_user_id, Event, undefined)),
        nullify(maps:get(actor_role, Event, undefined)),
        jsonb(maps:get(detail, Event, #{}))
    ].

jsonb(Map) when is_map(Map) ->
    jsone:encode(Map);
jsonb(Bin) when is_binary(Bin) ->
    Bin;
jsonb(_Other) ->
    <<"{}">>.

nullify(undefined) -> null;
nullify(null) -> null;
nullify(Value) -> Value.

first_id([{Id} | _]) -> Id;
first_id([Id | _]) when is_integer(Id) -> Id;
first_id([]) -> undefined.

normalize_error(Reason) ->
    case Reason of
        #error{} = Err -> {sql, Err#error.code, error_constraint(Err#error.extra)};
        Other -> {db, Other}
    end.

error_constraint(Extra) when is_list(Extra) ->
    case lists:keyfind(constraint_name, 1, Extra) of
        {constraint_name, Name} -> Name;
        false -> undefined
    end;
error_constraint(_Other) ->
    undefined.
