-module(enterprise_audit_event_repo).

%%%
% enterprise_audit_event_repo 是企业审计事件的**只读**面（表由迁移 00000119
% 建立，append-only；写入唯一入口是 features/enterprise_business/infrastructure/
% eb_pg_audit.erl 的 append/append_in）。
%
% 本模块为 Admin 治理面（FULL-08 / A-14）提供按资源维度的时间倒序分页查询。
% 只做数据访问，**不做**任何投影到 Admin 响应形状的工作（那是
% enterprise_admin_governance_logic 的事）。
%
% Org 边界在 SQL 内强制：调用方给错 organization_id 只会得到空列表，
% 看不到别人的审计（IDOR 由 SQL 自身保证，不靠调用方守纪律）。
%%%

-export([tablename/0, list_tx/5, next_id/0, append_tx/3, insert_sql/0]).

%% Admin 治理面（FULL-08）是 enterprise_audit_event 的**第二个写入者**。
%%
%% 为什么不复用 features/enterprise_business/infrastructure/eb_pg_audit：该模块
%% 属 enterprise_business 特性切片，会随特性裁剪（ERLC_EXCLUDE）不参与编译
%% （src/imboy_app.erl 里跨裁剪调用必须用 -ifdef 保护，正是这个坑）。
%% src/adm|src/logic 是常驻编译的存量层，无条件调用特性模块会在未选该特性的
%% 档位直接编译失败。故本模块自持一份 INSERT。
%%
%% 防漂移：两条写入路径必须是同一张表、同一组列。`insert_sql/0` 供测试与
%% eb_pg_audit:sql_statements/0 做**机械比对**（见
%% test/repo/enterprise_audit_event_repo_tests.erl）：列集合一旦分叉即红灯。
-define(SQL_INSERT_AUDIT, <<
    "INSERT INTO enterprise_audit_event"
    " (id, organization_id, resource_type, resource_id, action, business_identity_id,"
    "  actor_user_id, actor_role, detail)"
    " VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9::jsonb)"
    " RETURNING id"
>>).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, resource_type, resource_id, action, business_identity_id,"
    " actor_user_id, actor_role, detail, created_at"
>>).

%% 读面硬上限（无界导出负例：size 与 page 都被夹紧，不给「一次拉全量」的路径）。
-define(MAX_PAGE_SIZE, 100).
-define(MAX_PAGE, 1000).

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_audit_event">>).

%% @doc 事务内按资源维度分页读取审计事件（created_at DESC, id DESC；id 兜底保证
%% 同一时刻的事件顺序稳定，避免分页抖动）。
%% Opts 键（均可缺省）：page :: integer()、size :: integer()。
%% 越界值被夹紧而不是报错（读面 fail-soft；但**绝不**放宽上限）。
-spec list_tx(any(), integer(), binary(), integer() | undefined, map()) ->
    {ok, [map()]} | {error, term()}.
list_tx(Conn, OrgId, ResourceType, ResourceId, Opts) when
    is_integer(OrgId), is_binary(ResourceType), ResourceType =/= <<>>, is_map(Opts)
->
    Page = clamp_page(maps:get(page, Opts, 1)),
    Size = clamp_size(maps:get(size, Opts, 20)),
    Tb = tablename(),
    {Where, Params} =
        case ResourceId of
            RId when is_integer(RId) ->
                {<<"organization_id = $1 AND resource_type = $2 AND resource_id = $3">>, [
                    OrgId, ResourceType, RId
                ]};
            _ ->
                {<<"organization_id = $1 AND resource_type = $2">>, [OrgId, ResourceType]}
        end,
    LimitB = integer_to_binary(length(Params) + 1),
    OffsetB = integer_to_binary(length(Params) + 2),
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", Tb/binary, " WHERE ", Where/binary,
            " ORDER BY created_at DESC, id DESC LIMIT $", LimitB/binary, " OFFSET $",
            OffsetB/binary>>,
    case elib_pg:query(Conn, Sql, Params ++ [Size, (Page - 1) * Size]) of
        {ok, Rows} when is_list(Rows) -> {ok, Rows};
        {error, Reason} -> {error, Reason}
    end.

%% @doc enterprise_audit_event 命名空间 TSID（惰性注册，口径同
%% enterprise_application_repo:next_id/0）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_audit_event, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_audit_event)
    end,
    elib_tsid:generate(enterprise_audit_event).

%% @doc 冻结的 INSERT 语句（供与 eb_pg_audit:sql_statements/0 做机械比对，防止
%% 两条写入路径的列集合分叉）。
-spec insert_sql() -> binary().
insert_sql() ->
    ?SQL_INSERT_AUDIT.

%% @doc 事务内追加一条审计事实（append-only 表；UPDATE/DELETE 由
%% trg_enterprise_audit_event_append_only 拒绝）。
%% Event 键：resource_type（必填非空）、action（必填非空）、resource_id、
%% business_identity_id、actor_user_id、actor_role、detail（map 或 binary JSON）。
%% 缺省值口径与 eb_pg_audit:audit_params/2 保持一致。
-spec append_tx(any(), integer(), map()) -> {ok, integer()} | {error, term()}.
append_tx(Conn, OrgId, Event) when is_integer(OrgId), is_map(Event) ->
    Params = [
        maps:get(id, Event, next_id()),
        OrgId,
        maps:get(resource_type, Event, <<"enterprise">>),
        nullify(maps:get(resource_id, Event, undefined)),
        maps:get(action, Event, <<"unknown">>),
        nullify(maps:get(business_identity_id, Event, undefined)),
        nullify(maps:get(actor_user_id, Event, undefined)),
        nullify(maps:get(actor_role, Event, undefined)),
        jsonb(maps:get(detail, Event, #{}))
    ],
    case elib_pg:query(Conn, insert_sql(), Params) of
        {ok, [Row | _]} ->
            {ok, first_id(Row)};
        {ok, []} ->
            {error, audit_not_inserted};
        {error, Reason} ->
            {error, Reason}
    end;
append_tx(_Conn, OrgId, _Event) ->
    {error, {invalid_organization_id, OrgId}}.

%% ===================================================================
%% Internal
%% ===================================================================

-spec nullify(term()) -> term().
nullify(undefined) -> null;
nullify(null) -> null;
nullify(Value) -> Value.

-spec jsonb(term()) -> binary().
jsonb(Map) when is_map(Map) -> jsone:encode(Map);
jsonb(Bin) when is_binary(Bin) -> Bin;
jsonb(_Other) -> <<"{}">>.

%% RETURNING id 的行形态在不同 codec 下可能是 #{<<"id">> := Id} 或 {Id}。
-spec first_id(term()) -> integer() | undefined.
first_id(#{<<"id">> := Id}) when is_integer(Id) -> Id;
first_id({Id}) when is_integer(Id) -> Id;
first_id(Id) when is_integer(Id) -> Id;
first_id(_) -> undefined.

-spec clamp_page(term()) -> pos_integer().
clamp_page(P) when is_integer(P), P >= 1, P =< ?MAX_PAGE -> P;
clamp_page(_) -> 1.

-spec clamp_size(term()) -> pos_integer().
clamp_size(S) when is_integer(S), S >= 1, S =< ?MAX_PAGE_SIZE -> S;
clamp_size(_) -> 20.
