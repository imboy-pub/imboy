-module(enterprise_application_repo).

%%%
% enterprise_application_repo 是企业集成 Application 仓储层（迁移 00000136，
% EPGZ-01 / plan-gz §5）。Application 是 /api/internal/v1/* 的唯一调用主体
% （客户 OA 后端），归 Organization；本层只做数据访问，认证/scope 判定在
% 上层（Handler→Logic→DS→Repo 单向依赖）。
%
% 表结构：enterprise_application(id TSID PK, organization_id, principal_user_id
%   可空, application_key 同 Org 唯一, name, status active|disabled,
%   allowed_scopes jsonb 数组, allowed_redirect_uris text[]（EPGZ-01R：
%   exact redirect URI allowlist，元素 https-only/禁 fragment 由触发器
%   trg_enterprise_application_redirect_guard 守卫，exact match 消费谓词
%   <uri> = ANY(allowed_redirect_uris) 在应用层逐字节执行）, timestamps)；
%   uq_ea_org_id 复合唯一供子表复合 FK。
%%%

-export([
    tablename/0,
    next_id/0,
    create_tx/4,
    create_tx/5,
    create_tx/6,
    find_tx/3,
    lock_tx/3,
    find_by_key_tx/3,
    list_page_tx/5,
    workbench_entries_tx/3,
    update_status_tx/4,
    update_name_tx/4,
    update_scopes_tx/4,
    update_redirect_uris_tx/4,
    update_status_cas_tx/5,
    update_scopes_cas_tx/5,
    policy_tx/3,
    update_content_policy_tx/5
]).

-include_lib("epgsql/include/epgsql.hrl").

%% version 由迁移 00000143 加入（乐观锁；见 update_status_cas_tx / update_scopes_cas_tx）。
-define(COLUMNS, <<
    "id, organization_id, principal_user_id, application_key, name, status, version, "
    "allowed_scopes, allowed_redirect_uris, created_at, updated_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

%% 当前企业的 Human OA 配置；最小投影，无凭证，identity 用 EXISTS 防止重复。
-spec workbench_entries_tx(any(), integer(), integer()) -> {ok, [map()]} | {error, term()}.
workbench_entries_tx(Conn, OrgId, Uid) ->
    elib_pg:query(
        Conn,
        <<
            "SELECT a.id AS application_id, a.organization_id, a.application_key,"
            " a.name, a.allowed_redirect_uris FROM enterprise_application a"
            " JOIN organization o ON o.id = a.organization_id AND o.status = 'active'"
            " JOIN organization_member om ON om.organization_id = o.id"
            " AND om.user_id = $2 AND om.status = 'active'"
            " JOIN \"user\" u ON u.id = om.user_id AND u.status = 1 AND u.account_type = 0"
            " WHERE a.organization_id = $1 AND a.status = 'active'"
            " AND cardinality(a.allowed_redirect_uris) > 0"
            " AND EXISTS (SELECT 1 FROM enterprise_external_identity ei"
            " WHERE ei.organization_id = a.organization_id AND ei.application_id = a.id"
            " AND ei.user_id = $2 AND ei.status = 'active')"
            " ORDER BY a.id LIMIT 20"
        >>,
        [OrgId, Uid]
    ).

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application">>).

%% @doc enterprise_application 命名空间 TSID（惰性注册，镜像 agent_grant_pg 口径）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_application, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_application)
    end,
    elib_tsid:generate(enterprise_application).

%% @doc 事务内创建 Application（无 principal_user_id，allowlist 为空）。
-spec create_tx(any(), integer(), binary(), binary()) ->
    {ok, map()} | {error, key_conflict | term()}.
create_tx(Conn, OrgId, ApplicationKey, Name) ->
    create_tx(Conn, OrgId, ApplicationKey, Name, undefined).

%% @doc 事务内创建 Application（无 redirect allowlist，等价空 allowlist）。
-spec create_tx(
    any(), integer(), binary(), binary(), undefined | {binary(), [binary()] | binary()}
) ->
    {ok, map()} | {error, key_conflict | term()}.
create_tx(Conn, OrgId, ApplicationKey, Name, PrincipalAndScopes) ->
    create_tx(Conn, OrgId, ApplicationKey, Name, PrincipalAndScopes, []).

%% @doc 事务内创建 Application；allowed_scopes 默认 []（scope 授权由运维接口另行下发）。
%% Scopes 是二进制 JSON 数组（如 <<"[\"application:read\"]">>），由上层序列化。
%% Org 内 application_key 撞唯一约束归一 {error, key_conflict}。
%% RedirectUris 是 exact redirect URI allowlist（EPGZ-01R / EPGZ-05）：元素
%% 仅 https、禁 fragment，非法元素由 trg_enterprise_application_redirect_guard
%% 以 23514 拒绝（错误形态 {error, #error{code = <<"23514">>}}）；空表 = 空
%% allowlist（SSO 签发侧 fail-closed 一律拒绝）。参数以 $N::text[] 直传
%% Erlang list（epgsql 按服务端推断的 text[] 类型做数组编码）。
-spec create_tx(
    any(), integer(), binary(), binary(), undefined | {binary(), [binary()] | binary()}, [binary()]
) ->
    {ok, map()} | {error, key_conflict | term()}.
create_tx(Conn, OrgId, ApplicationKey, Name, PrincipalAndScopes, RedirectUris) ->
    {PrincipalUserId, ScopesJson} =
        case PrincipalAndScopes of
            undefined -> {null, <<"[]">>};
            {P, S} when is_list(S) -> {P, scopes_to_json(S)};
            {P, SJ} when is_binary(SJ) -> {P, SJ}
        end,
    Tb = tablename(),
    Id = next_id(),
    Now = elib_dt:now(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, organization_id, principal_user_id, application_key, name, status,",
            " allowed_scopes, allowed_redirect_uris, created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, $5, 'active', $6::jsonb, $7::text[], $8, $8)", " RETURNING ",
            ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [
            Id,
            OrgId,
            PrincipalUserId,
            ApplicationKey,
            Name,
            ScopesJson,
            RedirectUris,
            Now
        ])
    of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, #error{code = <<"23505">>}} ->
            {error, key_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按 (organization_id, id) 取行（Org 边界在 SQL 内强制）。
-spec find_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, Id) when is_integer(OrgId), is_integer(Id) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND id = $2 LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, Id]).

%% 与鉴权共享 Application 行锁；治理变更和审计在同一事务内完成。
-spec lock_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
lock_tx(Conn, OrgId, Id) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND id = $2 FOR UPDATE">>,
    one_tx(Conn, Sql, [OrgId, Id]).

%% @doc 事务内按 (organization_id, application_key) 取行。
-spec find_by_key_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_key_tx(Conn, OrgId, ApplicationKey) when is_integer(OrgId), is_binary(ApplicationKey) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_key = $2 LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, ApplicationKey]).

%% @doc 事务内按组织分页浏览 Application（Admin 治理面 A-01；迁移 00000143）。
%% Org 边界在 SQL 内强制：跨 Org 的行一律不可见，不靠调用方守纪律（IDOR 负例
%% 由 SQL 自身保证，见 test/repo/enterprise_admin_governance_pg_tests.erl）。
%%
%% Opts 键（均可缺省）：
%%   status :: binary() —— 四值生命周期之一；不在集合内 ⇒ {error, invalid_status}
%%             且**不执行任何 SQL**（fail-closed，DB 的 CHECK 只是第二道闸）
%%   q      :: binary() —— name / application_key 的大小写不敏感子串；空二进制等同缺省
%%
%% q 的匹配用 position(lower($N::text) in lower(col)) > 0：**不用 LIKE**——
%% LIKE 会把用户输入里的 % 与 _ 当通配符，既造成语义漂移（"_" 匹配任意字符）
%% 又可被构造为昂贵的全表模式匹配。position 保证始终把输入当字面量。
%%
%% 排序 created_at DESC, id DESC 与索引 i_ea_org_created 同向（EXPLAIN 见索引扫描），
%% id 兜底保证同 created_at 时分页稳定。
-spec list_page_tx(any(), integer(), pos_integer(), pos_integer(), map()) ->
    {ok, #{items := [map()], total := non_neg_integer()}} | {error, invalid_status | term()}.
list_page_tx(Conn, OrgId, Page, Size, Opts) when is_integer(OrgId), is_map(Opts) ->
    Page1 = normalize_page(Page),
    Size1 = normalize_size(Size),
    Status = maps:get(status, Opts, undefined),
    Q = maps:get(q, Opts, undefined),
    case valid_status_filter(Status) of
        false ->
            {error, invalid_status};
        true ->
            {Conds, Params0} = build_list_filters(OrgId, Status, Q),
            LimitIdx = length(Params0) + 1,
            OffsetIdx = LimitIdx + 1,
            LimitB = integer_to_binary(LimitIdx),
            OffsetB = integer_to_binary(OffsetIdx),
            WhereBin = join_conds(Conds),
            Sql =
                <<"SELECT ", ?COLUMNS/binary, ", COUNT(*) OVER() AS total_count FROM ",
                    (tablename())/binary, " WHERE ", WhereBin/binary,
                    " ORDER BY created_at DESC, id DESC LIMIT $", LimitB/binary, " OFFSET $",
                    OffsetB/binary>>,
            Params = Params0 ++ [Size1, (Page1 - 1) * Size1],
            case elib_pg:query(Conn, Sql, Params) of
                {ok, Rows} when is_list(Rows) ->
                    Total =
                        case Rows of
                            [First | _] -> to_int(maps:get(<<"total_count">>, First, 0));
                            [] -> 0
                        end,
                    {ok, #{
                        items => [maps:remove(<<"total_count">>, R) || R <- Rows], total => Total
                    }};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 事务内 CAS 替换生命周期（Admin 治理面 A-03；迁移 00000143 的 version 列）。
%% 并发下只有 expected_version 命中者生效（version+1）；其余 {error, version_conflict}。
%% status 值域由 DB ck_ea_status 兜底（非法值 23514 归一 invalid_status），本层不重复
%% 维护枚举副本（避免两处漂移）。
-spec update_status_cas_tx(any(), integer(), integer(), pos_integer(), binary()) ->
    ok | {error, invalid_status | not_found | version_conflict | term()}.
update_status_cas_tx(Conn, OrgId, Id, ExpectedVersion, Status) when
    is_integer(OrgId), is_integer(Id), is_integer(ExpectedVersion), is_binary(Status)
->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = $4, version = version + 1, updated_at = $5",
            " WHERE organization_id = $1 AND id = $2 AND version = $3">>,
    case elib_pg:execute(Conn, Sql, [OrgId, Id, ExpectedVersion, Status, Now]) of
        {ok, 1} -> ok;
        {ok, 0} -> cas_miss(Conn, OrgId, Id);
        {error, #error{code = <<"23514">>}} -> {error, invalid_status};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内 CAS 整体替换 allowed_scopes（Admin 治理面 A-04）。
%% 与 update_status_cas_tx 同口径的 CAS；scope 值域校验在 logic 层
%% （enterprise_internal_ops:validate_scopes/1）——Repo 不反向依赖 api 层枚举模块。
-spec update_scopes_cas_tx(any(), integer(), integer(), pos_integer(), [binary()] | binary()) ->
    ok | {error, not_found | version_conflict | term()}.
update_scopes_cas_tx(Conn, OrgId, Id, ExpectedVersion, Scopes) ->
    ScopesJson =
        case Scopes of
            L when is_list(L) -> scopes_to_json(L);
            J when is_binary(J) -> J
        end,
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET allowed_scopes = $4::jsonb, version = version + 1, updated_at = $5",
            " WHERE organization_id = $1 AND id = $2 AND version = $3">>,
    case elib_pg:execute(Conn, Sql, [OrgId, Id, ExpectedVersion, ScopesJson, Now]) of
        {ok, 1} -> ok;
        {ok, 0} -> cas_miss(Conn, OrgId, Id);
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内启停 Application（active|disabled；disabled 即拒绝全部 internal API）。
-spec update_status_tx(any(), integer(), integer(), binary()) -> ok | {error, not_found | term()}.
update_status_tx(Conn, OrgId, Id, Status) when Status =:= <<"active">>; Status =:= <<"disabled">> ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = $1, updated_at = $2 WHERE organization_id = $3 AND id = $4">>,
    case elib_pg:execute(Conn, Sql, [Status, Now, OrgId, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内改名。
-spec update_name_tx(any(), integer(), integer(), binary()) -> ok | {error, not_found | term()}.
update_name_tx(Conn, OrgId, Id, Name) when is_binary(Name), Name =/= <<>> ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET name = $1, updated_at = $2 WHERE organization_id = $3 AND id = $4">>,
    case elib_pg:execute(Conn, Sql, [Name, Now, OrgId, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内整体替换 allowed_scopes（固定 scopes 白名单成员校验在上层）。
-spec update_scopes_tx(any(), integer(), integer(), [binary()] | binary()) ->
    ok | {error, not_found | term()}.
update_scopes_tx(Conn, OrgId, Id, Scopes) ->
    ScopesJson =
        case Scopes of
            L when is_list(L) -> scopes_to_json(L);
            J when is_binary(J) -> J
        end,
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET allowed_scopes = $1::jsonb, updated_at = $2 WHERE organization_id = $3 AND id = $4">>,
    case elib_pg:execute(Conn, Sql, [ScopesJson, Now, OrgId, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内整体替换 redirect URI allowlist（EPGZ-01R）。空表 = 清空
%% allowlist（SSO 签发侧 fail-closed 一律拒绝）；元素校验同 create_tx/6
%%（触发器 23514 拒绝非法元素）；参数直传 list（同 create_tx/6 说明）。
-spec update_redirect_uris_tx(any(), integer(), integer(), [binary()]) ->
    ok | {error, not_found | term()}.
update_redirect_uris_tx(Conn, OrgId, Id, RedirectUris) when is_list(RedirectUris) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET allowed_redirect_uris = $1::text[], updated_at = $2",
            " WHERE organization_id = $3 AND id = $4">>,
    case elib_pg:execute(Conn, Sql, [RedirectUris, Now, OrgId, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内读**内容策略**（FULL-02 / plan-full §3.1「企业附件 … 内容策略」）：
%% allowed_mime_types（空数组 = 沿用全局白名单）+ max_file_size_bytes
%% （NULL = 沿用全局上限）。只读两列，不影响本模块其余读面的列集。
-spec policy_tx(any(), integer(), integer()) ->
    {ok, #{allowed_mime_types := [binary()], max_file_size_bytes := undefined | integer()}}
    | {error, not_found | term()}.
policy_tx(Conn, OrgId, Id) when is_integer(OrgId), is_integer(Id) ->
    Sql =
        <<"SELECT allowed_mime_types, max_file_size_bytes FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, Id]) of
        {ok, [Row | _]} ->
            Mimes =
                case maps:get(<<"allowed_mime_types">>, Row, []) of
                    L when is_list(L) -> [M || M <- L, is_binary(M)];
                    _ -> []
                end,
            Max =
                case maps:get(<<"max_file_size_bytes">>, Row, null) of
                    N when is_integer(N), N > 0 -> N;
                    _ -> undefined
                end,
            {ok, #{allowed_mime_types => Mimes, max_file_size_bytes => Max}};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内替换内容策略（元素级校验由 migration 00000140 的触发器 23514
%% 承担；非法元素归一 {error, invalid_policy}）。Mimes 为空表 = 清空 allowlist
%% （回到全局白名单）；MaxBytes 为 undefined/null 表示不限（沿用全局上限）。
-spec update_content_policy_tx(any(), integer(), integer(), [binary()], undefined | integer()) ->
    ok | {error, invalid_policy | not_found | term()}.
update_content_policy_tx(Conn, OrgId, Id, Mimes, MaxBytes) when is_list(Mimes) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET allowed_mime_types = $1::text[], max_file_size_bytes = $2, updated_at = $3",
            " WHERE organization_id = $4 AND id = $5">>,
    case elib_pg:execute(Conn, Sql, [Mimes, MaxBytes, Now, OrgId, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, #error{code = <<"23514">>}} -> {error, invalid_policy};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生命周期过滤值是否合法（四值；undefined 表示不过滤）。
%% 与 imboyadmin contracts.ts:APPLICATION_STATUSES 一致；DB ck_ea_status 是第二道闸。
-spec valid_status_filter(undefined | binary()) -> boolean().
valid_status_filter(undefined) ->
    true;
valid_status_filter(S) when
    S =:= <<"draft">>; S =:= <<"active">>; S =:= <<"disabled">>; S =:= <<"archived">>
->
    true;
valid_status_filter(_) ->
    false.

-spec normalize_page(term()) -> pos_integer().
normalize_page(P) when is_integer(P), P > 0 -> P;
normalize_page(_) -> 1.

-spec normalize_size(term()) -> pos_integer().
normalize_size(S) when is_integer(S), S > 0 -> S;
normalize_size(_) -> 10.

%% @doc 逐条拼 WHERE 条件，同时按最终位置生成 $N 占位符（避免手写错位）。
-spec build_list_filters(integer(), undefined | binary(), undefined | binary()) ->
    {[binary()], [term()]}.
build_list_filters(OrgId, Status, Q) ->
    C1 = <<"organization_id = $1">>,
    {Conds, Params} =
        case Status of
            undefined -> {[C1], [OrgId]};
            S when is_binary(S) -> {[C1, <<"status = $2">>], [OrgId, S]}
        end,
    case Q of
        Qq when is_binary(Qq), Qq =/= <<>> ->
            IdxB = integer_to_binary(length(Params) + 1),
            Frag =
                <<"(position(lower($", IdxB/binary,
                    "::text) in lower(name)) > 0"
                    " OR position(lower($", IdxB/binary,
                    "::text) in lower(application_key::text)) > 0)">>,
            {Conds ++ [Frag], Params ++ [Qq]};
        _ ->
            {Conds, Params}
    end.

-spec join_conds([binary()]) -> binary().
join_conds(Conds) ->
    binary:join(Conds, <<" AND ">>).

-spec to_int(term()) -> non_neg_integer().
to_int(N) when is_integer(N), N >= 0 -> N;
to_int(_) -> 0.

%% @doc CAS 未命中时区分「行不存在」与「版本已被他人推进」。
-spec cas_miss(any(), integer(), integer()) ->
    {error, not_found | version_conflict | term()}.
cas_miss(Conn, OrgId, Id) ->
    case find_tx(Conn, OrgId, Id) of
        {ok, _Row} -> {error, version_conflict};
        {error, not_found} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec scopes_to_json([binary()]) -> binary().
scopes_to_json(Scopes) when is_list(Scopes) ->
    jsone:encode(Scopes).

-spec one_tx(any(), binary(), list()) -> {ok, map()} | {error, not_found | term()}.
one_tx(Conn, Sql, Params) ->
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.
