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
    find_by_key_tx/3,
    update_status_tx/4,
    update_name_tx/4,
    update_scopes_tx/4,
    update_redirect_uris_tx/4,
    policy_tx/3,
    update_content_policy_tx/5
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, principal_user_id, application_key, name, status, "
    "allowed_scopes, allowed_redirect_uris, created_at, updated_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

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

%% @doc 事务内按 (organization_id, application_key) 取行。
-spec find_by_key_tx(any(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_key_tx(Conn, OrgId, ApplicationKey) when is_integer(OrgId), is_binary(ApplicationKey) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_key = $2 LIMIT 1">>,
    one_tx(Conn, Sql, [OrgId, ApplicationKey]).

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
