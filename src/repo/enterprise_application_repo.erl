-module(enterprise_application_repo).

%%%
% enterprise_application_repo 是企业集成 Application 仓储层（迁移 00000136，
% EPGZ-01 / plan-gz §5）。Application 是 /api/internal/v1/* 的唯一调用主体
% （客户 OA 后端），归 Organization；本层只做数据访问，认证/scope 判定在
% 上层（Handler→Logic→DS→Repo 单向依赖）。
%
% 表结构：enterprise_application(id TSID PK, organization_id, principal_user_id
%   可空, application_key 同 Org 唯一, name, status active|disabled,
%   allowed_scopes jsonb 数组, timestamps)；uq_ea_org_id 复合唯一供子表复合 FK。
%%%

-export([
    tablename/0,
    next_id/0,
    create_tx/4,
    create_tx/5,
    find_tx/3,
    find_by_key_tx/3,
    update_status_tx/4,
    update_name_tx/4,
    update_scopes_tx/4
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, principal_user_id, application_key, name, status, "
    "allowed_scopes, created_at, updated_at"
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

%% @doc 事务内创建 Application（无 principal_user_id）。
-spec create_tx(any(), integer(), binary(), binary()) ->
    {ok, map()} | {error, key_conflict | term()}.
create_tx(Conn, OrgId, ApplicationKey, Name) ->
    create_tx(Conn, OrgId, ApplicationKey, Name, undefined).

%% @doc 事务内创建 Application；allowed_scopes 默认 []（scope 授权由运维接口另行下发）。
%% Scopes 是二进制 JSON 数组（如 <<"[\"application:read\"]">>），由上层序列化。
%% Org 内 application_key 撞唯一约束归一 {error, key_conflict}。
-spec create_tx(any(), integer(), binary(), binary(), undefined | {binary(), [binary()]}) ->
    {ok, map()} | {error, key_conflict | term()}.
create_tx(Conn, OrgId, ApplicationKey, Name, PrincipalAndScopes) ->
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
            " allowed_scopes, created_at, updated_at)",
            " VALUES ($1, $2, $3, $4, $5, 'active', $6::jsonb, $7, $7)", " RETURNING ",
            ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [Id, OrgId, PrincipalUserId, ApplicationKey, Name, ScopesJson, Now])
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
