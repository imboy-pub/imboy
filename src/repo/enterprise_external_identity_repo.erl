-module(enterprise_external_identity_repo).

%%%
% enterprise_external_identity_repo 是外部身份映射仓储层（迁移 00000136，
% EPGZ-01 / plan-gz §5）。客户 OA 的 external_user_id <-> IMBoy active Human
% member 的唯一授权来源：sender_user_id 必须命中本表的 active 行。
%
% 表结构：enterprise_external_identity(id TSID PK, organization_id,
%   application_id, external_user_id, user_id, status active|removed, timestamps)；
%   双向唯一 (org, app, external_user_id) / (org, app, user_id)；
%   「active Human member」由复合 FK + trg_..._member_guard 触发器在 DB 层强制。
%%%

-export([
    tablename/0,
    next_id/0,
    bind_tx/5,
    find_by_external_tx/4,
    resolve_tx/4,
    unbind_tx/4,
    unbind_user_tx/4
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, application_id, external_user_id, user_id, status, "
    "created_at, updated_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_external_identity">>).

%% @doc mapping 命名空间 TSID（惰性注册，镜像 agent_grant_pg 口径）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_external_identity, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_external_identity)
    end,
    elib_tsid:generate(enterprise_external_identity).

%% @doc 事务内绑定 external_user_id <-> user（INT-02）。
%% 幂等 upsert：同 (org, app, external_user_id) 已存在即覆盖 user_id 并复活为
%% active（重绑）。目标不满足「同 Org active Human member」被触发器 23514 拒绝，
%% 归一 {error, invalid_member}；反向唯一 (org, app, user_id) 冲突（该 user 已绑
%% 到其他 external_user_id）归一 {error, user_already_mapped}。
-spec bind_tx(any(), integer(), integer(), binary(), integer()) ->
    {ok, map()} | {error, invalid_member | user_already_mapped | term()}.
bind_tx(Conn, OrgId, AppId, ExternalUserId, UserId) when
    is_integer(OrgId), is_integer(AppId), is_binary(ExternalUserId), is_integer(UserId)
->
    Tb = tablename(),
    Id = next_id(),
    Now = elib_dt:now(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, organization_id, application_id, external_user_id, user_id, status,",
            " created_at, updated_at)", " VALUES ($1, $2, $3, $4, $5, 'active', $6, $6)",
            " ON CONFLICT (organization_id, application_id, external_user_id)",
            " DO UPDATE SET user_id = EXCLUDED.user_id, status = 'active',",
            " updated_at = EXCLUDED.updated_at", " RETURNING ", ?COLUMNS/binary>>,
    case elib_pg:query(Conn, Sql, [Id, OrgId, AppId, ExternalUserId, UserId, Now]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, #error{code = <<"23514">>}} ->
            {error, invalid_member};
        {error, #error{code = <<"23505">>}} ->
            {error, user_already_mapped};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按 external_user_id 取映射（任意状态；active 判定在调用方）。
-spec find_by_external_tx(any(), integer(), integer(), binary()) ->
    {ok, map()} | {error, not_found | term()}.
find_by_external_tx(Conn, OrgId, AppId, ExternalUserId) when is_binary(ExternalUserId) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_id = $2",
            " AND external_user_id = $3 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, ExternalUserId]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内批量按 external_user_id 列表解析 active 映射（INT-03）。
%% 只返回 active 行且仅在入参集合内（不支持全量导出）；SQL IN 占位符逐个参数化。
-spec resolve_tx(any(), integer(), integer(), [binary()]) ->
    {ok, [map()]} | {error, term()}.
resolve_tx(_Conn, _OrgId, _AppId, []) ->
    {ok, []};
resolve_tx(Conn, OrgId, AppId, ExternalUserIds) when is_list(ExternalUserIds) ->
    Placeholders = placeholders(ExternalUserIds, 3, <<>>),
    Params = [OrgId, AppId | ExternalUserIds],
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND application_id = $2 AND status = 'active'",
            " AND external_user_id IN (", Placeholders/binary, ")">>,
    elib_pg:query(Conn, Sql, Params).

%% @doc 事务内解除映射（软删；幂等：非 active 返回 {error, not_active}）。
%% 解除后同一 external_user_id 可经 bind_tx 重绑；removed 行保留历史。
-spec unbind_tx(any(), integer(), integer(), binary()) ->
    ok | {error, not_found | not_active | term()}.
unbind_tx(Conn, OrgId, AppId, ExternalUserId) when is_binary(ExternalUserId) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET status = 'removed', updated_at = $1",
            " WHERE organization_id = $2 AND application_id = $3",
            " AND external_user_id = $4 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, OrgId, AppId, ExternalUserId]) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            case find_by_external_tx(Conn, OrgId, AppId, ExternalUserId) of
                {ok, _} -> {error, not_active};
                {error, not_found} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按 user_id 解除映射（成员离场路径；幂等：无 active 行返回
%% {error, not_found}）。
-spec unbind_user_tx(any(), integer(), integer(), integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
unbind_user_tx(Conn, OrgId, AppId, UserId) when is_integer(UserId) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET status = 'removed', updated_at = $1",
            " WHERE organization_id = $2 AND application_id = $3",
            " AND user_id = $4 AND status = 'active'">>,
    case elib_pg:execute(Conn, Sql, [Now, OrgId, AppId, UserId]) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% @doc 生成 $3,$4,... 逐项占位符（$1/$2 是 org/app，external 从 $3 起）。
-spec placeholders([binary()], pos_integer(), binary()) -> binary().
placeholders([], _N, Acc) ->
    Acc;
placeholders([_ | Rest], N, <<>>) ->
    placeholders(Rest, N + 1, <<"$", (integer_to_binary(N))/binary>>);
placeholders([_ | Rest], N, Acc) when is_binary(Acc) ->
    Next = <<Acc/binary, ", $", (integer_to_binary(N))/binary>>,
    placeholders(Rest, N + 1, Next).
