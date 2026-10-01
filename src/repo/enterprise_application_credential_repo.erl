-module(enterprise_application_credential_repo).

%%%
% enterprise_application_credential_repo 是 Application 凭证仓储层（迁移
% 00000136，EPGZ-01 / plan-gz §4.1）。凭证形态 ib_int_<credential_id>.<secret>：
% DB 只存 secret 的 SHA-256 hex digest 与全局唯一 credential_prefix（定位键），
% 明文不落库；constant-time 比对在认证层（EPGZ-02），本层只提供数据访问。
%
% 表结构：enterprise_application_credential(id TSID PK, organization_id,
%   application_id, credential_prefix 全局唯一, secret_digest 64hex,
%   status active|revoked, expires_at 可空, last_used_at, revoked_at, timestamps)。
%%%

-export([
    tablename/0,
    next_id/0,
    digest_hex/1,
    create_tx/5,
    create_tx/6,
    find_by_prefix/1,
    find_by_prefix_tx/2,
    find_active_by_prefix/1,
    find_active_by_prefix_tx/2,
    revoke_tx/3,
    touch_last_used_tx/2,
    authority_for_share_tx/4,
    find_tx/3,
    lock_tx/3,
    expired_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, application_id, credential_prefix, secret_digest, "
    "status, expires_at, last_used_at, created_at, revoked_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_credential">>).

%% @doc credential 命名空间 TSID（惰性注册，镜像 agent_grant_pg 口径）。
-spec next_id() -> pos_integer().
next_id() ->
    case lists:member(enterprise_application_credential, elib_tsid:registered()) of
        true -> ok;
        false -> elib_tsid:register(enterprise_application_credential)
    end,
    elib_tsid:generate(enterprise_application_credential).

%% @doc secret 的 SHA-256 小写 hex（64 字符；镜像 bot_repo:digest_hex/1）。
%% 入参是凭证的 secret 部分（不含 ib_int_ 前缀与 credential_id）。
-spec digest_hex(binary()) -> binary().
digest_hex(Secret) when is_binary(Secret) ->
    binary:encode_hex(crypto:hash(sha256, Secret), lowercase).

%% @doc 事务内创建凭证（无过期时间）。CredentialPrefix 是认证定位键
%% （如 <<"ib_int_", Id/binary>>），Secret 明文只在此前的生成阶段出现一次。
-spec create_tx(any(), integer(), integer(), binary(), binary()) ->
    {ok, map()} | {error, prefix_conflict | invalid_secret | term()}.
create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret) ->
    create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret, undefined).

%% @doc 事务内创建凭证；ExpiresAt 为二进制 RFC3339 或 undefined（永不过期）。
%% digest 计算失败（空 secret）前置拒绝，不产生任何 DB 写入。
%% credential_prefix 撞全局唯一归一 {error, prefix_conflict}。
-spec create_tx(any(), integer(), integer(), binary(), binary(), undefined | binary()) ->
    {ok, map()} | {error, prefix_conflict | invalid_secret | term()}.
create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret, ExpiresAt) when Secret =/= <<>> ->
    case digest_hex(Secret) of
        Digest when byte_size(Digest) =:= 64 ->
            create_tx1(Conn, OrgId, AppId, CredentialPrefix, Digest, ExpiresAt);
        _ ->
            {error, invalid_secret}
    end;
create_tx(_Conn, _OrgId, _AppId, _CredentialPrefix, _Secret, _ExpiresAt) ->
    %% 空 secret 不做 digest（空串的 SHA-256 仍是 64 字符，长度守卫拦不住）
    {error, invalid_secret}.

create_tx1(Conn, OrgId, AppId, CredentialPrefix, Digest, ExpiresAt) ->
    Tb = tablename(),
    Id = next_id(),
    Now = elib_dt:now(),
    ExpiresParam =
        case ExpiresAt of
            undefined -> null;
            Bin when is_binary(Bin) -> Bin
        end,
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, organization_id, application_id, credential_prefix, secret_digest,",
            " status, expires_at, created_at)",
            " VALUES ($1, $2, $3, $4, $5, 'active', $6::timestamptz, $7)", " RETURNING ",
            ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [Id, OrgId, AppId, CredentialPrefix, Digest, ExpiresParam, Now])
    of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, insert_empty_result};
        {error, #error{code = <<"23505">>}} ->
            {error, prefix_conflict};
        {error, Reason} ->
            {error, Reason}
    end.

%% 已认证上下文的事务内事实；锁序为组织、应用、凭证，锁持有到提交。
authority_for_share_tx(Conn, OrgId, AppId, CredId) ->
    Sql = <<
        "SELECT o.status AS organization_status, a.status AS application_status,"
        " c.status AS credential_status FROM organization o"
        " JOIN enterprise_application a ON a.organization_id=o.id"
        " JOIN enterprise_application_credential c ON c.organization_id=o.id AND c.application_id=a.id"
        " WHERE o.id=$1 AND a.id=$2 AND c.id=$3 FOR SHARE OF o, a, c"
    >>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, CredId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% 必须在取得权限行锁后调用，避免等锁前的投影/事务开始时间判断过期。
expired_tx(Conn, CredId) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT COALESCE(expires_at<=clock_timestamp(), false) AS expired FROM enterprise_application_credential WHERE id=$1">>,
            [CredId]
        )
    of
        {ok, [#{<<"expired">> := Expired}]} -> {ok, Expired};
        _ -> {error, expiry_read_failed}
    end.

%% @doc 按 prefix 全局定位凭证行（任意状态；active/expiry 判定在认证层）。
-spec find_by_prefix(binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_prefix(CredentialPrefix) when is_binary(CredentialPrefix), CredentialPrefix =/= <<>> ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE credential_prefix = $1 LIMIT 1">>,
    case elib_pg:query(Sql, [CredentialPrefix]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按 prefix 全局定位凭证行。
-spec find_by_prefix_tx(any(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_by_prefix_tx(Conn, CredentialPrefix) when
    is_binary(CredentialPrefix), CredentialPrefix =/= <<>>
->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE credential_prefix = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [CredentialPrefix]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 按 prefix 定位 active 且未过期的凭证行（含同行计算的 expired 布尔，
%% 避免在 Erlang 侧解析时间格式；过期与否最终判定仍在认证层做 constant-time
%% digest 比对之前完成）。
-spec find_active_by_prefix(binary()) -> {ok, map()} | {error, not_found | term()}.
find_active_by_prefix(CredentialPrefix) when
    is_binary(CredentialPrefix), CredentialPrefix =/= <<>>
->
    Sql =
        <<"SELECT ", ?COLUMNS/binary,
            ", COALESCE(expires_at < CURRENT_TIMESTAMP, false) AS expired", " FROM ",
            (tablename())/binary, " WHERE credential_prefix = $1 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Sql, [CredentialPrefix]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内按 prefix 定位 active 且未过期的凭证行（与 find_active_by_prefix/1
%% 同 SQL；供直连/事务上下文复用）。
-spec find_active_by_prefix_tx(any(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_active_by_prefix_tx(Conn, CredentialPrefix) when
    is_binary(CredentialPrefix), CredentialPrefix =/= <<>>
->
    Sql =
        <<"SELECT ", ?COLUMNS/binary,
            ", COALESCE(expires_at < CURRENT_TIMESTAMP, false) AS expired", " FROM ",
            (tablename())/binary, " WHERE credential_prefix = $1 AND status = 'active' LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [CredentialPrefix]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内吊销凭证（幂等：非 active 返回 {error, not_active}）。
-spec revoke_tx(any(), integer(), integer()) -> ok | {error, not_found | not_active | term()}.
revoke_tx(Conn, OrgId, Id) ->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET status = 'revoked', revoked_at = $1::timestamptz",
            " WHERE organization_id = $2 AND id = $3 AND status = 'active'", " RETURNING id">>,
    case elib_pg:query(Conn, Sql, [Now, OrgId, Id]) of
        {ok, [_Row | _]} ->
            ok;
        {ok, []} ->
            case find_tx(Conn, OrgId, Id) of
                {ok, _} -> {error, not_active};
                {error, not_found} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内记录最近认证时间（认证成功路径；不改变凭证生命周期）。
-spec touch_last_used_tx(any(), integer()) -> ok | {error, not_found | term()}.
touch_last_used_tx(Conn, Id) ->
    Now = elib_dt:now(),
    %% 使用时间只是尽力记录；权限锁/撤销持锁时跳过，不阻塞认证。
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET last_used_at = $1::timestamptz WHERE id IN (SELECT id FROM ",
            (tablename())/binary,
            " WHERE id = $2 AND status = 'active' FOR NO KEY UPDATE SKIP LOCKED)">>,
    case elib_pg:execute(Conn, Sql, [Now, Id]) of
        {ok, 1} -> ok;
        {ok, 0} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal Functions
%% ===================================================================

%% 调用方先锁 Application，再锁 credential，避免轮换并发及逆序死锁。
-spec lock_tx(any(), integer(), integer()) -> {ok, map()} | {error, term()}.
lock_tx(Conn, OrgId, Id) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND id = $2 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [OrgId, Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

-spec find_tx(any(), integer(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, Id) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id = $1 AND id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, Id]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.
