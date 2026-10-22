-module(enterprise_application_credential_repo).

-moduledoc "企业 Application 凭证仓储层。".

-export([
    tablename/0,
    next_id/0,
    signing_secret/1,
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
    find_by_id_tx/2,
    lock_tx/3,
    expired_tx/2
]).

-include_lib("epgsql/include/epgsql.hrl").

-define(COLUMNS, <<
    "id, organization_id, application_id, credential_prefix, "
    "status, expires_at, last_used_at, created_at, revoked_at, signing_secret_enc"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_application_credential">>).

-spec next_id() -> pos_integer().
next_id() ->
    elib_tsid:generate(enterprise_application_credential).

%% @doc secret 的 SHA-256 小写 hex（64 字符；镜像 bot_repo:digest_hex/1）。
%% 入参是凭证的 secret 部分（不含 ib_int_ 前缀与 credential_id）。

%% @doc 事务内创建凭证（无过期时间）。CredentialPrefix 是认证定位键
%% （如 <<"ib_int_", Id/binary>>），Secret 明文只在此前的生成阶段出现一次。
-spec create_tx(any(), integer() | null, integer(), binary(), binary()) ->
    {ok, map()} | {error, prefix_conflict | invalid_secret | term()}.
create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret) ->
    create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret, undefined).

%% @doc 事务内创建凭证；ExpiresAt 为二进制 RFC3339 或 undefined（永不过期）。
%% digest 计算失败（空 secret）前置拒绝，不产生任何 DB 写入。
%% credential_prefix 撞全局唯一归一 {error, prefix_conflict}。
-spec create_tx(any(), integer() | null, integer(), binary(), binary(), undefined | binary()) ->
    {ok, map()} | {error, prefix_conflict | invalid_secret | term()}.
create_tx(Conn, OrgId, AppId, CredentialPrefix, Secret, ExpiresAt) when Secret =/= <<>> ->
    case encrypt_signing_secret(Secret) of
        {ok, Encrypted} ->
            create_tx1(
                Conn, OrgId, AppId, CredentialPrefix, Encrypted, ExpiresAt
            );
        {error, _} = Error ->
            Error
    end;
create_tx(_Conn, _OrgId, _AppId, _CredentialPrefix, _Secret, _ExpiresAt) ->
    %% 空 secret 不做 digest（空串的 SHA-256 仍是 64 字符，长度守卫拦不住）
    {error, invalid_secret}.

create_tx1(Conn, OrgId, AppId, CredentialPrefix, Encrypted, ExpiresAt) ->
    Tb = tablename(),
    Id = next_id(),
    Now = elib_dt:now(),
    ExpiresParam =
        case ExpiresAt of
            undefined -> null;
            Bin when is_binary(Bin) -> Bin
        end,
    Sql =
        <<"INSERT INTO ", Tb/binary, " (id, organization_id, application_id, credential_prefix,",
            " status, expires_at, created_at, signing_secret_enc)",
            " VALUES ($1, $2, $3, $4, 'active', $5::timestamptz, $6, $7)", " RETURNING ",
            ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [
            Id, OrgId, AppId, CredentialPrefix, ExpiresParam, Now, Encrypted
        ])
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
authority_for_share_tx(Conn, null, AppId, CredId) ->
    Sql = <<
        "SELECT a.status AS application_status, c.status AS credential_status "
        "FROM enterprise_application a JOIN enterprise_application_credential c "
        "ON c.application_id=a.id WHERE a.organization_id IS NULL "
        "AND c.organization_id IS NULL AND a.id=$1 AND c.id=$2 FOR SHARE OF a,c"
    >>,
    case elib_pg:query(Conn, Sql, [AppId, CredId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
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
            <<
                "SELECT COALESCE(expires_at<=clock_timestamp(),"
                " false) AS expired FROM enterprise_application_credential WHERE id=$1"
            >>,
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
-spec revoke_tx(any(), integer() | null, integer()) ->
    ok | {error, not_found | not_active | term()}.
revoke_tx(Conn, OrgId, Id) when
    (is_integer(OrgId) orelse OrgId =:= null), is_integer(Id)
->
    Now = elib_dt:now(),
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET status = 'revoked', revoked_at = $1::timestamptz",
            " WHERE organization_id IS NOT DISTINCT FROM $2 AND id = $3 AND status = 'active'",
            " RETURNING id">>,
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
    end;
revoke_tx(_Conn, OrgId, _Arg3) when not is_integer(OrgId), OrgId =/= null ->
    {error, {invalid_organization_id, OrgId}}.

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

%% Resolve AppId and actual credential attribution; callers must authorize and
%% lock the application before using this row for credential mutations.
-spec find_by_id_tx(any(), integer()) -> {ok, map()} | {error, not_found | term()}.
find_by_id_tx(Conn, Id) when is_integer(Id) ->
    Sql = <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary, " WHERE id = $1 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% 调用方先锁 Application，再锁 credential，避免轮换并发及逆序死锁。
-spec lock_tx(any(), integer() | null, integer()) -> {ok, map()} | {error, term()}.
lock_tx(Conn, OrgId, Id) when
    (is_integer(OrgId) orelse OrgId =:= null), is_integer(Id)
->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id IS NOT DISTINCT FROM $1 AND id = $2 FOR UPDATE">>,
    case elib_pg:query(Conn, Sql, [OrgId, Id]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
lock_tx(_Conn, OrgId, _Arg3) when not is_integer(OrgId), OrgId =/= null ->
    {error, {invalid_organization_id, OrgId}}.

-spec find_tx(any(), integer() | null, integer()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, Id) when
    (is_integer(OrgId) orelse OrgId =:= null), is_integer(Id)
->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, " FROM ", (tablename())/binary,
            " WHERE organization_id IS NOT DISTINCT FROM $1 AND id = $2 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, Id]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end;
find_tx(_Conn, OrgId, _Arg3) when not is_integer(OrgId), OrgId =/= null ->
    {error, {invalid_organization_id, OrgId}}.

%% Shared signing keys use the existing database AEAD master key. A digest-only
%% credential cannot sign: rotate it through the normal issuance path first.
signing_secret(#{<<"signing_secret_enc">> := Encrypted}) when is_binary(Encrypted) ->
    case signing_key(postgre_aes_key) of
        {ok, Key} ->
            case elib_cipher:aes_gcm_decrypt(Encrypted, Key) of
                {ok, _} = Ok -> Ok;
                {error, _} -> decrypt_previous(Encrypted)
            end;
        Error ->
            Error
    end;
signing_secret(_) ->
    {error, signing_key_unavailable}.

encrypt_signing_secret(Secret) ->
    case signing_key(postgre_aes_key) of
        {ok, Key} -> elib_cipher:aes_gcm_encrypt(Secret, Key);
        Error -> Error
    end.

decrypt_previous(Encrypted) ->
    case signing_key(postgre_aes_key_old) of
        {ok, Key} -> elib_cipher:aes_gcm_decrypt(Encrypted, Key);
        Error -> Error
    end.

signing_key(Name) ->
    case config_ds:env(Name, <<>>) of
        Key when is_binary(Key), Key =/= <<>> -> {ok, crypto:hash(sha256, Key)};
        Key when is_list(Key), Key =/= [] -> {ok, crypto:hash(sha256, Key)};
        _ -> {error, signing_key_unavailable}
    end.
