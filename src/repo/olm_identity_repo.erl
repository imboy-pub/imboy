-module(olm_identity_repo).
%%%
% olm_identity_repo 是 Olm (X3DH + Double Ratchet) 设备密钥数据仓库层。
% 仅存公钥侧（curve25519 / ed25519 / one-time / fallback 公钥）；客户端私钥
% 经 pickle 加密落本地，服务端零接触。
%%%

-include("log.hrl").

-export([tablename_identity/0, tablename_one_time_key/0, tablename_fallback_key/0]).
-export([upsert_identity/6]).
-export([find_identity/2]).
-export([list_identity_by_uids/1]).
-export([upsert_one_time_keys/4]).
-export([count_one_time_keys/2]).
-export([claim_one_time_key/3]).
-export([cleanup_consumed_one_time_keys/1]).
-export([upsert_fallback_key/4]).
-export([claim_fallback_key/2]).

%% ===================================================================
%% 表名
%% ===================================================================

-spec tablename_identity() -> binary().
tablename_identity() ->
    elib_pg_sql:public_tablename(<<"olm_identity">>).

-spec tablename_one_time_key() -> binary().
tablename_one_time_key() ->
    elib_pg_sql:public_tablename(<<"olm_one_time_key">>).

-spec tablename_fallback_key() -> binary().
tablename_fallback_key() ->
    elib_pg_sql:public_tablename(<<"olm_fallback_key">>).

%% ===================================================================
%% 身份键
%% ===================================================================

%% @doc 上报/更新设备 Olm 身份键（ed25519 + curve25519 + 签名）
-spec upsert_identity(integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, term()} | {error, term()}.
upsert_identity(UserId, DeviceId, Ed25519Key, Curve25519Key, Signature, _DeviceType) when
    is_integer(UserId)
->
    Tb = tablename_identity(),
    Id = elib_tsid:generate(olm_identity),
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (id, user_id, device_id, ed25519_key, curve25519_key, signature)",
        " VALUES ($1, $2, $3, $4, $5, $6)",
        " ON CONFLICT (user_id, device_id) DO UPDATE",
        " SET ed25519_key = EXCLUDED.ed25519_key,",
        "     curve25519_key = EXCLUDED.curve25519_key,",
        "     signature = EXCLUDED.signature,",
        "     updated_at = CURRENT_TIMESTAMP"
    >>,
    elib_pg:query(Sql, [Id, UserId, DeviceId, Ed25519Key, Curve25519Key, Signature]).

%% @doc 查询单设备身份键
-spec find_identity(integer(), binary()) -> {ok, map() | not_found} | {error, term()}.
find_identity(UserId, DeviceId) ->
    Tb = tablename_identity(),
    Sql =
        <<"SELECT device_id, ed25519_key, curve25519_key, signature", " FROM ", Tb/binary,
            " WHERE user_id = $1 AND device_id = $2 LIMIT 1">>,
    case elib_pg:query(Sql, [UserId, DeviceId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {ok, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 批量查询多个用户的身份键（X3DH 用，客户端拉对端所有设备身份）
-spec list_identity_by_uids([integer()]) -> {ok, [map()]} | {error, term()}.
list_identity_by_uids(Uids) when is_list(Uids) ->
    Tb = tablename_identity(),
    Sql =
        <<"SELECT user_id, device_id, ed25519_key, curve25519_key, signature", " FROM ", Tb/binary,
            " WHERE user_id = ANY($1)">>,
    elib_pg:query(Sql, [Uids]).

%% ===================================================================
%% One-Time Keys（X3DH prekey；claim 标记 status='claimed' 不删，保留审计行）
%% 参见 ADR 03 §8.3：claim 语义由「即删」演进为「UPDATE 审计」（migration 00000045）。
%% ===================================================================

%% @doc 批量上报 one-time keys（替换「可用」keys；保留 status='claimed' 的审计行）。
-spec upsert_one_time_keys(integer(), binary(), [{binary(), binary()}], pos_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
upsert_one_time_keys(UserId, DeviceId, Keys, _MaxKeys) when is_list(Keys) ->
    Tb = tablename_one_time_key(),
    DeleteSql =
        <<"DELETE FROM ", Tb/binary,
            " WHERE user_id = $1 AND device_id = $2 AND status = 'available'">>,
    case elib_pg:execute(DeleteSql, [UserId, DeviceId]) of
        {ok, _} ->
            Rows = [
                {elib_tsid:generate(olm_one_time_key), UserId, DeviceId, KeyId, KeyB64}
             || {KeyId, KeyB64} <- Keys
            ],
            InsertSql = <<
                "INSERT INTO ",
                Tb/binary,
                " (id, user_id, device_id, key_id, key_base64) VALUES ($1, $2, $3, $4, $5)"
            >>,
            InsertFun = fun(Row) -> elib_pg:execute(InsertSql, tuple_to_list(Row)) end,
            Results = [InsertFun(R) || R <- Rows],
            case [E || {error, E} <- Results] of
                [] -> {ok, length(Rows)};
                Errors -> {error, {batch_insert_failed, Errors}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 统计某设备剩余 one-time keys 数量（客户端低水位补传）
-spec count_one_time_keys(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
count_one_time_keys(UserId, DeviceId) ->
    Tb = tablename_one_time_key(),
    Sql =
        <<"SELECT COUNT(*) AS n FROM ", Tb/binary,
            " WHERE user_id = $1 AND device_id = $2 AND status = 'available'">>,
    case elib_pg:query(Sql, [UserId, DeviceId]) of
        {ok, [#{<<"n">> := N}]} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 领取一个 one-time key（原子消费：SELECT FOR UPDATE SKIP LOCKED + UPDATE）。
%%  ADR 03 §6/§8：claim 从「即删」改为「UPDATE status='claimed'」，保留消费痕迹供
%%  审计与低水位统计；已消费行由 cleanup_consumed_one_time_keys/1 定期清理。
%%  并发安全：picked 只选 status='available' 且 FOR UPDATE SKIP LOCKED，同 OTK 不重复领取。
%%  UserId/DeviceId = OTK 拥有者（对端）；ClaimedBy = 领取方 uid，写入审计列 claimed_by。
-spec claim_one_time_key(integer(), binary(), integer()) ->
    {ok, map()} | {error, exhausted}.
claim_one_time_key(UserId, DeviceId, ClaimedBy) ->
    Tb = tablename_one_time_key(),
    %% 单条原子事务：选最早的一条可用 OTK，标记为 claimed（不删）并返回其 key。
    Sql = <<
        "WITH picked AS (",
        "  SELECT id, key_id, key_base64 FROM ",
        Tb/binary,
        "  WHERE user_id = $1 AND device_id = $2 AND status = 'available'",
        "  ORDER BY id ASC LIMIT 1",
        "  FOR UPDATE SKIP LOCKED",
        "),",
        "claimed AS (",
        "  UPDATE ",
        Tb/binary,
        "  SET status = 'claimed', consumed_at = CURRENT_TIMESTAMP, claimed_by = $3",
        "  WHERE id IN (SELECT id FROM picked)",
        "  RETURNING id",
        ")",
        "SELECT p.key_id, p.key_base64 FROM picked p",
        " JOIN claimed c ON p.id = c.id"
    >>,
    case elib_pg:query(Sql, [UserId, DeviceId, ClaimedBy]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, exhausted};
        {error, Reason} ->
            _ = ?ERROR_LOG({olm_claim_otk_error, UserId, DeviceId, Reason}),
            {error, exhausted}
    end.

%% @doc 清理已消费（claimed）且超过保留期的 one-time key 审计行（cleanup worker 调用）。
%%  ADR 03 §6：claim 不删只标记，需定期清理避免表膨胀。RetentionSeconds 内的保留供审计。
-spec cleanup_consumed_one_time_keys(non_neg_integer()) ->
    {ok, non_neg_integer()} | {error, term()}.
cleanup_consumed_one_time_keys(RetentionSeconds) when is_integer(RetentionSeconds) ->
    Tb = tablename_one_time_key(),
    Sql =
        <<"DELETE FROM ", Tb/binary, " WHERE status = 'claimed'",
            "   AND consumed_at < CURRENT_TIMESTAMP - ($1 || ' seconds')::interval">>,
    case elib_pg:execute(Sql, [integer_to_binary(RetentionSeconds)]) of
        {ok, N} -> {ok, N};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Fallback Key（OTK 耗尽兜底，每设备覆盖式 1 条，不删除）
%% ===================================================================

%% @doc 上报/覆盖 fallback key
-spec upsert_fallback_key(integer(), binary(), binary(), binary()) ->
    {ok, term()} | {error, term()}.
upsert_fallback_key(UserId, DeviceId, KeyId, KeyB64) ->
    Tb = tablename_fallback_key(),
    Id = elib_tsid:generate(olm_fallback_key),
    Sql = <<
        "INSERT INTO ",
        Tb/binary,
        " (id, user_id, device_id, key_id, key_base64) VALUES ($1, $2, $3, $4, $5)",
        " ON CONFLICT (user_id, device_id) DO UPDATE",
        " SET key_id = EXCLUDED.key_id, key_base64 = EXCLUDED.key_base64,",
        "     created_at = CURRENT_TIMESTAMP"
    >>,
    elib_pg:query(Sql, [Id, UserId, DeviceId, KeyId, KeyB64]).

%% @doc 领取 fallback key（不删除，可重复领取——客户端生成新 fallback 后旧的自然失效）
-spec claim_fallback_key(integer(), binary()) -> {ok, map()} | {error, exhausted}.
claim_fallback_key(UserId, DeviceId) ->
    Tb = tablename_fallback_key(),
    Sql =
        <<"SELECT key_id, key_base64 FROM ", Tb/binary,
            " WHERE user_id = $1 AND device_id = $2 LIMIT 1">>,
    case elib_pg:query(Sql, [UserId, DeviceId]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, exhausted};
        {error, _Reason} -> {error, exhausted}
    end.
