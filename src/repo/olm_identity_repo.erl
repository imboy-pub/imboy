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
-export([list_devices_with_identity/1]).
-export([upsert_one_time_keys/4]).
-export([count_one_time_keys/2]).
-export([claim_one_time_key/3]).
-export([claim_one_time_key/4]).
-export([cleanup_consumed_one_time_keys/1]).
-export([upsert_fallback_key/4]).
-export([claim_fallback_key/2]).
-export([delete_by_device/2]).

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

%% @doc 列出对端某用户全部活跃设备（含 olm 身份键 + 派生列），ADR 03 §8.1 统一设备列表 API。
%%  仅返回 status=1（活跃）且已注册 olm_identity 的设备（INNER JOIN）。
%%  capabilities/trust_state/identity_blob/identity_signature 为 user_device 侧
%%  （migration 00000043）；ed25519/curve25519/signature 为 olm_identity 侧公钥
%%  （服务端零私钥，ADR 07 §6）。服务端不解释 capabilities，仅透传（ADR 03 §5.2）。
-spec list_devices_with_identity(integer()) -> {ok, [map()]} | {error, term()}.
list_devices_with_identity(UserId) when is_integer(UserId) ->
    UdTb = elib_pg_sql:public_tablename(<<"user_device">>),
    OiTb = tablename_identity(),
    Sql = <<
        "SELECT ud.device_id, ud.device_type, ud.capabilities, ud.trust_state,",
        "       ud.identity_blob, ud.identity_signature,",
        "       oi.ed25519_key, oi.curve25519_key, oi.signature",
        " FROM ",
        UdTb/binary,
        " ud",
        " JOIN ",
        OiTb/binary,
        " oi ON ud.user_id = oi.user_id AND ud.device_id = oi.device_id",
        " WHERE ud.user_id = $1 AND ud.status = 1",
        " ORDER BY ud.device_id"
    >>,
    elib_pg:query(Sql, [UserId]).

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

%% @doc E2EE-062：带**幂等租约**的 claim。
%%
%%  X3DH 的 one-time prekey 是一次性资源。没有幂等键时，客户端一次网络超时
%%  后的重试就会再消费一条；恶意方重放同一请求即可定向耗尽某用户的池，把
%%  所有新会话逼到复用同一条 fallback prekey（前向保密显著下降）。
%%
%%  语义（21-playbook E2EE-025）：同 (ClaimedBy, UserId, DeviceId, RequestId)
%%  重放**只消费一条**，每次返回同一条 key。租约按**领取方**隔离——
%%  换一个领取方拿同样的 RequestId 不得读到别人的 claim 结果（否则
%%  RequestId 就成了越权读取他人已领 key 的通道）。
%%
%%  原子性：先查租约；未命中则消费一条并写入 claim_request_id。
%%  两个并发同 RequestId 的请求可能同时查空，此时
%%  `uk_olm_otk_claim_request`（部分唯一索引）让第二条 UPDATE 撞 23505，
%%  捕获后回查租约返回第一条的结果——**不重复消费**。
%%  这一层不能省：只靠先查后写是 TOCTOU。
%%
%%  RequestId 为空（旧客户端）→ 退回 /3 的逐次消费语义，零破坏。
-spec claim_one_time_key(integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, exhausted}.
claim_one_time_key(UserId, DeviceId, ClaimedBy, RequestId) when
    is_binary(RequestId), RequestId =/= <<>>
->
    case find_claim_by_request(UserId, DeviceId, ClaimedBy, RequestId) of
        {ok, Row} ->
            {ok, Row};
        {error, not_found} ->
            case claim_with_request_id(UserId, DeviceId, ClaimedBy, RequestId) of
                {ok, Row} ->
                    {ok, Row};
                {error, request_conflict} ->
                    %% 并发同 RequestId：另一路已消费并写入租约，回查返回同一条
                    case find_claim_by_request(UserId, DeviceId, ClaimedBy, RequestId) of
                        {ok, Row} -> {ok, Row};
                        {error, not_found} -> {error, exhausted}
                    end;
                {error, exhausted} ->
                    {error, exhausted}
            end
    end;
claim_one_time_key(UserId, DeviceId, ClaimedBy, _RequestId) ->
    claim_one_time_key(UserId, DeviceId, ClaimedBy).

%% @private 回查已发放的租约（严格按领取方 + 目标设备 + request_id 三元组）
-spec find_claim_by_request(integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, not_found}.
find_claim_by_request(UserId, DeviceId, ClaimedBy, RequestId) ->
    Tb = tablename_one_time_key(),
    Sql = <<
        "SELECT key_id, key_base64 FROM ",
        Tb/binary,
        " WHERE user_id = $1 AND device_id = $2 AND claimed_by = $3",
        "   AND claim_request_id = $4 AND status = 'claimed' LIMIT 1"
    >>,
    case elib_pg:query(Sql, [UserId, DeviceId, ClaimedBy, RequestId]) of
        {ok, [Row | _]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            %% fail-closed：查询失败不得当成「没有租约」而去消费新 key
            _ = ?ERROR_LOG({olm_claim_lease_lookup_error, UserId, DeviceId, Reason}),
            {error, not_found}
    end.

%% @private 消费一条并登记租约；唯一索引冲突返回 request_conflict
-spec claim_with_request_id(integer(), binary(), integer(), binary()) ->
    {ok, map()} | {error, exhausted | request_conflict}.
claim_with_request_id(UserId, DeviceId, ClaimedBy, RequestId) ->
    Tb = tablename_one_time_key(),
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
        "  SET status = 'claimed', consumed_at = CURRENT_TIMESTAMP,",
        "      claimed_by = $3, claim_request_id = $4",
        "  WHERE id IN (SELECT id FROM picked)",
        "  RETURNING id",
        ")",
        "SELECT p.key_id, p.key_base64 FROM picked p",
        " JOIN claimed c ON p.id = c.id"
    >>,
    case elib_pg:query(Sql, [UserId, DeviceId, ClaimedBy, RequestId]) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, exhausted};
        {error, {error, {error, <<"23505">>, unique_violation, _, _}}} ->
            {error, request_conflict};
        {error, Reason} ->
            case is_unique_violation(Reason) of
                true ->
                    {error, request_conflict};
                false ->
                    _ = ?ERROR_LOG({olm_claim_otk_error, UserId, DeviceId, Reason}),
                    {error, exhausted}
            end
    end.

%% @private epgsql 的错误包装层数随调用路径不同，统一按 sqlstate 判定
-spec is_unique_violation(term()) -> boolean().
is_unique_violation(unique_violation) ->
    true;
is_unique_violation(<<"23505">>) ->
    true;
is_unique_violation(T) when is_tuple(T) ->
    lists:any(fun is_unique_violation/1, tuple_to_list(T));
is_unique_violation(L) when is_list(L) ->
    lists:any(fun is_unique_violation/1, L);
is_unique_violation(_) ->
    false.

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

%% ===================================================================
%% 设备吊销级联
%% ===================================================================

%% @doc 清除某设备的全部 Olm 材料（身份键 + 一次性键 + fallback 键）。
%%
%% 为什么必须做：user_device 行是设备白名单，删行即吊销 token。但 Olm 材料存在
%% 独立三张表里，不随之消失。留着的后果不是"占点空间"，是**吊销对 E2EE 不生效**：
%%   1. list_devices_with_identity/1 仍把已吊销设备列为收件人 → 扇出继续向死设备
%%      加密，发送方每条消息都白算一次且永远等不到该设备的 ACK；
%%   2. claim_one_time_key/3 仍能领到它的预共享密钥 → 对端与一个不存在的设备
%%      建立 Olm 会话，密文无人能解。
%%
%% 三张表分别 DELETE 而非靠外键级联：这三张表未建 FK 到 user_device（设备行是
%% 硬删的白名单，建 FK 只会让删设备被 Olm 行阻塞）。
%%
%% 返回删除的总行数，供调用方记日志/断言。任一语句失败即短路返回 {error, _}——
%% 吊销清理失败必须让上层看见，不能静默吞掉。
-spec delete_by_device(integer(), binary()) -> {ok, non_neg_integer()} | {error, term()}.
delete_by_device(UserId, DeviceId) when is_integer(UserId), is_binary(DeviceId) ->
    Tbs = [tablename_identity(), tablename_one_time_key(), tablename_fallback_key()],
    lists:foldl(
        fun
            (_Tb, {error, _} = Err) ->
                Err;
            (Tb, {ok, Acc}) ->
                Sql = <<"DELETE FROM ", Tb/binary, " WHERE user_id = $1 AND device_id = $2">>,
                %% elib_pg:execute/2 契约含 {ok, N} 与 {ok, N, Rows} 两种成功形态
                %% （后者用于 RETURNING）。两者都算成功，别让将来加 RETURNING 的人
                %% 掉进"成功被判成 error"的坑。
                case elib_pg:execute(Sql, [UserId, DeviceId]) of
                    {ok, Cnt} when is_integer(Cnt) -> {ok, Acc + Cnt};
                    {ok, Cnt, _Rows} when is_integer(Cnt) -> {ok, Acc + Cnt};
                    {error, Reason} -> {error, Reason};
                    Other -> {error, Other}
                end
        end,
        {ok, 0},
        Tbs
    ).
