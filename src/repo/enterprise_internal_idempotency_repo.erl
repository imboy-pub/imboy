-module(enterprise_internal_idempotency_repo).

-moduledoc "internal API 幂等记录仓储层。".
%%%
% enterprise_internal_idempotency_repo 是 internal API 幂等记录仓储层（迁移
% 00000136 建、00000144 V2.1 扩展：response_body text 快照 + completion 一致性
% CHECK；plan §11：同 key 同 body 精确重放完整响应，同 key 异 body 409
% idempotency_conflict，过期行同事务原子重置）。
%
% 表结构：enterprise_internal_idempotency(
%   PK (organization_id, application_id, idempotency_key),
%   request_digest 64hex, resource_type, resource_id 可空, response_code 可空,
%   response_body text 可空（字节保序快照，与 response_code 同空/同非空
%   ——ck_eii_completion；jsonb 会重排 JSON，破坏字节精确重放，故用 text）,
%   expires_at NOT NULL, created_at)。
%
% record_tx 的并发/过期语义（§11 Concurrent/TTL，两步同事务）：
%   1) INSERT ... ON CONFLICT DO NOTHING：键未被占用则首插（ xmax 语义不再
%      需要——DO NOTHING 未命中时零行返回）；与未提交的同键事务在唯一索引
%      上天然排队（后来者阻塞到首事务提交/回滚为止）；
%   2) 未首插则 SELECT ... FOR UPDATE 行锁读取（READ COMMITTED 下拿到的是
%      阻塞结束后已提交的最新行版本——「其余等待首事务完成后 replay」）：
%      - 行已过期（expires_at < CURRENT_TIMESTAMP）→ 原子重置 UPDATE
%        （digest/result/expiry/created_at 全量重置，response_* 清空）
%        → {ok, inserted, NewRow}（作为新请求执行）；
%      - digest 一致 → {ok, existing, Row}（是否已完成由 response_code 判定，
%        完成行携 response_body 快照精确重放）；
%      - digest 不一致 → {error, digest_conflict}（上层 409）。
%%%

-export([
    tablename/0,
    record_tx/7,
    find_tx/4,
    claim_tx/7,
    delete_expired/0
]).

-define(COLUMNS, <<
    "organization_id, application_id, idempotency_key, request_digest, "
    "resource_type, resource_id, response_code, response_body, expires_at, created_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_internal_idempotency">>).

%% @doc 事务内登记幂等键（全部 internal mutation 的前置步骤；两步语义见
%% moduledoc）。ExpiresAt 是二进制 RFC3339；RequestDigest 由上层按 §11 公式
%% 生成（64 hex，长度约束由 ck_eii_request_digest 强制）。
-spec record_tx(any(), integer(), integer(), binary(), binary(), binary(), binary()) ->
    {ok, inserted | existing, map()} | {error, digest_conflict | term()}.
record_tx(Conn, OrgId, AppId, IdempotencyKey, ResourceType, RequestDigest, ExpiresAt) when
    is_integer(OrgId),
    is_integer(AppId),
    is_binary(IdempotencyKey),
    is_binary(ResourceType),
    is_binary(RequestDigest),
    is_binary(ExpiresAt)
->
    Tb = tablename(),
    Now = elib_dt:now(),
    InsertSql =
        <<"INSERT INTO ", Tb/binary,
            " (organization_id, application_id, idempotency_key, request_digest,",
            " resource_type, expires_at, created_at)",
            " VALUES ($1, $2, $3, $4, $5, $6::timestamptz, $7)",
            " ON CONFLICT (organization_id, application_id, idempotency_key)",
            " DO NOTHING RETURNING ", ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, InsertSql, [
            OrgId, AppId, IdempotencyKey, RequestDigest, ResourceType, ExpiresAt, Now
        ])
    of
        {ok, [Row | _]} ->
            {ok, inserted, Row};
        {ok, []} ->
            existing_or_reset_tx(
                Conn, OrgId, AppId, IdempotencyKey, ResourceType, RequestDigest, ExpiresAt
            );
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 事务内按主键取幂等行（含同行 expired 布尔，避免 Erlang 侧解析时间）。
-spec find_tx(any(), integer(), integer(), binary()) -> {ok, map()} | {error, not_found | term()}.
find_tx(Conn, OrgId, AppId, IdempotencyKey) when is_binary(IdempotencyKey) ->
    Sql =
        <<"SELECT ", ?COLUMNS/binary, ", (expires_at < CURRENT_TIMESTAMP) AS expired", " FROM ",
            (tablename())/binary, " WHERE organization_id = $1 AND application_id = $2",
            " AND idempotency_key = $3 LIMIT 1">>,
    case elib_pg:query(Conn, Sql, [OrgId, AppId, IdempotencyKey]) of
        {ok, [Row | _]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 事务内回填执行结果快照（业务执行成功后：资源 ID、响应码与完整
%% JSON 响应体）。仅允许回填一次：response_code IS NULL 才更新；已回填行
%% 返回 {error, already_claimed}（并发竞争的第二个执行者拿到它即放弃重复
%% 执行）。ResponseBody 为 JSON binary（**text 原样字节**——jsonb 会重排
%% 键序/空白破坏 §11 字节精确重放，见迁移 00000144 文件头）；nil → NULL。
-spec claim_tx(
    any(), integer(), integer(), binary(), integer() | null, integer(), binary() | null
) -> ok | {error, not_found | already_claimed | term()}.
claim_tx(Conn, OrgId, AppId, IdempotencyKey, ResourceId, ResponseCode, ResponseBody) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET resource_id = $4, response_code = $5,",
            " response_body = $6", " WHERE organization_id = $1 AND application_id = $2",
            " AND idempotency_key = $3 AND response_code IS NULL">>,
    case
        elib_pg:execute(Conn, Sql, [
            OrgId, AppId, IdempotencyKey, ResourceId, ResponseCode, ResponseBody
        ])
    of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            case find_tx(Conn, OrgId, AppId, IdempotencyKey) of
                {ok, _} -> {error, already_claimed};
                {error, not_found} -> {error, not_found};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 清理过期幂等行（i_eii_expires 索引路径；运维/定时任务调用——
%% 正确性不依赖它：过期行由 record_tx 同事务原子重置）。
-spec delete_expired() -> {ok, non_neg_integer()} | {error, term()}.
delete_expired() ->
    Sql =
        <<"DELETE FROM ", (tablename())/binary, " WHERE expires_at < CURRENT_TIMESTAMP">>,
    case elib_pg:execute(Sql, []) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% 首插未命中（键被占用或并发在途）：行锁读取后在锁内做过期重置/比对。
%% READ COMMITTED 下 FOR UPDATE 会阻塞到持锁事务提交，并返回其提交后的
%% 最新行版本——同 key 并发请求因此天然「等待首事务完成后重放」。
-spec existing_or_reset_tx(
    any(), integer(), integer(), binary(), binary(), binary(), binary()
) -> {ok, inserted | existing, map()} | {error, digest_conflict | term()}.
existing_or_reset_tx(Conn, OrgId, AppId, IdempotencyKey, ResourceType, RequestDigest, ExpiresAt) ->
    SelectSql =
        <<"SELECT ", ?COLUMNS/binary, ", (expires_at < CURRENT_TIMESTAMP) AS expired FROM ",
            (tablename())/binary, " WHERE organization_id = $1 AND application_id = $2",
            " AND idempotency_key = $3 FOR UPDATE">>,
    case elib_pg:query(Conn, SelectSql, [OrgId, AppId, IdempotencyKey]) of
        {ok, [Row]} ->
            case maps:get(<<"expired">>, Row) of
                true ->
                    reset_expired_tx(
                        Conn, OrgId, AppId, IdempotencyKey, ResourceType, RequestDigest, ExpiresAt
                    );
                false ->
                    case maps:get(<<"request_digest">>, Row) =:= RequestDigest of
                        true -> {ok, existing, maps:remove(<<"expired">>, Row)};
                        false -> {error, digest_conflict}
                    end
            end;
        {ok, []} ->
            %% 锁等待窗口内行消失（仅运维 DELETE 与首插事务回滚竞争可达；
            %% 正常链路不可达）。显式错误由上层重试，不静默当作新请求。
            {error, row_vanished};
        {error, Reason} ->
            {error, Reason}
    end.

%% 过期行原子重置（§11 TTL：同事务内重置 digest/result/expiry，作为新请求
%% 执行）。调用方已持该行的 FOR UPDATE 锁；WHERE 仍按 PK 精确命中。
-spec reset_expired_tx(
    any(), integer(), integer(), binary(), binary(), binary(), binary()
) -> {ok, inserted, map()} | {error, term()}.
reset_expired_tx(Conn, OrgId, AppId, IdempotencyKey, ResourceType, RequestDigest, ExpiresAt) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET request_digest = $4, resource_type = $5, resource_id = NULL,",
            " response_code = NULL, response_body = NULL, expires_at = $6::timestamptz,",
            " created_at = CURRENT_TIMESTAMP",
            " WHERE organization_id = $1 AND application_id = $2",
            " AND idempotency_key = $3 RETURNING ", ?COLUMNS/binary>>,
    case
        elib_pg:query(Conn, Sql, [
            OrgId, AppId, IdempotencyKey, RequestDigest, ResourceType, ExpiresAt
        ])
    of
        {ok, [Row | _]} ->
            {ok, inserted, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.
