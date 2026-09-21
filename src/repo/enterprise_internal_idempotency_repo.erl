-module(enterprise_internal_idempotency_repo).

%%%
% enterprise_internal_idempotency_repo 是 internal API 幂等记录仓储层（迁移
% 00000136，EPGZ-01 / plan-gz §6：所有 mutation 必带 Idempotency-Key；同 key
% 同 body 重放原结果，同 key 异 body 409 idempotency_conflict）。
%
% 表结构：enterprise_internal_idempotency(
%   PK (organization_id, application_id, idempotency_key),
%   request_digest 64hex, resource_type, resource_id 可空, response_code 可空,
%   expires_at NOT NULL, created_at)。
%%%

-export([
    tablename/0,
    record_tx/7,
    find_tx/4,
    claim_tx/6,
    delete_expired/0
]).

-define(COLUMNS, <<
    "organization_id, application_id, idempotency_key, request_digest, "
    "resource_type, resource_id, response_code, expires_at, created_at"
>>).

%% ===================================================================
%% API functions
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"enterprise_internal_idempotency">>).

%% @doc 事务内登记幂等键（全部 internal mutation 的前置步骤）。
%% 单语句原子判定（xmax = 0 即首插行；ON CONFLICT DO UPDATE 命中既有行时
%% xmax 为本事务 xid，非 0）：
%%   {ok, inserted, Row} —— 键首次登记，调用方继续执行业务并经 claim_tx/6 回填；
%%   {ok, existing, Row} —— 键已存在且 request_digest 一致（重放窗口命中，
%%                          是否已出结果由 Row 的 resource_id/response_code 判定）；
%%   {error, digest_conflict} —— 同键异 body（上层映射 409 idempotency_conflict）。
%% ExpiresAt 是二进制 RFC3339；RequestDigest 由上层对规范化请求体做
%% SHA-256 hex（64 字符，长度约束由 ck_eii_request_digest 强制）。
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
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (organization_id, application_id, idempotency_key, request_digest,",
            " resource_type, expires_at, created_at)",
            " VALUES ($1, $2, $3, $4, $5, $6::timestamptz, $7)",
            " ON CONFLICT (organization_id, application_id, idempotency_key)",
            " DO UPDATE SET idempotency_key = EXCLUDED.idempotency_key", " RETURNING ",
            ?COLUMNS/binary, ", (xmax = 0) AS inserted">>,
    case
        elib_pg:query(Conn, Sql, [
            OrgId, AppId, IdempotencyKey, RequestDigest, ResourceType, ExpiresAt, Now
        ])
    of
        {ok, [Row | _]} = Ok ->
            case maps:get(<<"inserted">>, Row) of
                true ->
                    {ok, inserted, maps:remove(<<"inserted">>, Row)};
                false ->
                    case maps:get(<<"request_digest">>, Row) =:= RequestDigest of
                        true ->
                            {ok, existing, maps:remove(<<"inserted">>, Row)};
                        false ->
                            %% 同键异 body：本语句对既有行无写入副作用
                            %% （DO UPDATE 只回写同值 idempotency_key），交由上层 409
                            _ = Ok,
                            {error, digest_conflict}
                    end
            end;
        {ok, []} ->
            {error, insert_empty_result};
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

%% @doc 事务内回填执行结果（业务执行成功后：资源 ID 与响应码）。
%% 仅允许回填一次：resource_id IS NULL 才更新；已回填行返回
%% {error, already_claimed}（并发竞争的第二个执行者拿到它即放弃重复执行）。
-spec claim_tx(any(), integer(), integer(), binary(), integer(), integer()) ->
    ok | {error, not_found | already_claimed | term()}.
claim_tx(Conn, OrgId, AppId, IdempotencyKey, ResourceId, ResponseCode) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET resource_id = $4, response_code = $5",
            " WHERE organization_id = $1 AND application_id = $2",
            " AND idempotency_key = $3 AND resource_id IS NULL">>,
    case elib_pg:execute(Conn, Sql, [OrgId, AppId, IdempotencyKey, ResourceId, ResponseCode]) of
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

%% @doc 清理过期幂等行（i_eii_expires 索引路径；运维/定时任务调用）。
-spec delete_expired() -> {ok, non_neg_integer()} | {error, term()}.
delete_expired() ->
    Sql =
        <<"DELETE FROM ", (tablename())/binary, " WHERE expires_at < CURRENT_TIMESTAMP">>,
    case elib_pg:execute(Sql, []) of
        {ok, Count} when is_integer(Count) -> {ok, Count};
        {error, Reason} -> {error, Reason}
    end.
