-module(talk_record_idempotency_repo).
-moduledoc "面聊共享幂等键仓储（talk_record_idempotency；迁移 00000211）。".
%%%
% 创建幂等键持久化（设计 §2/§4；CONTRACT-FREEZE-T02 §2.5）：
%   * Scope：唯一键 (actor_id, method, resource, idempotency_key)；
%   * payload_hash = SHA-256(method ++ path ++ "\n" ++ canonical_json_body)
%     ——digest 公式与 enterprise_internal_idempotency:request_digest/3
%     对齐（canonical_json 复用 enterprise_cursor_v2）；
%   * TTL 默认 7 天（config talk_record_idempotency_ttl_seconds 可覆写），
%     过期后在同一事务内原子重置（digest/expiry/result_ref 归零重来），
%     不依赖清理任务；
%   * result_ref 只存首次成功结果的业务引用 JSON（记录元数据快照），
%     不保存录音正文或签名 URL。
%%%

-export([tablename/0]).
-export([record_tx/7]).
-export([complete_tx/6]).
-export([find/4]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% ===================================================================
%% API
%% ===================================================================

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"talk_record_idempotency">>).

%% @doc 幂等前置（事务内）：
%%   * 首次 / 过期原子重置 → {ok, inserted}（执行业务，complete_tx 回填）；
%%   * 命中且未过期 → {ok, existing, Row}（上层比对 payload_hash、决定
%%     重放 result_ref 或按 pending 处理）。
-spec record_tx(any(), integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, inserted} | {ok, existing, map()} | {error, term()}.
record_tx(Conn, ActorId, Method, Resource, Key, Digest, ExpiresAt) ->
    Insert =
        <<"INSERT INTO ", (tablename())/binary,
            " (id, actor_id, method, resource, idempotency_key, payload_hash, expires_at)",
            " VALUES ($1,$2,$3,$4,$5,$6,$7)",
            " ON CONFLICT (actor_id, method, resource, idempotency_key) DO NOTHING">>,
    case
        elib_pg:execute(Conn, Insert, [
            elib_tsid:generate(talk_record), ActorId, Method, Resource, Key, Digest, ExpiresAt
        ])
    of
        {ok, 1} ->
            {ok, inserted};
        {ok, 0} ->
            claim_existing(Conn, ActorId, Method, Resource, Key, Digest, ExpiresAt);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 业务成功后回填首次结果（仅一次；result_ref IS NULL 才允许）。
-spec complete_tx(any(), integer(), binary(), binary(), binary(), binary()) ->
    ok | {error, already_claimed | not_found | term()}.
complete_tx(Conn, ActorId, Method, Resource, Key, ResultRef) ->
    Sql =
        <<"UPDATE ", (tablename())/binary, " SET result_ref = $5, updated_at = $6",
            " WHERE actor_id = $1 AND method = $2 AND resource = $3 AND idempotency_key = $4",
            " AND result_ref IS NULL">>,
    case
        elib_pg:execute_count(Conn, Sql, [ActorId, Method, Resource, Key, ResultRef, elib_dt:now()])
    of
        {ok, 1} -> ok;
        {ok, 0} -> {error, already_claimed};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 只读查询（诊断/测试）。
-spec find(integer(), binary(), binary(), binary()) -> {ok, map()} | {error, not_found | term()}.
find(ActorId, Method, Resource, Key) ->
    Sql =
        <<"SELECT id, actor_id, method, resource, idempotency_key, payload_hash,",
            " result_ref, expires_at FROM ", (tablename())/binary,
            " WHERE actor_id = $1 AND method = $2 AND resource = $3 AND idempotency_key = $4">>,
    case elib_pg:one(Sql, [ActorId, Method, Resource, Key]) of
        {ok, Row} -> {ok, Row};
        {error, Reason} -> {error, Reason}
    end.

%% ===================================================================
%% Internal
%% ===================================================================

%% @private 命中已存在行：FOR UPDATE 锁定后区分「过期原子重置」与「existing」。
-spec claim_existing(any(), integer(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, inserted} | {ok, existing, map()} | {error, term()}.
claim_existing(Conn, ActorId, Method, Resource, Key, Digest, ExpiresAt) ->
    Select =
        <<"SELECT id, payload_hash, result_ref, expires_at FROM ", (tablename())/binary,
            " WHERE actor_id = $1 AND method = $2 AND resource = $3 AND idempotency_key = $4",
            " FOR UPDATE">>,
    case elib_pg:query(Conn, Select, [ActorId, Method, Resource, Key]) of
        {ok, [Row = #{<<"expires_at">> := ExpiresAtBin}]} ->
            case expired(ExpiresAtBin) of
                true ->
                    Reset =
                        <<"UPDATE ", (tablename())/binary,
                            " SET payload_hash = $5, expires_at = $6, result_ref = NULL,",
                            " updated_at = $7",
                            " WHERE actor_id = $1 AND method = $2 AND resource = $3",
                            " AND idempotency_key = $4">>,
                    case
                        elib_pg:execute_count(Conn, Reset, [
                            ActorId, Method, Resource, Key, Digest, ExpiresAt, elib_dt:now()
                        ])
                    of
                        {ok, 1} -> {ok, inserted};
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {ok, existing, Row}
            end;
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private expires_at（RFC3339 binary，rfc3339 codec 形态）是否已过当前时刻。
%% 注意 elib_dt:now(microsecond) 返回 RFC3339 binary——必须先转整数再比较，
%% 直接与 rfc3339_to 的整数结果比较会因 number < binary 恒真而误判过期。
-spec expired(binary() | term()) -> boolean().
expired(ExpiresAtBin) when is_binary(ExpiresAtBin) ->
    try
        NowUs = elib_dt:rfc3339_to(elib_dt:now(microsecond), microsecond),
        elib_dt:rfc3339_to(ExpiresAtBin, microsecond) =< NowUs
    catch
        _:_ -> true
    end;
expired(_) ->
    true.
