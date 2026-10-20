-module(attach_direct_attempt_repo).
-moduledoc "direct-v1 每次可能外部写入的唯一 attempt 账本（迁移 00000200）。".
%%%
% direct-v1 attempt 账本（协议 §1.3，docs/architecture/
% attachment-direct-transfer-v1-protocol.md）：
%
%   每次可能写入 = 一行；attempt_state 单跳推进，推进只经迁移 200 的
%   受限函数 fn_dv1_attempt_transition（合法转换白名单在库内，调用方
%   不能随意改态）。UNKNOWN_QUARANTINED 本轮无晋升边：任何出边都会被
%   函数拒绝，需要新的成功只能新 attempt + 新 target。
%
%   本 repo 只做账本读写与函数调用的薄封装；授权/配额/expected 复核在
%   step-6 会话逻辑与 step-7 配额卡中组装，不在本层。
%%%

-export([create_tx/7]).
-export([mark_dispatched_tx/3]).
-export([record_outcome_tx/5]).
-export([record_proof_tx/5]).
-export([transition_tx/6]).
-export([winner_pin_tx/5]).
-export([list_for_session/1]).
-export([direct_profile_digest/0]).
-export([get_tx/3]).

-include("log.hrl").
-include_lib("epgsql/include/epgsql.hrl").

%% @doc 分配新 attempt（fn_dv1_attempt_create：锁 pending 行、预算 ≤3、
%% winner 已存在则拒绝）。返回 {ok, AttemptNo}。
-spec create_tx(any(), binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, integer()}
    | {error, attempt_budget_exhausted | winner_pinned | illegal_state | not_found | term()}.
create_tx(Conn, ObjectKey, SourceKey, TargetKey, Driver, Namespace, Container) ->
    ProfileDigest = direct_profile_digest(),
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_dv1_attempt_create($1,$2,$3,$4,$5,$6,$7) AS no">>,
            [ObjectKey, SourceKey, TargetKey, Driver, Namespace, Container, ProfileDigest]
        )
    of
        {ok, [#{<<"no">> := No}]} -> {ok, No};
        {ok, []} -> {error, attempt_not_returned};
        {error, #error{code = <<"23514">>}} -> {error, attempt_budget_exhausted};
        {error, #error{code = <<"23510">>}} -> {error, illegal_state_or_winner};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% @doc CREATED -> DISPATCHED 单跳（发包前必须已持久化，I12：DB fence
%% 不停止已发请求，但授权只此一次）。
-spec mark_dispatched_tx(any(), binary(), integer()) -> ok | {error, cas_lost | term()}.
mark_dispatched_tx(Conn, ObjectKey, AttemptNo) ->
    transition_tx(Conn, ObjectKey, AttemptNo, <<"CREATED">>, <<"DISPATCHED">>, undefined).

%% @doc dispatch 结果记录：仅明确成功响应 -> COPY_COMPLETED；超时/响应
%% 丢失/kill -> UNKNOWN_QUARANTINED（无晋升边，只读探测与隔离 GC）。
-spec record_outcome_tx(
    any(), binary(), integer(), copy_completed | unknown_quarantined, binary() | undefined
) ->
    ok | {error, cas_lost | term()}.
record_outcome_tx(Conn, ObjectKey, AttemptNo, copy_completed, ErrorCode) ->
    transition_tx(Conn, ObjectKey, AttemptNo, <<"DISPATCHED">>, <<"COPY_COMPLETED">>, ErrorCode);
record_outcome_tx(Conn, ObjectKey, AttemptNo, unknown_quarantined, ErrorCode) ->
    transition_tx(
        Conn, ObjectKey, AttemptNo, <<"DISPATCHED">>, <<"UNKNOWN_QUARANTINED">>, ErrorCode
    ).

%% @doc COPY_COMPLETED -> PROOF_VERIFIED，附 candidate full hash/size。
-spec record_proof_tx(any(), binary(), integer(), binary(), integer()) ->
    ok | {error, cas_lost | term()}.
record_proof_tx(Conn, ObjectKey, AttemptNo, Sha256Hex, Size) ->
    case
        elib_pg:query(
            Conn,
            <<
                "UPDATE attach_direct_attempt SET attempt_state='PROOF_VERIFIED',"
                " proof_sha256=$3, proof_size=$4, proof_received_at=now(), updated_at=now()"
                " WHERE object_key=$1 AND attempt_no=$2 AND attempt_state='COPY_COMPLETED'"
                " RETURNING attempt_no"
            >>,
            [ObjectKey, AttemptNo, Sha256Hex, Size]
        )
    of
        {ok, [_]} -> ok;
        {ok, []} -> {error, cas_lost};
        {error, R} -> {error, R}
    end.
%% @doc 通用单跳转换（白名单在 fn_dv1_attempt_transition 内）。
-spec transition_tx(any(), binary(), integer(), binary(), binary(), binary() | undefined) ->
    ok | {error, cas_lost | term()}.
transition_tx(Conn, ObjectKey, AttemptNo, From, To, ErrorCode) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_dv1_attempt_transition($1,$2,$3,$4,$5) AS ok">>,
            [ObjectKey, AttemptNo, From, To, ErrorCode]
        )
    of
        {ok, [#{<<"ok">> := true}]} -> ok;
        {ok, [#{<<"ok">> := false}]} -> {error, cas_lost};
        {ok, []} -> {error, transition_not_returned};
        {error, R} -> {error, R}
    end.

%% @doc winner 线性化点（协议 §4 锁序 + §1.3）：PROOF_VERIFIED -> WINNER、
%% attachment 物理映射、pending CONFIRMED 同事务。digest 与 physical key
%% 与 attempt 行不符即拒绝（fail-closed）。
-spec winner_pin_tx(any(), binary(), integer(), binary(), binary()) ->
    ok | {error, winner_conflict | not_found | term()}.
winner_pin_tx(Conn, ObjectKey, AttemptNo, PhysicalKey, LocatorSha256) ->
    case
        elib_pg:query(
            Conn,
            <<"SELECT fn_dv1_winner_pin($1,$2,$3,$4)">>,
            [ObjectKey, AttemptNo, PhysicalKey, LocatorSha256]
        )
    of
        {ok, _} -> ok;
        {error, #error{code = <<"23510">>}} -> {error, winner_conflict};
        {error, #error{code = <<"P0002">>}} -> {error, not_found};
        {error, #error{code = <<"40001">>}} -> {error, winner_race_lost};
        {error, R} -> {error, R}
    end.

%% @doc 会话 attempt 账本（恢复/审计用；含全部态）。
-spec list_for_session(binary()) -> {ok, [map()]} | {error, term()}.
list_for_session(ObjectKey) ->
    elib_pg:query(
        <<
            "SELECT attempt_no, attempt_state, source_key, target_key, target_driver,"
            " target_namespace, target_container, profile_digest, proof_sha256, proof_size,"
            " proof_received_at, last_error_code, created_at, updated_at"
            " FROM attach_direct_attempt WHERE object_key=$1 ORDER BY attempt_no"
        >>,
        [ObjectKey]
    ).

%% @doc 持锁读取单个 attempt（复核路径用）。
-spec get_tx(any(), binary(), integer()) -> {ok, map()} | {error, not_found | term()}.
get_tx(Conn, ObjectKey, AttemptNo) ->
    case
        elib_pg:query(
            Conn,
            <<
                "SELECT attempt_no, attempt_state, source_key, target_key, target_driver,"
                " target_namespace, target_container, profile_digest, dispatch_cas_uuid,"
                " proof_sha256, proof_size, worker_receipt, last_error_code"
                " FROM attach_direct_attempt WHERE object_key=$1 AND attempt_no=$2 FOR UPDATE"
            >>,
            [ObjectKey, AttemptNo]
        )
    of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, R} -> {error, R}
    end.

%% profile 指纹：driver 合同 + 运行配置摘要（A0 实测后由 step-5a 细化注入；
%% 当前为 config 基线，保证 attempt 行可追溯其冻结的存储语义版本）。
direct_profile_digest() ->
    Cfg = application:get_env(imboy, garage, #{}),
    Base = [
        {<<"driver">>, <<"garage">>},
        {<<"endpoint">>, maps:get(endpoint, Cfg, <<>>)},
        {<<"container">>, maps:get(bucket, Cfg, <<>>)}
    ],
    binary:encode_hex(crypto:hash(sha256, jsone:encode(maps:from_list(Base)))).
