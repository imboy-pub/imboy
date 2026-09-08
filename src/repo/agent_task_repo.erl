-module(agent_task_repo).

%%%
% Agent Task 持久层（DATA-01）。三表 SQL：agent_task / agent_task_event /
% agent_task_decision（migration 00000090）。
%
% 并发语义全部下沉数据库：
%   - 状态迁移 = 条件 UPDATE（WHERE status = ANY(源态集合)）CAS，0 行=迁移未发生；
%   - 决定仲裁 = agent_task_decision.task_id 唯一约束 + ON CONFLICT DO NOTHING，
%     第二个决定者插入 0 行（first-writer-wins）；
%   - 事件去重 = agent_task_event.idempotency_key 唯一索引 + ON CONFLICT DO NOTHING，
%     0 行=重复事件（调用方不得重复投递 durable 消息）。
% 所有写操作提供 _tx 版本（复用调用方事务连接），仲裁在 ds 层单事务内完成。
%%%

-export([tablename/0, event_tablename/0, decision_tablename/0]).
-export([with_tx/1]).
-export([ensure_task/1, ensure_task_tx/2, get_task/1, get_task_tx/2]).
-export([cas_status/2, cas_status_tx/3]).
-export([insert_event_tx/2, get_decision/1, insert_decision_tx/2]).
-export([set_result_tx/3]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() -> elib_pg_sql:public_tablename(<<"agent_task">>).

-spec event_tablename() -> binary().
event_tablename() -> elib_pg_sql:public_tablename(<<"agent_task_event">>).

-spec decision_tablename() -> binary().
decision_tablename() -> elib_pg_sql:public_tablename(<<"agent_task_decision">>).

%% @doc 事务透传（lib 约定：直接返回 fun 的值 R；回滚返回 {rollback, Reason}）。
-spec with_tx(fun((any()) -> term())) -> term().
with_tx(Fun) ->
    elib_pg:with_tx(Fun).

%% @doc 任务不存在则插入（初始态 submitted），存在则原样返回现有行。
%% 返回 {task, Row, Createdboolean()}。幂等：ON CONFLICT (id) DO NOTHING。
-spec ensure_task(map()) -> {ok, map(), boolean()} | {error, term()}.
%% 注意：elib_pg:with_tx 直接返回 fun 的值（R），回滚时返回 {rollback, Reason}。
ensure_task(Data) ->
    case elib_pg:with_tx(fun(Conn) -> ensure_task_tx(Conn, Data) end) of
        {rollback, Reason} -> {error, Reason};
        {ok, _Row, _Created} = Ok -> Ok;
        {error, _} = E -> E
    end.

-spec ensure_task_tx(any(), map()) -> {ok, map(), boolean()} | {error, term()}.
ensure_task_tx(Conn, Data) ->
    Tb = tablename(),
    #{id := TaskId} = Data,
    Status = maps:get(status, Data, <<"submitted">>),
    CorrelationId = maps:get(correlation_id, Data),
    Idem = maps:get(idempotency_key, Data),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, group_id, agent_uid, tool, params_digest, status, correlation_id, idempotency_key) ",
            "VALUES ($1,$2,$3,$4,$5,$6,$7,$8) ON CONFLICT (id) DO NOTHING RETURNING id">>,
    Params = [
        TaskId,
        maps:get(group_id, Data),
        maps:get(agent_uid, Data),
        maps:get(tool, Data, <<>>),
        maps:get(params_digest, Data, <<>>),
        Status,
        CorrelationId,
        Idem
    ],
    case elib_pg:query(Conn, Sql, Params) of
        {ok, []} ->
            case get_task_tx(Conn, TaskId) of
                {ok, Row} ->
                    {ok, Row, false};
                {error, Reason2} ->
                    ?ERROR_LOG(
                        "agent_task_repo:get_task_tx after conflict: ~p~n",
                        [Reason2]
                    ),
                    {error, Reason2}
            end;
        %% ON CONFLICT DO NOTHING 且零行命中时 equery 只回 {ok, 0}
        {ok, 0} ->
            case get_task_tx(Conn, TaskId) of
                {ok, Row} -> {ok, Row, false};
                {error, _} = E -> E
            end;
        {ok, [_]} ->
            case get_task_tx(Conn, TaskId) of
                {ok, Row} -> {ok, Row, true};
                {error, _} = E -> E
            end;
        {error, _} = E ->
            E
    end.

-spec get_task(binary()) -> {ok, map()} | {error, not_found | term()}.
get_task(TaskId) ->
    {Sql, Params} = get_task_sql(TaskId),
    case elib_pg:query(Sql, Params) of
        {ok, [Row]} ->
            {ok, Row};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            ?ERROR_LOG("agent_task_repo:get_task error ~p~n", [Reason]),
            {error, Reason}
    end.

-spec get_task_tx(any(), binary()) -> {ok, map()} | {error, not_found | term()}.
get_task_tx(Conn, TaskId) ->
    {Sql, Params} = get_task_sql(TaskId),
    case elib_pg:query(Conn, Sql, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

get_task_sql(TaskId) ->
    Tb = tablename(),
    Sql = iolist_to_binary(
        [
            <<"SELECT id, group_id, agent_uid, tool, params_digest, result_digest, status, ">>,
            <<"correlation_id, idempotency_key, created_at, updated_at FROM ">>,
            Tb,
            <<" WHERE id = $1">>
        ]
    ),
    {Sql, [ec_cnv:to_binary(TaskId)]}.

%% @doc CAS 状态迁移（池连接版，供 emit 通道的非事务调用）。
-spec cas_status(binary(), {list(), binary()}) ->
    {ok, updated | not_matched} | {error, term()}.
cas_status(TaskId, {FromStates, ToStatus}) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = $2, updated_at = now() ",
            "WHERE id = $1 AND status = ANY($3::text[]) RETURNING id">>,
    FromList = [ec_cnv:to_binary(S) || S <- FromStates],
    case elib_pg:query(Sql, [ec_cnv:to_binary(TaskId), ToStatus, FromList]) of
        {ok, [_]} -> {ok, updated};
        {ok, []} -> {ok, not_matched};
        %% UPDATE..RETURNING 零行时 equery 只回 {ok, 0}（无列无行）
        {ok, 0} -> {ok, not_matched};
        {ok, 0, []} -> {ok, not_matched};
        {ok, _N, [_]} -> {ok, updated};
        {error, _} = E -> E
    end.

%% @doc CAS 状态迁移：仅当当前 status ∈ FromStates 时置为 ToStatus。
%% 返回 {ok, updated | not_matched}；NOT matched 含"已在终态/被并发抢先/从未存在"。
-spec cas_status_tx(any(), binary(), {list(), binary()}) ->
    {ok, updated | not_matched} | {error, term()}.
cas_status_tx(Conn, TaskId, {FromStates, ToStatus}) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary, " SET status = $2, updated_at = now() ",
            "WHERE id = $1 AND status = ANY($3::text[]) RETURNING id">>,
    FromList = [ec_cnv:to_binary(S) || S <- FromStates],
    case elib_pg:query(Conn, Sql, [ec_cnv:to_binary(TaskId), ToStatus, FromList]) of
        {ok, [_]} -> {ok, updated};
        {ok, []} -> {ok, not_matched};
        {ok, 0} -> {ok, not_matched};
        {ok, 0, []} -> {ok, not_matched};
        {ok, _N, [_]} -> {ok, updated};
        {error, _} = E -> E
    end.

%% @doc 事件落库（幂等键去重）。返回 inserted=true 表示首次（调用方才可投递 durable）。
-spec insert_event_tx(any(), map()) -> {ok, inserted | duplicate} | {error, term()}.
insert_event_tx(Conn, Data) ->
    Tb = event_tablename(),
    #{id := Id, task_id := TaskId, status := Status, correlation_id := Corr} = Data,
    Idem = maps:get(idempotency_key, Data),
    Seq = maps:get(seq, Data, 0),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (id, task_id, status, seq, correlation_id, idempotency_key) ",
            "VALUES ($1,$2,$3,$4,$5,$6) ON CONFLICT (idempotency_key) DO NOTHING">>,
    case
        elib_pg:query(Conn, Sql, [
            Id,
            ec_cnv:to_binary(TaskId),
            ec_cnv:to_binary(Status),
            Seq,
            Corr,
            Idem
        ])
    of
        {ok, [_]} -> {ok, inserted};
        {ok, []} -> {ok, duplicate};
        %% INSERT 无 RETURNING 时 epgsql 返回 {ok, Count}（elib_pg 原样透传）
        {ok, Count} when is_integer(Count), Count > 0 -> {ok, inserted};
        {ok, Count} when is_integer(Count) -> {ok, duplicate};
        {ok, 1, [_]} -> {ok, inserted};
        {ok, 0, []} -> {ok, duplicate};
        {error, _} = E -> E
    end.

-spec get_decision(binary()) -> {ok, map()} | {error, not_found | term()}.
get_decision(TaskId) ->
    Tb = decision_tablename(),
    Sql = iolist_to_binary(
        [
            <<"SELECT id, task_id, decision, approver_uid, correlation_id, decided_at FROM ">>,
            Tb,
            <<" WHERE task_id = $1">>
        ]
    ),
    case elib_pg:query(Sql, [ec_cnv:to_binary(TaskId)]) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc 决定落库（first-writer-wins：task_id 唯一约束仲裁）。
%% 返回 inserted=true 表示本调用是第一写者。
-spec insert_decision_tx(any(), map()) -> {ok, inserted | duplicate} | {error, term()}.
insert_decision_tx(Conn, Data) ->
    Tb = decision_tablename(),
    #{
        id := Id,
        task_id := TaskId,
        decision := Decision,
        approver_uid := ApproverUid,
        correlation_id := Corr
    } = Data,
    Sql =
        <<"INSERT INTO ", Tb/binary, " (id, task_id, decision, approver_uid, correlation_id) ",
            "VALUES ($1,$2,$3,$4,$5) ON CONFLICT (task_id) DO NOTHING">>,
    case
        elib_pg:query(Conn, Sql, [
            Id,
            ec_cnv:to_binary(TaskId),
            ec_cnv:to_binary(Decision),
            ApproverUid,
            Corr
        ])
    of
        {ok, [_]} -> {ok, inserted};
        {ok, []} -> {ok, duplicate};
        %% INSERT 无 RETURNING 时 epgsql 返回 {ok, Count}（elib_pg 原样透传）
        {ok, Count} when is_integer(Count), Count > 0 -> {ok, inserted};
        {ok, Count} when is_integer(Count) -> {ok, duplicate};
        {ok, 1, [_]} -> {ok, inserted};
        {ok, 0, []} -> {ok, duplicate};
        {error, _} = E -> E
    end.

%% @doc 结果摘要落库（complete 时调用；digest 化，正文不落库）。
-spec set_result_tx(any(), binary(), binary()) -> {ok, updated} | {error, term()}.
set_result_tx(Conn, TaskId, ResultDigest) ->
    Tb = tablename(),
    Sql = <<"UPDATE ", Tb/binary, " SET result_digest = $2, updated_at = now() WHERE id = $1">>,
    case
        elib_pg:query(Conn, Sql, [
            ec_cnv:to_binary(TaskId),
            ec_cnv:to_binary(ResultDigest)
        ])
    of
        {ok, _} -> {ok, updated};
        {error, _} = E -> E
    end.
