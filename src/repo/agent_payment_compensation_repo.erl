-module(agent_payment_compensation_repo).

%% 持久化 Agent 支付预留补偿 outbox。
%% settling -> settled/released 的状态变化分别与钱包结算/额度释放处于同一事务。

-export([tablename/0]).
-export([mark_settled/2]).
-export([release/1]).
-export([claim_pending/1]).
-export([mark_retry/3]).

-include("log.hrl").

-spec tablename() -> binary().
tablename() ->
    elib_pg_sql:public_tablename(<<"agent_payment_compensation">>).

%% @doc 在钱包结算事务内标记 settled；失败必须抛 rollback，禁止出现已扣款未标记。
-spec mark_settled(integer(), epgsql:connection()) -> ok | no_return().
mark_settled(CompensationId, Conn) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 'settled', settled_at = NOW(), updated_at = NOW(),"
            "     lease_until = NULL"
            " WHERE id = $1 AND status = 'settling'">>,
    case elib_pg:execute(Conn, Sql, [CompensationId]) of
        {ok, 1} -> ok;
        {ok, 0} -> throw({rollback, compensation_not_settling});
        {error, Reason} -> throw({rollback, Reason})
    end.

%% @doc 幂等释放：锁住 outbox 行，在同一事务内扣回 mandate 额度并标记 released。
-spec release(integer()) -> ok | {error, term()}.
release(CompensationId) ->
    CompensationTb = tablename(),
    MandateTb = elib_pg_sql:public_tablename(<<"agent_payment_mandate">>),
    SelectSql =
        <<"SELECT mandate_id, amount_fen, status FROM ", CompensationTb/binary,
            " WHERE id = $1 FOR UPDATE">>,
    ReleaseSql =
        <<"UPDATE ", MandateTb/binary,
            " SET spent_fen = GREATEST(spent_fen - $2, 0), updated_at = NOW()"
            " WHERE id = $1">>,
    MarkSql =
        <<"UPDATE ", CompensationTb/binary,
            " SET status = 'released', released_at = NOW(), updated_at = NOW(),"
            "     lease_until = NULL, last_error = NULL"
            " WHERE id = $1 AND status IN ('settling', 'pending', 'processing')">>,
    elib_pg:with_tx(fun(Conn) ->
        case elib_pg:query(Conn, SelectSql, [CompensationId]) of
            {ok, []} ->
                {error, compensation_not_found};
            {ok, [
                #{
                    <<"mandate_id">> := MandateId,
                    <<"amount_fen">> := AmountFen,
                    <<"status">> := Status
                }
            ]} ->
                case Status of
                    <<"settled">> ->
                        ok;
                    <<"released">> ->
                        ok;
                    _ ->
                        case elib_pg:execute(Conn, ReleaseSql, [MandateId, AmountFen]) of
                            {ok, 1} ->
                                case elib_pg:execute(Conn, MarkSql, [CompensationId]) of
                                    {ok, 1} -> ok;
                                    {ok, 0} -> throw({rollback, compensation_state_changed});
                                    {error, Reason} -> throw({rollback, Reason})
                                end;
                            {ok, 0} ->
                                throw({rollback, mandate_not_found});
                            {error, Reason} ->
                                throw({rollback, Reason})
                        end
                end;
            {error, Reason} ->
                {error, Reason}
        end
    end).

%% @doc 领取到期/待重试/租约过期的补偿记录，单条 UPDATE 避免多节点重复处理。
-spec claim_pending(pos_integer()) -> {ok, [map()]} | {error, term()}.
claim_pending(Limit) ->
    Sql =
        <<
            "WITH claimable AS ("
            " SELECT id FROM ",
            (tablename())/binary,
            " WHERE next_attempt_at <= NOW()"
            "   AND ("
            "     (status = 'pending')"
            "     OR (status IN ('settling', 'processing') AND lease_until < NOW())"
            "   )"
            " ORDER BY id FOR UPDATE SKIP LOCKED LIMIT $1"
            ")"
            " UPDATE ",
            (tablename())/binary,
            " AS c SET status = 'processing', attempts = c.attempts + 1,"
            "     lease_until = NOW() + INTERVAL '5 minutes', updated_at = NOW()"
            " FROM claimable WHERE c.id = claimable.id"
            " RETURNING c.id, c.mandate_id, c.amount_fen, c.attempts"
        >>,
    elib_pg:query(Sql, [Limit]).

%% @doc 释放失败时退回 pending，保留错误和下次执行时间。
-spec mark_retry(integer(), pos_integer(), term()) -> ok | {error, term()}.
mark_retry(CompensationId, DelaySecs, Reason) ->
    Sql =
        <<"UPDATE ", (tablename())/binary,
            " SET status = 'pending', next_attempt_at = NOW() + ($2 * INTERVAL '1 second'),"
            "     lease_until = NULL, last_error = $3, updated_at = NOW()"
            " WHERE id = $1 AND status = 'processing'">>,
    case
        elib_pg:query(Sql, [
            CompensationId, DelaySecs, iolist_to_binary(io_lib:format("~p", [Reason]))
        ])
    of
        {ok, _} -> ok;
        {error, Error} -> {error, Error}
    end.
