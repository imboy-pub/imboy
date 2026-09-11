-module(agent_hub_audit_repo).

%%% TRACE-00 runtime audit source. The table only permits frozen chain metadata.

-export([record_task_start_tx/3, record_transition_tx/6, record_approval_tx/5]).
-export([record_delivery_tx/4, set_delivery_status_tx/3, list_by_correlation/1]).

-spec record_task_start_tx(any(), binary(), binary()) -> ok | no_return().
record_task_start_tx(Conn, Corr, TaskId) ->
    RequestId = stable_id(<<"req-">>, Corr),
    ok = insert_tx(Conn, Corr, <<"request">>, RequestId, null, <<"accepted">>),
    ok = insert_tx(Conn, Corr, <<"task">>, TaskId, RequestId, <<"created">>).

-spec record_transition_tx(any(), binary(), binary(), binary(), binary(), binary()) ->
    ok | no_return().
record_transition_tx(Conn, Corr, TaskId, EventId, Status, PreviousStatus) ->
    case entity_exists_tx(Conn, Corr, TaskId) of
        false ->
            abort({audit_parent_missing, task});
        true ->
            ok = insert_tx(Conn, Corr, <<"event">>, EventId, TaskId, Status),
            record_execution_or_outcome_tx(
                Conn, Corr, TaskId, EventId, Status, PreviousStatus
            )
    end.

-spec record_approval_tx(any(), binary(), binary(), binary(), binary()) ->
    ok | no_return().
record_approval_tx(Conn, Corr, TaskId, ApprovalId, Decision) ->
    case entity_exists_tx(Conn, Corr, TaskId) of
        false -> abort({audit_parent_missing, task});
        true -> insert_tx(Conn, Corr, <<"approval">>, ApprovalId, TaskId, Decision)
    end.

-spec record_delivery_tx(any(), binary(), binary(), binary()) -> ok | no_chain | no_return().
record_delivery_tx(Conn, Corr, DeliveryId, Status) ->
    case latest_parent_tx(Conn, Corr, [<<"execution">>, <<"event">>]) of
        {ok, ParentId} ->
            insert_tx(Conn, Corr, <<"delivery">>, DeliveryId, ParentId, Status);
        not_found ->
            case has_chain_tx(Conn, Corr) of
                true -> abort({audit_parent_missing, delivery});
                false -> no_chain
            end
    end.

-spec set_delivery_status_tx(any(), binary(), binary()) -> ok | no_chain | no_return().
set_delivery_status_tx(Conn, DeliveryId, Status) ->
    Tb = tablename(),
    Sql =
        <<"UPDATE ", Tb/binary,
            " SET status = $2, updated_at = now()"
            " WHERE entity_id = $1 AND entity_type = 'delivery' RETURNING entity_id">>,
    case elib_pg:query(Conn, Sql, [DeliveryId, delivery_status(Status)]) of
        {ok, [_]} -> ok;
        {ok, []} -> no_chain;
        {ok, 0} -> no_chain;
        {ok, _N, [_]} -> ok;
        {error, Reason} -> abort({audit_delivery_update_failed, Reason});
        Other -> abort({audit_delivery_update_failed, Other})
    end.

-spec list_by_correlation(binary()) -> {ok, [map()]} | {error, term()}.
list_by_correlation(Corr) ->
    Tb = tablename(),
    elib_pg:query(
        <<
            "SELECT correlation_id, entity_type, entity_id, parent_entity_id,"
            " status, occurred_at, updated_at FROM ",
            Tb/binary,
            " WHERE correlation_id = $1 ORDER BY occurred_at, entity_id"
        >>,
        [Corr]
    ).

record_execution_or_outcome_tx(Conn, Corr, TaskId, EventId, <<"working">>, Previous) ->
    ParentId = execution_parent_tx(Conn, Corr, TaskId, Previous),
    ExecutionId = stable_id(<<"exe-">>, EventId),
    insert_tx(Conn, Corr, <<"execution">>, ExecutionId, ParentId, <<"running">>);
record_execution_or_outcome_tx(Conn, Corr, TaskId, EventId, Status, _Previous) when
    Status =:= <<"completed">>; Status =:= <<"failed">>; Status =:= <<"cancelled">>
->
    ExecutionId = ensure_terminal_execution_tx(Conn, Corr, TaskId, EventId, Status),
    OutcomeId = stable_id(<<"out-">>, Corr),
    insert_tx(Conn, Corr, <<"outcome">>, OutcomeId, ExecutionId, outcome_status(Status));
record_execution_or_outcome_tx(_Conn, _Corr, _TaskId, _EventId, _Status, _Previous) ->
    ok.

execution_parent_tx(Conn, Corr, _TaskId, <<"approved">>) ->
    case latest_parent_tx(Conn, Corr, [<<"approval">>]) of
        {ok, ApprovalId} -> ApprovalId;
        not_found -> abort({audit_parent_missing, approval})
    end;
execution_parent_tx(_Conn, _Corr, TaskId, _Previous) ->
    TaskId.

ensure_terminal_execution_tx(Conn, Corr, TaskId, EventId, Status) ->
    case latest_parent_tx(Conn, Corr, [<<"execution">>]) of
        {ok, ExecutionId} ->
            ExecutionId;
        not_found ->
            ExecutionId = stable_id(<<"exe-">>, EventId),
            ok = insert_tx(
                Conn, Corr, <<"execution">>, ExecutionId, TaskId, outcome_status(Status)
            ),
            ExecutionId
    end.

insert_tx(Conn, Corr, Type, EntityId, ParentId, Status) ->
    Tb = tablename(),
    Sql =
        <<"INSERT INTO ", Tb/binary,
            " (correlation_id, entity_type, entity_id, parent_entity_id, status)"
            " VALUES ($1,$2,$3,$4,$5) RETURNING entity_id">>,
    case elib_pg:query(Conn, Sql, [Corr, Type, EntityId, ParentId, Status]) of
        {ok, [_]} -> ok;
        {ok, _N, [_]} -> ok;
        {error, Reason} -> abort({audit_insert_failed, Type, Reason});
        Other -> abort({audit_insert_failed, Type, Other})
    end.

entity_exists_tx(Conn, Corr, EntityId) ->
    Tb = tablename(),
    case
        elib_pg:query(
            Conn,
            <<"SELECT 1 FROM ", Tb/binary,
                " WHERE correlation_id = $1 AND entity_id = $2 LIMIT 1">>,
            [Corr, EntityId]
        )
    of
        {ok, [_]} -> true;
        _ -> false
    end.

has_chain_tx(Conn, Corr) ->
    Tb = tablename(),
    case
        elib_pg:query(
            Conn,
            <<"SELECT 1 FROM ", Tb/binary,
                " WHERE correlation_id = $1 AND entity_type = 'request' LIMIT 1">>,
            [Corr]
        )
    of
        {ok, [_]} -> true;
        _ -> false
    end.

latest_parent_tx(Conn, Corr, Types) ->
    Tb = tablename(),
    case
        elib_pg:query(
            Conn,
            <<"SELECT entity_id FROM ", Tb/binary,
                " WHERE correlation_id = $1 AND entity_type = ANY($2::text[])"
                " ORDER BY occurred_at DESC, entity_id DESC LIMIT 1">>,
            [Corr, Types]
        )
    of
        {ok, [#{<<"entity_id">> := EntityId}]} -> {ok, EntityId};
        {ok, []} -> not_found;
        {ok, 0} -> not_found;
        {error, Reason} -> abort({audit_parent_lookup_failed, Reason});
        Other -> abort({audit_parent_lookup_failed, Other})
    end.

stable_id(Prefix, Seed) ->
    Hex = binary:encode_hex(crypto:hash(sha256, Seed), lowercase),
    <<Head:32/binary, _/binary>> = Hex,
    <<Prefix/binary, Head/binary>>.

delivery_status(<<"success">>) -> <<"delivered">>;
delivery_status(<<"dead">>) -> <<"failed">>;
delivery_status(Status) -> Status.

outcome_status(<<"completed">>) -> <<"succeeded">>;
outcome_status(Status) -> Status.

tablename() -> elib_pg_sql:public_tablename(<<"agent_hub_audit">>).

abort(Reason) -> throw({abort_tx, Reason}).
