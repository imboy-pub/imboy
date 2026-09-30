%%% Organization-wide seat configuration on the caller's transaction connection.
-module(cs_pg_seat_governance).
-export([govern/4]).

-define(KEYS, [organization_id, business_identity_id, enabled, max_concurrent, version]).
-define(SELECT,
    <<"SELECT organization_id,business_identity_id,enabled,max_concurrent,version FROM customer_service_seat">>
).

govern(Conn, OrgId, Operation, Params) ->
    require_row(
        Conn,
        <<"SELECT id FROM organization WHERE id=$1 AND status='active' FOR SHARE">>,
        [OrgId],
        organization
    ),
    operate(Conn, OrgId, Operation, Params).

operate(Conn, OrgId, list, P) ->
    case
        elib_pg:query(
            Conn,
            <<?SELECT/binary,
                " WHERE organization_id=$1 AND business_identity_id>$2 ORDER BY business_identity_id LIMIT $3">>,
            [OrgId, maps:get(after_id, P, 0), maps:get(limit, P, 50)]
        )
    of
        {ok, Rows} -> {ok, [cs_pg_common:normalize_row(Row, ?KEYS) || Row <- Rows]};
        {error, R} -> rollback(cs_pg_common:normalize_error(R))
    end;
operate(Conn, OrgId, detail, P) ->
    {ok, seat(Conn, OrgId, maps:get(business_identity_id, P), <<>>)};
operate(Conn, OrgId, create, P) ->
    mutation_guard(Conn, OrgId, P, false),
    IdentityId = maps:get(business_identity_id, P),
    Stored = must(
        cs_pg_seat:create_seat_limit_tx(
            Conn,
            OrgId,
            IdentityId,
            maps:get(enabled, P, true),
            maps:get(max_concurrent, P, 1),
            undefined
        )
    ),
    audit(Conn, OrgId, P, <<"application.seat.created">>, #{}, Stored),
    {ok, maps:with(?KEYS, Stored)};
operate(Conn, OrgId, update, P) ->
    mutation_guard(Conn, OrgId, P, maps:get(enabled, P, undefined) =:= false),
    %% Same org lock order as capacity-checked creation, before locking a seat.
    must(elib_pg:query(Conn, <<"SELECT pg_advisory_xact_lock($1::bigint)">>, [OrgId])),
    IdentityId = maps:get(business_identity_id, P),
    Before = seat(Conn, OrgId, IdentityId, <<" FOR UPDATE">>),
    case maps:get(version, Before) =:= maps:get(expected_version, P) of
        false ->
            rollback(conflict);
        true ->
            must(
                cs_pg_seat:set_enabled_limit_tx(
                    Conn,
                    OrgId,
                    IdentityId,
                    maps:get(enabled, P, maps:get(enabled, Before)),
                    maps:get(at, P)
                )
            ),
            Max = maps:get(max_concurrent, P, maps:get(max_concurrent, Before)),
            case
                elib_pg:execute(
                    Conn,
                    <<"UPDATE customer_service_seat SET max_concurrent=$3 WHERE organization_id=$1 AND business_identity_id=$2">>,
                    [OrgId, IdentityId, Max]
                )
            of
                {ok, 1} -> ok;
                {error, R} -> rollback(cs_pg_common:normalize_error(R))
            end,
            After = seat(Conn, OrgId, IdentityId, <<>>),
            audit(Conn, OrgId, P, <<"application.seat.updated">>, Before, After),
            {ok, After}
    end.

mutation_guard(Conn, OrgId, P, AllowInactive) ->
    %% workspace_id is an explicit audit location, never the seat's ownership.
    require_row(
        Conn,
        <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 AND status='active' FOR SHARE">>,
        [OrgId, maps:get(workspace_id, P)],
        workspace
    ),
    require_row(
        Conn,
        <<"SELECT id FROM organization_business_identity WHERE organization_id=$1 AND id=$2 AND function_key='customer_service' AND (status='active' OR $3::boolean) FOR SHARE">>,
        [OrgId, maps:get(business_identity_id, P), AllowInactive],
        identity
    ).

seat(Conn, OrgId, IdentityId, Lock) ->
    must(
        cs_pg_common:fetch_one_conn(
            Conn,
            <<?SELECT/binary, " WHERE organization_id=$1 AND business_identity_id=$2",
                Lock/binary>>,
            [OrgId, IdentityId],
            ?KEYS
        )
    ).

require_row(Conn, Sql, Args, Resource) ->
    case elib_pg:query(Conn, Sql, Args) of
        {ok, [_ | _]} -> ok;
        {ok, []} -> rollback({not_found, Resource});
        {error, R} -> rollback(cs_pg_common:normalize_error(R))
    end.

audit(Conn, OrgId, P, Action, Before, After) ->
    Event = #{
        workspace_id => maps:get(workspace_id, P),
        business_identity_id => maps:get(business_identity_id, P),
        actor_kind => <<"application">>,
        action => Action,
        detail => #{
            <<"application_id">> => maps:get(application_id, P),
            <<"correlation_id">> => maps:get(correlation_id, P),
            <<"before">> => maps:with(?KEYS, Before),
            <<"after">> => maps:with(?KEYS, After)
        }
    },
    case cs_pg_seat:insert_event_in(Conn, OrgId, Event) of
        {ok, _} -> ok;
        {error, R} -> rollback({audit_append_failed, R})
    end.

must({ok, Value}) -> Value;
must({error, R}) -> rollback(R).
rollback(Reason) -> throw({rollback, {error, Reason}}).
