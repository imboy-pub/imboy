-module(talk_record_job_execution_mode_pg_checks).
-moduledoc "Controller-only real-PG checks; dedicated formal-schema DB and caller transaction required.".
-export([run/2, rollback_lock/4]).
-include_lib("eunit/include/eunit.hrl").

%% No connection/bootstrap, migrations, table clearing or mock fallback here.
%% The controller supplies its isolated connection and a synthetic live record.
%% All writes belong to the caller's transaction and must be rolled back by it.
run(C, RecordId) ->
    {ok, []} = elib_pg:query(C, <<"SELECT id FROM talk_record_job LIMIT 1">>, []),
    Server = enqueue(C, RecordId, <<"assemble">>, default),
    Client = enqueue(C, RecordId, <<"transcribe">>, <<"client_byok">>),
    Summary = enqueue(C, RecordId, <<"summarize">>, <<"client_byok">>),
    {ok, #{<<"id">> := Server, <<"execution_mode">> := <<"server_local">>}} =
        talk_record_job_repo:claim_next_tx(C, <<"mode-check">>, token(), 120),
    ?assertEqual(
        {error, empty}, talk_record_job_repo:claim_next_tx(C, <<"mode-check">>, token(), 120)
    ),
    ?assertEqual(<<"queued">>, state(C, Client)),
    ?assertEqual(<<"queued">>, state(C, Summary)),
    running_client_is_not_a_server_lease(C, Client),
    expired_client_is_not_requeued(C, Server, Client),
    failed_or_suspended_client_is_not_retried(C, Client, Summary),
    sql_constraints(C, RecordId, Client),
    cleanup_unblock(C, Client),
    governance_cancels_both_modes(C, RecordId, Client, Summary),
    {ok, #{
        checks => [
            claim,
            renew,
            finish,
            reaper,
            retry,
            unsuspend,
            execution_mode_constraints,
            immutable_custody,
            governance_cancel
        ]
    }}.

enqueue(C, RecordId, Kind, Mode) ->
    Id = elib_tsid:generate(attachment),
    Job = #{
        id => Id,
        record_id => RecordId,
        kind => Kind,
        dedupe_key => <<"mode-check:", (integer_to_binary(Id))/binary>>
    },
    Input =
        case Mode of
            default -> Job;
            _ -> Job#{execution_mode => Mode}
        end,
    {ok, created, Id} = talk_record_job_repo:enqueue_tx(C, Input),
    Id.

running_client_is_not_a_server_lease(C, Id) ->
    set_running(C, Id, 120),
    {ok, Job} = talk_record_job_repo:find_tx(C, Id),
    ?assertEqual({error, lease_lost}, talk_record_job_repo:renew_lease_tx(C, Job, 120)),
    ?assertEqual({error, lease_lost}, talk_record_job_repo:finish_success_tx(C, Id, token())),
    ?assertEqual(
        {error, lease_lost}, talk_record_job_repo:finish_failed_tx(C, Id, token(), <<"timeout">>)
    ),
    ?assertEqual(
        {error, lease_lost}, talk_record_job_repo:finish_retry_tx(C, Id, token(), <<"timeout">>, 5)
    ),
    ?assertEqual(
        {error, lease_lost},
        talk_record_job_repo:finish_suspended_tx(C, Id, token(), <<"waiting_processor">>)
    ),
    ?assertEqual(<<"running">>, state(C, Id)).

expired_client_is_not_requeued(C, Server, Client) ->
    set_running(C, Server, -1),
    set_running(C, Client, -1),
    ?assertEqual({ok, 1}, talk_record_job_repo:reap_expired_tx(C)),
    ?assertEqual(<<"queued">>, state(C, Server)),
    ?assertEqual(<<"running">>, state(C, Client)).

failed_or_suspended_client_is_not_retried(C, Client, Summary) ->
    {ok, [_]} = elib_pg:query(
        C,
        <<
            "UPDATE talk_record_job SET state='failed',finished_at=now(),"
            "error_code='provider_outcome_unknown' WHERE id=$1 RETURNING id"
        >>,
        [Client]
    ),
    ?assertEqual({error, not_failed}, talk_record_job_repo:retry_failed_tx(C, Client)),
    {ok, #{<<"state">> := <<"failed">>, <<"error_code">> := <<"provider_outcome_unknown">>}} =
        talk_record_job_repo:find_tx(C, Client),
    {ok, [_]} = elib_pg:query(
        C,
        <<"UPDATE talk_record_job SET state='retry_wait',suspended=true WHERE id=$1 RETURNING id">>,
        [Summary]
    ),
    ?assertEqual({ok, 0}, talk_record_job_repo:unsuspend_kind_tx(C, <<"summarize">>)),
    {ok, #{<<"suspended">> := true}} = talk_record_job_repo:find_tx(C, Summary).

sql_constraints(C, RecordId, Client) ->
    lists:foreach(
        fun({Mode, Kind}) ->
            Id = elib_tsid:generate(attachment),
            reject_sql(
                C,
                <<
                    "INSERT INTO talk_record_job(id,record_id,kind,dedupe_key,execution_mode)"
                    "VALUES($1,$2,$3,$4,$5)"
                >>,
                [Id, RecordId, Kind, integer_to_binary(Id), Mode]
            )
        end,
        [
            {<<"unknown">>, <<"transcribe">>},
            {<<"client_byok">>, <<"assemble">>},
            {<<"client_byok">>, <<"export">>},
            {<<"client_byok">>, <<"purge">>}
        ]
    ),
    reject_sql(C, <<"UPDATE talk_record_job SET execution_mode='server_local' WHERE id=$1">>, [
        Client
    ]),
    {ok, #{<<"execution_mode">> := <<"client_byok">>}} = talk_record_job_repo:find_tx(C, Client).

reject_sql(C, Sql, Params) ->
    {ok, []} = elib_pg:query(C, <<"SAVEPOINT execution_mode_rejection">>, []),
    Result = elib_pg:query(C, Sql, Params),
    {ok, []} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT execution_mode_rejection">>, []),
    {ok, []} = elib_pg:query(C, <<"RELEASE SAVEPOINT execution_mode_rejection">>, []),
    ?assertMatch({error, {error, _, <<"23514">>, _, _, _}}, Result).

cleanup_unblock(C, Client) ->
    {ok, [_]} = elib_pg:query(
        C,
        <<
            "UPDATE talk_record_job SET state='retry_wait',finished_at=NULL,suspended=true,"
            "error_code='processor_cleanup_pending' WHERE id=$1 RETURNING id"
        >>,
        [Client]
    ),
    ?assertEqual({ok, 0}, talk_record_processor_run_repo:unblock_waiting_tx(C)),
    {ok, #{
        <<"state">> := <<"retry_wait">>,
        <<"suspended">> := true,
        <<"error_code">> := <<"processor_cleanup_pending">>
    }} = talk_record_job_repo:find_tx(C, Client).

%% Two controller-owned connections to the SAME dedicated DB, both in caller
%% transactions. Execute the first real down-migration statement, then prove a
%% concurrent INSERT cannot pass its lock. This does not execute DROP/rollback.
rollback_lock(MigrationC, OtherC, RecordId, DownPath) ->
    IdentitySql =
        <<"SELECT current_database() AS db,inet_server_addr()::text AS host,inet_server_port() AS port">>,
    {ok, [Identity]} = elib_pg:query(MigrationC, IdentitySql, []),
    {ok, [Identity]} = elib_pg:query(OtherC, IdentitySql, []),
    {ok, Sql} = file:read_file(DownPath),
    [LockSql, _] = binary:split(Sql, <<";">>),
    ?assertNotEqual(
        nomatch,
        binary:match(
            LockSql,
            <<"LOCK TABLE public.talk_record_job IN ACCESS EXCLUSIVE MODE">>
        )
    ),
    ?assertEqual(nomatch, binary:match(LockSql, <<"IF EXISTS">>)),
    {ok, []} = elib_pg:query(MigrationC, LockSql, []),
    {ok, [#{<<"held">> := true}]} = elib_pg:query(
        MigrationC,
        <<
            "SELECT EXISTS(SELECT 1 FROM pg_locks WHERE pid=pg_backend_pid() AND granted"
            " AND relation='public.talk_record_job'::regclass AND mode='AccessExclusiveLock') AS held"
        >>,
        []
    ),
    {ok, []} = elib_pg:query(OtherC, <<"SAVEPOINT rollback_lock_insert">>, []),
    {ok, []} = elib_pg:query(OtherC, <<"SET LOCAL lock_timeout='200ms'">>, []),
    Id = elib_tsid:generate(attachment),
    Result = elib_pg:query(
        OtherC,
        <<
            "INSERT INTO public.talk_record_job(id,record_id,kind,dedupe_key,execution_mode)"
            "VALUES($1,$2,'transcribe',$3,'client_byok')"
        >>,
        [Id, RecordId, integer_to_binary(Id)]
    ),
    {ok, []} = elib_pg:query(OtherC, <<"ROLLBACK TO SAVEPOINT rollback_lock_insert">>, []),
    {ok, []} = elib_pg:query(OtherC, <<"RELEASE SAVEPOINT rollback_lock_insert">>, []),
    ?assertMatch({error, {error, _, <<"55P03">>, _, _, _}}, Result),
    {ok, concurrent_client_insert_blocked}.

governance_cancels_both_modes(C, RecordId, Client, Summary) ->
    ?assertEqual({ok, cancelled}, talk_record_job_repo:cancel_job_tx(C, Summary)),
    {ok, [_]} = elib_pg:query(
        C,
        <<"UPDATE talk_record_job SET state='retry_wait',finished_at=NULL WHERE id=$1 RETURNING id">>,
        [Client]
    ),
    ?assertEqual({ok, 2}, talk_record_job_repo:cancel_record_jobs_tx(C, RecordId)),
    ?assertEqual(<<"cancelled">>, state(C, Client)),
    ?assertEqual(<<"cancelled">>, state(C, Summary)).

set_running(C, Id, Seconds) ->
    {ok, [_]} = elib_pg:query(
        C,
        <<
            "UPDATE talk_record_job SET state='running',attempt=1,lease_token=$2,"
            "lease_owner='mode-check',lease_until=clock_timestamp()+make_interval(secs=>$3) WHERE id=$1 RETURNING id"
        >>,
        [Id, token(), Seconds]
    ),
    ok.

state(C, Id) ->
    {ok, #{<<"state">> := State}} = talk_record_job_repo:find_tx(C, Id),
    State.

token() -> <<"7a47d8c7-8151-46c5-95d4-c97ccabbe425">>.
