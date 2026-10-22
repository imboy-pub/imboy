-module(talk_record_processor_run_repo).
-moduledoc "本机处理执行证明；不凭PID消失或租约超时推断副本已清除。".
-export([reserve_tx/2, started/2, finish/2, unblock_waiting/0]).

reserve_tx(C, J) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT 1 FROM talk_record_processor_run r JOIN talk_record_job j ON j.id=r.job_id"
                " WHERE j.record_id=$1 AND r.state<>'stopped' LIMIT 1"
            >>,
            [maps:get(<<"record_id">>, J)]
        )
    of
        {ok, []} ->
            case
                elib_pg:execute(
                    C,
                    <<
                        "INSERT INTO talk_record_processor_run(job_id,attempt,lease_token,state)"
                        " VALUES($1,$2,$3,'prepared') ON CONFLICT DO NOTHING"
                    >>,
                    identity(J)
                )
            of
                {ok, 1} -> ok;
                {ok, 0} -> {error, processor_cleanup_pending};
                E -> E
            end;
        {ok, _} ->
            {error, processor_cleanup_pending};
        E ->
            E
    end.
started(J, Pid) ->
    case
        elib_pg:execute(
            <<
                "UPDATE talk_record_processor_run SET state='running',os_pid=$4,updated_at=now()"
                " WHERE job_id=$1 AND attempt=$2 AND lease_token=$3 AND state='prepared'"
            >>,
            identity(J) ++ [Pid]
        )
    of
        {ok, 1} -> ok;
        _ -> {error, processor_registration_failed}
    end.
finish(J, State) when State =:= <<"stopped">>; State =:= <<"unknown">> ->
    case
        elib_pg:execute(
            <<
                "UPDATE talk_record_processor_run SET state=$4,updated_at=now()"
                " WHERE job_id=$1 AND attempt=$2 AND lease_token=$3 AND (state<>'stopped' OR state=$4)"
            >>,
            identity(J) ++ [State]
        )
    of
        {ok, 1} -> ok;
        E -> E
    end.
identity(J) -> [maps:get(<<"id">>, J), maps:get(<<"attempt">>, J), maps:get(<<"lease_token">>, J)].

%% Resume only after positive stop+cleanup receipts, never from elapsed time.
unblock_waiting() ->
    elib_pg:execute(
        <<
            "UPDATE talk_record_job j SET suspended=false,state='queued',error_code=NULL,"
            "updated_at=now() WHERE j.suspended=true AND j.error_code='processor_cleanup_pending'"
            " AND NOT EXISTS(SELECT 1 FROM talk_record_processor_run r JOIN talk_record_job source ON "
            "source.id=r.job_id"
            " WHERE source.record_id=j.record_id AND r.state<>'stopped')"
        >>,
        []
    ).
