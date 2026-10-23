-module(talk_record_client_recovery_repo).
-moduledoc "Whole-job durable facts, not caller assertions, decide whether no-charge recovery is safe.".
-export([facts_tx/2]).
facts_tx(C, Jid) ->
    case
        elib_pg:query(
            C,
            <<
                "SELECT EXISTS(SELECT 1 FROM talk_record_provider_task t WHERE t.job_id=$1) AS has_task,"
                " EXISTS(SELECT 1 FROM talk_record_client_run_part p JOIN talk_record_client_run r ON r.id=p.run_id"
                " WHERE r.job_id=$1 AND (p.provider_state<>'not_dispatched' OR p.dispatch_intent_id IS NOT NULL"
                " OR p.request_hash IS NOT NULL OR p.result IS NOT NULL OR p.result_hash IS NOT NULL"
                " OR p.provider_request_id IS NOT NULL)) AS has_provider_fact,"
                " EXISTS(SELECT 1 FROM talk_record_client_run r WHERE r.job_id=$1"
                " AND r.state IN ('provider_outcome_unknown','complete')) AS has_uncertain_run"
            >>,
            [Jid]
        )
    of
        {ok, [F]} -> {ok, F};
        _ -> {error, db_error}
    end.
