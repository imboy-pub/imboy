-module(talk_record_client_reaper).
-moduledoc "Bounded client expiry barrier; never requeues a possible paid dispatch.".
-export([run_once/0]).

run_once() ->
    case talk_record_client_repo:expired_candidates() of
        {ok, Rows} -> reap(Rows, 0);
        E -> E
    end.

reap([], N) ->
    {ok, N};
reap([Row | Rows], N) ->
    case talk_record_client_ds:tx(fun(C) -> expire(C, Row) end) of
        {ok, Changed} -> reap(Rows, N + Changed);
        E -> E
    end.

%% Same record -> job -> run lock order as every foreground mutation.
%% This reconciles facts even after membership revocation; it grants no access.
expire(C, #{<<"record_id">> := Rid, <<"job_id">> := Jid, <<"id">> := Id}) ->
    _ = talk_record_client_gate:require(talk_record_client_repo:lock_record_tx(C, Rid)),
    _ = talk_record_client_gate:require(talk_record_client_repo:lock_job_tx(C, Rid, Jid)),
    Run = talk_record_client_gate:require(talk_record_client_repo:lock_run_tx(C, Rid, Jid, Id)),
    case talk_record_client_repo:expire_tx(C, Run) of
        {expired, _} -> {ok, 1};
        current -> {ok, 0}
    end.
