-module(talk_record_provider_constraints_pg_checks).
-moduledoc "Controller-owned real PG CHECK matrix; no connections, migration or application table writes.".
-export([run_tx/1, matrix/0, cancelled_run_cleanup_tx/3]).
-include_lib("eunit/include/eunit.hrl").

%% Caller provides an approved migrated-224 connection and rollback transaction.
%% Clone actual production CHECK definitions, not hand-reimplemented predicates.
run_tx(C) ->
    {ok, []} = elib_pg:query(
        C,
        <<
            "CREATE TEMP TABLE provider_constraint_probe (assembly jsonb,"
            "source_issued_at bigint,source_expires_at bigint,intent_id bigint,request_hash char(64)) ON COMMIT DROP"
        >>,
        []
    ),
    lists:foreach(
        fun(Name) -> copy_constraint(C, Name) end,
        [<<"provider_task_assembly_shape">>, <<"provider_task_source_window">>]
    ),
    Cases = matrix(),
    lists:foreach(fun(V) -> check(C, V) end, Cases),
    {ok, #{executed => length(Cases), rejected => length([ok || {reject, _, _} <- Cases])}}.
copy_constraint(C, Name) ->
    {ok, [#{<<"definition">> := Definition}]} = elib_pg:query(
        C,
        <<
            "SELECT pg_get_constraintdef(oid) AS definition FROM pg_constraint"
            " WHERE conrelid='public.talk_record_provider_task'::regclass AND conname=$1"
        >>,
        [Name]
    ),
    %% Only a trusted, fixed-name catalog CHECK is incorporated into test DDL.
    <<"CHECK ", _/binary>> = Definition,
    {ok, []} = elib_pg:query(
        C, <<"ALTER TABLE provider_constraint_probe ADD ", Definition/binary>>, []
    ).
check(C, {Expected, Assembly, Window}) ->
    {ok, []} = elib_pg:query(C, <<"SAVEPOINT provider_check_case">>, []),
    Result = elib_pg:execute(
        C,
        <<
            "INSERT INTO provider_constraint_probe"
            " (assembly,source_issued_at,source_expires_at,intent_id,request_hash) VALUES($1::jsonb,$2,$3,$4,$5)"
        >>,
        [jsone:encode(Assembly) | Window]
    ),
    {ok, []} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT provider_check_case">>, []),
    {ok, []} = elib_pg:query(C, <<"RELEASE SAVEPOINT provider_check_case">>, []),
    case Expected of
        allow -> ?assertMatch({ok, 1}, Result);
        reject -> ?assertMatch({error, {error, _, <<"23514">>, _, _, _}}, Result)
    end.
matrix() ->
    A = assembly(),
    Empty = [null, null, null, null],
    Full = [100, 400, 1, hash()],
    Valid = [{allow, A, Empty}, {allow, A, Full}, {allow, A, [100, 101, 1, hash()]}],
    Nulls = [{reject, A#{K => null}, Empty} || K <- maps:keys(A)],
    Types = [{reject, A#{K => V}, Empty} || {K, V} <- invalid_types()],
    Mixed = [
        {reject, A, [
            case (Mask band (1 bsl N)) of
                0 -> null;
                _ -> lists:nth(N + 1, Full)
            end
         || N <- lists:seq(0, 3)
        ]}
     || Mask <- lists:seq(1, 14)
    ],
    Limits = [
        {reject, A, [100, 401, 1, hash()]},
        {reject, A, [100, 100, 1, hash()]},
        {reject, A, [0, 100, 1, hash()]},
        {reject, A#{<<"byte_size">> => 134217728}, Empty},
        {reject, A#{<<"duration_ms">> => 7200001}, Empty},
        {reject, A#{<<"id">> => 9223372036854775808}, Empty}
    ],
    Valid ++ Nulls ++ Types ++ Mixed ++ Limits ++
        [{reject, V, Empty} || V <- [null, [], <<"invalid">>]].
invalid_types() ->
    [
        {<<"id">>, <<"1">>},
        {<<"id">>, true},
        {<<"id">>, 1.5},
        {<<"byte_size">>, <<"100">>},
        {<<"duration_ms">>, false},
        {<<"sha256">>, 100},
        {<<"mime_type">>, []},
        {<<"mime_type">>, <<"audio/wav">>}
    ].
assembly() ->
    #{
        <<"id">> => 1,
        <<"sha256">> => hash(),
        <<"byte_size">> => 100,
        <<"duration_ms">> => 1000,
        <<"mime_type">> => <<"audio/aac">>
    }.
hash() -> binary:copy(<<"a">>, 64).

%% Requires an approved real formal-table fixture: active query_only run,
%% running job, uncancelled task. Tests task cancel -> job cancellation and
%% task cancel -> expiry settlement independently, rolling back each case.
%% The caller must additionally exercise its real delete/governance operation;
%% raw state updates here do not prove endpoint ACLs or physical attachment GC.
cancelled_run_cleanup_tx(C, Tid, RunId) ->
    {ok, [#{<<"job_id">> := Jid}]} = elib_pg:query(
        C,
        <<"SELECT job_id FROM talk_record_client_run WHERE id=$1 AND execution_purpose='query_only' AND state='active'">>,
        [RunId]
    ),
    lists:foreach(
        fun(Action) ->
            {ok, []} = elib_pg:query(C, <<"SAVEPOINT provider_cancel_cleanup">>, []),
            {ok, 1} = talk_record_provider_repo:cancel_tx(C, Tid),
            case Action of
                job_cancel ->
                    {ok, cancelled} = talk_record_job_repo:cancel_job_tx(C, Jid);
                reaper ->
                    {ok, 1} = talk_record_client_repo:settle_tx(
                        C, RunId, <<"provider_outcome_unknown">>, <<"network_unknown">>
                    )
            end,
            {ok, [#{<<"state">> := State}]} = elib_pg:query(
                C, <<"SELECT state FROM talk_record_client_run WHERE id=$1">>, [RunId]
            ),
            ?assert(lists:member(State, [<<"cancelled">>, <<"provider_outcome_unknown">>])),
            {ok, []} = elib_pg:query(C, <<"ROLLBACK TO SAVEPOINT provider_cancel_cleanup">>, []),
            {ok, []} = elib_pg:query(C, <<"RELEASE SAVEPOINT provider_cancel_cleanup">>, [])
        end,
        [job_cancel, reaper]
    ),
    {ok, #{executed => 2}}.
