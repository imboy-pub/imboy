-module(talk_record_export_relay_tests).
-include_lib("eunit/include/eunit.hrl").

export_pipeline_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun talk_record_t19_support:teardown_all/1, fun(State) ->
            ?_test(pipeline(State))
        end}}.

setup() ->
    State = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            C = maps:get(conn, State),
            talk_record_t19_support:sql(C, "DELETE FROM attachment_derived_write"),
            talk_record_t19_support:seed(State),
            ok = talk_record_upload_test_support:configure(),
            State
        end,
        fun talk_record_t19_support:teardown_all/1
    ).

pipeline(State) ->
    C = maps:get(conn, State),
    Uid = 700001,
    {ok, Rid} = talk_record_t15_support:create_record(Uid, 901, <<"export relay">>),
    _ = talk_record_t15_support:seed_transcript(C, Rid, 1),
    Sid = talk_record_t15_support:seed_speaker(C, Rid, <<"S1">>, #{display_name => <<"speaker">>}),
    _ = talk_record_t15_support:seed_segment(C, Rid, 1, 0, Sid, 0, <<"first paragraph">>),
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"export-relay-key">>, #{
        <<"format">> => <<"md">>
    }),
    ?assertEqual(<<"queued">>, maps:get(<<"state">>, P)),
    Eid = maps:get(<<"export_id">>, P),
    {ok, Job} = claim(),
    ?assertEqual(maps:get(<<"job_id">>, P), maps:get(<<"id">>, Job)),
    {ok, Ready} = talk_record_export_job:run(Job),
    ?assertEqual(<<"ready">>, maps:get(<<"state">>, Ready)),
    ?assert(is_integer(maps:get(<<"attachment_id">>, Ready))),
    ?assertEqual(false, maps:is_key(<<"content">>, Ready)),
    {ok, #{<<"ticket">> := Ticket}} = talk_record_export_logic:download(Uid, Rid, Eid),
    {ok, Stream} = talk_record_export_download:verify_ticket(Ticket),
    ?assertNotEqual(nomatch, binary:match(maps:get(<<"content">>, Stream), <<"first paragraph">>)),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket(Ticket, Uid, Rid + 1, Eid)
    ),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket(Ticket, Uid, Rid, Eid + 1)
    ),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket(Ticket, 700002, Rid, Eid)
    ),
    {ok, Asset} = talk_record_export_repo:artifact_tx(C, maps:get(<<"attachment_id">>, Ready)),
    ?assertEqual(false, talk_record_attachment_adapter:authorize(Uid, Asset)),
    ?assertEqual({error, forbidden}, attach_logic:view_url(Uid, maps:get(<<"path">>, Asset))),
    blocked_policy(Uid, Rid, Eid, Ticket),
    grant_recheck(C, Rid),
    lease_loss(C, Uid, Rid),
    snapshot_errors(C, Uid, Rid),
    read_failures(C, Uid, Rid),
    publish_fence(C, Uid, Rid),
    concurrent_write_guard(C, Uid, Rid),
    unknown_guard(C, Uid, Rid),
    retention_blocks_sweep(C, Uid, Rid),
    expiry_error_rolls_back(C, Rid, Eid),
    expire(C, Rid, Eid, Ready),
    failed_export_poll(C, Uid, Rid),
    released_cleanup_retry(C, Uid),
    no_ref_cleanup(C, Uid),
    ok.

claim() ->
    elib_pg:with_tx(fun(C) ->
        talk_record_job_repo:claim_next_tx(
            C,
            <<"relay-export">>,
            <<"11111111-2222-4333-8444-555555555555">>,
            300
        )
    end).

expiry_error_rolls_back(C, Rid, Eid) ->
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_export SET expires_at=now()-interval '1 second' WHERE id=$1">>,
        [Eid]
    ),
    {ok, Before} = talk_record_export_repo:find_tx(C, Rid, Eid),
    ?assertEqual(<<"ready">>, maps:get(<<"state">>, Before)),
    ok = meck:new(talk_record_attachment_adapter, [passthrough, no_link]),
    try
        ok = meck:expect(
            talk_record_attachment_adapter,
            evaluate_release_blockers_tx,
            fun
                (_Conn, TargetRid) when TargetRid =:= Rid -> {error, injected_failure};
                (OtherConn, OtherRid) -> meck:passthrough([OtherConn, OtherRid])
            end
        ),
        ?assertEqual({error, injected_failure}, talk_record_export_artifact:sweep()),
        ?assert(
            meck:called(talk_record_attachment_adapter, evaluate_release_blockers_tx, ['_', Rid])
        )
    after
        meck:unload(talk_record_attachment_adapter)
    end,
    {ok, After} = talk_record_export_repo:find_tx(C, Rid, Eid),
    ?assertEqual(<<"ready">>, maps:get(<<"state">>, After)),
    ?assertEqual(maps:get(<<"updated_at">>, Before), maps:get(<<"updated_at">>, After)).

blocked_policy(Uid, Rid, Eid, Ticket) ->
    talk_record_t19_support:with_env(talk_record_export_policy, #{enabled => false}, fun() ->
        ?assertEqual(
            {error, export_disabled_by_policy}, talk_record_export_logic:download(Uid, Rid, Eid)
        ),
        ?assertEqual(
            {error, export_disabled_by_policy}, talk_record_export_download:verify_ticket(Ticket)
        )
    end).

grant_recheck(C, Rid) ->
    _ = talk_record_t15_support:add_grant(C, Rid, 700002, 700001, <<"viewer">>),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record_grant SET allow_export=true WHERE record_id=$1">>, [Rid]
    ),
    {ok, created, P} = talk_record_export_logic:create(700002, Rid, <<"grant-export">>, #{}),
    {ok, J} = claim(),
    {ok, _} = talk_record_export_job:run(J),
    Eid = maps:get(<<"export_id">>, P),
    {ok, #{<<"ticket">> := T}} = talk_record_export_logic:download(700002, Rid, Eid),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record_grant SET allow_export=false WHERE record_id=$1">>, [Rid]
    ),
    ?assertEqual({error, forbidden}, talk_record_export_logic:download(700002, Rid, Eid)),
    ?assertEqual({error, forbidden}, talk_record_export_download:verify_ticket(T)).

lease_loss(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"lost-export">>, #{}),
    {ok, J} = claim(),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET lease_until=now()-interval '1 second' WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    ?assertEqual({error, lease_lost}, talk_record_export_job:run(J)),
    {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
    ?assertEqual(<<"queued">>, maps:get(<<"state">>, Row)),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

snapshot_errors(C, Uid, Rid) ->
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"read-error-export">>, #{}),
    {ok, J} = claim(),
    ok = meck:new(talk_record_speaker_repo, [passthrough, no_link]),
    meck:expect(talk_record_speaker_repo, list_by_record_tx, fun(_, _) ->
        {error, injected_read_error}
    end),
    try
        ?assertEqual({error, injected_read_error}, talk_record_export_job:run(J))
    after
        meck:unload(talk_record_speaker_repo)
    end,
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

unknown_guard(C, Uid, Rid) ->
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"unknown-export">>, #{}),
    {ok, J} = claim(),
    {ok, Prepared} = elib_pg:with_tx(fun(X) -> talk_record_export_job:prepare_tx(X, J) end),
    A = maps:get(artifact, Prepared),
    Aid = maps:get(attachment_id, A),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE attachment_derived_write SET state='writing' WHERE attachment_id=$1">>, [Aid]
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"unknown">>}]} = elib_pg:query(
        C,
        <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>,
        [Aid]
    ),
    ?assertMatch(
        {error, _},
        elib_pg:execute(
            C,
            <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=$1">>,
            [Aid]
        )
    ),
    ?assertEqual({error, artifact_unavailable}, attachment_derived_artifact:read(Aid)).

expire(C, Rid, Eid, Ready) ->
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record_export SET expires_at=now()-interval '1 second' WHERE id=$1">>, [
            Eid
        ]
    ),
    {ok, _} = talk_record_export_artifact:sweep(),
    {ok, Row} = talk_record_export_repo:find_tx(C, Rid, Eid),
    ?assertEqual(<<"expired">>, maps:get(<<"state">>, Row)),
    {ok, [#{<<"lifecycle_state">> := <<"delete_intent">>}]} = elib_pg:query(
        C,
        <<"SELECT lifecycle_state FROM attachment WHERE id=$1">>,
        [maps:get(<<"attachment_id">>, Ready)]
    ),
    {ok, _} = attachment_delete_worker:run_batch(),
    {ok, [#{<<"job_state">> := <<"done">>}]} = elib_pg:query(
        C,
        <<"SELECT q.job_state FROM attachment_object_deletion q JOIN attachment a ON a.path=q.path WHERE a.id=$1">>,
        [maps:get(<<"attachment_id">>, Ready)]
    ),
    {ok, [#{<<"path">> := Path, <<"storage_container">> := Container}]} = elib_pg:query(
        C,
        <<"SELECT path,storage_container FROM attachment WHERE id=$1">>,
        [maps:get(<<"attachment_id">>, Ready)]
    ),
    File = filename:join([
        os:getenv("TALK_UPLOAD_DISK_ROOT"), binary_to_list(Container), binary_to_list(Path)
    ]),
    ?assertEqual(false, filelib:is_file(File)).

read_failures(C, Uid, Rid) ->
    lists:foreach(
        fun({Module, Function, Arity, Key}) ->
            {ok, created, P} = talk_record_export_logic:create(Uid, Rid, Key, #{}),
            {ok, J} = claim(),
            ok = meck:new(Module, [passthrough, no_link]),
            Expect =
                case Arity of
                    2 ->
                        fun(_, _) -> {error, injected_read_error} end;
                    4 ->
                        fun
                            (_, _, _, #{cursor := undefined}) ->
                                {ok, [
                                    #{<<"id">> => N, <<"start_ms">> => N}
                                 || N <- lists:seq(1, 200)
                                ]};
                            (_, _, _, _) ->
                                {error, injected_read_error}
                        end
                end,
            meck:expect(Module, Function, Expect),
            try
                ?assertEqual({error, injected_read_error}, talk_record_export_job:run(J))
            after
                meck:unload(Module)
            end,
            {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
            ?assertEqual(null, maps:get(<<"attachment_id">>, Row)),
            cancel(C, J)
        end,
        [
            {talk_record_summary_repo, find_by_record_tx, 2, <<"summary-error">>},
            {talk_record_segment_repo, page_tx, 4, <<"second-page-error">>}
        ]
    ).

publish_fence(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"publish-lease-lost">>, #{}),
    {ok, J} = claim(),
    ok = meck:new(attachment_storage_disk, [passthrough, no_link]),
    meck:expect(attachment_storage_disk, put_snapshot, fun(I, B, E) ->
        Result = meck:passthrough([I, B, E]),
        {ok, _} = elib_pg:execute(
            C,
            <<"UPDATE talk_record_job SET lease_until=now()-interval '1 second' WHERE id=$1">>,
            [maps:get(<<"id">>, J)]
        ),
        Result
    end),
    try
        ?assertEqual({error, lease_lost}, talk_record_export_job:run(J))
    after
        meck:unload(attachment_storage_disk)
    end,
    {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
    ?assertEqual(<<"queued">>, maps:get(<<"state">>, Row)),
    cancel(C, J).

concurrent_write_guard(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"concurrent-guard">>, #{}),
    {ok, J} = claim(),
    Parent = self(),
    ok = meck:new(attachment_storage_disk, [passthrough, no_link]),
    meck:expect(attachment_storage_disk, put_snapshot, fun(I, B, E) ->
        Parent ! {write_started, self()},
        receive
            allow_write -> meck:passthrough([I, B, E])
        after 5000 -> {error, test_timeout}
        end
    end),
    Worker = spawn(fun() -> Parent ! {write_result, talk_record_export_job:run(J)} end),
    try
        receive
            {write_started, Writer} ->
                {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
                ?assertMatch(
                    {error, _},
                    elib_pg:execute(
                        C,
                        <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=$1">>,
                        [maps:get(<<"attachment_id">>, Row)]
                    )
                ),
                Writer ! allow_write
        after 5000 -> erlang:error({write_not_started, Worker})
        end,
        receive
            {write_result, {ok, _}} -> ok
        after 5000 -> erlang:error(write_not_finished)
        end
    after
        meck:unload(attachment_storage_disk)
    end.

cancel(C, J) ->
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ),
    ok.

retention_blocks_sweep(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"retention-export">>, #{}),
    {ok, J} = claim(),
    {ok, Ready} = talk_record_export_job:run(J),
    Aid = maps:get(<<"attachment_id">>, Ready),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record SET retention_until=now()+interval '1 day' WHERE id=$1">>, [Rid]
    ),
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_export SET expires_at=now()-interval '1 second' WHERE id=$1">>,
        [maps:get(<<"export_id">>, P)]
    ),
    {ok, _} = talk_record_export_artifact:sweep(),
    {ok, [#{<<"state">> := <<"bound">>}]} = elib_pg:query(
        C,
        <<"SELECT state FROM talk_record_attachment_ref WHERE attachment_id=$1">>,
        [Aid]
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(
            C,
            <<
                "SELECT 1 FROM attachment_object_deletion q JOIN attachment a"
                " ON a.path=q.path WHERE a.id=$1"
            >>,
            [Aid]
        )
    ),
    {ok, _} = elib_pg:execute(C, <<"UPDATE talk_record SET retention_until=NULL WHERE id=$1">>, [
        Rid
    ]),
    ok.

no_ref_cleanup(C, Uid) ->
    {ok, Rid} = talk_record_t15_support:create_record(Uid, 902, <<"unpublished artifact">>),
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"no-ref-export">>, #{}),
    {ok, J} = claim(),
    Key = <<"unpublished/", (integer_to_binary(Rid))/binary, ".txt">>,
    Meta = #{
        <<"path">> => Key,
        <<"url">> => <<>>,
        <<"name">> => <<"unpublished.txt">>,
        <<"size">> => 1,
        <<"mime_type">> => <<"text/plain">>,
        <<"file_hash256">> => <<"h">>,
        <<"scope">> => <<"talk_record">>,
        <<"scope_ref">> => integer_to_binary(Rid)
    },
    Source = #{
        job_id => maps:get(<<"id">>, J), attempt => maps:get(<<"attempt">>, J), actor_id => Uid
    },
    {ok, Aid} = elib_pg:with_tx(fun(X) ->
        attachment_derived_artifact:reserve_tx(X, Source, Meta)
    end),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE attachment_derived_write SET state='writing' WHERE attachment_id=$1">>, [Aid]
    ),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE attachment_derived_write SET state='unknown' WHERE attachment_id=$1">>, [Aid]
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(C, <<"SELECT 1 FROM talk_record_attachment_ref WHERE record_id=$1">>, [Rid])
    ),
    {ok, _, _} = talk_record_delete_logic:delete(Uid, Rid, <<"1">>),
    {ok, <<"pending">>} = elib_pg:with_tx(fun(X) ->
        talk_record_cleanup_logic:refresh_cleanup_state_tx(X, Rid)
    end),
    {ok, Detail} = talk_record_delete_logic:deleted_detail(Uid, Rid),
    ?assertEqual(<<"pending">>, maps:get(<<"cleanup_status">>, Detail)),
    ?assertEqual(<<"write_outcome_unknown">>, maps:get(<<"cleanup_reason">>, Detail)),
    ?assertEqual(false, maps:is_key(<<"path">>, Detail)),
    ?assertEqual(false, maps:is_key(<<"job_id">>, Detail)),
    ?assertEqual({error, not_found}, talk_record_delete_logic:deleted_detail(700002, Rid)).

released_cleanup_retry(C, Uid) ->
    {ok, Rid} = talk_record_t15_support:create_record(Uid, 903, <<"released cleanup retry">>),
    _ = talk_record_t15_support:seed_transcript(C, Rid, 1),
    _ = talk_record_t15_support:seed_segment(C, Rid, 1, 0, null, 0, <<"synthetic cleanup">>),
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"released-retry">>, #{}),
    {ok, J} = claim(),
    {ok, E} = talk_record_export_job:run(J),
    Aid = maps:get(<<"attachment_id">>, E),
    {ok, [#{<<"path">> := Path}]} = elib_pg:query(
        C, <<"SELECT path FROM attachment WHERE id=$1">>, [Aid]
    ),
    %% Synthetic capability fixture: no production capability lifetime is changed.
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO attach_pending(object_key,bucket,scope,creator_user_id,"
            "session_state,upload_expires_at,confirmed_attachment_id) VALUES($1,'fixture','talk_record',$2,"
            "'registered',clock_timestamp()+interval '1 second',NULL)"
        >>,
        [Path, Uid]
    ),
    {ok, _, _} = talk_record_delete_logic:delete(Uid, Rid, <<"1">>),
    {ok, #{cleanup_status := <<"retained_by_other_reference">>}} = elib_pg:with_tx(fun(X) ->
        talk_record_cleanup_logic:execute_purge_tx(X, #{<<"record_id">> => Rid})
    end),
    {ok, [#{<<"state">> := <<"released">>}]} = elib_pg:query(
        C,
        <<"SELECT state FROM talk_record_attachment_ref WHERE record_id=$1">>,
        [Rid]
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(C, <<"SELECT 1 FROM attachment_object_deletion WHERE path=$1">>, [Path])
    ),
    Collect = elib_tsid:generate(attachment),
    {ok, 1} = elib_pg:execute(
        C,
        <<
            "INSERT INTO user_collect(id,user_id,status,attach_file_hash256)"
            " SELECT $1,700002,1,file_hash256 FROM attachment WHERE id=$2"
        >>,
        [Collect, Aid]
    ),
    wait_cap_expired(C, Path, erlang:monotonic_time(millisecond) + 3000),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    ?assertEqual(
        {ok, <<"retained_by_other_reference">>}, talk_record_delete_repo:cleanup_status_tx(C, Rid)
    ),
    ?assertEqual(
        {ok, []},
        elib_pg:query(C, <<"SELECT 1 FROM attachment_object_deletion WHERE path=$1">>, [Path])
    ),
    {ok, 1} = elib_pg:execute(C, <<"DELETE FROM user_collect WHERE id=$1">>, [Collect]),
    {ok, _} = talk_record_job_worker:refresh_cleanup_pending(),
    {ok, [_]} = elib_pg:query(C, <<"SELECT 1 FROM attachment_object_deletion WHERE path=$1">>, [
        Path
    ]),
    ?assertEqual({ok, <<"pending">>}, talk_record_delete_repo:cleanup_status_tx(C, Rid)),
    %% Repeated intent coordination remains idempotent with already released refs.
    {ok, <<"pending">>} = elib_pg:with_tx(fun(X) ->
        talk_record_cleanup_logic:refresh_cleanup_state_tx(X, Rid)
    end),
    {ok, _} = elib_pg:execute(
        C,
        <<
            "UPDATE talk_record_job SET state='cancelled',finished_at=now()"
            " WHERE record_id=$1 AND kind='purge' AND state='queued'"
        >>,
        [Rid]
    ),
    ok.
wait_cap_expired(C, Path, D) ->
    {ok, [#{<<"expired">> := Expired}]} = elib_pg:query(
        C,
        <<"SELECT upload_expires_at<=clock_timestamp() expired FROM attach_pending WHERE object_key=$1">>,
        [Path]
    ),
    case Expired of
        true ->
            ok;
        false ->
            true = erlang:monotonic_time(millisecond) < D,
            receive
            after 10 -> wait_cap_expired(C, Path, D)
            end
    end.

failed_export_poll(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"stale-export-poll">>, #{}),
    {ok, _} = elib_pg:execute(
        C, <<"UPDATE talk_record SET content_revision=content_revision+1 WHERE id=$1">>, [Rid]
    ),
    {ok, #{outcome := failed, error_code := input_stale}} = talk_record_job_worker:run_once(),
    {ok, S} = talk_record_export_logic:state(Uid, Rid, maps:get(<<"export_id">>, P)),
    ?assertEqual(<<"failed">>, maps:get(<<"state">>, S)).
