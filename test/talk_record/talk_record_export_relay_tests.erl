-module(talk_record_export_relay_tests).
-include_lib("eunit/include/eunit.hrl").

export_pipeline_test_() ->
    {timeout, 120,
        {setup, fun setup/0, fun teardown/1, fun(State) ->
            {timeout, 120, ?_test(pipeline(State))}
        end}}.

setup() ->
    State = talk_record_t19_support:setup_all(),
    talk_record_setup_fixture:prepare(
        State,
        fun() ->
            C = maps:get(conn, State),
            talk_record_t19_support:sql(C, "DELETE FROM attachment_derived_write"),
            talk_record_t19_support:seed(State),
            {ok, PgConf = #{start_mfa := {M, F, [Opts]}}} = application:get_env(imboy, pg_conf),
            Db = os:getenv("T05_PG_DB"),
            true = is_list(Db),
            ok = application:set_env(imboy, pg_conf, PgConf#{
                start_mfa := {M, F, [Opts#{database => Db}]}
            }),
            ok = talk_record_upload_test_support:configure(),
            State#{export_original_pg_conf => PgConf}
        end,
        fun talk_record_t19_support:teardown_all/1
    ).

teardown(#{export_original_pg_conf := PgConf} = State) ->
    ok = application:set_env(imboy, pg_conf, PgConf),
    talk_record_t19_support:teardown_all(State).

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
    ?assertMatch(
        {ok, {_, <<"talk-record-export.", _/binary>>}},
        talk_record_export_download:edge_target(Ticket)
    ),
    {ok, Stream} = talk_record_export_download:verify_ticket_ref(Ticket),
    ?assertEqual(false, maps:is_key(<<"content">>, Stream)),
    {ok, Bytes} = talk_record_export_artifact:read(Stream),
    ?assertNotEqual(nomatch, binary:match(Bytes, <<"first paragraph">>)),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket_ref(Ticket, Uid, Rid + 1, Eid)
    ),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket_ref(Ticket, Uid, Rid, Eid + 1)
    ),
    ?assertEqual(
        {error, export_ticket_invalid},
        talk_record_export_download:verify_ticket_ref(Ticket, 700002, Rid, Eid)
    ),
    {ok, Asset} = talk_record_export_repo:artifact_tx(C, maps:get(<<"attachment_id">>, Ready)),
    ?assertEqual(false, talk_record_attachment_adapter:authorize(Uid, Asset)),
    ?assertEqual({error, forbidden}, attach_logic:view_url(Uid, maps:get(<<"path">>, Asset))),
    blocked_policy(Uid, Rid, Eid, Ticket),
    grant_recheck(C, Rid),
    lease_loss(C, Uid, Rid),
    snapshot_errors(C, Uid, Rid),
    read_failures(C, Uid, Rid),
    second_page_failure(C, Uid),
    publish_fence(C, Uid, Rid),
    concurrent_write_guard(C, Uid, Rid),
    unknown_guard(C, Uid, Rid),
    unknown_after_remote_write(C, Uid, Rid),
    os_worker_killed(C, Uid, Rid),
    late_worker_receipt(C, Uid, Rid),
    receipt_capability_guard(C, Uid, Rid),
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
            {error, export_disabled_by_policy},
            talk_record_export_download:verify_ticket_ref(Ticket)
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
    ?assertEqual({error, forbidden}, talk_record_export_download:verify_ticket_ref(T)).

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
    {ok, PgConf = #{start_mfa := {Mod, Fun, [Opts]}}} = application:get_env(imboy, pg_conf),
    ok = application:set_env(imboy, pg_conf, PgConf#{start_mfa := {Mod, Fun, [Opts#{port => 1}]}}),
    try
        ?assertMatch({error, _}, talk_record_export_job:run(J))
    after
        application:set_env(imboy, pg_conf, PgConf)
    end,
    {ok, _} = elib_pg:execute(
        C,
        <<"UPDATE talk_record_job SET state='cancelled',finished_at=now() WHERE id=$1">>,
        [maps:get(<<"id">>, J)]
    ).

unknown_guard(C, Uid, Rid) ->
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"unknown-export">>, #{}),
    {ok, J} = claim(),
    {ok, Export} = elib_pg:with_tx(fun(X) -> talk_record_export_job:prepare_tx(X, J) end),
    Hash = binary:encode_hex(crypto:hash(sha256, <<"test">>), lowercase),
    {ok, A} = elib_pg:with_tx(fun(X) ->
        talk_record_export_artifact:prepare_tx(X, J, Export, #{size => 4, sha256 => Hash})
    end),
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
        fun({Table, Key}) ->
            {ok, created, P} = talk_record_export_logic:create(Uid, Rid, Key, #{}),
            {ok, J} = claim(),
            talk_record_t19_support:sql(C, [
                "ALTER TABLE ", Table, " RENAME TO export_fault_fixture"
            ]),
            try
                ?assertMatch({error, _}, talk_record_export_job:run(J))
            after
                talk_record_t19_support:sql(C, [
                    "ALTER TABLE export_fault_fixture RENAME TO ", Table
                ])
            end,
            {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
            ?assertEqual(null, maps:get(<<"attachment_id">>, Row)),
            cancel(C, J)
        end,
        [
            {"talk_record_summary", <<"summary-error">>},
            {"talk_record_segment", <<"segment-error">>}
        ]
    ).

unknown_after_remote_write(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"remote-unknown-export">>, #{}),
    {ok, J} = claim(),
    ok = meck:new(attachment_worker_port, [passthrough, no_link]),
    meck:expect(attachment_worker_port, upload_export, fun(Task) ->
        {ok, _} = meck:passthrough([Task]),
        {error, worker_outcome_unknown}
    end),
    try
        ?assertEqual({error, worker_outcome_unknown}, talk_record_export_job:run(J))
    after
        meck:unload(attachment_worker_port)
    end,
    {ok, Export} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
    Aid = maps:get(<<"attachment_id">>, Export),
    {ok, [Row]} = elib_pg:query(
        C,
        <<
            "SELECT a.*,w.state AS write_state FROM attachment a "
            "JOIN attachment_derived_write w ON w.attachment_id=a.id WHERE a.id=$1"
        >>,
        [Aid]
    ),
    ?assertEqual(<<"unknown">>, maps:get(<<"write_state">>, Row)),
    {ok, Location} = attachment_storage:frozen_location(maps:get(<<"path">>, Row), Row),
    {ok, #{size := Size}} = attachment_storage:head_snapshot(Location),
    ?assertEqual(maps:get(<<"size">>, Row), Size),
    ?assertMatch(
        {error, _},
        elib_pg:execute(
            C,
            <<"UPDATE attachment SET lifecycle_state='delete_intent' WHERE id=$1">>,
            [Aid]
        )
    ),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"unknown">>}]} = elib_pg:query(
        C, <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>, [Aid]
    ),
    cancel(C, J),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"complete">>}]} = elib_pg:query(
        C, <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>, [Aid]
    ).

os_worker_killed(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"os-worker-kill">>, #{}),
    {ok, J} = claim(),
    Base =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Path -> Path
        end,
    Dir = filename:join(
        Base,
        "c09-kill-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(16), lowercase))
    ),
    ok = file:make_dir(Dir),
    try
        ok = file:change_mode(Dir, 8#700),
        Prev = os:getenv("IMBOY_WORKER_TMPDIR"),
        true = os:putenv("IMBOY_WORKER_TMPDIR", Dir),
        try
            {ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {reuseaddr, true}]),
            try
                os_worker_killed_with_listener(C, Rid, P, J, Dir, Listener)
            after
                gen_tcp:close(Listener)
            end
        after
            case Prev of
                false -> os:unsetenv("IMBOY_WORKER_TMPDIR");
                _ -> os:putenv("IMBOY_WORKER_TMPDIR", Prev)
            end
        end
    after
        ok = file:del_dir_r(Dir)
    end.

os_worker_killed_with_listener(C, Rid, P, J, Dir, Listener) ->
    {ok, {_, Port}} = inet:sockname(Listener),
    Endpoint = iolist_to_binary(io_lib:format("http://127.0.0.1:~B", [Port])),
    Parent = self(),
    Server = spawn(fun() ->
        {ok, Socket} = gen_tcp:accept(Listener),
        {ok, <<"PUT ", _/binary>>} = gen_tcp:recv(Socket, 0, 10000),
        Parent ! put_entered,
        receive
            release -> gen_tcp:close(Socket)
        end
    end),
    try
        ok = meck:new(attachment_worker_port, [passthrough, no_link]),
        try
            meck:expect(attachment_worker_port, upload_export, fun(Task) ->
                meck:passthrough([Task#{endpoint => Endpoint}])
            end),
            {Runner, Monitor} = spawn_monitor(fun() ->
                Parent ! {killed_job_result, talk_record_export_job:run(J)}
            end),
            try
                os_worker_killed_assert(C, Rid, P, J, Dir)
            after
                exit(Runner, kill),
                try
                    receive
                        {'DOWN', Monitor, process, Runner, _} -> ok
                    after 5000 -> error(kill_runner_timeout)
                    end
                after
                    kill_test_workers(Dir)
                end
            end
        after
            meck:unload(attachment_worker_port)
        end
    after
        exit(Server, shutdown)
    end.

kill_test_workers(Dir) ->
    TaskFiles = filelib:wildcard(filename:join([Dir, "imboy-worker-*", "worker.task"])),
    Lines = string:split(os:cmd("ps -axo pid=,args="), "\n", all),
    lists:foreach(
        fun(TaskFile) ->
            lists:foreach(
                fun(Line) ->
                    [PidText | _] = string:lexemes(Line, " "),
                    _ = os:cmd("kill -KILL " ++ integer_to_list(list_to_integer(PidText))),
                    ok
                end,
                [L || L <- Lines, string:find(L, TaskFile) =/= nomatch]
            )
        end,
        TaskFiles
    ).

os_worker_killed_assert(C, Rid, P, J, Dir) ->
    receive
        put_entered -> ok
    after 30000 -> error(put_not_entered)
    end,
    [TaskFile] = filelib:wildcard(filename:join([Dir, "imboy-worker-*", "worker.task"])),
    Lines = string:split(os:cmd("ps -axo pid=,args="), "\n", all),
    [Line] = [L || L <- Lines, string:find(L, TaskFile) =/= nomatch],
    [PidText | _] = string:lexemes(Line, " "),
    Pid = list_to_integer(PidText),
    _ = os:cmd("kill -KILL " ++ integer_to_list(Pid)),
    receive
        {killed_job_result, R} -> ?assertEqual({error, worker_outcome_unknown}, R)
    after 30000 -> error(killed_worker_result_timeout)
    end,
    {ok, Export} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
    ?assertNotEqual(<<"ready">>, maps:get(<<"state">>, Export)),
    Aid = maps:get(<<"attachment_id">>, Export),
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
    cancel(C, J),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"unknown">>}]} = elib_pg:query(
        C,
        <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>,
        [Aid]
    ).

late_worker_receipt(C, Uid, Rid) ->
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"late-worker-receipt">>, #{}),
    {ok, J} = claim(),
    {ok, Export} = elib_pg:with_tx(fun(X) -> talk_record_export_job:prepare_tx(X, J) end),
    #{start_mfa := {_, _, [PgOptions]}} = config_ds:env(pg_conf),
    Binding = crypto:strong_rand_bytes(32),
    Render = #{
        pg_options => PgOptions,
        export => maps:with([<<"id">>, <<"record_id">>, <<"format">>], Export),
        revision => maps:get(<<"input_content_revision">>, J),
        source_job_id => maps:get(<<"id">>, J),
        source_attempt => maps:get(<<"attempt">>, J),
        binding => Binding
    },
    {ok, Generated} = attachment_worker_port:render_export(Render),
    {ok, Artifact} = elib_pg:with_tx(fun(X) ->
        talk_record_export_artifact:prepare_tx(X, J, Export, Generated)
    end),
    Aid = maps:get(attachment_id, Artifact),
    Token = crypto:strong_rand_bytes(32),
    {ok, Location} = attachment_derived_artifact:begin_export_write(
        Aid, crypto:hash(sha256, Token)
    ),
    Cfg = application:get_env(imboy, garage, #{}),
    Task = Render#{
        attachment_id => Aid,
        receipt_token => Token,
        endpoint => maps:get(endpoint, Cfg),
        ak => maps:get(access_key, Cfg),
        sk => maps:get(secret_key, Cfg),
        bucket => maps:get(container, Location),
        region => maps:get(region, Cfg, <<"garage">>),
        object_key => maps:get(key, Location),
        mime => maps:get(mime, Location),
        expected_sha256 => maps:get(sha256, Generated),
        expected_size => maps:get(size, Generated)
    },
    Parent = self(),
    Writer = spawn(fun() ->
        receive
            proceed ->
                Parent ! {late_worker_result, attachment_worker_port:upload_export(Task)}
        end
    end),
    {error, worker_outcome_unknown} = attachment_derived_artifact:finish_write(
        Aid, {error, worker_outcome_unknown}
    ),
    cancel(C, J),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"unknown">>}]} = elib_pg:query(
        C, <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>, [Aid]
    ),
    Writer ! proceed,
    receive
        {late_worker_result, {ok, _}} -> ok
    after 30000 -> error(late_worker_timeout)
    end,
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"complete">>}]} = elib_pg:query(
        C, <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>, [Aid]
    ).

receipt_capability_guard(C, Uid, Rid) ->
    {ok, created, _} = talk_record_export_logic:create(Uid, Rid, <<"receipt-capability">>, #{}),
    {ok, J} = claim(),
    {ok, Export} = elib_pg:with_tx(fun(X) -> talk_record_export_job:prepare_tx(X, J) end),
    Hash = binary:encode_hex(crypto:hash(sha256, <<"test">>), lowercase),
    {ok, Artifact} = elib_pg:with_tx(fun(X) ->
        talk_record_export_artifact:prepare_tx(X, J, Export, #{size => 4, sha256 => Hash})
    end),
    Aid = maps:get(attachment_id, Artifact),
    Token = crypto:strong_rand_bytes(32),
    {ok, Location} = attachment_derived_artifact:begin_export_write(
        Aid, crypto:hash(sha256, Token)
    ),
    DownPath = filename:join([
        code:priv_dir(imboy), "migrations", "00000262_export_worker_receipt.down.sql"
    ]),
    {ok, DownSql} = file:read_file(DownPath),
    [DownGuard | _] = binary:split(DownSql, <<"DROP FUNCTION">>),
    ?assertMatch({error, _}, epgsql:squery(C, DownGuard)),
    JobId = maps:get(<<"id">>, J),
    Attempt = maps:get(<<"attempt">>, J),
    Key = maps:get(key, Location),
    Sql = <<"SELECT fn_export_confirm_receipt($1,$2,$3,$4,$5,$6,$7) AS confirmed">>,
    Args = [Aid, Token, JobId, Attempt, Key, Hash, 4],
    {rollback, receipt_test_rollback} = elib_pg:with_tx(
        fun(X) ->
            {ok, [], []} = epgsql:squery(X, "SET LOCAL ROLE v5_app_runtime"),
            {ok, [#{<<"confirmed">> := false}]} = elib_pg:query(
                X,
                Sql,
                [Aid, crypto:strong_rand_bytes(32), JobId, Attempt, Key, Hash, 4]
            ),
            {ok, [#{<<"confirmed">> := false}]} = elib_pg:query(
                X,
                Sql,
                [Aid, Token, JobId, Attempt + 1, Key, Hash, 4]
            ),
            {ok, [#{<<"confirmed">> := false}]} = elib_pg:query(
                X,
                Sql,
                [Aid, Token, JobId, Attempt, <<"wrong-key">>, Hash, 4]
            ),
            {ok, [#{<<"confirmed">> := true}]} = elib_pg:query(X, Sql, Args),
            {ok, [#{<<"confirmed">> := false}]} = elib_pg:query(X, Sql, Args),
            throw(receipt_test_rollback)
        end,
        [{reraise, false}]
    ),
    lists:foreach(
        fun(Role) ->
            ?assertMatch(
                {rollback, _},
                elib_pg:with_tx(
                    fun(X) ->
                        {ok, [], []} = epgsql:squery(X, ["SET LOCAL ROLE ", Role]),
                        {ok, _} = elib_pg:query(X, Sql, Args)
                    end,
                    [{reraise, false}]
                )
            )
        end,
        ["v5_proof", "v5_worker"]
    ),
    ?assertMatch(
        {rollback, _},
        elib_pg:with_tx(
            fun(X) ->
                {ok, [], []} = epgsql:squery(X, "SET LOCAL ROLE v5_app_runtime"),
                {ok, _} = elib_pg:query(X, <<"SELECT * FROM attachment_export_write_receipt">>, [])
            end,
            [{reraise, false}]
        )
    ),
    cancel(C, J),
    {ok, _} = attachment_derived_artifact:reconcile(),
    {ok, [#{<<"state">> := <<"unknown">>}]} = elib_pg:query(
        C, <<"SELECT state FROM attachment_derived_write WHERE attachment_id=$1">>, [Aid]
    ).

second_page_failure(C, Uid) ->
    {ok, Rid} = talk_record_t15_support:create_record(Uid, 9903, <<"second page failure">>),
    _ = talk_record_t15_support:seed_transcript(C, Rid, 1),
    lists:foreach(
        fun(N) ->
            _ = talk_record_t15_support:seed_segment(C, Rid, 1, N, null, N, <<"fixture">>)
        end,
        lists:seq(1, 201)
    ),
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"second-page-fault">>, #{}),
    {ok, J} = claim(),
    talk_record_t19_support:sql(C, "ALTER TABLE talk_record_segment RENAME TO export_test_segment"),
    talk_record_t19_support:sql(C, <<
        "CREATE FUNCTION export_test_page_guard() RETURNS boolean LANGUAGE plpgsql AS $$ "
        "BEGIN IF strpos(current_query(),'AND (s.start_ms, s.id)') > 0 THEN "
        "RAISE EXCEPTION 'injected second page fault'; END IF; RETURN true; END $$"
    >>),
    talk_record_t19_support:sql(C, <<
        "CREATE VIEW talk_record_segment AS SELECT * FROM export_test_segment "
        "WHERE export_test_page_guard()"
    >>),
    try
        ?assertMatch({error, _}, talk_record_export_job:run(J)),
        {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
        ?assertEqual(null, maps:get(<<"attachment_id">>, Row))
    after
        talk_record_t19_support:sql(C, "DROP VIEW talk_record_segment"),
        talk_record_t19_support:sql(C, "DROP FUNCTION export_test_page_guard()"),
        talk_record_t19_support:sql(
            C, "ALTER TABLE export_test_segment RENAME TO talk_record_segment"
        )
    end,
    cancel(C, J).

publish_fence(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"publish-lease-lost">>, #{}),
    {ok, J} = claim(),
    ok = meck:new(attachment_worker_port, [passthrough, no_link]),
    meck:expect(attachment_worker_port, upload_export, fun(Task) ->
        Result = meck:passthrough([Task]),
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
        meck:unload(attachment_worker_port)
    end,
    {ok, Row} = talk_record_export_repo:find_tx(C, Rid, maps:get(<<"export_id">>, P)),
    ?assertEqual(<<"queued">>, maps:get(<<"state">>, Row)),
    cancel(C, J).

concurrent_write_guard(C, Uid, Rid) ->
    {ok, created, P} = talk_record_export_logic:create(Uid, Rid, <<"concurrent-guard">>, #{}),
    {ok, J} = claim(),
    Parent = self(),
    ok = meck:new(attachment_worker_port, [passthrough, no_link]),
    meck:expect(attachment_worker_port, upload_export, fun(Task) ->
        Parent ! {write_started, self()},
        receive
            allow_write -> meck:passthrough([Task])
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
        meck:unload(attachment_worker_port)
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
