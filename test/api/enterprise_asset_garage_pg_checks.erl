%%% Real Garage plus disposable PostgreSQL; production default assembly, no stub.
-module(enterprise_asset_garage_pg_checks).
-export([run/0]).
-include_lib("eunit/include/eunit.hrl").

run() ->
    {ok, _} = application:ensure_all_started(inets),
    H = intbe02_http_support:setup_all(),
    try
        application:unset_env(imboy, eb_asset_object_store),
        stub_regression(),
        Config = #{
            endpoint => env("IMBOY_TEST_ENDPOINT"),
            bucket => <<"synthetic-enterprise-assets">>,
            region => <<"garage">>,
            access_key => env("IMBOY_TEST_ACCESS"),
            secret_key => env("IMBOY_TEST_SECRET")
        },
        application:set_env(imboy, garage, Config),
        S = cs_pg_test_fixture:new_scope(),
        ok = cs_pg_test_fixture:exec(
            <<"UPDATE organization_business_identity_assignment SET business_identity_id=$3,function_key='customer_service' WHERE organization_id=$1 AND user_id=$2">>,
            [maps:get(org_id, S), maps:get(actor_user_id, S), maps:get(service_identity_id, S)]
        ),
        journey(S),
        rollback_object(S),
        cleanup_recovery(S, Config),
        cleanup_concurrency(S),
        cleanup_ineligible(S),
        purge_regressions(),
        ok = eb_ports_tests:ports_contracts_match_declared_callbacks_test(),
        ok = eb_ports_tests:ports_have_explicit_frozen_contracts_test(),
        ok = eb_ports_tests:asset_port_callback_names_cannot_leak_storage_handle_test(),
        application:unset_env(imboy, garage),
        Org = maps:get(org_id, S),
        Ws = maps:get(workspace_id, S),
        FailedId = cs_pg_test_fixture:id(),
        ?assertEqual(
            {error, {object_store, storage_not_configured}},
            eb_asset_store:put_private(Org, Ws, #{
                id => FailedId,
                payload => <<"x">>,
                object_hash => eb_asset_content:sha256_hex(<<"x">>)
            })
        ),
        {ok, #{status := pending_confirm}} = eb_asset_store:fetch_asset(Org, Ws, FailedId),
        application:set_env(imboy, eb_asset_object_store, invalid),
        ?assertEqual({error, invalid_object_store}, eb_asset_store:put_private(Org, Ws, #{})),
        io:format(
            "PASS: default Garage assembly; real PG presign/PUT/confirm/content, ACL, duplicate/concurrent PUT safety, metadata reservation, missing and invalid config~n"
        )
    after
        intbe02_http_support:teardown_all(H),
        inttest_marker_db:release(H)
    end.

journey(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Bytes = <<"synthetic enterprise asset in real Garage">>,
    Base = #{
        workspace_id => Ws,
        conversation_id => maps:get(conversation_id, S),
        actor_user_id => maps:get(actor_user_id, S),
        key_ref => cs_pg_test_fixture:key_ref()
    },
    {ok, Presign} = enterprise_business_facade:request_presign(Org, Base#{
        mime => <<"text/plain">>,
        size_bytes => byte_size(Bytes),
        object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    Ref = maps:get(upload_ref, Presign),
    Id = maps:get(asset_id, Presign),
    ?assertEqual(<<"private_object_store">>, maps:get(adapter, maps:get(upload, Presign))),
    concurrent_put(Org, Base#{upload_ref => Ref, payload => Bytes}),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:put_object(
            Org, Base#{upload_ref => Ref, payload => Bytes}
        )
    ),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
    {ok, _} = enterprise_business_facade:confirm_asset(Org, Base#{upload_ref => Ref}),
    {ok, #{body := Bytes}} = enterprise_business_facade:content_stream(Org, Base#{asset_id => Id}),
    ?assertEqual(
        {error, not_found},
        enterprise_business_facade:content_stream(
            maps:get(other_org_id, S), Base#{
                workspace_id => maps:get(other_workspace_id, S), asset_id => Id
            }
        )
    ),
    ?assertMatch(
        {error, _},
        enterprise_business_facade:content_stream(
            Org, Base#{actor_user_id => maps:get(peer_user_id, S), asset_id => Id}
        )
    ),
    {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
    ?assertEqual(active, maps:get(status, Row)),
    ok = eb_asset_store:delete_private(Org, Ws, Id).

rollback_object(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Id = cs_pg_test_fixture:id(),
    ?assertEqual(
        {error, invalid_asset_descriptor},
        eb_asset_store:put_private(
            Org, Ws, #{id => Id, payload => <<"synthetic rollback">>, mime => <<"text/plain">>}
        )
    ),
    ?assertEqual(
        {error, not_found},
        eb_asset_object_garage:get(
            eb_asset_store:scope_key(Org, Ws, Id), eb_pg_asset_meta:key_prefix(Org, Ws)
        )
    ).

env(Name) -> list_to_binary(os:getenv(Name)).

stub_regression() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    try
        ok = eunit:test(eb_asset_store_tests:cases({ok, synthetic}), [verbose])
    after
        eb_asset_object_stub:reset(),
        eb_pg_test_fixture:restore_asset_store(Previous)
    end,
    ?assertEqual(undefined, application:get_env(imboy, eb_asset_object_store)).

concurrent_put(Org, Params) ->
    Parent = self(),
    Workers = [
        spawn(fun() ->
            receive
                start -> Parent ! {self(), enterprise_business_facade:put_object(Org, Params)}
            end
        end)
     || _ <- [1, 2]
    ],
    [Pid ! start || Pid <- Workers],
    Results = [
        receive
            {Pid, Result} -> Result
        after 10000 -> error(upload_timeout)
        end
     || Pid <- Workers
    ],
    ?assertEqual(1, length([ok || {ok, _} <- Results])),
    ?assertEqual(1, length([error || {error, _} <- Results])).

%% Storage failure must leave a retryable pending asset, not a hidden orphan.
cleanup_recovery(S, Config) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Id = cs_pg_test_fixture:id(),
    Bytes = <<"synthetic expired pending cleanup">>,
    {ok, _} = eb_asset_store:put_private(Org, Ws, #{
        id => Id, payload => Bytes, object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET created_at=now()-interval '2 hours' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id]
    ),
    Params = #{workspace_id => Ws, asset_ids => [Id]},
    application:unset_env(imboy, garage),
    First =
        try
            eb_asset_app:cleanup_pending(Org, Params)
        after
            application:set_env(imboy, garage, Config)
        end,
    {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
    {ok, Down} = file:read_file("priv/migrations/00000162_enterprise_asset_cleanup_retry.down.sql"),
    [Guard, _] = binary:split(Down, <<"DROP INDEX">>),
    {error, GuardError} = elib_pg:query(Guard, []),
    ?assertEqual(<<"P0001">>, eb_pg_exec:error_of(GuardError)),
    {ok, Retry} = eb_asset_app:cleanup_pending(Org, Params),
    Object = eb_asset_object_garage:get(
        maps:get(object_key, Row), eb_pg_asset_meta:key_prefix(Org, Ws)
    ),
    ok = file:write_file(
        filename:join(os:getenv("IMBOY_GATE_RUN_DIR"), "cleanup-recovery.json"),
        jsone:encode(#{
            first_deleted => maps:get(deleted, element(2, First)),
            status_after_failure => maps:get(status, Row),
            pending_delete_after_failure => maps:get(pending_object_delete, Row),
            retry_deleted => maps:get(deleted, Retry),
            retry_skip_count => length(maps:get(skipped, Retry)),
            object_remains => element(1, Object) =:= ok
        })
    ),
    ?assertEqual(deleted, maps:get(status, Row)),
    ?assertEqual(true, maps:get(pending_object_delete, Row)),
    ?assertEqual([Id], maps:get(deleted, Retry)),
    ?assertEqual({error, not_found}, Object),
    {ok, Final} = eb_asset_store:fetch_asset(Org, Ws, Id),
    ?assertEqual(false, maps:get(pending_object_delete, Final)).

cleanup_concurrency(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pending(Org, Ws),
    Parent = self(),
    Worker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, Ws]
        ),
        Pid = cleanup_worker(Parent, Org, Ws, Id),
        ok = await_cleanup_lock(Conn, 100),
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"UPDATE enterprise_asset SET status='active' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3 AND status='pending_confirm' RETURNING id">>,
            [Org, Ws, Id]
        ),
        Pid
    end),
    assert_cleanup_skipped(Worker),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
    {HeldId, HeldBytes} = expired_pending(Org, Ws),
    HoldId = cs_pg_test_fixture:id(),
    HeldWorker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"INSERT INTO enterprise_retention_hold(id,organization_id,workspace_id,scope_type,reason_code) VALUES($3,$1,$2,'workspace','synthetic-cleanup-hold') RETURNING id">>,
            [Org, Ws, HoldId]
        ),
        Pid = cleanup_worker(Parent, Org, Ws, HeldId),
        ok = await_cleanup_lock(Conn, 100),
        Pid
    end),
    assert_cleanup_skipped(HeldWorker),
    {ok, {content_stream, HeldBytes}} = eb_asset_store:stream_content(Org, Ws, HeldId),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_retention_hold SET released_at=now(),released_by_user_id=$4 WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, HoldId, maps:get(owner_user_id, S)]
    ),
    {ok, #{deleted := [HeldId]}} = eb_asset_app:cleanup_pending(Org, #{
        workspace_id => Ws, asset_ids => [HeldId]
    }),
    io:format(
        "PASS: real cleanup workspace lock observed; concurrent confirmation and new hold prevent object deletion; released hold permits cleanup~n"
    ).

expired_pending(Org, Ws) ->
    Id = cs_pg_test_fixture:id(),
    Bytes = <<"synthetic concurrent pending cleanup">>,
    {ok, _} = eb_asset_store:put_private(Org, Ws, #{
        id => Id, payload => Bytes, object_hash => eb_asset_content:sha256_hex(Bytes)
    }),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET created_at=now()-interval '2 hours' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id]
    ),
    {Id, Bytes}.

cleanup_worker(Parent, Org, Ws, Id) ->
    spawn(fun() ->
        Parent !
            {self(), eb_asset_app:cleanup_pending(Org, #{workspace_id => Ws, asset_ids => [Id]})}
    end).

await_cleanup_lock(Conn, Attempts) ->
    {ok, _} = elib_pg:query(Conn, <<"SELECT pg_stat_clear_snapshot()">>, []),
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        Conn,
        <<"SELECT count(*)::integer AS n FROM pg_stat_activity WHERE datname=current_database() AND wait_event_type='Lock' AND query LIKE '%SELECT id FROM workspace%'">>,
        []
    ),
    case N of
        1 -> ok;
        _ when Attempts > 0 -> receive
            after 10 -> await_cleanup_lock(Conn, Attempts - 1)
            end;
        _ -> error(cleanup_did_not_lock_workspace)
    end.

assert_cleanup_skipped(Pid) ->
    receive
        {Pid, {ok, #{deleted := []}}} -> ok;
        {Pid, Other} -> error({cleanup_should_skip, Other})
    after 10000 -> error(cleanup_worker_timeout)
    end.

cleanup_ineligible(S) ->
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Retained, RetainedBytes} = expired_pending(Org, Ws),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET retain_until=now()+interval '1 day' WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Retained]
    ),
    {Legacy, LegacyBytes} = expired_pending(Org, Ws),
    ok = eb_asset_store:cleanup_asset(Org, Ws, Legacy),
    {ok, #{deleted := []}} = eb_asset_app:cleanup_pending(Org, #{
        workspace_id => Ws, asset_ids => [Retained, Legacy]
    }),
    [
        begin
            {ok, Row} = eb_asset_store:fetch_asset(Org, Ws, Id),
            ?assertEqual(false, maps:get(pending_object_delete, Row)),
            {ok, #{bytes := Bytes}} = eb_asset_object_garage:get(
                maps:get(object_key, Row), eb_pg_asset_meta:key_prefix(Org, Ws)
            )
        end
     || {Id, Bytes} <- [{Retained, RetainedBytes}, {Legacy, LegacyBytes}]
    ],
    migration_roundtrip(),
    io:format(
        "PASS: future retention and unmarked legacy tombstones keep their real objects; pending intent blocks rollback; empty-intent down/up succeeds~n"
    ).

migration_roundtrip() ->
    {ok, Down} = file:read_file("priv/migrations/00000162_enterprise_asset_cleanup_retry.down.sql"),
    {ok, Up} = file:read_file("priv/migrations/00000162_enterprise_asset_cleanup_retry.up.sql"),
    ok = elib_pg:with_tx(fun(Conn) ->
        [
            begin
                ?assert(element(1, R) =:= ok)
            end
         || R <- epgsql:squery(Conn, Down)
        ],
        [
            begin
                ?assert(element(1, R) =:= ok)
            end
         || R <- epgsql:squery(Conn, Up)
        ],
        ok
    end).

%% Instrument only the scheduling boundary; every read/write/delete stays native.
purge_regressions() ->
    ok = eb_pg_test_fixture:ensure_purge_role(),
    purge_confirm_before_claim(),
    purge_confirm_after_commit(),
    purge_native_message(),
    purge_failure_recovery(),
    purge_legacy_regression(),
    purge_queue_roundtrip(),
    io:format(
        "PASS: purge confirmation interleavings; committed queue retries native Garage failure; audit failure leaves metadata and bytes intact~n"
    ).

purge_confirm_before_claim() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pending(Org, Ws),
    Parent = self(),
    Worker = elib_pg:with_tx(fun(Conn) ->
        {ok, [_]} = elib_pg:query(
            Conn,
            <<"SELECT id FROM workspace WHERE organization_id=$1 AND id=$2 FOR UPDATE">>,
            [Org, Ws]
        ),
        Pid = spawn(fun() -> Parent ! {self(), purge_native(Org, Ws)} end),
        ok = await_cleanup_lock(Conn, 100),
        {ok, #{status := active}} = eb_asset_store:confirm_asset(Org, Ws, Id),
        Pid
    end),
    {ok, #{orphan_assets_deleted := 0}} = await_purge(Worker),
    {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id).

purge_confirm_after_commit() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pending(Org, Ws),
    Parent = self(),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    ok = meck:new(eb_asset_store, [passthrough, no_link]),
    try
        ok = meck:expect(eb_asset_store, delete_queued_object, fun(O, W, K) ->
            Parent ! {purge_before_object_delete, self()},
            receive
                continue_purge -> meck:passthrough([O, W, K])
            after 10000 -> error(purge_boundary_timeout)
            end
        end),
        Worker = spawn(fun() -> Parent ! {self(), purge_native(Org, Ws)} end),
        receive
            {purge_before_object_delete, Worker} -> ok
        after 10000 -> error(purge_did_not_reach_object_delete)
        end,
        ?assertEqual({error, not_found}, eb_asset_store:confirm_asset(Org, Ws, Id)),
        ?assertEqual(1, queue_count(Org, Ws)),
        ?assertEqual(
            {error, cleanup_pending},
            eb_asset_store:put_private(Org, Ws, #{
                id => Id, payload => Bytes, object_hash => eb_asset_content:sha256_hex(Bytes)
            })
        ),
        Worker ! continue_purge,
        {ok, #{orphan_assets_deleted := 1}} = await_purge(Worker),
        ?assertEqual(
            {error, not_found},
            eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
        ),
        ?assertEqual(0, queue_count(Org, Ws))
    after
        meck:unload(eb_asset_store)
    end.

purge_failure_recovery() ->
    S = cs_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    {Id, Bytes} = expired_pending(Org, Ws),
    {ok, Config} = application:get_env(imboy, garage),
    application:unset_env(imboy, garage),
    try
        {ok, #{orphan_assets_deleted := 1, object_delete_failures := [_]}} = purge_native(Org, Ws)
    after
        application:set_env(imboy, garage, Config)
    end,
    ?assertEqual({error, not_found}, eb_asset_store:fetch_asset(Org, Ws, Id)),
    ?assertEqual(1, queue_count(Org, Ws)),
    purge_queue_rollback_guard(),
    Key = eb_pg_asset_meta:object_key(Org, Ws, Id),
    {ok, #{bytes := Bytes}} = eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws)),
    {ok, #{orphan_assets_deleted := 0, object_delete_failures := []}} = purge_native(Org, Ws),
    ?assertEqual(0, queue_count(Org, Ws)),
    ?assertEqual(
        {error, not_found}, eb_asset_object_garage:get(Key, eb_pg_asset_meta:key_prefix(Org, Ws))
    ),
    purge_audit_failure(Org, Ws).

purge_audit_failure(Org, Ws) ->
    {Id, Bytes} = expired_pending(Org, Ws),
    ok = meck:new(eb_pg_audit, [passthrough, no_link]),
    try
        ok = meck:expect(eb_pg_audit, append_in, fun(_, _, _) ->
            {error, synthetic_audit_failure}
        end),
        ?assertEqual({error, {audit_failed, synthetic_audit_failure}}, purge_native(Org, Ws)),
        {ok, #{status := pending_confirm}} = eb_asset_store:fetch_asset(Org, Ws, Id),
        {ok, {content_stream, Bytes}} = eb_asset_store:stream_content(Org, Ws, Id),
        ?assertEqual(0, queue_count(Org, Ws))
    after
        meck:unload(eb_pg_audit)
    end.

purge_native(Org, Ws) ->
    eb_pg_purge:purge_batch(Org, Ws, #{
        now => eb_system_clock:now(),
        batch_limit => 10,
        orphan_asset_age_seconds => 3600
    }).

await_purge(Worker) ->
    receive
        {Worker, Result} -> Result
    after 10000 -> error(purge_completion_timeout)
    end.

queue_count(Org, Ws) ->
    {ok, [#{<<"n">> := N}]} = elib_pg:query(
        <<"SELECT count(*)::integer AS n FROM enterprise_asset_delete_queue WHERE organization_id=$1 AND workspace_id=$2">>,
        [Org, Ws]
    ),
    N.

purge_legacy_regression() ->
    Previous = eb_pg_test_fixture:select_asset_stub(),
    try
        ok = eunit:test(eb_retention_pg_tests:cases({ok, undefined}), [verbose]),
        ok = eunit:test(eb_purge_orphan_asset_pg_tests:cases({ok, undefined}), [verbose])
    after
        eb_pg_test_fixture:restore_asset_store(Previous)
    end.

purge_native_message() ->
    S = eb_pg_test_fixture:new_scope(),
    Org = maps:get(org_id, S),
    Ws = maps:get(workspace_id, S),
    Until = eb_system_clock:now() - 120,
    Msg = eb_retention_pg_tests:insert_message(S, <<"synthetic-native-garage-purge">>, Until),
    {Id, _} = expired_pending(Org, Ws),
    ok = cs_pg_test_fixture:exec(
        <<"UPDATE enterprise_asset SET status='active',conversation_id=$4,message_id=$5,retain_until=to_timestamp($6::bigint) WHERE organization_id=$1 AND workspace_id=$2 AND id=$3">>,
        [Org, Ws, Id, maps:get(conversation_id, S), Msg, Until]
    ),
    {ok, #{purged := [Msg], deleted := 1, object_delete_failures := []}} = purge_native(Org, Ws),
    ?assertEqual({error, not_found}, eb_asset_store:fetch_asset(Org, Ws, Id)),
    ?assertEqual(
        {error, not_found},
        eb_asset_object_garage:get(
            eb_pg_asset_meta:object_key(Org, Ws, Id), eb_pg_asset_meta:key_prefix(Org, Ws)
        )
    ),
    ?assertEqual(0, queue_count(Org, Ws)).

purge_queue_rollback_guard() ->
    {ok, Down} = file:read_file("priv/migrations/00000163_enterprise_asset_delete_queue.down.sql"),
    [Guard, _] = binary:split(Down, <<"DROP TABLE">>),
    {error, Error} = elib_pg:query(Guard, []),
    ?assertEqual(<<"P0001">>, eb_pg_exec:error_of(Error)).

purge_queue_roundtrip() ->
    {ok, Down} = file:read_file("priv/migrations/00000163_enterprise_asset_delete_queue.down.sql"),
    {ok, Up} = file:read_file("priv/migrations/00000163_enterprise_asset_delete_queue.up.sql"),
    ok = elib_pg:with_tx(fun(Conn) ->
        [?assert(element(1, R) =:= ok) || R <- epgsql:squery(Conn, Down)],
        [?assert(element(1, R) =:= ok) || R <- epgsql:squery(Conn, Up)],
        ok
    end).
