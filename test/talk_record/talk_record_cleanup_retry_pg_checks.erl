-module(talk_record_cleanup_retry_pg_checks).
-moduledoc "Required continuation of controller-owned, genuinely exhausted deletion fixtures.".
-export([run_registered/0, run/6, verify_complete/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-define(CASE, <<"talk_record_delete_tests:purge_failed_then_retry_test_">>).

%% This continuation never starts infrastructure, clears tables or seeds terminal rows.
%% The controller freezes the actual eight claim/expiry/reaper observations first.
run_registered() ->
    _ = talk_record_t20_case_registry:activate(?CASE),
    Previous = [
        {K, application:get_env(imboy, K)}
     || K <-
            [sql_driver, attachment_storage_registry]
    ],
    try
        {Registry, Case, Receipt} = registered_fixture(),
        configure_storage(Case, Receipt),
        lists:foreach(
            fun(A) -> {ok, _} = application:ensure_all_started(A) end,
            [crypto, epgsql, pooler]
        ),
        undefined = whereis(pgsql),
        undefined = whereis(pooler_pgsql_pool_sup),
        undefined = whereis(elib_tsid_meck),
        ok = ensure_allocator(),
        ok = application:set_env(imboy, sql_driver, pgsql),
        {ok, PoolSupervisor} = pooler:new_pool(#{
            name => pgsql,
            max_count => 4,
            init_count => 1,
            start_mfa => {epgsql, connect, [connection_options()]}
        }),
        Pool = whereis(pgsql),
        try
            true = is_pid(Pool),
            run_connected(Registry, Case, Receipt)
        after
            case {whereis(pooler_pgsql_pool_sup), whereis(pgsql)} of
                {PoolSupervisor, Pool} -> pooler:rm_pool(pgsql);
                {undefined, undefined} -> ok;
                _ -> error(cleanup_retry_pool_ownership_lost)
            end
        end
    after
        lists:foreach(fun restore_environment/1, Previous),
        talk_record_t20_case_registry:clear()
    end.

restore_environment({Key, undefined}) -> application:unset_env(imboy, Key);
restore_environment({Key, {ok, Value}}) -> application:set_env(imboy, Key, Value).

ensure_allocator() ->
    case elib_tsid:readiness() of
        ready -> ok;
        not_initialized -> elib_tsid:init(#{dc_id => 1, node_id => 1, dc_bits => 3});
        Other -> error({cleanup_retry_allocator_not_ready, Other})
    end.

configure_storage(
    #{<<"disk_root">> := Root, <<"namespace">> := Ns},
    #{<<"resources">> := Resources}
) ->
    [Container] = lists:usort([maps:get(<<"storage_container">>, R) || R <- Resources]),
    [RootId] = lists:usort([maps:get(<<"storage_root_id">>, R) || R <- Resources]),
    true = is_binary(RootId) andalso byte_size(RootId) > 0,
    ok = application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        upload_namespaces => #{<<"talk_record">> => Ns},
        roots => #{
            RootId => #{
                driver => <<"disk">>,
                namespace => Ns,
                container => Container,
                endpoint => <<"file://", Root/binary>>
            }
        }
    }),
    assert_storage_registry(Root, Ns, Resources).

run_connected(Registry, Case, Receipt) ->
    {ok, C} = epgsql:connect(connection_options()),
    try
        assert_identity(C, Registry, Case),
        ok = elib_pg:with_conn(fun(Pooled) -> assert_identity(Pooled, Registry, Case) end),
        #{
            <<"record_id">> := Rid,
            <<"owner_user_id">> := Uid,
            <<"stranger_user_id">> := Stranger,
            <<"revision">> := Rev,
            <<"cleanup_generation">> := Generation,
            <<"resources">> := Resources
        } = Receipt,
        assert_actors(C, Uid, Stranger, Rid),
        assert_exhaustion(C, Rid, Resources),
        assert_files(Resources, present),
        {ok, #{accepted := P}} = run(C, Uid, Stranger, Rid, Rev, Generation),
        %% The controller repairs its storage fault before invoking this continuation.
        %% Public recovery schedules the real purge worker; finish through production APIs.
        _ = guarded_write(Registry, fun talk_record_job_worker:run_once/0),
        {ok, Count} = guarded_write(Registry, fun() ->
            attachment_delete_worker:run_batch(
                C,
                fun(_Ns, _Container, _Key, Identity) ->
                    attachment_storage:delete_snapshot(Identity)
                end
            )
        end),
        ?assertEqual(length(Resources), Count),
        {ok, _} = guarded_write(Registry, fun talk_record_job_worker:refresh_cleanup_pending/0),
        {ok, physical_queue_and_record_complete} = guarded_write(
            Registry,
            fun() -> verify_complete(C, Rid) end
        ),
        assert_files(Resources, absent),
        {ok, [#{<<"state">> := <<"succeeded">>}]} = elib_pg:query(
            C,
            <<"SELECT state FROM talk_record_job WHERE id=$1 AND record_id=$2">>,
            [maps:get(<<"job_id">>, P), Rid]
        ),
        ok
    after
        epgsql:close(C)
    end.

registered_fixture() ->
    #{registry := Registry, entry := Case} = authorized_context(),
    ok = assert_runtime_lease(Registry),
    #{<<"owner">> := Owner, <<"controller_epoch">> := Epoch} = Registry,
    #{
        <<"exhaustion_receipt_path">> := Path,
        <<"exhaustion_receipt_sha256">> := Hash,
        <<"database">> := Db,
        <<"disk_root">> := Root,
        <<"namespace">> := Ns
    } = Case,
    Bytes = private_bytes(binary_to_list(Path)),
    Hash = hex(Bytes),
    Receipt = jsone:decode(Bytes),
    #{
        <<"case_key">> := ?CASE,
        <<"owner">> := Owner,
        <<"controller_epoch">> := Epoch,
        <<"database">> := Db,
        <<"disk_root">> := Root,
        <<"namespace">> := Ns,
        <<"resources">> := Resources
    } = Receipt,
    ok = talk_record_t20_frozen_config:same(Receipt, Registry, [
        <<"frozen_config_sha256">>,
        <<"candidate_commit">>,
        <<"source_sha256">>,
        <<"controller_thread_id">>,
        <<"controller_pid">>,
        <<"controller_process_start">>,
        <<"host">>,
        <<"connection_port">>,
        <<"server_port">>,
        <<"system_identifier">>,
        <<"user">>,
        <<"migration_head">>,
        <<"migration_count">>,
        <<"migration_versions">>
    ]),
    true = is_list(Resources) andalso Resources =/= [],
    assert_directory(binary_to_list(Root)),
    {Registry, Case, Receipt}.

authorized_context() ->
    talk_record_t20_case_registry:authorize(
        talk_record_delete_tests, purge_failed_then_retry_test_
    ).

assert_identity(C, Registry, Case) ->
    #{registry := Registry, entry := Case} = authorized_context(),
    talk_record_t20_case_registry:verify_connected(C).

connection_options() -> talk_record_t20_case_registry:connection_options().

assert_storage_registry(Root, Ns, Resources) ->
    %% Frozen file oracle and production snapshot must describe the same object.
    #{roots := Roots} = application:get_env(imboy, attachment_storage_registry, #{}),
    lists:foreach(
        fun(
            #{
                <<"storage_namespace">> := ResourceNs,
                <<"path">> := LogicalKey,
                <<"storage_container">> := Container,
                <<"storage_locator_sha256">> := Locator,
                <<"storage_physical_key">> := RawPhysicalKey,
                <<"resolved_storage_key">> := Key,
                <<"disk_file">> := File
            }
        ) ->
            Ns = ResourceNs,
            %% Preserve the actual nullable DB column. Resolve its physical key
            %% through the same production boundary used by the delete worker.
            {ok, #{driver := disk, key := Key}} = attachment_storage:frozen_location(
                LogicalKey, #{
                    <<"storage_driver">> => <<"disk">>,
                    <<"storage_namespace">> => Ns,
                    <<"storage_container">> => Container,
                    <<"storage_locator_sha256">> => Locator,
                    <<"storage_physical_key">> => RawPhysicalKey
                }
            ),
            false = filename:pathtype(binary_to_list(Key)) =:= absolute,
            true = lists:all(
                fun(P) -> P =/= ".." andalso P =/= "." end,
                filename:split(binary_to_list(Key))
            ),
            ExpectedFile = list_to_binary(
                filename:join([
                    binary_to_list(Root),
                    binary_to_list(Container),
                    binary_to_list(Key)
                ])
            ),
            ExpectedFile = File,
            Matches = [
                R
             || R <- maps:values(Roots),
                maps:get(driver, R, undefined) =:= <<"disk">>,
                maps:get(namespace, R, undefined) =:= Ns,
                maps:get(container, R, undefined) =:= Container,
                maps:get(endpoint, R, undefined) =:= <<"file://", Root/binary>>
            ],
            ?assertEqual(1, length(Matches))
        end,
        Resources
    ).

assert_exhaustion(C, Rid, Resources) ->
    Paths = [maps:get(<<"path">>, R) || R <- Resources],
    ?assertEqual(length(Paths), length(lists:usort(Paths))),
    {ok, Rows} = talk_record_delete_repo:purged_attachment_paths_tx(C, Rid),
    ?assertEqual(lists:sort(Paths), lists:sort([maps:get(<<"path">>, R) || R <- Rows])),
    %% No foreign pending row may be claimed by the batch worker below.
    {ok, Pending} = elib_pg:query(
        C,
        <<"SELECT path FROM attachment_object_deletion WHERE completed_at IS NULL ORDER BY path">>,
        []
    ),
    ?assertEqual(lists:sort(Paths), [maps:get(<<"path">>, R) || R <- Pending]),
    lists:foreach(fun(Resource) -> assert_exhausted_resource(C, Resource) end, Resources).

assert_exhausted_resource(C, #{
    <<"path">> := Path,
    <<"storage_namespace">> := Ns,
    <<"storage_container">> := Container,
    <<"storage_locator_sha256">> := Locator,
    <<"storage_physical_key">> := PhysicalKey,
    <<"attempt_base">> := Base,
    <<"generation">> := Gen,
    <<"steps">> := Steps
}) ->
    {ok, [
        #{
            <<"storage_driver">> := <<"disk">>,
            <<"storage_namespace">> := Ns,
            <<"storage_container">> := Container,
            <<"storage_locator_sha256">> := Locator,
            <<"storage_physical_key">> := PhysicalKey,
            <<"attempts">> := Attempts,
            <<"attempt_base">> := Base,
            <<"retry_generation">> := Gen,
            <<"job_state">> := <<"blocked">>,
            <<"last_error_code">> := <<"max_attempts">>,
            <<"lease_token">> := null,
            <<"lease_until">> := null,
            <<"completed_at">> := null
        }
    ]} =
        elib_pg:query(C, <<"SELECT * FROM attachment_object_deletion WHERE path=$1">>, [Path]),
    ?assertEqual(Base + 8, Attempts),
    ?assertEqual(8, length(Steps)),
    Tokens = [maps:get(<<"lease_token">>, S) || S <- Steps],
    ?assertEqual(8, length(lists:usort(Tokens))),
    lists:foreach(
        fun({Previous, Next}) ->
            true =
                maps:get(<<"claimed_at_unix">>, Next) >=
                    maps:get(<<"reaper_finished_at_unix">>, Previous)
        end,
        lists:zip(lists:sublist(Steps, 7), tl(Steps))
    ),
    lists:foreach(
        fun({Step, N}) -> assert_expiry_step(Step, Path, Gen, Base + N, N) end,
        lists:zip(Steps, lists:seq(1, 8))
    ).

assert_expiry_step(Step, Path, Gen, Attempt, N) ->
    Registry = active_registry(),
    ok = talk_record_t20_frozen_config:same(Step, Registry, [
        <<"owner">>,
        <<"controller_epoch">>,
        <<"controller_thread_id">>,
        <<"candidate_commit">>,
        <<"frozen_config_sha256">>
    ]),
    #{
        <<"path">> := Path,
        <<"generation">> := Gen,
        <<"attempt">> := Attempt,
        <<"lease_token">> := Token,
        <<"claimed_at_unix">> := Claim,
        <<"lease_until_unix">> := Until,
        <<"observed_expired_at_unix">> := Expired,
        <<"reaper_finished_at_unix">> := Reaped,
        <<"state_after">> := State,
        <<"raw_receipt_path">> := RawPath,
        <<"raw_receipt_sha256">> := RawHash
    } = Step,
    true = is_binary(Token) andalso byte_size(Token) > 0,
    true = lists:all(fun is_number/1, [Claim, Until, Expired, Reaped]),
    true = Until - Claim >= 120 andalso Expired > Until andalso Reaped >= Expired,
    true = Claim >= maps:get(<<"valid_from_unix">>, Registry),
    true = Reaped =< maps:get(<<"expires_at_unix">>, Registry),
    ExpectedState =
        case N of
            8 -> <<"blocked">>;
            _ -> <<"retry_wait">>
        end,
    ExpectedState = State,
    Bytes = private_bytes(binary_to_list(RawPath)),
    RawHash = hex(Bytes),
    true = Reaped =< erlang:system_time(second),
    %% Values must be the actual claim/reaper observation payload, not just a label.
    Raw = jsone:decode(Bytes),
    ?assertEqual(maps:without([<<"raw_receipt_path">>, <<"raw_receipt_sha256">>], Step), Raw).

assert_files(Resources, State) ->
    lists:foreach(
        fun(
            #{
                <<"disk_file">> := File,
                <<"byte_size">> := Size,
                <<"sha256">> := Hash
            }
        ) ->
            assert_directory(filename:dirname(binary_to_list(File))),
            case State of
                present ->
                    {ok, #file_info{type = regular, size = Size}} = file:read_link_info(File),
                    {ok, Bytes} = file:read_file(File),
                    Hash = hex(Bytes);
                absent ->
                    ?assertEqual({error, enoent}, file:read_link_info(File))
            end
        end,
        Resources
    ).

%% Public authenticated recovery: permissions, capability, replay and generation CAS.
run(C, Uid, Stranger, Rid, Revision, Generation) ->
    Registry = active_registry(),
    assert_actors(C, Uid, Stranger, Rid),
    Body = #{<<"cleanup_generation">> => Generation, <<"reason">> => <<"owned fault repaired">>},
    Key =
        <<"cleanup-retry-", (integer_to_binary(Rid))/binary, "-",
            (integer_to_binary(Generation))/binary>>,
    Match = integer_to_binary(Revision),
    ?assertEqual(
        false,
        maps:get(
            <<"allowed">>,
            talk_record_cleanup_retry_repo:capability_tx(C, Stranger, Rid)
        )
    ),
    {error, not_found} = guarded_retry(Registry, Stranger, Rid, Match, Key, Body),
    ?assertEqual(
        true,
        maps:get(
            <<"allowed">>,
            talk_record_cleanup_retry_repo:capability_tx(C, Uid, Rid)
        )
    ),
    {ok, P} = guarded_retry(Registry, Uid, Rid, Match, Key, Body),
    ?assert(maps:get(<<"reopened_resources">>, P) > 0),
    {ok, Replay} = guarded_retry(Registry, Uid, Rid, Match, Key, Body),
    ?assertEqual(maps:get(<<"job_id">>, P), maps:get(<<"job_id">>, Replay)),
    ?assertEqual(true, maps:get(<<"replayed">>, Replay)),
    {error, idempotency_conflict} = guarded_retry(
        Registry,
        Uid,
        Rid,
        Match,
        Key,
        Body#{<<"reason">> => <<"different request">>}
    ),
    {error, cleanup_generation_conflict} = guarded_retry(
        Registry,
        Uid,
        Rid,
        integer_to_binary(maps:get(<<"revision">>, P)),
        <<Key/binary, "-stale">>,
        Body
    ),
    {ok, #{accepted => P, replayed => Replay, physical_delete_verified => false}}.

verify_complete(C, Rid) ->
    {ok, Rows} = talk_record_delete_repo:purged_attachment_paths_tx(C, Rid),
    ?assert(Rows =/= []),
    lists:foreach(fun(R) -> ?assertEqual(true, maps:get(<<"queue_done">>, R)) end, Rows),
    {ok, <<"complete">>} = talk_record_cleanup_logic:refresh_cleanup_state_tx(C, Rid),
    {ok, physical_queue_and_record_complete}.

%% Registry and private resource/controller leases are re-read before every write.
active_registry() ->
    #{registry := Registry} = authorized_context(),
    Registry.

assert_runtime_lease(Registry) ->
    Registry = active_registry(),
    %% The shared strict verifier checks the current candidate, full source set,
    %% controller PID/start probe, ownership lease and exact 25-case registry.
    %% This continuation never relaxes its empty-fixture setup for ordinary cases.
    undefined = whereis(talk_record_job_worker),
    undefined = whereis(attachment_delete_worker),
    ok.

guarded_retry(Registry, Uid, Rid, Match, Key, Body) ->
    guarded_write(Registry, fun() ->
        talk_record_cleanup_retry_logic:retry(Uid, Rid, Match, Key, Body)
    end).

%% Ninety-second bound leaves thirty seconds before the required 120-second lease.
%% A timeout is a failed/indeterminate attempt, never permission to retry side effects.
guarded_write(Registry, Fun) ->
    assert_runtime_lease(Registry),
    Parent = self(),
    Tag = make_ref(),
    {Pid, Monitor} = spawn_opt(
        fun() ->
            Result =
                try
                    {ok, Fun()}
                catch
                    Class:Reason:Stack -> {raised, Class, Reason, Stack}
                end,
            Parent ! {Tag, Result}
        end,
        [link, monitor]
    ),
    receive
        {Tag, {ok, Value}} ->
            erlang:demonitor(Monitor, [flush]),
            Value;
        {Tag, {raised, Class, Reason, Stack}} ->
            erlang:demonitor(Monitor, [flush]),
            erlang:raise(Class, Reason, Stack);
        {'DOWN', Monitor, process, Pid, Reason} ->
            error({guarded_action_failed, Reason})
    after 90000 ->
        unlink(Pid),
        exit(Pid, kill),
        receive
            {'DOWN', Monitor, process, Pid, _} -> ok
        after 1000 -> ok
        end,
        error(guarded_action_timeout_indeterminate)
    end.

assert_actors(C, Uid, Stranger, Rid) ->
    true = is_integer(Uid) andalso Uid > 0 andalso is_integer(Stranger) andalso Stranger > 0,
    true = Uid =/= Stranger andalso is_integer(Rid) andalso Rid > 0,
    {ok, Rows} = elib_pg:query(
        C,
        <<
            "SELECT id FROM public.\"user\" WHERE id=ANY($1::bigint[])"
            " AND status=1 AND account_type=0 ORDER BY id"
        >>,
        [[Uid, Stranger]]
    ),
    ?assertEqual(lists:sort([Uid, Stranger]), [maps:get(<<"id">>, R) || R <- Rows]),
    {ok, [#{<<"owner_user_id">> := Uid, <<"organization_id">> := Org}]} =
        elib_pg:query(C, <<"SELECT owner_user_id,organization_id FROM talk_record WHERE id=$1">>, [
            Rid
        ]),
    {ok, []} = elib_pg:query(
        C,
        <<
            "SELECT user_id FROM organization_member WHERE organization_id=$1 AND user_id=$2"
            " AND status='active' AND role IN ('owner','admin')"
        >>,
        [Org, Stranger]
    ),
    ok.

private_bytes(Path) ->
    assert_directory(filename:dirname(Path)),
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(Path),
    true = Mode band 8#077 =:= 0,
    {ok, Bytes} = file:read_file(Path),
    Bytes.
assert_directory(Path) ->
    absolute = filename:pathtype(Path),
    lists:foldl(
        fun(Segment, Prefix) ->
            Current =
                case Prefix of
                    "" -> Segment;
                    _ -> filename:join(Prefix, Segment)
                end,
            {ok, #file_info{type = directory}} = file:read_link_info(Current),
            Current
        end,
        "",
        filename:split(Path)
    ),
    ok.
hex(Bytes) -> binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
