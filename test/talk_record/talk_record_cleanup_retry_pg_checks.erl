-module(talk_record_cleanup_retry_pg_checks).
-moduledoc "Required continuation of controller-owned, genuinely exhausted deletion fixtures.".
-export([run_registered/0, run/6, verify_complete/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-define(CASE, <<"talk_record_delete_tests:purge_failed_then_retry_test_">>).

%% This continuation never starts infrastructure, clears tables or seeds terminal rows.
%% The controller freezes the actual eight claim/expiry/reaper observations first.
run_registered() ->
    {Registry, Case, Receipt} = registered_fixture(),
    assert_sources(Registry),
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
    RegistryBytes = private_bytes(required("T20_CASE_REGISTRY")),
    RegistryHash = list_to_binary(required("T20_CASE_REGISTRY_SHA256")),
    RegistryHash = hex(RegistryBytes),
    Registry = jsone:decode(RegistryBytes),
    #{
        <<"owner">> := Owner,
        <<"controller_epoch">> := Epoch,
        <<"cases">> := Cases
    } = Registry,
    true =
        is_binary(Owner) andalso byte_size(Owner) > 0 andalso is_integer(Epoch) andalso Epoch > 0,
    assert_runtime_lease(Registry),
    Case = maps:get(?CASE, Cases),
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
    true = is_list(Resources) andalso Resources =/= [],
    assert_directory(binary_to_list(Root)),
    assert_storage_registry(Root, Ns, Resources),
    {Registry, Case, Receipt}.

assert_sources(#{<<"source_sha256">> := Sources}) when is_map(Sources) ->
    Repo = required("T20_CASE_REPO"),
    {ok, Repo} = file:get_cwd(),
    true = filename:pathtype(Repo) =:= absolute,
    assert_directory(Repo),
    Required = lists:sort(
        lists:append([
            source_tree(Repo, "src", ".erl"),
            source_tree(Repo, "include", ".hrl"),
            source_tree(Repo, "priv/migrations", ".sql"),
            [
                <<"test/talk_record/talk_record_delete_tests.erl">>,
                <<"test/talk_record/talk_record_cleanup_retry_pg_checks.erl">>
            ]
        ])
    ),
    ?assertEqual(Required, lists:sort(maps:keys(Sources))),
    lists:foreach(
        fun(Name) ->
            Expected = maps:get(Name, Sources),
            Path = source_path(Repo, binary_to_list(Name)),
            assert_directory(filename:dirname(Path)),
            {ok, #file_info{type = regular}} = file:read_link_info(Path),
            {ok, Bytes} = file:read_file(Path),
            Expected = hex(Bytes)
        end,
        Required
    ).

%% Derive the source set from the real tree; never traverse even a directory symlink.
source_tree(Repo, Relative, Extension) ->
    Directory = source_path(Repo, Relative),
    assert_directory(Directory),
    Files = source_directory(Repo, Relative, Extension),
    true = Files =/= [],
    Files.
source_directory(Repo, Relative, Extension) ->
    {ok, Names} = file:list_dir(source_path(Repo, Relative)),
    lists:append([
        source_entry(Repo, filename:join(Relative, Name), Extension)
     || Name <- lists:sort(Names)
    ]).
source_entry(Repo, Relative, Extension) ->
    Path = source_path(Repo, Relative),
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} ->
            source_directory(Repo, Relative, Extension);
        {ok, #file_info{type = regular}} ->
            case filename:extension(Relative) =:= Extension of
                true -> [unicode:characters_to_binary(Relative)];
                false -> []
            end;
        Other ->
            error({non_regular_candidate_source, Relative, Other})
    end.
source_path(Repo, Relative) ->
    true = filename:pathtype(Relative) =:= relative,
    Parts = filename:split(Relative),
    true =
        Parts =/= [] andalso
            lists:all(
                fun(P) ->
                    P =/= ".." andalso P =/= "." andalso P =/= ""
                end,
                Parts
            ),
    Relative = filename:join(Parts),
    filename:join(Repo, Relative).

assert_identity(C, Registry, #{<<"database">> := Db}) ->
    #{
        <<"host">> := Host,
        <<"connection_port">> := ConnectionPort,
        <<"server_port">> := ServerPort,
        <<"user">> := User,
        <<"system_identifier">> := SysId,
        <<"migration_head">> := Head,
        <<"migration_count">> := Count
    } = Registry,
    Host = list_to_binary(required("T20_PG_HOST")),
    ConnectionPort = list_to_integer(required("T20_PG_PORT")),
    User = list_to_binary(required("T20_PG_USER")),
    Db = list_to_binary(required("T20_PG_DB")),
    {ok, [
        #{
            <<"db">> := Db,
            <<"usr">> := User,
            <<"port">> := ServerPort,
            <<"sysid">> := SysId,
            <<"head">> := Head,
            <<"migrations">> := Count
        }
    ]} =
        elib_pg:query(
            C,
            <<
                "SELECT current_database()::text db,current_user::text usr,"
                "inet_server_port() port,(SELECT system_identifier::text FROM pg_control_system()) sysid,"
                "(SELECT max(version) FROM schema_migrations_history) head,"
                "(SELECT count(*) FROM schema_migrations_history) migrations"
            >>,
            []
        ),
    ok.

connection_options() ->
    [
        {host, required("T20_PG_HOST")},
        {port, list_to_integer(required("T20_PG_PORT"))},
        {username, required("T20_PG_USER")},
        {password, required("T20_PG_PASSWORD")},
        {database, required("T20_PG_DB")}
    ].

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
    Bytes = private_bytes(required("T20_CASE_REGISTRY")),
    Hash = list_to_binary(required("T20_CASE_REGISTRY_SHA256")),
    Hash = hex(Bytes),
    jsone:decode(Bytes).

assert_runtime_lease(Registry) ->
    Registry = active_registry(),
    Hash = list_to_binary(required("T20_CASE_REGISTRY_SHA256")),
    #{
        <<"owner">> := Owner,
        <<"controller_epoch">> := Epoch,
        <<"controller_pid">> := Pid,
        <<"controller_process_start">> := Start,
        <<"controller_owner_path">> := ControllerPath
    } = Registry,
    #{
        <<"approved">> := true,
        <<"exclusive_beam_and_pg_window">> := true,
        <<"owner">> := Owner,
        <<"controller_epoch">> := Epoch,
        <<"registry_sha256">> := Hash,
        <<"expires_at_unix">> := Until
    } =
        jsone:decode(private_bytes(required("T20_CASE_LEASE"))),
    true = is_integer(Until) andalso Until >= erlang:system_time(second) + 120,
    #{
        <<"session">> := Owner,
        <<"controller_epoch">> := Epoch,
        <<"pid">> := Pid,
        <<"process_start">> := Start,
        <<"lease_until">> := ControllerUntil,
        <<"host">> := ControllerHost
    } =
        jsone:decode(private_bytes(binary_to_list(ControllerPath))),
    {ok, LocalHost} = inet:gethostname(),
    true = lists:member(
        ControllerHost,
        [list_to_binary(LocalHost), list_to_binary(LocalHost ++ ".local")]
    ),
    ControllerExpiry = calendar:rfc3339_to_system_time(binary_to_list(ControllerUntil)),
    true = ControllerExpiry >= erlang:system_time(second) + 120,
    Start = controller_process_start(Pid),
    assert_sources(Registry),
    true = min(Until, ControllerExpiry) >= erlang:system_time(second) + 120,
    %% kick/periodic workers may not escape the guarded synchronous test window.
    undefined = whereis(talk_record_job_worker),
    undefined = whereis(attachment_delete_worker),
    ok.

controller_process_start(Pid) when is_integer(Pid), Pid > 0 ->
    Port = open_port({spawn_executable, "/bin/ps"}, [
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        {args, ["-p", integer_to_list(Pid), "-o", "lstart="]}
    ]),
    read_process_start(Port, <<>>).
read_process_start(Port, Bytes) ->
    receive
        {Port, {data, Chunk}} -> read_process_start(Port, <<Bytes/binary, Chunk/binary>>);
        {Port, {exit_status, 0}} -> list_to_binary(string:trim(binary_to_list(Bytes)));
        {Port, {exit_status, Code}} -> error({controller_not_live, Code})
    after 5000 ->
        port_close(Port),
        error(controller_probe_timeout)
    end.

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

required(Name) ->
    case os:getenv(Name) of
        Value when is_list(Value), Value =/= [] -> Value;
        _ -> error({required_fixture_environment_missing, Name})
    end.
private_bytes(Path) ->
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(Path),
    true = Mode band 8#077 =:= 0,
    {ok, Bytes} = file:read_file(Path),
    Bytes.
assert_directory(Path) ->
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
