%% Controller owns cloned, empty case databases. This module only verifies and
%% selects them; no database creation, reset, fallback or production acceptance.
-module(talk_record_t20_case_registry).
-export([
    activate/1,
    authorize/2,
    current/0,
    recheck/0,
    clear/0,
    verify_connected/1,
    verify_empty/1,
    prepared_disk_queue/1,
    disk_queue_snapshot/2,
    disk_queue_binding/0,
    configure_storage/0,
    connection_options/0,
    controller_process_start/1
]).
-include_lib("kernel/include/file.hrl").

activate({M, F}) when is_atom(M), is_atom(F) -> activate(key(M, F));
activate(Key) when is_binary(Key) ->
    undefined = application:get_env(imboy, t20_owned_case),
    Context = load(Key),
    ok = application:set_env(imboy, t20_owned_case, Context),
    Context.

authorize(M, F) -> load(key(M, F)).
current() ->
    {ok, Context} = application:get_env(imboy, t20_owned_case),
    Context.
clear() -> application:unset_env(imboy, t20_owned_case).
recheck() ->
    Previous = current(),
    Previous = load(maps:get(case_key, Previous)),
    ok.

load(Key) ->
    {Config, ConfigHash} = talk_record_t20_frozen_config:load(),
    RegistryPath = required("T20_CASE_REGISTRY"),
    true = list_to_binary(RegistryPath) =:= maps:get(<<"registry_path">>, Config),
    RegistryBytes = private_file(RegistryPath),
    Hash = list_to_binary(required("T20_CASE_REGISTRY_SHA256")),
    Hash = hex(RegistryBytes),
    Registry = jsone:decode(RegistryBytes),
    ConfigHash = maps:get(<<"frozen_config_sha256">>, Registry),
    ok = talk_record_t20_frozen_config:same(
        Registry,
        Config,
        talk_record_t20_frozen_config:binding_keys()
    ),
    ok = same_controller_schema(Registry, Config),
    verify_registry(Registry, Config),
    verify_lease(Registry, Hash, Config, ConfigHash),
    Cases = maps:get(<<"cases">>, Registry),
    Case = maps:get(Key, Cases),
    verify_case(Key, Case, Registry),
    #{case_key => Key, registry => Registry, registry_sha256 => Hash, entry => Case}.

verify_registry(R, Config) ->
    Repo = required("T20_CASE_REPO"),
    {ok, Repo} = file:get_cwd(),
    true = list_to_binary(Repo) =:= maps:get(<<"repo">>, Config),
    assert_directory(Repo),
    Sources = maps:get(<<"source_sha256">>, R),
    true = lists:sort(maps:keys(Sources)) =:= required_sources(Repo),
    maps:foreach(
        fun(Name, Expected) ->
            Segments = filename:split(binary_to_list(Name)),
            relative = filename:pathtype(binary_to_list(Name)),
            false = lists:member("..", Segments),
            Expected = hex(private_source(filename:join(Repo, binary_to_list(Name))))
        end,
        Sources
    ),
    Candidate = maps:get(<<"candidate_commit">>, R),
    Candidate = list_to_binary(required("T20_CASE_BASE_COMMIT")),
    Candidate = git_head(Repo),
    Cases = maps:get(<<"cases">>, R),
    verify_cases(Cases, maps:get(<<"case_authorizations">>, Config)),

    lists:foreach(
        fun(Field) ->
            Values = [maps:get(Field, V) || V <- maps:values(Cases)],
            true = length(Values) =:= length(lists:usort(Values))
        end,
        [<<"database">>, <<"disk_root">>, <<"namespace">>]
    ),
    ok.

verify_cases(Cases, Authorized) ->
    true = lists:sort(maps:keys(Cases)) =:= lists:sort(required_cases()),
    true = lists:sort(maps:keys(Authorized)) =:= lists:sort(required_cases()),
    maps:foreach(
        fun(K, Entry) ->
            ok = talk_record_t20_frozen_config:same(
                Entry,
                maps:get(K, Authorized),
                [<<"database">>, <<"disk_root">>, <<"namespace">>, <<"clone_receipt_path">>]
            ),
            case K of
                <<"talk_record_delete_tests:purge_failed_then_retry_test_">> ->
                    ok = talk_record_t20_frozen_config:same(
                        Entry,
                        maps:get(K, Authorized),
                        [<<"exhaustion_receipt_path">>]
                    );
                <<"talk_record_delete_tests:purge_complete_disk_queue_test_">> ->
                    ok = talk_record_t20_frozen_config:same(
                        Entry,
                        maps:get(K, Authorized),
                        [<<"preparation_receipt_path">>]
                    );
                _ ->
                    ok
            end
        end,
        Cases
    ).

verify_lease(R, Hash, Config, ConfigHash) ->
    LeasePath = required("T20_CASE_LEASE"),
    true = list_to_binary(LeasePath) =:= maps:get(<<"lease_path">>, Config),
    Lease = jsone:decode(private_file(LeasePath)),
    ConfigHash = maps:get(<<"frozen_config_sha256">>, Lease),
    ok = same_controller_schema(Lease, Config),
    #{
        <<"approved">> := true,
        <<"exclusive_beam_and_pg_window">> := true,
        <<"registry_sha256">> := Hash,
        <<"expires_at_unix">> := Until
    } = Lease,
    ok = talk_record_t20_frozen_config:lease_window(Lease, Config, erlang:system_time(second)),
    lists:foreach(
        fun(K) -> true = maps:get(K, R) =:= maps:get(K, Lease) end,
        [
            <<"owner">>,
            <<"controller_epoch">>,
            <<"controller_pid">>,
            <<"controller_process_start">>,
            <<"controller_thread_id">>
        ]
    ),
    verify_controller(R, Until).

same_controller_schema(Registry, Config) ->
    Schema = maps:get(<<"controller_schema">>, Config, <<"legacy">>),
    Schema = maps:get(<<"controller_schema">>, Registry, <<"legacy">>),
    case Schema of
        <<"legacy">> ->
            ok;
        <<"imboy_delivery_v3">> ->
            ok = talk_record_t20_frozen_config:same(
                Registry, Config, [<<"controller_run_id">>]
            );
        Other ->
            error({unsupported_controller_schema, Other})
    end.

verify_controller(R, Until) ->
    case maps:get(<<"controller_schema">>, R, <<"legacy">>) of
        <<"legacy">> -> verify_legacy_controller(R, Until);
        <<"imboy_delivery_v3">> -> verify_v3_controller(R);
        Other -> error({unsupported_controller_schema, Other})
    end.

verify_v3_controller(R) ->
    Lock = binary_to_list(maps:get(<<"controller_lock_path">>, R)),
    %% identity_file checks real regular file, no symlinks/group writes. V3's
    %% flock is held only during state mutations, never claimed as a test lease.
    _ = identity_file(Lock),
    StatePath = binary_to_list(maps:get(<<"controller_state_path">>, R)),
    true = filename:dirname(Lock) =:= filename:dirname(StatePath),
    true = filename:basename(Lock) =:= "controller.lock",
    true = filename:basename(StatePath) =:= "state.json",
    State = jsone:decode(identity_file(StatePath)),
    ok = talk_record_t20_frozen_config:controller_identity(
        R,
        #{<<"type">> => <<"regular">>, <<"path">> => list_to_binary(Lock)},
        State
    ),
    Host = maps:get(<<"controller_host">>, R),
    Host = command("/bin/hostname", []),
    Start = maps:get(<<"controller_process_start">>, R),
    Start = controller_process_start(maps:get(<<"controller_pid">>, R)),
    ok.

verify_legacy_controller(R, Until) ->
    Lock = binary_to_list(maps:get(<<"controller_lock_path">>, R)),
    Owner = jsone:decode(identity_file(filename:join(Lock, "owner.json"))),
    Pid = maps:get(<<"controller_pid">>, R),
    Start = maps:get(<<"controller_process_start">>, R),
    Host = maps:get(<<"controller_host">>, R),
    Host = command("/bin/hostname", []),
    OwnerUntil = calendar:rfc3339_to_system_time(
        binary_to_list(maps:get(<<"lease_until">>, Owner)), [{unit, second}]
    ),
    true = OwnerUntil >= Until,
    StatePath = maps:get(<<"controller_state_path">>, R),
    StatePath = maps:get(<<"state_path">>, Owner),
    State = jsone:decode(identity_file(binary_to_list(StatePath))),
    ok = talk_record_t20_frozen_config:controller_identity(R, Owner, State),
    true = is_integer(Pid) andalso Pid > 0,
    Start = controller_process_start(Pid),
    ok.

%% macOS forbids setuid /bin/ps in the test sandbox; libproc retains real identity checks.
controller_process_start(Pid) when is_integer(Pid), Pid > 0 ->
    case os:type() of
        {unix, darwin} ->
            Repo = required("T20_CASE_REPO"),
            Probe = filename:join([Repo, "test", "talk_record", "controller_pid_probe.py"]),
            command("/usr/bin/python3", ["-I", Probe, "--pid", integer_to_list(Pid)]);
        _ ->
            command("/bin/ps", ["-p", integer_to_list(Pid), "-o", "lstart="])
    end.

verify_case(Key, C, R) ->
    #{
        <<"database">> := Db,
        <<"disk_root">> := Root,
        <<"namespace">> := Ns,
        <<"clone_receipt_path">> := ReceiptPath,
        <<"clone_receipt_sha256">> := ReceiptHash
    } = C,
    <<"talk_record_t20_case_", Suffix/binary>> = Db,
    match = re:run(Suffix, <<"^[a-z0-9_]+$">>, [{capture, none}]),
    true = byte_size(Db) =< 63 andalso byte_size(Ns) > 0,
    Runtime = binary_to_list(maps:get(<<"runtime_root">>, R)),
    Root = list_to_binary(filename:join([Runtime, "disk", binary_to_list(Db)])),
    assert_directory(binary_to_list(Root)),
    Bytes = private_file(binary_to_list(ReceiptPath)),
    ReceiptHash = hex(Bytes),
    Receipt = jsone:decode(Bytes),
    #{
        <<"case_key">> := Key,
        <<"database">> := Db,
        <<"clone_exit_code">> := 0,
        <<"empty_business_verified">> := true,
        <<"disk_root">> := Root,
        <<"namespace">> := Ns
    } = Receipt,
    lists:foreach(
        fun(K) -> true = maps:get(K, Receipt) =:= maps:get(K, R) end,
        [
            <<"owner">>,
            <<"controller_epoch">>,
            <<"system_identifier">>,
            <<"migration_head">>,
            <<"migration_count">>,
            <<"candidate_commit">>,
            <<"frozen_config_sha256">>,
            <<"source_sha256">>,
            <<"controller_thread_id">>,
            <<"controller_pid">>,
            <<"controller_process_start">>,
            <<"host">>,
            <<"connection_port">>,
            <<"server_port">>,
            <<"user">>,
            <<"migration_versions">>
        ]
    ),
    ok.

connection_options() ->
    ok = recheck(),
    #{registry := R, entry := C} = current(),
    Host = required("T20_PG_HOST"),
    Port = list_to_integer(required("T20_PG_PORT")),
    User = required("T20_PG_USER"),
    true = list_to_binary(Host) =:= maps:get(<<"host">>, R),
    true = Port =:= maps:get(<<"connection_port">>, R),
    true = list_to_binary(User) =:= maps:get(<<"user">>, R),
    #{
        host => Host,
        port => Port,
        username => list_to_binary(User),
        password => required("T20_PG_PASSWORD"),
        database => maps:get(<<"database">>, C),
        ssl => false,
        timeout => 15000,
        codecs => [{epgsql_codec_rfc3339_bin, []}]
    }.

verify_connected(Conn) ->
    ok = recheck(),
    #{registry := R, entry := C} = current(),
    {ok, [Row]} = elib_pg:query(
        Conn,
        <<
            "SELECT current_database()::text db,current_user::text usr,inet_server_port() port,"
            "(SELECT system_identifier::text FROM pg_control_system()) sysid,"
            "(SELECT max(version) FROM public.schema_migrations_history) head,"
            "(SELECT count(*) FROM public.schema_migrations_history) migrations"
        >>,
        []
    ),
    true = maps:get(<<"db">>, Row) =:= maps:get(<<"database">>, C),
    lists:foreach(
        fun({Actual, Expected}) ->
            true = maps:get(Actual, Row) =:= maps:get(Expected, R)
        end,
        [
            {<<"usr">>, <<"user">>},
            {<<"port">>, <<"server_port">>},
            {<<"sysid">>, <<"system_identifier">>},
            {<<"head">>, <<"migration_head">>},
            {<<"migrations">>, <<"migration_count">>}
        ]
    ),
    {ok, Versions} = elib_pg:query(
        Conn,
        <<"SELECT version FROM public.schema_migrations_history ORDER BY version">>,
        []
    ),
    true = [maps:get(<<"version">>, V) || V <- Versions] =:= maps:get(<<"migration_versions">>, R),
    {ok, [#{<<"dirty">> := false}]} = elib_pg:query(
        Conn,
        <<"SELECT dirty FROM public.schema_migrations">>,
        []
    ),
    ok.

verify_empty(Conn) ->
    ok = verify_connected(Conn),
    Tables = [
        <<"talk_record">>,
        <<"talk_record_cleanup_retry">>,
        <<"talk_record_job">>,
        <<"talk_record_attachment_ref">>,
        <<"attachment">>,
        <<"attach_pending">>,
        <<"attachment_object_deletion">>,
        <<"attachment_derived_write">>,
        <<"enterprise_attachment_retention">>,
        <<"workspace_attachment_retention">>
    ],
    lists:foreach(
        fun(Table) ->
            {ok, [#{<<"n">> := 0}]} = elib_pg:query(
                Conn,
                <<"SELECT count(*) n FROM public.", Table/binary>>,
                []
            )
        end,
        Tables
    ),
    ok.

%% This binding deliberately excludes registry SHA: the final receipt hash is added
%% to that registry only AFTER publication. The immutable clone receipt is never rewritten.
disk_queue_binding() ->
    ok = recheck(),
    #{
        case_key := <<"talk_record_delete_tests:purge_complete_disk_queue_test_">>,
        registry := Registry,
        entry := Case
    } = current(),
    maps:merge(
        maps:with(
            talk_record_t20_frozen_config:binding_keys() ++
                [<<"frozen_config_sha256">>],
            Registry
        ),
        maps:with(
            [
                <<"database">>,
                <<"disk_root">>,
                <<"namespace">>,
                <<"clone_receipt_path">>,
                <<"clone_receipt_sha256">>,
                <<"preparation_receipt_path">>
            ],
            Case
        )
    ).

prepared_disk_queue(C) ->
    ok = verify_connected(C),
    #{entry := Case} = current(),
    Path = binary_to_list(maps:get(<<"preparation_receipt_path">>, Case)),
    Bytes = private_file(Path),
    true = hex(Bytes) =:= maps:get(<<"preparation_receipt_sha256">>, Case),
    #{
        <<"status">> := <<"PREPARED_NOT_TEST_PASS">>,
        <<"binding">> := Binding,
        <<"snapshot">> := Snapshot,
        <<"staged_path">> := StagedPath,
        <<"staged_sha256">> := StagedHash,
        <<"prepared_at_db_unix">> := PreparedAt
    } =
        jsone:decode(Bytes),
    Binding = disk_queue_binding(),
    StagedBytes = private_file(binary_to_list(StagedPath)),
    StagedHash = hex(StagedBytes),
    #{
        <<"status">> := <<"STAGED_WAIT_NATURAL_EXPIRY_NOT_TEST_PASS">>,
        <<"binding">> := Binding,
        <<"snapshot">> := Snapshot,
        <<"staged_at_db_unix">> := StagedAt
    } = jsone:decode(StagedBytes),
    #{<<"record">> := #{<<"id">> := Rid}, <<"uploads">> := Uploads} = Snapshot,
    Snapshot = disk_queue_snapshot(C, Rid),
    {ok, [#{<<"now">> := Now}]} = elib_pg:query(
        C,
        <<"SELECT extract(epoch FROM clock_timestamp())::float8 AS now">>,
        []
    ),
    true = StagedAt < PreparedAt andalso PreparedAt =< Now,
    lists:foreach(
        fun(#{<<"expires_epoch">> := Expiry}) ->
            T = binary_to_float(Expiry),
            true = StagedAt < T andalso T < PreparedAt
        end,
        Uploads
    ),
    Snapshot.

%% Strict read-only facts shared by create, finalize and original case admission.
%% Expiry is read verbatim; only the caller decides whether it is still live or naturally past.
disk_queue_snapshot(C, Rid) ->
    ok = verify_connected(C),
    #{registry := Registry, entry := Case} = current(),
    Clone = jsone:decode(private_file(binary_to_list(maps:get(<<"clone_receipt_path">>, Case)))),
    #{<<"case_key">> := <<"talk_record_delete_tests:purge_complete_disk_queue_test_">>} = Clone,
    {ok, [#{<<"marker">> := Marker}]} = elib_pg:query(
        C,
        <<
            "SELECT shobj_description(oid,'pg_database') AS marker FROM pg_database"
            " WHERE datname=current_database()"
        >>,
        []
    ),
    Marker = maps:get(<<"database_marker">>, Clone),
    ok = disk_queue_counts(C),
    {ok, [Record]} = elib_pg:query(
        C,
        <<"SELECT r.* FROM talk_record r WHERE r.id=$1">>,
        [Rid]
    ),
    #{
        <<"owner_user_id">> := 700001,
        <<"scope">> := <<"personal">>,
        <<"organization_id">> := null,
        <<"workspace_id">> := null,
        <<"revision">> := 1,
        <<"acl_revision">> := 1,
        <<"deleted_at">> := null
    } = Record,
    {ok, [#{<<"status">> := 1, <<"account_type">> := 0}]} = elib_pg:query(
        C,
        <<"SELECT status,account_type FROM public.\"user\" WHERE id=700001">>,
        []
    ),
    {ok, Jobs = [#{<<"kind">> := <<"assemble">>, <<"state">> := <<"succeeded">>}]} =
        elib_pg:query(
            C, <<"SELECT j.* FROM talk_record_job j WHERE j.record_id=$1 ORDER BY j.id">>, [Rid]
        ),
    {ok, Captures = [#{<<"status">> := <<"ended">>}]} = elib_pg:query(
        C,
        <<"SELECT c.* FROM talk_record_capture c WHERE c.record_id=$1 ORDER BY c.id">>,
        [Rid]
    ),
    {ok, Parts = [_, _]} = elib_pg:query(
        C,
        <<"SELECT p.* FROM talk_record_audio_part p WHERE p.record_id=$1 ORDER BY p.seq">>,
        [Rid]
    ),
    {ok, Refs} = elib_pg:query(
        C,
        <<"SELECT r.* FROM talk_record_attachment_ref r WHERE r.record_id=$1 ORDER BY r.id">>,
        [Rid]
    ),
    {ok, Derived = [#{<<"state">> := <<"complete">>}, #{<<"state">> := <<"complete">>}]} =
        elib_pg:query(
            C,
            <<
                "SELECT w.* FROM attachment_derived_write w JOIN talk_record_job j"
                " ON j.id=w.source_job_id WHERE j.record_id=$1 ORDER BY w.attachment_id"
            >>,
            [Rid]
        ),
    {ok, Assets} = elib_pg:query(
        C,
        <<
            "SELECT a.* FROM attachment a WHERE a.id IN"
            " (SELECT attachment_id FROM talk_record_attachment_ref WHERE record_id=$1 UNION"
            " SELECT w.attachment_id FROM attachment_derived_write w JOIN talk_record_job j ON j.id=w.source_job_id"
            " WHERE j.record_id=$1) ORDER BY a.id"
        >>,
        [Rid]
    ),
    5 = length(Assets),
    [Extra] = [A || #{<<"id">> := 8101} = A <- Assets],
    2 = length(lists:usort([maps:get(<<"attachment_id">>, P) || P <- Parts])),
    2 = length(lists:usort([maps:get(<<"attachment_id">>, D) || D <- Derived])),
    true =
        lists:sort([maps:get(<<"id">>, A) || A <- Assets]) =:=
            lists:usort(
                [8101] ++ [maps:get(<<"attachment_id">>, P) || P <- Parts] ++
                    [maps:get(<<"attachment_id">>, D) || D <- Derived]
            ),
    Objects = [disk_queue_object(A, Case) || A <- Assets],
    5 = length(lists:usort([maps:get(<<"disk_file">>, O) || O <- Objects])),
    {ok, Uploads = [_, _]} = elib_pg:query(
        C,
        <<
            "SELECT p.*,"
            "extract(epoch FROM p.upload_expires_at)::text AS expires_epoch"
            " FROM attach_pending p WHERE p.confirmed_attachment_id IN"
            " (SELECT attachment_id FROM talk_record_audio_part WHERE record_id=$1) ORDER BY p.confirmed_attachment_id"
        >>,
        [Rid]
    ),
    true =
        [maps:get(<<"confirmed_attachment_id">>, U) || U <- Uploads] =:=
            lists:sort([maps:get(<<"attachment_id">>, P) || P <- Parts]),
    lists:foreach(fun(U) -> verify_disk_upload(U, Assets, Parts, Rid) end, Uploads),
    [Job] = Jobs,
    [Capture] = Captures,
    {ok, [Assembly]} = elib_pg:query(
        C,
        <<"SELECT a.* FROM talk_record_assembly a WHERE a.record_id=$1">>,
        [Rid]
    ),
    verify_disk_assembly(Record, Capture, Parts, Job, Assembly, Derived, Refs),
    ok = recheck(),
    #{registry := Registry, entry := Case} = current(),
    #{
        <<"record">> => Record,
        <<"jobs">> => Jobs,
        <<"captures">> => Captures,
        <<"parts">> => Parts,
        <<"refs">> => Refs,
        <<"derived">> => Derived,
        <<"assembly">> => Assembly,
        <<"objects">> => Objects,
        <<"uploads">> => Uploads,
        <<"extra_attachment_path">> => maps:get(<<"path">>, Extra)
    }.

verify_disk_upload(
    #{
        <<"confirmed_attachment_id">> := Aid,
        <<"object_key">> := Path,
        <<"session_state">> := <<"confirmed">>,
        <<"expires_epoch">> := Expiry
    } = U,
    Assets,
    Parts,
    Rid
) ->
    true = is_binary(Expiry),
    [A] = [Asset || #{<<"id">> := Id} = Asset <- Assets, Id =:= Aid],
    [P] = [Part || #{<<"attachment_id">> := Id} = Part <- Parts, Id =:= Aid],
    Path = maps:get(<<"path">>, A),
    true = maps:get(<<"record_id">>, P) =:= Rid,
    true = maps:get(<<"scope">>, U) =:= <<"talk_record">>,
    true = maps:get(<<"creator_user_id">>, U) =:= 700001,
    %% V5 binds scope_ref in registration; the legacy pending column remains NULL.
    #{<<"subject">> := Subject, <<"declaration">> := Declaration} =
        disk_upload_json(maps:get(<<"upload_registration">>, U)),
    #{<<"kind">> := <<"user">>, <<"principal">> := <<"700001">>} = Subject,
    true = map_size(Subject) =:= 2,
    true = maps:get(<<"scope">>, Declaration) =:= <<"talk_record">>,
    true = maps:get(<<"scope_ref">>, Declaration) =:= integer_to_binary(Rid),
    true = maps:get(<<"scope_ref">>, A) =:= integer_to_binary(Rid),
    true = maps:get(<<"filename">>, Declaration) =:= maps:get(<<"name">>, A),
    true = maps:get(<<"mime">>, Declaration) =:= maps:get(<<"mime_type">>, A),
    lists:foreach(
        fun(K) -> true = maps:get(K, U) =:= maps:get(K, A) end,
        [
            <<"creator_user_id">>,
            <<"scope">>,
            <<"storage_driver">>,
            <<"storage_namespace">>,
            <<"storage_container">>,
            <<"storage_locator_sha256">>
        ]
    ),
    %% epgsql default json codec returns UTF-8 JSON bytes; retain those raw bytes
    %% in the snapshot and decode only for this content/ownership assertion.
    Pin = disk_upload_json(maps:get(<<"upload_content">>, U)),
    #{<<"sha256">> := Hash, <<"size_bytes">> := Size, <<"mime">> := Mime} = Pin,
    Hash = maps:get(<<"sha256">>, P),
    Hash = maps:get(<<"file_hash256">>, A),
    true = binary_to_integer(Size) =:= maps:get(<<"byte_size">>, P),
    true = binary_to_integer(Size) =:= maps:get(<<"size">>, A),
    Mime = maps:get(<<"mime_type">>, A),
    ok.

disk_upload_json(B) when is_binary(B) -> jsone:decode(B);
disk_upload_json(M) when is_map(M) -> M.

verify_disk_assembly(Record, Capture, Parts, Job, Assembly, Derived, Refs) ->
    Rid = maps:get(<<"id">>, Record),
    Cid = maps:get(<<"id">>, Capture),
    Jid = maps:get(<<"id">>, Job),
    Attempt = maps:get(<<"attempt">>, Job),
    Rid = maps:get(<<"record_id">>, Capture),
    Rid = maps:get(<<"record_id">>, Job),
    Rid = maps:get(<<"record_id">>, Assembly),
    Cid = maps:get(<<"capture_id">>, Assembly),
    Jid = maps:get(<<"job_id">>, Assembly),
    2 = maps:get(<<"part_count">>, Assembly),
    true = maps:get(<<"input_manifest_hash">>, Job) =:= maps:get(<<"manifest_hash">>, Assembly),
    lists:foreach(
        fun(P) ->
            Rid = maps:get(<<"record_id">>, P),
            Cid = maps:get(<<"capture_id">>, P),
            <<"confirmed">> = maps:get(<<"upload_status">>, P)
        end,
        Parts
    ),
    lists:foreach(
        fun(D) ->
            Jid = maps:get(<<"source_job_id">>, D),
            Attempt = maps:get(<<"source_attempt">>, D)
        end,
        Derived
    ),
    verify_disk_refs(Record, Parts, Job, Assembly, Derived, Refs).

verify_disk_refs(Record, Parts, Job, Assembly, Derived, Refs) ->
    Rid = maps:get(<<"id">>, Record),
    Jid = maps:get(<<"id">>, Job),
    Attempt = maps:get(<<"attempt">>, Job),
    Permanent = [
        R
     || #{
            <<"state">> := <<"bound">>,
            <<"lease_until">> := null,
            <<"released_at">> := null
        } = R <- Refs
    ],
    Released = [
        R
     || #{
            <<"state">> := <<"released">>,
            <<"lease_until">> := L,
            <<"released_at">> := At
        } = R <- Refs,
        is_binary(L),
        is_binary(At)
    ],
    9 = length(Refs),
    5 = length(Permanent),
    4 = length(Released),
    true = lists:all(fun(#{<<"record_id">> := Id}) -> Id =:= Rid end, Refs),
    AudioAid = maps:get(<<"audio_attachment_id">>, Assembly),
    ManifestAid = maps:get(<<"audio_manifest_attachment_id">>, Record),
    true =
        lists:sort([AudioAid, ManifestAid]) =:=
            lists:sort([maps:get(<<"attachment_id">>, D) || D <- Derived]),
    Artifacts = [{AudioAid, <<"transcoded_audio">>}, {ManifestAid, <<"manifest">>}],
    ExpectedPermanent =
        [{8101, <<"audio_part">>, <<"audio_part:1">>, null}] ++
            [
                {
                    maps:get(<<"attachment_id">>, P),
                    <<"audio_part">>,
                    talk_record_assembly_logic:part_business_key(maps:get(<<"id">>, P)),
                    maps:get(<<"capture_id">>, P)
                }
             || P <- Parts
            ] ++
            [
                {Aid, Role, <<"assembly:", Role/binary, ":", (integer_to_binary(Aid))/binary>>,
                    null}
             || {Aid, Role} <- Artifacts
            ],
    ExpectedReleased =
        [
            {
                maps:get(<<"attachment_id">>, P),
                <<"audio_part">>,
                talk_record_assembly_logic:lease_business_key(Jid, Attempt, maps:get(<<"id">>, P)),
                null
            }
         || P <- Parts
        ] ++
            [
                {Aid, Role, <<"assembly_pending:", (integer_to_binary(Aid))/binary>>, null}
             || {Aid, Role} <- Artifacts
            ],
    RefKey = fun(R) ->
        {
            maps:get(<<"attachment_id">>, R),
            maps:get(<<"role">>, R),
            maps:get(<<"business_key">>, R),
            maps:get(<<"capture_id">>, R)
        }
    end,
    true = lists:sort(ExpectedPermanent) =:= lists:sort([RefKey(R) || R <- Permanent]),
    true = lists:sort(ExpectedReleased) =:= lists:sort([RefKey(R) || R <- Released]),
    ok.

disk_queue_counts(C) ->
    lists:foreach(
        fun({Table, Expected}) ->
            {ok, [#{<<"n">> := Expected}]} = elib_pg:query(
                C,
                <<"SELECT count(*) AS n FROM public.", Table/binary>>,
                []
            )
        end,
        [
            {<<"talk_record">>, 1},
            {<<"attachment">>, 5},
            {<<"attach_pending">>, 2},
            {<<"attachment_derived_write">>, 2},
            {<<"talk_record_grant">>, 0},
            {<<"talk_record_cleanup_retry">>, 0},
            {<<"attachment_object_deletion">>, 0},
            {<<"enterprise_attachment_retention">>, 0},
            {<<"workspace_attachment_retention">>, 0}
        ]
    ),
    ok.

disk_queue_object(
    #{
        <<"path">> := Path,
        <<"lifecycle_state">> := <<"live">>,
        <<"gc_verified">> := GcVerified,
        <<"storage_driver">> := <<"disk">>
    } = A,
    Case
) ->
    true = is_boolean(GcVerified),
    Ns = maps:get(<<"namespace">>, Case),
    Ns = maps:get(<<"storage_namespace">>, A),
    <<"capture">> = maps:get(<<"storage_container">>, A),
    {ok,
        #{
            driver := disk,
            namespace := Ns,
            container := Container,
            key := Key,
            locator_digest := Digest
        } = I} = attachment_storage:frozen_location(Path, A),
    relative = filename:pathtype(binary_to_list(Key)),
    false = lists:any(
        fun(S) -> S =:= ".." orelse S =:= "." end, filename:split(binary_to_list(Key))
    ),
    File = filename:join([
        binary_to_list(maps:get(<<"disk_root">>, Case)),
        binary_to_list(Container),
        binary_to_list(Key)
    ]),
    Bytes = private_source(File),
    {ok, Bytes} = attachment_storage_disk:read_snapshot(I),
    Size = byte_size(Bytes),
    true = Size > 0,
    Size = maps:get(<<"size">>, A),
    Hash = hex(Bytes),
    Hash = maps:get(<<"file_hash256">>, A),
    #{
        <<"attachment">> => A,
        <<"path">> => Path,
        <<"disk_file">> => list_to_binary(File),
        <<"storage_namespace">> => Ns,
        <<"storage_container">> => Container,
        <<"resolved_storage_key">> => Key,
        <<"storage_locator_sha256">> => Digest,
        <<"byte_size">> => Size,
        <<"sha256">> => Hash
    }.

configure_storage() ->
    ok = recheck(),
    #{entry := #{<<"disk_root">> := Root, <<"namespace">> := Ns}} = current(),
    ok = application:set_env(imboy, attachment_storage_registry, #{
        version => 1,
        upload_namespaces => #{<<"talk_record">> => Ns},
        roots => #{
            t20_owned_case => #{
                driver => <<"disk">>,
                namespace => Ns,
                container => <<"capture">>,
                endpoint => <<"file://", Root/binary>>
            }
        }
    }),
    true = os:putenv("TALK_UPLOAD_DISK_ROOT", binary_to_list(Root)),
    ok.

required(Name) ->
    case os:getenv(Name) of
        false -> error({required_test_environment, Name});
        Value -> Value
    end.
key(M, F) -> <<(atom_to_binary(M))/binary, ":", (atom_to_binary(F))/binary>>.
hex(Bytes) -> binary:encode_hex(crypto:hash(sha256, Bytes), lowercase).
private_file(Path) ->
    assert_directory(filename:dirname(Path)),
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(Path),
    true = Mode band 8#077 =:= 0,
    private_source(Path).
%% Controller identity files contain no secrets and may be world-readable (0644),
%% but must not be symlinks or writable by group/other. Resource receipts remain 0600.
identity_file(Path) ->
    assert_directory(filename:dirname(Path)),
    {ok, #file_info{type = regular, mode = Mode}} = file:read_link_info(Path),
    true = Mode band 8#022 =:= 0,
    private_source(Path).
private_source(Path) ->
    assert_directory(filename:dirname(Path)),
    {ok, #file_info{type = regular}} = file:read_link_info(Path),
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
git_head(Repo) -> command(os:find_executable("git"), ["-C", Repo, "rev-parse", "HEAD"]).
command(Executable, Args) ->
    Port = open_port({spawn_executable, Executable}, [
        binary, exit_status, use_stdio, stderr_to_stdout, {args, Args}
    ]),
    collect(Port, <<>>).
collect(Port, Acc) ->
    receive
        {Port, {data, B}} -> collect(Port, <<Acc/binary, B/binary>>);
        {Port, {exit_status, 0}} -> list_to_binary(string:trim(binary_to_list(Acc)));
        {Port, {exit_status, Code}} -> error({identity_command_failed, Code})
    after 5000 ->
        port_close(Port),
        error(identity_command_timeout)
    end.

required_sources(Repo) ->
    Trees = lists:append([source_tree(Repo, Dir) || Dir <- ["src", "include", "priv/migrations"]]),
    Tests = [
        <<"test/talk_record/", Name/binary, ".erl">>
     || Name <-
            [
                <<"talk_record_t20_case_registry">>,
                <<"talk_record_t20_frozen_config">>,
                <<"talk_record_t20_frozen_config_tests">>,
                <<"talk_record_t20_support">>,
                <<"talk_record_delete_tests">>,
                <<"talk_record_t20_handler_tests">>,
                <<"talk_record_t05_support">>,
                <<"talk_record_t07_support">>,
                <<"talk_record_t12_support">>,
                <<"talk_record_parent_fixture">>,
                <<"talk_record_upload_test_support">>,
                <<"talk_record_cleanup_retry_pg_checks">>,
                <<"talk_record_cleanup_retry_tests">>
            ]
    ],
    Probes = [
        <<"test/talk_record/controller_pid_probe.py">>,
        <<"test/talk_record/test_controller_pid_probe.py">>
    ],
    HttpHelpers = [<<"test/common/cowboy_req_h.erl">>],
    lists:usort(Trees ++ Tests ++ HttpHelpers ++ Probes).

source_tree(Repo, Relative) ->
    {ok, Names} = file:list_dir(filename:join(Repo, Relative)),
    lists:append([source_entry(Repo, filename:join(Relative, Name)) || Name <- Names]).
source_entry(Repo, Relative) ->
    {ok, Info} = file:read_link_info(filename:join(Repo, Relative)),
    case Info#file_info.type of
        directory -> source_tree(Repo, Relative);
        regular -> [list_to_binary(Relative)];
        Other -> error({unsupported_candidate_source_type, Relative, Other})
    end.

required_cases() ->
    [
        <<"talk_record_delete_tests:delete_owner_202_pending_test_">>,
        <<"talk_record_delete_tests:delete_preconditions_test_">>,
        <<"talk_record_delete_tests:delete_idempotent_replay_test_">>,
        <<"talk_record_delete_tests:delete_fences_active_capture_test_">>,
        <<"talk_record_delete_tests:revocation_immediate_test_">>,
        <<"talk_record_delete_tests:playback_ticket_revoked_on_delete_test_">>,
        <<"talk_record_delete_tests:whitelist_shape_test_">>,
        <<"talk_record_delete_tests:whitelist_flags_test_">>,
        <<"talk_record_delete_tests:purge_complete_no_attachments_test_">>,
        <<"talk_record_delete_tests:purge_complete_disk_queue_test_">>,
        <<"talk_record_delete_tests:purge_blocked_retention_test_">>,
        <<"talk_record_delete_tests:purge_blocked_hold_test_">>,
        <<"talk_record_delete_tests:purge_retained_by_other_reference_test_">>,
        <<"talk_record_delete_tests:purge_failed_then_retry_test_">>,
        <<"talk_record_delete_tests:late_results_not_published_test_">>,
        <<"talk_record_delete_tests:delete_grants_mutual_exclusion_test_">>,
        <<"talk_record_delete_tests:grants_first_then_delete_consistent_test_">>,
        <<"talk_record_delete_tests:preflight_facts_personal_vs_organization_test_">>,
        <<"talk_record_delete_tests:preflight_run_integration_test_">>,
        <<"talk_record_delete_tests:departure_custody_projection_test_">>,
        <<"talk_record_delete_tests:scan_organization_l2_regression_test_">>,
        <<"talk_record_t20_handler_tests:delete_handler_202_test_">>,
        <<"talk_record_t20_handler_tests:delete_handler_error_codes_test_">>,
        <<"talk_record_t20_handler_tests:get_after_delete_whitelist_test_">>,
        <<"talk_record_t20_handler_tests:item_method_not_allowed_test_">>
    ].
