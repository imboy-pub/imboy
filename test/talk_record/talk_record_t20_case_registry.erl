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

verify_controller(R, Until) ->
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
    lists:usort(Trees ++ Tests ++ Probes).

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
