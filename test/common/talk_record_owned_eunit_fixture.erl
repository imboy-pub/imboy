-module(talk_record_owned_eunit_fixture).
-include_lib("eunit/include/eunit.hrl").
-export([
    entry/1,
    budgets/0,
    assert_summary/2, assert_summary/3,
    validate_roots/2,
    required/1,
    marker/0,
    fixture_env/1,
    assert_owned_database/2
]).
-behaviour(eunit_listener).
-export([start/1, init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2]).

%% Full-suite mapping preserves order and duplicates. Original generators/bodies
%% execute unmodified in a fresh VM. T20 retains its frozen per-case registry;
%% other mapped modules use a fully migrated, single-module marker DB.
entry(Module) ->
    case lists:keyfind(Module, 1, budgets()) of
        false ->
            Module;
        {Module, Seconds} ->
            {timeout, Seconds, fun() ->
                {Result, Summary} = isolated_pg_fixture:run(
                    fun() -> run_module(Module) end, Seconds
                ),
                %% Explicit user device survives EUnit capture and logger filters.
                io:format(
                    user,
                    "TALK_OWNED_EUNIT_CHILD module=~p result=~p counts=~0p~n",
                    [Module, Result, Summary]
                ),
                assert_summary(Module, Result, Summary)
            end}
    end.

%% Conservative source audit: maximum original EUnit timeout * number of
%% generator/direct-test/inline-test slots + 900s migration/cleanup allowance.
%% Gateway's HTTP 15000ms options are not EUnit seconds; its outer limit is 4200s.
%% These are ceilings, not forced waits; nested original timeouts remain intact.
budgets() ->
    [
        {talk_record_assembly_relay_tests, 1260},
        {talk_record_capture_tests, 4740},
        {talk_record_content_tests, 3420},
        {talk_record_departure_tests, 3780},
        {talk_record_ds_pg_tests, 2340},
        {talk_record_ds_split_tests, 1620},
        {talk_record_export_relay_tests, 1140},
        {talk_record_export_tests, 4380},
        {talk_record_gateway_http_tests, 9300},
        {talk_record_governance_relay_tests, 1140},
        {talk_record_grant_tests, 3540},
        {talk_record_handler, 2340},
        {talk_record_marks_tests, 1620},
        {talk_record_participant_tests, 1620},
        {talk_record_processor_relay_tests, 1260},
        {talk_record_results_relay_tests, 1140},
        {talk_record_t07_handler_tests, 1860},
        {talk_record_t12_tests, 4500},
        {talk_record_t15_handler_tests, 1500},
        {talk_record_t17_handler_tests, 1620},
        {talk_record_t19_handler_tests, 1500},
        {talk_record_t20_handler_tests, 1380},
        {talk_record_attachment_lifecycle_tests, 4020},
        {talk_record_parent_fixture_tests, 930},
        {attachment_reference_guard_relay_tests, 960},
        {talk_record_client_audio_http_pg_tests, 9300}
    ].

%% T20 owns 25 frozen registry resources. Never provision or rebind a marker DB
%% here: each original generator activates and tears down its registered case.
run_module(talk_record_t20_handler_tests = Module) ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    undefined = whereis(pgsql),
    undefined = whereis(pooler_pgsql_pool_sup),
    undefined = whereis(elib_tsid_meck),
    run_original(Module);
run_module(Module) ->
    false = lists:keymember(imboy, 1, application:which_applications()),
    undefined = whereis(pgsql),
    Opts = owned_options(),
    Identity = verify_server(Opts),
    %% Empty password must never fall back to inherited shared configuration.
    ok = application:set_env(imboy, pg_conf, #{start_mfa => {epgsql, connect, [Opts]}}),
    Pg = inttest_marker_db:provision(#{
        env_prefix => <<"TALK_OWNED">>,
        connect_extra => #{codecs => [{epgsql_codec_rfc3339_bin, []}]}
    }),
    Outcome = capture(fun() ->
        bind_fixture(Pg, Identity),
        run_original(Module)
    end),
    Cleanup = capture(fun() -> cleanup(Pg, Opts) end),
    finish(Outcome, Cleanup).

required(Name) ->
    case os:getenv(Name) of
        false -> error({owned_talk_missing_env, Name});
        Value -> Value
    end.

owned_options() ->
    Host = required("TALK_OWNED_PG_HOST"),
    "127.0.0.1" = Host,
    Port = list_to_integer(required("TALK_OWNED_PG_PORT")),
    true = Port > 0 andalso Port =< 65535,
    ok = validate_roots(
        required("TALK_OWNED_RUN_ROOT"),
        required("TALK_OWNED_SCRATCH_ROOT")
    ),
    _ = marker(),
    _ = required("TALK_OWNED_PG_SYSTEM_ID"),
    #{
        host => Host,
        port => Port,
        username => required("TALK_OWNED_PG_USER"),
        password => required("TALK_OWNED_PG_PASSWORD"),
        database => required("TALK_OWNED_MAINT_DB"),
        timeout => 10000
    }.

%% Both directories must already exist, be absolute real directories, and have
%% no symlink components. Lexical '..' is rejected before normalization.
validate_roots(RunRoot, ScratchRoot) ->
    lists:foreach(
        fun(Path) ->
            true = filename:pathtype(Path) =:= absolute,
            true = not lists:member("..", filename:split(Path)),
            true = Path =:= filename:absname(Path),
            ok = real_directory(Path)
        end,
        [RunRoot, ScratchRoot]
    ),
    ScratchParts = filename:split(ScratchRoot),
    RunParts = filename:split(RunRoot),
    true = length(RunParts) > length(ScratchParts),
    true = lists:prefix(ScratchParts, RunParts),
    ok.

real_directory(Path) ->
    case file:read_link(Path) of
        {ok, _} ->
            error({owned_talk_symlink, Path});
        {error, einval} ->
            true = filelib:is_dir(Path),
            case filename:dirname(Path) of
                Path -> ok;
                Parent -> real_directory(Parent)
            end;
        {error, Reason} ->
            error({owned_talk_path, Path, Reason})
    end.

marker() ->
    case required("TALK_OWNED_PG_MARKER") of
        [] ->
            error(owned_talk_empty_marker);
        Value ->
            true = string:trim(Value) =/= [],
            list_to_binary(Value)
    end.

verify_server(Opts) ->
    {ok, _} = application:ensure_all_started(epgsql),
    with_conn(Opts, fun(C) ->
        Identity = database_identity(C),
        #{
            db := Db,
            system_id := Id,
            user := User,
            marker := Marker,
            server_port := Port
        } = Identity,
        Db = list_to_binary(required("TALK_OWNED_MAINT_DB")),
        Id = list_to_binary(required("TALK_OWNED_PG_SYSTEM_ID")),
        User = list_to_binary(required("TALK_OWNED_PG_USER")),
        Marker = marker(),
        true = is_integer(Port) andalso Port > 0 andalso Port =< 65535,
        Identity
    end).

%% Query actual server identity, never infer the container port from a host NAT port.
database_identity(C) ->
    {ok, _, [{Db, Id, User, Port, Marker}]} = epgsql:equery(
        C,
        "SELECT current_database(), system_identifier::text, current_user, "
        "inet_server_port(), shobj_description(d.oid,'pg_database') "
        "FROM pg_control_system(), pg_database d WHERE d.datname=current_database()",
        []
    ),
    #{db => Db, system_id => Id, user => User, server_port => Port, marker => Marker}.

fixture_env(Db) ->
    Server = [
        {"HOST", required("TALK_OWNED_PG_HOST")},
        {"PORT", required("TALK_OWNED_PG_PORT")},
        {"USER", required("TALK_OWNED_PG_USER")},
        {"PASSWORD", required("TALK_OWNED_PG_PASSWORD")}
    ],
    [
        {Prefix ++ Suffix, Value}
     || Prefix <- ["T05_PG_", "T06_PG_", "T12_PG_"],
        {Suffix, Value} <- Server
    ] ++
        [
            {"T05_PG_DB", Db},
            {"T12_PG_DB", Db},
            {"T06_PG_DATABASE", Db},
            {"TALK_PARENT_FIXTURE_DATABASE", Db}
        ].

bind_fixture(#{db := Db} = Pg, Identity) ->
    ok = bind_child_proof(Pg, Identity),
    Name = binary_to_list(Db),
    lists:foreach(fun({Key, Value}) -> true = os:putenv(Key, Value) end, fixture_env(Name)),
    Root = filename:join(required("TALK_OWNED_RUN_ROOT"), Name),
    ok = filelib:ensure_dir(filename:join(Root, ".owner")),
    true = os:putenv("T06_RUN_ROOT", Root),
    ok.

%% Called only with the fresh provision/1 result in this isolated child VM.
%% Before COMMENT, require an unmarked child with no other client sessions.
%% TimescaleDB background workers belong to the fresh database, not external clients.
bind_child_proof(#{conn := C, db := Db, server := Server} = Pg, Identity) ->
    false = maps:is_key(reuse_key, Pg),
    {match, _} = re:run(Db, "^inttest_talk_owned_[0-9]+_[0-9]+$"),
    Opts = owned_options(),
    Identity = verify_server(Opts),
    lists:foreach(
        fun(K) ->
            V = maps:get(K, Opts),
            V = maps:get(K, Server)
        end,
        [host, port, username, password]
    ),
    Before = database_identity(C),
    #{db := Db, marker := null} = Before,
    ok = same_server(Before, Identity),
    {ok, _, [{0}]} = epgsql:equery(
        C,
        "SELECT count(*) FROM pg_stat_activity WHERE datname=$1 AND pid<>pg_backend_pid()"
        " AND (backend_type='client backend' OR client_port IS NOT NULL)",
        [Db]
    ),
    Marker = marker(),
    {ok, _, [{CommentSql}]} = epgsql:equery(
        C,
        "SELECT format('COMMENT ON DATABASE %I IS %L', $1::text, $2::text)",
        [Db, Marker]
    ),
    {ok, _, _} = epgsql:squery(C, CommentSql),
    After = database_identity(C),
    After = Before#{marker => Marker},
    Proof = After#{host => maps:get(host, Opts), port => maps:get(port, Opts)},
    ok = application:set_env(imboy, talk_owned_child_proof, Proof).

same_server(Child, Maint) ->
    lists:foreach(
        fun(K) ->
            V = maps:get(K, Maint),
            V = maps:get(K, Child)
        end,
        [system_id, user, server_port]
    ),
    ok.

%% T07 must recheck the actual connection before its first sequence mutation.
%% No proof means no owned-child authorization, even with a matching name.
assert_owned_database(C, Db) ->
    {match, _} = re:run(Db, "^inttest_talk_owned_[0-9]+_[0-9]+$"),
    Db = list_to_binary(required("T05_PG_DB")),
    Host = required("T05_PG_HOST"),
    "127.0.0.1" = Host,
    Host = required("TALK_OWNED_PG_HOST"),
    Port = list_to_integer(required("T05_PG_PORT")),
    Port = list_to_integer(required("TALK_OWNED_PG_PORT")),
    User = list_to_binary(required("T05_PG_USER")),
    User = list_to_binary(required("TALK_OWNED_PG_USER")),
    Id = list_to_binary(required("TALK_OWNED_PG_SYSTEM_ID")),
    Marker = marker(),
    {ok, Proof} = application:get_env(imboy, talk_owned_child_proof),
    #{
        db := Db,
        host := Host,
        port := Port,
        user := User,
        system_id := Id,
        marker := Marker
    } = Proof,
    Actual = database_identity(C),
    Actual = maps:without([host, port], Proof),
    ok.

run_original(Module) ->
    Ref = make_ref(),
    Result = eunit:test({module, Module}, [verbose, {report, {?MODULE, [{owner, {self(), Ref}}]}}]),
    receive
        {Ref, {ok, Summary}} ->
            %% Preserve an original failed result even if cleanup also fails.
            ok = assert_summary(Module, Result, Summary),
            {Result, Summary};
        {Ref, Other} ->
            error({owned_talk_report_failed, Module, Other})
    after 1000 -> error({owned_talk_report_missing, Module})
    end.

%% The frozen T20 handler module has exactly four original generators.
%% Check inside the peer and again after its result crosses the VM boundary.
assert_summary(Module, Result, Summary) ->
    ok = assert_summary(Result, Summary),
    case Module of
        talk_record_t20_handler_tests ->
            case proplists:get_value(pass, Summary) of
                4 -> ok;
                _ -> error({owned_talk_case_count, Module, 4, Summary})
            end;
        _ ->
            ok
    end.

assert_summary(Result, Summary) ->
    Valid =
        Result =:= ok andalso is_integer(proplists:get_value(pass, Summary)) andalso
            proplists:get_value(pass, Summary) > 0 andalso
            lists:all(
                fun(Key) -> proplists:get_value(Key, Summary) =:= 0 end,
                [fail, skip, cancel]
            ),
    case Valid of
        true -> ok;
        false -> error({owned_talk_incomplete, Result, Summary})
    end.

cleanup(Pg, Opts) ->
    %% Independent cleanup attempts; release's best-effort DROP is not proof.
    Results = [
        capture(fun() -> close_owned_pool(maps:get(db, Pg)) end),
        capture(fun() -> inttest_marker_db:release(Pg) end),
        capture(fun() -> verify_dropped(Opts, maps:get(db, Pg)) end)
    ],
    case [R || R <- Results, R =/= {ok, ok}] of
        [] -> ok;
        Errors -> error({owned_talk_cleanup_failed, Errors})
    end.

close_owned_pool(Db) ->
    case whereis(pgsql) of
        undefined ->
            ok;
        Pid ->
            C = pooler:take_member(pgsql),
            try
                {ok, _, [{Db}]} = epgsql:squery(C, "SELECT current_database()")
            after
                pooler:return_member(pgsql, C)
            end,
            Pid = whereis(pgsql),
            ok = pooler:rm_pool(pgsql)
    end.

verify_dropped(Opts, Db) ->
    with_conn(Opts, fun(C) ->
        {ok, _, [{0}]} = epgsql:equery(C, "SELECT count(*) FROM pg_database WHERE datname=$1", [Db]),
        ok
    end).

with_conn(Opts, Fun) ->
    {ok, C} = inttest_marker_db:safe_connect(Opts),
    try
        Fun(C)
    after
        epgsql:close(C)
    end.

capture(Fun) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:Stack -> {error, Class, Reason, Stack}
    end.
finish({error, Class, Reason, Stack}, Cleanup) ->
    case Cleanup of
        {ok, ok} -> ok;
        _ -> io:format(standard_error, "TALK_OWNED_CLEANUP_AFTER_FAILURE ~p~n", [Cleanup])
    end,
    erlang:raise(Class, Reason, Stack);
finish({ok, Value}, {ok, ok}) ->
    Value;
finish({ok, _}, {error, Class, Reason, Stack}) ->
    erlang:raise(Class, Reason, Stack).

start(Options) -> eunit_listener:start(?MODULE, Options).
init(Options) -> proplists:get_value(owner, Options).
handle_begin(_Type, _Data, State) -> State.
handle_end(_Type, _Data, State) -> State.
handle_cancel(_Type, _Data, State) -> State.
terminate(Result, {Owner, Ref}) ->
    Owner ! {Ref, Result},
    ok.
