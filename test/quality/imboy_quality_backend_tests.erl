%% Synthetic obligations are written independently of the observed event stream.
-module(imboy_quality_backend_tests).
-export([signal_fixture/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

selection_test() ->
    ?assertEqual(legacy, imboy_quality_backend:selection(false, ["eunit", "--full"])),
    ?assertEqual(backend, imboy_quality_backend:selection("1", ["backend-acceptance"])),
    ?assertError(
        backend_requires_explicit_operation,
        imboy_quality_backend:selection("1", ["eunit", "--full"])
    ),
    ?assertError(
        invalid_backend_selector,
        imboy_quality_backend:selection("true", ["backend-acceptance"])
    ),
    ?assertError(
        backend_selector_required,
        imboy_quality_backend:selection(false, ["backend-acceptance"])
    ).

expected_binding_test() ->
    C = context(),
    E = expected(C),
    P = per_root(),
    ?assertEqual(ok, imboy_quality_backend:verify_expected(E, C, [a, b, z], P)),
    ?assertMatch({error, _}, imboy_quality_backend:verify_expected(undefined, C, [a, b, z], P)),
    ?assertMatch(
        {error, _}, imboy_quality_backend:verify_expected(E, C#{build := <<"drift">>}, [a, b, z], P)
    ),
    ?assertMatch({error, _}, imboy_quality_backend:verify_expected(E, C, [a, b, z, special], P)),
    ?assertMatch(
        {error, _}, imboy_quality_backend:verify_expected(E, C, [a, b, z], maps:remove(b, P))
    ),
    ?assertMatch(
        {error, _}, imboy_quality_backend:verify_expected(E, C, [a, b, z], P#{b := root(b, 2)})
    ),
    lists:foreach(
        fun(K) ->
            Bad = (maps:get(a, P))#{
                counts := #{pass => 1, fail => 0, skip => 0, cancel => 0, K => 1}
            },
            ?assertMatch(
                {error, _}, imboy_quality_backend:verify_expected(E, C, [a, b, z], P#{a := Bad})
            )
        end,
        [skip, cancel]
    ),
    ?assertMatch(
        {error, _}, imboy_quality_backend:verify_expected(E, C, [a, b, z], P#{z := root(z, 1)})
    ).

owner_and_schema_test() ->
    ?assertNot(imboy_quality_backend:is_state(#{<<"task_id">> => <<"IMBOY_ERLANG_QUALITY">>})),
    ?assertMatch(
        {error, _},
        imboy_quality_backend:authorize(
            #{<<"schema">> => <<"IMBOY_BACKEND_ACCEPTANCE_V1">>}, cleanup
        )
    ),
    ?assertError(_, imboy_quality_backend:resource_root("/private/tmp", #{})).

reservation_test() ->
    Base =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/reserve-" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Base),
    try
        ?assertEqual(ok, imboy_quality_backend:reserve_directory(Base ++ "/control")),
        ?assertError(
            {run_already_reserved, _}, imboy_quality_backend:reserve_directory(Base ++ "/control")
        )
    after
        file:del_dir(Base ++ "/control"),
        file:del_dir(Base)
    end.

route_test() ->
    ?assertEqual(backend_acceptance, imboy_quality_build:entry_route(#{backend_acceptance => "1"})),
    ?assertEqual(
        backend_acceptance,
        imboy_quality_build:entry_route(#{backend_acceptance => "1", eunit_config => environment})
    ),
    ?assertEqual(native, imboy_quality_build:entry_route(#{inner => environment})).

owned_directory_identity_test() ->
    Base =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/directory-" ++
            integer_to_list(erlang:unique_integer([positive])),
    Ctrl = Base ++ "/control",
    Root = Base ++ "/resources",
    Marker = Root ++ "/.imboy-quality-owned",
    ok = file:make_dir(Base),
    ok = file:make_dir(Ctrl),
    ok = file:make_dir(Root),
    {ok, #file_info{uid = Uid}} = file:read_link_info(Ctrl),
    Owner = #{
        <<"start_identity">> => <<"synthetic-owner">>, <<"token">> => binary:copy(<<"a">>, 64)
    },
    Bytes = term_to_binary({backend_acceptance, "run1", Owner}),
    ok = file:write_file(Marker, Bytes, [exclusive]),
    D = #{
        <<"kind">> => <<"OWNED_DIRECTORY">>,
        <<"path">> => list_to_binary(Root),
        <<"ownership_marker">> => list_to_binary(Marker),
        <<"ownership_marker_sha256">> => hash(Bytes),
        <<"filesystem_identity">> => identity(Root),
        <<"control_identity">> => identity(Ctrl),
        <<"parent_identity">> => identity(Base),
        <<"uid">> => Uid,
        <<"owner_start_identity">> => <<"synthetic-owner">>
    },
    S = #{
        <<"schema">> => <<"IMBOY_BACKEND_ACCEPTANCE_V1">>,
        <<"control_dir">> => list_to_binary(Ctrl),
        <<"owned_resources">> => [D],
        <<"owner">> => Owner,
        <<"run_id">> => <<"run1">>
    },
    try
        ?assertEqual(Root, imboy_quality_backend:resource_root(Ctrl, S)),
        ?assertError(
            _,
            imboy_quality_backend:resource_root(Ctrl, S#{
                <<"owner">> := Owner#{<<"start_identity">> := <<"forged">>}
            })
        ),
        ?assertError(
            _,
            imboy_quality_backend:resource_root(Ctrl, S#{
                <<"owned_resources">> := [D#{<<"path">> := <<"/private/tmp/foreign">>}]
            })
        ),
        %% A replacement directory at the same pathname has a different inode.
        ok = file:rename(Root, Root ++ "-old"),
        ok = file:make_dir(Root),
        ?assertError(_, imboy_quality_backend:resource_root(Ctrl, S)),
        ok = file:del_dir(Root),
        ok = file:rename(Root ++ "-old", Root),
        ok = file:write_file(Marker, <<"forged">>),
        ?assertError(_, imboy_quality_backend:resource_root(Ctrl, S))
    after
        file:delete(Marker),
        file:del_dir(Root),
        file:del_dir(Ctrl),
        file:del_dir(Base)
    end.

postgres_exit_and_terminal_gate_test() ->
    PG = #{
        <<"kind">> => <<"ISOLATED_POSTGRESQL">>,
        <<"status">> => <<"STOPPED">>,
        <<"postgres_exit">> => 0,
        <<"pg_ctl_exit">> => 0,
        <<"log_ok">> => true
    },
    ?assertEqual(ok, imboy_quality_backend:verify_pg_exit(#{<<"owned_resources">> => [PG]})),
    lists:foreach(
        fun(P) ->
            ?assertMatch(
                {error, _}, imboy_quality_backend:verify_pg_exit(#{<<"owned_resources">> => [P]})
            )
        end,
        [
            PG#{<<"postgres_exit">> := 143},
            maps:remove(<<"pg_ctl_exit">>, PG),
            PG#{<<"status">> := <<"RUNNING">>}
        ]
    ),
    Good = #{
        <<"scope">> => <<"BACKEND_ACCEPTANCE">>,
        <<"run_id">> => <<"single-use">>,
        <<"cleanup">> => #{<<"ok">> => true},
        <<"input_binding">> => <<"VERIFIED">>,
        <<"roots_total">> => 3,
        <<"roots_verified">> => 3,
        <<"roots_ok">> => 2,
        <<"roots_missing">> => 0,
        <<"roots_failed">> => 0,
        <<"pass">> => 2,
        <<"fail">> => 0,
        <<"skip">> => 0,
        <<"cancel">> => 0
    },
    ?assertEqual(0, imboy_quality_backend:result_exit(Good)),
    lists:foreach(
        fun(R) -> ?assertEqual(2, imboy_quality_backend:result_exit(R)) end,
        [
            Good#{<<"roots_verified">> := 2},
            Good#{<<"skip">> := 1},
            Good#{<<"cancel">> := 1},
            Good#{<<"cleanup">> := #{<<"ok">> => false}},
            Good#{<<"input_binding">> := <<"NOT_VERIFIED">>}
        ]
    ).

post_cleanup_verification_precedes_terminal_receipt_test() ->
    Dir =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/terminal-" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Path = Dir ++ "/result.json",
    try
        put(order, []),
        Release = fun() ->
            put(order, [resources]),
            #{<<"ok">> => true}
        end,
        Config = fun() ->
            put(order, get(order) ++ [config]),
            ok
        end,
        Verify = fun(R) ->
            ?assertEqual([resources, config], get(order)),
            ?assertEqual(false, filelib:is_regular(Path)),
            R#{<<"input_binding">> => <<"NOT_VERIFIED">>}
        end,
        {R, Dir} = imboy_quality_full:finalize_run(
            Dir, #{<<"alias_cleanup">> => #{<<"ok">> => true}}, Release, Config, Verify
        ),
        ?assertMatch(#{<<"input_binding">> := <<"NOT_VERIFIED">>}, R),
        ?assert(filelib:is_regular(Path))
    after
        erase(order),
        file:delete(Path),
        file:del_dir(Dir)
    end.

corrupt_legacy_context_never_becomes_backend_test() ->
    Dir =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/corrupt-" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    Path = Dir ++ "/STATE.json",
    Sha = Path ++ ".sha256",
    try
        ok = file:write_file(Path, <<"corrupt old task state">>),
        ok = file:write_file(Sha, <<"invalid">>),
        ?assertError(_, imboy_quality_backend:read_state(Dir)),
        ?assertEqual(legacy, imboy_quality_backend:selection(false, ["eunit", "--full"])),
        {ok, _} = imboy_quality_state:write(Path, #{<<"task_id">> => <<"IMBOY_ERLANG_QUALITY">>}),
        {ok, B} = file:read_file(Path),
        ok = file:write_file(Sha, hash(B)),
        ?assertError(_, imboy_quality_backend:read_state(Dir))
    after
        file:delete(Path),
        file:delete(Sha),
        file:del_dir(Dir)
    end.

launch_cancel_never_claims_unverified_exit_test() ->
    Test = self(),
    Actor = spawn(fun() ->
        P = open_port({spawn_executable, "/bin/cat"}, [binary, exit_status]),
        {os_pid, Pid} = erlang:port_info(P, os_pid),
        {ok, F} = file:open("/dev/null", [write, binary]),
        Ref = make_ref(),
        put(pg_owner_start_ref, Ref),
        put(pg_owner_handle, #{}),
        Test ! {launch, self(), P, Pid, Ref},
        imboy_quality_pg_owner:owner_loop(P, F, #{}, Pid, <<"nonce">>, ok, none)
    end),
    receive
        {launch, Actor, Port, Pid, Ref} ->
            try
                ?assertMatch(
                    {exit_unverified, _},
                    imboy_quality_pg_owner:cancel_start(Actor, Ref, <<"nonce">>)
                ),
                ?assertEqual({os_pid, Pid}, erlang:port_info(Port, os_pid)),
                Other = make_ref(),
                Actor ! {pg_cancel_start_from, self(), Other, Ref, <<"wrong-nonce">>},
                Check = make_ref(),
                Actor ! {pg_check, self(), Check, <<"nonce">>},
                receive
                    {Check, ok} -> ok
                after 1000 -> error(unresponsive_owner)
                end,
                receive
                    {Other, _} -> error(forged_nonce_accepted)
                after 0 -> ok
                end
            after
                exit(Actor, kill)
            end
    after 1000 ->
        exit(Actor, kill),
        error(launch_timeout)
    end.

candidate_drift_prevents_binding_test_() ->
    {timeout, 60, fun() ->
        with_provision_fixture(fun(S, _Paths, Root) ->
            {ok, Repo} = file:get_cwd(),
            State = S#{
                <<"worktree">> => list_to_binary(Repo),
                <<"frozen_inputs">> => #{<<"head">> => <<"different-candidate">>}
            },
            {error, R} = imboy_quality_backend:verify_result(State, Root, #{}),
            ?assertEqual(<<"NOT_VERIFIED">>, maps:get(<<"input_binding">>, R)),
            Error = maps:get(<<"binding_error">>, R),
            ?assertNotEqual(nomatch, binary:match(Error, <<"badmatch">>)),
            ?assertEqual(nomatch, binary:match(Error, <<"provision_drift">>))
        end)
    end}.

duplicate_handler_registration_is_not_exclusive_test() ->
    {ok, Manager} = gen_event:start_link(),
    try
        ?assertEqual(ok, gen_event:add_handler(Manager, imboy_quality_signal, fun() -> ok end)),
        ?assertEqual(ok, gen_event:add_handler(Manager, imboy_quality_signal, fun() -> ok end)),
        ?assertEqual(
            [imboy_quality_signal, imboy_quality_signal], gen_event:which_handlers(Manager)
        )
    after
        gen_event:stop(Manager)
    end.

signal_stage_handoff_test_() ->
    {timeout, 30, fun() -> lists:foreach(fun signal_stage/1, [prepare, run]) end}.

signal_stage(Stage) ->
    Dir =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/signal-" ++ atom_to_list(Stage) ++ "-" ++
            integer_to_list(erlang:unique_integer([positive])),
    ok = file:make_dir(Dir),
    ok = file:make_dir(Dir ++ "/evidence"),
    %% 子 VM 代码路径必须同时含测试 beam 目录与 quality 工具 beam 目录
    %% （Makefile IMBOY_QUALITY_TEST_EBIN）：缺后者时 imboy_quality_backend
    %% 不可见，signal_fixture 于 boot eval 阶段 undef → "Runtime terminating
    %% during boot" 退出码 1（≠ 信号处理器 halt(143) 契约）。
    Build = filename:dirname(code:which(?MODULE)),
    ToolEbin = filename:dirname(code:which(imboy_quality_backend)),
    PaArgs = lists:append([["-pa", P] || P <- lists:usort([Build, ToolEbin])]),
    Eval = lists:flatten(
        io_lib:format("imboy_quality_backend_tests:signal_fixture(~p,~p).", [Dir, Stage])
    ),
    P = open_port({spawn_executable, os:find_executable("erl")}, [
        binary,
        exit_status,
        stderr_to_stdout,
        {args, ["+S", "2:2", "-noshell", "-noinput", "-no_dot_erlang" | PaArgs ++ ["-eval", Eval]]}
    ]),
    try
        {Code, Output} = collect_signal_vm(P, []),
        ?assertEqual({143, Stage, Output}, {Code, Stage, Output}),
        {ok, [Proof]} = file:consult(Dir ++ "/cleanup-thread.term"),
        ?assertMatch(
            #{handle_passed := true, signal_process := true, controller_suspended := true}, Proof
        ),
        {ok, [Receipt]} = file:consult(Dir ++ "/evidence/backend-signal-cleanup.term"),
        ?assertMatch(#{exit := 143, phase := Stage}, Receipt),
        ?assertEqual(false, filelib:is_regular(Dir ++ "/result.json")),
        {ok, [1]} = file:consult(Dir ++ "/handler-count.term")
    after
        file:delete(Dir ++ "/cleanup-thread.term"),
        file:delete(Dir ++ "/handler-count.term"),
        file:delete(Dir ++ "/evidence/backend-signal-cleanup.term"),
        file:del_dir(Dir ++ "/evidence"),
        file:del_dir(Dir)
    end.

collect_signal_vm(P, Acc) ->
    receive
        {P, {data, B}} -> collect_signal_vm(P, [B | Acc]);
        {P, {exit_status, C}} -> {C, iolist_to_binary(lists:reverse(Acc))}
    after 10000 ->
        port_close(P),
        error(signal_fixture_timeout)
    end.

signal_fixture(Dir, Stage) ->
    Controller = self(),
    Handle = #{synthetic => original_pg_handle},
    ok = imboy_quality_backend:begin_resources(Dir),
    imboy_quality_backend:track_pg(Handle),
    Proof = fun() ->
        {status, Status} = process_info(Controller, status),
        Data = #{
            handle_passed => get(imboy_pg_owner) =:= Handle,
            signal_process => self() =/= Controller,
            controller_suspended => Status =:= suspended
        },
        ok = file:write_file(Dir ++ "/cleanup-thread.term", io_lib:format("~p.~n", [Data]), [
            exclusive
        ])
    end,
    Tx = #{
        cleanup => fun(_, _) -> Proof() end,
        tracked => [{synthetic, <<"fixture">>, owned}],
        pending => #{}
    },
    imboy_quality_backend:track_prepare(Tx),
    case Stage of
        prepare ->
            ok;
        run ->
            ok = imboy_quality_backend:enter_run(fun() ->
                Proof(),
                #{<<"ok">> => false}
            end)
    end,
    Count = length([
        H
     || H <- gen_event:which_handlers(erl_signal_server), H =:= imboy_quality_signal
    ]),
    ok = file:write_file(Dir ++ "/handler-count.term", io_lib:format("~p.~n", [Count]), [exclusive]),
    ok = gen_event:notify(erl_signal_server, sigterm),
    receive
    after 12000 -> halt(99)
    end.

provision_drift_refuses_start_and_final_binding_test() ->
    with_provision_fixture(fun(S, Paths, Root) ->
        ?assertEqual(ok, imboy_quality_backend:verify_provision(S)),
        lists:foreach(
            fun(P) ->
                ok = file:write_file(P, <<"rewritten after freeze">>),
                ?assertMatch(
                    {error, {provision_drift, _, _}}, imboy_quality_backend:verify_provision(S)
                ),
                Start = imboy_quality_backend:authorize(S, start_pg),
                ?assertNotEqual(
                    nomatch,
                    binary:match(
                        iolist_to_binary(io_lib:format("~p", [Start])), <<"provision_drift">>
                    )
                ),
                {error, R} = imboy_quality_backend:verify_result(S, Root, #{
                    <<"scope">> => <<"BACKEND_ACCEPTANCE">>
                }),
                ?assertEqual(<<"NOT_VERIFIED">>, maps:get(<<"input_binding">>, R)),
                ?assertNotEqual(
                    nomatch, binary:match(maps:get(<<"binding_error">>, R), <<"provision_drift">>)
                ),
                ok = file:write_file(P, <<"frozen synthetic bytes">>)
            end,
            Paths
        ),
        ?assertEqual(ok, imboy_quality_backend:verify_provision(S))
    end).

with_provision_fixture(Fun) ->
    Root =
        "/private/tmp/imboy-otp-opt-backend-entry-20261111/provision-" ++
            integer_to_list(erlang:unique_integer([positive])),
    Paths = [
        Root ++ "/selected.config",
        Root ++ "/postgres-toolchain/bin/postgres",
        Root ++ "/postgres-toolchain/bin/pg_ctl",
        Root ++ "/pgdata/PG_VERSION"
    ],
    lists:foreach(
        fun(P) ->
            ok = filelib:ensure_dir(P),
            ok = file:write_file(P, <<"frozen synthetic bytes">>, [exclusive])
        end,
        Paths
    ),
    [Config | _] = Paths,
    Digest = imboy_quality_backend:provision_digest(Config, Root, <<"123">>, 2),
    S = #{
        <<"schema">> => <<"IMBOY_BACKEND_ACCEPTANCE_V1">>,
        <<"config">> => list_to_binary(Config),
        <<"workers">> => 2,
        <<"provision">> => Digest,
        <<"owned_resources">> => [
            #{<<"kind">> => <<"OWNED_DIRECTORY">>, <<"path">> => list_to_binary(Root)},
            #{<<"kind">> => <<"ISOLATED_POSTGRESQL">>, <<"system_identifier">> => <<"123">>}
        ]
    },
    try
        Fun(S, Paths, Root)
    after
        lists:foreach(fun file:delete/1, Paths),
        file:del_dir(Root ++ "/postgres-toolchain/bin"),
        file:del_dir(Root ++ "/postgres-toolchain"),
        file:del_dir(Root ++ "/pgdata"),
        file:del_dir(Root)
    end.

identity(P) ->
    {ok, #file_info{inode = I, major_device = A, minor_device = B}} = file:read_link_info(P),
    #{<<"major_device">> => A, <<"minor_device">> => B, <<"inode">> => I}.
hash(B) -> string:lowercase(binary:encode_hex(crypto:hash(sha256, B))).

context() ->
    maps:from_list([
        {K, atom_to_binary(K)}
     || K <- [candidate, build, provision, otp, erts, root_contract]
    ]).
expected(C) ->
    #{
        schema => backend_expected_v1,
        roots => [a, b, z],
        zero_leaf_roots => [z],
        leaf_manifest =>
            #{
                schema => 1,
                kind => expected,
                context => C,
                roots => [a, b],
                leaves => [leaf(a), leaf(b)]
            }
    }.
leaf(M) ->
    #{
        id => imboy_quality_leaf_identity:global_id(M, [1]),
        root => M,
        path => [1],
        metadata => meta(M)
    }.
meta(M) -> #{source => {M, check, 0}, line => 1, desc => undefined}.
per_root() -> #{a => root(a, 1), b => root(b, 1), z => zero()}.
root(M, N) ->
    Counts = #{pass => N, fail => 0, skip => 0, cancel => 0},
    Events =
        lists:append([
            [{test_begin, [I]}, {leaf_metadata, [I], meta(M)}, {test_end, [I], ok, 0}]
         || I <- lists:seq(1, N)
        ]) ++
            [{group_end, [], 0}, {result, {ok, maps:to_list(Counts)}}],
    #{exit => ok, counts => Counts, events => Events}.
zero() ->
    #{
        exit => ok,
        counts => #{pass => 0, fail => 0, skip => 0, cancel => 0},
        events =>
            [{group_end, [], 0}, {result, {ok, [{pass, 0}, {fail, 0}, {skip, 0}, {cancel, 0}]}}]
    }.
