-module(imboy_quality_pg_owner_tests).
-include_lib("eunit/include/eunit.hrl").

real_port_identity_requires_original_owner_executable_and_pid_test() ->
    Port = open_port({spawn_executable, "/bin/cat"}, [binary, exit_status]),
    {os_pid, Pid} = erlang:port_info(Port, os_pid),
    Other = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    try
        ?assertEqual(ok, imboy_quality_pg_owner:validate_port(Port, self(), "/bin/cat", Pid)),
        ?assertMatch(
            {error, _}, imboy_quality_pg_owner:validate_port(Port, Other, "/bin/cat", Pid)
        ),
        ?assertMatch(
            {error, _}, imboy_quality_pg_owner:validate_port(Port, self(), "/bin/other", Pid)
        ),
        ?assertMatch(
            {error, _}, imboy_quality_pg_owner:validate_port(Port, self(), "/bin/cat", Pid + 1)
        )
    after
        Other ! stop,
        port_close(Port)
    end,
    ?assertMatch({error, _}, imboy_quality_pg_owner:validate_port(Port, self(), "/bin/cat", Pid)).

matching_token_without_live_owner_is_rejected_test() ->
    {ok, Repo} = file:get_cwd(),
    Old = os:getenv("IMBOY_QUALITY_OWNER_TOKEN"),
    Token = binary:copy(<<"a">>, 64),
    State = #{
        <<"worktree">> => list_to_binary(Repo),
        <<"owner">> => #{<<"token">> => Token},
        <<"execution_mode">> => <<"INTEGRATED_EXECUTION">>,
        <<"execution_goal">> => <<"ALL_PHASES">>
    },
    try
        true = os:putenv("IMBOY_QUALITY_OWNER_TOKEN", binary_to_list(Token)),
        ?assertEqual(
            false, maps:get(owner_verified, imboy_quality_pg_owner:execution_context(State))
        )
    after
        case Old of
            false -> os:unsetenv("IMBOY_QUALITY_OWNER_TOKEN");
            _ -> os:putenv("IMBOY_QUALITY_OWNER_TOKEN", Old)
        end
    end.

pidfile_requires_matching_pid_directory_loopback_port_and_readiness_test() ->
    Dir = "/tmp/quality-pgdata",
    Pid = 1234,
    Port = 25496,
    Ready = pidfile("1234", Dir, "25496", "127.0.0.1", "ready"),
    ?assertEqual(ok, imboy_quality_pg_owner:validate_pidfile(Ready, Pid, Dir, Port, ready)),
    ?assertMatch(
        {error, _}, imboy_quality_pg_owner:validate_pidfile(Ready, Pid + 1, Dir, Port, ready)
    ),
    ?assertMatch(
        {error, _}, imboy_quality_pg_owner:validate_pidfile(Ready, Pid, "/other", Port, ready)
    ),
    ?assertMatch(
        {error, _}, imboy_quality_pg_owner:validate_pidfile(Ready, Pid, Dir, Port + 1, ready)
    ),
    ?assertMatch(
        {error, _},
        imboy_quality_pg_owner:validate_pidfile(
            pidfile("1234", Dir, "25496", "0.0.0.0", "ready"), Pid, Dir, Port, ready
        )
    ),
    Starting = pidfile("1234", Dir, "25496", "127.0.0.1", "starting"),
    ?assertMatch(
        {error, _}, imboy_quality_pg_owner:validate_pidfile(Starting, Pid, Dir, Port, ready)
    ),
    ?assertEqual(ok, imboy_quality_pg_owner:validate_pidfile(Starting, Pid, Dir, Port, stopping)),
    ?assertMatch(
        {error, _},
        imboy_quality_pg_owner:validate_pidfile(<<"truncated">>, Pid, Dir, Port, stopping)
    ).

pidfile(Pid, Dir, Port, Addr, Status) ->
    iolist_to_binary([Pid, "\n", Dir, "\n0\n", Port, "\n/tmp\n", Addr, "\n0\n", Status, "\n"]).

existing_postmaster_pidfile_is_preserved_test() ->
    Dir = filename:join("/tmp", "imboy-pg-pidfile-test-" ++ os:getpid()),
    ok = file:make_dir(Dir),
    Path = filename:join(Dir, "postmaster.pid"),
    try
        ?assertEqual(ok, imboy_quality_pg_owner:validate_empty_pidfile(Dir)),
        ok = file:write_file(Path, <<"foreign">>, [exclusive]),
        ?assertEqual(
            {error, existing_postmaster_pidfile}, imboy_quality_pg_owner:validate_empty_pidfile(Dir)
        ),
        ?assertEqual({ok, <<"foreign">>}, file:read_file(Path))
    after
        file:delete(Path),
        file:del_dir(Dir)
    end.

postgres_cleanup_requires_confirmed_exit_test() ->
    Clean = #{<<"ok">> => true},
    Good = {ok, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => true}},
    ?assertEqual(
        true, maps:get(<<"ok">>, imboy_quality_env_cleanup:finish_postgres_cleanup(Clean, Good))
    ),
    [
        ?assertEqual(
            false, maps:get(<<"ok">>, imboy_quality_env_cleanup:finish_postgres_cleanup(Clean, P))
        )
     || P <- [
            {error, postgres_exit_unverified},
            {ok, #{postgres_exit => 0, pg_ctl_exit => 0, log_ok => false}},
            {ok, #{postgres_exit => 137, pg_ctl_exit => 0, log_ok => true}},
            {ok, #{postgres_exit => 143, pg_ctl_exit => 0, log_ok => true}},
            {ok, #{postgres_exit => 0, pg_ctl_exit => 1}}
        ]
    ],
    ?assertEqual(
        false,
        maps:get(
            <<"ok">>, imboy_quality_env_cleanup:finish_postgres_cleanup(#{<<"ok">> => false}, Good)
        )
    ).

receipt_binding_and_single_use_test() ->
    Parent = self(),
    Actor = spawn(fun() ->
        Port = open_port({spawn_executable, "/bin/cat"}, [binary, exit_status]),
        {os_pid, Pid} = erlang:port_info(Port, os_pid),
        {ok, F} = file:open("/dev/null", [write, binary]),
        Token = make_ref(),
        Parent ! {ready, self(), Pid, Token},
        imboy_quality_pg_owner:owner_loop(
            Port,
            F,
            #{},
            Pid,
            <<"nonce">>,
            ok,
            {Token, register, #{binding => exact}}
        )
    end),
    receive
        {ready, Actor, Pid, Token} ->
            ?assertMatch(
                {error, _},
                imboy_quality_pg_owner:claim_receipt(
                    Actor, make_ref(), register, Pid, #{binding => exact}
                )
            ),
            ?assertMatch(
                {error, _},
                imboy_quality_pg_owner:claim_receipt(
                    Actor, Token, register, Pid, #{binding => altered}
                )
            ),
            ?assertMatch(
                {ok, _, <<"nonce">>},
                imboy_quality_pg_owner:claim_receipt(
                    Actor, Token, register, Pid, #{binding => exact}
                )
            ),
            ?assertMatch(
                {ok, _, _},
                imboy_quality_pg_owner:claim_receipt(
                    Actor, Token, register, Pid, #{binding => exact}
                )
            ),
            ?assertMatch(
                {ok, _, <<"nonce">>},
                imboy_quality_pg_owner:commit_receipt(
                    Actor, Token, register, Pid, #{binding => exact}
                )
            ),
            ?assertMatch(
                {ok, _, <<"nonce">>},
                imboy_quality_pg_owner:commit_receipt(
                    Actor, Token, register, Pid, #{binding => exact}
                )
            ),
            ?assertMatch(
                {error, _},
                imboy_quality_pg_owner:claim_receipt(
                    Actor, Token, register, Pid, #{binding => exact}
                )
            ),
            exit(Actor, kill)
    after 5000 -> error(actor_timeout)
    end.

cancel_and_parent_death_retain_unverified_port_test() ->
    Test = self(),
    Parent = spawn(fun() ->
        receive
            stop -> ok
        end
    end),
    Actor = spawn(fun() ->
        Port = open_port({spawn_executable, "/bin/cat"}, [binary, exit_status]),
        {os_pid, Pid} = erlang:port_info(Port, os_pid),
        {ok, F} = file:open("/dev/null", [write, binary]),
        StartRef = make_ref(),
        put(pg_owner_start_ref, StartRef),
        put(pg_owner_parent_monitor, erlang:monitor(process, Parent)),
        put(pg_owner_handle, #{}),
        Test ! {ready_cancel, self(), Port, Pid, StartRef},
        imboy_quality_pg_owner:owner_loop(Port, F, #{}, Pid, <<"nonce">>, ok, none)
    end),
    receive
        {ready_cancel, Actor, Port, Pid, StartRef} ->
            Actor ! {pg_cancel_start, StartRef},
            Parent ! stop,
            Check = make_ref(),
            Actor ! {pg_check, self(), Check, <<"nonce">>},
            receive
                {Check, ok} -> ok
            after 5000 -> error(actor_timeout)
            end,
            ?assert(is_process_alive(Actor)),
            ?assertEqual({os_pid, Pid}, erlang:port_info(Port, os_pid)),
            exit(Actor, kill)
    after 5000 -> error(actor_timeout)
    end.

public_receipt_identifier_hides_nonce_test() ->
    Nonce = <<"test-secret-nonce">>,
    Hash = imboy_quality_pg_owner:nonce_hash(Nonce),
    ?assertEqual(64, byte_size(Hash)),
    ?assertEqual(nomatch, binary:match(Hash, Nonce)),
    ?assertEqual(string:lowercase(binary:encode_hex(crypto:hash(sha256, Nonce))), Hash).
