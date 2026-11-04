-module(imboy_quality_garage_ownership_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

existing_directory_is_never_removed_test() ->
    with_root(fun(Root) ->
        {Dir, _, _} = imboy_quality_resources:create_garage_dir(Root, "existing"),
        Sentinel = filename:join(Dir, "sentinel"),
        ok = file:write_file(Sentinel, <<"preserve">>),
        ?assertException(
            error,
            {garage_directory_create_failed, Dir, eexist},
            imboy_quality_resources:create_garage_dir(Root, "existing")
        ),
        ?assertEqual({ok, <<"preserve">>}, file:read_file(Sentinel))
    end).

verified_exit_and_ownership_remove_directory_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "owned"),
        {ok, #file_info{mode = Mode}} = file:read_file_info(Dir),
        ?assertEqual(0, Mode band 8#077),
        G = #{owner => reply_owner(0), dir => Dir, dir_identity => Identity, dir_token => Token},
        ?assertEqual(#{exit => 0, cleanup => ok}, imboy_quality_resources:stop_garage(G)),
        ?assertEqual({error, enoent}, file:read_link_info(Dir))
    end).

stopped_log_is_copied_before_owned_cleanup_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "log"),
        Log = filename:join(Dir, "server.log"),
        ok = file:write_file(Log, <<"start\n">>),
        Owner = spawn(fun() ->
            receive
                {stop, Caller, Ref} ->
                    ok = file:write_file(Log, <<"tail\n">>, [append]),
                    Caller ! {Ref, stopped, 0}
            end
        end),
        G = #{owner => Owner, dir => Dir, dir_identity => Identity, dir_token => Token},
        Stop = imboy_quality_resources:stop_garage(G#{keep_dir => true}),
        ?assertMatch(#{exit := 0, cleanup := retained, receipt := _}, Stop),
        ?assertEqual({ok, <<"start\ntail\n">>}, file:read_file(Log)),
        ?assertEqual(
            {error, stop_not_verified},
            imboy_quality_resources:cleanup_stopped_garage(G, Stop#{receipt := make_ref()})
        ),
        ?assert(filelib:is_dir(Dir)),
        ?assertEqual(ok, imboy_quality_resources:cleanup_stopped_garage(G, Stop)),
        ?assertEqual({error, enoent}, file:read_link_info(Dir)),
        ?assertEqual(
            {error, stop_not_verified}, imboy_quality_resources:cleanup_stopped_garage(G, Stop)
        )
    end).

existing_quality_config_is_not_overwritten_test() ->
    with_root(fun(Root) ->
        ok = file:make_dir(filename:join(Root, "evidence")),
        Base = filename:join(Root, "base.config"),
        ok = file:write_file(Base, <<"[{imboy, [{http_port, 9800}]}].">>),
        ConfigDir = filename:join(Root, "config"),
        ok = file:make_dir(ConfigDir),
        ok = imboy_quality_config_receipts:begin_run(Root, "existing-config", Root),
        Target = filename:join(ConfigDir, "sys.quality." ++ os:getpid() ++ ".existing.config"),
        ok = file:write_file(Target, <<"other session">>),
        ?assertException(
            error,
            {quality_config_write_failed, Target, eexist},
            imboy_quality_resources:write_config(Root, "base", "existing", 0, {"old", "new"})
        ),
        ?assertEqual({ok, <<"other session">>}, file:read_file(Target)),
        ?assertMatch({error, _}, imboy_quality_config_receipts:cleanup()),
        ets:delete(imboy_quality_config_receipts)
    end).

unconfirmed_exit_never_removes_directory_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "alive"),
        G = #{
            owner => reply_owner(killed), dir => Dir, dir_identity => Identity, dir_token => Token
        },
        ?assertEqual(
            #{exit => killed, cleanup => {error, process_exit_unverified}},
            imboy_quality_resources:stop_garage(G)
        ),
        ?assert(filelib:is_dir(Dir))
    end).

wrong_token_never_removes_directory_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, _} = imboy_quality_resources:create_garage_dir(Root, "wrong"),
        G = #{
            owner => reply_owner(0), dir => Dir, dir_identity => Identity, dir_token => <<"wrong">>
        },
        ?assertEqual(
            #{
                exit => {cleanup_failed, ownership_not_verified},
                cleanup => {error, ownership_not_verified}
            },
            imboy_quality_resources:stop_garage(G)
        ),
        ?assert(filelib:is_dir(Dir))
    end).

invalid_tag_is_rejected_test() ->
    with_root(fun(Root) ->
        ?assertException(
            error,
            {badmatch, false},
            imboy_quality_resources:create_garage_dir(Root, "../other")
        )
    end).

symlinked_runtime_parent_is_rejected_test() ->
    with_root(fun(Root) ->
        Target = filename:join(Root, "other"),
        Link = filename:join(Root, "runtime"),
        ok = file:make_dir(Target),
        ok = file:make_symlink(Target, Link),
        try
            ?assertException(
                error,
                {badmatch, _},
                imboy_quality_resources:create_garage_dir(Root, "unsafe")
            ),
            ?assertEqual({error, enoent}, file:read_link_info(filename:join(Target, "garage")))
        after
            file:delete(Link)
        end
    end).

symlinked_owner_marker_is_rejected_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "marker"),
        Marker = filename:join(Dir, ".imboy-quality-owner"),
        Target = filename:join(Dir, "other"),
        ok = file:write_file(Target, Token),
        ok = file:delete(Marker),
        ok = file:make_symlink(Target, Marker),
        G = #{owner => reply_owner(0), dir => Dir, dir_identity => Identity, dir_token => Token},
        ?assertEqual(
            #{
                exit => {cleanup_failed, ownership_not_verified},
                cleanup => {error, ownership_not_verified}
            },
            imboy_quality_resources:stop_garage(G)
        ),
        ?assertEqual({ok, Token}, file:read_file(Target)),
        ok = file:delete(Marker)
    end).

owned_port_identity_never_claims_cross_session_test() ->
    {Port, Exe} = synthetic_otp_port(),
    try
        {ok, #{
            pid := Pid,
            executable := {_, _, Inode},
            executable_path := Exe,
            scope := <<"LOCAL_OTP_PORT_ONLY_NOT_CROSS_SESSION_IDENTITY">>
        }} =
            imboy_quality_resources:owned_port_identity(Port, Exe, "synthetic-config"),
        ?assert(Pid > 0),
        ?assert(Inode > 0),
        ?assertEqual(
            {error, port_identity_unverified},
            imboy_quality_resources:owned_port_identity(Port, "/bin/cat", "synthetic-config")
        ),
        ?assertEqual(
            {error, process_exit_unverified}, imboy_quality_resources:close_owned_port(Port)
        )
    after
        try
            port_close(Port)
        catch
            error:badarg -> ok
        end,
        timer:sleep(1050)
    end.

another_process_port_is_never_closed_test() ->
    Parent = self(),
    Ref = make_ref(),
    {Owner, Monitor} = spawn_monitor(fun() ->
        {Port, Exe} = synthetic_otp_port(),
        Parent ! {Ref, port, Port, Exe},
        receive
            stop -> port_close(Port)
        end
    end),
    {Port, Exe} =
        receive
            {Ref, port, P, E} -> {P, E}
        after 1000 -> error(port_owner_timeout)
        end,
    ?assertEqual(
        {error, port_identity_unverified},
        imboy_quality_resources:owned_port_identity(Port, Exe, "synthetic")
    ),
    ?assertEqual({error, process_exit_unverified}, imboy_quality_resources:close_owned_port(Port)),
    ?assert(is_process_alive(Owner)),
    Owner ! stop,
    receive
        {'DOWN', Monitor, process, Owner, normal} -> ok
    after 1000 -> error(owner_stop_timeout)
    end,
    timer:sleep(1050).

startup_timeout_cancels_owner_before_late_launch_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "timeout"),
        Late = filename:join(Dir, "late-launch"),
        Parent = self(),
        Ref = make_ref(),
        {Owner, Monitor} = spawn_monitor(fun() ->
            timer:sleep(100),
            ok = file:write_file(Late, <<"would start">>),
            Parent ! {Ref, os_pid, 42, {ok, fake}}
        end),
        Startup = #{
            dir => Dir,
            dir_identity => Identity,
            dir_token => Token,
            exe => "synthetic-otp",
            config => "fake-config"
        },
        ?assertEqual(
            {error, {garage_start_timeout, [], ok, confirmed}},
            imboy_quality_resources:await_garage_start(Owner, Monitor, Ref, Startup, 5)
        ),
        ?assertNot(is_process_alive(Owner)),
        timer:sleep(120),
        ?assertEqual({error, enoent}, file:read_file(Late)),
        {ok, Record} = file:read_file(filename:join(Dir, "startup-timeout.term")),
        ?assertMatch(#{status := exit_unverified, snapshots := []}, binary_to_term(Record, [safe]))
    end).

startup_timeout_records_open_port_pid_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "openport"),
        Parent = self(),
        Ref = make_ref(),
        {Owner, Monitor} = spawn_monitor(fun() ->
            {Port, Exe} = synthetic_otp_port(),
            {os_pid, Pid} = erlang:port_info(Port, os_pid),
            Parent ! {Ref, ready, Pid, Exe},
            receive
                stop -> ok
            end
        end),
        {Pid, Exe} =
            receive
                {Ref, ready, P, E} -> {P, E}
            after 1000 -> error(synthetic_owner_not_ready)
            end,
        Startup = #{
            dir => Dir,
            dir_identity => Identity,
            dir_token => Token,
            exe => Exe,
            config => "1"
        },
        {error, {garage_start_timeout, Snapshots, ok, confirmed}} =
            imboy_quality_resources:await_garage_start(Owner, Monitor, Ref, Startup, 5),
        ?assertMatch([{Pid, _}], Snapshots),
        {ok, Record} = file:read_file(filename:join(Dir, "startup-timeout.term")),
        ?assertMatch(
            #{status := exit_unverified, snapshots := [{Pid, _}]}, binary_to_term(Record, [safe])
        ),
        timer:sleep(1050)
    end).

startup_owner_crash_is_reported_test() ->
    with_root(fun(Root) ->
        {Dir, Identity, Token} = imboy_quality_resources:create_garage_dir(Root, "crash"),
        {Owner, Monitor} = spawn_monitor(fun() -> exit(synthetic_start_failure) end),
        Startup = #{
            dir => Dir,
            dir_identity => Identity,
            dir_token => Token,
            exe => "synthetic-otp",
            config => "fake-config"
        },
        ?assertEqual(
            {error, {garage_owner_down, synthetic_start_failure, undefined}},
            imboy_quality_resources:await_garage_start(Owner, Monitor, make_ref(), Startup, 1000)
        ),
        ?assert(filelib:is_dir(Dir))
    end).

synthetic_otp_port() ->
    Root = code:root_dir(),
    BinDir = filename:join([Root, "erts-" ++ erlang:system_info(version), "bin"]),
    Exe = filename:join(BinDir, "erlexec"),
    Crash = filename:join(
        "/tmp",
        "imboy-otp-child-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive])) ++ ".dump"
    ),
    Env = [
        {"ROOTDIR", Root},
        {"BINDIR", BinDir},
        {"EMU", "beam"},
        {"PROGNAME", "erl"},
        {"ERL_CRASH_DUMP", Crash}
    ],
    Args = [
        "+S",
        "1:1",
        "+SDcpu",
        "1",
        "+SDio",
        "1",
        "-noshell",
        "-noinput",
        "-eval",
        "timer:sleep(1000),halt(0)."
    ],
    {open_port({spawn_executable, Exe}, [{args, Args}, {env, Env}, exit_status]), Exe}.

reply_owner(Code) ->
    spawn(fun() ->
        receive
            {stop, Caller, Ref} -> Caller ! {Ref, stopped, Code}
        end
    end).

with_root(Fun) ->
    Root = filename:join(
        "/tmp",
        "imboy-garage-ownership-" ++ os:getpid() ++ "-" ++
            integer_to_list(erlang:unique_integer([positive]))
    ),
    ok = file:make_dir(Root),
    try
        Fun(Root)
    after
        ok = file:del_dir_r(Root)
    end.
