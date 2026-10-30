-module(attachment_storage_disk_owner_exit_tests).
-include_lib("eunit/include/eunit.hrl").

%% A writer killed after recheck starts must never resume its filesystem
%% mutation after another owner has successfully deleted the same object.
owner_exit_test_() -> {timeout, 20, fun owner_exit/0}.
legacy_lock_interop_test_() -> {timeout, 20, fun legacy_lock_interop/0}.

legacy_lock_interop() ->
    Root = filename:join(
        temp_dir(),
        "attv5-lock-interop-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Root),
    ok = file:change_mode(Root, 8#700),
    Identity = #{
        endpoint => list_to_binary("file://" ++ Root),
        container => <<"c">>,
        key => <<"owned">>
    },
    Digest = binary_to_list(
        binary:encode_hex(
            crypto:hash(sha256, term_to_binary({<<"c">>, <<"owned">>})), lowercase
        )
    ),
    Lock = filename:join([Root, ".v5-locks", Digest]),
    Parent = self(),
    Ref = make_ref(),
    try
        {Writer, Monitor} = spawn_monitor(fun() ->
            Result = attachment_storage_disk:put_snapshot(
                Identity,
                <<"after">>,
                infinity,
                fun() ->
                    Parent ! {Ref, ready, self()},
                    receive
                        {Ref, continue} -> ok
                    end
                end
            ),
            Parent ! {Ref, result, Result}
        end),
        try
            receive
                {Ref, ready, Writer} -> ok
            after 5000 -> error(recheck_timeout)
            end,
            ?assertNotEqual(0, legacy_lock_status(Lock)),
            Writer ! {Ref, continue},
            receive
                {Ref, result, ok} -> ok
            after 5000 -> error(writer_timeout)
            end,
            receive
                {'DOWN', Monitor, process, Writer, normal} -> ok
            after 2000 -> error(writer_not_stopped)
            end,
            ?assertEqual(0, legacy_lock_status(Lock)),
            ?assertEqual({ok, <<"after">>}, attachment_storage_disk:read_snapshot(Identity))
        after
            exit(Writer, kill),
            erlang:demonitor(Monitor, [flush])
        end
    after
        ok = file:del_dir_r(Root)
    end.

legacy_lock_status(Lock) ->
    {Exe, Args} =
        case os:find_executable("lockf") of
            false ->
                Flock = os:find_executable("flock"),
                true = is_list(Flock),
                {Flock, ["-x", "-w", "1", Lock, "/usr/bin/true"]};
            Lockf ->
                {Lockf, ["-k", "-t", "1", Lock, "/usr/bin/true"]}
        end,
    Port = open_port(
        {spawn_executable, Exe},
        [exit_status, {args, Args}, stderr_to_stdout]
    ),
    receive
        {Port, {exit_status, Status}} -> Status
    after 4000 ->
        port_close(Port),
        error(legacy_lock_timeout)
    end.

owner_exit() ->
    Root = filename:join(
        temp_dir(),
        "attv5-owner-exit-" ++
            binary_to_list(binary:encode_hex(crypto:strong_rand_bytes(12), lowercase))
    ),
    ok = file:make_dir(Root),
    ok = file:change_mode(Root, 8#700),
    Identity = #{
        endpoint => list_to_binary("file://" ++ Root),
        container => <<"c">>,
        key => <<"owned">>
    },
    Parent = self(),
    Ref = make_ref(),
    try
        ok = attachment_storage_disk:put_snapshot(Identity, <<"before">>, infinity),
        {Writer, Monitor} = spawn_monitor(fun() ->
            attachment_storage_disk:put_snapshot(
                Identity,
                <<"after">>,
                infinity,
                fun() ->
                    Parent ! {Ref, ready, self()},
                    receive
                        {Ref, continue} -> ok
                    end
                end
            )
        end),
        try
            receive
                {Ref, ready, Writer} -> ok
            after 5000 -> error(recheck_timeout)
            end,
            exit(Writer, kill),
            receive
                {'DOWN', Monitor, process, Writer, killed} -> ok
            after 2000 -> error(writer_not_stopped)
            end,
            %% A new delete must be able to acquire the same physical lock.
            ?assertEqual(ok, attachment_storage_disk:delete_snapshot(Identity)),
            ?assertEqual({error, not_found}, attachment_storage_disk:read_snapshot(Identity))
        after
            exit(Writer, kill),
            erlang:demonitor(Monitor, [flush])
        end
    after
        ok = file:del_dir_r(Root)
    end.

temp_dir() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.
