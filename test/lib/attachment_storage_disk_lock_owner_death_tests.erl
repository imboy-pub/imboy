-module(attachment_storage_disk_lock_owner_death_tests).
-include_lib("eunit/include/eunit.hrl").

%% The physical lock must remain held until the last filesystem mutation.
%% The lock lives on the writer's own NIF-held descriptor, so the owner-death
%% equivalent is killing the writer process while its DB recheck is paused:
%% the descriptor must eventually be released and a subsequent delete must
%% succeed and never be undone by the dead writer.
lock_owner_death_test_() -> {timeout, 30, fun lock_owner_death/0}.

lock_owner_death() ->
    Root = filename:join(
        temp_dir(),
        "attv5-lock-owner-death-" ++
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
            _ = attachment_storage_disk:put_snapshot(
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
            Parent ! {Ref, result, done}
        end),
        try
            receive
                {Ref, ready, Writer} -> ok
            after 5000 -> error(recheck_timeout)
            end,
            %% Kill the paused writer outright: its lock descriptor must go
            %% with it, leaving the object deletable and never resurrected.
            exit(Writer, kill),
            receive
                {'DOWN', Monitor, process, Writer, killed} -> ok
            after 5000 -> error(writer_not_stopped)
            end,
            %% The dead writer's descriptor is released asynchronously, so
            %% deletion success is required to be eventual, never skipped.
            eventually_deleted(Identity, 100),
            ?assertEqual(
                {error, not_found},
                attachment_storage_disk:read_snapshot(Identity)
            )
        after
            exit(Writer, kill),
            erlang:demonitor(Monitor, [flush])
        end
    after
        ok = file:del_dir_r(Root)
    end.

eventually_deleted(_Identity, 0) ->
    error(delete_never_succeeded);
eventually_deleted(Identity, N) when N > 0 ->
    try attachment_storage_disk:delete_snapshot(Identity) of
        ok -> ok;
        _ -> retry_delete(Identity, N)
    catch
        _:_ -> retry_delete(Identity, N)
    end.

retry_delete(Identity, N) ->
    timer:sleep(100),
    eventually_deleted(Identity, N - 1).

temp_dir() ->
    case file:read_link_all("/tmp") of
        {ok, Target} -> filename:absname(Target, "/");
        {error, einval} -> "/tmp"
    end.
