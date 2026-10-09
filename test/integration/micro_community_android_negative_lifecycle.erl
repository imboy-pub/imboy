-module(micro_community_android_negative_lifecycle).
-export([stop_actor/1, stop_owned/2, selftest/0]).

%% Only this run's probe, recovery worker and owned connection are stopped.
stop_actor(Pid) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    Owner = owner_socket(Pid),
    Graceful =
        try gen_server:stop(Pid, normal, 1000) of
            ok -> true
        catch
            exit:_ -> false
        end,
    LatestOwner = owner_socket(Pid),
    case is_process_alive(Pid) of
        true -> exit(Pid, kill);
        false -> ok
    end,
    down(Pid, Ref),
    lists:foreach(fun stop_socket/1, lists:usort([Owner, LatestOwner])),
    case Graceful of
        true -> ok;
        false -> {error, negative_probe_forced_stop}
    end.

stop_owned(Pid, Ctx) ->
    Stopped =
        try
            stop_actor(Pid)
        catch
            _:_ ->
                close_owned(maps:get(conn, Ctx)),
                {error, negative_probe_down_failed}
        end,
    Recovered =
        case Stopped of
            {error, negative_probe_down_failed} -> {error, negative_recovery_skipped};
            _ -> recover(Ctx)
        end,
    case {Stopped, Recovered} of
        {ok, ok} -> ok;
        {{error, Code}, _} -> {error, Code};
        _ -> {error, negative_recovery_failed}
    end.

owner_socket(Pid) ->
    case process_info(Pid, dictionary) of
        {dictionary, Dict} -> proplists:get_value(negative_owner_pid, Dict);
        undefined -> undefined
    end.

stop_socket(Pid) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    gun:close(Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 1000 ->
        exit(Pid, kill),
        down(Pid, Ref)
    end;
stop_socket(_) ->
    ok.

recover(Ctx = #{conn := Conn}) ->
    Parent = self(),
    Tag = make_ref(),
    {Worker, Ref} = spawn_monitor(fun() ->
        Result =
            try
                %% FIFO command fence: prior actor SQL finishes before this rollback.
                {ok, [], []} = epgsql:squery(Conn, <<"ROLLBACK">>),
                micro_community_android_negative_probe:cleanup_owned(Ctx),
                ok
            catch
                _:_ -> {error, negative_recovery_failed}
            end,
        Parent ! {Tag, Result}
    end),
    await_recovery(Worker, Ref, Tag, Conn).

await_recovery(Worker, Ref, Tag, Conn) ->
    receive
        {Tag, Result} ->
            down(Worker, Ref),
            case Result of
                ok ->
                    ok;
                _ ->
                    close_owned(Conn),
                    Result
            end;
        {'DOWN', Ref, process, Worker, _} ->
            close_owned(Conn),
            {error, negative_recovery_worker_failed}
    after 90000 ->
        exit(Worker, kill),
        down(Worker, Ref),
        close_owned(Conn),
        {error, negative_recovery_timeout}
    end.

close_owned(Conn) ->
    unlink(Conn),
    Ref = monitor(process, Conn),
    epgsql:close(Conn),
    receive
        {'DOWN', Ref, process, Conn, _} -> ok
    after 1000 ->
        exit(Conn, kill),
        down(Conn, Ref)
    end.

down(Pid, Ref) ->
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after 5000 -> erlang:error(negative_process_down_timeout)
    end.

%% Explicit pure fault probe, no database, app, EUnit discovery or business mock.
selftest() ->
    Pid = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    Ref = monitor(process, Pid),
    {error, negative_probe_forced_stop} = stop_actor(Pid),
    false = is_process_alive(Pid),
    down(Pid, Ref),
    io:format("MC22_STOP_SELFTEST forced_failure=1 actor_down=1 database=0~n"),
    ok.
