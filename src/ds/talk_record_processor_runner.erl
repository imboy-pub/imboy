-module(talk_record_processor_runner).
-moduledoc "本机处理协议：stdin一行JSON、stdout一个JSON；取消需确认Port退出。".
-export([run/2]).

run(C, Input) ->
    ParentMon = get(talk_model_parent_monitor),
    receive
        {'DOWN', ParentMon, process, _, _} -> {error, processor_parent_lost};
        {talk_model_cancel, R} -> {error, R}
    after 0 -> start(C, Input)
    end.
start(C, Input) ->
    Open =
        try
            open_port({spawn_executable, maps:get(executable, C)}, [
                binary,
                exit_status,
                use_stdio,
                stderr_to_stdout,
                {args, maps:get(args, C, [])},
                {env, clean_environment()}
            ])
        catch
            error:_ -> {error, processor_start_failed}
        end,
    case Open of
        {error, _} = E ->
            E;
        P ->
            try
                register_and_send(P, C, Input)
            catch
                _:_ -> stop(P, processor_registration_failed)
            end
    end.
register_and_send(P, C, Input) ->
    {os_pid, Pid} = erlang:port_info(P, os_pid),
    Register = maps:get(on_start, C, fun(_) -> ok end),
    case Register(Pid) of
        ok ->
            true = port_command(P, [jsone:encode(Input), <<"\n">>]),
            collect(
                P,
                erlang:monotonic_time(millisecond) + maps:get(timeout_ms, C),
                maps:get(max_output_bytes, C),
                <<>>
            );
        {error, R} ->
            stop(P, R);
        _ ->
            stop(P, processor_registration_failed)
    end.
collect(P, D, Max, Acc) ->
    ParentMon = get(talk_model_parent_monitor),
    Left = max(0, D - erlang:monotonic_time(millisecond)),
    receive
        {P, {data, B}} when byte_size(Acc) + byte_size(B) =< Max ->
            collect(P, D, Max, <<Acc/binary, B/binary>>);
        {P, {data, _}} ->
            stop(P, processor_output_limit);
        {P, {exit_status, 0}} ->
            decode(Acc);
        {P, {exit_status, _}} ->
            {error, processor_failed};
        {'DOWN', ParentMon, process, _, _} ->
            stop(P, processor_parent_lost);
        {talk_model_cancel, R} ->
            stop(P, R)
    after Left -> stop(P, processor_timeout)
    end.
decode(B) ->
    try jsone:decode(B) of
        M when is_map(M) -> {ok, M};
        _ -> {error, processor_schema_invalid}
    catch
        _:_ -> {error, processor_schema_invalid}
    end.
stop(P, Reason) ->
    case erlang:port_info(P, os_pid) of
        {os_pid, Id} -> signal_kill(Id);
        _ -> ok
    end,
    await_stop(P, Reason, erlang:monotonic_time(millisecond) + 2000).
await_stop(P, Reason, D) ->
    receive
        {P, {exit_status, _}} -> {error, Reason};
        {P, {data, _}} -> await_stop(P, Reason, D)
    after max(0, D - erlang:monotonic_time(millisecond)) ->
        close(P),
        {error, processor_stop_unconfirmed}
    end.
signal_kill(Id) ->
    K = open_port({spawn_executable, "/bin/kill"}, [
        exit_status,
        binary,
        stderr_to_stdout,
        {args, ["-KILL", integer_to_list(Id)]}
    ]),
    receive
        {K, {exit_status, _}} -> ok
    after 1000 -> close(K)
    end.
close(P) ->
    try
        port_close(P)
    catch
        error:badarg -> ok
    end.

%% Do not inherit the server's JWT/DB/storage credentials.
clean_environment() ->
    Names = [hd(string:split(Pair, "=", leading)) || Pair <- os:getenv()],
    [{N, false} || N <- Names, N =/= "PATH", N =/= "LANG"] ++
        [{"PATH", "/usr/bin:/bin"}, {"LANG", "C.UTF-8"}].
