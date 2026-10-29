#!/usr/bin/env escript
%% EUNIT_120S §2：外部墙钟计时父进程（T_command 口径）。
%% 用法: escript prep/timer.escript -- <executable> [args...]
%% 子进程输出原样透传；结束时落盘 prep/timer-<unix_ms>.term（Erlang term，
%% 可 file:consult；字段 cmd / t_command_ms / exit / started_at_utc_ms /
%% ended_at_utc_ms，UTC 整型毫秒）并以子进程原始退出码退出。单调钟计时，含子进程完成与结果落盘（本脚本自身）。
main(["--" | [Exe | Args]]) ->
    T0 = erlang:monotonic_time(millisecond),
    Wall0 = erlang:system_time(millisecond),
    Port = open_port(
        {spawn_executable, Exe},
        [{args, Args}, exit_status, binary, use_stdio, stderr_to_stdout, in, eof]
    ),
    Code = drain(Port),
    T1 = erlang:monotonic_time(millisecond),
    Wall1 = erlang:system_time(millisecond),
    Report = #{
        <<"cmd">> => [Exe | Args],
        <<"t_command_ms">> => T1 - T0,
        <<"exit">> => Code,
        <<"started_at_utc_ms">> => Wall0,
        <<"ended_at_utc_ms">> => Wall1
    },
    File = "prep/timer-" ++ integer_to_list(Wall0) ++ ".term",
    ok = filelib:ensure_dir(File),
    ok = file:write_file(File, io_lib:format("~p.~n", [Report])),
    io:format("~n=== T_command: ~p ms | exit: ~p | report: ~s ===~n", [
        T1 - T0, Code, File
    ]),
    halt(Code);
main(Other) ->
    io:format("usage: escript prep/timer.escript -- <exe> [args...]~ngot: ~p~n", [Other]),
    halt(2).

drain(Port) ->
    receive
        {Port, {data, D}} -> io:put_chars(D), drain(Port);
        {Port, eof} -> drain(Port);
        {Port, {exit_status, Code}} -> Code
    end.
