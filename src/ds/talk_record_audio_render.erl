-module(talk_record_audio_render).
-moduledoc "有界本地音频合成；只经附件driver取已核验源，不调用shell。".
-export([with_audio/2]).
-include_lib("kernel/include/file.hrl").
-define(MAX_BYTES, 134217728).
-define(PCM_BYTES_PER_MS, 32).

with_audio(Parts, Fun) ->
    case private_dir() of
        {ok, Dir} ->
            try
                render(Dir, Parts, Fun)
            after
                cleanup(Dir)
            end;
        Error ->
            Error
    end.

private_dir() ->
    Base = config_ds:env(imboy, talk_record_render_tmp_dir, "/tmp"),
    %% Existing spool verifies the parent chain before any payload is created.
    case attachment_upload_spool:new(Base) of
        {ok, W} ->
            ok = attachment_upload_spool:close(W),
            Dir = filename:join(
                Base,
                "talk-render-" ++
                    binary_to_list(
                        binary:encode_hex(crypto:strong_rand_bytes(16), lowercase)
                    )
            ),
            case file:make_dir(Dir) of
                ok ->
                    case file:change_mode(Dir, 8#700) of
                        ok ->
                            {ok, Dir};
                        Error ->
                            file:del_dir(Dir),
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

render(Dir, Parts, Fun) ->
    Total = lists:sum([maps:get(<<"byte_size">>, P) || P <- Parts]),
    Duration = lists:sum([maps:get(<<"end_ms">>, P) - maps:get(<<"start_ms">>, P) || P <- Parts]),
    Cap = talk_record_ds:capabilities(),
    case
        Total > 0 andalso Total =< maps:get(<<"max_recording_bytes">>, Cap, 0) andalso
            length(Parts) =< 2000 andalso Duration > 0 andalso
            Duration =< maps:get(<<"max_duration_ms">>, Cap, 0)
    of
        false ->
            {error, audio_size_limit};
        true ->
            case write_parts(Dir, Parts, 0, []) of
                {ok, Paths} -> render_paths(Dir, Paths, Duration, Fun);
                Error -> Error
            end
    end.

write_parts(_Dir, [], _N, Acc) ->
    {ok, lists:reverse(Acc)};
write_parts(Dir, [P | Rest], N, Acc) ->
    Result = attachment_derived_artifact:with_live_spool(
        maps:get(<<"attachment_id">>, P), part_limit(), fun(Source) ->
            Size = maps:get(<<"byte_size">>, P),
            Hash = maps:get(<<"sha256">>, P),
            case
                attachment_upload_spool:size(Source) =:= Size andalso
                    attachment_upload_spool:sha256(Source) =:= Hash
            of
                true ->
                    Name = "part-" ++ integer_to_list(N) ++ ".aac",
                    case file:open(filename:join(Dir, Name), [write, binary, exclusive]) of
                        {ok, F} ->
                            try
                                case attachment_upload_spool:copy_to(Source, F) of
                                    ok -> normalize_part(Dir, Name, P);
                                    E -> E
                                end
                            after
                                file:close(F)
                            end;
                        E ->
                            E
                    end;
                false ->
                    {error, audio_part_integrity_mismatch}
            end
        end
    ),
    case Result of
        {ok, Name} -> write_parts(Dir, Rest, N + 1, [Name | Acc]);
        Error -> Error
    end.

part_limit() -> maps:get(<<"max_part_bytes">>, talk_record_ds:capabilities(), 0).

normalize_part(Dir, Name, P) ->
    Input = filename:join(Dir, Name),
    PcmName = Name ++ ".pcm",
    Out = filename:join(Dir, PcmName),
    Expected = maps:get(<<"end_ms">>, P) - maps:get(<<"start_ms">>, P),
    MaxPcm = integer_to_list((Expected + 1000) * ?PCM_BYTES_PER_MS),
    Args = [
        "-nostdin",
        "-v",
        "error",
        "-xerror",
        "-err_detect",
        "explode",
        "-f",
        "aac",
        "-i",
        Input,
        "-map",
        "0:a:0",
        "-vn",
        "-ac",
        "1",
        "-ar",
        "16000",
        "-progress",
        "pipe:1",
        "-nostats",
        "-fs",
        MaxPcm,
        "-f",
        "s16le",
        "-n",
        Out
    ],
    case run_ffmpeg(Args) of
        {ok, _} ->
            case fit_pcm(Out, Expected) of
                ok ->
                    ok = file:delete(Input),
                    {ok, PcmName};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% AAC's 1024-sample frame at 16kHz is 64ms. Allow at most 2.5 frames,
%% independently per segment; then fit PCM to the declared effective timeline.
%% Final encoding adds one fixed padding allowance, never N*padding drift.
fit_pcm(Path, ExpectedMs) ->
    {ok, #file_info{size = Actual}} = file:read_file_info(Path),
    Desired = ExpectedMs * ?PCM_BYTES_PER_MS,
    case abs(Actual - Desired) =< 160 * ?PCM_BYTES_PER_MS of
        false ->
            {error, audio_duration_mismatch};
        true ->
            case file:open(Path, [read, write, binary]) of
                {ok, F} ->
                    try
                        case Actual < Desired of
                            true ->
                                {ok, _} = file:position(F, eof),
                                file:write(F, binary:copy(<<0>>, Desired - Actual));
                            false ->
                                {ok, _} = file:position(F, {bof, Desired}),
                                file:truncate(F)
                        end
                    after
                        file:close(F)
                    end;
                Error ->
                    Error
            end
    end.

render_paths(Dir, Paths, Duration, Fun) ->
    Input = filename:join(Dir, "input.pcm"),
    Out = filename:join(Dir, "audio.aac"),
    case concat_parts(Dir, Paths, Input) of
        ok ->
            %% Explicit AAC demuxing prevents untrusted bytes selecting a
            %% playlist/network/file protocol or reading arbitrary local files.
            Args = [
                "-nostdin",
                "-v",
                "error",
                "-xerror",
                "-f",
                "s16le",
                "-ar",
                "16000",
                "-ac",
                "1",
                "-i",
                Input,
                "-map",
                "0:a:0",
                "-vn",
                "-ac",
                "1",
                "-ar",
                "16000",
                "-c:a",
                "aac",
                "-b:a",
                "64000",
                "-progress",
                "pipe:1",
                "-nostats",
                "-fs",
                integer_to_list(?MAX_BYTES),
                "-f",
                "adts",
                "-n",
                Out
            ],
            case run_ffmpeg(Args) of
                {ok, ActualMs} ->
                    case abs(ActualMs - Duration) =< 200 of
                        true -> with_output(Out, Fun);
                        false -> {error, audio_duration_mismatch}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

concat_parts(Dir, Paths, Input) ->
    case file:open(Input, [write, binary, exclusive]) of
        {ok, F} ->
            try
                concat_into(Dir, Paths, F)
            after
                file:close(F)
            end;
        Error ->
            Error
    end.
concat_into(_Dir, [], _F) ->
    ok;
concat_into(Dir, [P | Rest], F) ->
    case file:copy(filename:join(Dir, P), F) of
        {ok, _} -> concat_into(Dir, Rest, F);
        Error -> Error
    end.

run_ffmpeg(Args) ->
    Default =
        case os:find_executable("ffmpeg") of
            false -> "/usr/bin/ffmpeg";
            Path -> Path
        end,
    Exe = config_ds:env(imboy, talk_record_ffmpeg_executable, Default),
    try
        open_port({spawn_executable, Exe}, [
            binary,
            exit_status,
            stderr_to_stdout,
            use_stdio,
            {args, Args}
        ])
    of
        Port -> await_port(Port, erlang:monotonic_time(millisecond) + 120000, <<>>)
    catch
        error:_ -> {error, audio_renderer_unavailable}
    end.

await_port(Port, Deadline, Output) ->
    Left = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {exit_status, 0}} ->
            progress_duration(Output);
        {Port, {exit_status, _}} ->
            {error, audio_render_failed};
        {Port, {data, B}} when byte_size(Output) + byte_size(B) =< 65536 ->
            await_port(Port, Deadline, <<Output/binary, B/binary>>);
        {Port, {data, _}} ->
            terminate(Port),
            {error, audio_render_output_limit}
    after Left ->
        terminate(Port),
        {error, audio_render_timeout}
    end.

progress_duration(Output) ->
    Values = [V || <<"out_time_us=", V/binary>> <- binary:split(Output, <<"\n">>, [global])],
    try binary_to_integer(lists:last(Values)) div 1000 of
        Ms when Ms > 0 -> {ok, Ms};
        _ -> {error, audio_render_invalid_duration}
    catch
        _:_ -> {error, audio_render_invalid_duration}
    end.

terminate(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, Pid} ->
            Kill = open_port({spawn_executable, "/bin/kill"}, [
                exit_status,
                {args, ["-KILL", integer_to_list(Pid)]}
            ]),
            receive
                {Kill, {exit_status, _}} -> ok
            after 2000 -> safe_close(Kill)
            end;
        _ ->
            ok
    end,
    safe_close(Port),
    ok.

safe_close(Port) ->
    try
        port_close(Port)
    catch
        error:badarg -> ok
    end.

with_output(Path, Fun) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = regular, size = N}} when N > 0, N < ?MAX_BYTES ->
            attachment_upload_spool:with_source({file, Path}, Fun);
        _ ->
            {error, audio_render_invalid_output}
    end.

cleanup(Dir) ->
    case file:list_dir(Dir) of
        {ok, Names} ->
            [file:delete(filename:join(Dir, N)) || N <- Names],
            file:del_dir(Dir);
        _ ->
            ok
    end.
