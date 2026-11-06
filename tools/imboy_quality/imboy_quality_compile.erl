-module(imboy_quality_compile).
-export([sources/0, compile_all/1, erlc/2, needs_compile/2, git_common_dir/0]).

git_common_dir() ->
    case os:find_executable("git") of
        false ->
            error;
        Exe ->
            Port = open_port({spawn_executable, Exe}, [
                binary,
                exit_status,
                stderr_to_stdout,
                {args, ["rev-parse", "--git-common-dir"]}
            ]),
            git_common_dir_result(Port, [])
    end.

git_common_dir_result(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            git_common_dir_result(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, string:trim(binary_to_list(iolist_to_binary(lists:reverse(Acc))))};
        {Port, {exit_status, _}} ->
            error
    after 5000 ->
        port_close(Port),
        error
    end.

sources() ->
    [
        {
            filename:rootname(filename:basename(P)),
            filename:join(["tools/imboy_quality", filename:basename(P)])
        }
     || P <- filelib:wildcard("tools/imboy_quality/*.erl")
    ] ++
        [
            {
                filename:rootname(filename:basename(P)),
                filename:join(["test/quality", filename:basename(P)])
            }
         || P <- filelib:wildcard("test/quality/*.erl")
        ].

compile_all(BuildDir) ->
    %% 编译经原生 erlc（open_port + 独立 argv，FRAMEWORK_DESIGN §3 允许边界）；
    %% 本机 OTP29 的 escript 内 compile:file 不落盘，erlc 为确定路径。
    lists:foreach(
        fun({_Mod, Src}) ->
            case needs_compile(Src, any_beam(BuildDir, _Mod)) of
                true ->
                    case erlc(Src, BuildDir) of
                        0 -> ok;
                        Code -> erlang:error({compile_failed, Src, Code})
                    end;
                false ->
                    ok
            end
        end,
        sources()
    ).

any_beam(BuildDir, Mod) ->
    filename:join(BuildDir, Mod ++ ".beam").

erlc(Src, OutDir) ->
    Erlc = os:find_executable("erlc"),
    Port = open_port(
        {spawn_executable, Erlc},
        [
            exit_status,
            use_stdio,
            stderr_to_stdout,
            %% Framework selftests call TEST-only helpers. This compiler builds
            %% quality tooling/fixtures, never the production application BEAMs.
            {args, ["-DTEST=1", "-pa", filename:absname("deps/proper/ebin"), "-o", OutDir, Src]}
        ]
    ),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, _D}} -> collect_port(Port, Acc);
        {Port, {exit_status, Code}} -> Code
    after 60000 ->
        port_close(Port),
        erlang:error(erlc_timeout)
    end.

needs_compile(Src, _Out) ->
    %% Framework/PBT inputs are rebuilt: restored mtimes cannot prove BEAM freshness.
    case filelib:is_regular(Src) of
        true -> true;
        false -> erlang:error({missing_source, Src})
    end.
