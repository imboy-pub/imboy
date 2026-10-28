-module(imboy_quality_guard).
-export([check/3, scan_added/2, git/2]).

check(Repo, Base0, Head0) ->
    try
        Head = commit(Repo, Head0),
        Base = base(Repo, Base0, Head),
        Changes = changed_paths(Repo, Base, Head),
        Violations = lists:append([
            check_path(Repo, Base, Head, Status, Path)
         || {Status, Path} <- Changes
        ]),
        case Violations of
            [] -> {ok, []};
            _ -> {error, {violations, Violations}}
        end
    catch
        Class:Reason -> {error, {infrastructure, Class, Reason}}
    end.

commit(Repo, Ref) ->
    Resolved = success(
        git(Repo, ["rev-parse", "--verify", "--end-of-options", Ref ++ "^{commit}"])
    ),
    Sha = string:trim(binary_to_list(Resolved)),
    {match, _} = re:run(Sha, "^[a-f0-9]{40}$"),
    Sha.

base(Repo, auto, Head) ->
    Parents = string:lexemes(
        binary_to_list(success(git(Repo, ["rev-list", "--parents", "-n", "1", Head]))), " \n"
    ),
    case Parents of
        [_] -> root;
        [_, Parent | _] -> Parent
    end;
base(Repo, Base, _Head) ->
    commit(Repo, Base).

changed_paths(Repo, Base, Head) ->
    Bytes = success(git(Repo, diff_args(Base, Head) ++ ["--name-status", "-z", "--no-renames"])),
    parse_paths(binary:split(Bytes, <<0>>, [global])).

parse_paths([<<>>]) ->
    [];
parse_paths([Status, Path | Rest]) when Status =/= <<>>, Path =/= <<>> ->
    [{Status, Path} | parse_paths(Rest)];
parse_paths(Other) ->
    error({invalid_nul_path_protocol, Other}).

diff_args(root, Head) -> ["diff-tree", "--root", "--no-commit-id", "-r", Head];
diff_args(Base, Head) -> ["diff", Base, Head].

check_path(_Repo, _Base, _Head, <<"D">>, _Path) ->
    [];
check_path(Repo, Base, Head, Status, Path) ->
    PathString = unicode:characters_to_list(Path),
    true = is_list(PathString),
    Ignore =
        case Status =:= <<"A">> andalso filename:basename(PathString) =:= ".eqwalizer" of
            true -> [#{rule => new_eqwalizer_file, path => Path}];
            false -> []
        end,
    case lists:member(filename:extension(PathString), [".erl", ".hrl", ".escript"]) of
        false ->
            Ignore;
        true ->
            Content = success(git(Repo, ["show", Head ++ ":" ++ PathString])),
            nomatch = binary:match(Content, <<0>>),
            Diff = success(
                git(
                    Repo,
                    diff_args(Base, Head) ++
                        [
                            "--text",
                            "--no-ext-diff",
                            "--no-textconv",
                            "--unified=0",
                            "-p",
                            "--",
                            PathString
                        ]
                )
            ),
            Ignore ++ scan_diff(Path, Diff)
    end.

scan_diff(Path, Diff) ->
    Lines = binary:split(Diff, <<"\n">>, [global]),
    {Violations, Block, _} = lists:foldl(
        fun(Line, {Found, Added, InHunk}) ->
            case Line of
                <<"@@", _/binary>> -> {Found ++ scan_block(Path, Added), [], true};
                <<"+", Text/binary>> when InHunk -> {Found, Added ++ [Text], true};
                <<"-", _/binary>> when InHunk -> {Found ++ scan_block(Path, Added), [], true};
                _ -> {Found ++ scan_block(Path, Added), [], false}
            end
        end,
        {[], [], false},
        Lines
    ),
    Violations ++ scan_block(Path, Block).

scan_block(_Path, []) -> [];
scan_block(Path, Added) -> scan_added(Path, iolist_to_binary(lists:join(<<"\n">>, Added))).

scan_added(Path, Added) ->
    %% 规则字面量拼接构造：避免规则定义自身在 diff 扫描中命中
    %% eqwalizer_fixme 子串规则（语义不变：运行时匹配完整 token）。
    Rules = [
        {eqwalizer_fixme, <<"eqwalizer", ":", "fixme">>},
        {dialyzer_nowarn, <<"-dialyzer\\s*\\(\\s*nowarn_">>},
        {dialyzer_tuple_nowarn, <<"-dialyzer\\s*\\(\\s*\\{\\s*nowarn_">>}
    ],
    lists:filtermap(
        fun({Rule, Pattern}) ->
            case re:compile(Pattern) of
                {ok, Compiled} ->
                    case re:run(Added, Compiled, [{capture, none}]) of
                        match -> {true, #{rule => Rule, path => Path}};
                        nomatch -> false;
                        Other -> error({regex_error, Other})
                    end;
                Error ->
                    error({regex_compile_error, Error})
            end
        end,
        Rules
    ).

git(Repo, Args) ->
    Port = open_port({spawn_executable, "/usr/bin/git"}, [
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        {cd, Repo},
        {args,
            ["-c", "maintenance.auto=false", "-c", "gc.auto=0", "-c", "core.hooksPath=/dev/null"] ++
                Args},
        {env, [
            {"GIT_CONFIG_GLOBAL", "/dev/null"},
            {"GIT_CONFIG_SYSTEM", "/dev/null"},
            {"GIT_TERMINAL_PROMPT", "0"},
            {"GIT_OPTIONAL_LOCKS", "0"}
        ]}
    ]),
    collect(Port, [], erlang:monotonic_time(millisecond) + 5000).

collect(Port, Parts, Deadline) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Port, {data, Data}} -> collect(Port, [Data | Parts], Deadline);
        {Port, {exit_status, Code}} -> {Code, iolist_to_binary(lists:reverse(Parts))}
    after Remaining ->
        port_close(Port),
        error(git_deadline)
    end.

success({0, Bytes}) -> Bytes;
success({Code, Output}) -> error({git_failed, Code, Output}).
