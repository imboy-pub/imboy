%% IMBOY_ERLANG_QUALITY P1-03：原子状态读写与安全新路径（纯 OTP，OTP29 json）。
%% 草稿（LP-03）正式化：write 临时文件 + sync + rename；拒绝 symlink/hardlink 目标。
-module(imboy_quality_state).
-export([
    write/2,
    read/1,
    new_path/2,
    trusted_context/1,
    admission_profile/2,
    candidate_input_fingerprint/1,
    full_binding_snapshot/3,
    full_binding_verdict/2
]).
-include_lib("kernel/include/file.hrl").

write(Path, State) when is_map(State) ->
    safe_parent(filename:dirname(filename:absname(Path))),
    validate_destination(Path),
    Bytes = iolist_to_binary(json:encode(State)),
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, Handle} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Temp, 8#600),
        ok = file:write(Handle, Bytes),
        ok = file:sync(Handle)
    after
        ok = file:close(Handle)
    end,
    ok = file:rename(Temp, Path),
    {ok, crypto:hash(sha256, Bytes)}.

read(Path) ->
    validate_destination(Path),
    {ok, Bytes} = file:read_file(Path),
    State = json:decode(Bytes),
    true = is_map(State),
    {ok, State, crypto:hash(sha256, Bytes)}.

%% 从 STATE.json 提取可信执行上下文（cli:permit 的输入；缺失字段不默认放行）。
trusted_context(State) ->
    #{
        task_id => maps:get(<<"task_id">>, State, <<>>),
        execution_mode => map_atom(State, <<"execution_mode">>),
        execution_goal => map_atom(State, <<"execution_goal">>),
        owner_verified => owner_verified(State),
        heavy_admitted => heavy_admitted(State),
        gimpl_profile => admission_profile(State, live_head())
    }.

admission_profile(State, Head) ->
    try
        Profile = snapshot_profile(State, Head),
        true = Profile =/= not_admitted,
        Admission = maps:get(<<"current_admission">>, State),
        Expected = maps:get(<<"input_fingerprint">>, Admission),
        Expected = candidate_input_fingerprint(binary_to_list(maps:get(<<"worktree">>, State))),
        Profile
    catch
        _:_ -> not_admitted
    end.

%% Full admission evidence is computed on both sides of actual EUnit.
full_binding_snapshot(State, Repo, Head) when is_map(State), is_list(Repo), is_binary(Head) ->
    Fingerprint = candidate_input_fingerprint(Repo),
    Worktree = unicode:characters_to_binary(Repo),
    Binding =
        case {maps:get(<<"worktree">>, State, undefined), admission_profile(State, Head)} of
            {Worktree, phase1_snapshot} -> <<"VERIFIED">>;
            {Worktree, phase2_integrated} -> <<"VERIFIED">>;
            _ -> <<"NOT_VERIFIED">>
        end,
    #{
        <<"candidate_sha">> => Head,
        <<"input_fingerprint">> => Fingerprint,
        <<"input_binding">> => Binding
    }.

full_binding_verdict(
    #{
        <<"input_binding">> := <<"VERIFIED">>,
        <<"candidate_sha">> := Head,
        <<"input_fingerprint">> := Fingerprint
    },
    #{
        <<"input_binding">> := <<"VERIFIED">>,
        <<"candidate_sha">> := Head,
        <<"input_fingerprint">> := Fingerprint
    }
) when is_binary(Head), byte_size(Head) =:= 40, is_binary(Fingerprint), byte_size(Fingerprint) > 0 ->
    <<"VERIFIED">>;
full_binding_verdict(_, _) ->
    <<"NOT_VERIFIED">>.

candidate_input_fingerprint(Repo) ->
    Patterns = [
        "src/**/*",
        "include/**/*",
        "test/**/*",
        "tools/imboy_quality/**/*",
        "priv/migrations/**/*",
        "config/**/*.config",
        "deps/**/*.erl",
        "deps/**/*.hrl",
        "deps/**/*.app.src",
        "deps/**/*.proto",
        "deps/**/*.config",
        "deps/**/*.term",
        "deps/**/*.mk",
        "deps/**/Makefile",
        "deps/**/*.yrl",
        "deps/**/*.xrl",
        "deps/**/*.asn1"
    ],
    Config =
        case os:getenv("EUNIT_CONFIG") of
            false -> "config/sys.local";
            V -> V
        end,
    SelectedConfig =
        case filename:extension(Config) of
            ".config" -> Config;
            _ -> Config ++ ".config"
        end,
    SelectedPath = filename:absname(SelectedConfig, Repo),
    true = filelib:is_regular(SelectedPath),
    {ok, _SelectedBytes} = file:read_file(SelectedPath),
    Paths = lists:usort(
        lists:append([filelib:wildcard(filename:join(Repo, P)) || P <- Patterns]) ++
            [
                SelectedPath
                | [
                    filename:join(Repo, P)
                 || P <- [
                        "Makefile",
                        "erlang.mk",
                        "include/deps.mk",
                        "scripts/quality.escript",
                        "config/sys.local.config",
                        "config/sys.config",
                        "config/sys.config.example"
                    ]
                ]
            ]
    ),
    Files = [P || P <- Paths, filelib:is_regular(P), source_input(P) orelse P =:= SelectedPath],
    true = Files =/= [],
    Hashes = [
        begin
            {ok, Bytes} = file:read_file(P),
            {filename:absname(P), crypto:hash(sha256, Bytes)}
        end
     || P <- Files
    ],
    string:lowercase(
        binary:encode_hex(
            crypto:hash(
                sha256,
                term_to_binary(
                    {erlang:system_info(otp_release), erlang:system_info(version), Hashes}
                )
            )
        )
    ).

source_input(Path) ->
    Name = filename:basename(Path),
    filename:extension(Path) =/= ".beam" andalso
        Name =/= "sys.runtime.config" andalso
        not lists:prefix("sys.quality.", Name).

snapshot_profile(State, Head) when
    is_binary(Head), byte_size(Head) =:= 40; is_binary(Head), byte_size(Head) =:= 64
->
    case
        {
            maps:get(<<"current_gate_status">>, State, #{}),
            maps:get(<<"current_admission">>, State, #{})
        }
    of
        {#{<<"G-IMPL">> := <<"ADMITTED">>}, #{
            <<"status">> := <<"ADMITTED">>,
            <<"candidate_sha">> := Head,
            <<"profile">> := <<"PHASE1_SNAPSHOT">>
        }} ->
            phase1_snapshot;
        {#{<<"G-IMPL">> := <<"ADMITTED">>}, #{
            <<"status">> := <<"ADMITTED">>,
            <<"candidate_sha">> := Head,
            <<"profile">> := <<"PHASE2_INTEGRATED">>
        }} ->
            phase2_integrated;
        _ ->
            not_admitted
    end;
snapshot_profile(_, _) ->
    not_admitted.

live_head() ->
    try
        {ok, Repo} = file:get_cwd(),
        {0, Head} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
        iolist_to_binary(string:trim(Head))
    catch
        _:_ -> undefined
    end.

owner_verified(State) ->
    case maps:get(<<"owner">>, State, null) of
        #{<<"token">> := T} when is_binary(T), byte_size(T) > 0 -> true;
        _ -> false
    end.

map_atom(State, Key) ->
    case maps:get(Key, State, undefined) of
        <<"LIMITED_PARALLEL">> -> limited_parallel;
        <<"INTEGRATED_EXECUTION">> -> integrated_execution;
        <<"PHASE1_COMPLETE">> -> phase1_complete;
        <<"ALL_PHASES">> -> all_phases;
        Other -> Other
    end.

heavy_admitted(State) ->
    case maps:get(<<"resource_admission">>, State, undefined) of
        #{<<"status">> := <<"ADMITTED_FOR_HEAVY">>} -> true;
        #{<<"status">> := <<"ADMITTED">>} -> true;
        _ -> false
    end.

new_path(Root, Relative) ->
    try
        absolute = filename:pathtype(Root),
        relative = filename:pathtype(Relative),
        Parts = filename:split(Relative),
        true = Parts =/= [],
        false = lists:any(fun(Part) -> Part =:= ".." orelse Part =:= "." end, Parts),
        Path = filename:join(Root, Relative),
        safe_parent(filename:dirname(Path)),
        {error, enoent} = file:read_link_info(Path),
        {ok, Path}
    catch
        _:_ -> {error, unsafe_or_existing_path}
    end.

safe_parent("/") ->
    ok;
safe_parent(Path) ->
    absolute = filename:pathtype(Path),
    safe_parent(filename:dirname(Path)),
    case file:read_link_info(Path) of
        {ok, #file_info{type = directory}} -> ok;
        {error, enoent} -> ok;
        Other -> error({unsafe_parent, Path, Other})
    end.

validate_destination(Path) ->
    case file:read_link_info(Path) of
        {error, enoent} -> ok;
        {ok, #file_info{type = regular, links = 1}} -> ok;
        Other -> error({unsafe_destination, Path, Other})
    end.
