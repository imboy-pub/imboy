-module(imboy_quality_full_budget).
-export([
    transaction/3,
    full_transition/7,
    require_test_build/1,
    run_id/0,
    build_profile/0,
    owner_verified/1
]).

transaction(Ctrl, Action, Options) ->
    PackageLock =
        "/Users/leeyi/project/imboy.pub/_task_packages/IMBOY_ERLANG_QUALITY/.package.lock",
    TaskLock = filename:join(Ctrl, "owner.lock"),
    ok = file:make_dir(PackageLock),
    try
        ok = file:make_dir(TaskLock),
        try
            full_transaction_locked(Ctrl, Action, Options)
        after
            ok = file:del_dir(TaskLock)
        end
    after
        ok = file:del_dir(PackageLock)
    end.

full_transaction_locked(Ctrl, Action, Options) ->
    Path = filename:join(Ctrl, "STATE.json"),
    {ok, State, _} = imboy_state_read(Path),
    true = map_size(State) >= 30,
    Ctx = (imboy_quality_state:trusted_context(State))#{owner_verified => owner_verified(State)},
    ok = imboy_quality_cli:permit(Ctx, eunit_full),
    {ok, Repo} = file:get_cwd(),
    true = unicode:characters_to_binary(Repo) =:= maps:get(<<"worktree">>, State),
    Recovery = maps:get(<<"approved_recovery_20261104">>, State),
    #{<<"category">> := Category, <<"run_id">> := RunId} = maps:get(<<"next_full">>, Recovery),
    %% Certification requires validated same-candidate batch settlement, not a mutable status.
    <<"p1_revalidation">> = Category,
    true = valid_run_id(RunId),
    Identity = full_identity(Repo, Options),
    Owner = maps:get(<<"owner">>, State),
    TokenDigest = string:lowercase(
        binary:encode_hex(crypto:hash(sha256, maps:get(<<"token">>, Owner)))
    ),
    NewState = full_transition(State, Action, Category, RunId, Identity, TokenDigest, Options),
    ok = write_state_checkpoint(Ctrl, NewState),
    #{op => Action, run_id => binary_to_list(RunId)}.

valid_run_id(Id) when is_binary(Id), byte_size(Id) > 0, byte_size(Id) =< 80 ->
    lists:all(
        fun(C) ->
            (C >= $a andalso C =< $z) orelse
                (C >= $A andalso C =< $Z) orelse
                (C >= $0 andalso C =< $9) orelse C =:= $_ orelse C =:= $-
        end,
        binary_to_list(Id)
    );
valid_run_id(_) ->
    false.

full_identity(Repo, Options) ->
    {0, Head} = imboy_quality_guard:git(Repo, ["rev-parse", "HEAD"]),
    #{key := BuildKey} = imboy_quality_build:build_key(Repo, maps:get(profile, Options)),
    Files = lists:sort(
        filelib:wildcard(filename:join(Repo, "tools/imboy_quality/*.erl")) ++
            [
                filename:join(Repo, P)
             || P <- [
                    "scripts/quality.escript",
                    "Makefile",
                    "erlang.mk",
                    "include/deps.mk",
                    maps:get(config, Options) ++ ".config"
                ]
            ]
    ),
    FileHashes = [{P, file_digest(P)} || P <- Files],
    Roots = imboy_quality_full:inventory(Repo),
    #{
        <<"candidate_sha">> => iolist_to_binary(string:trim(Head)),
        <<"input_fingerprint">> => digest_term({BuildKey, FileHashes}),
        <<"static_roots_digest">> => digest_term(Roots),
        <<"roots_scope">> => <<"STATIC_ROOTS_ONLY">>
    }.

file_digest(Path) ->
    {ok, Bytes} = file:read_file(Path),
    crypto:hash(sha256, Bytes).

digest_term(Term) ->
    string:lowercase(binary:encode_hex(crypto:hash(sha256, term_to_binary(Term)))).

full_transition(State, reserve, Category, RunId, Identity, TokenDigest, _Options) ->
    {ok, Reserved} = imboy_quality_budget:reserve(State, Category, RunId),
    update_full_run(Reserved, RunId, fun(Run) ->
        Run#{
            <<"identity">> => Identity,
            <<"owner_token_sha256">> => TokenDigest,
            <<"build_complete">> => false,
            <<"claimed">> => false
        }
    end);
full_transition(State, build_complete, _, RunId, Identity, TokenDigest, Options) ->
    {ok, Repo} = file:get_cwd(),
    verify_test_build(Repo, Options),
    update_full_run(State, RunId, fun(Run) ->
        verify_full_run(Run, Identity, TokenDigest),
        false = maps:get(<<"build_complete">>, Run),
        false = maps:get(<<"claimed">>, Run),
        Run#{<<"build_complete">> := true}
    end);
full_transition(State, claim, _, RunId, Identity, TokenDigest, Options) ->
    {ok, Repo} = file:get_cwd(),
    verify_test_build(Repo, Options),
    update_full_run(State, RunId, fun(Run) ->
        verify_full_run(Run, Identity, TokenDigest),
        true = maps:get(<<"build_complete">>, Run),
        false = maps:get(<<"claimed">>, Run),
        Run#{<<"claimed">> := true}
    end).

verify_test_build(Repo, Options) ->
    require_test_build(Repo),
    #{key := Key, inputs := _VtbInputs} = imboy_quality_build:build_key(
        Repo, maps:get(profile, Options)
    ),
    %% 2026-11-07 排障转储（IMBOY_QUALITY_KEY_DIAG=1 时启用；默认零开销）：
    %% 同树跨进程 build_key 漂移定位——转储本次计算 key 与收据 key 及
    %% src/test 映射的规范化指纹。
    case os:getenv("IMBOY_QUALITY_KEY_DIAG") of
        "1" ->
            SrcM = maps:get(src, _VtbInputs),
            TestM = maps:get(test, _VtbInputs),
            CanFp = crypto:hash(
                sha256,
                term_to_binary(
                    {
                        lists:sort(maps:to_list(SrcM)),
                        lists:sort(maps:to_list(TestM)),
                        maps:get(deps, _VtbInputs)
                    }
                )
            ),
            R = imboy_quality_build:receipt_verify(maps:get(receipt, Options), Repo, Key),
            io:format(
                "KEY-DIAG computed=~s receipt_cmp=~tp canon_fp=~ts src_n=~p test_n=~p~n",
                [
                    string:lowercase(binary:encode_hex(Key)),
                    R,
                    binary:encode_hex(CanFp),
                    map_size(SrcM),
                    map_size(TestM)
                ]
            ),
            {hit, Key} = R,
            ok;
        _ ->
            {hit, Key} = imboy_quality_build:receipt_verify(maps:get(receipt, Options), Repo, Key),
            ok
    end.

verify_full_run(Run, Identity, TokenDigest) ->
    <<"started">> = maps:get(<<"status">>, Run),
    Identity = maps:get(<<"identity">>, Run),
    TokenDigest = maps:get(<<"owner_token_sha256">>, Run),
    ok.

update_full_run(State, RunId, Update) ->
    Recovery = maps:get(<<"approved_recovery_20261104">>, State),
    Runs = maps:get(<<"new_full_runs">>, Recovery),
    1 = length([R || R <- Runs, maps:get(<<"run_id">>, R) =:= RunId]),
    NewRuns = [
        case maps:get(<<"run_id">>, R) of
            RunId -> Update(R);
            _ -> R
        end
     || R <- Runs
    ],
    State#{
        <<"approved_recovery_20261104">> := Recovery#{<<"new_full_runs">> := NewRuns},
        <<"checkpoint_sequence">> := maps:get(<<"checkpoint_sequence">>, State) + 1
    }.

write_state_checkpoint(Ctrl, State) ->
    Bytes = iolist_to_binary(json:encode(State)),
    Sha = string:lowercase(binary:encode_hex(crypto:hash(sha256, Bytes))),
    lists:foreach(
        fun(Name) ->
            Path = filename:join(Ctrl, Name),
            ok = atomic_bytes(Path, Bytes),
            ok = atomic_bytes(Path ++ ".sha256", Sha)
        end,
        ["STATE.json", "checkpoint.json"]
    ),
    ok.

atomic_bytes(Path, Bytes) ->
    Temp = Path ++ ".tmp." ++ os:getpid(),
    {ok, H} = file:open(Temp, [write, binary, raw, exclusive]),
    try
        ok = file:change_mode(Temp, 8#600),
        ok = file:write(H, Bytes),
        ok = file:sync(H)
    after
        ok = file:close(H)
    end,
    file:rename(Temp, Path).

require_test_build(Repo) ->
    Marker = filename:join(Repo, ".erlang.mk/imboy.test"),
    case filelib:is_regular(Marker) of
        true ->
            ok;
        false ->
            erlang:error(
                {test_build_marker_missing, Marker,
                    {hint, <<"run: IMBOYENV=local make test-build">>}}
            )
    end.

owner_verified(State) ->
    case {file:get_cwd(), os:getenv("IMBOY_QUALITY_OWNER_TOKEN")} of
        {{ok, Repo}, Supplied} when is_list(Supplied), Supplied =/= [] ->
            imboy_quality_task_owner:verify(
                State,
                unicode:characters_to_binary(Supplied),
                unicode:characters_to_binary(Repo)
            );
        _ ->
            false
    end.

imboy_state_read(Path) ->
    {ok, B} = file:read_file(Path),
    {ok, Sidecar} = file:read_file(Path ++ ".sha256"),
    Hash = crypto:hash(sha256, B),
    true = string:trim(Sidecar) =:= string:lowercase(binary:encode_hex(Hash)),
    Map = json:decode(B),
    true = is_map(Map),
    {ok, Map, Hash}.

run_id() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:universal_time(),
    lists:flatten(
        io_lib:format(
            "~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0BZ",
            [Y, Mo, D, H, Mi, S]
        )
    ).

build_profile() ->
    #{
        compiler_opts => split_opts(os:getenv("IMBOY_QUALITY_TEST_ERLC_OPTS")),
        erlc_opts => split_opts(os:getenv("IMBOY_QUALITY_ERLC_OPTS")),
        features => split_opts(os:getenv("IMBOY_QUALITY_FEATURES")),
        test_mode => true
    }.

split_opts(false) ->
    [];
split_opts(S) when is_list(S) ->
    [P || P <- string:split(S, " ", all), P =/= []].
