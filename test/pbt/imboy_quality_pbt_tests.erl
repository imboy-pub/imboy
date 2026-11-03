%% IMBOY_ERLANG_QUALITY P2-01：PropEr 执行底座自测（PBT01–PBT05 机制义务）。
%% 正向 = 真实 PropEr v1.5.0@5640d071 引擎真实计数；负向 = 坏 manifest、
%% 完成事件 0/2 次、undef/崩溃、见证失败——全部必须被拒绝且不得假绿。
-module(imboy_quality_pbt_tests).
-include_lib("eunit/include/eunit.hrl").

-define(PBT, imboy_quality_pbt).
-define(DEMO, imboy_quality_pbt_demo).

%% ---- PBT02：manifest 负向校验（启动前拒绝） ----

empty_manifest_rejected_test() ->
    ?assertMatch(
        {error, empty_manifest},
        ?PBT:validate_manifest(#{properties => []})
    ),
    ?assertMatch(
        {error, manifest_missing_properties},
        ?PBT:validate_manifest(#{})
    ).

duplicate_id_rejected_test() ->
    P = #{
        id => dup,
        kind => ordinary,
        mfa => {?DEMO, prop_sort_idempotent, 0},
        witnesses => [{?DEMO, witness_ok, 0}]
    },
    ?assertMatch(
        {error, duplicate_property_ids},
        ?PBT:validate_manifest(#{properties => [P, P]})
    ).

bad_mfa_and_missing_witnesses_rejected_test() ->
    BadMFA = #{
        id => m1,
        kind => ordinary,
        mfa => {?DEMO, prop_sort_idempotent, 1},
        witnesses => [{?DEMO, witness_ok, 0}]
    },
    ?assertMatch(
        {error, [{invalid_fields, _} | _]},
        ?PBT:validate_manifest(#{properties => [BadMFA]})
    ),
    NoW = #{id => m2, kind => ordinary, mfa => {?DEMO, prop_sort_idempotent, 0}},
    ?assertMatch(
        {error, [{invalid_fields, _} | _]},
        ?PBT:validate_manifest(#{properties => [NoW]})
    ).

unknown_profile_rejected_before_run_test() ->
    ?assertMatch(
        {error, {unknown_profile, weekend}},
        ?PBT:run(#{properties => []}, weekend, #{})
    ),
    ?assertError(function_clause, ?PBT:profile_numtests(weekend, ordinary)).

valid_manifest_accepted_test() ->
    P = #{
        id => ok1,
        kind => ordinary,
        mfa => {?DEMO, prop_sort_idempotent, 0},
        witnesses => [{?DEMO, witness_ok, 0}]
    },
    ?assertMatch({ok, _}, ?PBT:validate_manifest(#{properties => [P]})).

%% ---- PBT03：真实计数与完成事件合同 ----

count_completions_pure_test() ->
    ?assertEqual({0, 0}, ?PBT:count_completions([{some_other, []}])),
    ?assertEqual(
        {1, 100},
        ?PBT:count_completions([{"OK: Passed ~b test(s).~n", [100]}])
    ),
    ?assertEqual(
        {2, 200},
        ?PBT:count_completions([
            {"OK: Passed ~b test(s).~n", [100]},
            {"OK: Passed ~b test(s).~n", [100]}
        ])
    ).

completion_verdict_branches_test() ->
    %% 引擎 true + 恰 1 次 + 数量达标 → pass
    ?assertEqual(pass, ?PBT:completion_verdict(true, 1, 200, 200)),
    %% 引擎 true + 1 次 + 欠计数 → incomplete_undercount
    ?assertEqual(
        incomplete_undercount,
        ?PBT:completion_verdict(true, 1, 150, 200)
    ),
    %% 引擎 true + 0 次完成事件 → INCOMPLETE（假绿防线）
    ?assertEqual(
        incomplete_no_completion_event,
        ?PBT:completion_verdict(true, 0, 0, 200)
    ),
    %% 引擎 true + 2 次完成事件 → INCOMPLETE（重复执行防线）
    ?assertEqual(
        incomplete_duplicate_completion_events,
        ?PBT:completion_verdict(true, 2, 400, 200)
    ),
    %% 引擎 false → property_failed（真实失败不得被计数器掩盖）
    ?assertEqual(
        property_failed,
        ?PBT:completion_verdict(false, 0, 0, 200)
    ).

real_engine_counting_test_() ->
    %% 真实 PropEr：quick/sequence=100 例恒真性质 → 引擎 true、恰 1 次完成
    %% 事件、Performed=100（对 proper 版本字面量合同的真实性验证）。
    {timeout, 60, fun() ->
        Spec = #{
            id => real_count_seq,
            kind => sequence,
            mfa => {?DEMO, prop_sort_idempotent, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{
            verdict := pass,
            completion_events := 1,
            performed := 100,
            requested := 100,
            engine_return := true,
            counterexample := none
        } =
            ?PBT:run_property(Spec, quick)
    end}.

%% ---- PBT04：undef / 崩溃必须失败并保留原始原因（不吞异常、不假绿） ----

undef_property_test_() ->
    {timeout, 60, fun() ->
        Spec = #{
            id => undef_case,
            kind => sequence,
            mfa => {?DEMO, undef_target, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{verdict := error, random_started := 0, reason := undef} =
            ?PBT:run_property(Spec, quick)
    end}.

crashing_or_failing_property_test_() ->
    {timeout, 120, fun() ->
        Crash = #{
            id => crash_case,
            kind => sequence,
            mfa => {?DEMO, prop_crash, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{verdict := VC} = ?PBT:run_property(Crash, quick),
        ?assert(lists:member(VC, [property_failed, error])),

        False = #{
            id => false_case,
            kind => sequence,
            mfa => {?DEMO, prop_always_false, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{
            verdict := property_failed,
            counterexample := CE,
            random_started := 1
        } =
            ?PBT:run_property(False, quick),
        %% 同进程 proper:counterexample() 取到完整 term（REPLAY 材料合同）。
        ?assertNotEqual(none, CE),
        ?assertNotEqual(error, CE)
    end}.

%% ---- PBT05：见证失败拒绝（random_started=0，不伪造 PropEr 反例） ----

witness_failure_rejected_test() ->
    Spec = #{
        id => witness_fail_case,
        kind => sequence,
        mfa => {?DEMO, prop_sort_idempotent, 0},
        witnesses => [{?DEMO, witness_bad, 0}]
    },
    #{
        verdict := witness_failed,
        random_started := 0,
        failed_witness := {?DEMO, witness_bad, 0},
        witness_reason := {error, demo_witness_failed}
    } =
        ?PBT:run_property(Spec, quick).

%% ---- PBT01：真实依赖加载、版本固定与 release 隔离 ----

proper_really_loaded_test() ->
    Which = code:which(proper),
    %% -pa 动态加载下 which 可能返回相对字符串或 {file,_}；两者都必须指向
    %% deps/proper/ebin（真实加载验证，非仅 TEST_DEPS 文本）。
    Bin =
        case Which of
            {file, B} -> unicode:characters_to_binary(B);
            S when is_list(S) -> unicode:characters_to_binary(S);
            Other -> erlang:error({proper_not_loaded, Other})
        end,
    ?assertNotEqual(
        nomatch,
        binary:match(Bin, <<"deps/proper/ebin">>)
    ).

proper_pinned_commit_matches_deps_mk_test() ->
    RepoRoot = repo_root(),
    {ok, DepsMk} = file:read_file(filename:join(RepoRoot, "include/deps.mk")),
    {PinStart, _PinLen} = binary:match(DepsMk, <<"dep_proper = git ">>),
    Rest = binary:part(DepsMk, PinStart, byte_size(DepsMk) - PinStart),
    %% "dep_proper = git <url> <commit>\\n..."：空格全切取第 5 字段，
    %% 再按换行截到本行（binary:split 无 parts 选项，只有 global/scope）。
    Parts = binary:split(Rest, <<" ">>, [global]),
    [Pinned0 | _] = binary:split(lists:nth(5, Parts), <<"\n">>),
    Pinned = Pinned0,
    Head = os:cmd("git -C " ++ RepoRoot ++ "/deps/proper rev-parse HEAD"),
    ?assertEqual(
        lists:droplast(Head),
        binary:bin_to_list(Pinned)
    ).

release_excludes_proper_test() ->
    {ok, Relx} = file:read_file(filename:join(repo_root(), "relx.config")),
    ?assertEqual(nomatch, binary:match(Relx, <<"proper">>)).

repo_root() ->
    %% beam 位置可能不在仓库树内（外置快速验证）；以 git 顶层为权威定位。
    Out = os:cmd("git rev-parse --show-toplevel 2>/dev/null"),
    string:trim(Out, both, "\n").

%% ---- P2-02 REPLAY01–REPLAY06 ----

replay01_shrink_and_etf_roundtrip_test_() ->
    {timeout, 120, fun() ->
        Spec = #{
            id => r01,
            kind => sequence,
            mfa => {?DEMO, prop_shrink_witness, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{
            verdict := property_failed,
            num_shrinks := {verified, N},
            counterexample := CE
        } =
            ?PBT:run_property(Spec, quick),
        %% 可控非最小失败：缩减必须实际发生。
        ?assert(N > 0),
        ?assert(is_list(CE) andalso CE =/= []),
        %% ETF 往返：保存 → 加载 → 同输入可重放。
        Tmp = replay_tmp_dir(),
        {ok, Path} = ?PBT:save_counterexample(Tmp, #{
            id => r01,
            case_kind => random,
            mfa => {?DEMO, prop_shrink_witness, 0},
            input => CE,
            source_sha => <<"before">>,
            performed => 1,
            num_shrinks => {verified, N}
        }),
        {ok, #{input := CE2}} = ?PBT:load_counterexample(Path),
        ?assertEqual(CE, CE2)
    end}.

replay02_witness_kind_not_claimed_shrunk_test() ->
    Tmp = replay_tmp_dir(),
    {ok, Path} = ?PBT:save_counterexample(Tmp, #{
        id => r02,
        case_kind => witness,
        witness_id => {?DEMO, witness_bad_with_input},
        mfa => {?DEMO, witness_bad_with_input, 0},
        input => {demo_witness_failed, 42},
        source_sha => <<"witness-probe">>
    }),
    {ok, #{case_kind := witness} = E0} = ?PBT:load_counterexample(Path),
    %% witness 反例不带 num_shrinks 键（REPLAY02：不称已缩减）
    ?assertEqual(false, maps:is_key(num_shrinks, E0)),
    ?assertEqual(false, filelib:is_regular(Path ++ ".shrunk")).

replay03_fix_before_fail_after_pass_test_() ->
    {timeout, 120, fun() ->
        Before = #{
            id => r03,
            kind => sequence,
            mfa => {?DEMO, prop_repair_before, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{verdict := property_failed, counterexample := [X]} =
            ?PBT:run_property(Before, quick),
        %% 同 schema（仅 mfa 不同）修复前失败、修复后通过。
        {ok, still_fails} = ?PBT:replay_check(
            #{mfa => {?DEMO, prop_repair_before, 0}, input => [X]},
            [{?DEMO, prop_repair_before, 0}, {?DEMO, prop_repair_after, 0}]
        ),
        {ok, now_passes} = ?PBT:replay_check(
            #{mfa => {?DEMO, prop_repair_after, 0}, input => [X]},
            [{?DEMO, prop_repair_before, 0}, {?DEMO, prop_repair_after, 0}]
        ),
        %% 未知 MFA 不在白名单：apply 前拒绝（REPLAY04）
        ?assertMatch(
            {error, mfa_not_allowed},
            ?PBT:replay_check(
                #{mfa => {no_such_module_anywhere, f, 0}, input => [X]},
                [{?DEMO, prop_repair_before, 0}]
            )
        )
    end}.

replay04_rejects_bad_etf_test() ->
    Tmp = replay_tmp_dir(),
    %% 损坏（截断）
    Corrupt = filename:join(Tmp, "corrupt.etf"),
    ok = file:write_file(Corrupt, binary:part(term_to_binary(#{a => 1}), 0, 5)),
    ?assertMatch({error, corrupt_etf}, ?PBT:load_counterexample(Corrupt)),
    %% 超大（>1MB）
    Big = filename:join(Tmp, "big.etf"),
    ok = file:write_file(Big, binary:copy(<<"x">>, 1048577)),
    ?assertMatch({error, too_large}, ?PBT:load_counterexample(Big)),
    %% 未知 schema
    Unknown = filename:join(Tmp, "unknown.etf"),
    ok = file:write_file(Unknown, term_to_binary(#{schema => 99, id => x})),
    ?assertMatch({error, {unknown_schema, 99}}, ?PBT:load_counterexample(Unknown)),
    %% 恶意 ETF（含 fun）：safe 解码必须拒绝
    Evil = filename:join(Tmp, "evil.etf"),
    ok = file:write_file(Evil, term_to_binary(#{schema => 1, fun_payload => fun() -> ok end})),
    %% safe 解码不拒普通 fun，显式递归扫描拒绝（REPLAY04）
    ?assertMatch({error, fun_in_etf}, ?PBT:load_counterexample(Evil)).

replay05_no_fake_cex_on_success_test_() ->
    {timeout, 60, fun() ->
        Spec = #{
            id => r05,
            kind => sequence,
            mfa => {?DEMO, prop_sort_idempotent, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        #{counterexample := none, num_shrinks := unverified} =
            ?PBT:run_property(Spec, quick),
        %% 成功运行不产生反例文件；缩减消息缺失时标 unverified（不声称完成）
        Tmp = replay_tmp_dir(),
        Allowed06 = [{?DEMO, prop_always_false, 0}],
        ?assertEqual({error, no_pending_counterexamples}, ?PBT:replay_next(Tmp, Allowed06))
    end}.

replay06_duplicate_and_pending_test_() ->
    {timeout, 120, fun() ->
        Tmp = replay_tmp_dir(),
        Entry = #{
            id => r06,
            case_kind => random,
            mfa => {?DEMO, prop_always_false, 0},
            input => [1],
            source_sha => <<"t">>
        },
        {ok, _} = ?PBT:save_counterexample(Tmp, Entry),
        %% 重复材料拒绝，不静默覆盖
        ?assertMatch({error, duplicate_material}, ?PBT:save_counterexample(Tmp, Entry)),
        %% 无待重放时如实错误；有则重放并标记
        {ok, #{replay := {ok, still_fails}}} = ?PBT:replay_next(Tmp, [{?DEMO, prop_always_false, 0}]),
        ?assertMatch(
            {error, no_pending_counterexamples},
            ?PBT:replay_next(Tmp, [{?DEMO, prop_always_false, 0}])
        )
    end}.

replay_tmp_dir() ->
    D = io_lib:format("/tmp/imboy_quality_pbt_replay_~s_~p", [
        os:getpid(), erlang:unique_integer([positive])
    ]),
    ok = filelib:ensure_dir(filename:join(D, "x")),
    D.

%% ---- peer 隔离执行（复用纯 Erlang 底座；真实性质经 peer 跑通） ----

peer_isolated_property_test_() ->
    {timeout, 120, fun() ->
        Repo = repo_root(),
        %% erlang.mk TEST 构建把测试 beam 放 test/、框架 beam 放
        %% .erlang.mk/quality-test-ebin/；peer 需要两者 + proper。
        Candidates = [
            filename:join(Repo, "deps/proper/ebin"),
            test_ebin(Repo),
            which_dir(?PBT)
        ],
        Paths = lists:usort([P || P <- Candidates, P =/= undefined, filelib:is_dir(P)]),
        ?assertNotEqual([], Paths),
        Spec = #{
            id => peer_case,
            kind => sequence,
            mfa => {?DEMO, prop_sort_idempotent, 0},
            witnesses => [{?DEMO, witness_ok, 0}]
        },
        case ?PBT:run_property_peer(Spec, quick, Paths) of
            #{verdict := pass, completion_events := 1} ->
                ok;
            Other ->
                ?assert(false, {peer_run_unexpected, Other})
        end
    end}.

test_ebin(Repo) ->
    %% erlang.mk TEST 构建产物位置（eunit-local t= 路径的 test beam 目录）；
    %% 外置快速验证时以 ?DEMO beam 实际所在目录为准（which 可能返回
    %% {file,_} 或直接字符串两种形态）。
    case filelib:is_dir(filename:join(Repo, "test_ebin")) of
        true -> filename:join(Repo, "test_ebin");
        false -> which_dir(?DEMO)
    end.

which_dir(M) ->
    case code:which(M) of
        {file, B} -> filename:dirname(B);
        S when is_list(S), S =/= non_existing -> filename:dirname(S);
        _ -> undefined
    end.
