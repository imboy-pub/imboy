%% Draft only: producer must supply independently verified candidate and run context.
-module(imboy_quality_ct).
-export([
    run_suites/2,
    run_suites_detailed/2,
    classify/1,
    classify_detailed/1,
    critical_report/2,
    report_filename/2
]).

run_suites(Suites, TestDir) ->
    LogDir = ct_logdir(TestDir),
    [
        {S,
            classify(
                ct:run_test([{dir, TestDir}, {suite, atom_to_list(S)}, {logdir, LogDir}])
            )}
     || S <- Suites
    ].

run_suites_detailed(Suites, TestDir) ->
    LogDir = ct_logdir(TestDir),
    [
        {S,
            classify_detailed(
                ct:run_test([{dir, TestDir}, {suite, atom_to_list(S)}, {logdir, LogDir}])
            )}
     || S <- Suites
    ].

%% D5（QUALITY_GATE_HARDENING_PROPOSAL_20261110 §D5）：CT 日志/资产落 TMPDIR
%% 隔离目录，不再落 peer 继承的仓根 cwd（index.html/all_runs.html 等污染源）。
ct_logdir(TestDir) ->
    Dir =
        filename:join(
            os:getenv("TMPDIR", "/tmp"),
            "imboy_ct_" ++ integer_to_list(erlang:phash2([TestDir, os:timestamp()]))
        ),
    ok = filelib:ensure_dir(filename:join(Dir, "x")),
    Dir.

classify(Raw) ->
    case classify_detailed(Raw) of
        #{verdict := pass} -> pass;
        #{reason := Reason} -> {fail, Reason}
    end.

classify_detailed({Ok, Failed, {UserSkip, AutoSkip}}) when
    is_integer(Ok),
    Ok >= 0,
    is_integer(Failed),
    Failed >= 0,
    is_integer(UserSkip),
    UserSkip >= 0,
    is_integer(AutoSkip),
    AutoSkip >= 0
->
    counts(Ok, Failed, UserSkip, AutoSkip);
classify_detailed({error, Reason}) ->
    #{verdict => fail, reason => {ct_error, Reason}};
classify_detailed(Other) ->
    #{verdict => fail, reason => {unexpected_ct_result, Other}}.

counts(Ok, Failed, UserSkip, AutoSkip) ->
    Reason =
        if
            Ok + Failed + UserSkip + AutoSkip =:= 0 -> zero_testcases;
            Failed > 0 -> {failed, Failed};
            UserSkip + AutoSkip > 0 -> {auto_or_user_skip, UserSkip + AutoSkip};
            true -> none
        end,
    #{
        passed_cases => Ok,
        failed_cases => Failed,
        user_skip => UserSkip,
        auto_skip => AutoSkip,
        skip => UserSkip + AutoSkip,
        verdict =>
            case Reason of
                none -> pass;
                _ -> fail
            end,
        reason => Reason
    }.

critical_report(Results, Context) when is_list(Results), is_map(Context) ->
    Required = lists:sort([
        msg_delivery_SUITE,
        msg_ack_logic_SUITE,
        imboy_quality_ct02_org_boundary_SUITE,
        imboy_quality_ct03_migration_SUITE
    ]),
    Names = [S || {S, _} <- Results],
    case
        lists:sort(Names) =:= Required andalso length(Results) =:= length(Required) andalso
            lists:all(fun valid_suite/1, Results)
    of
        true -> {error, candidate_and_cleanup_binding_not_implemented};
        false -> {error, incomplete_or_failed_critical}
    end;
critical_report(_, _) ->
    {error, invalid_critical_result}.

valid_suite(
    {_, #{
        verdict := pass,
        passed_cases := N,
        failed_cases := 0,
        skip := 0
    }}
) when is_integer(N), N > 0 ->
    true;
valid_suite(_) ->
    false.

%% CT04 requires independently captured injection and cleanup events; no result
%% from count-only CT is promoted into a CT04 receipt.
report_filename(Profile, RunId) when
    (Profile =:= critical orelse Profile =:= ct04),
    is_binary(RunId),
    byte_size(RunId) > 0,
    byte_size(RunId) =< 128
->
    case lists:all(fun safe_char/1, binary_to_list(RunId)) of
        true -> {ok, <<"ct-", (atom_to_binary(Profile))/binary, "-", RunId/binary, ".json">>};
        false -> {error, invalid_run_id}
    end;
report_filename(_, _) ->
    {error, invalid_profile_or_run_id}.

safe_char(C) ->
    (C >= $a andalso C =< $z) orelse
        (C >= $A andalso C =< $Z) orelse
        (C >= $0 andalso C =< $9) orelse C =:= $- orelse C =:= $_.
