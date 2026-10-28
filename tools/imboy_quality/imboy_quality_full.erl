%% IMBOY_ERLANG_QUALITY P1-10/P1-12：分片全量执行器（纯 OTP）。
%% 从真实 U（EUNIT_MODS 规则重算：src 模块 + 非伴随 test 模块）构造 roots，
%% 以模块原子为根（EUnit 自动带入 M_tests 伴随），按 LPT 分到有界 peer 组，
%% 每组一 VM 顺序执行，结果按根落盘。独占根（Excl 清单）作为 U 成员单列。
-module(imboy_quality_full).
-export([inventory/1, run/3, root_identity_set/1, excluded/0]).

-define(EXCL, [
    elib_tsid,
    agent_grant_pg,
    agent_run_pg,
    agent_grant_pg_tests,
    agent_recovery_pg_tests,
    agent_run_pg_tests,
    agent_tool_authorizer_pg_tests,
    cs_pg_widget,
    cs_seat_console_pg_tests,
    imboy_hird_acceptance_tests,
    imboy_hird_replay_offline_tests,
    agent_hird_mini_spike_tests,
    imboy_hird_p1_auth_tool_tests,
    imboy_hird_gates_tests,
    imboy_hird_p2_replay_tests,
    imboy_hird_p3_compare_tests,
    talk_record_cleanup_retry_pg_checks,
    talk_record_delete_tests,
    attachment_lifecycle_v5_w403_gateway_protocol_tests,
    attachment_lifecycle_v5_w403_driver_matrix_tests,
    attachment_ordinary_http_protocol_tests,
    eb_asset_frozen_storage_protocol_tests,
    enterprise_asset_driver_pg_tests,
    attachment_pending_location_pg_tests,
    message_result_pg_tests,
    group_schedule_remind_pg_tests
]).

excluded() -> ?EXCL.

%% ---- U 盘点（与 P1-02 口径一致，运行时重算） ----

inventory(Repo) ->
    Src = mods(filelib:wildcard(filename:join(Repo, "src/**/*.erl"))),
    Test = mods(filelib:wildcard(filename:join(Repo, "test/**/*.erl"))),
    Companions = sets:from_list([list_to_atom(atom_to_list(M) ++ "_tests") || M <- Src]),
    TestRoots = [M || M <- Test, not sets:is_element(M, Companions)],
    All = lists:usort(Src ++ TestRoots),
    Ordinary = [M || M <- All, not lists:member(M, ?EXCL)],
    Excl = [M || M <- All, lists:member(M, ?EXCL)],
    #{ordinary => Ordinary, excluded => Excl, total => length(All)}.

mods(Paths) ->
    lists:usort([list_to_atom(filename:rootname(filename:basename(P))) || P <- Paths]).

%% ---- 执行 ----

run(Roots0, Opts, RunId) ->
    Repo = maps:get(repo, Opts),
    Ctrl = maps:get(ctrl, Opts),
    Prefix = maps:get(out_prefix, Opts, "full-"),
    OutDir = filename:join([Ctrl, "evidence", Prefix ++ RunId]),
    ok = filelib:ensure_dir(filename:join(OutDir, "x")),
    T0 = erlang:monotonic_time(millisecond),
    Est = maps:get(estimates, Opts, #{}),
    AliasState = ensure_lib_alias(Repo),
    try
        run_locked(Roots0, Opts, RunId, Repo, OutDir, T0, Est)
    after
        maybe_remove_lib_alias(Repo, AliasState)
    end.

run_locked(Roots0, Opts, RunId, Repo, OutDir, T0, Est) ->
    %% 分片原则：恰好 worker_count 个长驻 VM，每 VM 内顺序执行其份额，app 由
    %% 既有 boot 协调器每 VM 启动一次（legacy opt-run1 实证：app 启动 1 次不
    %% 退出）。不按 root 打独占资源标签（opt-run2 教训：pg 标签→165 组→165
    %% 次 app 启动→大量 context setup failed）；真正独占的 Excl 根单列不入
    %% 普通分片（legacy EUNIT_TEST_SPEC 同样整体剔除）。
    Roots = [
        #{
            id => M,
            test_mod => M,
            resources => [],
            estimated_ms => maps:get(M, Est, 3000)
        }
     || M <- Roots0
    ],
    {ok, Groups} = imboy_quality_pool:schedule(Roots, maps:get(worker_count, Opts, 2)),
    Paths = code_paths(Repo),
    Parent = self(),
    Ref = make_ref(),
    CfgFn = maps:get(worker_config_fn, Opts, fun(_) -> undefined end),
    Indexed = lists:zip(lists:seq(0, length(Groups) - 1), Groups),
    Pids = [
        spawn(fun() ->
            Mods = [maps:get(test_mod, R) || R <- G],
            WOpts = Opts#{
                schedulers => maps:get(schedulers, Opts, 2),
                config => CfgFn(K)
            },
            R = (imboy_quality_runner:run_group_in_worker(
                Mods, WOpts, maps:get(deadline_ms, Opts, 3000000), Paths
            ))#{
                group_index => K
            },
            Parent ! {grp, Ref, self(), length(Mods), R}
        end)
     || {K, G} <- Indexed, G =/= []
    ],
    PerRoot = collect(Ref, #{}, length(Pids)),
    Wall = erlang:monotonic_time(millisecond) - T0,
    Summary = summarize(PerRoot, Roots0),
    Result = Summary#{
        <<"run_id">> => list_to_binary(RunId),
        <<"wall_ms">> => Wall,
        <<"worker_count">> => maps:get(worker_count, Opts, 2),
        <<"groups">> => length(Pids),
        <<"roots_total">> => length(Roots0)
    },
    {ok, _} = imboy_quality_report:write(filename:join(OutDir, "result.json"), Result),
    ok = file:write_file(
        filename:join(OutDir, "per_root.term"),
        io_lib:format("~p.~n", [PerRoot])
    ),
    {Result, OutDir}.

%% 仓库根 lib_dir 别名：legacy eunit-local 的 `ln -sfn . imboy` 同口径。
%% worktree 目录名为 worktree，code:lib_dir(imboy)/code:priv_dir(imboy) 依赖
%% 路径条目的父目录名恰为 <app>；无此别名 app 启动即 terminology_priv_dir_error
%% （opt-run3 根因）。软链已在 .gitignore:123（/imboy），结束删除。
ensure_lib_alias(Repo) ->
    Alias = filename:join(Repo, "imboy"),
    case file:read_link(Alias) of
        {ok, "."} ->
            existing;
        {ok, Other} ->
            erlang:error({lib_alias_conflict, Alias, Other});
        {error, _} ->
            ok = file:make_symlink(".", Alias),
            created
    end.

maybe_remove_lib_alias(_Repo, existing) ->
    ok;
maybe_remove_lib_alias(Repo, created) ->
    _ = file:delete(filename:join(Repo, "imboy")),
    ok.

collect(_Ref, Acc, 0) ->
    Acc;
collect(Ref, Acc, N) ->
    receive
        {grp, Ref, _Pid, _Len, #{per_root := PR} = R} when is_map_key(group_index, R) ->
            Info = {worker_info, maps:get(group_index, R)},
            collect(Ref, maps:merge(Acc#{Info => maps:without([per_root], R)}, PR), N - 1);
        {grp, Ref, _Pid, _Len, #{per_root := PR}} ->
            collect(Ref, maps:merge(Acc, PR), N - 1);
        {grp, Ref, _Pid, Len, #{exit := Exit} = R} ->
            %% 整组 VM 崩溃/超时：组级失败记录（不丢身份，整体非通过）
            collect(Ref, Acc#{{group_failure, Exit, Len, erlang:unique_integer()} => R}, N - 1)
    end.

summarize(PerRoot, Roots) ->
    Fold = fun(M, {P, F, Cn, Ok, Bad, Missing}) ->
        case maps:get(M, PerRoot, undefined) of
            #{exit := ok, counts := C} ->
                {
                    P + maps:get(pass, C, 0),
                    F + maps:get(fail, C, 0),
                    Cn + maps:get(cancel, C, 0),
                    [M | Ok],
                    Bad,
                    Missing
                };
            #{counts := C} ->
                {
                    P + maps:get(pass, C, 0),
                    F + maps:get(fail, C, 0),
                    Cn + maps:get(cancel, C, 0),
                    Ok,
                    [M | Bad],
                    Missing
                };
            undefined ->
                {P, F, Cn, Ok, Bad, [M | Missing]}
        end
    end,
    {P, F, Cn, Ok, Bad, Missing} = lists:foldl(Fold, {0, 0, 0, [], [], []}, Roots),
    #{
        <<"pass">> => P,
        <<"fail">> => F,
        <<"cancel">> => Cn,
        <<"roots_ok">> => length(Ok),
        <<"roots_failed">> => length(Bad),
        <<"roots_missing">> => length(Missing),
        <<"failed_roots">> => [atom_to_binary(M) || M <- lists:sort(Bad)],
        <<"missing_roots">> => [atom_to_binary(M) || M <- lists:sort(Missing)]
    }.

%% 身份集合：EQ01 等价对照（根集合 + 每根叶计数）
root_identity_set(PerRoot) ->
    lists:sort([
        {M, maps:get(pass, C, 0) + maps:get(fail, C, 0) + maps:get(cancel, C, 0)}
     || {M, #{counts := C}} <- maps:to_list(PerRoot), is_atom(M)
    ]).

code_paths(Repo) ->
    %% 与 legacy EUNIT_ERL_OPTS 同序（-pa 后者优先 → 列表尾部优先级最高，
    %% runner 按原序逐个 -pa，FrameworkPath 垫底）：
    %%   imboy/ebin（lib_dir 别名，opt-run3 修复） < ebin < test
    %%   < feature-test-ebin（Makefile:269，ERLC_EXCLUDE 特性的单元 beam）。
    %% deps 不走 -pa：legacy 经 ERL_LIBS 进路径（优先级低于全部 -pa），
    %% 由调用方经 Opts.env 传 ERL_LIBS。
    Base = [
        filename:join(Repo, "imboy/ebin"),
        filename:join(Repo, "ebin"),
        filename:join(Repo, "test")
    ],
    Feature = filename:join(Repo, ".erlang.mk/feature-test-ebin"),
    Base ++ [Feature || filelib:is_dir(Feature)].
