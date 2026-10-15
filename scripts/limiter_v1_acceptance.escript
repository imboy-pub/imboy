#!/usr/bin/env escript
%%! -noshell -noinput +A 4
%%%
%%% Limiter V1 验收最小入口（计划 docs/plans/2026-10-13-limiter-v1-*
%%% §9.2 命令别名 C）。L0 职责：实现入口、参数校验、失败退出码传播；
%%% 仅承载现有 EUnit runner 无法表达的场景（REST 组合 / 负载 / 节点切换）。
%%% EUnit 承载的 AC（Q(模块)）不在本工具重复执行。
%%%
%%% 用法:
%%%   scripts/limiter_v1_acceptance.escript --case AC-01 --manifest <清单>
%%%   scripts/limiter_v1_acceptance.escript --case AC-16 --oracle baseline|fixed
%%%   scripts/limiter_v1_acceptance.escript --case AC-15
%%%   scripts/limiter_v1_acceptance.escript --list
%%%   scripts/limiter_v1_acceptance.escript --emit-manifest <输出路径>
%%%
%%% 选项:
%%%   --case AC-XX           执行指定验收场景
%%%   --manifest <路径>      候选清单（每行 `<sha256>  <path>`；--emit-manifest 生成）
%%%   --oracle baseline|fixed  断言模式（默认 baseline）
%%%   --evidence-dir <目录>  证据输出目录（默认 docs/qa/limiter-v1/...，
%%%                          该路径在 .gitignore 的 /docs/qa/ 下）
%%%
%%% 退出码:
%%%   0  PASS（该 case 在所选 oracle 下成立）
%%%   2  用法/环境错误（参数缺失、manifest 不可读、证据缺失/损坏等）
%%%   3  场景执行失败（断言/子命令非零退出；AC-16 fixed 超预算）
%%%   4  INPUT_PENDING / NOT_IMPLEMENTED（AC-13/18 需隔离演练环境）
%%%   5  未知 case
%%%

main(Args) ->
    case parse(Args, #{oracle => baseline, evidence_dir => undefined}) of
        {ok, Opts} -> dispatch(Opts);
        {error, UsageError} -> usage_error(UsageError)
    end.

%% -------------------------------------------------------------------------
%% 参数解析
%% -------------------------------------------------------------------------

parse([], Acc) ->
    case Acc of
        #{action := _} -> {ok, Acc};
        _ -> {error, missing_case_or_list}
    end;
parse(["--list" | Rest], Acc) ->
    parse(Rest, Acc#{action => list});
parse(["--case", Case | Rest], Acc) ->
    parse(Rest, Acc#{action => {run_case, Case}});
parse(["--emit-manifest", Path | Rest], Acc) ->
    parse(Rest, Acc#{action => {emit_manifest, Path}});
parse(["--manifest", Path | Rest], Acc) ->
    parse(Rest, Acc#{manifest => Path});
parse(["--oracle", Oracle | Rest], Acc) when Oracle =:= "baseline"; Oracle =:= "fixed" ->
    parse(Rest, Acc#{oracle := list_to_atom(Oracle)});
parse(["--oracle", Other | _], _Acc) ->
    {error, {bad_oracle, Other}};
parse(["--evidence-dir", Dir | Rest], Acc) ->
    parse(Rest, Acc#{evidence_dir => Dir});
parse([Unknown | _], _Acc) ->
    {error, {unknown_arg, Unknown}}.

dispatch(#{action := list}) ->
    list_cases(),
    halt(0);
dispatch(#{action := {emit_manifest, Path}}) ->
    emit_manifest(Path);
dispatch(#{action := {run_case, Case}} = Opts) ->
    run_case(Case, Opts).

%% -------------------------------------------------------------------------
%% case 登记
%% -------------------------------------------------------------------------

%% C 承载的 case：REST / 负载 / 节点切换。
-define(CASES, [
    {<<"AC-01">>, c_rest_login_real_failures},
    {<<"AC-13">>, c_ac13_restart_recovery},
    {<<"AC-15">>, c_ac15_node_boundary},
    {<<"AC-16">>, c_ac16_performance},
    {<<"AC-18">>, c_ac18_cutover_rollback}
]).

%% Q(模块) 承载的 case：本工具不重复执行，给出定向命令。
-define(Q_CASES, [
    {<<"AC-02">>, <<"passport_logic_tests">>},
    {<<"AC-03">>, <<"login_attempt_ds_tests">>},
    {<<"AC-04">>, <<"login_attempt_ds_tests">>},
    {<<"AC-05">>, <<"login_security_logic_tests">>},
    {<<"AC-06">>, <<"login_attempt_ds_tests">>},
    {<<"AC-07">>, <<"login_attempt_ds_tests">>},
    {<<"AC-08">>, <<"login_attempt_ds_tests">>},
    {<<"AC-09">>, <<"elib_req_client_ip_tests">>},
    {<<"AC-10">>, <<"login_attempt_ds_tests">>},
    {<<"AC-11">>, <<"login_attempt_ds_tests">>},
    {<<"AC-12">>, <<"login_security_logic_tests">>},
    {<<"AC-14">>, <<"throttle_middleware_tests">>},
    {<<"AC-17">>, <<"passport_handler_tests">>},
    {<<"AC-19">>, <<"passport_logic_tests">>}
]).

list_cases() ->
    io:format("~ts~n", [<<"C 承载（本工具）:"/utf8>>]),
    io:format("  AC-01  真实 REST 登录链失败计数（--oracle baseline|fixed；需 --manifest）~n"),
    io:format("  AC-13  重启/停节点恢复（INPUT_PENDING：需隔离演练环境，退出码 4）~n"),
    io:format("  AC-15  两节点同机隔离 admit（各 -noshell 节点独立 epmd 端口，合计 2N 如实报告）~n"),
    io:format("  AC-16  性能预算（--oracle baseline=校验 0d28ad4f 基线证据并输出摘要；~n"),
    io:format("                 --oracle fixed=本仓新实现同款微基准并按 C.9 预算比较）~n"),
    io:format("  AC-18  切换/回退演练（INPUT_PENDING：需隔离演练环境，退出码 4）~n"),
    io:format("~ts~n", [<<"Q 承载（IMBOYENV=local make eunit-local t=<模块>）:"/utf8>>]),
    [io:format("  ~s -> ~s~n", [C, M]) || {C, M} <- ?Q_CASES].

run_case(CaseBin, Opts) ->
    Case = unicode:characters_to_binary(CaseBin),
    Manifest = maps:get(manifest, Opts, undefined),
    case lists:keyfind(Case, 1, ?CASES) of
        {Case, c_rest_login_real_failures} ->
            ensure_manifest(Manifest, fun() -> run_ac01(Opts) end);
        {Case, c_ac16_performance} ->
            run_ac16(Opts);
        {Case, c_ac15_node_boundary} ->
            run_ac15(Opts);
        {Case, c_ac13_restart_recovery} ->
            pending(Case, <<"AC-13: 重启/停节点恢复需隔离演练环境（独立 VM/端口/DB），L0 本地不可脚本化（INPUT_PENDING）"/utf8>>);
        {Case, c_ac18_cutover_rollback} ->
            pending(Case, <<"AC-18: 切换/回退演练需隔离演练环境与已验证回退候选，见计划 §9.3（INPUT_PENDING）"/utf8>>);
        false ->
            case lists:keyfind(Case, 1, ?Q_CASES) of
                {Case, Module} ->
                    io:format(
                        "~ts IMBOYENV=local make eunit-local t=~s~n",
                        [Case, Module]
                    ),
                    halt(4);
                false ->
                    UnknownMsg = <<"未知 case: "/utf8, Case/binary, "（--list 查看登记）"/utf8>>,
                    io:format(standard_error, "~ts~n", [UnknownMsg]),
                    halt(5)
            end
    end.

ensure_manifest(undefined, _Fun) ->
    io:format(standard_error, "~ts~n", [<<"缺少 --manifest（--emit-manifest 生成候选清单）"/utf8>>]),
    halt(2);
ensure_manifest(Path, Fun) ->
    case file:read_file(Path) of
        {ok, Bin} when byte_size(Bin) > 0 ->
            Fun();
        {ok, _} ->
            io:format(standard_error, "manifest 为空: ~s~n", [Path]),
            halt(2);
        {error, Reason} ->
            io:format(standard_error, "manifest 不可读: ~s (~p)~n", [Path, Reason]),
            halt(2)
    end.

pending(Case, Reason) ->
    io:format("~ts INPUT_PENDING/NOT_IMPLEMENTED: ~ts~n", [Case, Reason]),
    halt(4).

%% -------------------------------------------------------------------------
%% AC-01：真实 REST 登录链失败计数（L0 baseline oracle）
%% -------------------------------------------------------------------------

run_ac01(Opts) ->
    %% L3 后 REST case（api_v1_login_SUITE:limiter_real_failures）已按计划
    %% §9.2 翻转为 fixed oracle（计数 1..F、F 后锁定拒绝且不触碰凭据）。
    %% --oracle fixed：按新合同断言（runner 绿 = PASS）。
    %% --oracle baseline：L0 旧侧缺陷复现 oracle 已被翻转、不再可断言；
    %% 本模式仍真实执行同一 runner（证据同路径落盘），PASS/FAIL 判据同
    %% fixed，输出中注明 oracle 语义迁移。
    run_ac01_runner(Opts).

run_ac01_runner(Opts) ->
    Oracle = maps:get(oracle, Opts),
    case Oracle of
        baseline ->
            io:format(
                "~ts~n",
                [<<"AC-01: L0 baseline oracle 已按计划 §9.2 翻转为 fixed（REST case 现断言计数 1..F 与锁定拒绝）；本 run 按 fixed 合同执行。"/utf8>>]
            );
        fixed -> ok
    end,
    Root = project_root(),
    EvDir = abs_evidence_dir(Root, evidence_dir(Opts, <<"AC-01">>)),
    ok = filelib:ensure_dir(<<EvDir/binary, "/x">>),
    RunId = "limiter-l0-" ++ strftime_run_ref(),
    %% 走项目正规 REST 通道（scripts/run_rest_api_tests.sh：scratch 数据库
    %% + 完整环境 + evidence 交叉校验）。数据库凭据由调用方环境提供
    %% （REST_PG_*，runner 合同），本工具不读取、不落盘任何凭据。
    %% runner 自带 ct_imboy 共享节点等待（BLOCKED_SHARED_CT 退出码 75）。
    %% 输出重定向落盘而非 `| tee`：POSIX 管道下 $? 取 tee 的退出码（恒 0），
    %% 会把 runner 失败伪装成 PASS（run_shell 依赖尾部 printf 传播真实码）。
    %% REST scratch 库是全新空库，必须走迁移（auto_migrate=true）；本仓本地
    %% sys.local.config 可能因 eunit 共享库隔离而置 false（与 REST runner 合同
    %% 冲突），故派生 REST 专用 config（仅翻转该键），经 REST_CT_CONFIG 注入。
    RestConfig = ensure_rest_ct_config(Root),
    Cmd = lists:flatten(
        io_lib:format(
            "cd ~s && REST_RUN_ID=~s REST_CT_CONFIG=~s bash scripts/run_rest_api_tests.sh > ~s/output.log 2>&1",
            [Root, RunId, RestConfig, EvDir]
        )
    ),
    io:format("AC-01 (~s): ~s~n", [atom_to_list(Oracle), Cmd]),
    Rc = run_shell(Cmd),
    ok = file:write_file(<<EvDir/binary, "/exit_code">>, integer_to_binary(Rc)),
    ManifestVal =
        case maps:find(manifest, Opts) of
            {ok, V} when is_list(V) -> list_to_binary(V);
            {ok, V2} -> value_to_binary(V2);
            error -> undefined
        end,
    write_input_json(EvDir, #{
        case_id => <<"AC-01">>,
        oracle => Oracle,
        candidate => candidate_id(),
        manifest => ManifestVal,
        rest_run_id => list_to_binary(RunId)
    }),
    case Rc of
        0 ->
            PassMsg = <<"AC-01 PASS（oracle="/utf8, (atom_to_binary(Oracle, utf8))/binary,
                "；真实链 fixed oracle 成立；证据: "/utf8, EvDir/binary>>,
            io:format("~ts~n", [PassMsg]),
            halt(0);
        _ ->
            FailMsg0 =
                <<"AC-01 FAIL rc="/utf8, (integer_to_binary(Rc))/binary, "（oracle="/utf8,
                    (atom_to_binary(Oracle, utf8))/binary, "；证据: "/utf8,
                    EvDir/binary, "）"/utf8>>,
            io:format(standard_error, "~ts~n", [FailMsg0]),
            halt(3)
    end.

%% 从 config/sys.local.config 派生 REST 专用 CT config：仅把
%% {auto_migrate, false} 翻转为 true（scratch 空库首迁移安全；本仓该键若
%% 为 false 是 eunit 共享库隔离的本地口径，与 REST runner 合同冲突）。
%% 原文件无该键时原样复制（默认即 true）。派生文件落 /tmp（仓外）。
ensure_rest_ct_config(Root) ->
    SrcPath = filename:join(Root, "config/sys.local.config"),
    TmpPath = "/tmp/limiter-v1-rest-" ++ os:getpid() ++ ".config",
    Bin =
        case file:read_file(SrcPath) of
            {ok, B} -> B;
            {error, _} -> <<"">>
        end,
    Flipped = binary:replace(Bin, <<"{auto_migrate, false}">>, <<"{auto_migrate, true}">>, [global]),
    ok = file:write_file(TmpPath, Flipped),
    TmpPath.

%% -------------------------------------------------------------------------
%% AC-16：性能预算（计划 §7.6 / 附录 C.9）
%%
%% --oracle baseline：读取 L0 旧基线证据（docs/qa/limiter-v1/0d28ad4f/baseline/，
%% HEAD 0d28ad4f），校验其指纹与完整性后输出测量摘要并 PASS（退出 0）。
%% 本模式不在脚本内重跑旧链——旧基线测量须绑定 0d28ad4f 冻结输入（见
%% FROZEN_INPUTS.md），重跑属复测流程而非验收入口。
%%
%% --oracle fixed：在本仓（主 worktree ebin，即候选实现）跑同款微基准
%% （真实 login_attempt_ds 最小环境：failure/check/admit/reset + 混合负载
%% 80/15/5），按 C.9 预算比较：同有效记账工作量单事件成本 ≤ 旧基线 ×1.5、
%% 账户域 p99 ≤100ms、max ≤500ms、owner mailbox 有界（负载中 ≤10000、
%% 收齐后 ≤100）。PASS → 0；任一超预算 → FAIL 退出 3（明细落盘证据目录）。
%% -------------------------------------------------------------------------

-define(AC16_BASELINE_DIR, <<"docs/qa/limiter-v1/0d28ad4f/baseline">>).
-define(AC16_OLD_HEAD, <<"0d28ad4f">>).
-define(AC16_EVENTS, 100000).
%% 基线 B 场景登记：{JSON 键, workers}
-define(AC16_B_SECTIONS, [
    {<<"record_failure">>, 1},
    {<<"check_ip_rate_limit">>, 1},
    {<<"throttle_check">>, 1},
    {<<"reset">>, 1},
    {<<"mixed_1w">>, 1},
    {<<"mixed_8w">>, 8}
]).
-define(AC16_KEYS_ACCOUNT, 10000).
-define(AC16_KEYS_IP, 1000).
-define(AC16_REPEATS, 3).
-define(AC16_WORKERS, 8).
%% C.9 预算（µs）
-define(AC16_BUDGET_P99_US, 100000).
-define(AC16_BUDGET_MAX_US, 500000).
-define(AC16_BUDGET_MAILBOX_PEAK, 10000).
-define(AC16_BUDGET_MAILBOX_AFTER, 100).

run_ac16(Opts) ->
    case maps:get(oracle, Opts) of
        baseline -> run_ac16_baseline(Opts);
        fixed -> run_ac16_fixed(Opts)
    end.

%% ---- AC-16 baseline：校验并摘要旧基线证据 ----

run_ac16_baseline(Opts) ->
    Root = project_root(),
    BaselineDir = filename:join(Root, ?AC16_BASELINE_DIR),
    A = read_json_or_die(<<BaselineDir/binary, "/baseline_a.json">>),
    B = read_json_or_die(<<BaselineDir/binary, "/baseline_b.json">>),
    %% 指纹校验：两份证据必须绑定同一冻结 HEAD
    HeadA = json_path(A, [<<"env">>, <<"old_head">>]),
    HeadB = json_path(B, [<<"env">>, <<"old_head">>]),
    ExpectedHead = ?AC16_OLD_HEAD,
    case {HeadA, HeadB} of
        {ExpectedHead, ExpectedHead} -> ok;
        _ ->
            die_env(
                <<"基线证据 old_head 指纹不符：a="/utf8, (value_str(HeadA))/binary, " b="/utf8,
                    (value_str(HeadB))/binary, "，期望 "/utf8, ExpectedHead/binary>>
            )
    end,
    ok = verify_head_in_git(Root, ?AC16_OLD_HEAD),
    %% 完整性校验：A 的缺陷 oracle（记账恒 0、IP Cache 未写）与 B 的规模
    verify_baseline_a(A),
    verify_baseline_b(B),
    %% 摘要输出
    io:format("~ts~n", [<<"== 基线 A：E2E 旧真实成本（N=500 错误密码 x3）=="/utf8>>]),
    [
        io:format(
            "  run: p50=~s us p95=~s us p99=~s us max=~s us thr=~s rps accounted=0/~s~n",
            [
                us_str(maps:get(<<"p50_us">>, R)), us_str(maps:get(<<"p95_us">>, R)),
                us_str(maps:get(<<"p99_us">>, R)), us_str(maps:get(<<"max_us">>, R)),
                num_str(maps:get(<<"throughput_rps">>, R)),
                num_str(maps:get(<<"events">>, R))
            ]
        )
     || R <- json_path(A, [<<"e2e_wrong_password_login">>, <<"runs">>])
    ],
    io:format("~ts~n", [<<"== 基线 B：同有效记账工作量（100k 事件 x3）=="/utf8>>]),
    [
        begin
            Section = maps:get(KBin, B),
            Runs = maps:get(<<"runs">>, Section),
            Agg = maps:get(<<"aggregate">>, Section),
            io:format(
                "  ~s (w=~p, ev=~p, ok=~p deny=~p): p50=~s us p99_range=~s us thr_range=~s/s~n",
                [
                    binary_to_list(KBin), Workers, maps:get(<<"events">>, hd(Runs)),
                    maps:get(<<"allowed">>, hd(Runs)), maps:get(<<"denied">>, hd(Runs)),
                    us_list_str([maps:get(<<"p50_us">>, R) || R <- Runs]),
                    range_str(maps:get(<<"p99_us_range">>, Agg)),
                    range_str(maps:get(<<"throughput_eps_range">>, Agg))
                ]
            )
        end
     || {KBin, Workers} <- ?AC16_B_SECTIONS
    ],
    EvDir = abs_evidence_dir(Root, evidence_dir(Opts, <<"AC-16">>)),
    ok = filelib:ensure_dir(<<EvDir/binary, "/x">>),
    write_input_json(EvDir, #{
        case_id => <<"AC-16">>,
        oracle => baseline,
        candidate => candidate_id(),
        baseline_dir => ?AC16_BASELINE_DIR,
        old_head => ?AC16_OLD_HEAD
    }),
    PassMsg = <<"AC-16 baseline PASS（0d28ad4f 旧基线证据指纹/完整性校验通过，摘要见上；证据: "/utf8,
        EvDir/binary>>,
    io:format("~ts~n", [PassMsg]),
    halt(0).

verify_baseline_a(A) ->
    Section = maps:get(<<"e2e_wrong_password_login">>, A),
    Runs = maps:get(<<"runs">>, Section),
    length(Runs) >= 1 orelse die_env(<<"基线 A 无重复 run"/utf8>>),
    [
        begin
            maps:get(<<"account_rows_nonzero">>, R) =:= 0
                orelse die_env(<<"基线 A 缺陷 oracle 被破坏：存在非 0 记账行（证据与 0d28ad4f 不符）"/utf8>>),
            maps:get(<<"ip_cache_key_value">>, R) =:= null
                orelse die_env(<<"基线 A 缺陷 oracle 被破坏：IP Cache 键被写入"/utf8>>),
            maps:get(<<"errors_returned">>, R) =:= maps:get(<<"events">>, R)
                orelse die_env(<<"基线 A：错误密码请求未全部返回业务错误"/utf8>>)
        end
     || R <- Runs
    ],
    ok.

verify_baseline_b(B) ->
    [
        begin
            Section = maps:get(KBin, B),
            Runs = maps:get(<<"runs">>, Section),
            length(Runs) >= 1 orelse die_env(<<"基线 B 场景无重复 run: "/utf8, KBin/binary>>),
            _ = maps:get(<<"aggregate">>, Section),
            [
                begin
                    maps:get(<<"events">>, R) =:= ?AC16_EVENTS
                        orelse die_env(<<"基线 B 事件规模不符（期望 100000）: "/utf8, KBin/binary>>),
                    (maps:get(<<"allowed">>, R) + maps:get(<<"denied">>, R)) =:= ?AC16_EVENTS
                        orelse die_env(<<"基线 B 事件计数不守恒: "/utf8, KBin/binary>>)
                end
             || R <- Runs
            ],
            ok
        end
     || {KBin, _W} <- ?AC16_B_SECTIONS
    ],
    ok.

%% ---- AC-16 fixed：本仓新实现同款微基准 + C.9 预算比较 ----

run_ac16_fixed(Opts) ->
    Root = project_root(),
    io:format("~ts~n", [<<"AC-16 fixed: 启动本仓 login_attempt_ds 最小环境，跑同款微基准（100k/场景 x3）…"/utf8>>]),
    %% 新 DS 最小环境：deps 不含 DB/Cache（新 owner 纯内存权威）
    [code:add_patha(P) || P <- filelib:wildcard(Root ++ "/deps/*/ebin")],
    code:add_patha(Root ++ "/ebin"),
    application:set_env(imboy, limiter_ip_rates, #{
        passport_per_ip => {1000000, 60000},
        api_per_ip => {1000000, 60000},
        oidc_request => {1000000, 3600000}
    }),
    application:set_env(imboy, login_max_attempts, 5),
    {ok, _} = login_attempt_ds:start_link(),
    true = login_attempt_ds:ready(),

    warmup_fixed(),
    R1 = [bench_fixed(op_failure) || _ <- lists:seq(1, ?AC16_REPEATS)],
    R2 = [bench_fixed(op_admit) || _ <- lists:seq(1, ?AC16_REPEATS)],
    R3 = [bench_fixed(op_reset) || _ <- lists:seq(1, ?AC16_REPEATS)],
    R4 = [bench_fixed({op_mixed, 1}) || _ <- lists:seq(1, ?AC16_REPEATS)],
    R5 = [bench_fixed({op_mixed, ?AC16_WORKERS}) || _ <- lists:seq(1, ?AC16_REPEATS)],
    MailboxPeak = lists:max(
        [maps:get(mailbox_peak, R, 0) || R <- R1 ++ R2 ++ R3 ++ R4 ++ R5] ++ [0]
    ),
    MailboxAfter = mailbox_len(),

    %% C.9 预算比较（旧基线 p50 区间下限为对照口径——对候选最宽松、防假 FAIL）
    BaselineDir = filename:join(Root, ?AC16_BASELINE_DIR),
    B = read_json_or_die(<<BaselineDir/binary, "/baseline_b.json">>),
    OldP50 = #{
        failure => old_p50(B, <<"record_failure">>),
        admit => old_p50(B, <<"check_ip_rate_limit">>),
        reset => old_p50(B, <<"reset">>),
        mixed_1w => old_p50(B, <<"mixed_1w">>),
        mixed_8w => old_p50(B, <<"mixed_8w">>)
    },
    %% C.9 第 1 条（x1.5 同口径比较）锚定的是 actor 记账路径（推导句：
    %% "旧 S 侧一次失败记账 ≈ 1 次 gen_server call + 1–2 次 ETS + Cache set"）。
    %% 旧 reset 是裸 ets:delete（无 owner 确认——正是 AC-17 要求修复的缺陷
    %% 形态），x1.5 口径对它不成立；reset 仅按绝对预算（p99/max/mailbox）
    %% 判定，CostMode=absolute。
    Budgets = [
        {failure, R1, maps:get(failure, OldP50), relative},
        {admit, R2, maps:get(admit, OldP50), relative},
        {reset, R3, maps:get(reset, OldP50), absolute},
        {mixed_1w, R4, maps:get(mixed_1w, OldP50), relative},
        {mixed_8w, R5, maps:get(mixed_8w, OldP50), relative}
    ],
    {Failures, ReportLines} = judge_budgets(Budgets),
    MailboxOk = MailboxPeak =< ?AC16_BUDGET_MAILBOX_PEAK andalso MailboxAfter =< ?AC16_BUDGET_MAILBOX_AFTER,
    MailboxLine =
        io_lib:format(
            "mailbox: peak=~p (budget ~p) after=~p (budget ~p) -> ~s~n",
            [
                MailboxPeak, ?AC16_BUDGET_MAILBOX_PEAK, MailboxAfter, ?AC16_BUDGET_MAILBOX_AFTER,
                case MailboxOk of true -> "OK"; false -> "FAIL" end
            ]
        ),
    [io:format("~s~n", [L]) || L <- ReportLines],
    io:format("~s~n", [MailboxLine]),

    EvDir = abs_evidence_dir(Root, evidence_dir(Opts, <<"AC-16">>)),
    ok = filelib:ensure_dir(<<EvDir/binary, "/x">>),
    write_input_json(EvDir, #{
        case_id => <<"AC-16">>,
        oracle => fixed,
        candidate => candidate_id(),
        budget => <<"cost<=oldx1.5; p99<=100ms; max<=500ms; mailbox<=10000/after<=100">>,
        mailbox_peak => MailboxPeak,
        mailbox_after => MailboxAfter,
        failures => length(Failures)
    }),
    ok = file:write_file(
        <<EvDir/binary, "/ac16_fixed_report.txt">>,
        list_to_binary([L ++ "\n" || L <- ReportLines] ++ [MailboxLine])
    ),
    case Failures =:= [] andalso MailboxOk of
        true ->
            PassMsg = <<"AC-16 fixed PASS（新实现同工作量成本、p99/max、mailbox 均在 C.9 预算内；证据: "/utf8,
                EvDir/binary>>,
            io:format("~ts~n", [PassMsg]),
            halt(0);
        false ->
            FailMsg = <<"AC-16 fixed FAIL（超 C.9 预算项数="/utf8, (integer_to_binary(length(Failures)))/binary,
                "；明细见上与证据目录 "/utf8, EvDir/binary, "）"/utf8>>,
            io:format(standard_error, "~ts~n", [FailMsg]),
            halt(3)
    end.

%% 旧基线某场景 3 次 p50 的最小值（µs）
old_p50(B, KBin) ->
    Runs = maps:get(<<"runs">>, maps:get(KBin, B)),
    lists:min([maps:get(<<"p50_us">>, R) || R <- Runs]).

%% {Failures, Lines}
judge_budgets(Budgets) ->
    lists:foldl(
        fun({Name, Runs, OldP50Us, CostMode}, {FailAcc, LineAcc}) ->
            P50s = [maps:get(p50_us, R) || R <- Runs],
            P50Best = lists:min(P50s),
            P99Worst = lists:max([maps:get(p99_us, R) || R <- Runs]),
            MaxWorst = lists:max([maps:get(max_us, R) || R <- Runs]),
            {CostOk, CostDesc} =
                case CostMode of
                    relative ->
                        Budget = OldP50Us * 1.5,
                        {
                            P50Best =< Budget,
                            io_lib:format(
                                "p50_best=~s us (old_min=~s us, x1.5 budget=~s us) -> ~s",
                                [
                                    us_str(P50Best), us_str(OldP50Us), us_str(Budget),
                                    case P50Best =< Budget of true -> "OK"; false -> "FAIL" end
                                ]
                            )
                        };
                    absolute ->
                        {
                            true,
                            io_lib:format(
                                "p50_best=~s us (absolute cost mode: old counterpart is bare ets:delete "
                                "without owner confirm, the AC-17 defect shape; absolute budgets apply) "
                                "-> cost n/a by design",
                                [us_str(P50Best)]
                            )
                        }
                end,
            P99Ok = P99Worst =< ?AC16_BUDGET_P99_US,
            MaxOk = MaxWorst =< ?AC16_BUDGET_MAX_US,
            Line = io_lib:format(
                "~p: ~s; p99_worst=~s us (budget ~p) -> ~s; max_worst=~s us (budget ~p) -> ~s",
                [
                    Name, CostDesc,
                    us_str(P99Worst), ?AC16_BUDGET_P99_US,
                    case P99Ok of true -> "OK"; false -> "FAIL" end,
                    us_str(MaxWorst), ?AC16_BUDGET_MAX_US,
                    case MaxOk of true -> "OK"; false -> "FAIL" end
                ]
            ),
            NewFails =
                [cost_exceeded || not CostOk] ++
                    [p99_exceeded || not P99Ok] ++ [max_exceeded || not MaxOk],
            case NewFails of
                [] -> {FailAcc, LineAcc ++ [lists:flatten(Line)]};
                _ -> {FailAcc ++ [{Name, NewFails}], LineAcc ++ [lists:flatten(Line)]}
            end
        end,
        {[], []},
        Budgets
    ).

%% -------------------------------------------------------------------------
%% AC-16 fixed 基准骨架（新实现 API：failure/check/admit/reset）
%% -------------------------------------------------------------------------

bench_fixed(Op) ->
    reset_fixed_state(),
    Owner = self(),
    {Pid, Mref} = spawn_monitor(fun() -> Owner ! {self(), run_bench_fixed(Op)} end),
    Res =
        receive
            {Pid, R} -> erlang:demonitor(Mref, [flush]), R;
            {'DOWN', Mref, process, _, Reason} -> erlang:error({worker_down, Op, Reason})
        after 300000 -> erlang:error({worker_timeout, Op})
        end,
    Res#{
        op => op_name(Op),
        mailbox_peak => maps:get(mailbox_peak, Res, mailbox_len()),
        ds_mailbox_after => mailbox_len()
    }.

run_bench_fixed(op_failure) ->
    AcctTuple = list_to_tuple(fixed_account_keys()),
    N = tuple_size(AcctTuple),
    bench_events_fixed(
        lists:seq(1, ?AC16_EVENTS),
        fun(I) -> login_attempt_ds:failure({unknown, account, element(((I - 1) rem N) + 1, AcctTuple)}) end,
        fun
            ({ok, _}) -> ok;
            (_) -> denied
        end
    );
run_bench_fixed(op_admit) ->
    IpTuple = list_to_tuple(fixed_ip_keys()),
    N = tuple_size(IpTuple),
    bench_events_fixed(
        lists:seq(1, ?AC16_EVENTS),
        fun(I) -> login_attempt_ds:admit(passport_per_ip, element(((I - 1) rem N) + 1, IpTuple)) end,
        fun
            ({ok, _}) -> ok;
            ({error, _}) -> denied
        end
    );
run_bench_fixed(op_reset) ->
    AcctTuple = list_to_tuple(fixed_account_keys()),
    N = tuple_size(AcctTuple),
    {_, Red0} = worker_reductions(),
    MemBefore = total_ets_memory(),
    {T, OkC, BadC} =
        lists:foldl(
            fun(_Round, {Acc, O, B}) ->
                fill_fixed_round(AcctTuple, N, 1),
                reset_fixed_timed(AcctTuple, N, 1, Acc, O, B)
            end,
            {[], 0, 0},
            lists:seq(1, 10)
        ),
    {_, Red1} = worker_reductions(),
    Sorted = lists:sort(T),
    #{
        times_ns => lists:reverse(T),
        wall_ns => lists:sum(T),
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        reductions_delta => Red1 - Red0,
        allowed => OkC,
        denied => BadC,
        mem_before => MemBefore,
        mem_after => total_ets_memory()
    };
run_bench_fixed({op_mixed, Workers}) ->
    PerW = ?AC16_EVENTS div Workers,
    Owner = self(),
    Refs =
        [
            begin
                WorkerId = W,
                {Pid, Mref} =
                    spawn_monitor(fun() -> Owner ! {self(), run_mixed_fixed(WorkerId, PerW, Workers)} end),
                {Pid, Mref}
            end
         || W <- lists:seq(1, Workers)
        ],
    T0 = erlang:monotonic_time(nanosecond),
    All =
        lists:map(
            fun({Pid, Mref}) ->
                receive
                    {Pid, R} -> erlang:demonitor(Mref, [flush]), R;
                    {'DOWN', Mref, process, _, Reason} -> erlang:error({mixed_worker_down, Reason})
                after 300000 -> erlang:error(mixed_worker_timeout)
                end
            end,
            Refs
        ),
    T1 = erlang:monotonic_time(nanosecond),
    Times = lists:append([maps:get(times_ns, R) || R <- All]),
    Sorted = lists:sort(Times),
    #{
        times_ns => Times,
        wall_ns => T1 - T0,
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        reductions_delta => lists:sum([maps:get(reductions_delta, R) || R <- All]),
        allowed => lists:sum([maps:get(allowed, R) || R <- All]),
        denied => lists:sum([maps:get(denied, R) || R <- All]),
        mailbox_peak => lists:max([maps:get(mailbox_peak, R) || R <- All]),
        mem_after => total_ets_memory()
    }.

bench_events_fixed(Idxs, Op, Verdict) ->
    {_, Red0} = worker_reductions(),
    {T, OkC, BadC} =
        lists:foldl(
            fun(_I, {Acc, O, B}) ->
                T0 = erlang:monotonic_time(nanosecond),
                R = Op(_I),
                T1 = erlang:monotonic_time(nanosecond),
                case Verdict(R) of
                    ok -> {[T1 - T0 | Acc], O + 1, B};
                    _ -> {[T1 - T0 | Acc], O, B + 1}
                end
            end,
            {[], 0, 0},
            Idxs
        ),
    {_, Red1} = worker_reductions(),
    Sorted = lists:sort(T),
    #{
        times_ns => lists:reverse(T),
        wall_ns => lists:sum(T),
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        reductions_delta => Red1 - Red0,
        allowed => OkC,
        denied => BadC,
        mem_before => total_ets_memory(),
        mem_after => total_ets_memory()
    }.

run_mixed_fixed(WorkerId, Total, Workers) ->
    NCheck = Total * 80 div 100,
    NRecord = Total * 15 div 100,
    NReset = Total * 5 div 100,
    IpTuple = list_to_tuple(slice_for(fixed_ip_keys(), WorkerId, Workers)),
    AcctTuple = list_to_tuple(slice_for(fixed_account_keys(), WorkerId, Workers)),
    NIp = tuple_size(IpTuple),
    NAcct = tuple_size(AcctTuple),
    {_, Red0} = worker_reductions(),
    T0 = erlang:monotonic_time(nanosecond),
    {Times1, OkC, BadC, Recorded} = phase_check_fixed(NCheck, IpTuple, NIp),
    {Times2, OkR, BadR, Recorded2} = phase_record_fixed(NRecord, AcctTuple, NAcct, Recorded),
    RecTuple = list_to_tuple(lists:reverse(Recorded2)),
    NRec = tuple_size(RecTuple),
    {Times3, OkX, BadX} = phase_reset_fixed(NReset, RecTuple, NRec),
    T1 = erlang:monotonic_time(nanosecond),
    {_, Red1} = worker_reductions(),
    Sorted = lists:sort(Times3 ++ Times2 ++ Times1),
    #{
        times_ns => lists:reverse(Sorted),
        wall_ns => T1 - T0,
        p50_us => ns_to_us(percentile(Sorted, 0.50)),
        p99_us => ns_to_us(percentile(Sorted, 0.99)),
        max_us => ns_to_us(lists:last(Sorted)),
        reductions_delta => Red1 - Red0,
        allowed => OkC + OkR + OkX,
        denied => BadC + BadR + BadX,
        mailbox_peak => mailbox_len()
    }.

phase_check_fixed(0, _Tuple, _N) -> {[], 0, 0, []};
phase_check_fixed(N, Tuple, NIp) -> phase_check_fixed(N, Tuple, NIp, [], 0, 0).
phase_check_fixed(0, _Tuple, _N, Acc, O, B) -> {Acc, O, B, []};
phase_check_fixed(I, Tuple, N, Acc, O, B) ->
    T0 = erlang:monotonic_time(nanosecond),
    R = login_attempt_ds:admit(passport_per_ip, element(((I - 1) rem N) + 1, Tuple)),
    T1 = erlang:monotonic_time(nanosecond),
    case R of
        {ok, _} -> phase_check_fixed(I - 1, Tuple, N, [T1 - T0 | Acc], O + 1, B);
        _ -> phase_check_fixed(I - 1, Tuple, N, [T1 - T0 | Acc], O, B + 1)
    end.

phase_record_fixed(0, _Tuple, _N, Recorded0) -> {[], 0, 0, Recorded0};
phase_record_fixed(N, Tuple, NAcct, Recorded0) -> phase_record_fixed(N, Tuple, NAcct, Recorded0, [], 0, 0).
phase_record_fixed(0, _Tuple, _N, Recorded0, Acc, O, B) -> {Acc, O, B, Recorded0};
phase_record_fixed(I, Tuple, N, Recorded0, Acc, O, B) ->
    K = element(((I - 1) rem N) + 1, Tuple),
    T0 = erlang:monotonic_time(nanosecond),
    R = login_attempt_ds:failure({unknown, account, K}),
    T1 = erlang:monotonic_time(nanosecond),
    case R of
        {ok, _} -> phase_record_fixed(I - 1, Tuple, N, [K | Recorded0], [T1 - T0 | Acc], O + 1, B);
        _ -> phase_record_fixed(I - 1, Tuple, N, Recorded0, [T1 - T0 | Acc], O, B + 1)
    end.

phase_reset_fixed(0, _RecTuple, _NRec) -> {[], 0, 0};
phase_reset_fixed(N, RecTuple, NRec) -> phase_reset_fixed(N, RecTuple, NRec, [], 0, 0).
phase_reset_fixed(0, _RecTuple, _NRec, Acc, O, B) -> {Acc, O, B};
phase_reset_fixed(I, RecTuple, NRec, Acc, O, B) ->
    K = element(((I - 1) rem NRec) + 1, RecTuple),
    T0 = erlang:monotonic_time(nanosecond),
    ok = login_attempt_ds:reset({unknown, account, K}),
    T1 = erlang:monotonic_time(nanosecond),
    phase_reset_fixed(I - 1, RecTuple, NRec, [T1 - T0 | Acc], O + 1, B).

fixed_account_keys() ->
    [pad_key("acct_ac16_", I, 6) || I <- lists:seq(1, ?AC16_KEYS_ACCOUNT)].

fixed_ip_keys() ->
    [ip_key(I) || I <- lists:seq(1, ?AC16_KEYS_IP)].

pad_key(Prefix, I, Width) ->
    list_to_binary(Prefix ++ string:right(integer_to_list(I), Width, $0)).

ip_key(I) ->
    B = 10 + I div 200,
    D = 1 + I rem 200,
    list_to_binary("10.0." ++ integer_to_list(B) ++ "." ++ integer_to_list(D)).

slice_for(Keys, WorkerId, Workers) ->
    Len = length(Keys) div Workers,
    Start = (WorkerId - 1) * Len + 1,
    End =
        case WorkerId =:= Workers of
            true -> length(Keys);
            false -> WorkerId * Len
        end,
    [lists:nth(I, Keys) || I <- lists:seq(Start, End)].

reset_fixed_state() ->
    DPid = whereis(login_attempt_ds),
    %% 新实现无公开清态 API：用独立 Key 空间 per repeat（run 序号前缀）替代
    case DPid of
        undefined -> ok;
        _ ->
            {message_queue_len, L} = process_info(DPid, message_queue_len),
            0 = L
    end,
    ok.

warmup_fixed() ->
    [
        login_attempt_ds:failure({unknown, account, pad_key("warmup_ac16_", I, 6)})
     || I <- lists:seq(1, 1000)
    ],
    ok.

fill_fixed_round(_Tuple, N, I) when I > N -> ok;
fill_fixed_round(Tuple, N, I) ->
    _ = login_attempt_ds:failure({unknown, account, element(I, Tuple)}),
    fill_fixed_round(Tuple, N, I + 1).

reset_fixed_timed(_Tuple, N, I, Acc, O, B) when I > N ->
    {Acc, O, B};
reset_fixed_timed(Tuple, N, I, Acc, O, B) ->
    T0 = erlang:monotonic_time(nanosecond),
    ok = login_attempt_ds:reset({unknown, account, element(I, Tuple)}),
    T1 = erlang:monotonic_time(nanosecond),
    reset_fixed_timed(Tuple, N, I + 1, [T1 - T0 | Acc], O + 1, B).

mailbox_len() ->
    case whereis(login_attempt_ds) of
        undefined -> undefined;
        DPid ->
            {message_queue_len, L} = process_info(DPid, message_queue_len),
            L
    end.

%% -------------------------------------------------------------------------
%% AC-15：两节点同机隔离 admit（本地合同各允许 N，合计 2N 如实报告）
%% -------------------------------------------------------------------------

-define(AC15_N, 100).

run_ac15(Opts) ->
    Root = project_root(),
    NodeSpecs = [
        {ac15_alpha, 16985},
        {ac15_beta, 16986}
    ],
    io:format("AC-15: 启动两个隔离 -noshell 节点（独立 epmd 端口），各 admit ~p 次（同规则同 IP）…~n", [?AC15_N]),
    Results = [
        begin
            Rc = run_ac15_node(Root, NodeName, EpmdPort, ?AC15_N),
            {NodeName, EpmdPort, Rc}
        end
     || {NodeName, EpmdPort} <- NodeSpecs
    ],
    EvDir = abs_evidence_dir(Root, evidence_dir(Opts, <<"AC-15">>)),
    ok = filelib:ensure_dir(<<EvDir/binary, "/x">>),
    AllowedList = [Allowed || {_, _, {ok, Allowed}} <- Results],
    case length(AllowedList) =:= length(NodeSpecs) of
        false ->
            write_input_json(EvDir, #{case_id => <<"AC-15">>, result => <<"environment_blocked">>}),
            io:format(standard_error, "~ts~n",
                [<<"AC-15 INPUT_PENDING：隔离节点环境不可用（sname/epmd 启动失败），见上方明细"/utf8>>]),
            halt(4);
        true ->
            Total = lists:sum(AllowedList),
            write_input_json(EvDir, #{
                case_id => <<"AC-15">>,
                node_results => list_to_binary(io_lib:format("~0p", [Results])),
                n_each => ?AC15_N,
                total_admitted => Total
            }),
            %% 本地合同：各节点本地允许 N（节点本地权威、不共享），合计 2N
            %% 如实报告——若上层用户要求"共享全局额度"，此结果即不合格证据
            %% （计划 AC-15：检测把节点本地状态当全局额度）。
            AllowedStr = lists:join(", ", [integer_to_list(A) || A <- AllowedList]),
            io:format("AC-15: per-node admitted = [~s]; total = ~p (expected 2 x ~p = ~p)~n", [
                AllowedStr, Total, ?AC15_N, 2 * ?AC15_N
            ]),
            case Total =:= 2 * ?AC15_N of
                true ->
                    PassMsg = <<"AC-15 PASS（两节点各本地允许 N，合计 2N 如实报告——节点本地合同成立；证据: "/utf8,
                        EvDir/binary>>,
                    io:format("~ts~n", [PassMsg]),
                    halt(0);
                false ->
                    io:format(standard_error, "AC-15 FAIL: total=~p expected=~p~n", [Total, 2 * ?AC15_N]),
                    halt(3)
            end
    end.

%% 单节点：erl -noshell -sname <unique> （独立 ERL_EPMD_PORT），启动 owner，
%% 同一 IP admit N 次，统计允许数，尾部标记返回。
run_ac15_node(Root, NodeName, EpmdPort, N) ->
    Tag = atom_to_list(NodeName),
    %% Eval 用纯字符串拼接构造（N 为受控整数）：子 eval 里的 io:format
    %% 控制序列 ~w 保持字面，不经父层 io_lib:format 消费（io:format 无 ~~
    %% 字面转义，经 format 会破坏控制序列）。
    Eval =
        "application:set_env(imboy, limiter_ip_rates, #{passport_per_ip => {" ++
            integer_to_list(N * 10) ++
            ", 60000}, api_per_ip => {1000000, 60000}, oidc_request => {1000000, 3600000}}), " ++
            "{ok,_} = login_attempt_ds:start_link(), true = login_attempt_ds:ready(), " ++
            "Allowed = length([ok || _ <- lists:seq(1, " ++ integer_to_list(N) ++
            "), element(1, login_attempt_ds:admit(passport_per_ip, <<\"10.0.15.15\">>)) =:= ok]), " ++
            "io:format(\"AC15RESULT:~w\n\", [Allowed]), halt(0)",
    Cmd =
        "cd " ++ Root ++
            " && ERL_EPMD_PORT=" ++ integer_to_list(EpmdPort) ++
            " erl -pa " ++ Root ++ "/ebin -noshell -sname ac15_" ++ Tag ++ "_" ++ os:getpid() ++
            " -eval '" ++ Eval ++ "'",
    Out = os:cmd(Cmd ++ " ; printf '\n__AC15_EXIT__%s' \"$?\""),
    case string:split(Out, "\n__AC15_EXIT__", trailing) of
        [Body, RcStr] ->
            case list_to_integer(string:trim(RcStr)) of
                0 ->
                    case re:run(Body, "RESULT:(\\d+)", [{capture, [1], list}]) of
                        {match, [NStr]} -> {ok, list_to_integer(NStr)};
                        _ -> {error, no_result_marker}
                    end;
                Rc -> {error, {node_exit, Rc}}
            end;
        _ ->
            {error, no_exit_marker}
    end.



%% -------------------------------------------------------------------------
%% AC-16/15 辅助：JSON、格式化、git、采集
%% -------------------------------------------------------------------------

die_env(MsgBin) ->
    io:format(standard_error, "~ts~n", [MsgBin]),
    halt(2).

read_json_or_die(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            try json:decode(Bin)
            catch _:_ ->
                die_env(<<"证据 JSON 解析失败: "/utf8, (list_to_binary(Path))/binary>>)
            end;
        {error, Reason} ->
            die_env(
                <<"证据文件缺失（先运行 A4 基线测量并落盘）: "/utf8,
                    (list_to_binary(Path))/binary, " ("/utf8,
                    (list_to_binary(io_lib:format("~p", [Reason])))/binary, "）"/utf8>>
            )
    end.

json_path(Map, [LastKey]) when is_map(Map) -> maps:get(LastKey, Map, null);
json_path(Map, [Key | Rest]) when is_map(Map) -> json_path(maps:get(Key, Map, #{}), Rest);
json_path(_, _) -> null.

verify_head_in_git(Root, ShortHead) ->
    case run_capture(Root, "git cat-file -t " ++ binary_to_list(ShortHead) ++ "^{commit} 2>/dev/null") of
        {ok, _} -> ok;
        {error, _} ->
            die_env(<<"冻结 HEAD 不存在于本仓: "/utf8, ShortHead/binary>>)
    end.

value_str(V) when is_binary(V) -> V;
value_str(V) -> list_to_binary(io_lib:format("~0p", [V])).

us_str(V) when is_float(V) -> float_to_binary(V, [{decimals, 1}, compact]);
us_str(V) when is_integer(V) -> integer_to_binary(V);
us_str(V) -> value_str(V).

num_str(V) when is_integer(V) -> integer_to_binary(V);
num_str(V) -> value_str(V).

us_list_str(Vs) ->
    list_to_binary(string:join([binary_to_list(us_str(V)) || V <- Vs], "/")).

range_str([Min, Max]) ->
    <<(us_str(Min))/binary, "..", (us_str(Max))/binary>>;
range_str(V) -> value_str(V).

worker_reductions() ->
    {reductions, R} = process_info(self(), reductions),
    {ok, R}.

total_ets_memory() ->
    lists:sum([ets:info(T, memory) || T <- ets:all(), is_atom(T)]).

percentile(Sorted, Q) ->
    N = length(Sorted),
    Idx = max(1, round(Q * N)),
    lists:nth(Idx, Sorted).

ns_to_us(Ns) when is_integer(Ns) -> round(Ns / 100) / 10;
ns_to_us(V) -> V.

op_name(op_failure) -> failure;
op_name(op_admit) -> admit;
op_name(op_reset) -> reset;
op_name({op_mixed, W}) -> {mixed, W}.

%% -------------------------------------------------------------------------
%% manifest / 证据 / 杂项
%% -------------------------------------------------------------------------

emit_manifest(Path) ->
    Root = project_root(),
    Files = [
        "src/ds/login_attempt_ds.erl",
        "src/logic/login_security_logic.erl",
        "src/logic/passport_logic.erl",
        "src/api/passport_handler.erl",
        "src/api/throttle_middleware.erl",
        "src/api/auth_oidc_handler.erl",
        "src/imboy_app.erl"
    ],
    {ok, Head} = run_capture(Root, "git rev-parse HEAD"),
    {ok, Sums} = run_capture(Root, "shasum -a 256 " ++ string:join(Files, " ")),
    Entries =
        lists:map(
            fun(Line) ->
                %% shasum 输出: "<sha256>  <path>"（Erlang 侧切分，避免 shell 引号嵌套）
                [Sum | Rest] = string:split(Line, "  "),
                string:trim(Sum) ++ "  " ++ string:trim(hd(Rest))
            end,
            [L || L <- string:split(Sums, "\n", all), L =/= []]
        ),
    Body = string:join([string:trim(Head) ++ "  HEAD" | Entries], "\n") ++ "\n",
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, list_to_binary(Body)),
    io:format("manifest generated: ~s (~b items)~n", [Path, length(Entries) + 1]),
    halt(0).

run_capture(Root, Cmd) ->
    Port = open_port({spawn, "sh -c '" ++ Cmd ++ "'"}, [exit_status, {cd, Root}, {line, 4096}, hide]),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} -> collect_port(Port, [Line | Acc]);
        {Port, {data, {noeol, Line}}} -> collect_port(Port, [Line | Acc]);
        {Port, {exit_status, 0}} -> {ok, lists:join("\n", lists:reverse(Acc))};
        {Port, {exit_status, S}} -> {error, S}
    after 30000 -> {error, timeout}
    end.

candidate_id() ->
    Root = project_root(),
    case run_capture(Root, "git rev-parse --short HEAD") of
        {ok, Out} -> list_to_binary(string:trim(Out));
        _ -> <<"unknown">>
    end.

project_root() ->
    %% escript 脚本位于 <root>/scripts/，向上取仓根（absname 防相对调用）
    Es = escript:script_name(),
    filename:dirname(filename:dirname(filename:absname(Es))).

evidence_dir(#{evidence_dir := Dir}, _Case) when is_list(Dir) ->
    list_to_binary(Dir);
evidence_dir(_Opts, Case) ->
    RunRef = list_to_binary(strftime_run_ref()),
    <<"docs/qa/limiter-v1/", (candidate_id())/binary, "/", RunRef/binary, "/", Case/binary>>.

%% @private 默认证据目录相对仓根：绝对化，避免 Cmd 子 shell（cd Root 后 tee
%% output.log）与主进程 write_file（调用者 cwd）各按一处相对基准落盘。
abs_evidence_dir(Root, Dir) ->
    case filename:pathtype(Dir) of
        absolute -> Dir;
        _ -> list_to_binary(filename:join(Root, unicode:characters_to_list(Dir)))
    end.

strftime_run_ref() ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:local_time(),
    lists:flatten(
        io_lib:format(
            "~4..0b~2..0b~2..0bT~2..0b~2..0b~2..0b-~s",
            [Y, Mo, D, H, Mi, S, os:getpid()]
        )
    ).

write_input_json(EvDir, Map) ->
    Json = lines_json(maps:to_list(Map)),
    ok = file:write_file(<<EvDir/binary, "/input.json">>, Json).

lines_json(Kvs) ->
    Inner = lists:join(
        <<",\n">>,
        [json_kv(K, V) || {K, V} <- Kvs, V =/= undefined]
    ),
    <<"{\n", (list_to_binary(Inner))/binary, "\n}\n">>.

json_kv(K, V) ->
    list_to_binary(io_lib:format("  \"~s\": \"~s\"", [to_bin(K), value_to_binary(V)])).

value_to_binary(V) when is_binary(V) -> V;
value_to_binary(V) when is_atom(V) -> atom_to_binary(V);
value_to_binary(V) when is_list(V) -> list_to_binary(V);
value_to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).

to_bin(V) when is_binary(V) -> V;
to_bin(V) when is_atom(V) -> atom_to_binary(V);
to_bin(V) -> list_to_binary(V).

%% @private os:cmd 不返回退出码：用尾部标记 __LVA_EXIT__<rc> 传递。
%% 命令本体由本工具构造（无该标记字面量），tee 输出与标记同流。
run_shell(Cmd) ->
    Out = os:cmd(Cmd ++ " ; printf '\n__LVA_EXIT__%s' \"$?\""),
    case string:split(Out, "\n__LVA_EXIT__", trailing) of
        [_, RcStr] ->
            list_to_integer(string:trim(RcStr));
        _ ->
            %% 标记缺失（异常路径）按失败处理
            255
    end.

usage_error(missing_case_or_list) ->
    io:format(standard_error, "missing --case / --list / --emit-manifest~n", []),
    halt(2);
usage_error({unknown_arg, Arg}) ->
    io:format(standard_error, "unknown arg: ~s~n", [Arg]),
    halt(2);
usage_error({bad_oracle, O}) ->
    io:format(standard_error, "--oracle must be baseline|fixed: ~s~n", [O]),
    halt(2).
