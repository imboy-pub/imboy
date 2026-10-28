%% IMBOY_ERLANG_QUALITY P1-03：命令许可门与退出码协议（纯 OTP）。
%% 上下文来自可信 STATE/owner 读取，命令行参数不能抬权（PAR24）。
-module(imboy_quality_cli).
-export([permit/2, exit_code/1, describe/0, perf_verdict/1, perf_verdict/2]).

%% EUNIT_120S：完整 U 全量目标（整条外部命令墙钟，毫秒）
-define(FULL_TARGET_MS, 120000).

%% PERF04：pilot 墙钟与 120s 合同的判定。输入必须是外部整命令时钟的整数毫秒
%% （prep/timer.escript 的 wall_ms），严格比较不四舍五入：120000 通过、120001 不通过；
%% 非整数输入直接 function_clause（不允许浮点四舍五入冒充）。
perf_verdict(WallMs) ->
    perf_verdict(WallMs, ?FULL_TARGET_MS).

perf_verdict(WallMs, TargetMs) when
    is_integer(WallMs), WallMs >= 0, is_integer(TargetMs), TargetMs > 0
->
    Within = WallMs =< TargetMs,
    #{
        wall_ms => WallMs,
        target_ms => TargetMs,
        verdict =>
            case Within of
                true -> within_target;
                false -> over_target
            end,
        label =>
            case Within of
                true -> <<"PILOT_WITHIN_TARGET">>;
                false -> <<"PILOT_OVER_TARGET">>
            end
    }.

%% Operation 分级（FRAMEWORK_DESIGN §2）：
%%  - 任何模式：inspect, report, selftest(合成)
%%  - LIMITED_PARALLEL：admission_check_only, prepare_selftest
%%  - INTEGRATED_EXECUTION：guard；eunit_group/eunit_full/baseline 另需 G-HEAVY
%%  - goal 含 P2 才有：pbt, ct, replay, certify（goal=PHASE1_COMPLETE 一律拒绝）
permit(
    #{
        task_id := <<"IMBOY_ERLANG_QUALITY">>,
        execution_mode := Mode,
        execution_goal := Goal,
        owner_verified := true
    } = Ctx,
    Operation
) ->
    case lists:member(Mode, [limited_parallel, integrated_execution]) of
        true -> permit_mode(Mode, Goal, Ctx, Operation);
        false -> {error, {unknown_execution_mode, Mode}}
    end;
permit(_, _) ->
    {error, missing_or_untrusted_task_context}.

permit_mode(limited_parallel, _, _, inspect) ->
    ok;
permit_mode(limited_parallel, _, _, report) ->
    ok;
permit_mode(limited_parallel, _, _, synthetic_selftest) ->
    ok;
permit_mode(limited_parallel, _, _, admission_check_only) ->
    ok;
permit_mode(limited_parallel, _, {prepare_selftest, Manifest}, prepare_selftest) ->
    case is_synthetic_manifest(Manifest) of
        true -> ok;
        false -> {error, non_synthetic_input}
    end;
permit_mode(limited_parallel, _, _, _) ->
    {error, operation_not_admitted_in_limited_parallel};
permit_mode(integrated_execution, _, _, synthetic_selftest) ->
    ok;
permit_mode(integrated_execution, _, _, inspect) ->
    ok;
permit_mode(integrated_execution, _, _, report) ->
    ok;
permit_mode(integrated_execution, _, _, guard) ->
    ok;
permit_mode(integrated_execution, _, Ctx, eunit_group) ->
    heavy_gate(Ctx, eunit_group);
permit_mode(integrated_execution, _, Ctx, eunit_full) ->
    heavy_gate(Ctx, eunit_full);
permit_mode(integrated_execution, _, Ctx, baseline) ->
    heavy_gate(Ctx, baseline);
permit_mode(integrated_execution, all_phases, _, pbt) ->
    ok;
permit_mode(integrated_execution, all_phases, _, ct) ->
    ok;
permit_mode(integrated_execution, all_phases, _, replay) ->
    ok;
permit_mode(integrated_execution, all_phases, _, certify) ->
    {error, g_cert_required};
permit_mode(integrated_execution, _, _, pbt) ->
    {error, phase2_not_authorized};
permit_mode(integrated_execution, _, _, ct) ->
    {error, phase2_not_authorized};
permit_mode(integrated_execution, _, _, replay) ->
    {error, phase2_not_authorized};
permit_mode(integrated_execution, _, _, certify) ->
    {error, phase2_not_authorized};
permit_mode(integrated_execution, _, _, _) ->
    {error, unknown_operation}.

heavy_gate(#{heavy_admitted := true}, _) -> ok;
heavy_gate(_, Op) -> {error, {g_heavy_not_admitted, Op}}.

is_synthetic_manifest(Manifest) when is_list(Manifest), Manifest =/= [] ->
    lists:all(
        fun
            (#{scope := synthetic, module := M, owned := true}) when is_atom(M) -> true;
            (_) -> false
        end,
        Manifest
    );
is_synthetic_manifest(_) ->
    false.

%% 退出码协议（FRAMEWORK_DESIGN §2.1）
exit_code(ok) -> 0;
exit_code({ok, _}) -> 0;
exit_code(test_failed) -> 1;
exit_code({test_failed, _}) -> 1;
exit_code({error, input_or_tool}) -> 2;
exit_code({error, {input_or_tool, _}}) -> 2;
exit_code({error, inventory_incomplete}) -> 3;
exit_code({error, {inventory_incomplete, _}}) -> 3;
exit_code(performance_not_met) -> 4;
exit_code({performance_not_met, _}) -> 4;
exit_code({error, timeout_or_cancelled}) -> 5;
exit_code({error, {timeout_or_cancelled, _}}) -> 5;
exit_code({error, blocked_or_deferred}) -> 6;
exit_code({error, {blocked_or_deferred, _}}) -> 6;
exit_code({error, denied}) -> 2;
exit_code({error, _}) -> 2;
exit_code(_) -> 2.

describe() ->
    #{
        vsn => 1,
        modes => [limited_parallel, integrated_execution],
        exit_codes => #{
            0 => all_promised_obligations_pass,
            1 => test_failed,
            2 => input_tool_permission,
            3 => inventory_incomplete,
            4 => performance_not_met,
            5 => timeout_or_cancelled,
            6 => blocked_or_deferred
        }
    }.
