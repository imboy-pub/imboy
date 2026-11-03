%% IMBOY_ERLANG_QUALITY P2-01：PropEr 执行底座（manifest、预算、真实计数、
%% 强制见证、反例捕获）。设计合同 FRAMEWORK_DESIGN §8：
%%  - quickcheck true 只是必要条件；完成事件必须恰 1 次（proper.erl:2239
%%    "OK: Passed ~b test(s).~n" 为 v1.5.0@5640d071 字面量，版本变化须重验）
%%  - 固定见证先执行，失败即拒（random_started=0，PBT05）
%%  - 反例同进程 proper:counterexample() 取完整 term（REPLAY 材料基础）
%%  - 不设 numworkers；并行由外部池控制
-module(imboy_quality_pbt).
-export([
    load_manifest/1,
    validate_manifest/1,
    profile_numtests/2,
    run/3,
    run_property/2,
    run_property_in/2,
    run_property_peer/3,
    count_completions/1,
    completion_verdict/4
]).

-define(KNOWN_PROFILES, [quick, nightly]).
-define(COMPLETION_FORMAT, "OK: Passed ~b test(s).~n").

%% CONTRACT.term pbt：quick ordinary=200 sequence=100；nightly 2000/500。
profile_numtests(quick, ordinary) -> 200;
profile_numtests(quick, sequence) -> 100;
profile_numtests(nightly, ordinary) -> 2000;
profile_numtests(nightly, sequence) -> 500.

%% ---- manifest（PBT02：启动前拒绝，不默认全部掩盖误配置） ----

load_manifest(Path) ->
    case file:consult(Path) of
        {ok, [Manifest]} when is_map(Manifest) -> validate_manifest(Manifest);
        {ok, _} -> {error, manifest_not_single_map};
        {error, _} = E -> E
    end.

validate_manifest(#{properties := Props} = M) when is_list(Props), Props =/= [] ->
    Ids = [maps:get(id, P, undefined) || P <- Props],
    case [E || P <- Props, E <- [prop_error(P)], E =/= ok] of
        [] ->
            case length(lists:usort(Ids)) =:= length(Ids) of
                true -> {ok, M};
                false -> {error, duplicate_property_ids}
            end;
        Errors ->
            {error, Errors}
    end;
validate_manifest(#{properties := []}) -> {error, empty_manifest};
validate_manifest(_) -> {error, manifest_missing_properties}.

prop_error(P) when is_map(P) ->
    checks(
        [
            {id, fun is_atom/1},
            {kind, fun(K) -> lists:member(K, [ordinary, sequence]) end},
            {mfa, fun mfa0/1},
            {witnesses, fun witness_list/1}
        ],
        P
    );
prop_error(_) ->
    {invalid_property, not_a_map}.

checks(Checks, P) ->
    Bad =
        lists:foldl(
            fun({K, Valid}, Acc) ->
                X = maps:get(K, P, undefined),
                case Valid(X) of
                    true -> Acc;
                    Reason -> [{K, X, Reason} | Acc]
                end
            end,
            [],
            Checks
        ),
    case Bad of
        [] -> ok;
        _ -> {invalid_fields, lists:reverse(Bad)}
    end.

mfa0({M, F, 0}) when is_atom(M), is_atom(F) -> true;
mfa0(_) -> mfa_arity0_expected.

witness_list(Ws) when is_list(Ws), Ws =/= [] ->
    case lists:all(fun witness_mfa/1, Ws) of
        true -> true;
        false -> invalid_witness_mfa
    end;
witness_list(_) -> witnesses_must_be_nonempty_mfa_list.

witness_mfa({M, F, A}) when is_atom(M), is_atom(F), is_integer(A), A >= 0 -> true;
witness_mfa(_) -> false.

%% ---- 执行 ----

%% run/3：串行逐性质（peer 由外层调度时用 run_property_peer）。
run(Manifest, Profile, _Opts) when is_atom(Profile) ->
    case lists:member(Profile, ?KNOWN_PROFILES) of
        false ->
            {error, {unknown_profile, Profile}};
        true ->
            Props = maps:get(properties, Manifest),
            Results = [run_property(P, Profile) || P <- Props],
            {ok, aggregate(Results)}
    end.

aggregate(Results) ->
    #{
        total => length(Results),
        passed => length([R || R <- Results, maps:get(verdict, R) =:= pass]),
        results => Results
    }.

%% run_property/2：见证门 → 随机 quickcheck（捕获 on_output 原始 Format/Args）。
run_property(Spec, Profile) ->
    Id = maps:get(id, Spec),
    try
        run_property_in(Spec, Profile)
    catch
        Class:Reason:Stack ->
            #{
                id => Id,
                verdict => error,
                class => Class,
                reason => Reason,
                stack_top => lists:sublist(Stack, 4),
                random_started => 0
            }
    end.

%% 在当前进程执行（peer:call 的目标入口；保持可独立调用便于 in-VM 自测）。
run_property_in(#{id := Id, mfa := MFA, witnesses := Ws} = Spec, Profile) ->
    case run_witnesses(Ws) of
        ok ->
            run_random(Id, MFA, Spec, Profile);
        {witness_failed, W, Reason} ->
            #{
                id => Id,
                verdict => witness_failed,
                failed_witness => W,
                witness_reason => Reason,
                random_started => 0
            }
    end.

run_witnesses([]) -> ok;
run_witnesses([W = {M, F, A} | Rest]) ->
    Args = [erlang:make_ref() || _ <- lists:seq(1, A)],
    case erlang:apply(M, F, Args) of
        ok -> run_witnesses(Rest);
        Other -> {witness_failed, W, Other}
    end.

run_random(Id, MFA, Spec, Profile) ->
    N = profile_numtests(Profile, maps:get(kind, Spec, ordinary)),
    {Engine, Msgs} = quickcheck_capture(MFA, N),
    {CompCount, Performed} = count_completions(Msgs),
    #{
        id => Id,
        verdict => completion_verdict(Engine, CompCount, Performed, N),
        engine_return => Engine,
        completion_events => CompCount,
        performed => Performed,
        requested => N,
        random_started => 1,
        counterexample =>
            case Engine of
                true -> none;
                false -> proper:counterexample()
            end,
        on_output_tail => lists:sublist(Msgs, 4)
    }.

quickcheck_capture({M, F, 0}, N) ->
    erlang:put(pbt_output, []),
    Prop = erlang:apply(M, F, []),
    %% proper v1.5.0 选项合同：on_output 为二元 fun((string(),[term()]) -> ok)
    %% （proper.erl:465/1058-1060），非三元 Printer 形态。
    Engine =
        proper:quickcheck(Prop, [
            {numtests, N},
            {on_output, fun capture/2}
        ]),
    Msgs = lists:reverse(erlang:get(pbt_output)),
    erlang:erase(pbt_output),
    {Engine, Msgs}.

capture(Format, Args) ->
    erlang:put(pbt_output, [{Format, Args} | erlang:get(pbt_output)]).

%% PBT03：完成事件恰 1 次且 Performed 覆盖请求例数才算 pass；
%% 引擎 true 但 0/2 次完成事件均 INCOMPLETE（不按请求数填报）。
%% 导出为纯函数：负向分支（0/2 次、欠计数）由自测直接断言。
completion_verdict(true, 1, Performed, N) when Performed >= N -> pass;
completion_verdict(true, 1, _Performed, _N) -> incomplete_undercount;
completion_verdict(true, 0, _, _) -> incomplete_no_completion_event;
completion_verdict(true, Comp, _, _) when Comp >= 2 ->
    incomplete_duplicate_completion_events;
completion_verdict(false, _, _, _) -> property_failed.

%% count_completions/1：对捕获的原始 {Format, Args} 消息序列统计完成事件
%% （纯函数，供 0/1/2 次负向自测直接断言）。
count_completions(Msgs) ->
    Performed = [P || {?COMPLETION_FORMAT, [P]} <- Msgs],
    {length(Performed), lists:sum(Performed)}.

%% ---- peer 隔离执行（每性质一个本机 peer；复用纯 Erlang 底座） ----

run_property_peer(Spec, Profile, CodePaths) ->
    %% 与 imboy_quality_runner 同款：loopback TCP 控制，无 distribution/epmd。
    PaArgs = lists:append([["-pa", CP] || CP <- CodePaths]),
    {ok, Peer, _Node} = peer:start_link(#{
        connection => {{127, 0, 0, 1}, 0},
        args => ["-noshell", "-noinput", "-boot", "no_dot_erlang"] ++ PaArgs
    }),
    try
        case peer:call(Peer, ?MODULE, run_property, [Spec, Profile], 60000) of
            Result when is_map(Result) -> Result;
            Other -> {error, {unexpected_peer_result, Other}}
        end
    after
        peer:stop(Peer)
    end.
