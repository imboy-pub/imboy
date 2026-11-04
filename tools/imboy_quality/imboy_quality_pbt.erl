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
    completion_verdict/4,
    extract_num_shrinks/1,
    save_counterexample/2,
    load_counterexample/1,
    replay_check/2,
    replay_next/2
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
validate_manifest(#{properties := []}) ->
    {error, empty_manifest};
validate_manifest(_) ->
    {error, manifest_missing_properties}.

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
        false -> invalid_witness_mfa_must_be_arity0
    end;
witness_list(_) ->
    witnesses_must_be_nonempty_mfa_list.

%% 见证合同：0 元 MFA，输入内蕴于见证函数（REPLAY02 的"原 input"
%% 由见证自身声明，不由框架注入）。
witness_mfa({M, F, 0}) when is_atom(M), is_atom(F) -> true;
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
            (run_random(Id, MFA, Spec, Profile))#{witnesses_passed_count => length(Ws)};
        {witness_failed, W, Reason} ->
            #{
                id => Id,
                verdict => witness_failed,
                failed_witness => W,
                witness_reason => Reason,
                random_started => 0
            }
    end.

run_witnesses([]) ->
    ok;
run_witnesses([W = {M, F, 0} | Rest]) ->
    case erlang:apply(M, F, []) of
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
        num_shrinks => extract_num_shrinks(Msgs),
        counterexample =>
            case Engine of
                true -> none;
                false -> proper:counterexample()
            end,
        on_output_tail => lists:sublist(Msgs, 6)
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
completion_verdict(true, 1, _Performed, _N) ->
    incomplete_undercount;
completion_verdict(true, 0, _, _) ->
    incomplete_no_completion_event;
completion_verdict(true, Comp, _, _) when Comp >= 2 ->
    incomplete_duplicate_completion_events;
completion_verdict(false, _, _, _) ->
    property_failed.

%% count_completions/1：对捕获的原始 {Format, Args} 消息序列统计完成事件
%% （纯函数，供 0/1/2 次负向自测直接断言）。
count_completions(Msgs) ->
    Performed = [P || {?COMPLETION_FORMAT, [P]} <- Msgs],
    {length(Performed), lists:sum(Performed)}.

%% ---- P2-02：反例持久化、缩减事实与重放（REPLAY01–REPLAY06） ----

-define(REPLAY_SCHEMA, 1).
-define(REPLAY_MAX_BYTES, 1048576).
-define(SHRINK_FORMAT, "(~b time(s))~n").

%% proper.erl:2375 缩减完成字面量 "(~b time(s))~n", [NumShrinks]。
%% REPLAY05：未捕获到该消息（noshrink/超时/中断）时标 unverified，
%% 不声称缩减完成或全局最小。
extract_num_shrinks(Msgs) ->
    case [N || {?SHRINK_FORMAT, [N]} <- Msgs] of
        [N] when is_integer(N), N >= 0 -> {verified, N};
        [] -> unverified
    end.

%% run_random 的结果 map 已带 num_shrinks（成功路径为 unverified，无害）；
%% 保存反例时该字段进入 ETF。

%% ETF 保存：原子写、同 ID 重复材料拒绝（REPLAY06）、case schema 固定。
%% Entry 必填：id / case_kind(witness|random) / mfa / input；
%% witness 失败不填 num_shrinks（REPLAY02：不称已缩减）。
save_counterexample(Ctrl, #{id := Id} = Entry0) when
    is_atom(Id)
->
    Dir = filename:join(Ctrl, "counterexamples"),
    Path = entry_path(Dir, Id),
    ok = filelib:ensure_dir(Path),
    case filelib:is_regular(Path) of
        true ->
            {error, duplicate_material};
        false ->
            Entry = Entry0#{
                schema => ?REPLAY_SCHEMA,
                saved_at => erlang:system_time(millisecond)
            },
            Bin = term_to_binary(Entry, [compressed]),
            atomic_write(Path, Bin),
            {ok, Path}
    end.

entry_path(Dir, Id) ->
    filename:join(Dir, atom_to_list(Id) ++ ".etf").

atomic_write(Path, Bin) ->
    Tmp = Path ++ ".tmp." ++ os:getpid(),
    ok = file:write_file(Tmp, Bin),
    ok = file:rename(Tmp, Path).

%% 安全加载（REPLAY04）：限制读取和解压大小，只接受已存在的 atom。
load_counterexample(Path) ->
    case file:open(Path, [read, binary, raw]) of
        {ok, File} ->
            try
                load_counterexample_bytes(file:read(File, ?REPLAY_MAX_BYTES + 1))
            after
                file:close(File)
            end;
        {error, _} = E ->
            E
    end.

load_counterexample_bytes({ok, Bin}) when byte_size(Bin) > ?REPLAY_MAX_BYTES ->
    {error, too_large};
load_counterexample_bytes({ok, <<131, 80, Size:32/big, _/binary>>}) when Size > ?REPLAY_MAX_BYTES ->
    {error, expanded_too_large};
load_counterexample_bytes({ok, Bin}) ->
    try binary_to_term(Bin, [safe]) of
        #{schema := ?REPLAY_SCHEMA} = E ->
            validate_entry(E);
        #{schema := Other} ->
            {error, {unknown_schema, Other}};
        _ ->
            {error, not_an_entry_map}
    catch
        _:_ -> {error, corrupt_or_unsafe_etf}
    end;
load_counterexample_bytes(eof) ->
    {error, corrupt_or_unsafe_etf};
load_counterexample_bytes({error, _} = E) ->
    E.

validate_entry(E) ->
    case contains_fun(E, 0) of
        true ->
            {error, fun_in_etf};
        false ->
            validate_entry_shape(E)
    end.

%% REPLAY04：safe 解码不拒普通本地 fun，须显式递归扫描拒绝（深度上限防
%% 恶意嵌套）。任何位置出现 fun 即拒绝——反例材料只允许纯数据。
contains_fun(_T, D) when D > 16 ->
    %% 过深结构保守视为可疑
    true;
contains_fun(F, _) when is_function(F) ->
    true;
contains_fun(L, D) when is_list(L) ->
    lists:any(fun(X) -> contains_fun(X, D + 1) end, L);
contains_fun(T, D) when is_tuple(T) ->
    lists:any(fun(X) -> contains_fun(X, D + 1) end, tuple_to_list(T));
contains_fun(M, D) when is_map(M) ->
    lists:any(fun(X) -> contains_fun(X, D + 1) end, maps:to_list(M));
contains_fun(_, _) ->
    false.

validate_entry_shape(E) ->
    case
        {maps:get(id, E, undefined), maps:get(case_kind, E, undefined), maps:get(mfa, E, undefined)}
    of
        {Id, Kind, {M, F, 0}} when
            is_atom(Id),
            is_atom(M),
            is_atom(F),
            Kind =:= witness orelse Kind =:= random
        ->
            case maps:is_key(input, E) of
                true -> validate_replay_receipt(E);
                false -> {error, missing_input}
            end;
        _ ->
            {error, invalid_entry_shape}
    end.

validate_replay_receipt(E) ->
    case {maps:find(replayed_at, E), maps:find(replay_result, E)} of
        {error, error} ->
            {ok, E};
        {{ok, At}, {ok, {ok, Verdict}}} when
            is_integer(At),
            At >= 0,
            (Verdict =:= now_passes orelse Verdict =:= still_fails)
        ->
            {ok, E};
        _ ->
            {error, invalid_replay_receipt}
    end.

%% 重放（REPLAY03）：proper:check/2 对保存的 input 重跑性质。
%% proper 语义（v1.5.0 实测）：true = input 通过测试（反例不再失败）；
%% false = input 仍使测试失败。修复前 still_fails、修复后 now_passes。
%% REPLAY04：MFA 必须在 allowlist 内（manifest 登记性质），不动态加载
%% 任意模块——未知 MFA 在 apply 前拒绝。
replay_check(#{mfa := MFA = {M, F, 0}, input := Input}, Allowed) ->
    case lists:member(MFA, Allowed) of
        false ->
            {error, mfa_not_allowed};
        true ->
            try
                Prop = erlang:apply(M, F, []),
                proper:check(Prop, Input)
            of
                true -> {ok, now_passes};
                false -> {ok, still_fails};
                {error, R} -> {error, R}
            catch
                Class:Reason -> {error, {replay_crash, Class, Reason}}
            end
    end.

%% replay_next：目录即真相（无独立索引可丢失）；按 saved_at 最旧未重放项。
%% 空目录/全部已重放 → 显式错误（REPLAY06 如实非零）。
%% Allowed：manifest 登记的 MFA 白名单（REPLAY04）。
replay_next(Ctrl, Allowed) ->
    Dir = filename:join(Ctrl, "counterexamples"),
    case filelib:wildcard(filename:join(Dir, "*.etf")) of
        [] ->
            {error, no_pending_counterexamples};
        Files ->
            case pending_entries(Files, []) of
                {error, _} = Error ->
                    Error;
                {ok, []} ->
                    {error, no_pending_counterexamples};
                {ok, [{At, Path, E} | _]} ->
                    case replay_check(E, Allowed) of
                        {error, _} = Error ->
                            Error;
                        {ok, _} = Result ->
                            ok = record_replay(Path, E, Result),
                            {ok, #{path => Path, saved_at => At, entry => E, replay => Result}}
                    end
            end
    end.

pending_entries([], Acc) ->
    {ok, lists:sort(Acc)};
pending_entries([Path | Rest], Acc) ->
    case load_counterexample(Path) of
        {error, Reason} ->
            {error, {invalid_counterexample, Path, Reason}};
        {ok, E} ->
            Next =
                case maps:is_key(replayed_at, E) of
                    true -> Acc;
                    false -> [{maps:get(saved_at, E, 0), Path, E} | Acc]
                end,
            pending_entries(Rest, Next)
    end.

record_replay(Path, E, Result) ->
    E2 = E#{replayed_at => erlang:system_time(millisecond), replay_result => Result},
    atomic_write(Path, term_to_binary(E2, [compressed])),
    ok.

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
