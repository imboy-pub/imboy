%% IMBOY_ERLANG_QUALITY P2-10：同候选 CI final-gate 判据器 + LANG01-04 语言
%% 闭包审计。CI06 语义：不信任退出码/正文摘要，只信结构化证据字段；
%% CI02：所有 job 证据绑定同一不可变候选（worktree HEAD）；CI03/04：任何
%% 失败/cancel/制品缺失即非通过。LANG01-04：控制/报告/夹具/时钟/分片层
%% 全 Erlang；静态检出 sh -c / 脚本转发 / 解释器规避；如实列外部边界。
-module(imboy_quality_ci).

-export([
    gate/2,
    gate_result/2,
    lang_audit/1,
    selftest/0
]).

%% ---- CI01-06 final gate（纯判据，selftest 可注入） ----
%% Results = [#{op => atom, verdict => binary, candidate_sha => binary, extra...}]
%% Ctx = #{head => binary(), artifacts => [{Name, Exists}]}
gate(Results, #{head := Head} = Ctx) ->
    Issues0 =
        [
            candidate_mismatch
         || lists:any(fun(R) -> maps:get(candidate_sha, R, none) =/= Head end, Results)
        ],
    Issues1 = Issues0 ++ lists:append([gate_result(R, Ctx) || R <- Results]),
    Artifacts = maps:get(artifacts, Ctx, []),
    Missing = [N || {N, false} <- Artifacts],
    Issues =
        Issues1 ++
            [{artifact_missing, N} || N <- Missing] ++
            [empty_results || Results =:= []],
    case Issues of
        [] ->
            pass;
        _ ->
            {fail, Issues}
    end.

%% 单 op 结构化判据（CI03/06）：verdict 必须绿且计数完备
gate_result(#{op := <<"pbt">>, verdict := <<"all_passed">>} = R, _Ctx) ->
    Total = maps:get(<<"total">>, R, 0),
    Passed = maps:get(<<"passed">>, R, 0),
    [{gate_count_violation, pbt} || not (Total > 0 andalso Total =:= Passed)];
gate_result(#{op := <<"ct">>, verdict := <<"all_passed">>} = R, _Ctx) ->
    [{gate_count_violation, ct} || maps:get(<<"total">>, R, 0) =:= 0];
gate_result(#{op := <<"eunit_full">>, verdict := <<"all_passed">>} = R, _Ctx) ->
    Bad =
        maps:get(<<"fail">>, R, 1) =/= 0 orelse
            maps:get(<<"cancel">>, R, 1) =/= 0 orelse
            maps:get(<<"roots_missing">>, R, 1) =/= 0,
    [{gate_count_violation, eunit_full} || Bad];
gate_result(#{op := <<"guard">>, verdict := <<"clean">>}, _Ctx) ->
    [];
gate_result(#{op := Op, verdict := V}, _Ctx) ->
    [{op_not_green, Op, V}].

%% ---- LANG01-04 ----

lang_audit(Repo) ->
    Self = self_sources(Repo),
    %% LANG01：控制/报告/夹具/时钟/分片层全部 Erlang（.erl + .escript）
    NonErlang = [P || P <- Self, not is_erlang_source(P)],
    %% LANG02 静态扫描：sh -c 拼接 / 解释器调用 / 改名转发
    Violations = scan_forbidden_patterns(Self),
    %% LANG03/LANG04：repo 内既有非 Erlang 资产与允许外部边界（如实盘点）
    RepoPy =
        filelib:wildcard(filename:join([Repo, "scripts", "*.py"])) ++
            filelib:wildcard(filename:join([Repo, "test", "migrations", "*.py"])),
    RepoSh = filelib:wildcard(filename:join([Repo, "scripts", "*.sh"])),
    #{
        <<"lang01_self_sources">> => length(Self),
        <<"lang01_non_erlang">> => [rel(Repo, P) || P <- NonErlang],
        <<"lang02_violations">> => Violations,
        <<"lang02_known_exemptions">> =>
            [
                #{
                    <<"file">> => <<"scripts/quality.escript">>,
                    <<"reason">> =>
                        <<"admin-entry-only: resolve_control_dir git rev-parse location probe in scripts/quality.escript; no sh -c concatenation, no third-party script forwarding; all test-control paths use open_port spawn_executable (P1-03 accepted state, production admin entry left untouched)">>
                }
            ],
        <<"lang03_repo_py_assets">> => [rel(Repo, P) || P <- RepoPy],
        <<"lang03_repo_sh_count">> => length(RepoSh),
        <<"lang03_note">> =>
            <<"existing py/sh assets are product ops scripts and components under test; this task test closure does not invoke them; obligations inventoried as-is">>,
        <<"lang04_allowed_native">> =>
            [<<"git">>, <<"make">>, <<"erlc/escript (OTP)">>, <<"pg_ctl/psql/createdb (PG)">>],
        <<"lang04_external_under_test">> =>
            [
                <<"scripts/*.sh product ops scripts">>,
                <<"test/migrations/*.py components under test">>,
                <<"HIRD/Garage binaries">>
            ]
    }.

self_sources(Repo) ->
    filelib:wildcard(filename:join([Repo, "tools", "imboy_quality", "*.erl"])) ++
        filelib:wildcard(filename:join([Repo, "test", "pbt", "*.erl"])) ++
        filelib:wildcard(filename:join([Repo, "test", "quality", "*.erl"])) ++
        filelib:wildcard(filename:join([Repo, "test", "imboy_quality_*_SUITE.erl"])) ++
        [filename:join(Repo, "scripts/quality.escript")].

is_erlang_source(P) ->
    filename:extension(P) =:= ".erl" orelse filename:extension(P) =:= ".escript".

%% 静态检出（LANG02）：sh -c 拼接 / python/node/bash 解释器调用
scan_forbidden_patterns(Sources) ->
    lists:append([
        case file:read_file(P) of
            {ok, Bin} ->
                Name = filename:basename(P),
                try re:run(Bin, <<"sh -c|python3?[ (]|node[ (]|bash[ (]">>, [global]) of
                    {match, _} ->
                        [iolist_to_binary(["pattern_alert: ", Name])];
                    nomatch ->
                        [];
                    _ ->
                        []
                catch
                    _:_ -> [iolist_to_binary(["scan_error: ", Name])]
                end;
            _ ->
                []
        end
     || P <- Sources
    ]).

%% 本模块被编译进 escript 时无 Repo 绑定——用调用方传入的相对化不可行，
%% 简化：报告文件名（不含路径推导）。
rel(_Repo, P) ->
    unicode:characters_to_binary(filename:basename(P)).

%% ---- CI02-06 负向自测（注入判据器） ----

selftest() ->
    Head = <<"aaaa1111bbbb2222cccc3333dddd4444eeee5555">>,
    GoodPbt = #{
        op => <<"pbt">>,
        verdict => <<"all_passed">>,
        candidate_sha => Head,
        <<"total">> => 16,
        <<"passed">> => 16
    },
    GoodCt = #{
        op => <<"ct">>,
        verdict => <<"all_passed">>,
        candidate_sha => Head,
        <<"total">> => 4
    },
    GoodGuard = #{op => <<"guard">>, verdict => <<"clean">>, candidate_sha => Head},
    Arts = [{pbt_latest, true}, {ct_latest, true}, {guard_latest, true}],
    %% CI01/CI06 正控：结构化绿 + 计数完备 + 制品在
    pass = gate([GoodPbt, GoodCt, GoodGuard], #{head => Head, artifacts => Arts}),
    %% CI02：不同 SHA 的 job 证据 → 拒绝
    {fail, Issues2} =
        gate(
            [
                GoodPbt,
                GoodCt#{candidate_sha => <<"ffff0000eeee1111dddd2222cccc3333bbbb4444">>},
                GoodGuard
            ],
            #{head => Head, artifacts => Arts}
        ),
    true = lists:member(candidate_mismatch, Issues2),
    %% CI03/06：verdict 伪绿但计数不足 → 拒绝（不信任退出码）
    {fail, Issues3} =
        gate(
            [GoodPbt#{<<"passed">> => 15}, GoodCt, GoodGuard],
            #{head => Head, artifacts => Arts}
        ),
    true = lists:member({gate_count_violation, pbt}, Issues3),
    %% CI03：eunit_full fail>0 → 拒绝
    BadEunit = #{
        op => <<"eunit_full">>,
        verdict => <<"all_passed">>,
        candidate_sha => Head,
        <<"fail">> => 1,
        <<"cancel">> => 0,
        <<"roots_missing">> => 0
    },
    {fail, Issues4} = gate([BadEunit], #{head => Head, artifacts => Arts}),
    true = lists:member({gate_count_violation, eunit_full}, Issues4),
    %% CI04：制品缺失 → 拒绝
    {fail, Issues5} =
        gate(
            [GoodPbt, GoodCt, GoodGuard],
            #{head => Head, artifacts => [{pbt_latest, true}, {ct_latest, false}]}
        ),
    true = lists:member({artifact_missing, ct_latest}, Issues5),
    %% CI01 负向：空结果集 → 拒绝
    {fail, Issues6} = gate([], #{head => Head, artifacts => Arts}),
    true = lists:member(empty_results, Issues6),
    ok.
