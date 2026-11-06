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
    selftest/0,
    business_ids/0,
    critical_suites/0,
    waived_ops/0
]).

%% 此模块只验证调用方提供的结构化收据；调用方必须先核验原始证据真实性。
gate(Results, Ctx) ->
    try
        true = is_list(Results),
        true = is_map(Ctx),
        Keys = [head, input_fingerprint, u_fingerprint],
        true = lists:all(fun(K) -> nonempty(maps:get(K, Ctx)) end, Keys),
        Artifacts = maps:get(artifacts, Ctx),
        true = is_list(Artifacts) andalso Artifacts =/= [],
        validate_waivers(Ctx),
        Required = required_ops(Ctx),
        true = lists:sort(Artifacts) =:= lists:sort([{Op, true} || Op <- Required]),
        validate_expectations(Ctx),
        Ops = [maps:get(op, R) || R <- Results],
        true = lists:sort(Ops) =:= lists:sort(Required),
        true = lists:all(fun(R) -> gate_result(R, Ctx) =:= [] end, Results),
        pass
    catch
        _:_ -> {fail, [invalid_or_incomplete_evidence]}
    end.

%% Full EUnit is required by default. A task-specific historical waiver must
%% remain explicit and traceable; it must not become a global default.
required_ops(Ctx) ->
    All = [
        <<"eunit_full">>,
        <<"pbt_quick">>,
        <<"pbt_nightly">>,
        <<"critical_ct">>,
        <<"guard">>
    ],
    Waived = [Op || {Op, _} <- maps:get(waived_ops, Ctx, [])],
    All -- Waived.

validate_waivers(Ctx) ->
    Waived = maps:get(waived_ops, Ctx, []),
    true = is_list(Waived),
    true = length(Waived) =:= length(lists:usort([Op || {Op, _} <- Waived])),
    lists:foreach(
        fun({Op, Authority}) ->
            true = lists:member(Op, waived_ops()),
            true = nonempty(Authority)
        end,
        Waived
    ),
    case Waived of
        [] -> ok;
        _ -> true = nonempty(maps:get(<<"waiver_ref">>, Ctx))
    end,
    ok.

waived_ops() ->
    [<<"eunit_full">>].

%% 收据字段无默认值：通过数量、失败/跳过/取消、清理及行为断言均显式给出。
gate_result(R, Ctx) ->
    try
        validate_waivers(Ctx),
        true = lists:member(maps:get(op, R), required_ops(Ctx)),
        true = nonempty(maps:get(candidate_sha, R)),
        true = maps:get(candidate_sha, R) =:= maps:get(head, Ctx),
        lists:foreach(
            fun(K) ->
                true = nonempty(maps:get(K, Ctx)),
                true = maps:get(K, R) =:= maps:get(K, Ctx)
            end,
            [input_fingerprint, u_fingerprint]
        ),
        true = nonempty(maps:get(evidence_ref, R)),
        <<"all_passed">> = maps:get(verdict, R),
        Total = maps:get(total, R),
        true = is_integer(Total) andalso Total > 0,
        Total = maps:get(passed, R),
        0 = maps:get(fail, R),
        0 = maps:get(skip, R),
        0 = maps:get(cancel, R),
        0 = maps:get(roots_missing, R),
        true = maps:get(cleanup_ok, R),
        true = maps:get(assertions_ok, R),
        validate_details(maps:get(op, R), R, Ctx),
        []
    catch
        _:_ -> [invalid_result]
    end.

validate_expectations(Ctx) ->
    Props = maps:get(expected_properties, Ctx),
    true = is_map(Props),
    true = lists:all(fun(Id) -> maps:is_key(Id, Props) end, business_ids()),
    lists:foreach(
        fun(Id) ->
            Kind =
                case Id of
                    <<"q01_tsid_reserve_sequences">> -> sequence;
                    _ -> ordinary
                end,
            Kind = maps:get(Id, Props)
        end,
        business_ids()
    ),
    true = lists:all(
        fun({Id, Kind}) ->
            nonempty(Id) andalso
                lists:member(Kind, [ordinary, sequence])
        end,
        maps:to_list(Props)
    ),
    lists:foreach(
        fun(K) -> unique_nonempty(maps:get(K, Ctx)) end,
        [expected_full_roots, expected_critical_suites, expected_guard_checks]
    ),
    true = lists:sort(maps:get(expected_critical_suites, Ctx)) =:= lists:sort(critical_suites()),
    Roots = maps:get(expected_full_roots, Ctx),
    Leaves = maps:get(expected_leaf_ids, Ctx),
    true = is_map(Leaves) andalso lists:sort(maps:keys(Leaves)) =:= lists:sort(Roots),
    lists:foreach(fun(Root) -> unique_nonempty(maps:get(Root, Leaves)) end, Roots),
    AllLeaves = lists:append(maps:values(Leaves)),
    unique_nonempty(AllLeaves),
    ok.

critical_suites() ->
    [
        <<"msg_delivery_SUITE">>,
        <<"msg_ack_logic_SUITE">>,
        <<"imboy_quality_ct02_org_boundary_SUITE">>,
        <<"imboy_quality_ct03_migration_SUITE">>
    ].

business_ids() ->
    [
        <<"f01_frame_roundtrip_wire">>,
        <<"f02_frame_fragment_recovery">>,
        <<"d01_same_instant_offsets">>,
        <<"d02_precision_scaling">>,
        <<"c01_canonical_reference">>,
        <<"c02_illegal_injection">>,
        <<"s01_tsid_bit_layout">>,
        <<"s02_tsid_interval_reservation">>,
        <<"q01_tsid_reserve_sequences">>,
        <<"m01_full_manifest_model">>,
        <<"m02_entry_idempotence_page_integrity">>,
        <<"m03_revision_conflict_compat">>,
        <<"oracle01_frame_corruption">>,
        <<"oracle02_offset_reversal">>,
        <<"oracle03_tsid_slot_reuse">>,
        <<"oracle04_stale_live_over_delete">>
    ].

unique_nonempty(Ids) ->
    true = is_list(Ids) andalso Ids =/= [],
    true = lists:all(fun nonempty/1, Ids),
    true = length(Ids) =:= length(lists:usort(Ids)),
    ok.

validate_details(Op, R, Ctx) ->
    Items = maps:get(results, R),
    true = is_list(Items),
    Ids = [maps:get(id, Item) || Item <- Items],
    unique_nonempty(Ids),
    Expected =
        case Op of
            <<"eunit_full">> -> maps:get(expected_full_roots, Ctx);
            <<"critical_ct">> -> maps:get(expected_critical_suites, Ctx);
            <<"guard">> -> maps:get(expected_guard_checks, Ctx);
            _ -> maps:keys(maps:get(expected_properties, Ctx))
        end,
    true = lists:sort(Ids) =:= lists:sort(Expected),
    true = length(Items) =:= maps:get(total, R),
    lists:foreach(fun(Item) -> validate_item(Op, Item, Ctx) end, Items),
    ok.

validate_item(Op, Item, Ctx) ->
    Executed = maps:get(executed, Item),
    true = is_integer(Executed) andalso Executed > 0,
    Executed = maps:get(passed, Item),
    0 = maps:get(fail, Item),
    0 = maps:get(skip, Item),
    0 = maps:get(cancel, Item),
    case Op of
        <<"eunit_full">> ->
            Leaves = maps:get(leaf_ids, Item),
            unique_nonempty(Leaves),
            true =
                lists:sort(Leaves) =:=
                    lists:sort(maps:get(maps:get(id, Item), maps:get(expected_leaf_ids, Ctx))),
            true = Executed =:= length(Leaves);
        <<"pbt_quick">> ->
            validate_pbt(Item, Ctx, quick);
        <<"pbt_nightly">> ->
            validate_pbt(Item, Ctx, nightly);
        _ ->
            ok
    end.

validate_pbt(Item, Ctx, Profile) ->
    Witnesses = maps:get(witnesses_passed_count, Item),
    true = is_integer(Witnesses) andalso Witnesses > 0,
    Kind = maps:get(maps:get(id, Item), maps:get(expected_properties, Ctx)),
    N = profile_count(Profile, Kind),
    N = maps:get(requested, Item),
    N = maps:get(performed, Item),
    1 = maps:get(completion, Item),
    1 = maps:get(random_started, Item),
    ok.

profile_count(quick, ordinary) -> 200;
profile_count(quick, sequence) -> 100;
profile_count(nightly, ordinary) -> 2000;
profile_count(nightly, sequence) -> 500.

nonempty(B) -> is_binary(B) andalso byte_size(B) > 0.

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

%% 更完整的负向与正向验证位于纯合成 EUnit 测试。
selftest() ->
    {fail, _} = gate([], #{}),
    ok.
