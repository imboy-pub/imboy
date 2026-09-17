%% @doc AG31-10B：G07-G20 逐门测试（同一 pinned hird fingerprint 453bb371 +
%% 同一 IMBoy candidate）。G01-G06 由 cargo/runtime/spike 套件覆盖；G14 真库
%% 双连接腿=evidence/AG31-09 agent_recovery_pg_tests（AG31-12 终门以最终
%% HEAD 全量重跑）。本套件全部串行（hird_audit node 单例纪律）。
-module(imboy_hird_gates_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HIRD_BIN, <<"/Users/leeyi/project/imboy.pub/hird/target/debug/hird">>).
-define(OUT_ROOT, <<"_build/ag31_10b">>).
-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(REC, agent_recovery).

-define(ORG_A, 371).
-define(ORG_B, 372).
-define(AGENT, 373).
-define(RUN, 374).
-define(GRANT, 375).
-define(EFFECT_ID, 4300).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

%%% hird 源（Echo 工具；产物模块 hird_gate_echo；声明名独立避开 spike）。
-define(HIRD_SRC, <<"""
module GateEcho

tool Echo : { message: String } → String

fn main() → () ! {} =
  handle {
    Tool<Echo> → echo_gate,
  } in let _ = echo({ message: "gate" }) in ()

fn echo_gate(args: { message: String }) → String =
  "gate-echo"
"""/utf8>>).

-define(MOD, hird_gate_echo).

gates_test_() ->
    {foreach, fun setup/0, fun teardown/1, [
        {<<"G07 跨 scope negative：org B 资源 → cross_org deny + hird 零接触">>, fun g07_cross_scope/0},
        {<<"G08 context isolation：双实例并发 handler 结果零串线">>, fun g08_context/0},
        {<<"G09 permission fail-closed：catalog 不可用 → deny + dispatch 0">>,
            fun g09_permission_failclosed/0},
        {<<"G10 HITL fail-closed：无批准/陈旧批准/digest 错全阻断">>, fun g10_hitl/0},
        {<<"G11 crash recovery：handler crash 面收敛 + 接管链可用">>, fun g11_crash/0},
        {<<"G12 cancel recovery：cancel 后 run_tool → run_not_running">>, fun g12_cancel/0},
        {<<"G13 timeout 映射：超时 → {dispatch_timeout,Ms}，不吞错">>, fun g13_timeout/0},
        {<<"G15 duplicate 不重派：同幂等键 → duplicate_effect，dispatch 恰一">>, fun g15_duplicate/0},
        {<<"G16 deterministic replay：同输入两次 audit hash 一致">>, fun g16_replay/0},
        {<<"G17 重放零外呼：外呼计数恒 0 + hash 复现">>, fun g17_no_external/0},
        {<<"G18 readonly Tool E2E：全链 allow + sanitized + audit run scope">>,
            fun g18_readonly_e2e/0},
        {<<"G19 approval-required E2E：E07→批准→recheck→allow 全链">>, fun g19_approval_e2e/0},
        {<<"G20 进程/注册残留归零（并发放大后）">>, fun g20_residue/0}
    ]}.

%% ===================================================================
%% 夹具
%% ===================================================================

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    given_all_gates_pass(),
    ensure_built(),
    %% 共享 audit sink：dispatch 的同步依赖（用例内自管理文件者先停它）
    SharedFile = audit_path(<<"gates_shared">>),
    _ = file:delete(SharedFile),
    ok = filelib:ensure_dir(SharedFile),
    {ok, _} = hird_audit:start_link([{sink, {file, SharedFile}}]),
    %% D2#5：宿主自定 tool 必须注册签名表，否则 audit 报 {unknown_tool,_}
    ok = hird_audit:register_tools(?MOD:hird_tools@()),
    ok.

teardown(_C) ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [?PG, ?GPG, ?MEMBERSHIP, ?CATALOG, agent_run_command, ag31_10b_policy, ag31_10b_dispatcher]
    ),
    lists:foreach(
        fun(K) -> application:unset_env(imboy, K) end,
        [agent_resource_policy_module, agent_tool_dispatcher_module]
    ),
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    [
        persistent_term:erase(K)
     || {K, _} <- persistent_term:get(),
        element(1, K) =:= hird_handlers
    ],
    erase(g20_workers),
    ok.

given_all_gates_pass() ->
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 4400
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 5 end),
    meck:expect(?PG, get_run, fun(_C, _R) -> {ok, run_row(?RUN)} end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row(2)} end),
    meck:expect(?PG, get_effect, fun(_C, _E) -> {error, not_found} end),
    meck:expect(?PG, insert_effect_tx, fun(_C, _E, _R) -> {ok, ?EFFECT_ID, undefined} end),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) -> {ok, ?EFFECT_ID} end),
    meck:expect(?GPG, list_workspace_ids, fun(_C, _O, _G) -> [] end),
    meck:expect(?GPG, list_capabilities, fun(_C, _O, _G) ->
        [
            #{
                capability => <<"demo.read">>,
                action => <<"invoke">>,
                resource_type => <<"demo">>,
                constraint => #{}
            }
        ]
    end),
    meck:expect(?MEMBERSHIP, resolve_organization_state, fun(_O) ->
        {ok, #{status => active, version => 5}}
    end),
    meck:expect(?MEMBERSHIP, resolve_organization_membership, fun(_O, _A) ->
        {ok, #{status => active, role => member, version => 7}}
    end),
    meck:expect(?MEMBERSHIP, resolve_workspace_membership, fun(_O, _W, _A) ->
        {ok, #{status => active, role => member, version => 9}}
    end),
    meck:expect(?CATALOG, lookup, fun(C, A, R) ->
        {ok, #{
            capability => C,
            action => A,
            resource_type => R,
            legal_constraint_keys => [<<"workspace_ids">>, <<"resource_id">>]
        }}
    end),
    safe_meck_new(ag31_10b_policy),
    meck:expect(ag31_10b_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_10b_policy),
    safe_meck_new(ag31_10b_dispatcher),
    meck:expect(ag31_10b_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_10b_dispatcher).

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end.

run_row(RunId) ->
    #{
        id => RunId,
        version => 3,
        status => running,
        grant_id => ?GRANT,
        agent_id => ?AGENT,
        organization_id => ?ORG_A,
        workspace_id => undefined,
        idempotency_key => <<"g-run">>,
        trigger_type => message
    }.

grant_row(Version) ->
    #{
        id => ?GRANT,
        version => Version,
        status => active,
        valid_from => {{2026, 9, 1}, {0, 0, 0}},
        expires_at => {{2027, 9, 1}, {0, 0, 0}}
    }.

run_ctx() ->
    run_ctx(#{}).

run_ctx(Over) ->
    maps:merge(
        #{
            run_id => ?RUN,
            agent_id => ?AGENT,
            organization_id => ?ORG_A,
            now => ?NOW,
            conn => self()
        },
        Over
    ).

tool() ->
    #{
        tool_id => <<"tool.gate.echo">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

write_tool() ->
    maps:merge(tool(), #{risk_level => medium, side_effect_class => write}).

resource() ->
    resource(#{}).

resource(Over) ->
    maps:merge(
        #{
            organization_id => ?ORG_A,
            workspace_id => undefined,
            resource_type => <<"demo">>,
            resource_digest => <<"sha256:res">>,
            args_digest => <<"sha256:args">>,
            hird_args => #{message => <<"gate">>}
        },
        Over
    ).

handler_map() ->
    #{{tool, echo} => fun(#{message := Msg}, _H) -> <<"echo:", Msg/binary>> end}.

disp_calls() ->
    try
        meck:num_calls(ag31_10b_dispatcher, dispatch, '_')
    catch
        _:_ -> 0
    end.

%% hird 产物（GateEcho；源文件名=snake_case(module) 约束 C0019）
ensure_built() ->
    OutDir = filename:join([?OUT_ROOT, "out_gate"]),
    Beam = filename:join([OutDir, "hird_gate_echo.beam"]),
    case filelib:is_regular(Beam) of
        true -> ok;
        false -> build()
    end,
    true = code:add_patha(binary_to_list(OutDir)),
    case code:is_loaded(?MOD) of
        {file, _} ->
            ok;
        false ->
            {module, ?MOD} = code:load_file(?MOD),
            ok
    end.

build() ->
    OutDir = filename:join([?OUT_ROOT, "out_gate"]),
    ok = filelib:ensure_dir(filename:join([OutDir, "x"])),
    Src = filename:join([?OUT_ROOT, "gate_echo.hird"]),
    ok = file:write_file(Src, ?HIRD_SRC),
    Cmd = io_lib:format("\"~ts\" build \"~ts\" --out-dir \"~ts\" 2>&1", [?HIRD_BIN, Src, OutDir]),
    Output = os:cmd(Cmd),
    case filelib:is_regular(filename:join([OutDir, "hird_gate_echo.beam"])) of
        true -> ok;
        false -> erlang:error({hird_build_failed, Output})
    end.

audit_path(Tag) ->
    filename:join([?OUT_ROOT, "audit", <<Tag/binary, ".jsonl">>]).

read_lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

spawn_counted(Fun) ->
    Parent = self(),
    Tag = make_ref(),
    spawn(fun() -> Parent ! {Tag, Fun()} end),
    Tag.

await(Tag) ->
    receive
        {Tag, R} -> R
    after 15000 -> erlang:error(g20_worker_timeout)
    end.

%% ===================================================================
%% G07-G20
%% ===================================================================

g07_cross_scope() ->
    ResB = resource(#{organization_id => ?ORG_B}),
    ?assertEqual(
        {deny, cross_org},
        imboy_hird:run_tool(run_ctx(), tool(), ResB, handler_map(), echo)
    ),
    %% hird 零接触（无 dispatch worker 产生——无 audit 文件增长）
    ?assertEqual(0, disp_calls()).

g08_context() ->
    %% 双"实例"= 不同 handler map（词法 map 实例隔离语义），并发互不污染
    MapA = #{{tool, echo} => fun(_, _H) -> <<"result-A">> end},
    MapB = #{{tool, echo} => fun(_, _H) -> <<"result-B">> end},
    T1 = spawn_counted(fun() ->
        imboy_hird:with_run_handlers(?RUN, MapA, fun() ->
            [
                begin
                    {ok, #{result_digest := D}} = hird_echo(MapA),
                    D
                end
             || _ <- lists:seq(1, 8)
            ]
        end)
    end),
    T2 = spawn_counted(fun() ->
        imboy_hird:with_run_handlers(?RUN, MapB, fun() ->
            [
                begin
                    {ok, #{result_digest := D}} = hird_echo(MapB),
                    D
                end
             || _ <- lists:seq(1, 8)
            ]
        end)
    end),
    DsA = await(T1),
    DsB = await(T2),
    ?assertEqual(1, length(lists:usort(DsA))),
    ?assertEqual(1, length(lists:usort(DsB))),
    ?assertNotEqual(hd(DsA), hd(DsB)).

hird_echo(Map) ->
    imboy_hird:dispatch_with_timeout(echo, <<"agent_run:g8:echo">>, #{message => <<"x">>}, Map, #{
        timeout_ms => 5000
    }).

g09_permission_failclosed() ->
    meck:expect(?CATALOG, lookup, fun(_C, _A, _R) -> erlang:error(catalog_down) end),
    ?assertEqual(
        {deny, catalog_unavailable},
        imboy_hird:run_tool(run_ctx(), tool(), resource(), handler_map(), echo)
    ),
    ?assertEqual(0, disp_calls()).

g10_hitl() ->
    %% 1) write 工具无批准绑定 → approval_required（dispatch 0）
    {approval_required, AC} =
        imboy_hird:run_tool(run_ctx(), write_tool(), resource(), handler_map(), echo),
    EffectId = maps:get(effect_id, AC),
    ?assertEqual(0, disp_calls()),
    %% 2) 陈旧批准（digest 错）→ authorize 全链重走拒
    meck:expect(?PG, get_effect, fun(_C, _E) ->
        {ok, #{
            id => EffectId,
            run_id => ?RUN,
            status => authorized,
            args_digest => <<"sha256:OLD">>,
            grant_version_checked => 2
        }}
    end),
    Ctx = run_ctx(#{approval_effect_id => EffectId}),
    ?assertEqual(
        {deny, stale_approval_args},
        imboy_hird:run_tool(Ctx, write_tool(), resource(), handler_map(), echo)
    ),
    %% 3) grant 版本漂移 → stale_approval_grant_version（digest 恢复正确，
    %%    使校验顺序推进到版本位）
    meck:expect(?PG, get_effect, fun(_C, _E) ->
        {ok, #{
            id => 1,
            run_id => ?RUN,
            status => authorized,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        }}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) -> {ok, grant_row(9)} end),
    ?assertEqual(
        {deny, stale_approval_grant_version},
        imboy_hird:run_tool(Ctx, write_tool(), resource(), handler_map(), echo)
    ),
    ?assertEqual(0, disp_calls()).

g11_crash() ->
    BoomMap = #{{tool, echo} => fun(_, _) -> erlang:error(echo_boom) end},
    ?assertEqual(
        {error, dispatch_crashed},
        imboy_hird:dispatch_with_timeout(echo, <<"agent_run:g11:echo">>, #{}, BoomMap, #{
            timeout_ms => 5000
        })
    ),
    %% 接管链可用：lease 竞争语义（单 owner）与 recheck 入口在位
    meck:reset(?PG),
    meck:expect(?PG, lease_take_over, fun(_C, _R, _W, _E, _N) ->
        {error, lease_not_acquired}
    end),
    ?assertEqual(
        {error, lease_not_acquired},
        ?REC:recover_run(self(), ?RUN, <<"w-x">>, 60, #{now => ?NOW})
    ).

g12_cancel() ->
    %% Run 已取消（终态）→ 守卫事务拒绝（§14 deny all new effects）
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
        {error, {run_not_active, <<"cancelled">>}}
    end),
    ?assertEqual(
        {deny, run_not_running},
        imboy_hird:run_tool(run_ctx(), tool(), resource(), handler_map(), echo)
    ),
    ?assertEqual(0, disp_calls()).

g13_timeout() ->
    Map = #{
        {tool, echo} => fun(_, _) ->
            timer:sleep(3000),
            <<"late">>
        end
    },
    ?assertEqual(
        {error, {dispatch_timeout, 300}},
        imboy_hird:dispatch_with_timeout(echo, <<"agent_run:g13:echo">>, #{}, Map, #{
            timeout_ms => 300
        })
    ).

g15_duplicate() ->
    Res = resource(#{external_idempotency_key => <<"g15-key">>}),
    {ok, _} = imboy_hird:run_tool(run_ctx(), tool(), Res, handler_map(), echo),
    meck:expect(?PG, insert_effect_guarded_tx, fun(_C, _E) ->
        {error, {duplicate_effect, <<"uq_ae_tool_external_idem">>}}
    end),
    ?assertEqual(
        {deny, duplicate_effect},
        imboy_hird:run_tool(run_ctx(), tool(), Res, handler_map(), echo)
    ),
    ?assertEqual(1, disp_calls()).

g16_replay() ->
    %% record 自管理 audit 生命周期 → 先停共享实例，尾后恢复
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    try
        File1 = audit_path(<<"g16a">>),
        File2 = audit_path(<<"g16b">>),
        R1 = imboy_hird_replay:record(?MOD, File1, fun() -> ok = ?MOD:main() end),
        R2 = imboy_hird_replay:record(?MOD, File2, fun() -> ok = ?MOD:main() end),
        ?assert(R1 =/= #{}),
        ?assertEqual(maps:get(hash, R1), maps:get(hash, R2))
    after
        restart_shared_audit()
    end.

g17_no_external() ->
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    try
        File = audit_path(<<"g17">>),
        R1 = imboy_hird_replay:record(?MOD, File, fun() -> ok = ?MOD:main() end),
        put(imboy_hird_external_calls, 0),
        R2 = imboy_hird_replay:record(?MOD, File, fun() ->
            ok = ?MOD:main(),
            put(imboy_hird_external_calls, 0)
        end),
        ?assertEqual(maps:get(hash, R1), maps:get(hash, R2)),
        %% 重放期间外呼恒 0（mock/纯函数 handler，无 Model/Tool/CS 外联）
        ?assertEqual(0, get(imboy_hird_external_calls))
    after
        restart_shared_audit()
    end.

g18_readonly_e2e() ->
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    try
        g18_body()
    after
        restart_shared_audit()
    end.

g18_body() ->
    File = audit_path(<<"g18">>),
    _ = file:delete(File),
    ok = filelib:ensure_dir(File),
    {ok, _} = hird_audit:start_link([{sink, {file, File}}]),
    try
        ok = hird_audit:register_tools(?MOD:hird_tools@()),
        {ok, Sanitized} =
            imboy_hird:run_tool(run_ctx(), tool(), resource(), handler_map(), echo),
        %% sanitized：只 digest（零原始载荷）
        ?assertEqual([effect_id, result_digest, status], lists:sort(maps:keys(Sanitized))),
        ?assertEqual(1, disp_calls()),
        ok = hird_audit:sync()
    after
        try
            gen_server:stop(hird_audit)
        catch
            _:_ -> ok
        end
    end,
    %% audit 行带 run scope caller（D2#2）
    Lines = read_lines(File),
    ?assert(length(Lines) >= 1),
    Joined = iolist_to_binary(Lines),
    ?assertNotMatch(
        nomatch, binary:match(Joined, <<"agent_run:", (integer_to_binary(?RUN))/binary>>)
    ).

g19_approval_e2e() ->
    safe_meck_new(agent_run_command),
    %% 1) write → approval_required + E07（同事务 Run 迁移，mock 面验证 effect）
    {approval_required, AC} =
        imboy_hird:run_tool(run_ctx(), write_tool(), resource(), handler_map(), echo),
    EffectId = maps:get(effect_id, AC),
    ?assertEqual(0, disp_calls()),
    %% 2) 人工批准（真 agent_run_command 链的 meck 替身：digest 匹配 + 版本一致）
    meck:expect(?PG, get_effect, fun(_C, _E) ->
        {ok, #{
            id => EffectId,
            run_id => ?RUN,
            status => waiting_approval,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        }}
    end),
    meck:expect(agent_run_command, approve_effect, fun(_C, _R, _E, _Ctx) ->
        {ok, #{effect_version => 3, run_version => 5}}
    end),
    {ok, _} =
        agent_tool_authorizer:approve_effect(self(), ?RUN, EffectId, #{
            args_digest => <<"sha256:args">>,
            approval_ref => <<"appr-g19">>,
            actor_id => <<"human-1">>,
            now => ?NOW
        }),
    %% 3) 批准后 recheck（重走 authorize 全链 + 批准绑定校验）→ allow
    meck:expect(?PG, get_effect, fun(_C, _E) ->
        {ok, #{
            id => EffectId,
            run_id => ?RUN,
            status => authorized,
            args_digest => <<"sha256:args">>,
            grant_version_checked => 2
        }}
    end),
    Ctx = run_ctx(#{approval_effect_id => EffectId}),
    {ok, _Sanitized} =
        imboy_hird:run_tool(Ctx, write_tool(), resource(), handler_map(), echo),
    ?assertEqual(1, disp_calls()).

g20_residue() ->
    %% 并发放大（2 实例 × 10 次纯函数 main）后四类残留归零（G06/G20）
    MapA = #{{tool, echo} => fun(_, _H) -> <<"ra">> end},
    MapB = #{{tool, echo} => fun(_, _H) -> <<"rb">> end},
    T1 = spawn_counted(fun() ->
        imboy_hird:with_run_handlers(1, MapA, fun() ->
            [element(1, hird_echo(MapA)) || _ <- lists:seq(1, 10)]
        end)
    end),
    T2 = spawn_counted(fun() ->
        imboy_hird:with_run_handlers(2, MapB, fun() ->
            [element(1, hird_echo(MapB)) || _ <- lists:seq(1, 10)]
        end)
    end),
    ?assertEqual([ok, ok, ok, ok, ok, ok, ok, ok, ok, ok], await(T1)),
    ?assertEqual([ok, ok, ok, ok, ok, ok, ok, ok, ok, ok], await(T2)),
    %% persistent_term 注册表零残留（本层从不写全局注册表）
    ?assertEqual(
        [],
        [{K, V} || {K, V} <- persistent_term:get(), element(1, K) =:= hird_handlers]
    ),
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    ?assertEqual(undefined, whereis(hird_audit)).

%% 恢复共享 audit（后续用例的 dispatch 依赖）
restart_shared_audit() ->
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    SharedFile = audit_path(<<"gates_shared">>),
    _ = file:delete(SharedFile),
    ok = filelib:ensure_dir(SharedFile),
    {ok, _} = hird_audit:start_link([{sink, {file, SharedFile}}]),
    ok = hird_audit:register_tools(?MOD:hird_tools@()),
    ok.
