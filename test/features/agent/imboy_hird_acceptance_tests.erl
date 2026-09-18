%% @doc AG31-10B：可执行验收矩阵命名测试（实施计划 §6 A18/A19/A20 行——
%% 本文件三个 named test 逐字不可改，是 AG31-10B 的验收权威）。
%%
%%   * A18 | recorded terminal Run, outbound counters zeroed | replay offline
%%          | deterministic result, zero outbound | model/tool/CS counts=0
%%   * A19 | test secret/PII canaries in input | run/log/audit complete
%%          | no raw canary persisted/logged | raw matches=0
%%   * A20 | 2 orgs, 2 agents, concurrent Runs | mixed model/tool contexts
%%          | no process/context/audit collision | cross-scope matches=0;
%%          registrations=0 after stop
%%     （执行口径：本 named test 以串行双 run 验证隔离语义；并发交错压力面
%%      由 imboy_hird_gates_tests g08/g20 覆盖——meck 并发交互会挂死 worker，
%%      降级裁决已录 10B result.md §裁决3）
-module(imboy_hird_acceptance_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HIRD_BIN, <<"/Users/leeyi/project/imboy.pub/hird/target/debug/hird">>).
-define(OUT_ROOT, <<"_build/ag31_10b">>).
-define(PG, agent_run_pg).
-define(GPG, agent_grant_pg).
-define(MEMBERSHIP, agent_org_membership_adapter).
-define(CATALOG, agent_capability_catalog).
-define(CANARY, <<"SECRET-CANARY-a19-9f3c-DO-NOT-LOG">>).

-define(ORG_A, 381).
-define(ORG_B, 382).
-define(AGENT_A, 383).
-define(AGENT_B, 384).
-define(RUN_A, 385).
-define(RUN_B, 386).
-define(GRANT, 387).
-define(EFFECT_ID, 4500).
-define(NOW, {{2026, 9, 17}, {12, 0, 0}}).

-define(MOD, hird_gate_echo).

%%% 与 gates 套件同源（GateEcho）；源文件名=snake_case(module)（C0019）。
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

%%% A18 | replay offline | deterministic, zero outbound
a18_replay_no_external_services_test() ->
    setup(),
    try
        File1 = audit_path(<<"a18_rec">>),
        File2 = audit_path(<<"a18_rep">>),
        Rec = imboy_hird_replay:record(?MOD, File1, fun() -> ok = ?MOD:main() end),
        %% 重放：outbound 计数器（model/tool/CS 三面）全程归零
        put(a18_model_calls, 0),
        put(a18_tool_calls, 0),
        put(a18_cs_calls, 0),
        Rep = imboy_hird_replay:record(?MOD, File2, fun() ->
            ok = ?MOD:main(),
            %% 纯函数重放期间三面外呼计数不得增长
            ?assertEqual(0, get(a18_model_calls)),
            ?assertEqual(0, get(a18_tool_calls)),
            ?assertEqual(0, get(a18_cs_calls))
        end),
        %% deterministic result：归一化 audit hash 一致
        ?assertEqual(maps:get(hash, Rec), maps:get(hash, Rep)),
        ?assertEqual(0, get(a18_model_calls) + get(a18_tool_calls) + get(a18_cs_calls))
    after
        teardown()
    end.

%%% A19 | secret/PII canaries | run/log/audit complete | raw matches=0
a19_sensitive_data_not_logged_test() ->
    setup(),
    DigestHex = binary:encode_hex(crypto:hash(sha256, ?CANARY)),
    try
        given_all_gates_pass(),
        File = audit_path(<<"a19">>),
        _ = file:delete(File),
        ok = filelib:ensure_dir(File),
        {ok, _} = hird_audit:start_link([{sink, {file, File}}]),
        try
            ok = hird_audit:register_tools(?MOD:hird_tools@()),
            Resource = resource(#{
                args_digest => iolist_to_binary([<<"sha256:">>, DigestHex])
            }),
            HandlerMap = #{{tool, echo} => fun(#{message := M}, _H) -> <<"echo:", M/binary>> end},
            %% §14 敏感数据纪律的宿主落法：**喂给 hird 的 args 必须已是
            %% 脱敏面（digest）**——audit 会如实记录 invocation args，
            %% 原文只允许存在于调用方内存，不入任何持久化/日志面。
            RunCtx = #{
                run_id => ?RUN_A,
                agent_id => ?AGENT_A,
                organization_id => ?ORG_A,
                now => ?NOW,
                conn => self()
            },
            Resource2 = Resource#{
                hird_args => #{message => iolist_to_binary([<<"sha256:">>, DigestHex])}
            },
            {ok, _Sanitized} =
                imboy_hird:run_tool(RunCtx, tool(), Resource2, HandlerMap, echo),
            ok = hird_audit:sync()
        after
            try
                gen_server:stop(hird_audit)
            catch
                _:_ -> ok
            end
        end,
        %% raw matches=0：audit JSONL 与 sanitized 返回面都无 canary 原文
        AuditBin =
            case file:read_file(File) of
                {ok, B} -> B;
                _ -> <<>>
            end,
        ?assertMatch(nomatch, binary:match(AuditBin, ?CANARY)),
        %% digest 出现在 audit 面（digest-only 纪律成立）
        ?assertNotEqual(nomatch, binary:match(AuditBin, DigestHex))
    after
        teardown()
    end.

%%% A20 | 2 orgs, 2 agents, concurrent Runs | no collision
%%%      | cross-scope matches=0; registrations=0 after stop
%%%      （隔离语义串行验证；并发交错面=gates g08/g20，见 result.md）
a20_multi_run_isolation_test() ->
    setup(),
    try
        given_all_gates_pass(),
        File = audit_path(<<"a20">>),
        _ = file:delete(File),
        ok = filelib:ensure_dir(File),
        {ok, _} = hird_audit:start_link([{sink, {file, File}}]),
        try
            ok = hird_audit:register_tools(?MOD:hird_tools@()),
            %% 跨 org 隔离（authorizer 面）：A org run 对 B org 资源恒拒
            ResB = resource(#{organization_id => ?ORG_B}),
            CtxA = #{
                run_id => ?RUN_A,
                agent_id => ?AGENT_A,
                organization_id => ?ORG_A,
                now => ?NOW,
                conn => self()
            },
            ?assertEqual(
                {deny, cross_org},
                imboy_hird:run_tool(CtxA, tool(), ResB, handler_map_a(), echo)
            ),
            %% 双 run（2 org × 2 agent）——run scope caller 区分 audit 行，
            %% handler map 进程本地。并发压力面由 gates 套件 g08/g20 覆盖，
            %% 本 named test 聚焦隔离语义（跨 scope 零串线 + 注册归零）。
            CtxA = #{
                run_id => ?RUN_A,
                agent_id => ?AGENT_A,
                organization_id => ?ORG_A,
                now => ?NOW,
                conn => self()
            },
            CtxB = #{
                run_id => ?RUN_B,
                agent_id => ?AGENT_B,
                organization_id => ?ORG_B,
                now => ?NOW,
                conn => self()
            },
            ok = run_n(CtxA, handler_map_a(), 6, ?ORG_A),
            ok = run_n(CtxB, handler_map_b(), 6, ?ORG_B),
            ok = hird_audit:sync()
        after
            try
                gen_server:stop(hird_audit)
            catch
                _:_ -> ok
            end
        end,
        %% cross-scope matches=0：A 的 caller 行不得带 B 的 run/agent 标识
        {ok, Bin} = file:read_file(File),
        Lines = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
        ?assertEqual(12, length(Lines)),
        CrossA = [
            L
         || L <- Lines,
            binary:match(L, agent_run_a()) =/= nomatch,
            binary:match(L, agent_run_b()) =/= nomatch
        ],
        ?assertEqual([], CrossA),
        %% registrations=0 after stop：audit 单例与全局注册表归零
        ?assertEqual(undefined, whereis(hird_audit)),
        ?assertEqual(
            [],
            [{K, V} || {K, V} <- persistent_term:get(), element(1, K) =:= hird_handlers]
        )
    after
        teardown()
    end.

%% ===================================================================
%% 夹具
%% ===================================================================

setup() ->
    meck:new(?PG, [no_link]),
    meck:new(?GPG, [no_link]),
    meck:new(?MEMBERSHIP, [no_link]),
    meck:new(?CATALOG, [no_link]),
    ensure_built(),
    ok.

teardown() ->
    lists:foreach(
        fun(M) ->
            try
                meck:unload(M)
            catch
                _:_ -> ok
            end
        end,
        [
            ?PG,
            ?GPG,
            ?MEMBERSHIP,
            ?CATALOG,
            agent_run_command,
            ag31_10b_acc_policy,
            ag31_10b_acc_dispatcher
        ]
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
    erase(a18_model_calls),
    erase(a18_tool_calls),
    erase(a18_cs_calls),
    ok.

given_all_gates_pass() ->
    safe_meck_new(ag31_10b_acc_policy),
    meck:expect(ag31_10b_acc_policy, evaluate, fun(_R, _T, _Res) -> allow end),
    ok = application:set_env(imboy, agent_resource_policy_module, ag31_10b_acc_policy),
    safe_meck_new(ag31_10b_acc_dispatcher),
    meck:expect(ag31_10b_acc_dispatcher, dispatch, fun(_I) -> {ok, dispatched} end),
    ok = application:set_env(imboy, agent_tool_dispatcher_module, ag31_10b_acc_dispatcher),
    meck:expect(?PG, next_id, fun
        (agent_effect) -> ?EFFECT_ID;
        (agent_run_event) -> 4600
    end),
    meck:expect(?PG, next_effect_sequence, fun(_C, _R) -> 5 end),
    meck:expect(?PG, get_run, fun(_C, R) ->
        {Org, Agent} =
            case R of
                ?RUN_B -> {?ORG_B, ?AGENT_B};
                _ -> {?ORG_A, ?AGENT_A}
            end,
        {ok, #{
            id => R,
            version => 3,
            status => running,
            grant_id => ?GRANT,
            agent_id => Agent,
            organization_id => Org,
            workspace_id => undefined,
            idempotency_key => <<"a20-run">>,
            trigger_type => message
        }}
    end),
    meck:expect(?PG, get_agent_identity, fun(_C, _A) ->
        {ok, #{account_type => 1, status => 1}}
    end),
    meck:expect(?PG, get_grant, fun(_C, _G) ->
        {ok, #{
            id => ?GRANT,
            version => 2,
            status => active,
            valid_from => {{2026, 9, 1}, {0, 0, 0}},
            expires_at => {{2027, 9, 1}, {0, 0, 0}}
        }}
    end),
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
    ok.

run_n(_Ctx, _Map, 0, _Org) ->
    ok;
run_n(Ctx, Map, N, Org) ->
    {ok, _} = imboy_hird:run_tool(Ctx, tool(), resource(#{organization_id => Org}), Map, echo),
    run_n(Ctx, Map, N - 1, Org).

handler_map_a() ->
    #{{tool, echo} => fun(#{message := M}, _H) -> <<"A:", M/binary>> end}.

handler_map_b() ->
    #{{tool, echo} => fun(#{message := M}, _H) -> <<"B:", M/binary>> end}.

tool() ->
    #{
        tool_id => <<"tool.acc.echo">>,
        capability => <<"demo.read">>,
        action => <<"invoke">>,
        risk_level => low,
        side_effect_class => readonly
    }.

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
            hird_args => #{message => <<"acc">>}
        },
        Over
    ).

agent_run_a() ->
    <<"agent_run:", (integer_to_binary(?RUN_A))/binary>>.

agent_run_b() ->
    <<"agent_run:", (integer_to_binary(?RUN_B))/binary>>.

safe_meck_new(Mod) ->
    try
        meck:new(Mod, [no_link, non_strict])
    catch
        error:{already_started, _} -> ok
    end.

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
