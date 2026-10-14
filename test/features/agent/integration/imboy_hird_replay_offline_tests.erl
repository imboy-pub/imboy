%% @doc IMBOY_HIRD_AUTH_REPLAY_POC_V1_CLOSEOUT — 回放语义与护栏的离线验证。
%%
%% C2 增补门禁的**不依赖 PG**执行面：调用差异与 cursor 生命周期语义与
%% 日志来源无关——以 `hird_types:encode_invocation/2`（与 audit 相同的
%% 类型化编码器）离线构造格式合法日志，端到端语义与 P1 授权链录制一致
%% （录制链端到端由 P2 承担）。
%%
%% 覆盖（C2 门禁映射）：
%%   #3  次序漂移        → t_call_order_divergence（逆序程序变体）
%%   #4  运行时工具名漂移 → t_runtime_tool_mismatch（同签名别名变体，
%%       加载期放行、调用匹配期失败——区别于 P2 t_tool_mismatch 的
%%       加载期签名不兼容）
%%   #7  schema 版本门    → t_schema_version_gate（加载期显式拒绝）
%%   #10 cursor 运行中死亡 → t_cursor_death（monitor 确定性触发 + 宿主
%%       回放护栏在资源解析/授权之前拦截 live 回退；trap+atomics 零执行）
%%   C3  effect 凭证绑定  → t_effect_credential_binding（拒绝面 + trap
%%       边界信号；真实读取的正向对照在 P1，需 PG）
%%   C5  模型驱动决策步  → t_model_decision_finish（决策 Finish 时业务
%%       Tool 零执行）/ t_model_decision_callws（决策 CallWs 时恰一次
%%       调用）——类型化 Decision ADT + match 分派（Provider 返回封闭
%%       ADT，无法表达签名外工具请求——类型层拒绝证据）；真实授权链下
%%       的循环验证在 P1/PG（BLOCKED 口径不变，本组为零 PG 前置）
%%
%% 运行前置：IMBOY_HIRD_BIN（无 HIRDPOC_PG_* —— 本套件零 PG、零 marker 库）。
-module(imboy_hird_replay_offline_tests).

-include_lib("eunit/include/eunit.hrl").

-define(P1, imboy_hird_p1_auth_tool_tests).
-define(MOD, ?P1:mod()).
-define(OUT_ROOT,
    unicode:characters_to_binary(
        os:getenv("IMBOY_HIRD_OUT_ROOT", "_build/hird_poc_v1/prog")
    )
).
-define(STATE_KEY, {imboy_hird_replay_offline, state}).
-define(TRAP_HANDLER, fun imboy_hird_poc_registry:model_handler/2).

%%%===================================================================
%%% 套件
%%%===================================================================

offline_replay_test_() ->
    {timeout, 600,
        {foreach, fun setup/0, fun cleanup/1, [
            {timeout, 120,
                {~B'运行时工具名漂移（同签名别名变体）：加载放行、调用匹配期 tool_mismatch', fun t_runtime_tool_mismatch/0}},
            {timeout, 120, {~B'可区分调用次序漂移（逆序变体）：首个调用即匹配期失败', fun t_call_order_divergence/0}},
            {timeout, 120,
                {~B'schema 版本不兼容：加载期 unsupported_schema_version 拒绝', fun t_schema_version_gate/0}},
            {timeout, 120, {~B'cursor 运行中死亡：护栏拦截 live 回退，真实执行零次', fun t_cursor_death/0}},
            {timeout, 120,
                {~B'effect 凭证资源绑定：窗口内跨资源拒绝、匹配凭证到达执行边界', fun t_effect_credential_binding/0}},
            {timeout, 120, {~B'模型决策 Finish（不请求工具）：业务 Tool 零执行', fun t_model_decision_finish/0}},
            {timeout, 120, {~B'模型决策 CallWs（请求工具）：业务 Tool 恰一次调用', fun t_model_decision_callws/0}}
        ]}}.

%%%===================================================================
%%% setup / cleanup（零 PG：只需 hird 编译器与生成程序产物）
%%%===================================================================

setup() ->
    os:getenv("IMBOY_HIRD_BIN") =/= false orelse
        erlang:error({offline_missing_env, "IMBOY_HIRD_BIN"}),
    ok = ?P1:ensure_built(),
    Counters = #{
        biz => atomics:new(1, [{signed, false}]),
        provider => atomics:new(1, [{signed, false}])
    },
    %% conn 占位：本套件不发生真实读取（凭证拒绝在业务边界之前；trap 在
    %% 边界处 error）——仅满足 registry ctx 形状。
    ok = imboy_hird_poc_registry:install(#{
        conn => self(),
        biz_counter => maps:get(biz, Counters),
        provider_counter => maps:get(provider, Counters),
        trap => false
    }),
    persistent_term:put(?STATE_KEY, #{counters => Counters}),
    ok.

cleanup(_) ->
    imboy_hird_host_bridge:disarm_replay_guard(),
    imboy_hird_poc_registry:teardown(),
    stop_cursor(),
    stop_sup(),
    persistent_term:erase(?STATE_KEY),
    ok.

state() -> persistent_term:get(?STATE_KEY).
biz_calls() -> imboy_hird_poc_registry:biz_calls().
provider_calls() -> imboy_hird_poc_registry:provider_calls().

%%%===================================================================
%%% 离线日志 / 程序变体 / 回放编排
%%%===================================================================

%% 与 P1 授权链录制轨迹同形的两行（同编码器、同签名表；值同 P1 scripted）。
offline_log() ->
    Table = ?MOD:hird_tools@(),
    Records = [
        #{
            tool => model_chat,
            args => #{prompt => <<"plan">>},
            result => {ok, <<"scripted-model-reply:plan">>},
            timestamp => 0,
            caller => <<"Offline.t">>
        },
        #{
            tool => ws_detail,
            args => #{ws_id => 912001},
            result => {ok, <<"{\"id\":912001,\"name\":\"poc-ws-alpha\",\"owner_id\":910002}">>},
            timestamp => 0,
            caller => <<"Offline.t">>
        }
    ],
    Lines = [hird_types:encode_invocation(R, Table) || R <- Records],
    Tag = integer_to_binary(erlang:unique_integer([positive])),
    Path = filename:join([?OUT_ROOT, "audit", <<"offline_", Tag/binary, ".jsonl">>]),
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, [[L, $\n] || L <- Lines]),
    Path.

variant_mod(alias) -> hird_imboy_agent_probe_alias;
variant_mod(reversed) -> hird_imboy_agent_probe_reversed;
variant_mod(loop) -> hird_imboy_agent_loop.

%% 别名变体：WsDetailAlias 与 WsDetail 同签名——离线日志（ws_detail 行）
%% 可被变体签名表解码（加载期放行），程序体只调 alias → 运行时
%% tool_mismatch。逆序变体：签名同原程序（加载无碍），调用次序逆转。
variant_src(alias) ->
    <<"""
    module ImboyAgentProbeAlias

    tool ModelChat : { prompt: String } → String
    tool WsDetail : { ws_id: Int } → String ! {Exn<AgentToolError>}
    tool WsDetailAlias : { ws_id: Int } → String ! {Exn<AgentToolError>}

    type AgentToolError = ToolDenied(String)
    type ProbeStatus = ProbeStatus(Int)
    type alias ProbeState = { steps: Int }

    actor ProbeRunner {
      state: ProbeState,

      message: ProbeMsg =
        | ProbeRun
        | ProbeDone(ReplyTo<ProbeStatus>)
        | Shutdown,

      init: fn(cfg: Int) ! {} = { steps: 0 },

      handle ProbeRun, st ! {Tool<ModelChat>, Tool<WsDetailAlias>, Exn<AgentToolError>} =
        let plan = model_chat({ prompt: "plan" }) in
        let detail = ws_detail_alias({ ws_id: 912001 }) in
        Continue({ steps: st.steps + 1, ..st }),

      handle ProbeDone(reply_to), st ! {Send<ProbeStatus>} =
        reply(reply_to, ProbeStatus(st.steps));
        Continue(st),

      handle Shutdown, _ ! {} = Stop,
    } ! {Tool<ModelChat>, Tool<WsDetailAlias>, Exn<AgentToolError>, Send<ProbeStatus>}

    supervisor ProbeSup {
      strategy: one_for_one,
      intensity: 5,
      period: 60,
      children: [
        { id: probe_runner, actor: ProbeRunner, start_args: 0, restart: transient },
      ]
    }

    fn main() → () ! {Supervise, Send<ProbeMsg>, Await<ProbeStatus>} =
      supervise(ProbeSup);
      let r = child(ProbeSup, probe_runner) in
      send(r, ProbeRun);
      let ProbeStatus(n) = request(r, ProbeDone) in
      send(r, Shutdown);
      if n >= 0 then () else crash!("no steps")
    """/utf8>>;
variant_src(reversed) ->
    <<"""
    module ImboyAgentProbeReversed

    tool ModelChat : { prompt: String } → String
    tool WsDetail : { ws_id: Int } → String ! {Exn<AgentToolError>}

    type AgentToolError = ToolDenied(String)
    type ProbeStatus = ProbeStatus(Int)
    type alias ProbeState = { steps: Int }

    actor ProbeRunner {
      state: ProbeState,

      message: ProbeMsg =
        | ProbeRun
        | ProbeDone(ReplyTo<ProbeStatus>)
        | Shutdown,

      init: fn(cfg: Int) ! {} = { steps: 0 },

      handle ProbeRun, st ! {Tool<WsDetail>, Tool<ModelChat>, Exn<AgentToolError>} =
        let detail = ws_detail({ ws_id: 912001 }) in
        let plan = model_chat({ prompt: "plan" }) in
        Continue({ steps: st.steps + 1, ..st }),

      handle ProbeDone(reply_to), st ! {Send<ProbeStatus>} =
        reply(reply_to, ProbeStatus(st.steps));
        Continue(st),

      handle Shutdown, _ ! {} = Stop,
    } ! {Tool<WsDetail>, Tool<ModelChat>, Exn<AgentToolError>, Send<ProbeStatus>}

    supervisor ProbeSup {
      strategy: one_for_one,
      intensity: 5,
      period: 60,
      children: [
        { id: probe_runner, actor: ProbeRunner, start_args: 0, restart: transient },
      ]
    }

    fn main() → () ! {Supervise, Send<ProbeMsg>, Await<ProbeStatus>} =
      supervise(ProbeSup);
      let r = child(ProbeSup, probe_runner) in
      send(r, ProbeRun);
      let ProbeStatus(n) = request(r, ProbeDone) in
      send(r, Shutdown);
      if n >= 0 then () else crash!("no steps")
    """/utf8>>;
variant_src(loop) ->
    <<"""
    module ImboyAgentLoop

    tool ModelDecide : { step: Int } → Decision ! {Exn<AgentToolError>}
    tool WsDetail : { ws_id: Int } → String ! {Exn<AgentToolError>}

    type Decision = | CallWs(Int) | Finish(String)
    type AgentToolError = ToolDenied(String)
    type LoopStatus = LoopStatus(Int, String)
    type alias LoopState = { steps: Int, answer: String }

    actor LoopRunner {
      state: LoopState,

      message: LoopMsg =
        | LoopStart
        | LoopDone(ReplyTo<LoopStatus>)
        | Shutdown,

      init: fn(cfg: Int) ! {} = { steps: 0, answer: "" },

      handle LoopStart, st ! {Tool<ModelDecide>, Tool<WsDetail>, Exn<AgentToolError>} =
        let d = model_decide({ step: st.steps }) in
        match d {
          CallWs(id) →
            let detail = ws_detail({ ws_id: id }) in
            Continue({ steps: st.steps + 1, answer: detail }),
          Finish(a) →
            Continue({ steps: st.steps + 1, answer: a }),
        },

      handle LoopDone(reply_to), st ! {Send<LoopStatus>} =
        reply(reply_to, LoopStatus(st.steps, st.answer));
        Continue(st),

      handle Shutdown, _ ! {} = Stop,
    } ! {Tool<ModelDecide>, Tool<WsDetail>, Exn<AgentToolError>, Send<LoopStatus>}

    supervisor LoopSup {
      strategy: one_for_one,
      intensity: 5,
      period: 60,
      children: [
        { id: loop_runner, actor: LoopRunner, start_args: 0, restart: transient },
      ]
    }

    fn main() → () ! {Supervise, Send<LoopMsg>, Await<LoopStatus>} =
      supervise(LoopSup);
      let r = child(LoopSup, loop_runner) in
      send(r, LoopStart);
      let LoopStatus(n, a) = request(r, LoopDone) in
      send(r, Shutdown);
      if n >= 0 then () else crash!("bad steps")
    """/utf8>>.

ensure_variant_built(Kind) ->
    OutDir = filename:join([?OUT_ROOT, "out_variant_" ++ atom_to_list(Kind)]),
    Mod = variant_mod(Kind),
    case os:getenv("IMBOY_HIRD_FORCE_REBUILD") of
        "1" ->
            _ = file:del_dir_r(binary_to_list(OutDir)),
            ok;
        _ ->
            ok
    end,
    Beam = filename:join([OutDir, atom_to_list(Mod) ++ ".beam"]),
    case filelib:is_regular(Beam) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(filename:join([OutDir, "x"])),
            %% C0019：module 名与 .hird 文件名须一致（snake_case 归一化）。
            Src = filename:join([
                ?OUT_ROOT,
                %% C0019：文件名 = 模块名去 hird_ 前缀（snake_case 与
                %% module 声明一一对应，跨变体统一成立）。
                (fun(M) ->
                    case M of
                        "hird_" ++ Rest -> Rest;
                        Other -> Other
                    end
                end)(
                    atom_to_list(variant_mod(Kind))
                ) ++ ".hird"
            ]),
            ok = file:write_file(Src, variant_src(Kind)),
            Cmd = io_lib:format(
                "\"~ts\" build \"~ts\" --out-dir \"~ts\" 2>&1",
                [os:getenv("IMBOY_HIRD_BIN", "hird"), Src, OutDir]
            ),
            Output = os:cmd(Cmd),
            case filelib:is_regular(Beam) of
                true -> ok;
                false -> erlang:error({hird_variant_build_failed, Kind, Output})
            end
    end,
    %% 生成 actor/supervisor 模块名跨变体同名（派生自 actor 名）——purge
    %% 后按最新 path 加载，防 code server 缓存遮蔽本轮变体产物。
    purge_dir_beams(OutDir),
    true = code:add_patha(binary_to_list(OutDir)),
    case code:is_loaded(Mod) of
        {file, _} ->
            ok;
        false ->
            {module, Mod} = code:load_file(Mod),
            ok
    end,
    Mod.

purge_dir_beams(Dir) ->
    lists:foreach(
        fun(BeamName) ->
            M = list_to_atom(filename:rootname(BeamName)),
            case code:is_loaded(M) of
                {file, _} ->
                    code:delete(M),
                    code:purge(M),
                    ok;
                false ->
                    ok
            end
        end,
        filelib:wildcard("*.beam", binary_to_list(unicode:characters_to_binary(Dir)))
    ),
    ok.

%% 离线回放：cursor + 回放护栏 + trap 注册表 + 变体程序重跑 + finish。
%% 返回 {MainOutcome, Finish}。actor 崩溃 → transient 重启 → main 的
%% request 可能命中新 actor（ok）或重启窗口（caught）——形态不限。
offline_replay(LogFile, Mod) ->
    run_isolated(fun() ->
        {ok, _Cursor} = safe_start_link(LogFile, [Mod:hird_tools@()]),
        try
            true = hird_replay:active(),
            ok = imboy_hird_host_bridge:arm_replay_guard(),
            Outcome =
                try
                    ok = hird_handlers:with_handlers(
                        [{{tool, model_chat}, ?TRAP_HANDLER}],
                        fun() -> Mod:main(#{}) end
                    ),
                    ok
                catch
                    Class:Reason -> {caught, Class, Reason}
                end,
            Finish = hird_replay:finish(),
            {Outcome, Finish}
        after
            stop_sup(),
            stop_cursor(),
            imboy_hird_host_bridge:disarm_replay_guard()
        end
    end).

%% cursor init 失败的 EXIT 迟到补刀隔离（P2 同款）。
safe_start_link(Path, Tables) ->
    Parent = self(),
    Worker = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try hird_replay:start_link(Path, Tables) of
                Res -> Res
            catch
                Class:Reason -> {safe_raise, Class, Reason}
            end,
        receive
            {'EXIT', _, _} -> ok
        after 500 -> ok
        end,
        Parent ! {self(), R}
    end),
    receive
        {Worker, {safe_raise, C, R}} -> erlang:raise(C, R, []);
        {Worker, R} -> R
    after 5000 ->
        erlang:error({safe_start_link_timeout, Path})
    end.

%% 用例体隔离（P2 同款）：任意被链接进程的 EXIT 信号都被吸收。
run_isolated(Fun) ->
    Parent = self(),
    W = spawn(fun() ->
        process_flag(trap_exit, true),
        R =
            try
                {ok, Fun()}
            catch
                C:Rn -> {caught, C, Rn}
            end,
        absorb_exits(2),
        Parent ! {self(), R}
    end),
    receive
        {W, {ok, Result}} -> Result;
        {W, {caught, C, Rn}} -> erlang:raise(C, Rn, []);
        {W, Other} -> Other
    after 120000 ->
        erlang:error({isolated_case_timeout, W})
    end.

absorb_exits(0) ->
    ok;
absorb_exits(N) ->
    receive
        {'EXIT', _, _} -> absorb_exits(N - 1)
    after 200 -> ok
    end.

stop_cursor() ->
    try
        gen_server:stop(hird_replay)
    catch
        _:_ -> ok
    end,
    ok.

stop_sup() ->
    try
        gen_server:stop(hird_probe_sup)
    catch
        _:_ -> ok
    end,
    ok.

find_divergence_kind({replay_divergence, #{kind := K}}) ->
    {ok, K};
find_divergence_kind(T) when is_tuple(T) ->
    find_divergence_kind(tuple_to_list(T));
find_divergence_kind([H | T]) ->
    case find_divergence_kind(H) of
        {ok, V} -> {ok, V};
        error -> find_divergence_kind(T)
    end;
find_divergence_kind(_) ->
    error.

%%%===================================================================
%%% 用例
%%%===================================================================

%% C2#4：工具名漂移且签名兼容（类型合法变体）——加载期放行，调用匹配期
%% tool_mismatch（区别于 P2 t_tool_mismatch 的加载期签名不兼容）。
t_runtime_tool_mismatch() ->
    LogFile = offline_log(),
    AliasMod = ensure_variant_built(alias),
    {MainOutcome, Finish} = offline_replay(LogFile, AliasMod),
    %% 首个调用（ModelChat）匹配消费行 1；第二个调用（WsDetailAlias）与
    %% 行 2（WsDetail）工具名不匹配 → 匹配阶段 tool_mismatch、游标不前移。
    ?assertMatch({error, {replay_incomplete, #{consumed := 1}}}, Finish),
    case MainOutcome of
        ok -> ok;
        {caught, _, Reason} -> ?assertMatch({ok, tool_mismatch}, find_divergence_kind(Reason))
    end,
    ?assertEqual(0, provider_calls()),
    ?assertEqual(0, biz_calls()).

%% C2#3：两个可区分调用（ModelChat / WsDetail）的次序漂移——类型合法的
%% 程序变体（签名同原程序，日志加载无碍），首个调用即在匹配阶段失败。
t_call_order_divergence() ->
    LogFile = offline_log(),
    RevMod = ensure_variant_built(reversed),
    {MainOutcome, Finish} = offline_replay(LogFile, RevMod),
    ?assertMatch({error, {replay_incomplete, #{consumed := 0}}}, Finish),
    case MainOutcome of
        ok -> ok;
        {caught, _, Reason} -> ?assertMatch({ok, tool_mismatch}, find_divergence_kind(Reason))
    end,
    ?assertEqual(0, provider_calls()),
    ?assertEqual(0, biz_calls()).

%% C2#7：schema 版本不兼容在加载期显式拒绝（hird_types 版本门）。
t_schema_version_gate() ->
    LogFile = offline_log(),
    Lines = ?P1:read_lines(LogFile),
    Tmp = <<LogFile/binary, ".v2">>,
    ok = file:write_file(
        Tmp,
        [
            [L, $\n]
         || L <-
                lists:sublist(Lines, 1) ++
                    [
                        binary:replace(
                            lists:nth(2, Lines),
                            <<"\"schema_version\":1">>,
                            <<"\"schema_version\":2">>
                        )
                    ]
        ]
    ),
    ProvBefore = provider_calls(),
    BizBefore = biz_calls(),
    ?assertMatch(
        {error,
            {replay_load_error, #{
                line := 2,
                reason := {decode_error, {unsupported_schema_version, 2}}
            }}},
        safe_start_link(Tmp, [?MOD:hird_tools@()])
    ),
    ?assertNot(hird_replay:active()),
    ?assertEqual(ProvBefore, provider_calls()),
    ?assertEqual(BizBefore, biz_calls()).

%% C2#10：cursor 成功启动并完成至少一个匹配后运行中死亡——后续 dispatch
%% 回退 live 落到注册表桥，宿主回放护栏在资源解析/授权之前拦截；
%% trap+atomics 证明真实 Provider/业务执行为零，不存在静默恢复 live。
t_cursor_death() ->
    run_isolated(fun t_cursor_death_body/0).

t_cursor_death_body() ->
    LogFile = offline_log(),
    {ok, Cursor} = safe_start_link(LogFile, [?MOD:hird_tools@()]),
    true = hird_replay:active(),
    Ref = erlang:monitor(process, Cursor),
    ok = imboy_hird_host_bridge:arm_replay_guard(),
    try
        %% cursor 已启动并完成至少一个匹配（录制值原样回送）。
        R1 = hird_tool_dispatch:call(
            model_chat,
            <<"Offline.CursorDeath">>,
            #{},
            #{prompt => <<"plan">>}
        ),
        ?assertEqual(<<"scripted-model-reply:plan">>, R1),
        %% 确定性触发 cursor 退出并以 monitor 等待 DOWN（消息屏障，无 sleep）。
        ok = gen_server:stop(hird_replay),
        receive
            {'DOWN', Ref, process, Cursor, _Reason} -> ok
        after 5000 ->
            erlang:error(cursor_down_timeout)
        end,
        ?assertNot(hird_replay:active()),
        %% cursor 死后 active() 翻 false：dispatch 回退 live、注册表查询命中
        %% 桥——护栏拦截（拒绝发生在 resource_of/authorize/biz 之前）。
        Bridge = imboy_hird_host_bridge:bridge_handler(#{
            run_ctx => #{run_id => 914001},
            tool => ?P1:tool_desc(),
            hird_tool_name => ws_detail,
            resource_of => fun(_Args) -> erlang:error(resource_resolved_under_guard) end,
            biz => fun(_R, _E) -> erlang:error(biz_executed_under_guard) end
        }),
        ok = hird_handlers:with_handlers([{{tool, ws_detail}, Bridge}], fun() ->
            try
                _ = hird_tool_dispatch:call(
                    ws_detail,
                    <<"Offline.CursorDeath">>,
                    #{},
                    #{ws_id => 912001}
                ),
                erlang:error(cursor_death_guard_not_triggered)
            catch
                throw:{hird_exn, {tool_denied, <<"replay_guard_active">>}} -> ok
            end
        end),
        %% trap + 跨进程计数：真实 Provider / 业务执行为零（无静默 live 恢复）。
        ?assertEqual(0, biz_calls()),
        ?assertEqual(0, provider_calls())
    after
        erlang:demonitor(Ref, [flush]),
        imboy_hird_host_bridge:disarm_replay_guard()
    end.

%% C3：effect 凭证绑定目标资源——窗口内跨资源拒绝；匹配凭证恰好到达
%% 业务执行边界（trap 信号）；无凭证直调拒绝。真实读取的正向对照在 P1。
t_effect_credential_binding() ->
    {ok, Handler} = imboy_hird_poc_registry:lookup(<<"tool.workspace.detail">>),
    ReqA = #{workspace_id => 912001, organization_id => 911001},
    %% 无凭证直调 → 拒绝。
    ?assertEqual({error, no_effect_credential}, Handler(ReqA)),
    %% 凭证绑定 (911002, 912002)：请求 (911001, 912001) → 资源不匹配拒绝。
    ok = imboy_hird_poc_registry:set_effect(
        {1, 914001, #{organization_id => 911002, workspace_id => 912002}}
    ),
    ?assertEqual({error, effect_resource_mismatch}, Handler(ReqA)),
    %% 同 org 跨 ws 同样拒绝（绑定的是具体资源，非"任意授权过的 org"）。
    ok = imboy_hird_poc_registry:set_effect(
        {1, 914001, #{organization_id => 911001, workspace_id => 912002}}
    ),
    ?assertEqual({error, effect_resource_mismatch}, Handler(ReqA)),
    %% 匹配凭证 + trap 模式：凭证通过并**恰好到达**业务执行边界（边界
    %% 信号即执行尝试被拦的证明——真实读取在 trap 之后，计数恒零）。
    #{counters := #{biz := B, provider := P}} = state(),
    ok = imboy_hird_poc_registry:install(#{
        conn => self(), biz_counter => B, provider_counter => P, trap => true
    }),
    ok = imboy_hird_poc_registry:set_effect(
        {1, 914001, #{organization_id => 911001, workspace_id => 912001}}
    ),
    ?assertError(replay_must_not_execute_biz, Handler(ReqA)),
    ?assertEqual(0, biz_calls()),
    ok = imboy_hird_poc_registry:clear_effect(),
    %% 清凭证后恢复拒绝（恢复 trap=false 的基础面）。
    ok = imboy_hird_poc_registry:install(#{
        conn => self(), biz_counter => B, provider_counter => P, trap => false
    }),
    ?assertEqual({error, no_effect_credential}, Handler(ReqA)),
    ?assertEqual(0, biz_calls()).

%%%===================================================================
%%% C5 前置：模型驱动的工具决策步（类型化 Decision ADT + match 分派）
%%%===================================================================

%% 跑一次决策步程序：脚本化 Decision + 业务 stub（atomics 计数）。
%% 返回 {Outcome, WsCalls, DecideCalls}。
run_decision_step(DecideFun) ->
    run_isolated(fun() ->
        LoopMod = ensure_variant_built(loop),
        #{counters := #{biz := B, provider := P}} = state(),
        Decide = fun(_Args, _Hs) ->
            _ = atomics:add_get(P, 1, 1),
            DecideFun()
        end,
        WsStub = fun(_Args, _Hs) ->
            _ = atomics:add_get(B, 1, 1),
            <<"{\"id\":912001,\"stub\":true}">>
        end,
        Outcome =
            try
                ok = hird_handlers:with_handlers(
                    [{{tool, model_decide}, Decide}, {{tool, ws_detail}, WsStub}],
                    fun() -> LoopMod:main(#{}) end
                ),
                ok
            catch
                Class:Reason -> {caught, Class, Reason}
            end,
        try
            gen_server:stop(hird_loop_sup)
        catch
            _:_ -> ok
        end,
        {Outcome, atomics:get(B, 1), atomics:get(P, 1)}
    end).

%% C5#1：模型决策 Finish（不请求工具）→ 业务 Tool 零执行（对照旧固定
%% 流程程序：无条件调用 ws_detail，见 P1 t_provider_variant）。
t_model_decision_finish() ->
    {Outcome, WsCalls, DecideCalls} =
        run_decision_step(fun() -> {finish, <<"all done">>} end),
    ?assertEqual(ok, Outcome),
    ?assertEqual(1, DecideCalls),
    ?assertEqual(0, WsCalls).

%% C5#2：模型决策 CallWs（请求工具）→ 业务 Tool 恰一次调用（离线 stub
%% 面；经真实授权链的同路径在 P1/PG，BLOCKED 口径不变）。
t_model_decision_callws() ->
    {Outcome, WsCalls, DecideCalls} =
        run_decision_step(fun() -> {call_ws, 912001} end),
    ?assertEqual(ok, Outcome),
    ?assertEqual(1, DecideCalls),
    ?assertEqual(1, WsCalls).
