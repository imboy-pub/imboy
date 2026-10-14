%% @doc IMBOY_HIRD_AUTH_REPLAY_POC_V1 — P1：Hirð 生成程序（actor 实际发出的
%% 业务 tool 调用）→ 宿主注册表桥 → 真实授权器（十步 fail-closed，一次性
%% marker 库真 PG）→ 真实只读业务读取（workspace_repo:internal_find_tx）。
%%
%% 与 gates/acceptance（全 meck）的本质区别：本套件零 meck——membership、
%% grant、run、catalog、policy、业务读取全部走真实实现 + 真实表。
%% 能力面经测试内显式 env 启用单一工具；生产默认（空目录 + 恒拒策略）由
%% default_off 用例验证仍然关闭。
%%
%% 运行前置（no-skip 铁律，缺失即 FAIL）：
%%   HIRDPOC_PG_HOST/_PG_PORT/_PG_DB/_PG_USER/_PG_PASSWORD[/_PG_MAINT_DB]
%%   IMBOY_HIRD_BIN（P0 重建编译器）
%% 隔离：一次性 marker 库（inttest_marker_db，VM 内 reuse 共享）+ 专用测试
%% VM；hird_handlers 注册表 / audit 单例为节点全局——套件内串行（P2 作用域
%% 纪律）。
%%
%% deny 场景形态说明：桥以域错误 throw {hird_exn, _} 回传（ADR-016）→
%% actor gen_server 崩溃 → transient 重启；main 的 request(ProbeDone) 可能
%% 命中重启后的新 actor（steps=0）正常返回，也可能在重启窗口 noproc 崩溃
%% ——两种形态都接受，用例断言核心语义（业务执行 0 次 + audit {err,_} 行）。
-module(imboy_hird_p1_auth_tool_tests).

-include_lib("eunit/include/eunit.hrl").

%% P2/P3 套件复用（同 VM 串行口径下的 fixture/程序编排公共面）。
-ifdef(TEST).
-export([
    ensure_env/0,
    ensure_tsid/0,
    ensure_built/0,
    given_full_fixture/1,
    reset_tables/1,
    tool_desc/0,
    resource_of/1,
    biz_of/1,
    run_ctx/3,
    run_program/3,
    audit_file/0,
    open_audit/1,
    stop_audit/0,
    stop_supervisor/0,
    read_audit/1,
    read_lines/1,
    deny_err/1,
    install_poc_env/0,
    uninstall_poc_env/0,
    mod/0
]).
-endif.

-define(HIRD_BIN, os:getenv("IMBOY_HIRD_BIN", "hird")).
-define(OUT_ROOT,
    unicode:characters_to_binary(
        os:getenv("IMBOY_HIRD_OUT_ROOT", "_build/hird_poc_v1/prog")
    )
).
-define(MOD, hird_imboy_agent_probe).
-define(SUP_NAME, hird_probe_sup).

%% fixture 合成标识（小整数区，避开 TSID 生成段）。
-define(AGENT_UID, 910001).
-define(HUMAN_UID, 910002).
-define(ORG_A, 911001).
-define(ORG_B, 911002).
-define(WS_A, 912001).
-define(WS_B, 912002).
-define(GRANT_A, 913001).
-define(GRANT_B, 913002).
-define(RUN_A, 914001).
-define(RUN_B, 914002).
-define(NOW, {{2026, 10, 13}, {6, 0, 0}}).
-define(EV_ID, 915001).
-define(STATE_KEY, {imboy_hird_p1, state}).

-define(HIRD_SRC, <<"""
module ImboyAgentProbe

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

  handle ProbeRun, st ! {Tool<ModelChat>, Tool<WsDetail>, Exn<AgentToolError>} =
    let plan = model_chat({ prompt: "plan" }) in
    let detail = ws_detail({ ws_id: 912001 }) in
    Continue({ steps: st.steps + 1, ..st }),

  handle ProbeDone(reply_to), st ! {Send<ProbeStatus>} =
    reply(reply_to, ProbeStatus(st.steps));
    Continue(st),

  handle Shutdown, _ ! {} = Stop,
} ! {Tool<ModelChat>, Tool<WsDetail>, Exn<AgentToolError>, Send<ProbeStatus>}

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
"""/utf8>>).

%%%===================================================================
%%% 套件
%%%===================================================================

p1_test_() ->
    {timeout, 900,
        {foreach, fun setup/0, fun cleanup/1, [
            {~B'合法请求：生成程序实际调用→授权 allow→真实读取，业务恰执行一次', fun t_legal/0},
            {~B'无 Grant capability：deny，业务执行零次，域错误回传', fun t_not_granted/0},
            {~B'Grant 撤销提交完成后：新请求 deny，业务执行零次', fun t_revoked/0},
            {~B'跨组织资源：cross_org deny，不泄露目标数据', fun t_cross_org/0},
            {~B'伪造执行上下文（不存在的 Run）：deny，业务执行零次', fun t_forged_ctx/0},
            {~B'未知 capability（未注册工具）：deny，业务执行零次', fun t_unknown_tool/0},
            {~B'绕过授权链直调 registry handler：无凭证拒绝，业务读取零次', fun t_bypass_direct/0},
            {~B'作用域内绕过桥直调 dispatch：仍走真实授权', fun t_bypass_dispatch/0},
            {~B'run_tool 同工具递归防护：HandlerMap 缺目标键即拒绝', fun t_recursion_guard/0},
            {~B'同组织跨 Workspace（grant 未授权）：grant_scope_mismatch 拒', fun t_same_org_other_ws/0},
            {~B'effect 凭证资源绑定：窗口内跨资源请求拒绝', fun t_effect_resource_binding/0},
            {~B'Provider 变体（纯文本回复）：业务 Tool 仍被固定执行（C5 如实取证）', fun t_provider_variant/0},
            {~B'默认关闭：不配 PoC env 时目录空集+策略恒拒，业务执行零次', fun t_default_off/0}
        ]}}.

%%%===================================================================
%%% setup / cleanup
%%%===================================================================

setup() ->
    ensure_env(),
    ensure_tsid(),
    ensure_built(),
    State = inttest_marker_db:provision(#{
        env_prefix => <<"HIRDPOC">>,
        reuse => true
    }),
    ok = imboy_hird_poc_membership:install(maps:get(conn, State)),
    ok = install_poc_env(),
    Counters = #{
        biz => atomics:new(1, [{signed, false}]),
        provider => atomics:new(1, [{signed, false}])
    },
    ok = imboy_hird_poc_registry:install(#{
        conn => maps:get(conn, State),
        biz_counter => maps:get(biz, Counters),
        provider_counter => maps:get(provider, Counters),
        trap => false
    }),
    persistent_term:put(?STATE_KEY, State#{counters => Counters}),
    ok.

state() ->
    persistent_term:get(?STATE_KEY).

cleanup(_) ->
    State = persistent_term:get(?STATE_KEY, undefined),
    imboy_hird_poc_membership:teardown(),
    imboy_hird_poc_registry:teardown(),
    uninstall_poc_env(),
    stop_supervisor(),
    stop_audit(),
    persistent_term:erase(?STATE_KEY),
    %% reuse 共享：release 走引用计数（State 含 reuse_key，原样传递）。
    case State of
        undefined -> ok;
        _ -> inttest_marker_db:release(State)
    end.

install_poc_env() ->
    ok = application:set_env(imboy, agent_capability_catalog_module, imboy_hird_poc_catalog),
    ok = application:set_env(imboy, agent_resource_policy_module, imboy_hird_poc_policy),
    ok = application:set_env(imboy, agent_native_tool_registry_module, imboy_hird_poc_registry),
    ok = application:set_env(imboy, agent_membership_module, imboy_hird_poc_membership),
    ok.

uninstall_poc_env() ->
    [
        ok = application:unset_env(imboy, K)
     || K <- [
            agent_capability_catalog_module,
            agent_resource_policy_module,
            agent_native_tool_registry_module,
            agent_membership_module
        ]
    ],
    ok.

ensure_env() ->
    Missing = [
        K
     || K <- [
            "HIRDPOC_PG_HOST",
            "HIRDPOC_PG_PORT",
            "HIRDPOC_PG_DB",
            "HIRDPOC_PG_USER",
            "IMBOY_HIRD_BIN"
        ],
        os:getenv(K) =:= false
    ],
    case Missing of
        [] -> ok;
        _ -> erlang:error({poc_missing_env, Missing})
    end,
    %% C4 CLOSEOUT：显式 PoC 测试连接配置作 marker_db 的 Defaults 兜底
    %% （本机非生产实例、trust 无密码）。实际连接目标恒由 HIRDPOC_PG_*
    %% env 决定；本配置不来自任何日常/生产配置文件解析。
    ok = application:set_env(imboy, pg_conf, #{
        start_mfa =>
            {epgsql, connect, [
                #{
                    host => os:getenv("HIRDPOC_PG_HOST", "127.0.0.1"),
                    port => list_to_integer(os:getenv("HIRDPOC_PG_PORT", "5432")),
                    username => os:getenv("HIRDPOC_PG_USER", "leeyi"),
                    password => os:getenv("HIRDPOC_PG_PASSWORD", "")
                }
            ]}
    }),
    ok.

ensure_tsid() ->
    try
        _ = elib_tsid:generate(default),
        ok
    catch
        _:_ ->
            ok = elib_tsid:init(#{dc_id => 0, node_id => 11})
    end.

%%%===================================================================
%%% hird 产物构建（强制重建：BEAM 必须来自本轮编译器）
%%%===================================================================

ensure_built() ->
    OutDir = filename:join([?OUT_ROOT, "out_probe"]),
    case os:getenv("IMBOY_HIRD_FORCE_REBUILD") of
        "1" ->
            _ = file:del_dir_r(binary_to_list(OutDir)),
            ok;
        _ ->
            ok
    end,
    Beam = filename:join([OutDir, atom_to_list(?MOD) ++ ".beam"]),
    case filelib:is_regular(Beam) of
        true -> ok;
        false -> build(OutDir)
    end,
    %% CLOSEOUT：code server 不复用旧/异源 BEAM——生成 actor/supervisor
    %% 模块名跨程序变体同名（派生自 actor 名），前序用例加载的旧版会
    %% 遮蔽本轮目录产物（P0 F1 陷阱的 code 层补全）。
    purge_dir_beams(OutDir),
    true = code:add_patha(binary_to_list(OutDir)),
    case code:is_loaded(?MOD) of
        {file, _} ->
            ok;
        false ->
            {module, ?MOD} = code:load_file(?MOD),
            ok
    end.

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

build(OutDir) ->
    ok = filelib:ensure_dir(filename:join([OutDir, "x"])),
    Src = filename:join([?OUT_ROOT, "imboy_agent_probe.hird"]),
    ok = file:write_file(Src, ?HIRD_SRC),
    Cmd = io_lib:format(
        "\"~ts\" build \"~ts\" --out-dir \"~ts\" 2>&1",
        [?HIRD_BIN, Src, OutDir]
    ),
    Output = os:cmd(Cmd),
    case filelib:is_regular(filename:join([OutDir, atom_to_list(?MOD) ++ ".beam"])) of
        true -> ok;
        false -> erlang:error({hird_build_failed, Output})
    end.

%%%===================================================================
%%% fixture（一次性 marker 库内合成数据）
%%%===================================================================

reset_tables(Conn) ->
    case
        epgsql:squery(
            Conn,
            "TRUNCATE agent_effect, agent_run_event, agent_run, agent_grant_event, "
            "agent_grant_capability, agent_grant_workspace, agent_grant, "
            "workspace_member, workspace, organization_member, organization, \"user\" "
            "RESTART IDENTITY CASCADE"
        )
    of
        {ok, _, _} -> ok;
        {error, Reason} -> erlang:error({truncate_failed, Reason})
    end.

insert_base(Conn) ->
    insert_user(Conn, ?AGENT_UID, 1),
    insert_user(Conn, ?HUMAN_UID, 0),
    insert_org(Conn, ?ORG_A),
    insert_org(Conn, ?ORG_B),
    insert_member(Conn, ?ORG_A, ?AGENT_UID),
    insert_member(Conn, ?ORG_B, ?AGENT_UID),
    insert_ws(Conn, ?WS_A, ?ORG_A, <<"poc-ws-alpha">>),
    insert_ws(Conn, ?WS_B, ?ORG_B, <<"poc-ws-beta">>),
    insert_ws_member(Conn, ?WS_A, ?AGENT_UID),
    insert_ws_member(Conn, ?WS_B, ?AGENT_UID),
    ok.

insert_user(Conn, Id, AccountType) ->
    q(
        Conn,
        "INSERT INTO \"user\" (id, account, password, account_type, status, reg_ip, reg_cosv) "
        "VALUES ($1, $2, 'poc-no-login', $3, 1, '127.0.0.1', 'poc')",
        [Id, <<"poc-user-", (integer_to_binary(Id))/binary>>, AccountType]
    ).

insert_org(Conn, OrgId) ->
    q(
        Conn,
        "INSERT INTO organization (id, name, owner_id, status) VALUES ($1, $2, $3, 'active')",
        [OrgId, <<"poc-org-", (integer_to_binary(OrgId))/binary>>, ?HUMAN_UID]
    ).

insert_member(Conn, OrgId, Uid) ->
    q(
        Conn,
        "INSERT INTO organization_member (organization_id, user_id, role, status, joined_at) "
        "VALUES ($1, $2, 'member', 'active', $3)",
        [OrgId, Uid, ?NOW]
    ).

insert_ws(Conn, WsId, OrgId, Name) ->
    q(
        Conn,
        "INSERT INTO workspace (id, name, owner_id, status, organization_id) "
        "VALUES ($1, $2, $3, 'active', $4)",
        [WsId, Name, ?HUMAN_UID, OrgId]
    ).

insert_ws_member(Conn, WsId, Uid) ->
    q(
        Conn,
        "INSERT INTO workspace_member (workspace_id, user_id, role, status, joined_at) "
        "VALUES ($1, $2, 'member', 'active', $3)",
        [WsId, Uid, ?NOW]
    ).

insert_grant(Conn, GrantId, OrgId) ->
    q(
        Conn,
        "INSERT INTO agent_grant (id, agent_id, organization_id, delegator_user_id, "
        "workspace_scope_kind, status, valid_from, expires_at, version, idempotency_key) "
        "VALUES ($1, $2, $3, $4, 'explicit', 'active', $5, $6, 1, $7)",
        [
            GrantId,
            ?AGENT_UID,
            OrgId,
            ?HUMAN_UID,
            {{2026, 10, 1}, {0, 0, 0}},
            {{2027, 10, 1}, {0, 0, 0}},
            <<"poc-grant-", (integer_to_binary(GrantId))/binary>>
        ]
    ).

insert_grant_ws(Conn, OrgId, GrantId, WsId) ->
    q(
        Conn,
        "INSERT INTO agent_grant_workspace (organization_id, grant_id, workspace_id) "
        "VALUES ($1, $2, $3)",
        [OrgId, GrantId, WsId]
    ).

insert_capability(Conn, GrantId) ->
    q(
        Conn,
        "INSERT INTO agent_grant_capability (grant_id, capability, action, resource_type, constraint_json) "
        "VALUES ($1, 'workspace.detail', 'read', 'workspace', '{}'::jsonb)",
        [GrantId]
    ).

insert_run(Conn, RunId, OrgId, GrantId) ->
    q(
        Conn,
        "INSERT INTO agent_run (id, agent_id, organization_id, workspace_id, grant_id, "
        "grant_version_at_start, delegating_principal_id, trigger_type, trigger_id, "
        "runtime_type, status, context_digest, idempotency_key, started_at) "
        "VALUES ($1, $2, $3, NULL, $4, 1, $5, 'message', $6, 'hird_poc', "
        "'running', 'sha256:poc-ctx', $7, $8)",
        [
            RunId,
            ?AGENT_UID,
            OrgId,
            GrantId,
            ?HUMAN_UID,
            <<"poc-trg-", (integer_to_binary(RunId))/binary>>,
            <<"poc-run-idem-", (integer_to_binary(RunId))/binary>>,
            ?NOW
        ]
    ).

q(Conn, Sql, Params) ->
    case epgsql:equery(Conn, Sql, Params) of
        {ok, _} -> ok;
        {ok, _, _} -> ok;
        {ok, _, _, _} -> ok;
        {error, Reason} -> erlang:error({fixture_failed, Reason, Sql})
    end.

%%%===================================================================
%%% 桥 Ctx / 运行程序
%%%===================================================================

tool_desc() ->
    #{
        tool_id => <<"tool.workspace.detail">>,
        capability => <<"workspace.detail">>,
        action => <<"read">>,
        risk_level => low,
        side_effect_class => readonly
    }.

%% 服务端资源解析：从 args.ws_id 查 workspace 真实归属 org（不存在 →
%% not_found；跨 org 由授权链步骤 1 裁决）。
resource_of(Conn) ->
    fun(#{ws_id := WsId} = Args) ->
        ArgsDigest = <<"sha256:args-ws-", (integer_to_binary(WsId))/binary>>,
        case epgsql:equery(Conn, "SELECT organization_id FROM workspace WHERE id = $1", [WsId]) of
            {ok, _, [{OrgId}]} ->
                #{
                    organization_id => OrgId,
                    workspace_id => WsId,
                    resource_type => <<"workspace">>,
                    resource_digest => <<"sha256:ws-", (integer_to_binary(WsId))/binary>>,
                    args_digest => ArgsDigest,
                    hird_args => Args
                };
            {ok, _, []} ->
                {error, workspace_not_found}
        end
    end.

%% 业务执行体（授权 allow 后由桥同进程调用）：签发 effect 凭证（绑定
%% EffectId+RunId+目标资源三元组，C3 CLOSEOUT）→ registry handler（真实
%% internal_find_tx + atomics 计数）→ 清凭证。
biz_of(RunCtx) ->
    fun(Resource, EffectId) ->
        Cred = #{
            organization_id => maps:get(organization_id, Resource),
            workspace_id => maps:get(workspace_id, Resource)
        },
        imboy_hird_poc_registry:set_effect({EffectId, maps:get(run_id, RunCtx), Cred}),
        {ok, Handler} = imboy_hird_poc_registry:lookup(<<"tool.workspace.detail">>),
        Result = Handler(Resource),
        imboy_hird_poc_registry:clear_effect(),
        Result
    end.

%% 跑一次生成程序（audit + 注册表桥作用域内）。
%% 返回 {Outcome, AuditFile}：Outcome = ok | {caught, Class, Reason}
%% （deny 域错误使 actor 崩溃时 main 可能异常退出——形态不限，见模块头）。
run_program(State, RunCtx, OverTool) ->
    run_program(
        State,
        RunCtx,
        OverTool,
        fun imboy_hird_poc_registry:model_handler/2
    ).

%% C5 CLOSEOUT：Provider handler 可覆盖（脚本化 Provider 变体验证用）。
run_program(State, RunCtx, OverTool, ProviderHandler) ->
    Conn = maps:get(conn, State),
    Tool = maps:merge(tool_desc(), OverTool),
    BridgeCtx = #{
        run_ctx => RunCtx,
        tool => Tool,
        hird_tool_name => ws_detail,
        resource_of => resource_of(Conn),
        biz => biz_of(RunCtx)
    },
    Bridge = imboy_hird_host_bridge:bridge_handler(BridgeCtx),
    AuditFile = audit_file(),
    open_audit(AuditFile),
    try
        ok = hird_audit:register_tools(?MOD:hird_tools@()),
        Outcome =
            try
                ok = imboy_hird_host_bridge:run_program(
                    BridgeCtx#{
                        program => ?MOD,
                        registry => [
                            {{tool, model_chat}, ProviderHandler},
                            {{tool, ws_detail}, Bridge}
                        ]
                    },
                    #{}
                ),
                ok
            catch
                Class:Reason -> {caught, Class, Reason}
            end,
        ok = hird_audit:sync(),
        {Outcome, AuditFile}
    after
        stop_supervisor(),
        stop_audit()
    end.

run_ctx(Conn, RunId, OrgId) ->
    #{
        run_id => RunId,
        agent_id => ?AGENT_UID,
        organization_id => OrgId,
        now => ?NOW,
        conn => Conn
    }.

audit_file() ->
    Tag = integer_to_binary(erlang:unique_integer([positive])),
    filename:join([?OUT_ROOT, "audit", <<"p1_", Tag/binary, ".jsonl">>]).

open_audit(File) ->
    _ = file:delete(File),
    ok = filelib:ensure_dir(File),
    {ok, _} = hird_audit:start_link([{sink, {file, File}}]).

stop_audit() ->
    try
        gen_server:stop(hird_audit)
    catch
        _:_ -> ok
    end,
    ok.

stop_supervisor() ->
    try
        gen_server:stop(?SUP_NAME)
    catch
        _:_ -> ok
    end,
    ok.

%%%===================================================================
%%% audit 结构化断言（jsone 全量解析；顺序敏感，不排序）
%%%===================================================================

read_audit(File) ->
    [jsone:decode(L) || L <- read_lines(File)].

read_lines(Path) ->
    {ok, Bin} = file:read_file(Path),
    [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>].

tool_result_stream(Records) ->
    [{maps:get(<<"tool">>, R), maps:get(<<"result">>, R)} || R <- Records].

%% 桥以 throw {hird_exn,{tool_denied,Bin}} 回传 deny（ADR-016）；audit 的
%% 类型导向编码把该值编为 AgentToolError 的构造器形状
%% （ctor="ToolDenied", args=[Reason]）。
deny_err(Reason) ->
    #{<<"err">> => #{<<"args">> => [Reason], <<"ctor">> => <<"ToolDenied">>}}.

biz_calls() -> imboy_hird_poc_registry:biz_calls().
provider_calls() -> imboy_hird_poc_registry:provider_calls().

%%%===================================================================
%%% 用例
%%%===================================================================

t_legal() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    {ok, File} = run_program(State, run_ctx(Conn, ?RUN_A, ?ORG_A), #{}),
    %% 生成程序两步都真实发生：模拟 Provider 1 次、业务读取恰 1 次。
    ?assertEqual(1, provider_calls()),
    ?assertEqual(1, biz_calls()),
    %% audit：model_chat {ok} → ws_detail {ok, 真实 fixture 数据}，顺序敏感。
    ?assertEqual(
        [
            {<<"ModelChat">>, #{<<"ok">> => <<"scripted-model-reply:plan">>}},
            {<<"WsDetail">>, #{
                <<"ok">> =>
                    <<"{\"id\":912001,\"name\":\"poc-ws-alpha\",\"owner_id\":910002}">>
            }}
        ],
        tool_result_stream(read_audit(File))
    ),
    %% 授权决策真实落库：agent_effect 恰一条 authorized（run 行锁守卫事务）。
    {ok, _, [{1}]} = epgsql:equery(
        Conn,
        "SELECT count(*) FROM agent_effect WHERE run_id = $1 AND status = 'authorized'",
        [?RUN_A]
    ),
    %% caller 为 codegen 事实（actor 消息处理点）。
    [_, WsLine] = read_audit(File),
    ?assertEqual(<<"ProbeRunner.handle_msg/ProbeRun">>, maps:get(<<"caller">>, WsLine)).

t_not_granted() ->
    State = state(),
    Conn = maps:get(conn, State),
    ok = reset_tables(Conn),
    ok = insert_base(Conn),
    ok = insert_grant(Conn, ?GRANT_A, ?ORG_A),
    %% 无 capability 行。
    ok = insert_run(Conn, ?RUN_A, ?ORG_A, ?GRANT_A),
    {_Outcome, File} = run_program(State, run_ctx(Conn, ?RUN_A, ?ORG_A), #{}),
    ?assertEqual(0, biz_calls()),
    ?assertEqual(1, provider_calls()),
    ?assertEqual(
        [
            {<<"ModelChat">>, #{<<"ok">> => <<"scripted-model-reply:plan">>}},
            {<<"WsDetail">>, deny_err(<<"not_granted">>)}
        ],
        tool_result_stream(read_audit(File))
    ).

t_revoked() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 真实撤销提交（CAS + 事件，agent_grant_pg 冻结入口）。
    Event = #{
        id => 916001,
        grant_id => ?GRANT_A,
        event_type => revoked,
        actor_kind => human,
        actor_user_id => ?HUMAN_UID,
        revoked_by_user_id => ?HUMAN_UID,
        from_version => 1,
        to_version => 2,
        detail => #{},
        idempotency_key => <<"poc-revoke-1">>,
        now => ?NOW
    },
    {ok, 2} = agent_grant_pg:revoke_cas_tx(Conn, ?ORG_A, ?GRANT_A, 1, ?NOW, Event),
    %% 撤销后的新 Run：授权链实时重读 → grant_revoked 拒。
    ok = insert_run(Conn, ?RUN_B, ?ORG_A, ?GRANT_A),
    {_Outcome, File} = run_program(State, run_ctx(Conn, ?RUN_B, ?ORG_A), #{}),
    ?assertEqual(0, biz_calls()),
    ?assertMatch(
        {<<"WsDetail">>, #{
            <<"err">> :=
                #{<<"args">> := [<<"grant_revoked">>], <<"ctor">> := <<"ToolDenied">>}
        }},
        lists:last(tool_result_stream(read_audit(File)))
    ).

t_cross_org() ->
    State = state(),
    Conn = maps:get(conn, State),
    ok = reset_tables(Conn),
    ok = insert_base(Conn),
    ok = insert_grant(Conn, ?GRANT_B, ?ORG_B),
    ok = insert_grant_ws(Conn, ?ORG_B, ?GRANT_B, ?WS_B),
    ok = insert_capability(Conn, ?GRANT_B),
    ok = insert_run(Conn, ?RUN_B, ?ORG_B, ?GRANT_B),
    %% run 属 org B，程序请求 WS_A（真实归属 org A）→ 步骤 1 cross_org 拒。
    {_Outcome, File} = run_program(State, run_ctx(Conn, ?RUN_B, ?ORG_B), #{}),
    ?assertEqual(0, biz_calls()),
    ?assertMatch(
        {<<"WsDetail">>, #{
            <<"err">> :=
                #{<<"args">> := [<<"cross_org">>], <<"ctor">> := <<"ToolDenied">>}
        }},
        lists:last(tool_result_stream(read_audit(File)))
    ),
    %% 不泄露目标数据：audit 全文无 org A 的 WS 数据原文。
    ?assertMatch(
        nomatch,
        binary:match(
            iolist_to_binary(read_lines(File)),
            <<"poc-ws-alpha">>
        )
    ).

t_forged_ctx() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 伪造上下文：声明一个不存在的 Run id。
    {_Outcome, File} = run_program(State, run_ctx(Conn, 999999, ?ORG_A), #{}),
    ?assertEqual(0, biz_calls()),
    ?assertMatch(
        {<<"WsDetail">>, #{
            <<"err">> :=
                #{<<"args">> := [<<"run_not_found">>], <<"ctor">> := <<"ToolDenied">>}
        }},
        lists:last(tool_result_stream(read_audit(File)))
    ).

t_unknown_tool() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 未注册 capability（目录未声明）：unknown_capability 拒。
    {_Outcome, File} = run_program(
        State,
        run_ctx(Conn, ?RUN_A, ?ORG_A),
        #{capability => <<"session.list">>}
    ),
    ?assertEqual(0, biz_calls()),
    ?assertMatch(
        {<<"WsDetail">>, #{
            <<"err">> :=
                #{<<"args">> := [<<"unknown_capability">>], <<"ctor">> := <<"ToolDenied">>}
        }},
        lists:last(tool_result_stream(read_audit(File)))
    ),
    %% native registry 面：未注册 tool_id 同样 not_found。
    ?assertEqual({error, not_found}, imboy_hird_poc_registry:lookup(<<"tool.unknown.x">>)).

t_bypass_direct() ->
    %% 绕过授权链直调 registry handler：无 effect 凭证 → 拒绝，读取零次。
    {ok, DirectHandler} = imboy_hird_poc_registry:lookup(<<"tool.workspace.detail">>),
    ?assertEqual(
        {error, no_effect_credential},
        DirectHandler(#{workspace_id => ?WS_A, organization_id => ?ORG_A})
    ),
    ?assertEqual(0, biz_calls()).

t_bypass_dispatch() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 作用域外：注册表无桥 → unhandled_tool（不能获取业务资源）。
    ?assertError(
        {unhandled_tool, ws_detail},
        hird_tool_dispatch:call(ws_detail, <<"Bypass.outside">>, #{}, #{ws_id => ?WS_A})
    ),
    %% 作用域内（模拟程序内任意调用点）：桥仍在——真实授权后再执行。
    Bridge = imboy_hird_host_bridge:bridge_handler(#{
        run_ctx => run_ctx(Conn, ?RUN_A, ?ORG_A),
        tool => tool_desc(),
        hird_tool_name => ws_detail,
        resource_of => resource_of(Conn),
        biz => biz_of(run_ctx(Conn, ?RUN_A, ?ORG_A))
    }),
    ok = hird_handlers:with_handlers([{{tool, ws_detail}, Bridge}], fun() ->
        R = hird_tool_dispatch:call(ws_detail, <<"Bypass.inside">>, #{}, #{ws_id => ?WS_A}),
        ?assertMatch(<<"{\"id\":912001", _/binary>>, R)
    end),
    ?assertEqual(1, biz_calls()).

t_recursion_guard() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 合法授权 + HandlerMap 不含目标键 → 结构性拒绝（不是无限递归）。
    Resource = (resource_of(Conn))(#{ws_id => ?WS_A}),
    Self = self(),
    Pid = spawn(fun() ->
        R = imboy_hird:run_tool(
            run_ctx(Conn, ?RUN_A, ?ORG_A),
            tool_desc(),
            Resource,
            #{},
            ws_detail
        ),
        Self ! {self(), R}
    end),
    Ref = erlang:monitor(process, Pid),
    receive
        {Pid, {error, {handler_map_missing, ws_detail}}} -> ok;
        {Pid, Other} -> erlang:error({unexpected, Other})
    after 5000 ->
        erlang:error(recursion_guard_timeout)
    end,
    erlang:demonitor(Ref, [flush]),
    ?assertEqual(0, biz_calls()).

t_same_org_other_ws() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 同 org A 的第三个 Workspace，agent 是其成员，但 GRANT_A（explicit）
    %% 只绑 WS_A —— grant scope 校验（IMBoy 公共授权能力，两条路径共享）。
    ok = insert_ws(Conn, 912003, ?ORG_A, <<"poc-ws-gamma">>),
    ok = insert_ws_member(Conn, 912003, ?AGENT_UID),
    Resource = #{
        organization_id => ?ORG_A,
        workspace_id => 912003,
        resource_type => <<"workspace">>,
        resource_digest => <<"sha256:ws-912003">>,
        args_digest => <<"sha256:args-ws-912003">>
    },
    ?assertEqual(
        {deny, grant_scope_mismatch},
        agent_tool_authorizer:authorize(
            run_ctx(Conn, ?RUN_A, ?ORG_A),
            tool_desc(),
            Resource
        )
    ),
    ?assertEqual(0, biz_calls()).

t_effect_resource_binding() ->
    State = state(),
    Conn = maps:get(conn, State),
    %% 凭证绑定校验发生在业务读取之前：本用例无需 fixture 即可断言拒绝面。
    {ok, Handler} = imboy_hird_poc_registry:lookup(<<"tool.workspace.detail">>),
    ReqA = #{workspace_id => ?WS_A, organization_id => ?ORG_A},
    %% 凭证绑定 (ORG_B, WS_B)：请求 WS_A（跨 org 且跨 ws）→ 拒绝、零读取。
    ok = imboy_hird_poc_registry:set_effect(
        {1, ?RUN_A, #{organization_id => ?ORG_B, workspace_id => ?WS_B}}
    ),
    ?assertEqual({error, effect_resource_mismatch}, Handler(ReqA)),
    ?assertEqual(0, biz_calls()),
    %% 同 org 跨 ws 同样拒绝（凭证绑定的是具体资源，非"任意授权过的 org"）。
    ok = imboy_hird_poc_registry:set_effect(
        {1, ?RUN_A, #{organization_id => ?ORG_A, workspace_id => ?WS_B}}
    ),
    ?assertEqual({error, effect_resource_mismatch}, Handler(ReqA)),
    ?assertEqual(0, biz_calls()),
    %% 正向对照：凭证与请求资源一致时才执行（真实 fixture 读取恰一次）。
    given_full_fixture(Conn),
    ok = imboy_hird_poc_registry:set_effect(
        {1, ?RUN_A, #{organization_id => ?ORG_A, workspace_id => ?WS_A}}
    ),
    ?assertMatch({ok, <<"{\"id\":912001", _/binary>>}, Handler(ReqA)),
    ?assertEqual(1, biz_calls()),
    ok = imboy_hird_poc_registry:clear_effect().

t_provider_variant() ->
    %% C5 CLOSEOUT（脚本化 Provider 变体）：模型返回纯文本（不含任何工具
    %% 请求）时，当前 Hirð 程序**仍然固定执行** WsDetail —— 证明业务调用
    %% 与模型输出无关（固定流程），真实模型接入需要程序层工具决策循环。
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    PlainReply = fun(#{prompt := P}, _Hs) ->
        <<"plain-answer-no-tool:", P/binary>>
    end,
    {ok, File} = run_program(State, run_ctx(Conn, ?RUN_A, ?ORG_A), #{}, PlainReply),
    ?assertEqual(1, biz_calls()),
    ?assertEqual(1, provider_calls()),
    ?assertEqual(
        [
            {<<"ModelChat">>, #{<<"ok">> => <<"plain-answer-no-tool:plan">>}},
            {<<"WsDetail">>, #{
                <<"ok">> =>
                    <<"{\"id\":912001,\"name\":\"poc-ws-alpha\",\"owner_id\":910002}">>
            }}
        ],
        tool_result_stream(read_audit(File))
    ).

t_default_off() ->
    State = state(),
    Conn = maps:get(conn, State),
    given_full_fixture(Conn),
    %% 不配能力面 PoC env：目录=生产默认空集、策略=生产默认恒拒
    %% （membership 是基础设施读取而非能力面开关——保留注入实现，
    %% 拒绝必须来自空目录/恒拒策略本身）。
    [
        ok = application:unset_env(imboy, K)
     || K <- [
            agent_capability_catalog_module,
            agent_resource_policy_module,
            agent_native_tool_registry_module
        ]
    ],
    try
        {_Outcome, File} = run_program(State, run_ctx(Conn, ?RUN_A, ?ORG_A), #{}),
        ?assertEqual(0, biz_calls()),
        ?assertMatch(
            {<<"WsDetail">>, #{
                <<"err">> :=
                    #{<<"args">> := [<<"unknown_capability">>], <<"ctor">> := <<"ToolDenied">>}
            }},
            lists:last(tool_result_stream(read_audit(File)))
        )
    after
        ok = install_poc_env()
    end.

%%%===================================================================
%%% 内部
%%%===================================================================

given_full_fixture(Conn) ->
    ok = reset_tables(Conn),
    ok = insert_base(Conn),
    ok = insert_grant(Conn, ?GRANT_A, ?ORG_A),
    ok = insert_grant_ws(Conn, ?ORG_A, ?GRANT_A, ?WS_A),
    ok = insert_capability(Conn, ?GRANT_A),
    ok = insert_run(Conn, ?RUN_A, ?ORG_A, ?GRANT_A),
    ok.

%% P2/P3 复用：生成程序模块名（?MOD = hird_imboy_agent_probe）。
mod() -> ?MOD.
