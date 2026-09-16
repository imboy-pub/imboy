%%% @doc AgentRun 八状态 FSM 纯函数真源（AG31-04B；架构合同 §9.3 Frozen AgentRun FSM）。
%%%
%%% 合同规范本：docs/architecture/2026-09-16-imboy-agent-runtime-v3.1.md
%%% （SHA256=05808674d4825320de867a2a8d2899fb4babddbfcea6fe4bf27a4e43a55dd6b2）。
%%%
%%% 职责边界：
%%%   * 本模块是 FSM 的**语义唯一真源**（镜像 cs_session 的 domain 角色）：
%%%     八状态枚举、16 条合法边（E01-E16）、per-edge 默认 actor（AG-A0 裁决 CS-5）、
%%%     agent_effect 子状态迁移矩阵（裁决 CS-7）、attempt 上限（裁决 CS-6）、
%%%     timeout→failed 映射（裁决 CS-3）、unknown 禁止新 Effect 的门禁。
%%%   * 纯函数：无 I/O、无时间/随机隐式依赖（Now 由调用方传入）、无进程状态。
%%%   * 未列出的边一律非法（§9.3 "未列出的边全部非法"；终态零出边）。
%%%   * 不复用 agent_task、不接 router/HTTP、不依赖 epgsql——分层见
%%%     scripts/check_feature_architecture.sh 铁律 4。
-module(agent_run_fsm).

-export([
    states/0,
    terminal_states/0,
    is_state/1,
    is_terminal/1,
    legal_edges/0,
    edge_for/2,
    transition_allowed/2,
    assert_transition/2,
    edge_default_actor_kind/1,
    effect_states/0,
    effect_terminal_states/0,
    effect_legal_edges/0,
    effect_transition_allowed/2,
    effect_edge_for/2,
    actor_kinds/0,
    assert_actor_kind/1,
    max_attempts/0,
    assert_attempt_below_cap/1,
    timeout_failure/0,
    new_effect_gate/1
]).

-export_type([run_status/0, effect_status/0, edge_id/0, actor_kind/0]).

-type run_status() ::
    created
    | queued
    | running
    | waiting_approval
    | succeeded
    | failed
    | cancelled
    | unknown.
-type effect_status() ::
    created
    | denied
    | waiting_approval
    | authorized
    | dispatching
    | succeeded
    | failed
    | unknown.
-type actor_kind() :: human | system | agent.
-type edge_id() ::
    e01
    | e02
    | e03
    | e04
    | e05
    | e06
    | e07
    | e08
    | e09
    | e10
    | e11
    | e12
    | e13
    | e14
    | e15
    | e16.

%% ===================================================================
%% Run FSM：八状态与终态（§9.3 L424-427 + L450）
%% ===================================================================

%% @doc 八状态枚举，恰好八个，顺序固定（§9.3）。
-spec states() -> [run_status()].
states() ->
    [created, queued, running, waiting_approval, succeeded, failed, cancelled, unknown].

%% @doc 终态：succeeded|failed|cancelled，零出边（§9.3 "为终态"）。
-spec terminal_states() -> [run_status()].
terminal_states() ->
    [succeeded, failed, cancelled].

-spec is_state(term()) -> boolean().
is_state(State) -> lists:member(State, states()).

-spec is_terminal(run_status()) -> boolean().
is_terminal(State) -> lists:member(State, terminal_states()).

%% ===================================================================
%% 16 条合法边（§9.3 mermaid L430-448；AG31-04A 64 格矩阵逐字）
%% ===================================================================

%% @doc 全部 16 条合法边。`actor` 为 per-edge 默认 actor_kind（AG-A0 裁决 CS-5：
%% trigger/lease/reconcile 类=system，cancel/approve 类=human）。
-spec legal_edges() ->
    [{edge_id(), run_status(), run_status(), actor_kind(), string()}].
legal_edges() ->
    [
        {e01, created, queued, system, "context committed"},
        {e02, created, failed, system, "validation/persistence failure"},
        {e03, created, cancelled, human, "cancellation wins"},
        {e04, queued, running, system, "lease acquired"},
        {e05, queued, failed, system, "start/timeout failure"},
        {e06, queued, cancelled, human, "cancel"},
        {e07, running, waiting_approval, system, "effect requires HITL"},
        {e08, running, succeeded, system, "terminal success"},
        {e09, running, failed, system, "deterministic failure/timeout"},
        {e10, running, cancelled, human, "cancel before next dispatch"},
        {e11, running, unknown, system, "external write outcome indeterminate"},
        {e12, waiting_approval, queued, human, "valid approval"},
        {e13, waiting_approval, failed, human, "rejection/expiry/auth revoked"},
        {e14, waiting_approval, cancelled, human, "cancel"},
        {e15, unknown, succeeded, system, "reconcile proves success"},
        {e16, unknown, failed, system, "reconcile proves no success/failure"}
    ].

%% @doc 按 {From, To} 查合法边；未列出即非法（§9.3 L450）。
-spec edge_for(run_status(), run_status()) ->
    {ok, edge_id(), actor_kind()} | {error, illegal_transition}.
edge_for(From, To) ->
    case lists:filter(fun({_Id, F, T, _A, _L}) -> F =:= From andalso T =:= To end, legal_edges()) of
        [{Id, _, _, Actor, _Label}] -> {ok, Id, Actor};
        [] -> {error, illegal_transition}
    end.

-spec transition_allowed(run_status(), run_status()) -> boolean().
transition_allowed(From, To) ->
    ok =:= element(1, edge_for(From, To)).

%% @doc 断言边合法；未列出的边一律 {error, illegal_transition}。
-spec assert_transition(run_status(), run_status()) -> ok | {error, illegal_transition}.
assert_transition(From, To) ->
    case edge_for(From, To) of
        {ok, _Id, _Actor} -> ok;
        {error, _} = Err -> Err
    end.

%% @doc 边默认 actor_kind（CS-5）。
-spec edge_default_actor_kind(edge_id() | {run_status(), run_status()}) ->
    {ok, actor_kind()} | {error, term()}.
edge_default_actor_kind(EdgeId) when is_atom(EdgeId) ->
    case lists:filter(fun({Id, _, _, _, _}) -> Id =:= EdgeId end, legal_edges()) of
        [{_, _, _, Actor, _}] -> {ok, Actor};
        [] -> {error, {unknown_edge_id, EdgeId}}
    end;
edge_default_actor_kind({From, To}) ->
    case edge_for(From, To) of
        {ok, _Id, Actor} -> {ok, Actor};
        {error, _} = Err -> Err
    end.

%% ===================================================================
%% agent_effect 子状态矩阵（AG-A0 裁决 CS-7；合同 §9.4 L507 + §10.3 L557）
%% ===================================================================

%% @doc Effect 八状态（§9.4 L507）。
-spec effect_states() -> [effect_status()].
effect_states() ->
    [created, denied, waiting_approval, authorized, dispatching, succeeded, failed, unknown].

%% @doc Effect 终态：denied/succeeded/failed 零出边（dispatching 链的收束；
%% unknown 经显式 reconcile 收敛，非终态）。
-spec effect_terminal_states() -> [effect_status()].
effect_terminal_states() ->
    [denied, succeeded, failed].

%% @doc Effect 子状态迁移矩阵（CS-7 补全，落 AG31-04B evidence）：
%%   created→{denied, waiting_approval, authorized}（授权判定三分支，§10.1）
%%   waiting_approval→{authorized, denied}（有效批准 / 拒绝·过期·Grant 撤销，§10.3）
%%   authorized→dispatching（dispatch 前必须先持久化，§10.3 L557）
%%   dispatching→{succeeded, failed, unknown}（unknown=crash/timeout 结果不可知）
%%   unknown→{succeeded, failed}（仅显式 reconcile 收敛 ledger，§10.3 "以查询/返回结果收敛"）
%%   denied/succeeded/failed 零出边。
-spec effect_legal_edges() -> [{effect_status(), effect_status()}].
effect_legal_edges() ->
    [
        {created, denied},
        {created, waiting_approval},
        {created, authorized},
        {waiting_approval, authorized},
        {waiting_approval, denied},
        {authorized, dispatching},
        {dispatching, succeeded},
        {dispatching, failed},
        {dispatching, unknown},
        {unknown, succeeded},
        {unknown, failed}
    ].

-spec effect_edge_for(effect_status(), effect_status()) -> ok | {error, illegal_transition}.
effect_edge_for(From, To) ->
    case lists:member({From, To}, effect_legal_edges()) of
        true -> ok;
        false -> {error, illegal_transition}
    end.

-spec effect_transition_allowed(effect_status(), effect_status()) -> boolean().
effect_transition_allowed(From, To) ->
    ok =:= effect_edge_for(From, To).

%% ===================================================================
%% actor_kind / attempt 上限 / timeout 映射 / unknown 门禁
%% ===================================================================

%% @doc actor_kind 值域（AG-A0 裁决 CS-4，与 00000133 ck_are_actor_kind 一致）。
-spec actor_kinds() -> [actor_kind()].
actor_kinds() ->
    [human, system, agent].

-spec assert_actor_kind(term()) -> ok | {error, invalid_actor_kind}.
assert_actor_kind(Kind) ->
    case lists:member(Kind, actor_kinds()) of
        true -> ok;
        false -> {error, invalid_actor_kind}
    end.

%% @doc attempt 上限（AG-A0 裁决 CS-6，依 §14 无盲重试原则冻结）：一次 Run 生命周期
%% 内 lease 获取（含过期接管）至多 3 次；第 4 次获取被拒，Run 以
%% failed + reason_code=max_attempts_exceeded 收束。应用层常量，非 schema 字段。
-spec max_attempts() -> pos_integer().
max_attempts() ->
    3.

%% @doc 当前 attempt=Attempt 时是否仍允许再获取一次 lease。
-spec assert_attempt_below_cap(non_neg_integer()) -> ok | {error, max_attempts_exceeded}.
assert_attempt_below_cap(Attempt) when is_integer(Attempt), Attempt >= 0 ->
    case Attempt < max_attempts() of
        true -> ok;
        false -> {error, max_attempts_exceeded}
    end;
assert_attempt_below_cap(Bad) ->
    erlang:error({badarg, Bad}).

%% @doc timeout 不是独立状态（§9.3 L451；AG-A0 裁决 CS-3）：
%% 映射为 failed 存储态 + reason_code=timeout（预算值属 product policy，不进 schema）。
-spec timeout_failure() -> {failed, timeout}.
timeout_failure() ->
    {failed, timeout}.

%% @doc 新 Effect 门禁（§9.3 L450-451 "unknown 禁止执行新 Effect" + §14
%% "Run terminal/cancelled/timed out | deny all new effects"）：仅 running 放行。
-spec new_effect_gate(run_status()) ->
    ok
    | {error, run_not_running | run_awaiting_approval | run_unknown_no_new_effect | run_terminal}.
new_effect_gate(running) ->
    ok;
new_effect_gate(created) ->
    {error, run_not_running};
new_effect_gate(queued) ->
    {error, run_not_running};
new_effect_gate(waiting_approval) ->
    {error, run_awaiting_approval};
new_effect_gate(unknown) ->
    {error, run_unknown_no_new_effect};
new_effect_gate(Succeeded) when
    Succeeded =:= succeeded;
    Succeeded =:= failed;
    Succeeded =:= cancelled
->
    {error, run_terminal};
new_effect_gate(Bad) ->
    erlang:error({badrun_status, Bad}).
