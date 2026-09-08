#!/usr/bin/env python3
"""Agent Hub 任务状态机矩阵 verifier（FSM-00 交付）。

冻结 docs/api-contracts/agent_hub_task_state_machine.json 的机器契约：
状态全集、终态全集、初始态、动作全集、合法边全集均以本文件常量锁定，
JSON 矩阵与常量任何一侧漂移都 fail-closed。

检查项（对应任务卡 FSM-00 Actions）：
  1. schema 自身结构完整（必需字段、类型、非空）。
  2. 状态恰为九状态契约：submitted/working/awaiting_approval/approved/
     rejected/expired/completed/failed/cancelled；终态恰为五个；
     initial_state=submitted。
  3. 无重复边：每个 (from, action) 至多一条出边（确定性迁移），
     (from, action, to) 三元组不得重复。
  4. 无悬空状态：所有边的 from/to 都在状态全集内，
     所有 allowed_actors 都在 actors 声明内。
  5. terminal 无出边且不可逆。
  6. 所有非 terminal 状态存在收敛路径（可达任一 terminal）。
  7. 非法迁移默认拒绝：illegal_transition_policy.default_outcome=rejected，
     且补集（states × actions − 合法边）逐对在 evaluate() 下均被拒。
  8. 审批规则：first-writer-wins、重复决定 already_decided（无状态变更、
     无新副作用）、approve/reject 仅 human_approver、仅发自 awaiting_approval。
  9. 过期规则：expire 仅由 system 从 submitted/awaiting_approval 触发，
     expired 为终态。
  10. 重启恢复：从持久化状态重放，不自动产生新副作用。
  11. 执行语义：恢复重试须可证明幂等，否则 at-most-once + 人工复核；
      交付文件文本中不得出现被禁的恰好一次承诺字样。

退出码：0=合法矩阵；2=违规（含文件不可读、JSON 损坏、结构缺失）。
只用 Python 标准库（python3.9 兼容，无 match 语法）。

用法：
    verify_agent_hub_task_state_machine.py <path/to/agent_hub_task_state_machine.json>
"""

import json
import sys
from collections import deque

# ---------------------------------------------------------------------------
# 冻结面：任务卡 FSM-00 九状态契约。任何一侧漂移即 fail-closed。
# ---------------------------------------------------------------------------

EXPECTED_INITIAL = "submitted"

EXPECTED_STATES = (
    "submitted",
    "working",
    "awaiting_approval",
    "approved",
    "rejected",
    "expired",
    "completed",
    "failed",
    "cancelled",
)

EXPECTED_TERMINAL = frozenset(
    ("rejected", "expired", "completed", "failed", "cancelled")
)

EXPECTED_ACTIONS = (
    "start",
    "cancel",
    "fail",
    "expire",
    "progress",
    "need_approval",
    "complete",
    "approve",
    "reject",
    "resume",
)

# (from, action, to) 合法边全集。
EXPECTED_EDGES = frozenset(
    (
        ("submitted", "start", "working"),
        ("submitted", "cancel", "cancelled"),
        ("submitted", "fail", "failed"),
        ("submitted", "expire", "expired"),
        ("working", "progress", "working"),
        ("working", "need_approval", "awaiting_approval"),
        ("working", "complete", "completed"),
        ("working", "fail", "failed"),
        ("working", "cancel", "cancelled"),
        ("awaiting_approval", "approve", "approved"),
        ("awaiting_approval", "reject", "rejected"),
        ("awaiting_approval", "expire", "expired"),
        ("awaiting_approval", "cancel", "cancelled"),
        ("approved", "resume", "working"),
        ("approved", "fail", "failed"),
    )
)

REQUIRED_TOP_FIELDS = (
    "contract_id",
    "schema_version",
    "initial_state",
    "actors",
    "states",
    "transitions",
    "approval_rules",
    "expiry_rules",
    "terminal_rules",
    "restart_recovery",
    "execution_semantics",
    "illegal_transition_policy",
)

TRANSITION_FIELDS = (
    "from",
    "action",
    "to",
    "allowed_actors",
    "idempotency",
    "side_effect",
)

# 文本禁令关键词以拼接构造：verifier 源文件本身也不得出现被禁字样。
FORBIDDEN_TEXT = "exactly" + "-once"


class MatrixError(Exception):
    """矩阵违规（fail-closed 条件），message 为机器可读违规码。"""


def _require(cond, code):
    if not cond:
        raise MatrixError(code)


def _is_str(value):
    return isinstance(value, str) and value != ""


def _is_bool(value):
    return isinstance(value, bool)


# ---------------------------------------------------------------------------
# 纯查询函数（测试与补集机验共用）
# ---------------------------------------------------------------------------

def edge_index(matrix):
    """返回 {(from, action): to}；重复 (from, action) 会被结构检查拦下。"""
    return {(t["from"], t["action"]): t["to"] for t in matrix["transitions"]}


def evaluate(matrix, state, action):
    """在矩阵上求值 (state, action)：返回目标状态；未声明（属于补集）返回 None=拒绝。"""
    return edge_index(matrix).get((state, action))


def illegal_complement(matrix):
    """非法迁移补集：states × actions_universe − 合法 (from, action) 边。"""
    actions = set(matrix["illegal_transition_policy"]["actions_universe"])
    legal = set(edge_index(matrix))
    return {
        (state, action)
        for state in (s["name"] for s in matrix["states"])
        for action in actions
        if (state, action) not in legal
    }


def reachable_terminals(matrix, start):
    """从 start 出发可达的 terminal 状态集合（沿合法边 BFS）。"""
    terminals = {s["name"] for s in matrix["states"] if s["terminal"]}
    adjacency = {}
    for t in matrix["transitions"]:
        adjacency.setdefault(t["from"], set()).add(t["to"])
    seen = {start}
    queue = deque([start])
    while queue:
        node = queue.popleft()
        for nxt in adjacency.get(node, ()):
            if nxt not in seen:
                seen.add(nxt)
                queue.append(nxt)
    return seen & terminals


# ---------------------------------------------------------------------------
# 结构与冻结面检查
# ---------------------------------------------------------------------------

def check_structure(matrix):
    """schema 自身结构完整（必需字段与基本类型）。"""
    _require(isinstance(matrix, dict), "top.not_object")
    for field in REQUIRED_TOP_FIELDS:
        _require(field in matrix, "top.missing:" + field)
    _require(_is_str(matrix["contract_id"]), "contract_id.not_string")
    _require(matrix["schema_version"] == 1, "schema_version.not_1")
    _require(_is_str(matrix["initial_state"]), "initial_state.not_string")

    _require(isinstance(matrix["actors"], list) and matrix["actors"],
             "actors.not_array_or_empty")
    names = []
    for actor in matrix["actors"]:
        _require(isinstance(actor, dict), "actors.item_not_object")
        _require(_is_str(actor.get("name")), "actors.item_name_bad")
        _require(_is_str(actor.get("description")), "actors.item_description_bad")
        names.append(actor["name"])
    _require(len(set(names)) == len(names), "actors.duplicate_name")

    _require(isinstance(matrix["states"], list) and matrix["states"],
             "states.not_array_or_empty")
    for state in matrix["states"]:
        _require(isinstance(state, dict), "states.item_not_object")
        _require(_is_str(state.get("name")), "states.item_name_bad")
        _require(_is_bool(state.get("terminal")), "states.item_terminal_not_bool")
        _require(_is_str(state.get("convergence_path")),
                 "states.item_convergence_path_bad")

    _require(isinstance(matrix["transitions"], list) and matrix["transitions"],
             "transitions.not_array_or_empty")
    for transition in matrix["transitions"]:
        _require(isinstance(transition, dict), "transitions.item_not_object")
        for field in TRANSITION_FIELDS:
            _require(field in transition, "transitions.item_missing:" + field)
        _require(_is_str(transition["from"]), "transitions.from_bad")
        _require(_is_str(transition["action"]), "transitions.action_bad")
        _require(_is_str(transition["to"]), "transitions.to_bad")
        _require(_is_str(transition["idempotency"]),
                 "transitions.idempotency_bad")
        _require(_is_str(transition["side_effect"]),
                 "transitions.side_effect_bad")
        actors = transition["allowed_actors"]
        _require(isinstance(actors, list) and actors,
                 "transitions.allowed_actors_bad")
        for actor in actors:
            _require(_is_str(actor), "transitions.allowed_actor_not_string")


def check_state_contract(matrix):
    """九状态、终态全集与初始态的冻结面。"""
    names = [s["name"] for s in matrix["states"]]
    _require(len(set(names)) == len(names), "states.duplicate_name")
    missing = sorted(set(EXPECTED_STATES) - set(names))
    unexpected = sorted(set(names) - set(EXPECTED_STATES))
    _require(not missing, "states.missing:" + ",".join(missing))
    _require(not unexpected, "states.unexpected:" + ",".join(unexpected))
    _require(matrix["initial_state"] == EXPECTED_INITIAL,
             "initial_state.not_submitted")
    for state in matrix["states"]:
        expected = state["name"] in EXPECTED_TERMINAL
        _require(state["terminal"] is expected,
                 "states.terminal_mismatch:" + state["name"])


def check_transitions_contract(matrix):
    """无重复边、无悬空、边全集恰为冻结面、actor 引用合法。"""
    actor_names = {a["name"] for a in matrix["actors"]}
    state_names = {s["name"] for s in matrix["states"]}

    seen_pair = set()
    seen_edge = set()
    edges = set()
    for transition in matrix["transitions"]:
        edge = (transition["from"], transition["action"], transition["to"])
        _require(edge not in seen_edge, "transitions.duplicate_edge")
        seen_edge.add(edge)
        pair = (transition["from"], transition["action"])
        _require(pair not in seen_pair,
                 "transitions.duplicate_from_action:%s+%s" % pair)
        seen_pair.add(pair)
        edges.add(edge)
        _require(transition["from"] in state_names,
                 "transitions.dangling_from:" + transition["from"])
        _require(transition["to"] in state_names,
                 "transitions.dangling_to:" + transition["to"])
        for actor in transition["allowed_actors"]:
            _require(actor in actor_names,
                     "transitions.unknown_actor:" + actor)

    missing = sorted(EXPECTED_EDGES - edges)
    unexpected = sorted(edges - EXPECTED_EDGES)
    _require(not missing, "edges.missing:" + ",".join("%s+%s>%s" % e for e in missing))
    _require(not unexpected,
             "edges.unexpected:" + ",".join("%s+%s>%s" % e for e in unexpected))


def check_terminal_rules(matrix):
    """terminal 无出边 + terminal_rules 声明一致 + 收敛路径。"""
    state_by_name = {
        s.get("name"): s for s in matrix["states"] if isinstance(s, dict)
    }
    for transition in matrix["transitions"]:
        source = state_by_name.get(transition["from"])
        if source is not None:
            _require(source["terminal"] is False,
                     "terminal.has_outgoing:" + transition["from"])

    rules = matrix.get("terminal_rules")
    _require(isinstance(rules, dict), "terminal_rules.not_object")
    _require(rules.get("terminal_states") is not None,
             "terminal_rules.missing:terminal_states")
    declared = list(rules["terminal_states"])
    _require(set(declared) == EXPECTED_TERMINAL and
             len(declared) == len(EXPECTED_TERMINAL),
             "terminal_rules.states_mismatch")
    _require(rules.get("no_outgoing_transitions") is True,
             "terminal_rules.no_outgoing_not_true")
    _require(rules.get("irreversible") is True,
             "terminal_rules.irreversible_not_true")

    # 所有非 terminal 状态必须可达任一 terminal（收敛路径）。
    for state in matrix["states"]:
        if state["terminal"]:
            continue
        _require(reachable_terminals(matrix, state["name"]),
                 "convergence.no_path:" + state["name"])


def check_illegal_complement(matrix):
    """补集 = states × actions_universe − 合法边，全部默认拒绝。"""
    policy = matrix.get("illegal_transition_policy")
    _require(isinstance(policy, dict), "illegal_transition_policy.not_object")
    _require(policy.get("default_outcome") == "rejected",
             "illegal_transition_policy.default_outcome_not_rejected")
    _require(_is_str(policy.get("error_code")),
             "illegal_transition_policy.error_code_bad")
    _require(policy.get("state_change") is False,
             "illegal_transition_policy.state_change_not_false")
    _require(policy.get("side_effect") is False,
             "illegal_transition_policy.side_effect_not_false")
    actions = policy.get("actions_universe")
    _require(isinstance(actions, list) and actions,
             "illegal_transition_policy.actions_universe_bad")
    _require(set(actions) == set(EXPECTED_ACTIONS) and
             len(actions) == len(EXPECTED_ACTIONS),
             "illegal_transition_policy.actions_mismatch")
    # 已出现的 action 必须都在全集内（不引入未声明动作）。
    for transition in matrix["transitions"]:
        _require(transition["action"] in set(actions),
                 "illegal_transition_policy.action_not_in_universe:" +
                 transition["action"])

    # 逐对机验：补集在 evaluate() 下必须全部被拒（返回 None）。
    for state, action in illegal_complement(matrix):
        _require(evaluate(matrix, state, action) is None,
                 "illegal_complement.not_denied:%s+%s" % (state, action))


def check_approval_rules(matrix):
    """审批：first-writer-wins、重复决定幂等、仅 human_approver。"""
    rules = matrix.get("approval_rules")
    _require(isinstance(rules, dict), "approval_rules.not_object")
    _require(set(rules.get("decision_actions", [])) == {"approve", "reject"},
             "approval_rules.decision_actions_mismatch")
    _require(rules.get("arbitration") == "first-writer-wins",
             "approval_rules.arbitration_not_first_writer_wins")
    constraints = rules.get("approver_constraints")
    _require(isinstance(constraints, list) and constraints,
             "approval_rules.approver_constraints_bad")
    duplicate = rules.get("duplicate_decision")
    _require(isinstance(duplicate, dict), "approval_rules.duplicate_decision_bad")
    _require(duplicate.get("outcome") == "already_decided",
             "approval_rules.duplicate_outcome_not_already_decided")
    _require(duplicate.get("state_change") is False,
             "approval_rules.duplicate_state_change_not_false")
    _require(duplicate.get("new_side_effects") is False,
             "approval_rules.duplicate_new_side_effects_not_false")
    _require(_is_str(rules.get("single_execution_after_approval")),
             "approval_rules.single_execution_bad")

    for transition in matrix["transitions"]:
        if transition["action"] in ("approve", "reject"):
            _require(transition["from"] == "awaiting_approval",
                     "approval_rules.bad_source:%s" % transition["from"])
            _require(transition["allowed_actors"] == ["human_approver"],
                     "approval_rules.decision_actor_not_human:" +
                     ",".join(transition["allowed_actors"]))


def check_expiry_rules(matrix):
    """过期：expire 仅 system、仅 submitted/awaiting_approval、expired 终态。"""
    rules = matrix.get("expiry_rules")
    _require(isinstance(rules, dict), "expiry_rules.not_object")
    _require(rules.get("action") == "expire", "expiry_rules.action_not_expire")
    _require(rules.get("trigger_actor") == "system",
             "expiry_rules.trigger_actor_not_system")
    _require(rules.get("expired_is_terminal") is True,
             "expiry_rules.expired_not_terminal")
    _require(_is_str(rules.get("semantics")), "expiry_rules.semantics_bad")

    expire_sources = set()
    for transition in matrix["transitions"]:
        if transition["action"] == "expire":
            _require(transition["allowed_actors"] == ["system"],
                     "expiry_rules.expire_actor_not_system")
            _require(transition["to"] == "expired",
                     "expiry_rules.expire_target_not_expired")
            expire_sources.add(transition["from"])
    declared_sources = set(rules.get("source_states", []))
    _require(declared_sources == expire_sources,
             "expiry_rules.source_states_mismatch")


def check_restart_and_execution(matrix, raw_text):
    """重启恢复与执行语义（含被禁承诺字样的文本禁令）。"""
    restart = matrix.get("restart_recovery")
    _require(isinstance(restart, dict), "restart_recovery.not_object")
    _require(restart.get("replay_from") == "persisted_state",
             "restart_recovery.replay_from_not_persisted_state")
    _require(restart.get("auto_new_side_effects") is False,
             "restart_recovery.auto_new_side_effects_not_false")
    _require(_is_str(restart.get("rule")), "restart_recovery.rule_bad")

    semantics = matrix.get("execution_semantics")
    _require(isinstance(semantics, dict), "execution_semantics.not_object")
    _require(_is_str(semantics.get("side_effect_key")),
             "execution_semantics.side_effect_key_bad")
    _require(_is_str(semantics.get("scheduling")),
             "execution_semantics.scheduling_bad")
    _require(_is_str(semantics.get("default_guarantee")),
             "execution_semantics.default_guarantee_bad")
    _require("at-most-once" in semantics["default_guarantee"],
             "execution_semantics.default_guarantee_missing_at_most_once")
    _require(semantics.get("recoverable_retry_requires_provable_idempotency")
             is True,
             "execution_semantics.retry_requires_provable_not_true")
    _require(semantics.get("uncertain_outcome_handling") == "manual_review",
             "execution_semantics.uncertain_outcome_not_manual_review")
    _require(_is_str(semantics.get("forbidden_claims")),
             "execution_semantics.forbidden_claims_bad")

    # 文本级禁令：交付矩阵中不得出现无法兑现的恰好一次承诺字样。
    _require(FORBIDDEN_TEXT not in raw_text,
             "execution_semantics.forbidden_text_present")


def verify_matrix(matrix, raw_text=""):
    """全量校验；返回 (errors, counts)。合法时 errors 为空。"""
    errors = []
    checks = (
        check_structure,
        check_state_contract,
        check_transitions_contract,
        check_terminal_rules,
        check_illegal_complement,
        check_approval_rules,
        check_expiry_rules,
    )
    for check in checks:
        try:
            check(matrix)
        except MatrixError as exc:
            errors.append(str(exc))
        except Exception as exc:  # fail-closed：结构异常也归为违规，绝不崩溃放行
            errors.append("check.%s.crash:%s" % (check.__name__, type(exc).__name__))
    try:
        check_restart_and_execution(matrix, raw_text)
    except MatrixError as exc:
        errors.append(str(exc))
    except Exception as exc:  # fail-closed：同上
        errors.append("check.check_restart_and_execution.crash:%s" % type(exc).__name__)

    counts = {
        "states": len(EXPECTED_STATES),
        "terminal_states": len(EXPECTED_TERMINAL),
        "legal_edges": len(EXPECTED_EDGES),
        "actions": len(EXPECTED_ACTIONS),
        "illegal_complement_pairs": (
            len(EXPECTED_STATES) * len(EXPECTED_ACTIONS) - len(EXPECTED_EDGES)
        ),
    }
    return errors, counts


def main(argv):
    usage = "usage: verify_agent_hub_task_state_machine.py <matrix.json>"
    if len(argv) != 1:
        print(usage, file=sys.stderr)
        return 2
    path = argv[0]
    try:
        with open(path, "r", encoding="utf-8") as handle:
            raw_text = handle.read()
        matrix = json.loads(raw_text)
    except (OSError, ValueError) as exc:
        print(json.dumps(
            {"decision": "FAIL", "matrix": path,
             "errors": ["matrix.unreadable:%s" % type(exc).__name__]},
            ensure_ascii=False, sort_keys=True))
        return 2
    errors, counts = verify_matrix(matrix, raw_text)
    result = {
        "decision": "PASS" if not errors else "FAIL",
        "matrix": path,
        "counts": counts,
    }
    if errors:
        result["errors"] = errors
    print(json.dumps(result, ensure_ascii=False, sort_keys=True))
    return 0 if not errors else 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
