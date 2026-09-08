"""FSM-00 状态机矩阵 verifier 测试。

正例：真实交付矩阵（docs/api-contracts/agent_hub_task_state_machine.json）
通过全部检查。反例：对每条 verifier 规则构造违规矩阵，逐条命中违规码。
加载方式与 test_verify_product_feature_artifacts.py 一致（importlib）。
"""

import copy
import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "scripts/verify_agent_hub_task_state_machine.py"
MATRIX = REPO_ROOT / "docs/api-contracts/agent_hub_task_state_machine.json"

SPEC = importlib.util.spec_from_file_location("agent_hub_fsm_verifier", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


def load_real_matrix():
    with open(MATRIX, "r", encoding="utf-8") as handle:
        return json.load(handle)


def errors_of(matrix, raw_text=""):
    errors, _ = MODULE.verify_matrix(matrix, raw_text)
    return errors


def has_code(errors, code):
    """违规码可带参数后缀（code:detail），按前缀精确匹配。"""
    return any(e == code or e.startswith(code + ":") for e in errors)


def drop_transition(matrix, from_state, action):
    matrix["transitions"] = [
        t for t in matrix["transitions"]
        if not (t["from"] == from_state and t["action"] == action)
    ]


class AgainstRealMatrixTest(unittest.TestCase):
    """正例：真实矩阵必须通过全部检查。"""

    def setUp(self):
        self.matrix = load_real_matrix()
        with open(MATRIX, "r", encoding="utf-8") as handle:
            self.raw = handle.read()

    def test_real_matrix_passes(self):
        errors, counts = MODULE.verify_matrix(self.matrix, self.raw)
        self.assertEqual(errors, [])

    def test_frozen_counts(self):
        _, counts = MODULE.verify_matrix(self.matrix, self.raw)
        self.assertEqual(counts["states"], 9)
        self.assertEqual(counts["terminal_states"], 5)
        self.assertEqual(counts["legal_edges"], 15)
        self.assertEqual(counts["actions"], 10)
        self.assertEqual(counts["illegal_complement_pairs"], 75)

    def test_every_legal_edge_evaluates(self):
        for edge in MODULE.EXPECTED_EDGES:
            source, action, target = edge
            self.assertEqual(MODULE.evaluate(self.matrix, source, action), target)

    def test_complement_size_and_samples_denied(self):
        complement = MODULE.illegal_complement(self.matrix)
        self.assertEqual(len(complement), 75)
        # terminal 上的一切动作 / 过期后补批准 / 非源头 approve 等。
        for state, action in (
            ("completed", "approve"),
            ("rejected", "reject"),
            ("expired", "approve"),
            ("cancelled", "complete"),
            ("failed", "cancel"),
            ("working", "start"),
            ("approved", "approve"),
            ("submitted", "resume"),
        ):
            self.assertIn((state, action), complement)
            self.assertIsNone(MODULE.evaluate(self.matrix, state, action))

    def test_every_non_terminal_reaches_terminal(self):
        for state in self.matrix["states"]:
            if not state["terminal"]:
                self.assertTrue(
                    MODULE.reachable_terminals(self.matrix, state["name"]),
                    state["name"],
                )

    def test_terminal_states_have_no_outgoing(self):
        sources = {t["from"] for t in self.matrix["transitions"]}
        for state in self.matrix["states"]:
            if state["terminal"]:
                self.assertNotIn(state["name"], sources)


class StructureRulesTest(unittest.TestCase):
    """反例：schema 自身结构。"""

    def test_missing_top_field(self):
        matrix = load_real_matrix()
        del matrix["approval_rules"]
        self.assertIn("top.missing:approval_rules", errors_of(matrix))

    def test_bad_schema_version(self):
        matrix = load_real_matrix()
        matrix["schema_version"] = 2
        self.assertIn("schema_version.not_1", errors_of(matrix))

    def test_transition_missing_field(self):
        matrix = load_real_matrix()
        del matrix["transitions"][0]["idempotency"]
        self.assertIn(
            "transitions.item_missing:idempotency", errors_of(matrix))

    def test_transition_empty_actors(self):
        matrix = load_real_matrix()
        matrix["transitions"][0]["allowed_actors"] = []
        self.assertIn("transitions.allowed_actors_bad", errors_of(matrix))

    def test_actor_missing_description(self):
        matrix = load_real_matrix()
        del matrix["actors"][0]["description"]
        self.assertIn("actors.item_description_bad", errors_of(matrix))


class StateContractTest(unittest.TestCase):
    """反例：九状态冻结面与初始态。"""

    def test_missing_state(self):
        matrix = load_real_matrix()
        matrix["states"] = [s for s in matrix["states"] if s["name"] != "rejected"]
        self.assertIn("states.missing:rejected", errors_of(matrix))

    def test_unexpected_state(self):
        matrix = load_real_matrix()
        matrix["states"].append(
            {"name": "paused", "terminal": False, "convergence_path": "x"})
        self.assertIn("states.unexpected:paused", errors_of(matrix))

    def test_duplicate_state_name(self):
        matrix = load_real_matrix()
        matrix["states"].append(copy.deepcopy(matrix["states"][0]))
        self.assertIn("states.duplicate_name", errors_of(matrix))

    def test_terminal_flag_mismatch(self):
        matrix = load_real_matrix()
        for state in matrix["states"]:
            if state["name"] == "submitted":
                state["terminal"] = True
        self.assertIn("states.terminal_mismatch:submitted", errors_of(matrix))

    def test_wrong_initial_state(self):
        matrix = load_real_matrix()
        matrix["initial_state"] = "working"
        self.assertIn("initial_state.not_submitted", errors_of(matrix))


class EdgeRulesTest(unittest.TestCase):
    """反例：重复边、悬空、actor 引用、边全集漂移。"""

    def test_duplicate_edge(self):
        matrix = load_real_matrix()
        matrix["transitions"].append(
            copy.deepcopy(matrix["transitions"][0]))
        self.assertIn("transitions.duplicate_edge", errors_of(matrix))

    def test_nondeterministic_pair(self):
        matrix = load_real_matrix()
        clone = copy.deepcopy(matrix["transitions"][0])
        clone["to"] = "failed" if clone["to"] != "failed" else "cancelled"
        matrix["transitions"].append(clone)
        self.assertTrue(
            has_code(errors_of(matrix), "transitions.duplicate_from_action"))

    def test_dangling_target_state(self):
        matrix = load_real_matrix()
        matrix["transitions"][0]["to"] = "nowhere"
        self.assertIn("transitions.dangling_to:nowhere", errors_of(matrix))

    def test_dangling_source_state(self):
        matrix = load_real_matrix()
        matrix["transitions"][0]["from"] = "nowhere"
        self.assertIn("transitions.dangling_from:nowhere", errors_of(matrix))

    def test_unknown_actor_reference(self):
        matrix = load_real_matrix()
        matrix["transitions"][0]["allowed_actors"] = ["mystery"]
        self.assertIn("transitions.unknown_actor:mystery", errors_of(matrix))

    def test_unexpected_extra_edge(self):
        # 在终态上私加 approve 边：非法迁移混入矩阵必须被冻结面拒绝。
        matrix = load_real_matrix()
        matrix["transitions"].append({
            "from": "completed",
            "action": "approve",
            "to": "approved",
            "allowed_actors": ["human_approver"],
            "idempotency": "x",
            "side_effect": "x",
        })
        self.assertIn("edges.unexpected:completed+approve>approved", errors_of(matrix))

    def test_missing_legal_edge(self):
        matrix = load_real_matrix()
        drop_transition(matrix, "awaiting_approval", "reject")
        self.assertIn("edges.missing:awaiting_approval+reject>rejected",
                      errors_of(matrix))


class TerminalAndConvergenceTest(unittest.TestCase):
    """反例：terminal 出边与收敛路径。"""

    def test_terminal_with_outgoing_edge(self):
        matrix = load_real_matrix()
        matrix["states"].append(
            {"name": "paused", "terminal": False, "convergence_path": "x"})
        matrix["transitions"].append({
            "from": "completed",
            "action": "start",
            "to": "working",
            "allowed_actors": ["agent_worker"],
            "idempotency": "x",
            "side_effect": "x",
        })
        errors = errors_of(matrix)
        self.assertIn("terminal.has_outgoing:completed", errors)

    def test_no_convergence_path(self):
        matrix = load_real_matrix()
        drop_transition(matrix, "approved", "resume")
        drop_transition(matrix, "approved", "fail")
        self.assertIn("convergence.no_path:approved", errors_of(matrix))

    def test_terminal_rules_flag_flipped(self):
        matrix = load_real_matrix()
        matrix["terminal_rules"]["irreversible"] = False
        self.assertIn("terminal_rules.irreversible_not_true", errors_of(matrix))

    def test_terminal_rules_states_mismatch(self):
        matrix = load_real_matrix()
        matrix["terminal_rules"]["terminal_states"] = [
            "rejected", "expired", "completed", "failed",
        ]
        self.assertIn("terminal_rules.states_mismatch", errors_of(matrix))


class IllegalComplementTest(unittest.TestCase):
    """反例：默认拒绝策略被削弱。"""

    def test_default_outcome_not_rejected(self):
        matrix = load_real_matrix()
        matrix["illegal_transition_policy"]["default_outcome"] = "allowed"
        self.assertIn(
            "illegal_transition_policy.default_outcome_not_rejected",
            errors_of(matrix))

    def test_actions_universe_mismatch(self):
        matrix = load_real_matrix()
        matrix["illegal_transition_policy"]["actions_universe"].append("retry")
        self.assertIn(
            "illegal_transition_policy.actions_mismatch", errors_of(matrix))

    def test_complement_allows_state_change(self):
        matrix = load_real_matrix()
        matrix["illegal_transition_policy"]["state_change"] = True
        self.assertIn(
            "illegal_transition_policy.state_change_not_false", errors_of(matrix))


class ApprovalRulesTest(unittest.TestCase):
    """反例：并发（first-writer-wins）与重复决定语义。"""

    def test_arbitration_weakened(self):
        matrix = load_real_matrix()
        matrix["approval_rules"]["arbitration"] = "last-writer-wins"
        self.assertIn(
            "approval_rules.arbitration_not_first_writer_wins", errors_of(matrix))

    def test_duplicate_decision_with_side_effects(self):
        matrix = load_real_matrix()
        matrix["approval_rules"]["duplicate_decision"]["new_side_effects"] = True
        self.assertIn(
            "approval_rules.duplicate_new_side_effects_not_false", errors_of(matrix))

    def test_duplicate_decision_changes_state(self):
        matrix = load_real_matrix()
        matrix["approval_rules"]["duplicate_decision"]["state_change"] = True
        self.assertIn(
            "approval_rules.duplicate_state_change_not_false", errors_of(matrix))

    def test_decision_actions_mismatch(self):
        matrix = load_real_matrix()
        matrix["approval_rules"]["decision_actions"] = ["approve"]
        self.assertIn(
            "approval_rules.decision_actions_mismatch", errors_of(matrix))

    def test_agent_cannot_self_approve(self):
        matrix = load_real_matrix()
        for t in matrix["transitions"]:
            if t["action"] == "approve":
                t["allowed_actors"] = ["agent_worker", "human_approver"]
        self.assertIn(
            "approval_rules.decision_actor_not_human:agent_worker,human_approver",
            errors_of(matrix))

    def test_approve_from_wrong_source(self):
        matrix = load_real_matrix()
        for t in matrix["transitions"]:
            if t["action"] == "approve":
                t["from"] = "working"
        self.assertIn("approval_rules.bad_source:working", errors_of(matrix))


class ExpiryRulesTest(unittest.TestCase):
    """反例：过期规则。"""

    def test_trigger_actor_not_system(self):
        matrix = load_real_matrix()
        matrix["expiry_rules"]["trigger_actor"] = "agent_worker"
        self.assertIn("expiry_rules.trigger_actor_not_system", errors_of(matrix))

    def test_source_states_mismatch(self):
        matrix = load_real_matrix()
        matrix["expiry_rules"]["source_states"] = ["awaiting_approval"]
        self.assertIn("expiry_rules.source_states_mismatch", errors_of(matrix))

    def test_expired_not_terminal_flag(self):
        matrix = load_real_matrix()
        matrix["expiry_rules"]["expired_is_terminal"] = False
        self.assertIn("expiry_rules.expired_not_terminal", errors_of(matrix))


class RestartExecutionTest(unittest.TestCase):
    """反例：重启恢复与执行语义（含被禁承诺字样的文本禁令）。"""

    def test_restart_produces_side_effects(self):
        matrix = load_real_matrix()
        matrix["restart_recovery"]["auto_new_side_effects"] = True
        self.assertIn(
            "restart_recovery.auto_new_side_effects_not_false", errors_of(matrix))

    def test_restart_not_from_persisted_state(self):
        matrix = load_real_matrix()
        matrix["restart_recovery"]["replay_from"] = "ets_snapshot"
        self.assertIn(
            "restart_recovery.replay_from_not_persisted_state", errors_of(matrix))

    def test_retry_without_provable_idempotency(self):
        matrix = load_real_matrix()
        matrix["execution_semantics"][
            "recoverable_retry_requires_provable_idempotency"] = False
        self.assertIn(
            "execution_semantics.retry_requires_provable_not_true",
            errors_of(matrix))

    def test_uncertain_outcome_not_manual_review(self):
        matrix = load_real_matrix()
        matrix["execution_semantics"]["uncertain_outcome_handling"] = "auto_retry"
        self.assertIn(
            "execution_semantics.uncertain_outcome_not_manual_review",
            errors_of(matrix))

    def test_default_guarantee_without_at_most_once(self):
        matrix = load_real_matrix()
        matrix["execution_semantics"]["default_guarantee"] = "尽力交付"
        self.assertIn(
            "execution_semantics.default_guarantee_missing_at_most_once",
            errors_of(matrix))

    def test_forbidden_exactly_once_claim(self):
        matrix = load_real_matrix()
        # 拼接构造被禁字样，保证本测试源文件自身不含该词。
        raw = json.dumps(matrix, ensure_ascii=False) + " " + "exactly" + "-once" + " "
        self.assertIn(
            "execution_semantics.forbidden_text_present",
            errors_of(matrix, raw))

    def test_real_matrix_text_has_no_forbidden_claim(self):
        with open(MATRIX, "r", encoding="utf-8") as handle:
            raw = handle.read()
        self.assertNotIn(MODULE.FORBIDDEN_TEXT, raw)


class CliExitCodeTest(unittest.TestCase):
    """CLI 退出码：0=合法矩阵，2=违规/不可读/用法错误。"""

    def run_cli(self, *args):
        return subprocess.run(
            [sys.executable, str(SCRIPT)] + [str(a) for a in args],
            capture_output=True, text=True)

    def test_real_matrix_exit_zero(self):
        proc = self.run_cli(MATRIX)
        self.assertEqual(proc.returncode, 0)
        payload = json.loads(proc.stdout)
        self.assertEqual(payload["decision"], "PASS")

    def test_violating_matrix_exit_two(self):
        matrix = load_real_matrix()
        drop_transition(matrix, "working", "need_approval")
        with tempfile.TemporaryDirectory() as directory:
            bad = Path(directory) / "bad.json"
            bad.write_text(json.dumps(matrix, ensure_ascii=False),
                           encoding="utf-8")
            proc = self.run_cli(bad)
            self.assertEqual(proc.returncode, 2)
            payload = json.loads(proc.stdout)
            self.assertEqual(payload["decision"], "FAIL")

    def test_broken_json_exit_two(self):
        with tempfile.TemporaryDirectory() as directory:
            bad = Path(directory) / "broken.json"
            bad.write_text("{not json", encoding="utf-8")
            proc = self.run_cli(bad)
            self.assertEqual(proc.returncode, 2)
            self.assertIn("matrix.unreadable", proc.stdout)

    def test_missing_args_exit_two(self):
        proc = self.run_cli()
        self.assertEqual(proc.returncode, 2)


if __name__ == "__main__":
    unittest.main()
