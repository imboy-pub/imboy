"""EVID-00 verifier 测试：Schema 正反 fixtures 与全部 fail-closed 条件。

fixtures 全部在 tempfile 中动态构造，不新增仓库文件（任务卡独占文件仅
schema/verifier/本测试三个）。
"""

import copy
import hashlib
import importlib.util
import json
import os
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/verify_agent_hub_task_evidence.py"
SPEC = importlib.util.spec_from_file_location("agent_hub_evidence", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)

SHA_A = "0" * 64
SHA_B = "1" * 64
BASE = {
    "imboy": "a" * 40, "imboyapp": "b" * 40, "imboyadmin": "c" * 40,
}


def digest(path):
    with open(path, "rb") as handle:
        return hashlib.sha256(handle.read()).hexdigest()


def make_evidence(tmp, task_id="TST-01", status="PASS",
                  verification="verified", task_dir=None,
                  superseded_by=None, base_override=None):
    """构造一份结构完整的 evidence 并落盘 artifact 文件。

    superseded_by：verification="superseded" 时写入指针。
    base_override：可选 {repo: sha} 覆盖项，用于跨 Base 负例。"""
    task_dir = task_dir or task_id
    tdir = Path(tmp) / task_dir
    tdir.mkdir(parents=True, exist_ok=True)
    art_file = tdir / "baseline.md"
    art_file.write_text("baseline", encoding="utf-8")
    art_hash = digest(art_file)
    base = dict(BASE)
    if base_override:
        base.update(base_override)
    ev = {
        "schema_version": 1,
        "task_id": task_id,
        "status": status,
        "verification_status": verification,
        "base_sha": base,
        "final_diff": [],
        "commands": [
            {"id": "cmd-01", "command": "make eunit-local t=x_tests", "exit_code": 0},
        ],
        "tests": {"passed": 3, "failed": 0, "skipped": 0},
        "acceptance": [
            {
                "acceptance_id": task_id + "-A01",
                "status": "PASS",
                "command_ids": ["cmd-01"],
                "artifact_ids": ["artifact-01"],
                "assertions": ["ok"],
                "counts": {"n": 1},
            },
        ],
        "artifacts": [
            {"id": "artifact-01", "path": str(art_file), "sha256": art_hash},
        ],
        "residual_risks": [],
        "commit": "not-created-no-identity-approval",
    }
    if superseded_by is not None:
        ev["superseded_by"] = superseded_by
    path = tdir / "evidence.json"
    path.write_text(json.dumps(ev, indent=2), encoding="utf-8")
    return ev, path


def write_required_tsv(tmp, rows, header="task_id\tsuperseded_by\tnotes"):
    """写一份 required-set TSV。rows: [(task_id, superseded_by 或 None, notes)]。

    None → "-"（无 supersession）。返回 TSV 路径。"""
    lines = [header]
    for task_id, target, note in rows:
        lines.append("%s\t%s\t%s" % (task_id, target or "-", note))
    path = Path(tmp) / "required-agent-hub-tasks.tsv"
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    return path


class ShapePositiveTest(unittest.TestCase):
    """A01 正例：合法证据得到预期 decision。"""

    def run_case(self, **kwargs):
        with tempfile.TemporaryDirectory() as tmp:
            _, path = make_evidence(tmp, **kwargs)
            return MODULE.verify_task_file(str(path))

    def test_verified_pass(self):
        self.assertEqual(self.run_case()["decision"], "PASS")

    def test_draft_pass_is_draft(self):
        self.assertEqual(self.run_case(verification="draft")["decision"], "DRAFT")

    def test_superseded(self):
        self.assertEqual(self.run_case(verification="superseded")["decision"],
                         "SUPERSEDED")

    def test_superseded_with_pointer(self):
        # superseded 证据携带 superseded_by 指针：合法，decision 仍 SUPERSEDED。
        with tempfile.TemporaryDirectory() as tmp:
            ev, path = make_evidence(tmp, verification="superseded")
            ev["superseded_by"] = "NEW-01"
            path.write_text(json.dumps(ev), encoding="utf-8")
            result = MODULE.verify_task_file(str(path))
            self.assertEqual(result["decision"], "SUPERSEDED")

    def test_blocked_is_legal_evidence(self):
        # BLOCKED 是合法证据：结构完整即机器结论 BLOCKED（非 INVALID）。
        with tempfile.TemporaryDirectory() as tmp:
            ev, path = make_evidence(tmp, status="BLOCKED")
            ev["acceptance"][0]["status"] = "SKIP"
            path.write_text(json.dumps(ev), encoding="utf-8")
            self.assertEqual(MODULE.verify_task_file(str(path))["decision"],
                             "BLOCKED")

    def test_partial_with_fail_acceptance(self):
        with tempfile.TemporaryDirectory() as tmp:
            ev, path = make_evidence(tmp, status="PARTIAL")
            ev["acceptance"][0]["status"] = "FAIL"
            ev["tests"]["failed"] = 1
            path.write_text(json.dumps(ev), encoding="utf-8")
            self.assertEqual(MODULE.verify_task_file(str(path))["decision"],
                             "PARTIAL")


class FailClosedTest(unittest.TestCase):
    """A02：每种 fail-closed 条件都得到 INVALID。"""

    def verify_mutated(self, mutate, task_id="TST-01", **kwargs):
        with tempfile.TemporaryDirectory() as tmp:
            ev, path = make_evidence(tmp, task_id=task_id, **kwargs)
            mutate(ev, path, tmp)
            path.write_text(json.dumps(ev), encoding="utf-8")
            result = MODULE.verify_task_file(str(path))
            self.assertEqual(result["decision"], "INVALID")
            return result["errors"]

    def test_missing_required_field(self):
        for field in ("status", "commands", "acceptance", "artifacts", "commit"):
            def drop(ev, path, tmp, missing=field):
                ev.pop(missing)
            errors = self.verify_mutated(drop)
            self.assertTrue(any(field in e or "missing" in e for e in errors),
                            errors)

    def test_missing_acceptance_entries(self):
        errors = self.verify_mutated(lambda ev, p, t: ev.__setitem__("acceptance", []))
        self.assertIn("acceptance.empty", errors)

    def test_duplicate_acceptance_id(self):
        def dup(ev, path, tmp):
            ev["acceptance"].append(dict(ev["acceptance"][0]))
        self.assertIn("acceptance.duplicate_id",
                      self.verify_mutated(dup))

    def test_forged_artifact_path(self):
        def forge(ev, path, tmp):
            ev["artifacts"][0]["path"] = "/nonexistent/forbidden.bin"
        self.assertIn("artifacts.file_missing", self.verify_mutated(forge))

    def test_hash_mismatch(self):
        def bad_hash(ev, path, tmp):
            ev["artifacts"][0]["sha256"] = SHA_B
        self.assertIn("artifacts.hash_mismatch", self.verify_mutated(bad_hash))

    def test_nonzero_command_backing_pass(self):
        def nonzero(ev, path, tmp):
            ev["commands"][0]["exit_code"] = 1
        self.assertIn("acceptance.pass_backed_by_nonzero_command",
                      self.verify_mutated(nonzero))

    def test_pass_with_failed_tests(self):
        def failed(ev, path, tmp):
            ev["tests"]["failed"] = 2
        self.assertIn("status.pass_with_failed_tests",
                      self.verify_mutated(failed))

    def test_blocked_impersonating_pass(self):
        # status=PASS 但 acceptance 有 FAIL → 冒充 → INVALID。
        def impersonate(ev, path, tmp):
            ev["acceptance"][0]["status"] = "FAIL"
        self.assertIn("status.pass_with_non_pass_acceptance",
                      self.verify_mutated(impersonate))

    def test_task_id_directory_mismatch(self):
        # 目录名与 task_id 不一致：evidence 必须位于 <TASK_ID>/evidence.json。
        errors = self.verify_mutated(lambda ev, p, t: None,
                                     task_id="OTH-99", task_dir="TST-01")
        self.assertIn("task_id.mismatch_with_directory", errors)

    def test_unknown_command_reference(self):
        def unknown(ev, path, tmp):
            ev["acceptance"][0]["command_ids"] = ["cmd-99"]
        self.assertIn("acceptance.unknown_command_ref",
                      self.verify_mutated(unknown))

    def test_bad_sha256_pattern(self):
        def badsha(ev, path, tmp):
            ev["artifacts"][0]["sha256"] = "xyz"
        self.assertIn("artifacts.sha256_bad_pattern", self.verify_mutated(badsha))

    def test_bad_verification_status(self):
        def badv(ev, path, tmp):
            ev["verification_status"] = "final"
        self.assertIn("verification_status.bad_enum",
                      self.verify_mutated(badv))

    def test_bad_status_enum(self):
        def bads(ev, path, tmp):
            ev["status"] = "DRAFT"
        self.assertIn("status.bad_enum", self.verify_mutated(bads))

    def test_unreadable_json(self):
        result = MODULE.verify_task_file("/nonexistent/evidence.json")
        self.assertEqual(result["decision"], "INVALID")

    def test_final_diff_repo_relative_ok_and_bare_path_rejected(self):
        def good(ev, path, tmp):
            ev["final_diff"] = ["imboy:src/logic/x.erl", "imboyapp:lib/page/y.dart"]
        # repo 相对路径合法：不触发 INVALID。
        with tempfile.TemporaryDirectory() as tmp:
            _, path = make_evidence(tmp)
            ev = json.loads(path.read_text(encoding="utf-8"))
            good(ev, path, tmp)
            path.write_text(json.dumps(ev), encoding="utf-8")
            self.assertEqual(MODULE.verify_task_file(str(path))["decision"],
                             "PASS")
        # 裸路径（无 repo: 前缀）非法。
        def bare(ev, path, tmp):
            ev["final_diff"] = ["src/logic/x.erl"]
        self.assertIn("final_diff.item_bad_pattern",
                      self.verify_mutated(bare))

    def test_missing_base_sha_repo(self):
        def drop_repo(ev, path, tmp):
            ev["base_sha"].pop("imboyadmin")
        self.assertIn("base_sha.missing:imboyadmin",
                      self.verify_mutated(drop_repo))

    def test_superseded_by_requires_superseded_status(self):
        def add_pointer(ev, path, tmp):
            ev["superseded_by"] = "NEW-01"
        self.assertIn("superseded_by.requires_superseded_status",
                      self.verify_mutated(add_pointer))

    def test_superseded_by_self_reference(self):
        def self_ref(ev, path, tmp):
            ev["verification_status"] = "superseded"
            ev["superseded_by"] = ev["task_id"]
        self.assertIn("superseded_by.self_reference",
                      self.verify_mutated(self_ref))

    def test_superseded_by_bad_pattern(self):
        def bad_ref(ev, path, tmp):
            ev["verification_status"] = "superseded"
            ev["superseded_by"] = "not-a-task-ref"
        self.assertIn("superseded_by.bad_pattern",
                      self.verify_mutated(bad_ref))


class GateAggregationTest(unittest.TestCase):
    """A04：gate 汇总不把 BLOCKED/PARTIAL/DRAFT 聚合成 PASS。"""

    def gate(self, builder):
        with tempfile.TemporaryDirectory() as tmp:
            builder(tmp)
            return MODULE.verify_gate(tmp)

    def test_all_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="BBB-02")
        result = self.gate(build)
        self.assertEqual(result["decision"], "PASS")

    def test_blocked_not_aggregated_to_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="BBB-02", status="BLOCKED")
        result = self.gate(build)
        self.assertEqual(result["decision"], "NOT_PASS")

    def test_partial_not_aggregated_to_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01", status="PARTIAL")
        self.assertEqual(self.gate(build)["decision"], "NOT_PASS")

    def test_draft_not_aggregated_to_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01", verification="draft")
        self.assertEqual(self.gate(build)["decision"], "NOT_PASS")

    def test_invalid_takes_over(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            ev, _ = make_evidence(tmp, task_id="BBB-02")
            del ev["commands"]
            with open(Path(tmp) / "BBB-02" / "evidence.json", "w",
                      encoding="utf-8") as h:
                json.dump(ev, h)
        result = self.gate(build)
        self.assertEqual(result["decision"], "INVALID")

    def test_empty_gate_is_invalid(self):
        with tempfile.TemporaryDirectory() as tmp:
            self.assertEqual(MODULE.verify_gate(tmp)["decision"], "INVALID")

    def test_superseded_skipped_in_aggregation(self):
        # PASS + SUPERSEDED → gate PASS（superseded 不参与聚合）。
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="OLD-01", verification="superseded")
        result = self.gate(build)
        self.assertEqual(result["decision"], "PASS")
        self.assertEqual(result["superseded_skipped"], ["OLD-01"])

    def test_all_superseded_is_not_pass(self):
        # 全部 superseded = 空活跃集：fail-closed，不得聚合为 PASS。
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01", verification="superseded")
            make_evidence(tmp, task_id="OLD-02", verification="superseded")
        result = self.gate(build)
        self.assertEqual(result["decision"], "NOT_PASS")
        self.assertIn("gate.no_active_tasks", result["errors"])

    def test_superseded_but_corrupt_still_invalid(self):
        # 已 superseded 但结构损坏：仍是 INVALID（fail-closed 不因跳过而豁免）。
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            ev, _ = make_evidence(tmp, task_id="OLD-01",
                                  verification="superseded")
            del ev["commands"]
            with open(Path(tmp) / "OLD-01" / "evidence.json", "w",
                      encoding="utf-8") as h:
                json.dump(ev, h)
        result = self.gate(build)
        self.assertEqual(result["decision"], "INVALID")


class CliExitCodeTest(unittest.TestCase):
    """exit code 机器语义：0=PASS，1=合法未过，2=INVALID。"""

    def run_main(self, builder, flag):
        with tempfile.TemporaryDirectory() as tmp:
            target = builder(tmp)
            import contextlib, io
            buf = io.StringIO()
            with contextlib.redirect_stdout(buf):
                code = MODULE.main([flag, str(target)])
            return code, json.loads(buf.getvalue())

    def test_task_pass_exit_zero(self):
        def build(tmp):
            _, p = make_evidence(tmp)
            return p
        code, out = self.run_main(build, "--task")
        self.assertEqual((code, out["decision"]), (0, "PASS"))

    def test_task_invalid_exit_two(self):
        def build(tmp):
            ev, p = make_evidence(tmp)
            ev.pop("status")
            p.write_text(json.dumps(ev), encoding="utf-8")
            return p
        code, out = self.run_main(build, "--task")
        self.assertEqual((code, out["decision"]), (2, "INVALID"))

    def test_task_draft_exit_one(self):
        def build(tmp):
            _, p = make_evidence(tmp, verification="draft")
            return p
        code, out = self.run_main(build, "--task")
        self.assertEqual((code, out["decision"]), (1, "DRAFT"))

    def test_gate_not_pass_exit_one(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01", status="BLOCKED")
            return Path(tmp)
        code, out = self.run_main(build, "--gate")
        self.assertEqual((code, out["decision"]), (1, "NOT_PASS"))

    def test_bad_usage_exit_two(self):
        import contextlib, io
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            self.assertEqual(MODULE.main(["--wrong"]), 2)


class SchemaContractTest(unittest.TestCase):
    """Schema 文件与 verifier 常量的一致性（防两头漂移）。"""

    def test_schema_file_exists_and_parseable(self):
        with open(MODULE.SCHEMA_PATH, "r", encoding="utf-8") as h:
            schema = json.load(h)
        self.assertEqual(schema["properties"]["status"]["enum"],
                         list(MODULE.STATUS_ENUM))
        self.assertEqual(
            schema["properties"]["verification_status"]["enum"],
            list(MODULE.VERIFICATION_ENUM))
        self.assertEqual(
            schema["properties"]["acceptance"]["items"]["properties"]["status"]["enum"],
            list(MODULE.ACCEPTANCE_ENUM))


class RequiredSetGateTest(unittest.TestCase):
    """GATE-01-A01B：--required-set 严格模式。

    计划五负例（缺 required card 目录 / 集合外未知 card / replacement
    target 缺失、失败、跨 Base / replacement 环 / target 不在 required
    集合）都必须返回非 PASS（这里断言最强 fail-closed 语义：INVALID）。"""

    def strict_gate(self, rows, build, header="task_id\tsuperseded_by\tnotes"):
        with tempfile.TemporaryDirectory() as tmp:
            tsv = write_required_tsv(tmp, rows, header=header)
            build(tmp)
            return MODULE.verify_gate(tmp, required_tsv=str(tsv))

    # --- 负例 1：required card 目录缺失（删除整卡目录不得让 Gate 绿） ---
    def test_missing_required_card_directory(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
        result = self.strict_gate(
            [("AAA-01", None, "a"), ("BBB-02", None, "b")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("required_set.missing_task:BBB-02", result["errors"])
        self.assertEqual(result["required_set"]["missing"], ["BBB-02"])
        self.assertEqual(result["required_set"]["unknown"], [])

    # --- 负例 2：集合外多出未知 card ---
    def test_unknown_extra_card(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="BBB-02")
            make_evidence(tmp, task_id="ZZZ-99")
        result = self.strict_gate(
            [("AAA-01", None, "a"), ("BBB-02", None, "b")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("required_set.unknown_task:ZZZ-99", result["errors"])
        self.assertEqual(result["required_set"]["unknown"], ["ZZZ-99"])
        self.assertEqual(result["required_set"]["missing"], [])

    # --- 负例 3a：replacement target 目录缺失 ---
    def test_replacement_target_missing(self):
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.target_missing:OLD-01->NEW-01",
                      result["errors"])

    # --- 负例 3b：replacement target 未 PASS（失败/未结算） ---
    def test_replacement_target_not_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01", status="FAIL")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.target_not_pass:NEW-01:FAIL",
                      result["errors"])

    def test_replacement_target_draft_is_not_pass(self):
        # draft = 替换证据未结算，同样不得放行。
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01", verification="draft")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertIn("supersession.target_not_pass:NEW-01:DRAFT",
                      result["errors"])
        self.assertNotEqual(result["decision"], "PASS")

    # --- 负例 3c：replacement target 跨 Base ---
    def test_replacement_target_cross_base(self):
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01",
                          base_override={"imboy": "d" * 40})
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.cross_base:OLD-01->NEW-01",
                      result["errors"])

    # --- 负例 4：replacement 环 ---
    def test_replacement_cycle(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01",
                          verification="superseded", superseded_by="BBB-02")
            make_evidence(tmp, task_id="BBB-02",
                          verification="superseded", superseded_by="AAA-01")
        result = self.strict_gate(
            [("AAA-01", "BBB-02", ""), ("BBB-02", "AAA-01", "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertTrue(
            any(e.startswith("supersession.cycle:") for e in result["errors"]),
            result["errors"])

    def test_replacement_self_reference_cycle(self):
        # 自指 = 长度 1 的环。
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01",
                          verification="superseded", superseded_by="AAA-01")
        result = self.strict_gate([("AAA-01", "AAA-01", "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertTrue(
            any(e.startswith("supersession.cycle:") for e in result["errors"]),
            result["errors"])

    # --- 负例 5：replacement target 不在 required 集合 ---
    def test_replacement_target_not_in_required_set(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="OUT-01")
        result = self.strict_gate(
            [("AAA-01", None, "a"), ("OLD-01", "OUT-01", "")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.target_not_in_required_set:OLD-01->OUT-01",
                      result["errors"])

    # --- 正例：required-set 相等、supersession 闭合、全部活跃卡 PASS ---
    def test_required_set_match_all_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01")
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01")
        result = self.strict_gate(
            [("AAA-01", None, "a"),
             ("OLD-01", "NEW-01", "superseded via NEW-01"),
             ("NEW-01", None, "n")], build)
        self.assertEqual(result["decision"], "PASS")
        self.assertEqual(result["superseded_skipped"], ["OLD-01"])
        self.assertEqual(result["required_set"]["missing"], [])
        self.assertEqual(result["required_set"]["unknown"], [])

    # --- TSV 与证据的一致性（闭合语义的一部分） ---
    def test_tsv_superseded_but_evidence_active(self):
        # TSV 声明 OLD-01 已被取代，但其证据仍是活跃态 → 不一致，INVALID。
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01")
            make_evidence(tmp, task_id="NEW-01")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertEqual(result["decision"], "INVALID")
        self.assertTrue(
            any(e.startswith("supersession.evidence_not_superseded:OLD-01")
                for e in result["errors"]), result["errors"])

    def test_tsv_active_but_evidence_superseded(self):
        # TSV 声明活跃、证据却自称 superseded → 不一致，INVALID。
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01",
                          verification="superseded", superseded_by="ZZZ-01")
            make_evidence(tmp, task_id="BBB-02")
        result = self.strict_gate(
            [("AAA-01", None, "a"), ("BBB-02", None, "b")], build)
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.evidence_superseded_but_tsv_active:AAA-01",
                      result["errors"])

    def test_superseded_by_pointer_mismatch(self):
        # 证据指针与 TSV 目标不一致 → INVALID。
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="WRONG-01")
            make_evidence(tmp, task_id="NEW-01")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("NEW-01", None, "")], build)
        self.assertEqual(result["decision"], "INVALID")
        self.assertIn("supersession.pointer_mismatch:OLD-01:WRONG-01->NEW-01",
                      result["errors"])

    # --- 活跃卡未过：合法证据但 NOT_PASS（不是 INVALID） ---
    def test_active_card_blocked_is_not_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="AAA-01", status="BLOCKED")
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01")
        result = self.strict_gate(
            [("AAA-01", None, "a"), ("OLD-01", "NEW-01", ""),
             ("NEW-01", None, "n")], build)
        self.assertNotEqual(result["decision"], "PASS")
        self.assertEqual(result["decision"], "NOT_PASS")

    # --- 全部被取代：fail-closed，不得聚合为 PASS ---
    def test_all_superseded_strict_is_not_pass(self):
        def build(tmp):
            make_evidence(tmp, task_id="OLD-01",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="OLD-02",
                          verification="superseded", superseded_by="NEW-01")
            make_evidence(tmp, task_id="NEW-01",
                          verification="superseded", superseded_by="OLD-01")
        result = self.strict_gate(
            [("OLD-01", "NEW-01", ""), ("OLD-02", "NEW-01", ""),
             ("NEW-01", "OLD-01", "")], build)
        self.assertNotEqual(result["decision"], "PASS")


class RequiredSetTsvRobustnessTest(unittest.TestCase):
    """required-set TSV 解析 fail-closed：坏 header/重复行/不可读 → INVALID。"""

    def test_bad_header(self):
        with tempfile.TemporaryDirectory() as tmp:
            bad = Path(tmp) / "bad.tsv"
            bad.write_text("id\tsub\tnote\nAAA-01\t-\tx\n", encoding="utf-8")
            make_evidence(tmp, task_id="AAA-01")
            result = MODULE.verify_gate(tmp, required_tsv=str(bad))
            self.assertEqual(result["decision"], "INVALID")
            self.assertIn("required_set.tsv_bad_header", result["errors"])

    def test_duplicate_task_row(self):
        with tempfile.TemporaryDirectory() as tmp:
            tsv = Path(tmp) / "dup.tsv"
            tsv.write_text(
                "task_id\tsuperseded_by\tnotes\nAAA-01\t-\ta\nAAA-01\t-\tb\n",
                encoding="utf-8")
            make_evidence(tmp, task_id="AAA-01")
            result = MODULE.verify_gate(tmp, required_tsv=str(tsv))
            self.assertEqual(result["decision"], "INVALID")
            self.assertIn("required_set.tsv_duplicate_task:AAA-01",
                          result["errors"])

    def test_unreadable_tsv(self):
        with tempfile.TemporaryDirectory() as tmp:
            make_evidence(tmp, task_id="AAA-01")
            result = MODULE.verify_gate(
                tmp, required_tsv=str(Path(tmp) / "nope.tsv"))
            self.assertEqual(result["decision"], "INVALID")
            self.assertTrue(
                any(e.startswith("required_set.tsv_unreadable:")
                    for e in result["errors"]), result["errors"])

    def test_bad_row_pattern(self):
        with tempfile.TemporaryDirectory() as tmp:
            tsv = Path(tmp) / "bad-row.tsv"
            tsv.write_text(
                "task_id\tsuperseded_by\tnotes\nnot a task\t-\tx\n",
                encoding="utf-8")
            make_evidence(tmp, task_id="AAA-01")
            result = MODULE.verify_gate(tmp, required_tsv=str(tsv))
            self.assertEqual(result["decision"], "INVALID")
            self.assertIn("required_set.tsv_bad_row:2", result["errors"])


class RequiredSetCliTest(unittest.TestCase):
    """CLI：--gate DIR --required-set TSV 的退出码与用法错误。"""

    def run_main(self, argv):
        import contextlib, io
        buf = io.StringIO()
        with contextlib.redirect_stdout(buf):
            code = MODULE.main(argv)
        return code, json.loads(buf.getvalue())

    def test_strict_gate_pass_exit_zero(self):
        with tempfile.TemporaryDirectory() as tmp:
            tsv = write_required_tsv(tmp, [("AAA-01", None, "a")])
            make_evidence(tmp, task_id="AAA-01")
            code, out = self.run_main(
                ["--gate", tmp, "--required-set", str(tsv)])
            self.assertEqual((code, out["decision"]), (0, "PASS"))

    def test_strict_gate_missing_card_exit_two(self):
        with tempfile.TemporaryDirectory() as tmp:
            tsv = write_required_tsv(
                tmp, [("AAA-01", None, "a"), ("BBB-02", None, "b")])
            make_evidence(tmp, task_id="AAA-01")
            code, out = self.run_main(
                ["--gate", tmp, "--required-set", str(tsv)])
            self.assertEqual((code, out["decision"]), (2, "INVALID"))

    def test_strict_gate_blocked_card_exit_one(self):
        # 集合相等但活跃卡 BLOCKED：合法证据未过 → 1（不是 2）。
        with tempfile.TemporaryDirectory() as tmp:
            tsv = write_required_tsv(tmp, [("AAA-01", None, "a")])
            make_evidence(tmp, task_id="AAA-01", status="BLOCKED")
            code, out = self.run_main(
                ["--gate", tmp, "--required-set", str(tsv)])
            self.assertEqual((code, out["decision"]), (1, "NOT_PASS"))

    def test_bare_gate_still_works_legacy(self):
        # 无 --required-set 的既有用法保持兼容（目录枚举语义不变）。
        with tempfile.TemporaryDirectory() as tmp:
            make_evidence(tmp, task_id="AAA-01")
            code, out = self.run_main(["--gate", tmp])
            self.assertEqual((code, out["decision"]), (0, "PASS"))
            self.assertNotIn("required_set", out)

    def test_usage_errors_exit_two(self):
        for argv in (
            ["--gate", "/x", "--required-set"],          # 缺 TSV 值
            ["--task", "/x", "--required-set", "/y"],    # --task 不接受该参数
            ["--gate", "/x", "--required", "/y"],        # 拼错 flag
            ["--required-set", "/y"],                    # 缺模式
        ):
            self.assertEqual(MODULE.main(list(argv)), 2, argv)


if __name__ == "__main__":
    unittest.main()
