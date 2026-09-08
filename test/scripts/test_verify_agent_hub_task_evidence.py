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
                  verification="verified", task_dir=None):
    """构造一份结构完整的 evidence 并落盘 artifact 文件。"""
    task_dir = task_dir or task_id
    tdir = Path(tmp) / task_dir
    tdir.mkdir(parents=True, exist_ok=True)
    art_file = tdir / "baseline.md"
    art_file.write_text("baseline", encoding="utf-8")
    art_hash = digest(art_file)
    ev = {
        "schema_version": 1,
        "task_id": task_id,
        "status": status,
        "verification_status": verification,
        "base_sha": dict(BASE),
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
    path = tdir / "evidence.json"
    path.write_text(json.dumps(ev, indent=2), encoding="utf-8")
    return ev, path


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


if __name__ == "__main__":
    unittest.main()
