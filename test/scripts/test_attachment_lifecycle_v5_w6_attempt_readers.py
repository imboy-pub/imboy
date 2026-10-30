"""W6 adapters must consume the exact bound generation and candidate."""
import json
import os
from pathlib import Path
import sys
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / "scripts"))
sys.path.insert(0, str(ROOT / "scripts/test"))
import attachment_lifecycle_v5_case_store as store
import attachment_lifecycle_v5_runner as runner
import attachment_lifecycle_v5_adapter_w6_01_all_ac01_ac07 as union
import attachment_lifecycle_v5_adapter_w6_03_ledger_verify as ledger
import attachment_lifecycle_v5_adapter_w6_02_support as w602


class W6BoundReadersTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.run = Path(self.tmp.name).resolve()
        self.case_id = union.SOURCE_CASE
        self.case = self.run / "evidence/W6-01/real-pg-garage-disk"
        self.candidate = {"heads": {"imboy": "a" * 40}, "owned_tree_digest": "b" * 64,
                          "plan_manifest_sha256": "c" * 64, "migration_digest": "d" * 64}
        self.real = [dict(ac_id=ac, domain="ordinary", driver="disk", entrypoint="upload",
                          ordering_or_fault="scenario-" + ac, schema_phase="integrated",
                          source_case_ref="W2-01/pin", source_scenario="scenario-" + ac)
                     for ac in union.ACS]
        self.contract = {"expected_leaf_matrix": {self.case_id: self.real,
                         "W2-01/pin": [dict(ordering_or_fault="scenario-" + ac) for ac in union.ACS],
                         union.CASE_ID: [dict(ac_id="PLAN")]},
                         "final_ac_oracle_refs": {ac: ["W2-01/pin"] for ac in union.ACS},
                         "tasks": [{"task_id": "W6-01", "case_ids": [self.case_id]}]}
        self.state = {"verification_candidate": self.candidate,
                      "tasks": {"W6-01": {"state": "RUNNING", "attempt": 2}},
                      "case_manifest_attempts": {self.case_id: 2}, "case_manifest_digests": {}}
        self.rows = [dict(leaf, oracle_result="PASS", exit_code=0, failed=0, skipped=0,
                          executed=1, task_attempt=2, candidate_heads=self.candidate["heads"],
                          owned_tree_digest=self.candidate["owned_tree_digest"],
                          plan_manifest_sha256=self.candidate["plan_manifest_sha256"],
                          migration_digest=self.candidate["migration_digest"],
                          actor_provenance={"kind": "real", "resource_id": "fixture"},
                          request_ids=["req"]) for leaf in self.real]

    def publish(self, directory, rows):
        directory.mkdir(parents=True)
        fd = os.open(directory, os.O_RDONLY | os.O_DIRECTORY)
        try:
            store.publish(fd, rows)
            self.state["case_manifest_digests"][self.case_id] = store.manifest_digest(fd)
        finally:
            os.close(fd)

    def build_union(self):
        ctx = {"state": self.state, "contract": self.contract, "allowlist": {}}
        def report(_cid, _count, details):
            raise ValueError(json.dumps(details))
        with mock.patch.object(union.S.C, "argv_paths", return_value=(str(self.run), str(self.run), str(self.run))), \
             mock.patch.object(union.S, "context", return_value=ctx), \
             mock.patch.object(union.S, "worktree", return_value=str(self.run)), \
             mock.patch.object(union.S, "report_gaps", side_effect=report), \
             mock.patch.object(union.S, "step_record", return_value={}), \
             mock.patch.object(union.S, "step_window", return_value=("start", "end")), \
             mock.patch.object(union.S, "assertion", side_effect=lambda *a: a), \
             mock.patch.object(union.S, "repo_actor", return_value={}), \
             mock.patch.object(union.S.C, "build_row", return_value={}), \
             mock.patch.object(union.S.C, "assert_leaf_set"):
            return union.build()

    def test_attempt_bound_case_is_read_by_union_and_ledger(self):
        self.publish(self.case / "attempt-2", self.rows)
        self.assertEqual(1, len(self.build_union()[1]))
        with mock.patch.object(runner, "_require_state", return_value=self.state):
            result = ledger._sealed_receipts(str(self.run), ledger.CASE_ID, self.contract)
        self.assertEqual(7, len(result[self.case_id]))
        self.assertEqual(str(self.case / "attempt-2"), result[self.case_id][0][2])

    def test_legacy_flat_case_remains_readable_when_bound(self):
        self.state.pop("case_manifest_attempts")
        self.publish(self.case, self.rows)
        self.assertEqual(1, len(self.build_union()[1]))

    def test_earlier_sealed_source_survives_later_task_attempt(self):
        self.publish(self.case / "attempt-2", self.rows)
        self.state["tasks"]["W6-01"]["attempt"] = 3
        self.assertEqual(1, len(self.build_union()[1]))

    def test_source_receipt_must_match_its_own_binding(self):
        rows = [dict(row, task_attempt=3) for row in self.rows]
        self.publish(self.case / "attempt-2", rows)
        self.state["tasks"]["W6-01"]["attempt"] = 3
        with self.assertRaisesRegex(ValueError, "task attempt binding mismatch"):
            self.build_union()

    def test_missing_current_attempt_does_not_fall_back_to_flat(self):
        self.publish(self.case, self.rows)
        with self.assertRaisesRegex(ValueError, "unbound or corrupt source case"):
            self.build_union()

    def test_duplicate_or_wrong_candidate_is_rejected(self):
        wrong = [dict(row) for row in self.rows]
        wrong[-1] = dict(wrong[0], candidate_heads={"imboy": "f" * 40})
        self.publish(self.case / "attempt-2", wrong)
        with self.assertRaisesRegex(ValueError, "duplicate sealed leaf identities") as caught:
            self.build_union()
        self.assertIn("candidate binding mismatch", str(caught.exception))

    def test_w602_producer_probe_uses_current_attempt_layout(self):
        path = str(ROOT / "scripts/attachment_lifecycle_v5_producer.py")
        self.assertEqual([], w602.producer_drops(path, ["device_provenance", "browser_provenance"], "probe"))

    def test_ledger_rejects_attempt_mismatch_and_manifest_tampering(self):
        wrong = [dict(row, task_attempt=1) for row in self.rows]
        self.publish(self.case / "attempt-2", wrong)
        with mock.patch.object(runner, "_require_state", return_value=self.state), \
             mock.patch.object(ledger.S6, "die", side_effect=ValueError("unbound")):
            with self.assertRaisesRegex(ValueError, "unbound"):
                ledger._sealed_receipts(str(self.run), ledger.CASE_ID, self.contract)
            self.state["case_manifest_digests"][self.case_id] = "0" * 64
            with self.assertRaisesRegex(ValueError, "unbound"):
                ledger._sealed_receipts(str(self.run), ledger.CASE_ID, self.contract)


if __name__ == "__main__":
    unittest.main()
