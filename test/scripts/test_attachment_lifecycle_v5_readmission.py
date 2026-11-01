import copy
import datetime
import json
import unittest
from pathlib import Path

from scripts import attachment_lifecycle_v5_readmission as admission


def encoded(value):
    return json.dumps(value, sort_keys=True, separators=(",", ":")).encode()


def iso(seconds):
    return datetime.datetime.fromtimestamp(seconds, datetime.timezone.utc).isoformat()


def archive(contract, extra=b"frozen"):
    files = {"execution-contract.json": encoded(contract), "evidence/input.txt": extra}
    manifest = "".join(f"{admission._sha(data)}  {name}\n" for name, data in sorted(files.items()))
    return {"manifest": manifest.encode(), "files": files}


class ReadmissionTest(unittest.TestCase):
    def setUp(self):
        actual = json.loads((Path(__file__).resolve().parents[2] / "docs/architecture/attachment-lifecycle-v5/execution-contract.json").read_text())
        leaves = actual["expected_leaf_matrix"]["W6-01/real-pg-garage-disk"]
        old_contract = {"retry_policy": {"task_attempts_total_max": 5},
                        "tasks": [{"task_id": "W6-01", "wave": "W6"}],
                        "timeouts": {"task_seconds": 9, "wave_seconds": 19, "run_seconds": 29},
                        "upload_surface_contract": {"applicability": admission.OLD_APPLICABILITY},
                        "expected_leaf_matrix": {"W6-01/x": [{"required": True}],
                                                 "W6-01/real-pg-garage-disk": leaves}}
        self.old_archive = archive(old_contract)
        old_hash = admission._sha(self.old_archive["manifest"])
        paths = {"imboy": "/isolated/imboy"}
        resources = {"pg": "owned"}
        self.old_receipt = encoded({"plan_manifest_sha256": old_hash,
                                    "receipt_id": "receipt-old",
                                    "trusted_user_message_reference": "old user message",
                                    "allowed_tasks": ["W6-01"],
                                    "implementation_allowed": True,
                                    "local_commit_allowed": True,
                                    "repository_paths": paths,
                                    "resource_allowlist": resources})
        self.review = b"independent-review"
        self.old = {"plan_dir": "/old-plan", "plan_manifest_sha256": old_hash,
                    "authorization_sha256": admission._sha(self.old_receipt),
                    "authorization_path": "/old-receipt",
                    "verification_candidate": {"heads": {"imboy": "abc"}, "repository_paths": paths,
                                               "plan_manifest_sha256": old_hash},
                    "resource_allowlist": resources,
                    "tasks": {"W6-01": {"state": "RUNNING", "attempt": 5,
                                         "task_started_at": 1, "task_deadline": 10},
                              "W6-02": {"state": "NOT_STARTED", "attempt": 0}},
                    "time_budgets": {"waves": {"W6": {"started_at": 1, "deadline": 20}},
                                     "run": {"started_at": 1, "deadline": 30}},
                    "gate_candidates": {"G5": {"plan_manifest_sha256": old_hash, "heads": {"imboy": "abc"}}},
                    "case_manifest_digests": {"historical": "unchanged"}}
        self.new_times = {"task": 110, "wave": 120, "run": 130}
        new_contract = copy.deepcopy(old_contract)
        new_contract["retry_policy"]["task_attempts_overrides"] = {"W6-01": 8}
        new_contract["upload_surface_contract"]["applicability"] = admission.NEW_APPLICABILITY
        new_contract["reviewed_readmission"] = {"task_id": "W6-01", "attempt_limit": 8,
                                                "deadlines": self.new_times}
        self.new_archive = archive(new_contract)
        self.new = copy.deepcopy(self.old)
        self.new["plan_dir"] = "/new-plan"
        self.new["plan_manifest_sha256"] = admission._sha(self.new_archive["manifest"])
        self.new["verification_candidate"]["plan_manifest_sha256"] = self.new["plan_manifest_sha256"]
        self.new["authorization_path"] = "/new-receipt"
        self.new["tasks"]["W6-01"]["task_deadline"] = 110
        self.new["time_budgets"]["waves"]["W6"]["deadline"] = 120
        self.new["time_budgets"]["run"]["deadline"] = 130
        self.receipt = {"plan_manifest_sha256": self.new["plan_manifest_sha256"],
                        "receipt_id": "receipt-new", "trusted_user_message_reference": "new user message",
                        "supersedes": {"receipt_id": "receipt-old", "sha256": admission._sha(self.old_receipt)},
                        "w6_02_decisions": {"backend_env": "local", "backend_port": 19800,
                                             "seed_accounts": "synthetic", "device_id": "XWE6R19916004085",
                                             "browser": "chromium"},
                        "allowed_tasks": ["W6-01"], "implementation_allowed": True,
                        "local_commit_allowed": True, "issued_at": iso(90), "expires_at": iso(200),
                        "repository_paths": copy.deepcopy(paths),
                        "resource_allowlist": copy.deepcopy(resources),
                        "readmission": {"old_manifest_sha256": old_hash,
                                        "new_manifest_sha256": self.new["plan_manifest_sha256"],
                                        "old_receipt_sha256": admission._sha(self.old_receipt),
                                        "task_id": "W6-01", "attempt_limit": 8,
                                        "old_deadlines": admission._deadlines(self.old),
                                        "new_deadlines": self.new_times,
                                        "preserve_attempts": True, "preserve_required": True,
                                        "review_sha256": admission._sha(self.review),
                                        "candidate_sha256": admission._digest_json(self.old["verification_candidate"]),
                                        "resources_sha256": admission._digest_json(self.old["resource_allowlist"]),
                                        "enterprise_leaf_selectors": copy.deepcopy(admission.ENTERPRISE_LEAVES)}}
        self.bind_receipt()

    def bind_receipt(self):
        self.new_receipt = encoded(self.receipt)
        self.new["authorization_sha256"] = admission._sha(self.new_receipt)
        self.new["reviewed_readmission"] = admission._sha(self.new_receipt)

    def validate(self, user=lambda _: True, review=lambda *_: True):
        return admission.validate_readmission(self.old, self.new, self.old_archive,
            self.new_archive, self.old_receipt, self.new_receipt, self.review, 100, user, review)[0]

    def test_valid_proposal_does_not_modify_inputs(self):
        before = copy.deepcopy((self.old, self.new, self.old_archive, self.new_archive))
        self.assertTrue(self.validate())
        self.assertEqual(before, (self.old, self.new, self.old_archive, self.new_archive))

    def test_verifiers_are_mandatory_and_exceptions_fail_closed(self):
        self.assertFalse(self.validate(user=None))
        self.assertFalse(self.validate(review=None))
        self.assertFalse(self.validate(review=lambda *_: False))
        def broken(*_):
            raise RuntimeError("unavailable verifier")
        self.assertFalse(self.validate(user=broken))

    def test_expired_receipt_fails(self):
        self.receipt["expires_at"] = iso(99)
        self.bind_receipt()
        self.assertFalse(self.validate())

    def test_receipt_scope_must_be_unchanged_and_bound(self):
        for field in ("repository_paths", "resource_allowlist", "allowed_tasks", "extra_privilege"):
            with self.subTest(field=field):
                original = copy.deepcopy(self.receipt)
                self.receipt[field] = {"unauthorized": True}
                self.bind_receipt()
                self.assertFalse(self.validate())
                self.receipt = original
                self.bind_receipt()
        self.assertTrue(self.validate())

    def test_iso_timezone_and_deadline_coverage(self):
        for issued, expires in ((90, iso(200)), ("1970-01-01T00:01:30", iso(200)),
                                (iso(90), iso(129)), (iso(90), iso(90 + 604801))):
            with self.subTest(issued=issued, expires=expires):
                self.receipt.update(issued_at=issued, expires_at=expires)
                self.bind_receipt()
                self.assertFalse(self.validate())
        self.receipt.update(issued_at="1970-01-01T08:01:30+08:00", expires_at=iso(130))
        self.bind_receipt()
        self.assertTrue(self.validate())

    def test_receipt_identity_and_w602_decisions(self):
        cases = (("receipt_id", "receipt-old"), ("trusted_user_message_reference", ""),
                 ("supersedes", {"receipt_id": "receipt-old", "sha256": "wrong"}),
                 ("w6_02_decisions", {**self.receipt["w6_02_decisions"], "backend_port": 9800}),
                 ("w6_02_decisions", {**self.receipt["w6_02_decisions"], "backend_port": 4323}),
                 ("w6_02_decisions", {**self.receipt["w6_02_decisions"], "seed_accounts": "real"}),
                 ("w6_02_decisions", {**self.receipt["w6_02_decisions"], "device_id": "other"}),
                 ("w6_02_decisions", {**self.receipt["w6_02_decisions"], "browser": "mock"}))
        for field, value in cases:
            with self.subTest(field=field, value=value):
                original = copy.deepcopy(self.receipt)
                self.receipt[field] = value
                self.bind_receipt()
                self.assertFalse(self.validate())
                self.receipt = original
                self.bind_receipt()
        self.assertTrue(self.validate())

    def test_eight_real_matrix_selectors_required_and_unique(self):
        actual = admission._json(self.old_archive["files"]["execution-contract.json"])
        matrix = actual["expected_leaf_matrix"]["W6-01/real-pg-garage-disk"]
        admission._validate_enterprise_leaves(actual)
        for selector in admission.ENTERPRISE_LEAVES:
            matches = [leaf for leaf in matrix if all(leaf.get(k) == v for k, v in selector.items())]
            self.assertEqual(1, len(matches))
        selector = admission.ENTERPRISE_LEAVES[0]
        for field in ("domain", "driver", "source_scenario", "ac_id", "entrypoint"):
            with self.subTest(field=field):
                changed = copy.deepcopy(actual)
                leaves = changed["expected_leaf_matrix"]["W6-01/real-pg-garage-disk"]
                target = next(leaf for leaf in leaves if all(leaf.get(k) == v for k, v in selector.items()))
                target[field] = "wrong"
                with self.assertRaises(ValueError):
                    admission._validate_enterprise_leaves(changed)
        changed = copy.deepcopy(actual)
        leaves = changed["expected_leaf_matrix"]["W6-01/real-pg-garage-disk"]
        leaves.append(copy.deepcopy(next(leaf for leaf in leaves if all(leaf.get(k) == v for k, v in selector.items()))))
        with self.assertRaises(ValueError):
            admission._validate_enterprise_leaves(changed)
        self.receipt["readmission"]["enterprise_leaf_selectors"][0]["driver"] = "none"
        self.bind_receipt()
        self.assertFalse(self.validate())

    def test_tampered_archive_or_receipt_fails(self):
        self.old_archive["files"]["evidence/input.txt"] = b"tampered"
        self.assertFalse(self.validate())
        self.old_archive["files"]["evidence/input.txt"] = b"frozen"
        self.review = b"different review"
        self.assertFalse(self.validate())
        self.review = b"independent-review"
        self.receipt["readmission"]["preserve_required"] = False
        self.bind_receipt()
        self.assertFalse(self.validate())

    def test_attempt_history_other_tasks_and_evidence_cannot_change(self):
        for mutate in (lambda s: s["tasks"]["W6-01"].update(attempt=0),
                       lambda s: s["tasks"]["W6-02"].update(attempt=1),
                       lambda s: s["case_manifest_digests"].clear(),
                       lambda s: s["gate_candidates"].clear(),
                       lambda s: s["verification_candidate"]["heads"].clear(),
                       lambda s: s["verification_candidate"].update(plan_manifest_sha256="stale"),
                       lambda s: s["verification_candidate"].update(migration_digest="changed"),
                       lambda s: s["resource_allowlist"].clear()):
            with self.subTest(mutate=mutate):
                original = copy.deepcopy(self.new)
                mutate(self.new)
                self.assertFalse(self.validate())
                self.new = original

    def test_deadline_and_repetition_fail_closed(self):
        self.new["time_budgets"]["waves"]["W6"]["deadline"] = 105
        self.assertFalse(self.validate())
        self.new["time_budgets"]["waves"]["W6"]["deadline"] = 120
        self.old["reviewed_readmission"] = "already-used"
        self.assertFalse(self.validate())

    def test_contract_changes_and_other_archive_inputs_fail_closed(self):
        original = admission._json(self.new_archive["files"]["execution-contract.json"])
        for change in ("required", "other_task_limit", "other_input", "other_surface"):
            with self.subTest(change=change):
                contract = copy.deepcopy(original)
                if change == "required":
                    contract["expected_leaf_matrix"]["W6-01/x"][0]["required"] = False
                elif change == "other_task_limit":
                    contract["retry_policy"]["task_attempts_overrides"]["W6-02"] = 8
                elif change == "other_surface":
                    contract["upload_surface_contract"]["applicability"] += " bypass"
                self.new_archive = archive(contract, b"other" if change == "other_input" else b"frozen")
                digest = admission._sha(self.new_archive["manifest"])
                self.new["plan_manifest_sha256"] = digest
                self.new["verification_candidate"]["plan_manifest_sha256"] = digest
                self.receipt["plan_manifest_sha256"] = digest
                self.receipt["readmission"]["new_manifest_sha256"] = digest
                self.bind_receipt()
                self.assertFalse(self.validate())


if __name__ == "__main__":
    unittest.main()
