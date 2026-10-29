"""W6-01 observations must bind to the correct AC and step window."""
import itertools
import sys
import unittest
from contextlib import ExitStack
from datetime import datetime, timedelta, timezone
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parent))
import attachment_lifecycle_v5_adapter_w6_01_real_pg_garage_disk as adapter


class ObservationBindingTests(unittest.TestCase):
    def setUp(self):
        self.key = ("W2-01/pending-multipart", "pending-multipart", "multipart_success", "enterprise", "disk")

    def test_ac_identity_and_duplicate_rejection(self):
        ac1 = {"measured": {"ac_id": "AC01", "tag": 1}}
        ac2 = {"measured": {"ac_id": "AC02", "tag": 2}}
        for entries in ([(1, ac1), (2, ac2)], [(2, ac2), (1, ac1)]):
            index = {self.key: entries}
            self.assertEqual(1, adapter._match_observation(index, self.key, "AC01")["measured"]["tag"])
            self.assertEqual(2, adapter._match_observation(index, self.key, "AC02")["measured"]["tag"])
        self.assertIsNone(adapter._match_observation({self.key: [(1, ac1)]}, self.key, "AC02"))
        for value in ({}, {"ac_id": None}, {"ac_id": ""}):
            self.assertIsNone(adapter._match_observation({self.key: [(1, {"measured": value})]}, self.key, "AC01"))
        with self.assertRaises(SystemExit):
            adapter._match_observation({self.key: [(1, ac1), (2, ac1)]}, self.key, "AC01")

    def test_legacy_missing_ac_is_explicitly_limited(self):
        for source in adapter.LEGACY_WITHOUT_AC:
            with self.subTest(source=source):
                key = (source,) + self.key[1:]
                no_ac = {"measured": {"driver": "disk"}}
                self.assertIs(no_ac, adapter._match_observation({key: [(1, no_ac)]}, key, "AC01"))
                self.assertIsNone(adapter._match_observation({key: [(1, {"measured": {"ac_id": "AC02"}})]}, key, "AC01"))
                with self.assertRaises(SystemExit):
                    adapter._match_observation({key: [(1, no_ac), (2, no_ac)]}, key, "AC01")
                with self.assertRaises(SystemExit):
                    adapter._match_observation({key: [(1, no_ac), (2, {"measured": {"ac_id": "AC01"}})]}, key, "AC01")
        self.assertIsNone(adapter._match_observation({self.key: [(1, {"measured": {}})]}, self.key, "AC01"))

    def test_enterprise_success_leaf_binds_via_seat_observation(self):
        # 2026-10-29 ruling: the enterprise pending-multipart success leaves
        # are measured on the enterprise_seat real chain by the same w2upload
        # battery; the adapter binds them through the identical observation
        # path as any other leaf, and a missing observation stays a named gap
        # with exit 30 (never a PASS without a receipt).
        enterprise = {"source_case_ref": "W2-01/pending-multipart", "entrypoint": "pending-multipart", "driver": "disk", "domain": "enterprise", "ordering_or_fault": "W2-01/pending-multipart/expand-queue/multipart_success", "ac_id": "AC01"}
        start = datetime(2026, 1, 1, tzinfo=timezone.utc)
        window = (start, start + timedelta(seconds=10))
        log = {"parsed": {"markers": ["W403_DIRECT_OK"], "tests": [1], "summary_raw": "ok"}, "path": "log", "sha256": "digest"}

        def step(name, domain="enterprise"):
            ref = "W2-01/pending-multipart"
            key = (ref, "pending-multipart", "multipart_success", domain, "disk")
            obs = {"measured": {"driver": "disk", "ac_id": "AC01"}, "emitted_at_ms": int((start + timedelta(seconds=1)).timestamp() * 1000)}
            return {"started": window[0], "finished": window[1], "index": {key: [(1, obs)]}, "path": name, "record": {"line": name}, "log": log, "markers": set()}

        def run_build(stack, leaf):
            gaps = stack.enter_context(patch.object(adapter.S, "group_gap"))
            gaps.side_effect = lambda gap_groups, *args: gap_groups.setdefault("gap", []).append(args)
            report = stack.enter_context(patch.object(adapter.S, "report_gaps", side_effect=lambda *args: (_ for _ in ()).throw(SystemExit(30))))
            build_row = stack.enter_context(patch.object(adapter.S.C, "build_row", side_effect=lambda leaf_, a, b, *args: {"source": leaf_["source_case_ref"], "domain": leaf_["domain"]}))
            for item in (
                patch.object(adapter.S.C, "argv_paths", return_value=("run", "case", "root")),
                patch.object(adapter.S, "context", return_value={"allowlist": {}}),
                patch.object(adapter.S.C, "resolve_slot", return_value="slot1"),
                patch.object(adapter.S.C, "db_resource", return_value="db"),
                patch.object(adapter.S.C, "garage_bucket", return_value="bucket"),
                patch.object(adapter.W4, "resolve_disk_root", return_value="diskroot"),
                patch.object(adapter, "_body_sha256", return_value=("sha", 1, "file")),
                patch.object(adapter.S, "step_record", side_effect=lambda _, name, __: {"exit": 0, "line": name}),
                patch.object(adapter.S, "eunit_log", return_value=log),
                patch.object(adapter.S, "step_window", side_effect=lambda rec, _: window),
                patch.object(adapter, "_obs_index", return_value=({}, "obs403")),
                patch.object(adapter.S, "probe_file", return_value={}),
                patch.object(adapter, "_replay_count", return_value=1),
                patch.object(adapter.S.C, "assert_leaf_set"),
                patch.object(adapter.S, "sha256_file", return_value="sha"),
            ):
                stack.enter_context(item)
            leaf_mock = stack.enter_context(patch.object(adapter.S, "leaves"))
            leaf_mock.return_value = [leaf]
            binding = stack.enter_context(patch.object(adapter, "_step_binding", side_effect=lambda _, name, cache: step(name)))
            return gaps, report, build_row, binding

        # The enterprise leaf binds normally through the w2upload observation:
        # no gap, exactly one build_row receipt for the enterprise domain.
        with ExitStack() as stack:
            gaps, report, build_row, _ = run_build(stack, enterprise)
            rows = adapter.build()[1]
            self.assertFalse(gaps.called)
            self.assertFalse(report.called)
            self.assertEqual([{"source": enterprise["source_case_ref"], "domain": "enterprise"}], rows)
            self.assertTrue(build_row.called)

        # Without a matching seat observation the leaf stays a named gap and
        # the case fails with exit 30; no receipt row is manufactured.
        with ExitStack() as stack:
            gaps, report, build_row, binding = run_build(stack, enterprise)
            binding.side_effect = lambda _, name, cache: step(name, domain="ordinary")
            with self.assertRaises(SystemExit) as caught:
                adapter.build()
            self.assertEqual(30, caught.exception.code)
            self.assertTrue(gaps.called)
            self.assertIn("no w2upload-step observation matches", gaps.call_args[0][3])
            self.assertTrue(report.called)
            self.assertFalse(build_row.called)

    def test_step_windows_are_independent_of_leaf_order(self):
        start = datetime(2026, 1, 1, tzinfo=timezone.utc)
        windows = {name: (start + timedelta(seconds=n), start + timedelta(seconds=n + 10))
                   for name, n in (("w403", 0), ("w2upload", 20), ("w4queue", 40))}
        matrix_leaf = {"source_case_ref": "W4-03/driver-content-immutable", "entrypoint": "surface", "driver": "disk", "domain": "ordinary", "ordering_or_fault": "scenario", "ac_id": "AC02"}
        obs_leaves = [
            {"source_case_ref": ref, "entrypoint": "entry", "driver": "disk", "domain": "ordinary", "ordering_or_fault": ref + "/expand-queue/scenario", "ac_id": "AC01"}
            for ref in ("W2-01/session-recovery-api", "W4-01/delete-entrypoints")]
        ctx = {"allowlist": {}}
        log = {"parsed": {"markers": ["W403_DIRECT_OK"], "tests": [1], "summary_raw": "ok"}, "path": "log", "sha256": "digest"}

        def step(name):
            first, last = windows[name]
            ref = {"w2upload": "W2-01/session-recovery-api", "w4queue": "W4-01/delete-entrypoints"}.get(name, "W4-03/storage-garage")
            key = (ref, "entry", "scenario", "ordinary", "disk")
            obs = {"measured": {"driver": "disk", "ac_id": "AC01"}, "emitted_at_ms": int((first + timedelta(seconds=1)).timestamp() * 1000)}
            return {"started": first, "finished": last, "index": {key: [(1, obs)]}, "path": name, "record": {"line": name}, "log": log, "markers": set()}

        patches = [
            patch.object(adapter.S.C, "argv_paths", return_value=("run", "case", "root")),
            patch.object(adapter.S, "context", return_value=ctx),
            patch.object(adapter.S.C, "resolve_slot", return_value="slot1"),
            patch.object(adapter.S.C, "db_resource", return_value="db"),
            patch.object(adapter.S.C, "garage_bucket", return_value="bucket"),
            patch.object(adapter.W4, "resolve_disk_root", return_value="diskroot"),
            patch.object(adapter, "_body_sha256", return_value=("sha", 1, "file")),
            patch.object(adapter.S, "step_record", side_effect=lambda _, name, __: {"exit": 0, "line": name}),
            patch.object(adapter.S, "eunit_log", return_value=log),
            patch.object(adapter.S, "step_window", side_effect=lambda rec, _: windows.get(rec["line"], windows["w403"])),
            patch.object(adapter, "_obs_index", return_value=({}, "obs403")),
            patch.object(adapter.S, "probe_file", return_value={}),
            patch.object(adapter, "_replay_count", return_value=1),
            patch.object(adapter.S, "find_eunit_unit", return_value={"func": "f", "duration": 0.1}),
            patch.object(adapter.S, "http_evidence", return_value={}),
            patch.object(adapter.S, "validate_requests", return_value=(["id"], [200], [{"method": "GET", "route": "/", "handler": "h", "status": 200, "request_id": "id"}])),
            patch.object(adapter, "_scenario_line", return_value=(1, "file")),
            patch.object(adapter.S, "disk_actor", return_value={}),
            patch.object(adapter.S, "assertion", side_effect=lambda *args: args),
            patch.object(adapter.S, "sha256_file", return_value="sha"),
            patch.object(adapter.S.C, "assert_leaf_set"),
            patch.object(adapter.S.C, "build_row", side_effect=lambda leaf, a, b, *args: {"source": leaf["source_case_ref"], "window": (a, b)}),
            patch.object(adapter, "_step_binding", side_effect=lambda _, name, cache: step(name)),
        ]
        with ExitStack() as stack:
            for item in patches:
                stack.enter_context(item)
            leaf_mock = stack.enter_context(patch.object(adapter.S, "leaves"))
            for ordering in itertools.permutations([matrix_leaf] + obs_leaves):
                leaf_mock.return_value = list(ordering)
                _, rows = adapter.build()
                actual = {row["source"]: row["window"] for row in rows}
                self.assertEqual(windows["w403"], actual[matrix_leaf["source_case_ref"]])
                self.assertEqual(windows["w2upload"], actual[obs_leaves[0]["source_case_ref"]])
                self.assertEqual(windows["w4queue"], actual[obs_leaves[1]["source_case_ref"]])


if __name__ == "__main__":
    unittest.main()
