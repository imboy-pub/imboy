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

    def test_group_file_entrypoints_bind_strictly(self):
        """The 12 unlocked leaves bind only to real group-file chain evidence.

        Positive: the enterprise multipart/group_file scenarios and the
        ordinary group_file scenario bind when the observation carries the
        production markers (entrypoint_face, /api/v1/group/file/upload route
        + handler, pin facts, bind request, in-request storage binding,
        enterprise workspace facts). Negative: any missing or substituted
        marker — including the forbidden attachment-multipart substitute for
        the ordinary leaf — stays a named gap and never publishes a row.
        """
        start = datetime(2026, 1, 1, tzinfo=timezone.utc)
        window = (start, start + timedelta(seconds=10))
        log = {"parsed": {"markers": ["W403_DIRECT_OK"], "tests": [1], "summary_raw": "ok"}, "path": "log", "sha256": "digest"}
        object_key = "u9/g55/20261031/att_1/w403.txt"
        upload = {"request_id": "w403-upload", "action": "upload", "method": "POST", "route": "/api/v1/group/file/upload", "handler": "group_file_handler", "status": 200, "object_key": object_key, "server_process": "<1.2.3>", "connection_id": "<1.2.4>", "diagnostic_untrusted_locations": [{"trust": "diagnostic_untrusted", "source": "attachment_storage:frozen_location/2", "identity": {"key": object_key, "driver": "disk", "namespace": "ns", "container": "container", "locator_digest": "digest"}}]}
        bind = {"request_id": "w403-bind", "action": "bind", "method": "POST", "route": "/api/v1/group/file/bind", "handler": "group_file_handler", "status": 200, "object_key": object_key, "server_process": "<1.2.5>", "connection_id": "<1.2.6>"}

        def measured(scenario, domain, mutate=lambda m: None):
            m = {"driver": "disk", "ac_id": "AC01", "route": "/api/v1/group/file/upload", "handler": "group_file_handler", "status": 200, "request_id": "w403-upload", "object_key": object_key, "pin_sha256": "a" * 64, "pin_before_put": True, "http_requests": [dict(upload)]}
            if scenario == "group_file_success":
                m["entrypoint_face"] = "group_file_bind"
                m["http_requests"].append(dict(bind))
                m["bind_route"] = "/api/v1/group/file/bind"
                m["bind_request_id"] = "w403-bind"
                m["bind_status"] = 200
                m["group_file_id"] = "file_1793452084668_254671"
            else:
                m["entrypoint_face"] = "group_file_multipart_upload"
            if domain == "enterprise":
                m["org_id"] = 42
                m["workspace_id"] = 7
            mutate(m)
            return m

        def index_for(scenario, domain, m, driver="disk", ac="AC01"):
            key = ("W2-01/pending-multipart", "pending-multipart", scenario, domain, driver)
            return {key: [(1, {"measured": m, "emitted_at_ms": int((start + timedelta(seconds=1)).timestamp() * 1000)})]}

        def leaf_for(scenario, domain, driver="disk", ac="AC01"):
            return {"source_case_ref": "W2-01/pending-multipart", "entrypoint": "pending-multipart", "driver": driver, "domain": domain, "ordering_or_fault": "W2-01/pending-multipart/expand-queue/" + scenario, "ac_id": ac}

        state = {"index": {}}

        def step(name):
            return {"started": window[0], "finished": window[1], "index": state["index"], "path": name, "record": {"line": name}, "log": log, "markers": set()}

        with ExitStack() as stack:
            gaps = stack.enter_context(patch.object(adapter.S, "group_gap"))
            gaps.side_effect = lambda gap_groups, *args: gap_groups.setdefault("gap", []).append(args)
            report = stack.enter_context(patch.object(adapter.S, "report_gaps", side_effect=lambda *args: (_ for _ in ()).throw(SystemExit(30))))
            build_row = stack.enter_context(patch.object(adapter.S.C, "build_row", side_effect=lambda leaf, a, b, *args: {"source": leaf["source_case_ref"], "domain": leaf["domain"]}))
            for item in (
                patch.object(adapter.S.C, "argv_paths", return_value=("run", "case", "root")),
                patch.object(adapter.S, "context", return_value={"allowlist": {}, "contract": {}}),
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
                patch.object(adapter, "_step_binding", side_effect=lambda _, name, cache: step(name)),
            ):
                stack.enter_context(item)
            leaf_mock = stack.enter_context(patch.object(adapter.S, "leaves"))

            # Positive: real-chain observations bind for all three shapes.
            for scenario, domain in (
                ("multipart_success", "enterprise"),
                ("group_file_success", "enterprise"),
                ("group_file_success", "ordinary"),
            ):
                state["index"] = index_for(scenario, domain, measured(scenario, domain))
                leaf_mock.return_value = [leaf_for(scenario, domain)]
                rows = adapter.build()
                self.assertFalse(gaps.called)
                self.assertTrue(build_row.called)
                self.assertEqual(1, len(rows[1]))
                self.assertEqual("W2-01/pending-multipart", rows[1][0]["source"])
                self.assertEqual(domain, rows[1][0]["domain"])
                # The row carries the real entrypoint provenance, not a proxy.
                self.assertEqual(
                    "/api/v1/group/file/upload", rows[1][0]["entrypoint_provenance"]["route"])
                self.assertEqual(
                    "group_file_handler", rows[1][0]["entrypoint_provenance"]["handler"])
                gaps.reset_mock()
                build_row.reset_mock()

            # Negative: any substituted or missing marker stays a named gap.
            def wrong_face(m):
                m["entrypoint_face"] = "group_file_bind"

            def multipart_face_on_bind_leaf(m):
                m["entrypoint_face"] = "group_file_multipart_upload"

            def wrong_object_key(m):
                m["object_key"] = ""

            def substitute_route(m):
                m["route"] = "/api/v1/attachment/upload"

            def substitute_handler(m):
                m["handler"] = "attach_handler"

            def not_ok(m):
                m["status"] = 500

            def no_pin_hash(m):
                m["pin_sha256"] = "not-hex"

            def no_pin_order(m):
                m["pin_before_put"] = False

            def no_storage(m):
                del m["http_requests"][0]["diagnostic_untrusted_locations"]

            def no_bind(m):
                m["bind_request_id"] = ""

            def substitute_bind(m):
                m["http_requests"][1]["route"] = "/api/v1/attachment/confirm"

            def no_workspace(m):
                del m["org_id"]

            def bind_on_multipart_face(m):
                m["http_requests"].append(dict(bind))

            cases = [
                ("multipart_success", "enterprise", wrong_face),
                ("multipart_success", "enterprise", wrong_object_key),
                ("multipart_success", "enterprise", no_workspace),
                ("multipart_success", "enterprise", bind_on_multipart_face),
                ("group_file_success", "ordinary", multipart_face_on_bind_leaf),
                ("group_file_success", "ordinary", substitute_route),
                ("group_file_success", "ordinary", substitute_handler),
                ("group_file_success", "ordinary", not_ok),
                ("group_file_success", "ordinary", no_pin_hash),
                ("group_file_success", "ordinary", no_pin_order),
                ("group_file_success", "ordinary", no_storage),
                ("group_file_success", "ordinary", no_bind),
                ("group_file_success", "ordinary", substitute_bind),
                ("group_file_success", "enterprise", no_workspace),
            ]
            for scenario, domain, mutate in cases:
                with self.subTest(scenario=scenario, domain=domain, mutate=mutate.__name__):
                    state["index"] = index_for(scenario, domain, measured(scenario, domain, mutate))
                    leaf_mock.return_value = [leaf_for(scenario, domain)]
                    with self.assertRaises(SystemExit) as caught:
                        adapter.build()
                    self.assertEqual(30, caught.exception.code)
                    self.assertTrue(gaps.called)
                    self.assertIn("/api/v1/group/file/upload", gaps.call_args[0][3])
                    self.assertFalse(build_row.called)
                gaps.reset_mock()
                build_row.reset_mock()

            # An ordinary group-scope attachment is not a group-file upload.
            # No driver or AC can publish a row from that substitute evidence
            # (the observation carries no group-file chain markers at all).
            for driver, ac in itertools.product(("disk", "garage"), ("AC01", "AC02")):
                state["index"] = index_for("group_file_success", "ordinary", {"driver": driver, "ac_id": ac, "http_requests": []}, driver=driver, ac=ac)
                leaf_mock.return_value = [leaf_for("group_file_success", "ordinary", driver=driver, ac=ac)]
                with self.subTest(domain="ordinary", driver=driver, ac=ac):
                    with self.assertRaises(SystemExit) as caught:
                        adapter.build()
                    self.assertEqual(30, caught.exception.code)
                    self.assertIn("/api/v1/group/file/upload", gaps.call_args[0][3])
                    self.assertFalse(build_row.called)
                gaps.reset_mock()
                build_row.reset_mock()

            # …while the ordinary-domain multipart_success twin of the same
            # case still binds through the pre-existing evidence path.
            state["index"] = index_for("multipart_success", "ordinary", {"driver": "disk", "ac_id": "AC01"})
            leaf_mock.return_value = [leaf_for("multipart_success", "ordinary")]
            rows = adapter.build()
            self.assertFalse(gaps.called)
            self.assertEqual([{"source": "W2-01/pending-multipart", "domain": "ordinary"}], rows[1])

    def test_step_windows_are_independent_of_leaf_order(self):
        start = datetime(2026, 1, 1, tzinfo=timezone.utc)
        windows = {name: (start + timedelta(seconds=n), start + timedelta(seconds=n + 10))
                   for name, n in (("w403", 0), ("w2upload", 20), ("w4queue", 40))}
        matrix_leaf = {"source_case_ref": "W4-03/driver-content-immutable", "entrypoint": "ordinary", "driver": "disk", "domain": "ordinary", "ordering_or_fault": "scenario", "source_scenario": "scenario", "ac_id": "AC02"}
        obs_leaves = [
            {"source_case_ref": ref, "entrypoint": "entry", "driver": "disk", "domain": "ordinary", "ordering_or_fault": ref + "/expand-queue/scenario", "ac_id": "AC01"}
            for ref in ("W2-01/session-recovery-api", "W4-01/delete-entrypoints")]
        ctx = {"allowlist": {}, "contract": {}}
        log = {"parsed": {"markers": ["W403_DIRECT_OK"], "tests": [1], "summary_raw": "ok"}, "path": "log", "sha256": "digest"}
        key = (matrix_leaf["source_case_ref"], "ordinary", "scenario", "ordinary", "disk")
        common = {"object_key": "owned/key", "server_process": "<1.2.3>",
                  "connection_id": "<1.2.4>", "status": 200}
        presign = {**common, "request_id": "presign", "action": "presign",
                   "method": "GET", "route": "/api/v1/attachment/presign",
                   "handler": "attach_handler"}
        put = {**common, "request_id": "put", "action": "put", "method": "PUT",
               "route": "/api/v1/attachment/gateway/12345678-1234-1234-1234-123456789abc",
               "handler": "attachment_storage_gateway_handler",
               "diagnostic_untrusted_locations": [{
                   "trust": "diagnostic_untrusted", "source": "attachment_storage:session_location/1",
                   "identity": {"key": "owned/key", "driver": "disk", "namespace": "ns",
                                "container": "container", "locator_digest": "digest"}}]}
        matrix_obs = {"measured": {"driver": "disk", "ac_id": "AC02", "object_key": "owned/key",
                                   "http_requests": [presign, put]},
                      "emitted_at_ms": int((start + timedelta(seconds=1)).timestamp() * 1000)}

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
            patch.object(adapter, "_obs_index", return_value=({key: [(1, matrix_obs)]}, "obs403")),
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
