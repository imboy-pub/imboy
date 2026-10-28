import contextlib
import datetime
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import time
from types import SimpleNamespace
import unittest
from unittest.mock import patch

from scripts import attachment_lifecycle_v5_runner as runner


class ExistingAdmissionTest(unittest.TestCase):
    def test_expired_run_resume_and_next_never_offer_unstarted_wave(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            plan = root / 'plan'
            plan.mkdir()
            contract = {'tasks': [{'task_id': 'W0-01', 'wave': 'W0', 'dependencies': []},
                                  {'task_id': 'W1-01', 'wave': 'W1', 'dependencies': ['W0-01']}],
                        'wave_gates': {'G1': {'wave': 'W1', 'previous_gate': 'G0'}},
                        'timeouts': {'run_seconds': 10, 'wave_seconds': 30}}
            (plan / 'execution-contract.json').write_text(json.dumps(contract))
            now = datetime.datetime.now(datetime.timezone.utc)
            auth = root / 'auth.json'
            auth.write_text(json.dumps({'plan_manifest_sha256': 'digest', 'implementation_allowed': True,
                'local_commit_allowed': True, 'allowed_tasks': ['W0-01', 'W1-01'],
                'issued_at': (now - datetime.timedelta(minutes=1)).isoformat(),
                'expires_at': (now + datetime.timedelta(days=1)).isoformat()}))
            start = time.time() - 20
            store = runner.StateStore(tmp)
            candidate = {'plan_manifest_sha256': 'digest'}
            state = store.write_snapshot({'run_id': root.name, 'plan_dir': str(plan), 'revision': 0,
                'plan_manifest_sha256': 'digest', 'authorization_path': str(auth),
                'authorization_sha256': runner.sha256_file(auth), 'gates': {'G0': 'PASS'},
                'verification_candidate': candidate, 'gate_candidates': {'G0': candidate},
                'tasks': {'W0-01': {'state': 'PASS', 'attempt': 1},
                          'W1-01': {'state': 'NOT_STARTED', 'attempt': 0}},
                'time_budgets': {'run': {'started_at': start, 'deadline': start + 10},
                                 'waves': {'W0': {'started_at': start, 'deadline': start + 30}}}}, 'admit')
            before = Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()
            with patch.object(runner, 'verify_plan_manifest', return_value=(True, 'digest')), \
                 patch.object(runner, 'candidate_errors', return_value=[]):
                out = io.StringIO()
                with contextlib.redirect_stdout(out):
                    self.assertEqual(runner.EXIT_OK, runner.cmd_resume(SimpleNamespace(run_dir=tmp)))
                self.assertEqual([], json.loads(out.getvalue())['ready'])
                with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_next(SimpleNamespace(run_dir=tmp))
                self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
            self.assertEqual(before, (Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()))
            self.assertEqual('NOT_STARTED', state['tasks']['W1-01']['state'])

    def test_expired_run_recovery_records_no_go_and_preserves_commit_unknown(self):
        for status, target in [('RUNNING', 'NO_GO'), ('COMMITTING', 'NO_GO'),
                               ('COMMITTING', 'BLOCKED_EXTERNAL')]:
            with self.subTest(status=status, target=target), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                plan = root / 'plan'
                plan.mkdir()
                contract = {'tasks': [{'task_id': 'W0-01', 'wave': 'W0', 'dependencies': []}],
                            'wave_gates': {}, 'timeouts': {'run_seconds': 10, 'wave_seconds': 5}}
                (plan / 'execution-contract.json').write_text(json.dumps(contract))
                now = datetime.datetime.now(datetime.timezone.utc)
                auth = root / 'auth.json'
                auth.write_text(json.dumps({'plan_manifest_sha256': 'digest', 'implementation_allowed': True,
                    'local_commit_allowed': True, 'allowed_tasks': ['W0-01'],
                    'issued_at': (now - datetime.timedelta(minutes=1)).isoformat(),
                    'expires_at': (now + datetime.timedelta(days=1)).isoformat()}))
                start = time.time() - 20
                store = runner.StateStore(tmp)
                state = store.write_snapshot({'run_id': root.name, 'plan_dir': str(plan), 'revision': 0,
                    'plan_manifest_sha256': 'digest', 'authorization_path': str(auth),
                    'authorization_sha256': runner.sha256_file(auth), 'gates': {},
                    'tasks': {'W0-01': {'state': status, 'attempt': 1}},
                    'time_budgets': {'run': {'started_at': start, 'deadline': start + 10},
                                     'waves': {'W0': {'started_at': start, 'deadline': start + 5}}}}, 'admit')
                args = SimpleNamespace(run_dir=tmp, task_id='W0-01', to=target,
                                       case_results=None, commit_vector=None, blocked_reason='expired')
                with patch.object(runner, 'verify_plan_manifest', return_value=(True, 'digest')):
                    self.assertEqual(state, runner._resume_state(tmp))
                    if target == 'NO_GO' and status == 'COMMITTING':
                        with self.assertRaises(SystemExit) as exc:
                            runner.cmd_task(args)
                        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                        continue
                    with patch.object(runner.StateStore, 'persist_snapshot', side_effect=OSError('crash')):
                        with self.assertRaises(OSError):
                            runner.cmd_task(args)
                    recovered = runner._resume_state(tmp)
                    self.assertEqual(target, recovered['tasks']['W0-01']['state'])
                    self.assertEqual(1, recovered['tasks']['W0-01']['attempt'])
                    self.assertEqual(state['time_budgets'], recovered['time_budgets'])
                    if status == 'COMMITTING':
                        self.assertEqual('COMMITTING', recovered['tasks']['W0-01']['resume_state'])

    def test_ready_excludes_expired_wave_and_changed_boot_blocks(self):
        contract = {'tasks': [{'task_id': 'W0-01', 'wave': 'W0', 'dependencies': []}]}
        state = {'tasks': {'W0-01': {'state': 'NOT_STARTED'}},
                 'time_budgets': {'run': None, 'waves': {'W0': {'deadline': time.time() - 1}}}}
        self.assertEqual([], runner._compute_ready(state, contract))
        anchor = {'boot': 'previous boot', 'wall': time.time() - 1,
                  'monotonic': time.clock_gettime(time.CLOCK_MONOTONIC) - 1}
        with self.assertRaises(SystemExit) as exc:
            runner._clock_check(anchor)
        self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)

    def test_run_wave_deadlines_persist_across_restart_and_expire(self):
        contract = {'tasks': [{'task_id': 'W0-01', 'wave': 'W0'}, {'task_id': 'W6-01', 'wave': 'W6'}],
                    'timeouts': {'run_seconds': 100, 'wave_seconds': 20, 'wave_overrides': {'W6': 50}}}
        with tempfile.TemporaryDirectory() as tmp:
            store = runner.StateStore(tmp)
            state = store.write_snapshot({'tasks': {}, 'time_budgets': {'run': None, 'waves': {}}}, 'admit')
            now = time.time()
            state['time_budgets'] = runner._budget_start(state, contract, 'W0-01', now)
            state = store.write_snapshot(state)
            reloaded = store.load_state()
            self.assertEqual(state['time_budgets'], reloaded['time_budgets'])
            runner._budget_check(reloaded, contract, 'W0-01')
            with patch.object(runner.time, 'time', return_value=now + 21), self.assertRaises(SystemExit) as exc:
                runner._budget_check(reloaded, contract, 'W0-01')
            self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
            with patch.object(runner.time, 'time', return_value=now + 101), self.assertRaises(SystemExit) as exc:
                runner._budget_check(reloaded, contract)
            self.assertEqual(runner.EXIT_NOGO, exc.exception.code)
            after = runner._budget_start(reloaded, contract, 'W6-01', now + 1)
            self.assertEqual(reloaded['time_budgets']['run'], after['run'])
            self.assertEqual(now + 51, after['waves']['W6']['deadline'])

    def test_clock_rollback_and_legacy_budget_fail_closed(self):
        anchor = {'boot': runner._boot_id(), 'wall': time.time(),
                  'monotonic': time.clock_gettime(time.CLOCK_MONOTONIC)}
        with patch.object(runner.time, 'time', return_value=anchor['wall'] - 2), self.assertRaises(SystemExit) as exc:
            runner._clock_check(anchor)
        self.assertEqual(runner.EXIT_BLOCKED, exc.exception.code)
        with self.assertRaises(SystemExit) as exc:
            runner._budget_check({}, {'tasks': []})
        self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)

    def test_w0_cannot_start_with_stale_frozen_candidate(self):
        contract = {"tasks": [{"task_id": "W0-01", "wave": "W0", "dependencies": []}]}
        state = {"plan_manifest_sha256": "current", "tasks": {},
                 "verification_candidate": {"plan_manifest_sha256": "old"}}
        with patch.object(runner, "candidate_errors", return_value=[]):
            self.assertFalse(runner._task_prerequisites(state, contract, "W0-01"))

    def test_task_start_and_replay_enforce_prerequisites_and_blocked_reconciliation(self):
        for variant in ("candidate_missing", "candidate_plan_stale", "dependency_missing", "gate_missing", "gate_candidate_stale", "blocked_external", "blocked_commit_resume", "unstarted_commit"):
            with self.subTest(variant=variant), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                plan = root / "plan"
                plan.mkdir()
                contract = {"tasks": [{"task_id": "W0-01", "wave": "W0", "dependencies": []},
                                      {"task_id": "W3-01", "wave": "W3", "dependencies": ["W0-01"]}],
                            "wave_gates": {"G3": {"wave": "W3", "previous_gate": "G2"}},
                            "timeouts": {"task_seconds": 100}}
                (plan / "execution-contract.json").write_text(json.dumps(contract))
                now_dt = datetime.datetime.now(datetime.timezone.utc)
                auth = root / "auth.json"
                auth.write_text(json.dumps({"plan_manifest_sha256": "digest", "implementation_allowed": True,
                    "local_commit_allowed": True, "allowed_tasks": ["W0-01", "W3-01"],
                    "issued_at": (now_dt - datetime.timedelta(minutes=1)).isoformat(),
                    "expires_at": (now_dt + datetime.timedelta(days=1)).isoformat()}))
                candidate = {"synthetic": "candidate", "plan_manifest_sha256": "stale" if variant == "candidate_plan_stale" else "digest"}
                old_task = {"state": "READY", "attempt": 0}
                target = "RUNNING"
                if variant == "blocked_external":
                    old_task = {"state": "BLOCKED_EXTERNAL", "attempt": 1, "resume_state": "RUNNING"}
                    target = "READY"
                elif variant == "blocked_commit_resume":
                    old_task = {"state": "BLOCKED_EXTERNAL", "attempt": 0, "resume_state": "NOT_STARTED"}
                    target = "COMMITTING"
                elif variant == "unstarted_commit":
                    old_task = {"state": "NOT_STARTED", "attempt": 0}
                    target = "COMMITTING"
                state = {"run_id": root.name, "revision": 0, "plan_dir": str(plan),
                         "plan_manifest_sha256": "digest", "authorization_path": str(auth),
                         "authorization_sha256": runner.sha256_file(auth),
                         "tasks": {"W0-01": {"state": "NOT_STARTED" if variant == "dependency_missing" else "PASS"},
                                   "W3-01": old_task},
                         "gates": {"G2": "NOT_STARTED" if variant == "gate_missing" else "PASS"},
                         "gate_candidates": {"G2": {"synthetic": "stale"} if variant == "gate_candidate_stale" else candidate}}
                if variant != "candidate_missing":
                    state["verification_candidate"] = candidate
                store = runner.StateStore(tmp)
                state = store.write_snapshot(state, "admit", {"synthetic": True})
                args = SimpleNamespace(run_dir=tmp, task_id="W3-01", to=target,
                                       case_results=None, commit_vector=None, blocked_reason=None)
                with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                     patch.object(runner, "candidate_errors", return_value=[]), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_task(args)
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                now = time.time()
                new_task = dict(old_task, state=target)
                if target == "RUNNING":
                    new_task.update(attempt=1, task_started_at=now, task_deadline=now + 100)
                with patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
                    with self.assertRaises(OSError):
                        store.write_snapshot(dict(state, tasks=dict(state["tasks"], **{"W3-01": new_task})),
                            "task_state", {"task": "W3-01", "to": target, "attempt": new_task["attempt"]})
                before_state, before_events = Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()
                with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                     patch.object(runner, "candidate_errors", return_value=[]), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_resume(SimpleNamespace(run_dir=tmp))
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                self.assertEqual(before_state, Path(store.state_path).read_bytes())
                self.assertEqual(before_events, Path(store.events_path).read_bytes())

    def test_replay_refuses_exhausted_forged_or_expired_budget_and_commit_unknown(self):
        for variant in ("overbudget", "long_deadline", "expired", "commit_unknown", "blocked_commit_unknown"):
            with self.subTest(variant=variant), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                plan = root / "plan"
                plan.mkdir()
                contract = {"tasks": [{"task_id": "W0-01", "wave": "W0", "dependencies": []}],
                            "wave_gates": {}, "timeouts": {"task_seconds": 100},
                            "retry_policy": {"task_attempts_total_max": 5}}
                (plan / "execution-contract.json").write_text(json.dumps(contract))
                now_dt = datetime.datetime.now(datetime.timezone.utc)
                auth = root / "auth.json"
                auth.write_text(json.dumps({"plan_manifest_sha256": "digest", "implementation_allowed": True,
                    "local_commit_allowed": True, "allowed_tasks": ["W0-01"],
                    "issued_at": (now_dt - datetime.timedelta(minutes=1)).isoformat(),
                    "expires_at": (now_dt + datetime.timedelta(days=1)).isoformat()}))
                now = time.time()
                old_task = {"state": "READY", "attempt": 5 if variant == "overbudget" else 0}
                new_task = {"state": "RUNNING", "attempt": old_task["attempt"] + 1,
                            "task_started_at": now - (200 if variant == "expired" else 1),
                            "task_deadline": now - 100 if variant == "expired" else now + (1000 if variant == "long_deadline" else 99)}
                if variant in ("commit_unknown", "blocked_commit_unknown"):
                    old_task = {"state": "COMMITTING" if variant == "commit_unknown" else "BLOCKED_EXTERNAL",
                                "attempt": 1, "resume_state": "COMMITTING"}
                    new_task = dict(old_task, state="RETRYABLE" if variant == "commit_unknown" else "READY")
                store = runner.StateStore(tmp)
                old = store.write_snapshot({"run_id": root.name, "revision": 0, "plan_dir": str(plan),
                    "plan_manifest_sha256": "digest", "authorization_path": str(auth),
                    "authorization_sha256": runner.sha256_file(auth), "tasks": {"W0-01": old_task}, "gates": {}},
                    "admit", {"synthetic": True})
                if variant in ("commit_unknown", "blocked_commit_unknown"):
                    args = SimpleNamespace(run_dir=tmp, task_id="W0-01", to=new_task["state"],
                                           case_results=None, commit_vector=None, blocked_reason=None)
                    with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                         contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                        runner.cmd_task(args)
                    self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                forged = dict(old, tasks={"W0-01": new_task})
                if new_task["state"] == "RUNNING":
                    forged["time_budgets"] = runner._budget_start(old, contract, "W0-01", new_task["task_started_at"])
                with patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
                    with self.assertRaises(OSError):
                        store.write_snapshot(forged, "task_state", {"task": "W0-01", "to": new_task["state"],
                                                                     "attempt": new_task["attempt"]})
                before_state, before_events = Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()
                with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_resume(SimpleNamespace(run_dir=tmp))
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                self.assertEqual(before_state, Path(store.state_path).read_bytes())
                self.assertEqual(before_events, Path(store.events_path).read_bytes())

    def test_task_cannot_start_after_authorization_expiry_revocation_or_replacement(self):
        for variant in ("expired", "revoked", "changed", "plan_changed"):
            with self.subTest(variant=variant), tempfile.TemporaryDirectory() as tmp:
                root = Path(tmp)
                plan = root / "plan"
                plan.mkdir()
                contract = {"tasks": [{"task_id": "W0-01"}], "wave_gates": {}}
                (plan / "execution-contract.json").write_text(json.dumps(contract))
                now = datetime.datetime.now(datetime.timezone.utc)
                auth_data = {"plan_manifest_sha256": "digest", "implementation_allowed": variant != "revoked",
                             "local_commit_allowed": True, "allowed_tasks": ["W0-01"],
                             "issued_at": (now - datetime.timedelta(days=1)).isoformat(),
                             "expires_at": (now - datetime.timedelta(minutes=1) if variant == "expired"
                                            else now + datetime.timedelta(days=1)).isoformat()}
                auth = root / "auth.json"
                auth.write_text(json.dumps(auth_data))
                store = runner.StateStore(tmp)
                store.write_snapshot({"run_id": root.name, "revision": 0, "plan_dir": str(plan),
                    "plan_manifest_sha256": "digest", "authorization_path": str(auth),
                    "authorization_sha256": runner.sha256_file(auth),
                    "tasks": {"W0-01": {"state": "READY", "attempt": 0}}, "gates": {}},
                    "admit", {"synthetic": True})
                if variant == "changed":
                    auth.write_text(auth.read_text() + " ")
                state_before, events_before = Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()
                args = SimpleNamespace(run_dir=tmp, task_id="W0-01", to="RUNNING",
                                       case_results=None, commit_vector=None, blocked_reason=None)
                digest = "different" if variant == "plan_changed" else "digest"
                with patch.object(runner, "verify_plan_manifest", return_value=(True, digest)), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_task(args)
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                self.assertEqual(state_before, Path(store.state_path).read_bytes())
                self.assertEqual(events_before, Path(store.events_path).read_bytes())

    def test_resume_replays_durable_running_event_without_resetting_budget(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            plan = root / "plan"
            plan.mkdir()
            contract = {"tasks": [{"task_id": "W0-01", "wave": "W0", "dependencies": []}],
                        "wave_gates": {}, "timeouts": {"task_seconds": 100}}
            (plan / "execution-contract.json").write_text(json.dumps(contract))
            now = datetime.datetime.now(datetime.timezone.utc)
            auth = root / "auth.json"
            auth.write_text(json.dumps({"plan_manifest_sha256": "digest", "implementation_allowed": True,
                                        "local_commit_allowed": True, "allowed_tasks": ["W0-01"],
                                        "issued_at": (now - datetime.timedelta(minutes=1)).isoformat(),
                                        "expires_at": (now + datetime.timedelta(days=1)).isoformat()}))
            store = runner.StateStore(tmp)
            state = {"run_id": root.name, "revision": 0, "plan_dir": str(plan),
                     "plan_manifest_sha256": "digest", "authorization_path": str(auth),
                     "authorization_sha256": runner.sha256_file(auth),
                     "tasks": {"W0-01": {"state": "READY", "attempt": 0}},
                     "gates": {}}
            store.write_snapshot(state, "admit", {"synthetic": True})
            args = SimpleNamespace(run_dir=tmp, task_id="W0-01", to="RUNNING",
                                   case_results=None, commit_vector=None, blocked_reason=None)
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
                with self.assertRaises(OSError):
                    runner.cmd_task(args)
            durable = store.load_events()[-1]["payload"]["state_payload"]["tasks"]["W0-01"]
            self.assertEqual(1, durable["attempt"])
            before_events = Path(store.events_path).read_bytes()
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(runner.EXIT_OK, runner.cmd_resume(SimpleNamespace(run_dir=tmp)))
                self.assertEqual(runner.EXIT_OK, runner.cmd_resume(SimpleNamespace(run_dir=tmp)))
            self.assertEqual(durable, store.load_state()["tasks"]["W0-01"])
            self.assertEqual(before_events, Path(store.events_path).read_bytes())

    def test_resume_rejects_digest_valid_budget_extension(self):
        with tempfile.TemporaryDirectory() as tmp:
            store = runner.StateStore(tmp)
            prior = {"run_id": Path(tmp).name, "revision": 0,
                     "tasks": {"W0-01": {"state": "READY", "attempt": 2,
                                           "task_started_at": 10, "task_deadline": 100}},
                     "gates": {}}
            prior = store.write_snapshot(prior, "admit", {"synthetic": True})
            forged = dict(prior, tasks={"W0-01": {"state": "RUNNING", "attempt": 3,
                                                   "task_started_at": 10, "task_deadline": 200}})
            with patch.object(runner.StateStore, "persist_snapshot", side_effect=OSError("crash")):
                with self.assertRaises(OSError):
                    store.write_snapshot(forged, "task_state", {"task": "W0-01", "to": "RUNNING", "attempt": 3})
            before_state = Path(store.state_path).read_bytes()
            before_events = Path(store.events_path).read_bytes()
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_resume(SimpleNamespace(run_dir=tmp))
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            self.assertEqual(before_state, Path(store.state_path).read_bytes())
            self.assertEqual(before_events, Path(store.events_path).read_bytes())

    def test_snapshot_task_tampering_and_legacy_chain_are_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            store = runner.StateStore(tmp)
            store.append_event("admit", {"synthetic": True})
            state = store.write_snapshot({"revision": 0, "tasks": {"W0-01": {"state": "RUNNING", "attempt": 1}}})
            tampered = dict(state, tasks={"W0-01": {"state": "NOT_STARTED", "attempt": 0}})
            Path(store.state_path).write_text(json.dumps(tampered))
            before, events = Path(store.state_path).read_bytes(), Path(store.events_path).read_bytes()
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner._require_state(tmp)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            self.assertEqual(before, Path(store.state_path).read_bytes())
            self.assertEqual(events, Path(store.events_path).read_bytes())
            tampered = dict(state, tasks={"W0-01": {"state": "RUNNING", "attempt": True}})
            Path(store.state_path).write_text(json.dumps(tampered))
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner._require_state(tmp)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
        with tempfile.TemporaryDirectory() as tmp:
            store = runner.StateStore(tmp)
            store.append_event("admit", {"synthetic": True})
            Path(store.state_path).write_text(json.dumps({"revision": 1, "events_head_hash": store.head_hash()}))
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner._require_state(tmp)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)

    def test_next_does_not_report_stale_ready_during_concurrent_transition(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            plan = root / "plan"
            plan.mkdir()
            (plan / "execution-contract.json").write_text(json.dumps({
                "tasks": [{"task_id": "W0-01", "wave": "W0", "dependencies": []}],
                "wave_gates": {}}))
            contract_file = plan / "execution-contract.json"
            (plan / "manifest.sha256").write_text(runner.sha256_file(contract_file) + "  execution-contract.json\n")
            digest = runner.sha256_file(plan / "manifest.sha256")
            now = datetime.datetime.now(datetime.timezone.utc)
            auth = root / "auth.json"
            auth.write_text(json.dumps({"plan_manifest_sha256": digest, "implementation_allowed": True,
                                        "local_commit_allowed": True, "allowed_tasks": ["W0-01"],
                                        "issued_at": (now - datetime.timedelta(minutes=1)).isoformat(),
                                        "expires_at": (now + datetime.timedelta(days=1)).isoformat()}))
            store = runner.StateStore(tmp)
            store.append_event("admit", {"synthetic": True})
            state = store.write_snapshot({"run_id": root.name, "revision": 0, "plan_dir": str(plan),
                                          "plan_manifest_sha256": digest, "authorization_path": str(auth),
                                          "authorization_sha256": runner.sha256_file(auth),
                                          "tasks": {"W0-01": {"state": "NOT_STARTED", "attempt": 0}},
                                          "gates": {}})
            lock = runner.RunLock(tmp)
            self.assertTrue(lock.acquire_nb())
            try:
                process = subprocess.Popen([sys.executable, runner.__file__, "next", "--run-dir", tmp],
                                           stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
                store.append_event("task_state", {"task": "W0-01", "to": "RUNNING", "attempt": 1})
                stdout, stderr = process.communicate(timeout=8)
                self.assertEqual(runner.EXIT_BLOCKED, process.returncode, stderr)
                self.assertEqual("", stdout)
                state["tasks"]["W0-01"] = {"state": "RUNNING", "attempt": 1}
                store.write_snapshot(state)
            finally:
                lock.release()
            result = subprocess.run([sys.executable, runner.__file__, "next", "--run-dir", tmp],
                                    capture_output=True, text=True, timeout=8)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertEqual([], json.loads(result.stdout)["ready"])

    def test_durable_transition_ahead_of_snapshot_blocks_resume_and_next(self):
        with tempfile.TemporaryDirectory() as tmp:
            store = runner.StateStore(tmp)
            store.append_event("admit", {"synthetic": True})
            store.write_snapshot({"run_id": Path(tmp).name, "revision": 0,
                                  "tasks": {"W0-01": {"state": "NOT_STARTED", "attempt": 0}},
                                  "gates": {}})
            store.append_event("task_state", {"task": "W0-01", "to": "RUNNING", "attempt": 1})
            state_before = Path(store.state_path).read_bytes()
            events_before = Path(store.events_path).read_bytes()
            for action in (runner.cmd_next, runner.cmd_resume):
                with self.subTest(action=action.__name__), contextlib.redirect_stderr(io.StringIO()), \
                     contextlib.redirect_stdout(io.StringIO()) as output, self.assertRaises(SystemExit) as exc:
                    action(SimpleNamespace(run_dir=tmp))
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                self.assertEqual("", output.getvalue())
                self.assertEqual(state_before, Path(store.state_path).read_bytes())
                self.assertEqual(events_before, Path(store.events_path).read_bytes())

    def test_expiration_requires_valid_timezone_and_future_time(self):
        now = datetime.datetime.now(datetime.timezone.utc)
        issued = (now - datetime.timedelta(minutes=1)).isoformat()
        for value in (None, "zzzz", "2999-01-01T00:00:00", "2000-01-01T00:00:00Z"):
            with self.subTest(value=value):
                self.assertFalse(runner.authorization_unexpired({"issued_at": issued, "expires_at": value}))
        self.assertFalse(runner.authorization_unexpired({"issued_at": issued, "expires_at": (now + datetime.timedelta(days=8)).isoformat()}))
        self.assertFalse(runner.authorization_unexpired({"issued_at": (now + datetime.timedelta(days=1)).isoformat(), "expires_at": (now + datetime.timedelta(days=2)).isoformat()}))
        self.assertTrue(runner.authorization_unexpired({"issued_at": issued, "expires_at": (now + datetime.timedelta(days=1)).isoformat()}))

    def test_non_boolean_authorization_does_not_admit(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            auth = root / "authorization.json"
            auth.write_text(json.dumps({"plan_manifest_sha256": "digest",
                                        "implementation_allowed": "false",
                                        "local_commit_allowed": True}))
            args = SimpleNamespace(plan_dir=tmp, run_dir=tmp, authorization=str(auth))
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_admit(args)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            self.assertFalse((root / "state.json").exists())

    def test_allowed_tasks_must_match_without_duplicates(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            auth = root / "authorization.json"
            args = SimpleNamespace(plan_dir=tmp, run_dir=tmp, authorization=str(auth))
            for tasks in (["W0-01", "extra"], ["W0-01", "W0-01"], []):
                with self.subTest(tasks=tasks):
                    auth.write_text(json.dumps({"plan_manifest_sha256": "digest",
                                                "implementation_allowed": True,
                                                "local_commit_allowed": True,
                                                "allowed_tasks": tasks}))
                    with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                         patch.object(runner, "load_contract", return_value={"tasks": [{"task_id": "W0-01"}]}), \
                         contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                        runner.cmd_admit(args)
                    self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
                    self.assertFalse((root / "state.json").exists())

    def test_freeze_rejects_invalid_authorization_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "auth.json"
            args = SimpleNamespace(run_dir=tmp)
            for value in (None, str(path)):
                with self.subTest(path=value), \
                     patch.object(runner, "_require_state", return_value={"authorization_path": value}), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                    runner.cmd_freeze_candidate(args)
                self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            path.write_text("{")
            state = {"authorization_path": str(path), "authorization_sha256": runner.sha256_file(path)}
            with patch.object(runner, "_require_state", return_value=state), \
                 contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_freeze_candidate(args)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)

    def test_changed_authorization_cannot_reuse_old_admission(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            plan = root / "plan"
            run = root / "run"
            plan.mkdir()
            run.mkdir()
            auth = root / "authorization.json"
            auth.write_text(json.dumps({"plan_manifest_sha256": "digest",
                                        "implementation_allowed": True,
                                        "local_commit_allowed": True,
                                        "allowed_tasks": [],
                                        "issued_at": (datetime.datetime.now(datetime.timezone.utc) - datetime.timedelta(minutes=1)).isoformat(),
                                        "expires_at": (datetime.datetime.now(datetime.timezone.utc) + datetime.timedelta(days=1)).isoformat()}))
            old = {"run_id": "run", "mode": "IMPLEMENTATION",
                   "plan_dir": str(plan), "plan_manifest_sha256": "digest",
                   "authorization_path": str(auth),
                   "authorization_sha256": "old-receipt-hash"}
            store = runner.StateStore(str(run))
            store.append_event("admit", {"plan_manifest_sha256": "digest",
                                         "authorization_sha256": "old-receipt-hash"})
            store.write_snapshot(old)
            before = (run / "state.json").read_bytes()
            events_before = (run / "events.jsonl").read_bytes()
            args = SimpleNamespace(plan_dir=str(plan), run_dir=str(run), authorization=str(auth))
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                 contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_admit(args)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            self.assertEqual(before, (run / "state.json").read_bytes())
            self.assertEqual(events_before, (run / "events.jsonl").read_bytes())
            fresh = root / "fresh"
            fresh.mkdir()
            args.run_dir = str(fresh)
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                 contextlib.redirect_stdout(io.StringIO()):
                self.assertEqual(runner.EXIT_OK, runner.cmd_admit(args))
                self.assertEqual(runner.EXIT_OK, runner.cmd_admit(args))
                before_resume = (fresh / "state.json").read_bytes()
                events_resume = (fresh / "events.jsonl").read_bytes()
                self.assertEqual(runner.EXIT_OK, runner.cmd_resume(SimpleNamespace(run_dir=str(fresh))))
                self.assertEqual(before_resume, (fresh / "state.json").read_bytes())
                self.assertEqual(events_resume, (fresh / "events.jsonl").read_bytes())
                self.assertEqual(runner.EXIT_OK, runner.cmd_admit(args))
            valid_snapshot = json.loads((fresh / "state.json").read_text())
            for field in ("plan_dir", "authorization_path"):
                altered = dict(valid_snapshot, **{field: None})
                (fresh / "state.json").write_text(json.dumps(altered))
                with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                     patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                     contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as invalid:
                    runner.cmd_admit(args)
                self.assertEqual(runner.EXIT_CONTRACT, invalid.exception.code)
            (fresh / "state.json").write_text(json.dumps(valid_snapshot))
            (fresh / "events.jsonl").unlink()
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                 contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as exc:
                runner.cmd_admit(args)
            self.assertEqual(runner.EXIT_CONTRACT, exc.exception.code)
            (fresh / "state.json").write_text("null")
            with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                 patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                 contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as invalid:
                runner.cmd_admit(args)
            self.assertEqual(runner.EXIT_CONTRACT, invalid.exception.code)
            self.assertEqual("null", (fresh / "state.json").read_text())
            for state_content, event_content in (("{", "{}\n"), ("[]", "{}\n"),
                                                  (None, "{}\n"), ("{}", "[]\n")):
                with self.subTest(state=state_content, events=event_content):
                    state_path = fresh / "state.json"
                    if state_content is None:
                        state_path.unlink(missing_ok=True)
                    else:
                        state_path.write_text(state_content)
                    (fresh / "events.jsonl").write_text(event_content)
                    with patch.object(runner, "verify_plan_manifest", return_value=(True, "digest")), \
                         patch.object(runner, "load_contract", return_value={"tasks": [], "wave_gates": {}}), \
                         contextlib.redirect_stderr(io.StringIO()), self.assertRaises(SystemExit) as invalid:
                        runner.cmd_admit(args)
                    self.assertEqual(runner.EXIT_CONTRACT, invalid.exception.code)


if __name__ == "__main__":
    unittest.main()
