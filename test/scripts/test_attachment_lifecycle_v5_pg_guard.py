"""Synthetic ownership checks only; Docker and PostgreSQL are never started."""
import hashlib
import importlib.util
import json
import os
import subprocess
import sys
from pathlib import Path
import tempfile
import unittest
import time
from datetime import datetime, timedelta, timezone
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("pg_guard",
    ROOT / "scripts/test/attachment_lifecycle_v5_pg_guard.py")
guard = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(guard)


class PgGuardTests(unittest.TestCase):
    def setUp(self):
        temp = tempfile.TemporaryDirectory()
        self.addCleanup(temp.cleanup)
        self.root = Path(temp.name).resolve() / "synthetic-run"
        (self.root / "resources").mkdir(parents=True)
        self.resource = dict(owner_run_id=self.root.name, container="attv5-synthetic-pg",
            container_id="a" * 64, image_id="sha256:" + "b" * 64, allowed_slots=["slot1"], host_binding=[{"HostIp": "127.0.0.1", "HostPort": "54443"}])
        plan = self.root / "frozen-plan"
        plan.mkdir()
        contract = plan / "execution-contract.json"
        contract.write_text('{}')
        manifest = plan / "manifest.sha256"
        manifest.write_text(hashlib.sha256(contract.read_bytes()).hexdigest()
                            + "  execution-contract.json\n")
        plan_hash = hashlib.sha256(manifest.read_bytes()).hexdigest()
        self.auth = self.root / "authorization.json"
        self.auth.write_text(json.dumps({
            "resource_allowlist": {"postgres_container": [self.resource["container"]],
                                   "scratch_postgres": ["attv5_slot1_ok"]},
            "implementation_allowed": True,
            "issued_at": (datetime.now(timezone.utc) - timedelta(minutes=1)).isoformat(),
            "expires_at": (datetime.now(timezone.utc) + timedelta(hours=1)).isoformat(),
            "plan_manifest_sha256": plan_hash,
        }))
        (self.root / "state.json").write_text(json.dumps(dict(
            authorization_path=str(self.auth),
            authorization_sha256=hashlib.sha256(self.auth.read_bytes()).hexdigest(),
            mode="IMPLEMENTATION", run_id=self.root.name, owner_token="synthetic-owner",
            lease_until=time.time() + 3600, plan_dir=str(plan),
            plan_manifest_sha256=plan_hash)))
        self.actual = dict(id=self.resource["container_id"], image=self.resource["image_id"],
            running=True, ports={"5432/tcp": self.resource["host_binding"]}, labels={"codex.run": self.root.name,
                "codex.role": "attachment-lifecycle-pg"})

    def resolve(self, slot="slot1"):
        (self.root / "resources/postgres.json").write_text(json.dumps(self.resource))
        with mock.patch.object(guard.subprocess, "check_output", return_value=json.dumps(self.actual)):
            return guard.owned_container(str(self.root), slot)

    def test_exact_owned_container(self):
        self.assertEqual(("a" * 64, "127.0.0.1", "54443"), self.resolve())

    def test_shared_container_and_sql_injection_slots_are_rejected(self):
        for slot in ["slot1;DROP DATABASE postgres", "../slot1", "slot2"]:
            with self.assertRaises(ValueError): self.resolve(slot)
        self.resource["container"] = "imboy_pg18"
        with self.assertRaises(ValueError): self.resolve()

    def test_replaced_stopped_or_foreign_container_is_rejected(self):
        for field, value in [("id", "c" * 64), ("running", False),
                             ("labels", {"codex.run": "other-run"})]:
            original = dict(self.actual)
            self.actual[field] = value
            with self.assertRaises(ValueError): self.resolve()
            self.actual = original

    def test_changed_or_public_binding_is_rejected(self):
        self.actual["ports"] = {"5432/tcp": [{"HostIp": "0.0.0.0", "HostPort": "54443"}]}
        with self.assertRaises(ValueError): self.resolve()
        self.resource["host_binding"] = self.actual["ports"]["5432/tcp"]
        with self.assertRaises(ValueError): self.resolve()

    def test_cli_refuses_even_valid_resource_until_fixture_identity_is_bound(self):
        (self.root / "resources/postgres.json").write_text(json.dumps(self.resource))
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        calls = self.root / "calls.jsonl"
        inspect = self.root / "inspect.json"
        inspect.write_text(json.dumps(self.actual))
        docker = bin_dir / "docker"
        docker.write_text("#!/usr/bin/env python3\nimport json,os,sys\n"
            "with open(os.environ['FAKE_CALLS'],'a') as f: f.write(json.dumps(sys.argv[1:])+'\\n')\n"
            "if sys.argv[1] != 'inspect': sys.exit(99)\n"
            "print(open(os.environ['FAKE_INSPECT']).read())\n")
        docker.chmod(0o700)
        env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"],
                   FAKE_CALLS=str(calls), FAKE_INSPECT=str(inspect))
        result = subprocess.run([sys.executable, str(ROOT / "scripts/test/attachment_lifecycle_v5_pg_guard.py"),
                                 str(self.root), "slot1"], env=env, capture_output=True,
                                text=True, timeout=10)
        self.assertEqual(20, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertIn("scratch database allowlist and connection identity", result.stderr)
        rows = [json.loads(line) for line in calls.read_text().splitlines()]
        self.assertEqual(1, len(rows))
        self.assertEqual("inspect", rows[0][0])

    def test_cli_rejects_non_object_authorization_before_docker(self):
        (self.root / "resources/postgres.json").write_text(json.dumps(self.resource))
        self.auth.write_text("[]")
        state_path = self.root / "state.json"
        state = json.loads(state_path.read_text())
        state["authorization_sha256"] = hashlib.sha256(self.auth.read_bytes()).hexdigest()
        state_path.write_text(json.dumps(state))
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        calls = self.root / "docker-called"
        docker = bin_dir / "docker"
        docker.write_text("#!/bin/sh\ntouch \"$FAKE_CALLS\"\nexit 99\n")
        docker.chmod(0o700)
        env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"], FAKE_CALLS=str(calls))
        result = subprocess.run([sys.executable, str(ROOT / "scripts/test/attachment_lifecycle_v5_pg_guard.py"),
                                 str(self.root), "slot1", "attv5_slot1_ok"],
                                env=env, capture_output=True, text=True, timeout=10)
        self.assertEqual(20, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertIn("CONTRACT_CONFLICT", result.stderr)
        self.assertFalse(calls.exists())

    def test_probe_subprocesses_refuse_foreign_databases_before_exec(self):
        (self.root / "resources/postgres.json").write_text(json.dumps(self.resource))
        (self.root / "resources/pg-attv5.password").write_text("synthetic-only")
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        calls = self.root / "docker-calls.jsonl"
        actual = self.root / "inspect.json"
        actual.write_text(json.dumps(self.actual))
        docker = bin_dir / "docker"
        docker.write_text("#!/usr/bin/env python3\nimport json,os,sys\n"
            "with open(os.environ['FAKE_CALLS'],'a') as f: f.write(json.dumps(sys.argv[1:])+'\\n')\n"
            "if sys.argv[1] != 'inspect': sys.exit(99)\n"
            "print(open(os.environ['FAKE_INSPECT']).read())\n")
        docker.chmod(0o700)
        env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"],
            FAKE_CALLS=str(calls), FAKE_INSPECT=str(actual),
            V5_RUN_DIR=str(self.root), V5_SLOT="slot1")
        for script, phase in [("w102", "guards"), ("w201", "ordinary")]:
            probe = ROOT / f"scripts/test/attachment_lifecycle_v5_{script}_probe.sh"
            for database, extra in [("postgres", {}),
                                    ("attv5_slot1_ok", {"V5_EXPAND_DB": "attv5_slot2_foreign"}),
                                    ("attv5_slot1_ok", {"PROBE_DB": "postgres"}),
                                    ("attv5_slot1_ok", {"V5_CYCLE_DB": "attv5_slot1_unlisted"})]:
                with self.subTest(script=script, database=database, extra=extra):
                    result = subprocess.run(["bash", str(probe), database, phase, str(self.root / "out")],
                        env=dict(env, **extra), capture_output=True, text=True, timeout=10)
                    self.assertEqual(20, result.returncode, result.stderr)
        rows = [json.loads(line) for line in calls.read_text().splitlines()]
        # W102 now rejects these unlisted targets before Docker inspection.
        self.assertEqual(3, len(rows))
        self.assertTrue(all(row[0] == "inspect" for row in rows))
        self.assertFalse((self.root / "out").exists())

        # W2 validates the exact requested database before even inspecting Docker.
        before = calls.read_bytes()
        probe = ROOT / "scripts/test/attachment_lifecycle_v5_w201_probe.sh"
        result = subprocess.run(["bash", str(probe), "attv5_slot1_unlisted", "ordinary", str(self.root / "out")],
                                env=env, capture_output=True, text=True, timeout=10)
        self.assertEqual(20, result.returncode)
        self.assertEqual(before, calls.read_bytes())
        self.assertFalse((self.root / "out").exists())

    def test_config_replaces_foreign_pool_without_connecting(self):
        config = self.root / "foreign.config"
        output = self.root / "isolated.config"
        password = self.root / "synthetic.password"
        password.write_text("new-synthetic-password")
        config.write_text('[{imboy,[{http_port,19980},{pg_conf,#{init_count => 1, start_mfa => {epgsql,connect,[#{host => "foreign.example",port => 4323,username => "foreign",password => "old",database => "shared"}]}}}]}].')
        result = subprocess.run(["escript", str(ROOT / "scripts/test/attachment_lifecycle_v5_pg_config.escript"),
            str(config), str(output), str(password), "127.0.0.1", "54443", "attv5_slot1_pool"],
            capture_output=True, text=True, timeout=15)
        self.assertEqual(0, result.returncode, result.stderr)
        text = output.read_text()
        self.assertNotIn("foreign", text)
        self.assertNotIn("4323", text)
        self.assertNotIn('"shared"', text)
        self.assertIn("54443", text)
        self.assertIn("attv5_slot1_pool", text)
        self.assertIn("19980", text)
        self.assertEqual(0o600, output.stat().st_mode & 0o777)
        self.assertNotIn("new-synthetic-password", result.stdout + result.stderr)

    def test_static_case_stays_blocked_until_output_paths_are_safe(self):
        plan = self.root / "plan"
        plan.mkdir()
        (plan / "execution-contract.json").write_text(json.dumps({
            "tasks": [{"task_id": "W1-01", "case_ids": ["W1-01/contract"]}],
            "schema_phase_enum": [], "expected_leaf_matrix": {"W1-01/contract": []}}))
        (self.root / "state.json").write_text(json.dumps({"plan_dir": str(plan)}))
        (self.root / "logs").mkdir()
        result = subprocess.run(["bash", str(ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh"),
            "--task", "W1-01", "--case", "contract", "--run-dir", str(self.root), "--slot", "slot1"],
            cwd=ROOT, capture_output=True, text=True, timeout=15)
        self.assertNotIn("dedicated owned PostgreSQL", result.stderr + result.stdout)
        self.assertNotIn("isolated PG password missing", result.stderr + result.stdout)
        self.assertEqual(75, result.returncode, result.stdout + result.stderr)
        self.assertFalse((self.root / "evidence").exists())
        self.assertEqual(["slot-locks"], sorted(p.name for p in (self.root / "resources").iterdir()))
        self.assertEqual(["slot1.lock"], [p.name for p in (self.root / "resources/slot-locks").iterdir()])

    def test_static_gate_does_not_follow_existing_raw_log_symlink(self):
        target = self.root / "preserve.txt"
        target.write_text("preserve")
        for task, case in (("W0-01", "baseline"), ("W1-01", "contract")):
            with self.subTest(task=task):
                case_dir = self.root / "evidence" / task / case
                case_dir.mkdir(parents=True)
                (case_dir / "raw.log").symlink_to(target)
                result = subprocess.run(
                    ["bash", str(ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh"),
                     "--task", task, "--case", case, "--run-dir", str(self.root), "--slot", "slot1"],
                    cwd=ROOT, capture_output=True, text=True, timeout=15)
                self.assertEqual(75, result.returncode, result.stderr)
                self.assertEqual("preserve", target.read_text())
                self.assertTrue((case_dir / "raw.log").is_symlink())

    def test_extension_commands_quote_identifiers_and_stop_on_failure(self):
        lines = [line.strip() for line in (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text().splitlines()
                 if 'psql ' in line and 'CREATE EXTENSION IF NOT EXISTS' in line]
        self.assertEqual(6, len(lines))
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        calls = self.root / "extension-calls.jsonl"
        docker = bin_dir / "docker"
        docker.write_text("#!/usr/bin/env python3\nimport json,os,sys\n"
            "with open(os.environ['FAKE_CALLS'],'a') as f: f.write(json.dumps(sys.argv[1:])+'\\n')\n"
            "sys.exit(int(os.environ['FAKE_EXIT']))\n")
        docker.chmod(0o700)
        env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"],
            FAKE_CALLS=str(calls), E="uuid-ossp", PG_CONTAINER="a" * 64,
            DB="attv5_slot1_test", GUARDS_DB="attv5_slot1_guards", cdir=str(self.root))
        for line in lines:
            for failure in [0, 1]:
                command = "check() { " + line + "; echo reached; }; check"
                result = subprocess.run(["bash", "-c", command], env=dict(env, FAKE_EXIT=str(failure)),
                    capture_output=True, text=True, timeout=5)
                self.assertEqual(75 if failure else 0, result.returncode, result.stderr)
                self.assertEqual("" if failure else "reached", result.stdout.strip())
        rows = [json.loads(line) for line in calls.read_text().splitlines()]
        self.assertTrue(all(row[-1] == 'CREATE EXTENSION IF NOT EXISTS "uuid-ossp"' for row in rows))

    def test_w202_success_text_with_nonzero_exit_never_reaches_sealer(self):
        source = (ROOT / "scripts/test/attachment_lifecycle_v5_gate.sh").read_text()
        def function(name):
            start = source.index(name + "() {")
            return source[start:source.index("\n}", start) + 2]
        bin_dir = self.root / "bin"
        bin_dir.mkdir()
        make = bin_dir / "make"
        make.write_text("#!/bin/sh\necho 'All 10 tests passed'\nexit 7\n")
        make.chmod(0o700)
        case_dir = self.root / "case"
        case_dir.mkdir()
        env = dict(os.environ, PATH=str(bin_dir) + os.pathsep + os.environ["PATH"],
            ROOT=str(self.root), RUN_DIR=str(self.root), EVID=str(self.root / "evidence"),
            TEST_CASE=str(case_dir), CUR_CASE="W2-02/confirm-idempotent")
        script = function("w202_eunit") + "\n" + function("w202_case") + '\nw202_case "$TEST_CASE"'
        result = subprocess.run(["bash", "-c", script], env=env, capture_output=True, text=True, timeout=10)
        self.assertEqual(30, result.returncode, result.stderr)
        self.assertFalse(list(case_dir.glob("leaf-*.json")))
        self.assertIn("All 10 tests passed", (self.root / "evidence/W2-02-shared/eunit-confirm.log").read_text())

    def test_modified_authorization_is_rejected(self):
        self.auth.write_text("{}")
        with self.assertRaises(ValueError): self.resolve()


if __name__ == "__main__":
    unittest.main()
