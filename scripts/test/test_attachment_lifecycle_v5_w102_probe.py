import json
import hashlib
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class W102ProbeTest(unittest.TestCase):
    def test_all_w102_database_targets_must_be_authorized_before_docker(self):
        script = Path(__file__).with_name("attachment_lifecycle_v5_w102_probe.sh")
        db = "attv5_slot1_w102_guards"
        expand = "attv5_slot1_w102_expand"
        cycle = "attv5_slot1_w102_queue"
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "resources").mkdir()
            auth_path = root / "auth.json"
            auth = {"resource_allowlist": {"scratch_postgres": [db, expand, cycle]}}
            raw = json.dumps(auth).encode()
            auth_path.write_bytes(raw)
            (root / "state.json").write_text(json.dumps({"authorization_path": str(auth_path),
                "authorization_sha256": hashlib.sha256(raw).hexdigest()}))
            (root / "resources" / "postgres.json").write_text(json.dumps({"allowed_slots": ["slot1"]}))
            fakebin = root / "bin"
            fakebin.mkdir()
            docker = fakebin / "docker"
            docker.write_text("#!/bin/sh\necho called >> \"$DOCKER_CALLS\"\nexit 1\n")
            docker.chmod(0o755)
            calls = root / "docker.calls"
            output = root / "out.jsonl"
            env = dict(os.environ, V5_RUN_DIR=str(root), V5_SLOT="slot1",
                       DOCKER_CALLS=str(calls), PATH=f"{fakebin}:{os.environ['PATH']}")
            # Execute the actual preflight with an injected SQL target; stop before
            # the guard so this test never inspects or connects to a database.
            source = script.read_text()
            preflight = source[source.index("set -u\n"):source.index("PG_RESOURCE=")]
            preflight = preflight.replace('SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"',
                                          f'SCRIPT_DIR="{script.parent}"')
            result = subprocess.run(["bash", "-c", preflight + '\nprintf "%s" "${PROBE_DB-unset}"',
                                     "preflight", db, "expand", str(output)],
                                    env=dict(env, PROBE_DB="postgres"), capture_output=True, text=True)
            self.assertEqual(0, result.returncode, result.stderr)
            self.assertEqual("unset", result.stdout)
            result = subprocess.run(["bash", str(script), "attv5_slot1_unlisted", "expand", str(output)],
                                    env=env, capture_output=True, text=True)
            self.assertEqual(20, result.returncode)
            self.assertIn("databases not explicitly authorized", result.stderr)
            self.assertFalse(calls.exists())
            self.assertFalse(output.exists())
            result = subprocess.run(["bash", str(script), db, "expand", str(output)],
                                    env=dict(env, V5_GUARDS_DB=expand), capture_output=True, text=True)
            self.assertEqual(20, result.returncode)
            self.assertIn("expand and guards databases must be distinct", result.stderr)
            self.assertFalse(calls.exists())
            self.assertFalse(output.exists())
            for key in ("V5_EXPAND_DB", "V5_GUARDS_DB", "V5_CYCLE_DB"):
                with self.subTest(key=key):
                    env_bad = dict(env, **{key: "attv5_slot1_unlisted"}, PROBE_DB="postgres")
                    result = subprocess.run(["bash", str(script), db, "expand", str(output)],
                                            env=env_bad, capture_output=True, text=True)
                    self.assertEqual(20, result.returncode)
                    self.assertIn("databases not explicitly authorized", result.stderr)
                    self.assertFalse(calls.exists())
                    self.assertFalse(output.exists())
            for target in (db, expand):
                with self.subTest(cycle_alias=target):
                    result = subprocess.run(["bash", str(script), db, "expand", str(output)],
                                            env=dict(env, V5_CYCLE_DB=target), capture_output=True, text=True)
                    self.assertEqual(20, result.returncode)
                    self.assertIn("cycle database must be distinct", result.stderr)
                    self.assertFalse(calls.exists())
                    self.assertFalse(output.exists())

    def test_any_failure_is_not_proof_of_expected_rejection(self):
        source = Path(__file__).with_name("attachment_lifecycle_v5_w102_probe.sh").read_text()
        functions = source[source.index("sanitize() {"):source.index("sqlval() {")]
        with tempfile.TemporaryDirectory() as tmp:
            script = f'''set -u
PROBE_TMP="$1"; OUT="$1/result.jsonl"; ok=0; bad=0
PSQL_RAW() {{ cat >/dev/null; echo "connection refused" >&2; return 1; }}
{functions}
probe example fail 'SELECT 1'
'''
            result = subprocess.run(["bash", "-c", script, "probe-test", tmp],
                                    check=True, capture_output=True, text=True)
            self.assertEqual("", result.stdout)
            record = json.loads((Path(tmp) / "result.jsonl").read_text())
            self.assertFalse(record["pass"])
            self.assertIn("exact SQLSTATE", record["detail"])


if __name__ == "__main__":
    unittest.main()
