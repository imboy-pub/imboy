import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class ProbeEvidenceTest(unittest.TestCase):
    def run_probe(self, error, state="t", name="failclosed_insert_error", after=None):
        source = Path(__file__).with_name("attachment_lifecycle_v5_w201_probe.sh").read_text()
        probe = source[source.index("probe() {"):source.index("\nseed() {")]
        with tempfile.TemporaryDirectory() as directory:
            env = dict(os.environ, PROBE_TMP=directory, OUT=f"{directory}/result",
                       ERROR=error, STATE=state, STATE_AFTER=after or state, NAME=name)
            script = r'''
set -u
KEY=k; KEY2=k2; CID=00000000-0000-0000-0000-000000000001
CID2=$CID; REG='{}'; REG_APP='{}'; ok=0; bad=0
PSQL() {
  input=$(cat)
  case "$input" in
    "SELECT EXISTS"*)
      if [ -f "$PROBE_TMP/pre-observed" ]; then printf '%s\n' "$STATE_AFTER";
      else touch "$PROBE_TMP/pre-observed"; printf '%s\n' "$STATE"; fi ;;
    *) printf '%s\n' "$ERROR" >&2; return 3 ;;
  esac
}
''' + probe + r'''
probe "$NAME" ordinary insert_error AC01 fail 'SELECT rejected()'
'''
            completed = subprocess.run(["bash", "-c", script], env=env,
                                       capture_output=True, text=True, check=True)
            self.assertEqual(completed.returncode, 0)
            return json.loads(Path(env["OUT"]).read_text())

    def test_exact_error_and_state_are_only_sql_evidence(self):
        row = self.run_probe("ERROR:  23514: v5 register requires deadline and expiry<=deadline")
        self.assertTrue(row["sql_pass"])
        self.assertFalse(row["pass"])
        self.assertNotIn("entrypoint_provenance", row)

    def test_wrong_error_connection_failure_and_missing_state_fail(self):
        for error, state in [("ERROR:  42501: permission denied", "t"),
                             ("could not connect", "t"),
                             ("ERROR:  23514: wrong validation", "t"),
                             ("ERROR:  23514: v5 register requires deadline and expiry<=deadline", "f")]:
            with self.subTest(error=error, state=state):
                self.assertFalse(self.run_probe(error, state)["sql_pass"])

    def test_unimplemented_database_fault_never_passes(self):
        row = self.run_probe("ERROR:  23514: v5 register requires deadline and expiry<=deadline", name="mp_db_unavailable")
        self.assertFalse(row["sql_pass"])
        self.assertFalse(row["pass"])

    def test_changed_post_state_is_rejected(self):
        row = self.run_probe("ERROR:  23514: v5 register requires deadline and expiry<=deadline", after="f")
        self.assertFalse(row["sql_pass"])


if __name__ == "__main__":
    unittest.main()
