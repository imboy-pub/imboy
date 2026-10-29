import os
import json
from pathlib import Path
import re
import subprocess
import tempfile
import unittest


class GateDatabaseGuardTest(unittest.TestCase):
    def test_w102_second_database_guard_precedes_second_create(self):
        source = Path(__file__).with_name("attachment_lifecycle_v5_gate.sh").read_text()
        definitions = []
        for name in ("authorize_fixture_database", "w102_phase_run"):
            match = re.search(r"(?ms)^" + name + r"\(\) \{.*?^\}", source)
            self.assertIsNotNone(match, name)
            definitions.append(match.group(0))
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "resources").mkdir()
            (root / "resources" / "pg-attv5.password").write_text("fixture")
            (root / "priv" / "migrations").mkdir(parents=True)
            script = '''set -u
RUN_DIR="$1"; ROOT="$RUN_DIR"; SLOT=slot1; SCRIPT_DIR="$RUN_DIR"; PG_CONTAINER=owned
python3() { printf '%s\\n' "$4" >> "$RUN_DIR/checked"; [ "$4" != attv5_slot1_w102_guards ]; }
docker() { printf '%s\\n' "$*" >> "$RUN_DIR/docker-calls"; return 0; }
''' + "\n".join(definitions) + '\nw102_phase_run "$RUN_DIR" expand\n'
            result = subprocess.run(["bash", "-c", script, "test", tmp],
                                    capture_output=True, text=True, env=os.environ.copy())
            self.assertEqual(20, result.returncode, result.stderr)
            self.assertEqual(["attv5_slot1_w102_expand", "attv5_slot1_w102_guards"],
                             (root / "checked").read_text().splitlines())
            calls = (root / "docker-calls").read_text()
            self.assertIn("CREATE DATABASE attv5_slot1_w102_expand", calls)
            self.assertNotIn("CREATE DATABASE attv5_slot1_w102_guards", calls)

    def test_w403_skip_rejects_unowned_or_unchecked_manifest(self):
        source = Path(__file__).with_name("attachment_lifecycle_v5_gate.sh").read_text()
        definitions = []
        for name in ("authorize_fixture_database", "w403_case"):
            match = re.search(r"(?ms)^" + name + r"\(\) \{.*?^\}", source)
            self.assertIsNotNone(match, name)
            definitions.append(match.group(0))
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "resources").mkdir()
            manifest = root / "resources" / "w403-resource-manifest.json"
            script = '''set -u
RUN_DIR="$1"; SLOT=slot1; SCRIPT_DIR="$RUN_DIR"; PG_CONTAINER=owned
W403_SKIP_DB_SETUP=1; W403_RESOURCE_MANIFEST="$2"
PG_CONFIG_DIR="$RUN_DIR/config"; PG_CONFIG_READY=1; EUNIT_CONFIG="$PG_CONFIG_DIR/isolated"
mkdir -p "$PG_CONFIG_DIR"; : > "$EUNIT_CONFIG.config"
python3() { if [ "$1" = - ]; then command python3 "$@"; else touch "$RUN_DIR/checked"; return 20; fi; }
docker() { touch "$RUN_DIR/docker-called"; return 0; }
erl() { touch "$RUN_DIR/erl-called"; return 0; }
ensure_disk_root() { return 0; }
''' + "\n".join(definitions) + '\nw403_case "$RUN_DIR"\n'
            for manifest_db, supplied, guard_expected in (
                    ("attv5_chain_slot1_w403", str(root / "outside.json"), False),
                    ("attv5_chain_slot2_w403", str(manifest), False),
                    ("attv5_chain_slot1_w403", str(manifest), True)):
                with self.subTest(database=manifest_db, supplied=supplied):
                    manifest.write_text(json.dumps({"database": manifest_db}))
                    checked = root / "checked"
                    checked.unlink(missing_ok=True)
                    result = subprocess.run(["bash", "-c", script, "test", tmp, supplied],
                                            capture_output=True, text=True, env=os.environ.copy())
                    self.assertEqual(20, result.returncode, result.stderr)
                    self.assertEqual(guard_expected, checked.exists())
                    self.assertFalse((root / "docker-called").exists())
                    self.assertFalse((root / "erl-called").exists())

            manifest.write_text(json.dumps({"database": "attv5_chain_slot1_w403"}))
            env = dict(os.environ, W403_CONFIG_PATH=str(root / "foreign.config"))
            result = subprocess.run(["bash", "-c", script, "test", tmp, str(manifest)],
                                    capture_output=True, text=True, env=env)
            self.assertEqual(20, result.returncode, result.stderr)
            self.assertIn("run-owned isolated config", result.stderr)
            self.assertFalse((root / "docker-called").exists())
            self.assertFalse((root / "erl-called").exists())

    def test_destructive_entrypoints_reject_before_docker(self):
        source = Path(__file__).with_name("attachment_lifecycle_v5_gate.sh").read_text()
        functions = ("authorize_fixture_database", "w201_db_setup", "w102_phase_run",
                     "bootstrap_w002_schema", "bootstrap_w002_history", "w403_case",
                     "strict_ledger_case")
        definitions = []
        for name in functions:
            match = re.search(r"(?ms)^" + name + r"\(\) \{.*?^\}", source)
            self.assertIsNotNone(match, name)
            definitions.append(match.group(0))
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            (root / "resources").mkdir()
            (root / "resources" / "pg-attv5.password").write_text("fixture")
            for name, args in (("w201_db_setup", '"$RUN_DIR"'),
                               ("w102_phase_run", '"$RUN_DIR" guards'),
                               ("bootstrap_w002_schema", '"$RUN_DIR"'),
                               ("bootstrap_w002_history", '"$RUN_DIR"'),
                               ("w403_case", '"$RUN_DIR"'),
                               ("strict_ledger_case", '"$RUN_DIR"')):
                with self.subTest(entry=name):
                    script = '''set -u
RUN_DIR="$1"; SLOT=slot1; SCRIPT_DIR="$RUN_DIR"; PG_CONTAINER=owned
PG_CONFIG_DIR="$RUN_DIR/config"; PG_CONFIG_READY=1; EUNIT_CONFIG="$PG_CONFIG_DIR/isolated"
mkdir -p "$PG_CONFIG_DIR"; : > "$EUNIT_CONFIG.config"
python3() { printf '%s\n' "$4" >> "$RUN_DIR/checked"; return 20; }
docker() { touch "$RUN_DIR/docker-called"; return 0; }
ensure_disk_root() { return 0; }
''' + "\n".join(definitions) + "\n" + name + " " + args
                    result = subprocess.run(["bash", "-c", script, "test", tmp],
                                            capture_output=True, text=True, env=os.environ.copy())
                    self.assertEqual(20, result.returncode, result.stderr)
                    self.assertFalse((root / "docker-called").exists())
                    self.assertTrue((root / "checked").read_text().startswith("attv5_"))
                    (root / "checked").unlink()
        self.assertIn('authorize_fixture_database "$GUARDS_DB" || return 20', source)


if __name__ == "__main__":
    unittest.main()
