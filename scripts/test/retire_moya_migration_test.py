#!/usr/bin/env python3
"""Migration 224 checks in a new, private PostgreSQL cluster; no existing DB access."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
UP = (ROOT / "priv/migrations/00000224_retire_moya.up.sql").read_text()
DOWN = (ROOT / "priv/migrations/00000224_retire_moya.down.sql").read_text()
SEQUENCES = (96, 97, 98, 99, 100, 105, 106, 110, 151, 152)
BASE = """
CREATE TABLE public."user" (id bigint PRIMARY KEY);
CREATE TABLE public.organization (id bigint PRIMARY KEY);
CREATE TABLE public.workspace (id bigint PRIMARY KEY, organization_id bigint);
CREATE TABLE public."group" (id bigint PRIMARY KEY, workspace_id bigint);
CREATE TABLE public.group_task (
 id bigint PRIMARY KEY, task_id text UNIQUE, group_id bigint, creator_id bigint);
CREATE TABLE public.group_task_assignment (
 id bigint PRIMARY KEY, task_id text, user_id bigint,
 CONSTRAINT group_task_assignment_task_id_user_id_key UNIQUE (task_id, user_id));
CREATE TABLE public.attachment (id bigint PRIMARY KEY, status smallint, scope text);
CREATE TABLE public.attach_pending (object_key text PRIMARY KEY, scope text);
"""


class RetirementMigrationTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        initdb = shutil.which("initdb")
        if not initdb:
            raise RuntimeError("initdb is required; existing databases are never used")
        cls.bin_dir = Path(initdb).parent
        cls.temp = tempfile.TemporaryDirectory(prefix="imboy-retire-moya-")
        cls.data = Path(cls.temp.name) / "data"
        cls.socket = Path(cls.temp.name) / "socket"
        cls.socket.mkdir(mode=0o700)
        cls.env = {key: value for key, value in os.environ.items() if not key.startswith("PG")}
        cls.env.update(PGPASSFILE="/dev/null", PGSERVICEFILE="/dev/null")
        cls.call("initdb", "-D", str(cls.data), "-U", "retirement_test", "-A", "trust",
                 "--no-locale", "--encoding=UTF8")
        cls.call("pg_ctl", "-D", str(cls.data), "-l", str(Path(cls.temp.name) / "postgres.log"),
                 "-o", f"-F -k {cls.socket} -h '' -p 55432", "-w", "start")
        cls.addClassCleanup(cls.stop_cluster)
        cls.case_number = 0

    @classmethod
    def stop_cluster(cls):
        try:
            cls.call("pg_ctl", "-D", str(cls.data), "-m", "fast", "-w", "stop")
        finally:
            cls.temp.cleanup()

    @classmethod
    def call(cls, tool, *args, check=True):
        return subprocess.run([str(cls.bin_dir / tool), *args], env=cls.env,
                              capture_output=True, text=True, check=check)

    @classmethod
    def sql(cls, database, statement, check=True):
        return cls.call("psql", "-X", "-w", "-h", str(cls.socket), "-p", "55432",
                        "-U", "retirement_test", "-d", database, "-v", "ON_ERROR_STOP=1",
                        "-A", "-t", "-c", statement, check=check)

    def setUp(self):
        type(self).case_number += 1
        self.database = f"retirement_case_{self.case_number}"
        self.sql("postgres", f"CREATE DATABASE {self.database}")
        statements = [BASE]
        for sequence in SEQUENCES:
            source = next((ROOT / "priv/migrations").glob(f"{sequence:08d}_*.up.sql"))
            statements.append(source.read_text())
        self.sql(self.database, "BEGIN;\n" + "\n".join(statements) + "\nCOMMIT;")

    def schema(self):
        result = self.call("pg_dump", "-h", str(self.socket), "-p", "55432",
                           "-U", "retirement_test", "-d", self.database, "--schema-only",
                           "--no-owner", "--no-privileges", "--no-comments")
        return "\n".join(line for line in result.stdout.splitlines()
                         if line.strip() and not line.startswith(("\\restrict", "\\unrestrict", "--")))

    def assert_refused(self, setup, message):
        self.sql(self.database, setup)
        before = self.schema()
        result = self.sql(self.database, "BEGIN;\n" + UP + "\nCOMMIT;", check=False)
        self.assertNotEqual(0, result.returncode)
        self.assertIn(message, result.stderr)
        self.assertEqual(before, self.schema())

    def test_empty_roundtrip_preserves_shared_rows_and_schema(self):
        self.sql(self.database, "INSERT INTO group_task VALUES (1,'task',2,3,NULL,NULL);"
                 "INSERT INTO group_task_assignment(id,task_id,user_id) VALUES (1,'task',3)")
        before = self.schema()
        self.sql(self.database, "BEGIN;\n" + UP + "\nCOMMIT;")
        result = self.sql(self.database, "SELECT to_regclass('public.learner') IS NULL;")
        self.assertEqual("t", result.stdout.strip())
        self.assertEqual("1", self.sql(self.database,
                         "SELECT count(*) FROM group_task_assignment").stdout.strip())
        self.sql(self.database, "BEGIN;\n" + DOWN + "\nCOMMIT;")
        self.assertEqual(before, self.schema())
        self.sql(self.database, "BEGIN;\n" + UP + "\nCOMMIT;")

    def test_nonempty_audit_is_preserved(self):
        self.assert_refused(
            "INSERT INTO teaching_admin_audit(id,action,operator_uid,learner_id) "
            "VALUES (1,'data_export',0,1)", "teaching_admin_audit is not empty")
        self.assertEqual("1", self.sql(self.database,
                         "SELECT count(*) FROM teaching_admin_audit").stdout.strip())

    def test_task_metadata_is_preserved(self):
        self.assert_refused("INSERT INTO group_task VALUES (1,'task',2,3,'key','digest')",
                            "shared group task tables contain teaching metadata")
        self.assertEqual("digest", self.sql(self.database,
                         "SELECT request_digest FROM group_task").stdout.strip())

    def test_legacy_attachment_is_preserved(self):
        self.assert_refused("INSERT INTO attachment VALUES (1,1,'teaching')",
                            "legacy teaching attachments or upload sessions remain")

    def test_legacy_pending_session_is_preserved(self):
        self.assert_refused("INSERT INTO attach_pending VALUES ('synthetic','moya')",
                            "legacy teaching attachments or upload sessions remain")

    def test_unknown_table_dependency_is_refused(self):
        self.assert_refused("CREATE VIEW external_learner_view AS SELECT id FROM learner",
                            "depend on it")

    def test_unknown_shared_column_dependency_is_refused(self):
        self.assert_refused("CREATE INDEX external_learner_index ON group_task_assignment(learner_id)",
                            "unknown shared-column dependency")

    def test_unknown_function_body_dependency_is_refused(self):
        self.assert_refused("CREATE FUNCTION external_reader() RETURNS bigint LANGUAGE plpgsql "
                            "AS $$ BEGIN RETURN (SELECT count(*) FROM learner); END $$",
                            "function may reference retired tables")


if __name__ == "__main__":
    unittest.main(verbosity=2)
