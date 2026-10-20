"""T20候选启动器；默认只显示计划，--execute且完整freeze匹配才允许构建。"""

import argparse
import collections
import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import re
import signal
import sys

sys.dont_write_bytecode = True
SUITE = "textguard_business_tests"
TASK = "T20"
STANDALONE_CONSUMERS = set()
ACCEPTANCE = [f"AC-{value:03d}" for value in range(78, 84)]
BUSINESS_SOURCES = ["test/lib/textguard_business_tests.erl",
                    "test/support/textguard_business_fixture.erl"]
CONSUMER_SOURCES = [
    "test/logic/moderation_policy_tests.erl", "test/logic/adm_moderation_logic_tests.erl",
    "test/logic/user_logic_tests.erl", "test/logic/moment_logic_tests.erl",
    "test/logic/channel_logic_message_tests.erl", "test/logic/channel_publish_idempotency_tests.erl",
    "test/logic/channel_logic_tests.erl", "test/ds/moderation_ds_tests.erl",
    "test/repo/channel_message_repo_tests.erl", "test/repo/channel_message_idempotency_tests.erl"]


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def load(path, name):
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write_new(path, value):
    with path.open("x", encoding="utf-8") as output:
        json.dump(value, output, ensure_ascii=False, indent=2)
        output.write("\n")


def schema_subset(snapshot, output, save):
    sources = {}
    statements = []
    table_sources = {
        "00000001_foundation.up.sql": ['"user"', "user_friend", "user_denylist"],
        "00000003_channel.up.sql": ["channel", "channel_admin", "channel_message", "channel_subscription"],
        "00000004_social.up.sql": ["moment_post", "moment_post_acl", "moment_timeline"],
    }
    migrations = snapshot / "priv/migrations"
    for name, tables in table_sources.items():
        path = migrations / name
        text = path.read_text()
        sources[name] = sha(path)
        for table in tables:
            escaped = re.escape(table)
            creates = re.findall(r"CREATE TABLE public\." + escaped + r"\s*\([\s\S]*?\n\);", text)
            if len(creates) != 1:
                raise RuntimeError("fixture must resolve exactly one production table: " + table)
            statements.extend(creates)
            statements.extend(re.findall(r"ALTER TABLE ONLY public\." + escaped +
                                         r"\s+ADD CONSTRAINT [^;]+;", text))
            statements.extend(re.findall(r"CREATE (?:UNIQUE )?INDEX [^;]+ ON public\." +
                                         escaped + r"\s+[^;]+;", text))
    for number in [22, 34, 39, 59, 63, 72, 197]:
        paths = sorted(migrations.glob(f"{number:08d}_*.up.sql"))
        if len(paths) != 1:
            raise RuntimeError("ambiguous required migration " + str(number))
        path = paths[0]
        sources[path.name] = sha(path)
        statements.append(path.read_text())
    scope = migrations / "00000077_resource_scope.up.sql"
    sources[scope.name] = sha(scope)
    for expression in [
        r"ALTER TABLE channel ADD COLUMN IF NOT EXISTS scope [^;]+;",
        r"ALTER TABLE channel ADD COLUMN IF NOT EXISTS workspace_id [^;]+;",
        r"ALTER TABLE channel ADD CONSTRAINT chk_channel_scope_xor CHECK \([\s\S]*?\);",
    ]:
        matches = re.findall(expression, scope.read_text())
        if len(matches) != 1:
            raise RuntimeError("scope fixture production DDL changed")
        statements.extend(matches)
    path = output / "business-schema-subset.sql"
    with path.open("x") as target:
        target.write("\n\n".join(statements) + "\n")
    save(output / "business-schema-subset-proof.json", {
        "fixture_sha256": sha(path), "production_migrations_sha256": sources,
        "table_sources": table_sources, "full_migration_chain_tested": False,
        "omitted": ["unrelated foundation/channel/social tables", "workspace FK and membership triggers"],
        "scope": "owned empty PG, personal channels; workspace behavior is not certified",
    })
    return path


def parser():
    cli = argparse.ArgumentParser(description=__doc__)
    cli.add_argument("--execute", action="store_true")
    for key in ["cwd", "output", "rust-cwd", "cache-source", "base-launcher", "cargo-cache",
                "rustup-home", "freeze", "t19-launcher", "worker-log"]:
        cli.add_argument("--" + key, type=Path, required=True)
    for key in ["expected-sha", "expected-tree", "rust-sha", "rust-tree", "resource-clearance"]:
        cli.add_argument("--" + key, required=True)
    return cli


def main():
    args = parser().parse_args()
    if not args.execute:
        print(json.dumps({"status": "NOT_EXECUTED", "candidate": str(args.cwd),
            "pending": "main freeze/API review and explicit resource clearance",
            "order": ["freeze verify", "fresh native Makefile", "fresh locked offline Rust",
                      "compile and discover T20", "owned PG/Rust", "T20 leaves", "cleanup"],
            "source_write": False, "prepared_supported": False}, ensure_ascii=False))
        return 0
    freeze = json.loads(args.freeze.read_text())
    files = freeze["files"]
    if not isinstance(files, dict) or not files:
        raise RuntimeError("freeze.files must be a nonempty path-to-sha256 map")
    candidate = args.cwd.resolve()
    if not set(BUSINESS_SOURCES).issubset(files):
        raise RuntimeError("freeze must cover both formal business test files")
    for name, digest in files.items():
        path = (candidate / name).resolve()
        if not path.is_relative_to(candidate) or sha(path) != digest:
            raise RuntimeError("freeze mismatch: " + name)
    module = load(args.t19_launcher.resolve(), "tg20_t19_lifecycle")
    base = module.load_base(args.base_launcher.resolve())
    original_snapshot = module.source_snapshot

    def complete_snapshot(root):
        inventory = original_snapshot(root)
        inventory.update({name: sha(root / name) for name in files})
        return inventory

    module.source_snapshot = complete_snapshot
    module.SUITE = SUITE
    module.ACCEPTANCE = ACCEPTANCE
    base.SUITES = {SUITE: 1}
    args.prepare_only = False
    Parent = module.run_type(base)

    class Run(Parent):
        def __init__(self, values):
            super().__init__(values)
            self.env["ERL_CRASH_DUMP"] = str(self.out / "erl_crash.dump")

        def preflight(self):
            super().preflight()
            if any(self.identity_before["source_sha256"].get(name) != digest
                   for name, digest in files.items()):
                raise RuntimeError("freeze differs from snapshot inventory")
            dirty = module.git(self.root, "diff", "--name-only", "HEAD").splitlines()
            untracked = module.git(self.root, "ls-files", "--others", "--exclude-standard").splitlines()
            owned_inputs = [name for name in dirty + untracked if name.startswith(
                ("src/", "include/", "priv/", "config/", "test/", "scripts/")) or name == "Makefile"]
            if not set(owned_inputs).issubset(files):
                raise RuntimeError("dirty candidate inputs not covered by freeze")
            base.save(self.out / "t20-source-freeze-before.json", {
                "path": str(args.freeze), "manifest_sha256": sha(args.freeze), "files": files,
                "formal_test_sha256": {name: sha(self.root / name) for name in BUSINESS_SOURCES},
                "consumer_source_sha256": {name: sha(self.root / name) for name in CONSUMER_SOURCES}})

        def compile_tests(self):
            env = dict(self.env, ERL_LIBS=str(self.snapshot / "deps"))
            if TASK == "T21":
                output, _ = self.command("runner-privacy-selftest", [sys.executable, "-m", "unittest",
                    "discover", "-s", str(self.snapshot / "test/scripts"),
                    "-p", "test_textguard_privacy.py", "-v"], env=env)
                count = re.search(r"Ran (\d+) tests?", output)
                if not count or int(count.group(1)) < 1 or not output.rstrip().endswith("OK"):
                    raise RuntimeError("privacy oracle selftest discovered no successful tests")
                base.save(self.out / "runner-privacy-selftest-counts.json", {
                    "discovered": int(count.group(1)), "executed": int(count.group(1)),
                    "failed": 0, "passed": True})
            sources = [self.snapshot / "test/lib/textguard_export_tests.erl",
                self.snapshot / "test/common/ewh_http_fixture.erl",
                self.snapshot / "test/common/isolated_pg_fixture.erl",
                self.snapshot / "test/common/inttest_marker_db.erl",
                self.snapshot / "test/common/eunit_runner.erl",
                self.snapshot / "test/common/test_helper.erl",
                self.snapshot / "test/common/meck_helper.erl",
                self.snapshot / "test/common/test_config.erl",
                args.base_launcher.resolve().parent / "tg_leaf_listener.erl"]
            sources += sorted((self.snapshot / "test/support").glob("textguard_*.erl"))
            sources += [self.snapshot / name for name in CONSUMER_SOURCES]
            for name in BUSINESS_SOURCES:
                formal = self.snapshot / name
                if not formal.is_file() or sha(formal) != files[name]:
                    raise RuntimeError("formal T20 test differs from freeze: " + name)
                if formal not in sources:
                    sources.append(formal)
            self.command("compile-t20-business", ["erlc", "+debug_info", "-DTEST=1", "-I",
                str(self.snapshot / "include"), "-pa", str(self.snapshot / "ebin"),
                "-o", str(self.build), *map(str, sources)], env=env)
            if (self.build / "textguard_pb.beam").exists():
                raise RuntimeError("test build shadows production protobuf")
            expression = ('Expected=' + module.erlang_string(self.snapshot / "ebin/textguard_pb.beam") +
                ',Expected=code:which(textguard_pb),{module,textguard_pb}=code:ensure_loaded(textguard_pb),'
                'io:format("~s",[jsone:encode(#{loaded_from=>list_to_binary(Expected)})]),halt().')
            raw, _ = self.command("production-protobuf-loaded", ["erl", "-noinput", "-boot",
                "no_dot_erlang", "-noshell", "-pa", str(self.snapshot / "ebin"),
                "-pa", str(self.build), "-eval", expression], env=env)
            self.proto_proof = dict(self.proto_proof, **json.loads(raw))
            base.save(self.out / "production-protobuf-loaded-data.json", self.proto_proof)
            expression = ('Cases=[maps:with([id,acceptance],Case)||Case<-'
                + SUITE + ':manifest()],io:format("~s",[jsone:encode(Cases)]),halt().')
            raw, _ = self.command("discover-t20-cases", ["erl", "-noinput", "-boot", "no_dot_erlang",
                "-noshell", "-pa", str(self.snapshot / "ebin"), "-pa", str(self.build), "-eval", expression], env=env)
            self.tests = json.loads(raw)
            if not self.tests or len({case["id"] for case in self.tests}) != len(self.tests):
                raise RuntimeError("empty or duplicate T20 manifest")
            base.save(self.out / "declared-cases.json", self.tests)
            self.schema = schema_subset(self.snapshot, self.out, base.save)
            base.save(self.out / "t20-build-bindings.json", {
                "fresh_build": True, "prepared_supported": False,
                "test_source_sha256": {str(path): sha(path) for path in sources},
                "production_beam_sha256": {path.name: sha(path) for path in
                    (self.snapshot / "ebin").glob("*.beam")},
                "erlang_uncompiled_at_draft_time": True,
                "runtime_test_inputs": "candidate formal test/lib and test/support only"})

        def start_service(self):
            super().start_service()
            self.test_env["TG20_SCHEMA_FILE"] = str(self.schema)
            migrations = self.snapshot / "priv/migrations"
            for direction in ("up", "down"):
                path = migrations / ("00000197_textguard_review_metadata." + direction + ".sql")
                self.test_env["TG20_MIGRATION_" + direction.upper()] = str(path)

        def consumer_config(self, suite="affected"):
            path = self.credentials / ("consumer-" + suite + ".config")
            quoted = json.dumps
            options = ("#{host=>\"127.0.0.1\",username=>" + quoted(self.test_env["TG16_PG_USER"]) +
                ",password=>" + quoted(self.test_env["TG16_PG_PASSWORD"]) +
                ",database=>" + quoted(self.test_env["TG16_PG_DB"]) +
                ",port=>" + self.test_env["TG16_PG_PORT"] +
                ",ssl=>false,timeout=>4000,codecs=>[{epgsql_codec_rfc3339_bin,[]}]}" )
            settings = ("[{imboy,[{env,test},{sql_driver,pgsql},{auto_migrate,false},{http_port,0},"
                "{start_mode,http},{dsync_enabled,false},{push,[{enabled,false}]},"
                "{sms,[{switch,<<\"off\">>}]},{moderation_detector,[{mode,legacy}]},"
                "{pg_conf,#{name=>pgsql,init_count=>20,max_count=>40,"
                "start_mfa=>{epgsql,connect,[" + options + "]}}}]},"
                "{lager,[{log_root," + quoted(str(self.out / "consumer-log")) + "}]}].\n")
            descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
            with os.fdopen(descriptor, "w") as handle:
                handle.write(settings)
            env = {key: value for key, value in self.test_env.items()
                   if not key.startswith("IMBOY") and key not in ["HTTP_PORT", "TEST_HTTP_PORT", "IMBOY_TEST_CONFIG"]}
            env.update(IMBOYENV="test", IMBOY_ENV="test", HTTP_PORT="0", TEST_HTTP_PORT="0",
                       IMBOY_TEST_CONFIG=str(path))
            base.save(self.out / ("consumer-" + suite + "-environment-policy.json"), {
                "config_file": str(path), "mode": "0600", "credential_values_recorded": False,
                "db": "same task-owned PG only", "auto_migrate": False,
                "cwd": str(self.snapshot), "runner": "test/common/eunit_runner.erl",
                "listener": "cowboy start_clear passthrough forces loopback port0",
                "unrelated_IMBOY_environment_removed": True})
            return path, env

        def execute(self):
            super().execute()
            self.consumer_summaries = {}
            if not self.execution or not self.execution["passed"]:
                return
            config, env = self.consumer_config()
            application_root = self.out / "imboy"
            application_root.symlink_to(self.snapshot, target_is_directory=True)
            for source in CONSUMER_SOURCES:
                suite = Path(source).stem
                if suite in STANDALONE_CONSUMERS:
                    database = "tg_" + suite
                    self.command("consumer-db-" + suite, ["docker", "exec", self.container,
                        "createdb", "-U", self.test_env["TG16_PG_USER"], database])
                    self.test_env = dict(self.test_env, TG16_PG_DB=database)
                    config, env = self.consumer_config(suite)
                events = self.out / (suite + "-events.jsonl")
                bootstrap = ("ok=textguard_business_fixture:consumer_bootstrap(),"
                    "Boot=eunit_runner:eunit_setup(),"
                    "true=lists:member(Boot,[{app_started,imboy},{app_already_started,imboy}]),")
                if suite in STANDALONE_CONSUMERS:
                    bootstrap = ""
                expression = ("ok=file:set_cwd(" + module.erlang_string(self.snapshot) + "),"
                    "Expected=" + module.erlang_string(self.build / "eunit_runner.beam") +
                    ",Expected=code:which(eunit_runner),"
                    + bootstrap +
                    "Result=eunit:test(" + suite + ",[verbose,{report,{tg_leaf_listener,[{path," +
                    module.erlang_string(events) + "},{suite,<<\"" + suite + "\">>}]}}]),"
                    "application:stop(imboy),case Result of ok->halt(0);_->halt(1) end.")
                _, code = self.command("consumer-" + suite, ["erl", "-noinput", "-boot", "no_dot_erlang",
                    "-noshell", "-config", str(config), "-pa", str(application_root / "ebin"),
                    "-pa", str(self.build), "-eval", expression], env=env, require=False)
                records = [json.loads(line) for line in events.read_text().splitlines()] if events.exists() else []
                groups = {kind: [row for row in records if row["event"] == kind]
                          for kind in ["discovered", "executed", "cancelled"]}
                for kind, rows in groups.items():
                    base.save(self.out / (suite + "-" + kind + ".json"), rows)
                discovered = [row["id"] for row in groups["discovered"]]
                executed = [row["id"] for row in groups["executed"]]
                outcomes = collections.Counter(row["outcome"] for row in groups["executed"])
                summary = {"exit_code": code, "discovered": len(discovered), "executed": len(executed),
                    "cancelled": len(groups["cancelled"]), "outcomes": dict(outcomes),
                    "same_leaf_ids": discovered == executed, "static_expected_count": None,
                    "passed": code == 0 and bool(discovered) and discovered == executed and
                        len({tuple(value) for value in executed}) == len(executed) and
                        outcomes["passed"] == len(executed) and not groups["cancelled"] and
                        any(row["event"] == "summary" for row in records)}
                self.consumer_summaries[suite] = summary
                base.save(self.out / (suite + "-summary.json"), summary)
                if not summary["passed"]:
                    raise RuntimeError("targeted consumer failed: " + suite)

        def finish(self, error, cleanup_ok):
            code = super().finish(error, cleanup_ok)
            inherited = json.loads((self.out / "dual-protocol.json").read_text())
            after = {name: sha(self.root / name) for name in files}
            formal_tests = {name: sha(self.root / name) for name in BUSINESS_SOURCES}
            before_path = self.out / "t20-source-freeze-before.json"
            initial = json.loads(before_path.read_text()) if before_path.exists() else {}
            unchanged = after == files and formal_tests == initial.get("formal_test_sha256")
            consumers = getattr(self, "consumer_summaries", {})
            consumers_ok = len(consumers) == len(CONSUMER_SOURCES) and all(
                item["passed"] for item in consumers.values())
            markers = [value.encode() for value in self.redactions]
            markers.append(b"TG20-CANARY-NO-LOG")
            logs = sorted(set(self.out.rglob("*.log")) | set(self.out.rglob("*.dump")))
            findings = []
            for path in logs:
                content = path.read_bytes()
                path.chmod(0o600)
                matches = sum(content.count(marker) for marker in markers if marker)
                if matches:
                    findings.append({"path": str(path.relative_to(self.out)), "matches": matches,
                                     "sha256": hashlib.sha256(content).hexdigest()})
            privacy = {"scanned_files": len(logs), "findings": findings,
                       "passed": bool(logs) and not findings, "secret_values_recorded": False}
            base.save(self.out / "operational-log-privacy.json", privacy)
            final_code = code if unchanged and consumers_ok and privacy["passed"] else 1
            base.save(self.out / "verification-final.json", {
                "status": "PASS_" + TASK + "_SCOPED" if final_code == 0 else "FAILED_OR_BLOCKED", "exit_code": final_code,
                "backend_sha": args.expected_sha, "backend_tree": args.expected_tree,
                "rust_sha": args.rust_sha, "rust_tree": args.rust_tree,
                "freeze_unchanged": after == files, "formal_tests_unchanged": unchanged,
                "cleanup_ok": cleanup_ok, "suite": self.summaries.get(SUITE),
                "execution": self.execution, "error": error, "source_write": False,
                "ac_results": inherited["ac_results"],
                "source_unchanged": inherited["source_unchanged"],
                "rust_source_unchanged": inherited["rust_source_unchanged"],
                "shared_caches_unchanged": inherited["shared_caches_unchanged"],
                "dependency_proof": inherited["dependency_proof"],
                "commands": inherited["commands"],
                "consumer_summaries": consumers,
                "operational_log_privacy": privacy,
                "task": TASK, "automated_task_acceptance_passed": final_code == 0,
                "full_product_acceptance": False,
                "not_certified": ["full EUnit", "external push delivery", "full migration chain",
                                  "T22 and later", "wire receive memory bound"]})
            return final_code

    run = Run(args)
    base.save(run.out / "worker-start.json", {
        "pid": os.getpid(), "ppid": os.getppid(), "argv": [sys.executable, *sys.argv],
        "started_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "driver_sha256": sha(Path(__file__)), "source_write": False,
        "log": str(args.worker_log.resolve()), "status": "STARTED_BEFORE_BUILD"})
    def interrupted(signum, frame):
        raise RuntimeError("owned worker interrupted: " + str(signum))
    signal.signal(signal.SIGTERM, interrupted)
    signal.signal(signal.SIGINT, interrupted)
    error = None
    try:
        run.preflight()
        run.prepare()
        run.execute()
    except BaseException as failure:
        error = type(failure).__name__ + ": " + str(failure)
        print(error, file=sys.stderr, flush=True)
    finally:
        try:
            cleanup_ok = run.cleanup()
        except Exception as failure:
            cleanup_ok = False
            error = (error or "") + "; cleanup: " + type(failure).__name__
    if run.identity_before is None or run.rust_before is None:
        base.save(run.out / "verification-final.json", {"status": "FAILED_OR_BLOCKED",
            "error": error, "cleanup_ok": cleanup_ok, "exit_code": 1})
        code = 1
    else:
        code = run.finish(error, cleanup_ok)
    base.save(run.out / "worker-terminal.json", {"pid": os.getpid(), "exit_code": code,
        "cleanup_ok": cleanup_ok, "status": "FINISHED", "automatic_rerun": False})
    return code


if __name__ == "__main__":
    raise SystemExit(main())
