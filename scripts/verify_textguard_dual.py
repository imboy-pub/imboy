"""T19 isolated PG/OTP/Rust integration launcher.

The real channel uses grpcbox client.channels and the production default bridge.
Check and publication use separate loopback ports and task-owned secret files.
This suite does not establish a wire-buffer bound: locked grpcbox buffers bytes
before the production 256 KiB decode check. That remains a T23 risk.
"""

import argparse
import datetime
import hashlib
import importlib.util
import json
import os
from pathlib import Path
import secrets
import shutil
import signal
import socket
import subprocess
import sys
import tarfile
import time
import urllib.error
import urllib.request


sys.dont_write_bytecode = True
BACKEND_SHA = "fa72f33d4b954cb28e1e51c85dc31d33dab8fc45"
BACKEND_TREE = "b0ca75b144038567803983ec592c4163ef90fc72"
RUST_SHA = "7c8d981b457e0e59af3c508db52d8d094d84c95f"
RUST_TREE = "62f0003fdae8b5008672a9c7c43dbff5881daecf"
SUITE = "textguard_integration_tests"
ACCEPTANCE = ["AC-060", "AC-061", "AC-076", "AC-077", "AC-115"]


def file_sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args], text=True).strip()


def source_snapshot(root):
    paths = git(root, "ls-files", "-z").split("\0")
    paths += [str(path.relative_to(root)) for path in (root / "test/support").glob("textguard_*")]
    paths += ["test/lib/textguard_integration_tests.erl", "scripts/verify_textguard_dual.py",
              "src/textguard.proto", "src/moderation/moderation_detector_grpcbox.erl",
              "priv/textguard/transport.example.config"]
    return {name: file_sha(root / name) for name in sorted(set(paths)) if name and (root / name).is_file()}


def load_base(path):
    spec = importlib.util.spec_from_file_location("tg19_owned_lifecycle", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.SUITES = {SUITE: 1}
    return module


def erlang_string(value):
    return json.dumps(str(value))


def run_type(base):
    class Run(base.Run):
        def __init__(self, args):
            super().__init__(args)
            self.server = None
            self.server_log = None
            self.tests = []
            self.execution = None
            self.rust = args.rust_cwd.resolve()
            self.rust_source = self.out / "rust-source"
            self.target = self.out / "rust-target"
            self.production_before = None
            self.rust_before = None
            self.dependency_proof = None
            self.clearance = args.resource_clearance
            self.liveness = None
            self.proto_proof = None
            self.raw_privacy = []
            base.save(self.out / "launcher-invocation.json", {
                "argv": [sys.executable, *sys.argv], "cwd": str(Path.cwd()),
                "launcher_sha256": file_sha(Path(__file__).resolve()),
            })

        def private_output(self, name, raw):
            markers = [value.encode() for value in self.redactions]
            markers.append(b"TG20-CANARY-NO-LOG")
            matches = sum(raw.count(marker) for marker in markers if marker)
            self.raw_privacy.append({"source": name, "bytes": len(raw), "matches": matches,
                                     "raw_sha256": hashlib.sha256(raw).hexdigest()})
            for secret in self.redactions:
                raw = raw.replace(secret.encode(), b"<REDACTED>")
            return raw

        def private_log(self, name, path):
            path.write_bytes(self.private_output(name, path.read_bytes()))
            path.chmod(0o600)

        def command(self, name, argv, *, env=None, timeout=900, require=True):
            print(name, flush=True)
            record = {"argv": list(map(str, argv)), "cwd": str(self.root),
                      "started_at": datetime.datetime.now(datetime.timezone.utc).isoformat()}
            process = subprocess.Popen(record["argv"], cwd=self.root, env=env or self.env,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True)
            try:
                raw, _ = process.communicate(timeout=timeout)
                record["exit_code"] = process.returncode
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                raw, _ = process.communicate()
                record.update(exit_code=None, timed_out=True, timeout_seconds=timeout)
            except BaseException:
                os.killpg(process.pid, signal.SIGKILL)
                raw, _ = process.communicate()
                self.private_output(name, raw)
                raise
            raw = self.private_output(name, raw)
            log = self.out / (name + ".log")
            descriptor = os.open(log, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
            with os.fdopen(descriptor, "wb") as target:
                target.write(raw)
            record.update(finished_at=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                          log=str(log), log_sha256=file_sha(log))
            self.commands.append(record)
            base.save(self.out / (name + ".json"), record)
            if require and record["exit_code"] != 0:
                raise RuntimeError(name + " failed: " + str(record["exit_code"]))
            return raw.decode("utf-8", errors="replace").strip(), record["exit_code"]

        def identity(self):
            return {
                "head": git(self.root, "rev-parse", "HEAD"),
                "tree": git(self.root, "rev-parse", "HEAD^{tree}"),
                "status": git(self.root, "status", "--porcelain"),
                "source_sha256": source_snapshot(self.root),
            }

        def preflight(self):
            if git(self.root, "rev-parse", "--show-toplevel") != str(self.root):
                raise RuntimeError("backend must be an actual independent worktree")
            self.identity_before = self.identity()
            if self.identity_before["head"] != self.args.expected_sha or self.identity_before["tree"] != self.args.expected_tree:
                raise RuntimeError("backend candidate identity differs")
            if git(self.rust, "rev-parse", "HEAD") != self.args.rust_sha or git(self.rust, "rev-parse", "HEAD^{tree}") != self.args.rust_tree:
                raise RuntimeError("Rust candidate identity differs")
            if git(self.rust, "status", "--porcelain"):
                raise RuntimeError("Rust candidate has unreviewed source changes")
            self.rust_before = {name: file_sha(self.rust / name) for name in git(self.rust, "ls-files", "-z").split("\0")
                                if name and (self.rust / name).is_file()}
            names = [str(path.relative_to(self.root)) for path in (self.root / "src/moderation").glob("*.erl")
                     if path.name.startswith(("moderation_detector", "textguard"))]
            self.manifest = {"source_sha256": {name: file_sha(self.root / name) for name in names}}
            self.production_before = dict(self.manifest["source_sha256"])
            self.check_proto_contract()
            base.save(self.out / "candidate-before.json", self.identity_before)
            base.save(self.out / "rust-candidate.json", {
                "cwd": str(self.rust), "sha": self.args.rust_sha, "tree": self.args.rust_tree,
                "source_sha256": self.rust_before,
            })
            base.save(self.out / "resource-clearance.json", {
                "clearance": self.clearance, "mode": "prepare" if self.args.prepare_only else "execute",
                "performance_test": False, "exclusive_build_and_vm": True,
            })
            for tool in ["erl", "erlc", "make", "docker", "cargo", "protoc"]:
                if not shutil.which(tool, path=self.env["PATH"]):
                    raise RuntimeError("missing tool: " + tool)
            self.command("otp-version", ["erl", "-noinput", "-boot", "no_dot_erlang", "-noshell", "-eval",
                'io:format("OTP ~s ERTS ~s~n",[erlang:system_info(otp_release),erlang:system_info(version)]),halt().'])

        def prepare(self):
            self.prepare_backend()
            self.prepare_rust()
            self.compile_tests()

        def check_proto_contract(self):
            backend = self.root / "src/textguard.proto"
            rust = self.rust / "proto/textguard.proto"
            hashes = {"backend": file_sha(backend), "rust": file_sha(rust)}
            base.save(self.out / "proto-contract.json", {
                "backend_path": str(backend), "rust_path": str(rust),
                "sha256": hashes, "identical": hashes["backend"] == hashes["rust"],
            })
            if hashes["backend"] != hashes["rust"]:
                raise RuntimeError("production and Rust proto differ")

        def prepare_backend(self, target="compile"):
            cache = self.args.cache_source.resolve()
            inputs = {name: {"candidate": file_sha(self.root / name), "cached": file_sha(cache / name)}
                      for name in base.BUILD_INPUTS}
            base.save(self.out / "build-inputs.json", inputs)
            if any(values["candidate"] != values["cached"] for name, values in inputs.items() if name != "Makefile"):
                raise RuntimeError("locked dependency/build inputs differ from cache")
            def without_proto_generator(path):
                before, marker, remaining = path.read_text().partition("define compile_proto.erl\n")
                _generator, end, after = remaining.partition("\nendef")
                if not marker or not end:
                    raise RuntimeError("missing production protobuf Makefile generator")
                return before + after
            if without_proto_generator(self.root / "Makefile") != without_proto_generator(cache / "Makefile"):
                raise RuntimeError("cache Makefile differs outside the protobuf generator")
            self.shared_before = {name: base.inventory(cache / name) for name in ["deps", "ebin", ".erlang.mk"]}
            base.save(self.out / "shared-cache-before.json", self.shared_before)
            archive = self.out / "backend-source.tar"
            self.command("backend-source-archive", ["git", "archive", "--format=tar",
                         "--output=" + str(archive), self.args.expected_sha])
            self.snapshot.mkdir(mode=0o700)
            with tarfile.open(archive) as source:
                for member in source.getmembers():
                    if not (self.snapshot / member.name).resolve().is_relative_to(self.snapshot):
                        raise RuntimeError("unsafe backend archive")
                    if member.issym() or member.islnk():
                        anchor = (self.snapshot / member.name).parent if member.issym() else self.snapshot
                        if not (anchor / member.linkname).resolve().is_relative_to(self.snapshot):
                            raise RuntimeError("backend archive link escapes private snapshot")
                    elif not (member.isfile() or member.isdir()):
                        raise RuntimeError("unsupported backend archive member")
                source.extractall(self.snapshot)
            for name, expected in self.identity_before["source_sha256"].items():
                source = self.root / name
                if file_sha(source) != expected:
                    raise RuntimeError("candidate changed while copying: " + name)
                destination = self.snapshot / name
                destination.parent.mkdir(parents=True, exist_ok=True)
                if source.is_symlink():
                    if not destination.is_symlink() or os.readlink(source) != os.readlink(destination):
                        raise RuntimeError("candidate symlink differs from archive: " + name)
                else:
                    shutil.copy2(source, destination)
            for name in ["deps", ".erlang.mk"]:
                shutil.copytree(cache / name, self.snapshot / name, symlinks=True)
            for path in self.snapshot.rglob("*"):
                if path.is_symlink() and not path.resolve().is_relative_to(self.snapshot):
                    raise RuntimeError("private cache link escapes snapshot")
            generated = [self.snapshot / name for name in
                         ["src/textguard_pb.erl", "include/textguard_pb.hrl", "ebin/textguard_pb.beam"]]
            if any(path.exists() for path in generated):
                raise RuntimeError("production protobuf must be generated fresh by Makefile")
            base.save(self.out / "production-protobuf-before.json", {
                "generated_paths": [str(path) for path in generated],
                "absent_before": True, "make_target": target,
            })
            self.build.mkdir()
            self.command("native-make-" + target, ["make", "-C", str(self.snapshot), target],
                         env=dict(self.env, ERL_LIBS=str(self.snapshot / "deps")))
            if any(file_sha(self.snapshot / name) != expected
                   for name, expected in self.identity_before["source_sha256"].items()):
                raise RuntimeError("native build changed candidate source")
            self.proto_proof = {
                "generator": "production Makefile via erlang.mk", "target": target,
                "absent_before": True, "makefile_sha256": file_sha(self.snapshot / "Makefile"),
                "erlang_mk_sha256": file_sha(self.snapshot / "erlang.mk"),
                "proto_sha256": file_sha(self.snapshot / "src/textguard.proto"),
                "cache_source": str(cache), "cache_scope": "private copy, identical locks and build declarations",
                "gpb_compiler_sha256": file_sha(self.snapshot / "deps/gpb/ebin/gpb_compile.beam"),
                "generated_source_sha256": file_sha(generated[0]),
            }
            if target == "compile":
                self.proto_proof["beam_sha256"] = file_sha(generated[2])
            base.save(self.out / "production-protobuf.json", self.proto_proof)

        def prepare_rust(self):
            archive = self.out / "rust-source.tar"
            self.command("rust-source-archive", ["git", "-C", str(self.rust), "archive", "--format=tar",
                         "--output=" + str(archive), self.args.rust_sha])
            self.rust_source.mkdir()
            with tarfile.open(archive) as source:
                for member in source.getmembers():
                    if not (self.rust_source / member.name).resolve().is_relative_to(self.rust_source):
                        raise RuntimeError("unsafe source archive")
                    if member.issym() or member.islnk():
                        raise RuntimeError("Rust source symlink requires explicit review")
                source.extractall(self.rust_source)
            home = self.out / "cargo-home"
            home.mkdir()
            shutil.copytree(self.args.cargo_cache.resolve() / "registry", home / "registry")
            self.rust_env = dict(self.env, CARGO_HOME=str(home), RUSTUP_HOME=str(self.args.rustup_home.resolve()),
                                 RUSTUP_TOOLCHAIN="1.90.0", CARGO_TARGET_DIR=str(self.target), CARGO_NET_OFFLINE="true")
            self.command("cargo-version", ["cargo", "--version"], env=self.rust_env)
            self.command("protoc-version", ["protoc", "--version"])
            self.command("rust-build", ["cargo", "build", "--locked", "--offline", "--manifest-path",
                         str(self.rust_source / "Cargo.toml"), "--bin", "imboy-textguard"], env=self.rust_env)
            raw, _ = self.command("cargo-metadata", ["cargo", "metadata", "--locked", "--offline", "--format-version", "1",
                         "--manifest-path", str(self.rust_source / "Cargo.toml")], env=self.rust_env)
            metadata = json.loads(raw)
            packages = [package for package in metadata["packages"] if package["name"] == "aho-corasick"]
            graph_ids = {node["id"] for node in metadata["resolve"]["nodes"]}
            proven = bool(packages) and all(package["id"] in graph_ids and
                package.get("source") == "registry+https://github.com/rust-lang/crates.io-index" for package in packages)
            for index, package in enumerate(packages):
                self.command("aho-inverse-" + str(index), ["cargo", "tree", "--locked", "--offline", "--manifest-path",
                    str(self.rust_source / "Cargo.toml"), "--invert", "aho-corasick@" + package["version"]], env=self.rust_env)
            self.command("cargo-duplicates", ["cargo", "tree", "--locked", "--offline", "--manifest-path",
                         str(self.rust_source / "Cargo.toml"), "--duplicates"], env=self.rust_env)
            self.command("cargo-all-edges", ["cargo", "tree", "--locked", "--offline", "--manifest-path",
                         str(self.rust_source / "Cargo.toml"), "--edges", "all", "--prefix", "none"], env=self.rust_env)
            configs = []
            for parent in [self.root, *self.root.parents]:
                for name in ["config", "config.toml"]:
                    path = parent / ".cargo" / name
                    if path.is_file():
                        configs.append({"path": str(path), "sha256": file_sha(path)})
            for name in ["config", "config.toml"]:
                path = home / name
                if path.is_file():
                    configs.append({"path": str(path), "sha256": file_sha(path)})
            self.dependency_proof = {
                "mode": "default_registry", "registry_proven": proven and not configs,
                "aho_packages": [{key: package.get(key) for key in ["id", "version", "source", "manifest_path"]} for package in packages],
                "cargo_lock_sha256": file_sha(self.rust_source / "Cargo.lock"),
                "cargo_toml_sha256": file_sha(self.rust_source / "Cargo.toml"),
                "effective_config_files": configs, "isolated_cargo_home": str(home),
                "actual_cargo_cwd": str(self.root), "toolchain": "1.90.0",
                "local_patch_validation": False, "shared_cache_write": False,
            }
            base.save(self.out / "resolved-dependencies.json", self.dependency_proof)

        def compile_tests(self):
            env = dict(self.env, ERL_LIBS=str(self.snapshot / "deps"))
            proto = self.snapshot / "src/textguard.proto"
            sources = [self.root / "test/lib/textguard_integration_tests.erl", self.root / "test/lib/textguard_export_tests.erl",
                       self.root / "test/common/ewh_http_fixture.erl", self.args.base_launcher.resolve().parent / "tg_leaf_listener.erl"]
            sources += sorted((self.root / "test/support").glob("textguard_*.erl"))
            self.command("compile-t19", ["erlc", "+debug_info", "-DTEST=1", "-I", str(self.root / "include"),
                "-pa", str(self.snapshot / "ebin"), "-o", str(self.build), *map(str, sources)], env=env)
            if (self.build / "textguard_pb.beam").exists():
                raise RuntimeError("test build shadows production protobuf")
            expression = ('Expected=' + erlang_string(self.snapshot / "ebin/textguard_pb.beam") +
                ',Expected=code:which(textguard_pb),{module,textguard_pb}=code:ensure_loaded(textguard_pb),'
                'io:format("~s",[jsone:encode(#{loaded_from=>list_to_binary(Expected)})]),halt().')
            raw, _ = self.command("production-protobuf-loaded", ["erl", "-noinput", "-boot", "no_dot_erlang", "-noshell",
                "-pa", str(self.snapshot / "ebin"), "-pa", str(self.build), "-eval", expression], env=env)
            self.proto_proof = dict(self.proto_proof, **json.loads(raw))
            raw, _ = self.command("discover-declared-cases", ["erl", "-noinput", "-boot", "no_dot_erlang", "-noshell",
                "-pa", str(self.snapshot / "ebin"), "-pa", str(self.build), "-eval",
                'Cases=[maps:with([id,acceptance],Case)||Case<-textguard_integration_tests:manifest()],io:format("~s",[jsone:encode(Cases)]),halt().'], env=env)
            self.tests = json.loads(raw)
            if not self.tests or len({case["id"] for case in self.tests}) != len(self.tests):
                raise RuntimeError("empty or duplicated declared leaves")
            base.save(self.out / "declared-cases.json", self.tests)
            base.save(self.out / "prepared.json", {
                "status": "PREPARED_NOT_EXECUTED", "backend_sha": self.args.expected_sha,
                "rust_sha": self.args.rust_sha, "production_sha256": self.production_before,
                "binary_sha256": file_sha(self.target / "debug/imboy-textguard"),
                "proto_sha256": file_sha(proto), "declared_leaf_count": len(self.tests),
                "test_source_sha256": {str(path): file_sha(path) for path in sources},
                "base_launcher_sha256": file_sha(self.args.base_launcher.resolve()),
                "production_protobuf": self.proto_proof,
            })

        def start_service(self):
            self.start_pg()
            for name in ["check", "publish"]:
                value = secrets.token_urlsafe(48)
                self.redactions.append(value)
                path = self.credentials / (name + ".secret")
                descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
                with os.fdopen(descriptor, "w") as output:
                    output.write(value)
            reservations = []
            for _ in range(4):
                handle = socket.socket()
                handle.bind(("127.0.0.1", 0))
                reservations.append(handle)
            ports = [handle.getsockname()[1] for handle in reservations]
            keys = ["TEXTGUARD_HTTP_ADDR", "TEXTGUARD_GRPC_ADDR", "TEXTGUARD_MANAGE_ADDR"]
            runtime = {key: "127.0.0.1:" + str(port) for key, port in zip(keys, ports)}
            runtime.update(TEXTGUARD_DATA_DIR=str(self.out / "service-data"),
                TEXTGUARD_CHECK_SECRET_FILE=str(self.credentials / "check.secret"),
                TEXTGUARD_PUBLISH_SECRET_FILE=str(self.credentials / "publish.secret"),
                TEXTGUARD_T2S_TABLE_ID="test-t2s-v1")
            env = dict(self.env, **runtime)
            for handle in reservations:
                handle.close()
            self.server_log = (self.out / "server.log").open("xb")
            argv = [str(self.target / "debug/imboy-textguard")]
            self.server = subprocess.Popen(argv, cwd=self.root, env=env, stdout=self.server_log,
                                           stderr=subprocess.STDOUT, start_new_session=True)
            base.save(self.out / "server-owned.json", {"argv": argv, "cwd": str(self.root), "pid": self.server.pid,
                "process_group": self.server.pid, "environment_keys": sorted(runtime), "ports": ports,
                "rust_sha": self.args.rust_sha, "binary_sha256": file_sha(Path(argv[0]))})
            url = "http://127.0.0.1:" + str(ports[0])
            self.wait_for_liveness(url)
            self.test_env.update(runtime, TG19_HTTP_URL=url, TG19_MANAGE_URL="http://127.0.0.1:" + str(ports[2]),
                TG19_GRPC_PORT=str(ports[1]), TG19_FAULT_GRPC_PORT=str(ports[3]), TG19_EVIDENCE=str(self.out),
                TG19_ENH_FILE=str(self.out / "enhancement.json"))

        def wait_for_liveness(self, url):
            opener = urllib.request.build_opener(urllib.request.ProxyHandler({}))
            self.liveness = {
                "method": "GET", "url": url + "/health/live", "proxy_policy": "explicit_direct",
                "timeout_seconds": 0.2, "max_attempts": 50, "passed": False, "attempts": [],
            }
            try:
                for number in range(1, 51):
                    record = {"attempt": number, "started_at_unix_ns": time.time_ns()}
                    started = time.monotonic()
                    try:
                        server_exit = self.server.poll()
                        if server_exit is not None:
                            record["server_exit"] = server_exit
                            raise RuntimeError("real Rust server exited during startup")
                        with opener.open(self.liveness["url"], timeout=0.2) as response:
                            record["http_status"] = response.status
                            if response.status == 200:
                                self.liveness["passed"] = True
                                return
                    except urllib.error.HTTPError as failure:
                        record.update(error_type=type(failure).__name__, http_status=failure.code)
                        failure.close()
                    except (urllib.error.URLError, OSError) as failure:
                        reason = failure.reason if isinstance(failure, urllib.error.URLError) else failure
                        record.update(error_type=type(failure).__name__, reason_type=type(reason).__name__)
                        if isinstance(reason, OSError) and isinstance(reason.errno, int):
                            record["errno"] = reason.errno
                    finally:
                        record["elapsed_ms"] = round((time.monotonic() - started) * 1000, 3)
                        self.liveness["attempts"].append(record)
                    time.sleep(0.1)
                raise RuntimeError("real Rust liveness startup exhausted")
            finally:
                base.save(self.out / "server-liveness.json", self.liveness)

        def execute(self):
            self.start_service()
            passed = self.execute_suite(SUITE, len(self.tests))
            rows_path = self.out / "case-results.jsonl"
            rows = [json.loads(line) for line in rows_path.read_text().splitlines()] if rows_path.exists() else []
            declared = [case["id"] for case in self.tests]
            executed = [row["id"] for row in rows]
            if declared != executed or len(set(executed)) != len(executed):
                passed = False
            self.execution = {"passed": passed, "cases": rows,
                              "suite": self.summaries.get(SUITE), "declared_equals_executed": declared == executed}

        def cleanup(self):
            receipt = {"server_stopped": self.server is None, "server_exit": None, "signals": [], "errors": []}
            if self.server:
                try:
                    if self.server.poll() is None:
                        os.killpg(self.server.pid, signal.SIGTERM)
                        receipt["signals"].append("SIGTERM")
                        self.server.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    try:
                        os.killpg(self.server.pid, signal.SIGKILL)
                        receipt["signals"].append("SIGKILL")
                        self.server.wait(timeout=5)
                    except Exception as failure:
                        receipt["errors"].append("server-kill: " + type(failure).__name__)
                except Exception as failure:
                    receipt["errors"].append("server-stop: " + type(failure).__name__)
                receipt.update(server_stopped=self.server.poll() is not None, server_exit=self.server.returncode)
            if self.server_log:
                try:
                    self.server_log.close()
                    path = self.out / "server.log"
                    self.private_log("rust-server", path)
                except Exception as failure:
                    receipt["errors"].append("server-log: " + type(failure).__name__)
            try:
                parent_ok = super().cleanup()
            except Exception as failure:
                parent_ok = False
                receipt["errors"].append("base-cleanup: " + type(failure).__name__)
                if self.credentials:
                    try:
                        shutil.rmtree(self.credentials, ignore_errors=False)
                    except FileNotFoundError:
                        pass
                    except Exception as recovery:
                        receipt["errors"].append("credentials: " + type(recovery).__name__)
                try:
                    if self.tmp_reserved:
                        path = base.TMP_FIXTURE
                        if path.is_symlink() or (path.exists() and file_sha(path) != base.KNOWN_TMP_SHA):
                            raise RuntimeError("foreign temporary fixture preserved")
                        if self.tmp_prior is None:
                            path.unlink(missing_ok=True)
                        else:
                            path.write_bytes(self.tmp_prior["bytes"])
                            os.chmod(path, self.tmp_prior["mode"])
                            os.utime(path, ns=(self.tmp_prior["atime_ns"], self.tmp_prior["mtime_ns"]))
                    if self.tmp_lock_acquired:
                        self.tmp_lock.rmdir()
                except Exception as recovery:
                    receipt["errors"].append("temporary-fixture: " + type(recovery).__name__)
            base.save(self.out / "server-cleanup.json", receipt)
            return parent_ok and receipt["server_stopped"] and not receipt["errors"]

        def finish(self, error, cleanup_ok):
            after = self.identity()
            base.save(self.out / "candidate-after.json", after)
            unchanged = after == self.identity_before
            rust_after = {name: file_sha(self.rust / name) for name in self.rust_before or {}
                          if (self.rust / name).is_file()}
            rust_unchanged = rust_after == self.rust_before
            shared_unchanged = False
            if self.shared_before is not None:
                shared_after = {name: base.inventory(self.args.cache_source.resolve() / name) for name in self.shared_before}
                base.save(self.out / "shared-cache-after.json", shared_after)
                shared_unchanged = self.shared_before == shared_after
            components = {}
            for ac in ACCEPTANCE:
                rows = [row for row in (self.execution or {}).get("cases", []) if ac in row["acceptance"]]
                ok = bool(rows) and all(row["outcome"] == "passed" for row in rows)
                if ac == "AC-115":
                    ok = bool(self.execution and self.execution["passed"] and self.dependency_proof and self.dependency_proof["registry_proven"])
                components[ac] = {"status": "PASS" if ok else "NOT_EXECUTED" if not self.execution else "FAILED_OR_BLOCKED",
                                  "executed_leaves": len(rows), "case_ids": [row["id"] for row in rows]}
            privacy_ok = bool(self.raw_privacy) and all(row["matches"] == 0 for row in self.raw_privacy)
            base.save(self.out / "raw-output-privacy.json", {"checks": self.raw_privacy,
                "passed": privacy_ok, "checked_before_redaction": True,
                "secret_values_recorded": False})
            prepared = self.args.prepare_only and error is None and cleanup_ok and unchanged and shared_unchanged and rust_unchanged and privacy_ok
            passed = bool(not self.args.prepare_only and self.execution and self.execution["passed"] and error is None
                          and cleanup_ok and unchanged and shared_unchanged and rust_unchanged and privacy_ok
                          and all(value["status"] == "PASS" for value in components.values()))
            status = "PASS" if passed else "PREPARED_NOT_EXECUTED" if prepared else "FAILED_OR_BLOCKED"
            base.save(self.out / "dual-protocol.json", {
                "status": status, "error": error, "backend_sha": self.args.expected_sha, "backend_tree": self.args.expected_tree,
                "rust_sha": self.args.rust_sha, "rust_tree": self.args.rust_tree, "cwd": str(self.root),
                "declared_leaves": len(self.tests), "execution": self.execution, "ac_results": components,
                "source_unchanged": unchanged, "rust_source_unchanged": rust_unchanged,
                "shared_caches_unchanged": shared_unchanged, "cleanup_ok": cleanup_ok,
                "dependency_proof": self.dependency_proof, "commands": self.commands,
                "raw_output_privacy_passed": privacy_ok,
                "startup_probe": self.liveness,
                "production_protobuf": self.proto_proof,
                "remaining_risks": ["T23: grpcbox wire receive buffer has no proven pre-buffer size limit; "
                                    "the production 256 KiB pre-decode check does not bound wire buffering"],
                "not_performed": ["commit", "push", "production", "full_eunit", "performance_load", "shared_state_write"],
            })
            return 0 if passed or prepared else 1

    return Run


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--cwd", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--rust-cwd", type=Path, required=True)
    parser.add_argument("--cache-source", type=Path, required=True)
    parser.add_argument("--base-launcher", type=Path, required=True)
    parser.add_argument("--cargo-cache", type=Path, required=True)
    parser.add_argument("--rustup-home", type=Path, required=True)
    parser.add_argument("--resource-clearance", required=True)
    parser.add_argument("--expected-sha", default=BACKEND_SHA)
    parser.add_argument("--expected-tree", default=BACKEND_TREE)
    parser.add_argument("--rust-sha", default=RUST_SHA)
    parser.add_argument("--rust-tree", default=RUST_TREE)
    parser.add_argument("--prepare-only", action="store_true")
    args = parser.parse_args()
    base = load_base(args.base_launcher.resolve())
    run = run_type(base)(args)
    error = None
    try:
        run.preflight()
        run.prepare()
        if not args.prepare_only:
            run.execute()
    except BaseException as failure:
        error = type(failure).__name__ + ": " + str(failure)
        print(error, file=sys.stderr)
    finally:
        try:
            cleanup_ok = run.cleanup()
        except Exception as failure:
            cleanup_ok = False
            error = (error or "") + "; cleanup: " + type(failure).__name__
    return run.finish(error, cleanup_ok)


if __name__ == "__main__":
    raise SystemExit(main())
