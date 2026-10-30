#!/usr/bin/env python3
"""Exercise the real Makefile override without requiring an Erlang toolchain."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class TestCompileBatches(unittest.TestCase):
    def run_build(self, fail=False, jobs=None, invalid=False):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            # Match the failing runner's path depth and exceed Linux MAX_ARG_STRLEN.
            files = ["/home/runner/work/imboy/imboy/candidate/" + str(p.relative_to(ROOT))
                     for p in sorted((ROOT / "test").rglob("*.erl"))]
            self.assertGreater(len(" ".join(files + [Path(p).name for p in files])), 131072)
            makefile = (ROOT / "Makefile").read_text()
            start = makefile.index("TEST_COMPILE_JOBS ?= 4\n")
            macro = makefile[start:makefile.index("endef", start) + len("endef")]
            (work / "Makefile").write_text(
                "ERLANG_MK_TMP := .\nPROJECT := fixture\nTEST_DIR := test\n"
                "TEST_ERLC_OPTS := +debug_info -DTEST=1\nverbose := @\n" + macro +
                "\n.PHONY: all\nall:\n\t$(call compile_test_erl," + " ".join(files) +
                ") && touch success\n")
            compiler = work / "erlc"
            (work / "started").mkdir()
            compiler.write_text(
                "#!/usr/bin/env python3\nimport json,os,sys,time,pathlib\n"
                "start=time.monotonic_ns()\n"
                "pathlib.Path('started',str(os.getpid())).touch()\n"
                "deadline=time.monotonic()+10\n"
                "while len(list(pathlib.Path('started').iterdir()))<int(os.environ['EXPECTED_JOBS']):\n"
                " if time.monotonic()>deadline: sys.exit(2)\n"
                " time.sleep(.005)\n"
                "time.sleep(.005)\n"
                "row={'args':sys.argv[1:],'start':start,'end':time.monotonic_ns()}\n"
                "with open('calls.jsonl','a') as f: f.write(json.dumps(row)+'\\n')\n"
                "sys.exit(1 if os.environ.get('FAIL_SOURCE') in sys.argv[1:] else 0)\n")
            compiler.chmod(0o755)
            env = dict(os.environ, PATH=str(work) + os.pathsep + os.environ["PATH"])
            env["EXPECTED_JOBS"] = str(jobs if jobs is not None else 4)
            if fail:
                env["FAIL_SOURCE"] = files[-1] if fail == "last" else files[0]
            else:
                env.pop("FAIL_SOURCE", None)
            command = ["make", "--no-print-directory", "all"]
            if jobs is not None:
                command.append("TEST_COMPILE_JOBS=" + str(jobs))
            result = subprocess.run(command, cwd=work,
                                    env=env, capture_output=True, text=True)
            log = work / "calls.jsonl"
            if invalid:
                self.assertNotEqual(result.returncode, 0)
                self.assertIn("TEST_COMPILE_JOBS must be", result.stderr)
                self.assertFalse(log.exists())
                self.assertFalse((work / "success").exists())
                return
            calls = [json.loads(row) for row in log.read_text().splitlines()]
            self.assertGreater(len(calls), 1)
            actual = []
            for call in calls:
                args = call["args"]
                self.assertEqual(args[:9], ["-v", "+debug_info", "-DTEST=1", "-o", "test",
                                           "-pa", "ebin/", "-I", "include/"])
                self.assertLessEqual(len(args[9:]), 64)
                actual.extend(args[9:])
            self.assertCountEqual(actual, files)
            if jobs == 1:
                self.assertEqual(actual, files)
            events = sorted([(call["start"], 1) for call in calls] +
                            [(call["end"], -1) for call in calls])
            active = peak = 0
            for _, delta in events:
                active += delta
                peak = max(peak, active)
            self.assertEqual(peak, int(env["EXPECTED_JOBS"]))
            self.assertEqual((work / "success").exists(), not fail)
            self.assertEqual(result.returncode == 0, not fail, result.stderr)

    def test_complete_source_set_and_options(self):
        self.run_build()

    def test_compiler_failure_does_not_stamp_success(self):
        self.run_build(fail=True)

    def test_late_parallel_failure_does_not_stamp_success(self):
        self.run_build(fail="last")

    def test_serial_override_preserves_complete_build(self):
        self.run_build(jobs=1)

    def test_invalid_or_unbounded_concurrency_is_rejected(self):
        for jobs in [0, 5, "", "1 2"]:
            with self.subTest(jobs=jobs):
                self.run_build(jobs=jobs, invalid=True)


if __name__ == "__main__":
    unittest.main()
