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
    def run_build(self, fail=False):
        with tempfile.TemporaryDirectory() as directory:
            work = Path(directory)
            # Match the failing runner's path depth and exceed Linux MAX_ARG_STRLEN.
            files = ["/home/runner/work/imboy/imboy/candidate/" + str(p.relative_to(ROOT))
                     for p in sorted((ROOT / "test").rglob("*.erl"))]
            self.assertGreater(len(" ".join(files + [Path(p).name for p in files])), 131072)
            makefile = (ROOT / "Makefile").read_text()
            start = makefile.index("define compile_test_erl\n")
            macro = makefile[start:makefile.index("endef", start) + len("endef")]
            (work / "Makefile").write_text(
                "ERLANG_MK_TMP := .\nPROJECT := fixture\nTEST_DIR := test\n"
                "TEST_ERLC_OPTS := +debug_info -DTEST=1\nverbose := @\n" + macro +
                "\n.PHONY: all\nall:\n\t$(call compile_test_erl," + " ".join(files) +
                ") && touch success\n")
            compiler = work / "erlc"
            compiler.write_text(
                "#!/usr/bin/env python3\nimport json,os,sys\n"
                "with open('calls.jsonl','a') as f: f.write(json.dumps(sys.argv[1:])+'\\n')\n"
                "sys.exit(1 if os.environ.get('FAIL_SOURCE') in sys.argv[1:] else 0)\n")
            compiler.chmod(0o755)
            env = dict(os.environ, PATH=str(work) + os.pathsep + os.environ["PATH"])
            if fail:
                env["FAIL_SOURCE"] = files[0]
            else:
                env.pop("FAIL_SOURCE", None)
            result = subprocess.run(["make", "--no-print-directory", "all"], cwd=work,
                                    env=env, capture_output=True, text=True)
            calls = [json.loads(row) for row in (work / "calls.jsonl").read_text().splitlines()]
            self.assertGreater(len(calls), 1)
            actual = []
            for args in calls:
                self.assertEqual(args[:9], ["-v", "+debug_info", "-DTEST=1", "-o", "test",
                                           "-pa", "ebin/", "-I", "include/"])
                self.assertLessEqual(len(args[9:]), 64)
                actual.extend(args[9:])
            self.assertEqual(actual, files)
            self.assertEqual((work / "success").exists(), not fail)
            self.assertEqual(result.returncode == 0, not fail, result.stderr)

    def test_complete_source_set_and_options(self):
        self.run_build()

    def test_compiler_failure_does_not_stamp_success(self):
        self.run_build(fail=True)


if __name__ == "__main__":
    unittest.main()
