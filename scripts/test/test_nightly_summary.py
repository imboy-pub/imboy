import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
JOBS = ("full-eunit", "dialyze", "gradualize", "eqwalize", "elvis", "pg-behavior-harnesses")


def collect_script():
    """Read the workflow's actual shell block, without YAML dependencies."""
    lines = (ROOT / ".github/workflows/nightly.yml").read_text().splitlines()
    start = lines.index("      - name: Collect gate classifications")
    start = lines.index("        run: |", start) + 1
    body = []
    for line in lines[start:]:
        if line and not line.startswith("          "):
            break
        body.append(line[10:])
    return "\n".join(body) + "\n"


def classification(gate, new=0, known=0, fixed=0, flaky=0):
    verdict = "NEW_FAILURE" if new else "FLAKY" if flaky else "RATCHET" if fixed else "PASS"
    return (f"QA_CLASSIFY gate={gate} NEW={new} KNOWN={known} FIXED={fixed} "
            f"FLAKY={flaky} INFRA=0 VERDICT={verdict}")


class NightlySummaryTest(unittest.TestCase):
    def run_collect(self, summaries, fail_awk=False):
        with tempfile.TemporaryDirectory(prefix="imboy-nightly-summary-") as temporary:
            root = Path(temporary)
            output = root / "github-output"
            script = re.sub(
                r"\$\{\{\s*needs\.([a-z-]+)\.outputs\.classify\s*\}\}",
                lambda match: summaries.get(match.group(1), ""), collect_script(),
            )
            self.assertNotIn("${{", script)
            script = script.replace(
                "/tmp/qa-classify-all.txt", shlex.quote(str(root / "classifications"))
            )
            environment = {"PATH": os.environ["PATH"], "GITHUB_OUTPUT": str(output)}
            if fail_awk:
                # Fail gate-name extraction after the total count succeeds.
                # A blanket `|| true` would hide this genuine command failure.
                executable = root / "awk"
                executable.write_text(
                    '#!/bin/sh\nif [ -e "$AWK_CALL_STATE" ]; then exit 47; fi\n'
                    ': > "$AWK_CALL_STATE"\nexec ' + shlex.quote(shutil.which("awk")) + ' "$@"\n'
                )
                executable.chmod(0o755)
                environment.update(
                    PATH=str(root) + os.pathsep + environment["PATH"],
                    AWK_CALL_STATE=str(root / "awk-called"),
                )
            result = subprocess.run(
                ["bash", "-c", script], env=environment, text=True,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=10,
            )
            outputs = (
                dict(line.split("=", 1) for line in output.read_text().splitlines())
                if output.exists() else {}
            )
            return result, outputs

    def assert_summary(self, summaries, total, gates):
        result, outputs = self.run_collect(summaries)
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(outputs, {"total_new": str(total), "gates_new": gates})

    def test_all_gates_pass(self):
        self.assert_summary({job: classification(job) for job in JOBS}, 0, "")

    def test_one_gate_has_new_failures(self):
        self.assert_summary({"full-eunit": classification("full-eunit", new=2)}, 2, "full-eunit")

    def test_mixed_classifications_only_list_new_gates(self):
        self.assert_summary({
            "full-eunit": classification("full-eunit", new=2),
            "dialyze": classification("dialyze", known=3),
            "gradualize": classification("gradualizer", fixed=4),
            "eqwalize": classification("eqwalizer-lib", new=10),
            "elvis": classification("elvis", flaky=1),
            "pg-behavior-harnesses": classification("pg-behavior-harnesses"),
        }, 12, "full-eunit,eqwalizer-lib")

    def test_known_failures_do_not_create_new_gate_names(self):
        self.assert_summary({"elvis": classification("elvis", known=4)}, 0, "")

    def test_missing_classifications_preserve_empty_summary(self):
        # Failed/skipped prerequisite jobs can have no classification output;
        # their job results still determine the overall workflow conclusion.
        self.assert_summary({}, 0, "")

    def test_gate_extraction_errors_remain_failures(self):
        result, outputs = self.run_collect(
            {"full-eunit": classification("full-eunit", new=1)}, fail_awk=True
        )
        self.assertEqual(result.returncode, 47, result.stdout)
        self.assertEqual(outputs, {})


if __name__ == "__main__":
    unittest.main()
