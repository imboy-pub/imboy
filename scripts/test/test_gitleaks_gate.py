"""Exercise the actual CI shell and commit-bound secret exemptions offline."""

import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github/workflows/quality.yml"
HISTORY = "Scan git history (ratchet — baseline 0)"
CURRENT = "Verify no new leaks"


def step_script(name):
    # Same run-block extraction convention as test_sbom_workflow.py.
    lines = WORKFLOW.read_text().splitlines()
    start = lines.index(f"      - name: {name}")
    start = lines.index("        run: |", start) + 1
    body = []
    for line in lines[start:]:
        if line and not line.startswith("          "):
            break
        body.append(line[10:])
    return "\n".join(body) + "\n"


class GitleaksGateTest(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="imboy-gitleaks-test-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name)
        for name in (".gitleaks.toml", ".gitleaksignore"):
            shutil.copyfile(ROOT / name, self.root / name)
        self.env = {name: os.environ[name] for name in ("PATH", "HOME", "TMPDIR", "LANG")
                    if name in os.environ}
        self.env.update(GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
                        GIT_AUTHOR_NAME="CI Fixture", GIT_COMMITTER_NAME="CI Fixture",
                        GIT_AUTHOR_EMAIL="fixture@example.invalid",
                        GIT_COMMITTER_EMAIL="fixture@example.invalid")
        self.run_command(["git", "init", "-q"])

    def run_command(self, args, success=True):
        result = subprocess.run(args, cwd=self.root, env=self.env, text=True,
                                capture_output=True, timeout=30)
        if success:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        return result

    def commit(self):
        self.run_command(["git", "add", "--all"])
        self.run_command(["git", "commit", "-q", "-m", "synthetic fixture"])

    def run_step(self, name):
        script = step_script(name).replace("/tmp/leaks.json", str(self.root / "workflow-report.json"))
        return self.run_command(["bash", "-c", script], success=False)

    def secret_file(self, relative="config/gate-canary.env", line=1, token=None):
        path = self.root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        # Test-only synthetic data, never a provider credential or checked-in secret.
        canary = token if token is not None else self.canary_token()
        path.write_text("\n" * (line - 1) + f'api_key="{canary}"\n')
        return path

    @staticmethod
    def canary_token():
        # 固定 canary：随机 token_hex 的取值会偶发落出 generic-api-key 的
        # 判定窗（实测 500 样本约 0.2% 漏检、token_urlsafe 更差），CI 抽中
        # 即假绿（run 45083463136：expected 1, got 0）。gitleaks 版本由
        # quality.yml 钉死（8.30.1 + sha256），固定值使自检确定可复现；
        # 本值经 10/10 检出验证、熵 3.85 留有余量。两段拼接避免源码内出
        # 现「关键词=完整 token」字面量触发仓库自身扫描（两段单独均无
        # 关键词上下文，已实测不命中）。
        return (
            "c733294485c3797ace2dbbfa9700a565"
            "06017486ba515b7bb7a8af9c454ff288"
        )

    @staticmethod
    def canary_token_alt():
        # 第二个固定 canary：指纹豁免测试需要在同一 vector 落「内容不同
        # 的新提交」——两个提交内容一致时 git diff 为空、无 finding，随
        # 机 token 时代靠重新生成天然达成。同款验证口径（5/5 检出、熵 3.8）。
        return (
            "bdf00156403e1106b4447952f565ddfc"
            "e37beb5372bcba3aefa955166059e6ef"
        )

    def history_findings(self):
        report = self.root / "report.json"
        result = self.run_command(
            ["gitleaks", "detect", "--config", ".gitleaks.toml", "--redact", "--no-banner",
             "--report-format", "json", "--report-path", str(report)], success=False)
        self.assertEqual(result.returncode, 1)
        findings = json.loads(report.read_text())
        self.assertGreater(len(findings), 0)
        return findings

    def test_scanner_errors_fail_both_workflow_steps(self):
        binary = self.root / "bin/gitleaks"
        binary.parent.mkdir()
        binary.write_text('#!/bin/sh\nexit "$SCANNER_EXIT"\n')
        binary.chmod(0o755)
        self.env["PATH"] = str(binary.parent) + os.pathsep + self.env["PATH"]
        for name in (HISTORY, CURRENT):
            for code in (1, 2):
                with self.subTest(step=name, exit_code=code):
                    self.env["SCANNER_EXIT"] = str(code)
                    result = self.run_step(name)
                    self.assertNotEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_current_tree_and_deleted_history_secret_remain_blocking(self):
        self.assertEqual(self.run_step(CURRENT).returncode, 0)
        path = self.secret_file()
        self.assertEqual(self.run_step(CURRENT).returncode, 1)
        self.commit()
        self.assertEqual(self.run_step(HISTORY).returncode, 1)
        path.unlink()
        self.commit()
        self.assertEqual(self.run_step(CURRENT).returncode, 0)
        self.assertEqual(self.run_step(HISTORY).returncode, 1)

    def test_invalid_configuration_is_an_execution_failure(self):
        (self.root / ".gitleaks.toml").write_text("not valid TOML [\n")
        for name in (HISTORY, CURRENT):
            with self.subTest(step=name):
                self.assertNotEqual(self.run_step(name).returncode, 0)

    def test_manifest_hash_allowlist_is_limited_to_exact_path_and_values(self):
        relative = "docs/architecture/attachment-lifecycle-v5/evidence/current-baseline.json"
        manifest = self.root / relative
        manifest.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(ROOT / relative, manifest)
        self.assertEqual(self.run_step(CURRENT).returncode, 0)
        duplicate = self.root / "config/baseline-copy.json"
        duplicate.parent.mkdir()
        shutil.copyfile(manifest, duplicate)
        self.assertEqual(self.run_step(CURRENT).returncode, 1)
        duplicate.unlink()
        data = json.loads(manifest.read_text())
        data["ci_gate_canary"] = {"api_key": self.canary_token()}
        manifest.write_text(json.dumps(data, indent=2) + "\n")
        self.assertEqual(self.run_step(CURRENT).returncode, 1)

    def test_exact_fingerprint_does_not_hide_a_later_commit(self):
        self.secret_file()
        self.commit()
        findings = self.history_findings()
        with (self.root / ".gitleaksignore").open("a") as stream:
            for finding in findings:
                self.assertRegex(finding["Fingerprint"], r"^[0-9a-f]{40}:")
                stream.write(finding["Fingerprint"] + "\n")
        self.assertEqual(self.run_step(HISTORY).returncode, 0)
        self.secret_file(token=self.canary_token_alt())
        self.commit()
        self.assertEqual(self.run_step(HISTORY).returncode, 1)

    def test_new_secret_at_historical_vector_path_is_not_exempt(self):
        entries = [line for line in (ROOT / ".gitleaksignore").read_text().splitlines()
                   if line and not line.startswith("#")]
        for entry in entries:
            self.assertRegex(entry, r"^[0-9a-f]{40}:.+:[^:]+:[1-9][0-9]*$")
        vectors = [entry.split(":", 3) for entry in entries
                   if ":docs/design/2026-10-03-e2ee-production-excellence/" in entry
                   and ":generic-api-key:" in entry]
        self.assertGreater(len(vectors), 0, "historical vector fingerprints are missing")
        _, relative, _, line = min(vectors, key=lambda parts: int(parts[3]))
        self.secret_file(relative, int(line))
        self.commit()
        self.assertEqual(self.run_step(HISTORY).returncode, 1)
        self.assertTrue(any(finding["File"] == relative and finding["StartLine"] == int(line)
                            and finding["RuleID"] == "generic-api-key"
                            for finding in self.history_findings()))


if __name__ == "__main__":
    unittest.main()
