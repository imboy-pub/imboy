import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/verify_product_feature_artifacts.py"
SPEC = importlib.util.spec_from_file_location("feature_artifacts", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class ProductFeatureArtifactTest(unittest.TestCase):
    contract = {
        "manifest_hash": "sha256:test",
        "compiled_features": ["channel", "core"],
    }

    def test_matching_artifacts_pass(self):
        payload = b"sha256:test channel core"
        MODULE.verify_markers(
            {"backend": payload, "flutter": payload, "admin": payload},
            self.contract,
        )

    def test_stale_hash_fails(self):
        with self.assertRaisesRegex(MODULE.ArtifactError, "backend.*sha256:test"):
            MODULE.verify_markers(
                {
                    "backend": b"sha256:stale channel core",
                    "flutter": b"sha256:test channel core",
                    "admin": b"sha256:test channel core",
                },
                self.contract,
            )

    def test_app_or_admin_feature_subset_fails(self):
        for artifact in ("flutter", "admin"):
            payloads = {
                "backend": b"sha256:test channel core",
                "flutter": b"sha256:test channel core",
                "admin": b"sha256:test channel core",
            }
            payloads[artifact] = b"sha256:test core"
            with self.subTest(artifact=artifact), self.assertRaisesRegex(
                MODULE.ArtifactError, f"{artifact}.*channel"
            ):
                MODULE.verify_markers(payloads, self.contract)

    def test_debug_apk_uses_kernel_blob(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            apk = root / "app-debug.apk"
            beam = root / "imboy_feature.beam"
            dist = root / "dist"
            dist.mkdir()
            beam.write_bytes(b"backend")
            (dist / "index.js").write_bytes(b"admin")
            with zipfile.ZipFile(apk, "w") as archive:
                archive.writestr(
                    "assets/flutter_assets/kernel_blob.bin", b"flutter-debug"
                )

            payloads = MODULE.artifact_payloads(beam, apk, dist)

            self.assertEqual(payloads["flutter"], b"flutter-debug")

    def _probe_payloads(self, with_moment):
        markers = b"moment_create moment_detail moment_feed"
        return {
            "backend": b"sha256:test channel core",
            "flutter": b"sha256:test channel core " + (markers if with_moment else b""),
            "admin": b"sha256:test channel core",
        }

    def _probe_dist(self, with_chunk):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        dist = Path(directory.name)
        (dist / "index.js").write_bytes(b"admin")
        if with_chunk:
            (dist / "assets").mkdir()
            (dist / "assets" / "moments-ABC123.js").write_bytes(b"moment pages")
        return dist

    def test_probe_assets_present_when_feature_compiled(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core", "moment"],
        }
        MODULE.verify_probe_assets(
            self._probe_dist(True), self._probe_payloads(True), contract
        )

    def test_probe_assets_missing_chunk_fails_when_feature_compiled(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core", "moment"],
        }
        with self.assertRaisesRegex(MODULE.ArtifactError, "admin.*missing.*moments"):
            MODULE.verify_probe_assets(
                self._probe_dist(False), self._probe_payloads(True), contract
            )

    def test_probe_assets_missing_marker_fails_when_feature_compiled(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core", "moment"],
        }
        with self.assertRaisesRegex(MODULE.ArtifactError, "flutter.*moment_"):
            MODULE.verify_probe_assets(
                self._probe_dist(True), self._probe_payloads(False), contract
            )

    def test_probe_assets_absent_when_feature_excluded(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core"],
        }
        MODULE.verify_probe_assets(
            self._probe_dist(False), self._probe_payloads(False), contract
        )

    def test_probe_assets_chunk_present_fails_when_feature_excluded(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core"],
        }
        with self.assertRaisesRegex(MODULE.ArtifactError, "admin.*contains.*moments"):
            MODULE.verify_probe_assets(
                self._probe_dist(True), self._probe_payloads(False), contract
            )

    def test_probe_assets_marker_present_fails_when_feature_excluded(self):
        contract = {
            "manifest_hash": "sha256:test",
            "compiled_features": ["channel", "core"],
        }
        with self.assertRaisesRegex(MODULE.ArtifactError, "flutter.*moment_"):
            MODULE.verify_probe_assets(
                self._probe_dist(False), self._probe_payloads(True), contract
            )

    def test_evidence_does_not_hash_itself(self):
        with tempfile.TemporaryDirectory() as directory:
            repo = Path(directory)
            (repo / "source.txt").write_text("source")
            evidence = repo / MODULE.EVIDENCE_PREFIX / "base-only.json"
            evidence.parent.mkdir(parents=True)
            evidence.write_text("first")
            original_root = MODULE.ROOT
            MODULE.ROOT = repo
            try:
                with mock.patch.object(
                    MODULE.subprocess,
                    "check_output",
                    side_effect=[
                        b"diff",
                        "docs/compliance/feature-composition-evidence/base-only.json\nsource.txt\n",
                        b"diff",
                        "docs/compliance/feature-composition-evidence/base-only.json\nsource.txt\n",
                    ],
                ):
                    first = MODULE.worktree_hash(repo)
                    evidence.write_text("second")
                    self.assertEqual(MODULE.worktree_hash(repo), first)
            finally:
                MODULE.ROOT = original_root


if __name__ == "__main__":
    unittest.main()
