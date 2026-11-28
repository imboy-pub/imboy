#!/usr/bin/env python3
"""Offline behavior regressions: never call real Docker, skopeo, or Git."""
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]
COMPONENTS = ("backend", "pg18", "admin", "widget")
VERSION = "1.2.3-alpha.4"
REFS = [
    f"ghcr.io/imboy-pub/imboy-{name}@sha256:{i:064x}"
    for i, name in enumerate(COMPONENTS, 1)
]
MOCK = """#!/usr/bin/env python3
import os, pathlib, sys
state = pathlib.Path(os.environ["STATE"])
a = sys.argv[1:]
tool = pathlib.Path(sys.argv[0]).name
with (state / "calls").open("a") as f:
    f.write(tool + " " + " ".join(a) + "\\n")
if tool == "git":
    if a[-2:] == ["rev-parse", "HEAD"]:
        print("a" * 40)
        sys.exit(0)
    sys.exit(90)
if tool == "skopeo":
    if a[:3] != ["copy", "--all", "--preserve-digests"]:
        sys.exit(91)
    src, dst = a[3:]
    name = dst.split("/")[-1].split(":")[0]
    (state / name).write_text(src.split("@")[1])
    sys.exit(0)
if a[:3] != ["buildx", "imagetools", "inspect"]:
    sys.exit(92)
ref = a[3]
name = ref.split("/")[-1].split("@")[0].split(":")[0]
component = name.removeprefix("imboy-")
n = ("backend", "pg18", "admin", "widget").index(component) + 1
digest = f"sha256:{n:064x}"
if ref.startswith("ghcr.io/"):
    if os.environ.get("SCENARIO") == "source_conflict" and component == "widget" and "@" not in ref:
        print("sha256:" + "f" * 64)
    else:
        print(digest)
    sys.exit(0)
if os.environ.get("SCENARIO") == "auth" and component == "widget":
    print("unauthorized: not found", file=sys.stderr)
    sys.exit(1)
if os.environ.get("SCENARIO") == "network" and component == "widget":
    print("TLS handshake timeout", file=sys.stderr)
    sys.exit(1)
if (state / component).exists():
    print((state / component).read_text())
    sys.exit(0)
print("manifest unknown: MANIFEST_UNKNOWN", file=sys.stderr)
sys.exit(1)
"""


class DockerfileLayoutTest(unittest.TestCase):
    def test_backend_production_and_dev_recipes_have_distinct_roles(self):
        production = ROOT / "docker/backend_Dockerfile"
        development = ROOT / "docker/backend_dev_Dockerfile"
        legacy = ROOT / "Dockerfile"
        self.assertTrue(production.is_file())
        self.assertTrue(development.is_file())
        self.assertTrue(legacy.is_symlink())
        self.assertEqual(legacy.resolve(), production.resolve())
        self.assertIn("AS runtime", production.read_text())
        self.assertIn('CMD ["/opt/imboy/bin/imboy", "foreground"]', production.read_text())
        self.assertIn('CMD ["bash"]', development.read_text())
        self.assertFalse((ROOT / "docker/imboy_Dockerfile_dev").exists())


class BuildImagesTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        for name in ("docker", "scripts", "deploy", "bin", "state"):
            (self.root / name).mkdir()
        for path in ("docker/build_images.sh", "scripts/release_version.sh"):
            shutil.copy(ROOT / path, self.root / path)
        (self.root / "VERSION").write_text(VERSION + "\n")
        (self.root / "deploy/.env.example").write_text("IMBOY_VERSION=" + VERSION + "\n")
        for tool in ("docker", "skopeo", "git"):
            file = self.root / "bin" / tool
            file.write_text(MOCK)
            file.chmod(0o755)
        self.state = self.root / "state"
        self.env = dict(os.environ, PATH=str(self.root / "bin") + os.pathsep + os.environ["PATH"],
                        STATE=str(self.state), ADMIN_REPO_REF="", GITHUB_RUN_ID="1", GITHUB_RUN_ATTEMPT="1",
                        SCENARIO="")
        self.log = self.state / "calls"

    def call(self, *args, scenario="", ok=True):
        result = subprocess.run(["bash", str(self.root / "docker/build_images.sh"), *args],
                                env=dict(self.env, SCENARIO=scenario), capture_output=True, text=True)
        self.assertEqual(result.returncode == 0, ok, result.stdout + result.stderr)
        return result

    def writes(self):
        return [x for x in self.log.read_text().splitlines() if x.startswith("skopeo ")] if self.log.exists() else []

    def test_help_and_bad_build_arguments(self):
        self.call("--help")
        self.call("build", "--platform", "linux/amd64,linux/arm64", ok=False)
        self.call("build", "--platform", "linux/riscv64", ok=False)
        self.call("build", ok=False)  # missing approved frontend SHA
        self.assertFalse(self.log.exists())

    def test_publish_read_only(self):
        self.call("publish", VERSION, *REFS)
        self.assertEqual(self.writes(), [])
        self.assertEqual(len([x for x in self.log.read_text().splitlines() if x.startswith("docker ")]), 12)

    def test_apply_exact_digests_and_idempotency(self):
        self.call("publish", "--apply", VERSION, *REFS)
        self.assertEqual(len(self.writes()), 4)
        for name, ref in zip(COMPONENTS, REFS):
            self.assertEqual((self.state / name).read_text(), ref.split("@")[1])
        self.call("publish", "--apply", VERSION, *REFS)
        self.assertEqual(len(self.writes()), 4)

    def test_conflicting_hub_tag_blocks_all_writes(self):
        (self.state / "widget").write_text("sha256:" + "f" * 64)
        self.call("publish", "--apply", VERSION, *REFS, ok=False)
        self.assertEqual(self.writes(), [])

    def test_unapproved_source_tag_blocks_all_writes(self):
        self.call("publish", "--apply", VERSION, *REFS, scenario="source_conflict", ok=False)
        self.assertEqual(self.writes(), [])

    def test_registry_failures_do_not_authorize_writes(self):
        for scenario in ("auth", "network"):
            with self.subTest(scenario=scenario):
                self.call("publish", "--apply", VERSION, *REFS, scenario=scenario, ok=False)
                self.assertEqual(self.writes(), [])

    def test_floating_and_wrong_component_refs_rejected(self):
        for bad in ("ghcr.io/imboy-pub/imboy-widget:latest", REFS[0], REFS[3].replace("imboy-pub", "other")):
            with self.subTest(bad=bad):
                self.call("publish", "--apply", VERSION, *REFS[:3], bad, ok=False)
        self.assertFalse(any(line.startswith("docker ") for line in self.log.read_text().splitlines()))


if __name__ == "__main__":
    unittest.main(verbosity=2)
