#!/usr/bin/env python3
import argparse
import hashlib
import importlib.util
import json
import subprocess
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
WORKSPACE = ROOT.parent
EVIDENCE_PREFIX = "docs/compliance/feature-composition-evidence/"
GENERATOR_PATH = ROOT / "scripts/generate_product_features.py"
SPEC = importlib.util.spec_from_file_location("product_features", GENERATOR_PATH)
GENERATOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(GENERATOR)


class ArtifactError(ValueError):
    pass


def sha256_bytes(data):
    return hashlib.sha256(data).hexdigest()


def sha256_file(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def git_value(repo, *args):
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True
    ).strip()


def worktree_hash(repo):
    digest = hashlib.sha256()
    digest.update(subprocess.check_output(["git", "-C", str(repo), "diff", "--binary", "HEAD"]))
    untracked = git_value(repo, "ls-files", "--others", "--exclude-standard").splitlines()
    for relative in sorted(untracked):
        if repo == ROOT and relative.startswith(EVIDENCE_PREFIX):
            continue
        path = repo / relative
        digest.update(relative.encode())
        if path.is_file():
            digest.update(path.read_bytes())
    return digest.hexdigest()


def artifact_payloads(backend_beam, flutter_apk, admin_dist):
    with zipfile.ZipFile(flutter_apk) as archive:
        flutter_entry = next(
            (name for name in archive.namelist() if name.endswith("/libapp.so")),
            None,
        )
        if flutter_entry is None:
            flutter_entry = next(
                (name for name in archive.namelist() if name.endswith("/kernel_blob.bin")),
                None,
            )
        if flutter_entry is None:
            raise ArtifactError("Flutter APK contains neither libapp.so nor kernel_blob.bin")
        flutter_payload = archive.read(flutter_entry)
    admin_files = sorted(admin_dist.rglob("*.js"))
    if not admin_files:
        raise ArtifactError("Admin dist contains no JavaScript artifacts")
    return {
        "backend": backend_beam.read_bytes(),
        "flutter": flutter_payload,
        "admin": b"\n".join(path.read_bytes() for path in admin_files),
    }


def verify_markers(payloads, contract):
    required = [contract["manifest_hash"], *contract["compiled_features"]]
    for artifact, payload in payloads.items():
        missing = [marker for marker in required if marker.encode() not in payload]
        if missing:
            raise ArtifactError(f"{artifact} artifact missing contract markers: {', '.join(missing)}")


def verify_generated_outputs(contract):
    stale = [
        str(path)
        for path, expected in GENERATOR.render(contract).items()
        if not path.exists() or path.read_text() != expected
    ]
    if stale:
        raise ArtifactError("stale generated output: " + ", ".join(stale))


def repo_evidence(repo):
    return {
        "head": git_value(repo, "rev-parse", "HEAD"),
        "dirty": bool(git_value(repo, "status", "--porcelain")),
        "worktree_sha256": worktree_hash(repo),
    }


def verify(manifest_path, backend_beam, flutter_apk, admin_dist):
    manifest = GENERATOR.read_manifest(manifest_path)
    contract = GENERATOR.validate(manifest, GENERATOR.runtime_catalog(ROOT))
    verify_generated_outputs(contract)
    payloads = artifact_payloads(backend_beam, flutter_apk, admin_dist)
    verify_markers(payloads, contract)
    return {
        "schema_version": contract["schema_version"],
        "manifest_hash": contract["manifest_hash"],
        "profile": contract["profile"],
        "compiled_features": contract["compiled_features"],
        "repositories": {
            name: repo_evidence(WORKSPACE / name)
            for name in ("imboy", "imboyapp", "imboyadmin")
        },
        "artifacts": {
            "backend": {
                "path": str(backend_beam),
                "sha256": sha256_file(backend_beam),
                "size": backend_beam.stat().st_size,
            },
            "flutter": {
                "path": str(flutter_apk),
                "sha256": sha256_file(flutter_apk),
                "size": flutter_apk.stat().st_size,
            },
            "admin": {
                "path": str(admin_dist),
                "sha256": sha256_bytes(payloads["admin"]),
                "javascript_files": len(list(admin_dist.rglob("*.js"))),
            },
        },
    }


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--backend-beam", required=True, type=Path)
    parser.add_argument("--flutter-apk", required=True, type=Path)
    parser.add_argument("--admin-dist", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args(argv)
    try:
        evidence = verify(
            args.manifest.resolve(),
            args.backend_beam.resolve(),
            args.flutter_apk.resolve(),
            args.admin_dist.resolve(),
        )
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(json.dumps(evidence, indent=2, sort_keys=True) + "\n")
    except (ArtifactError, GENERATOR.ManifestError, OSError, zipfile.BadZipFile) as error:
        print(f"product feature artifacts: {error}")
        return 1
    print(f"product feature artifacts OK {evidence['manifest_hash']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
