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


# BUILD-00 探针：feature 物理资产断言（与 compiled_features 契约 marker 不同级）。
# - admin_dist_chunks: Admin dist 下动态 chunk 文件 glob；feature 被裁剪时 chunk
#   文件必须不存在（文件级物理资产，非菜单/路由守卫）。
# - flutter_payload_markers: release APK AOT payload（libapp.so）字符串 marker；
#   marker 选自 generated_product_feature_routes.dart 中 probe 专属 GoRoute name，
#   裁剪时路由代码不进编译单元、字符串不得残留。注意避开 '/moment/feed' 这类
#   被核心页面硬编码引用的路径（contact_page.dart 残留跳转，会假阴）。
PROBE_FEATURE_ASSETS = {
    "moment": {
        "admin_dist_chunks": ["moments-*.js"],
        "flutter_payload_markers": [b"moment_create", b"moment_detail", b"moment_feed"],
    },
}


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


def verify_probe_assets(admin_dist, payloads, contract):
    """BUILD-00：probe feature 的物理资产正/负向断言。

    compiled_features 包含 probe 时资产必须存在（证明断言能发现资产）；
    不包含时资产必须不存在（证明裁剪是物理级，而非 marker/菜单/运行时开关）。
    Backend 的模块级裁剪由独立探针命令验证，不在本函数范围。
    """
    compiled = set(contract["compiled_features"])
    for feature, spec in PROBE_FEATURE_ASSETS.items():
        included = feature in compiled
        for pattern in spec.get("admin_dist_chunks", []):
            found = sorted(path.name for path in admin_dist.rglob(pattern))
            if included and not found:
                raise ArtifactError(
                    f"admin artifact missing probe asset {pattern} for feature {feature}"
                )
            if not included and found:
                raise ArtifactError(
                    f"admin artifact contains probe asset {pattern} for feature {feature}: "
                    + ", ".join(found)
                )
        flutter_payload = payloads["flutter"]
        for marker in spec.get("flutter_payload_markers", []):
            present = marker in flutter_payload
            if included and not present:
                raise ArtifactError(
                    f"flutter payload missing probe marker {marker!r} for feature {feature}"
                )
            if not included and present:
                raise ArtifactError(
                    f"flutter payload contains probe marker {marker!r} for feature {feature}"
                )


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
    verify_probe_assets(admin_dist, payloads, contract)
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
