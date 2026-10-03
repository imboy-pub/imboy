"""Read-only synthetic golden-vector Ed25519 checks with OpenSSL."""
import json
import subprocess
import sys
import tempfile
from pathlib import Path

vectors = json.loads(Path(sys.argv[1]).read_text(encoding="utf-8"))
checks = []
with tempfile.TemporaryDirectory(prefix="imboy-vector-openssl-") as directory:
    root = Path(directory)
    for scenario in vectors["scenarios"]:
        if "tree_head" not in scenario:
            continue
        key = bytes.fromhex(scenario["signing_key"]["public_key_hex"])
        (root / "public.der").write_bytes(bytes.fromhex("302a300506032b6570032100") + key)
        head = scenario["tree_head"]
        mutation = next(v["expected"] for v in scenario["vectors"] if v["kind"] == "tampered_head")
        cases = [
            ("original", head["signing_input_hex"], head["signature_hex"], 0),
            ("tampered-original-signature", mutation["tampered_signing_input_hex"], head["signature_hex"], 1),
            ("tampered-resigned", mutation["tampered_signing_input_hex"], mutation["tampered_signature_hex"], 0),
        ]
        for name, message, signature, expected in cases:
            (root / "message.bin").write_bytes(bytes.fromhex(message))
            (root / "signature.bin").write_bytes(bytes.fromhex(signature))
            result = subprocess.run([
                "openssl", "pkeyutl", "-verify", "-pubin", "-keyform", "DER",
                "-inkey", str(root / "public.der"), "-rawin",
                "-in", str(root / "message.bin"), "-sigfile", str(root / "signature.bin"),
            ], capture_output=True, text=True, timeout=10)
            checks.append({"scenario": scenario["id"], "case": name,
                           "exit": result.returncode, "expected_exit": expected,
                           "pass": result.returncode == expected,
                           "stdout": result.stdout.strip(), "stderr": result.stderr.strip()})
report = {"checks": checks, "count": len(checks),
          "fails": sum(not check["pass"] for check in checks)}
print(json.dumps(report, indent=2))
if ({check["scenario"] for check in checks} != {"S1", "S2", "S3"}
        or report["count"] != 9 or report["fails"] != 0):
    raise SystemExit(1)
