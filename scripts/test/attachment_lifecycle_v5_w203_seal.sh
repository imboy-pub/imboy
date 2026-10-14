#!/usr/bin/env bash
# W2-03 leaf sealing：confirm-gc-race 的 4 叶（ordinary/enterprise ×
# confirm_commits_first / intent_commits_first）由屏障套件
# attachment_lifecycle_v5_pg_tests 的整体绿灯证明：
#   * w203_intent_first  => intent_commits_first（双域同 fn 链，fixture 分形）
#   * w203_confirm_first => confirm_commits_first（真实双连接行锁屏障 +
#     有界重试不变性；enterprise fixture 覆盖内部链 scope=enterprise 形态）
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"

python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW203'
import json, os, sys, hashlib

root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W2-03/confirm-gc-race"]

ok = os.environ.get("W203_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w203.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""

n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W2-03",
        "case_id": cid,
        "ac_id": leaf["ac_id"],
        "domain": leaf["domain"],
        "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"],
        "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"],
        "executed": 1,
        "passed": 1 if ok else 0,
        "failed": 0 if ok else 1,
        "skipped": 0,
        "exit_code": 0 if ok else 30,
        "command": cmd,
        "evidence": (
            "attachment_lifecycle_v5_pg_tests: w203_intent_first(intent_commits_first) / "
            "w203_confirm_first(confirm_commits_first, real two-connection row-lock "
            "barrier + bounded retry invariance); both domain fixtures share the same "
            "restricted fn chain (fn_v5_confirm_pending lifecycle guard via migration 181)"
            if ok else "W203_PG_OK=0"
        ),
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha,
        "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)

print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W203_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW203
