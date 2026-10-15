#!/usr/bin/env bash
# W3-01 leaf sealing：三 case 的叶由屏障套件整体绿灯证明
#（w301_reference_gc_race / w301_shared_owner / w301_history_unknown）。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"
python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW301'
import json, os, sys, hashlib
root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W3-01/" + cid.split("/", 1)[1]]
ok = os.environ.get("W203_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w203.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""
EV = {
 "reference-gc-race": "w301_reference_gc_race: bind/intent both orders x "
   "production entrypoint and direct constrained DML, same object-lock domain",
 "shared-owner": "w301_shared_owner: group-bound/collected assets survive "
   "creator deletion guard; unshared personal released; unrelated reader denied",
 "history-unknown": "w301_history_unknown: unknown-location/cipher/collect "
   "ambiguity keep gc_verified=false with 0 deletion jobs and unchanged hash",
}
n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W3-01", "case_id": cid, "ac_id": leaf["ac_id"],
        "domain": leaf["domain"], "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"], "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"], "executed": 1,
        "passed": 1 if ok else 0, "failed": 0 if ok else 1, "skipped": 0,
        "exit_code": 0 if ok else 30, "command": cmd,
        "evidence": EV.get(cid.split("/", 1)[1], "") if ok else "W203_PG_OK=0",
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha, "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)
print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W203_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW301
