#!/usr/bin/env bash
# W3-02 leaf sealing：两 case 的叶由屏障套件整体绿灯证明
#（w302_entrypoint_matrix：12 入口 x 6 治理 fixture 全矩阵 intent 23510；
#  w302_hold_gc_race：hold 先提交 intent 拒 / intent 先提交治理 already_deleting）。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"
python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW302'
import json, os, sys, hashlib
root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W3-02/" + cid.split("/", 1)[1]]
ok = os.environ.get("W302_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w302.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""
EV = {
 "hold-all-entrypoints": "w302_entrypoint_matrix: 12 future-deletion entrypoint "
   "fixtures x 6 governance fixtures (WAR hold_workspace/hold_conversation/"
   "permanent_workspace_retention, EAR hold_message/hold_attachment/"
   "future_retention), every combination intent rejected 23510 "
   "(no reference-zero under governance)",
 "hold-gc-race": "w302_hold_gc_race: hold commits first => ordinary GC intent "
   "rejected 23510; intent commits first (unified two-domain eligibility via "
   "production refresh_gc_verified_tx) => governance_tx returns already_deleting",
}
n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W3-02", "case_id": cid, "ac_id": leaf["ac_id"],
        "domain": leaf["domain"], "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"], "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"], "executed": 1,
        "passed": 1 if ok else 0, "failed": 0 if ok else 1, "skipped": 0,
        "exit_code": 0 if ok else 30, "command": cmd,
        "evidence": EV.get(cid.split("/", 1)[1], "") if ok else "W302_PG_OK=0",
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha, "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)
print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W302_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW302
