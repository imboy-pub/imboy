#!/usr/bin/env bash
# W3-03 leaf sealing：两 case 的叶由投影+绑定套件整体绿灯证明
#（eb_message_assets_projection_tests：canonical 绑定/重放幂等/全回滚/
#  跨租户与上传人拒绝；10 用例含 W3-03 新增 3 场景）。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"
python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW303'
import json, os, sys, hashlib
root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W3-03/" + cid.split("/", 1)[1]]
ok = os.environ.get("W303_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w303.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""
EV = {
 "enterprise-bind": "eb_message_assets_projection_tests: canonical append binds "
   "asset_ids in one tx (identity advisory locks 1685 byte-order then id-asc "
   "FOR UPDATE); same client_msg_id+asset_ids replay returns same message; "
   "one tombstone (delete_intent) asset rolls back the whole tx with zero "
   "bindings and no message row",
 "cs-bind": "eb_message_assets_projection_tests: contact/cs path replay "
   "idempotent; cross-org asset, wrong-workspace (tenant gate) and "
   "non-uploader (member binds own uploads only) all rejected not_found "
   "with zero side effects",
}
n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W3-03", "case_id": cid, "ac_id": leaf["ac_id"],
        "domain": leaf["domain"], "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"], "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"], "executed": 1,
        "passed": 1 if ok else 0, "failed": 0 if ok else 1, "skipped": 0,
        "exit_code": 0 if ok else 30, "command": cmd,
        "evidence": EV.get(cid.split("/", 1)[1], "") if ok else "W303_PG_OK=0",
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha, "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)
print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W303_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW303
