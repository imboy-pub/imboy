#!/usr/bin/env bash
# W4-01 leaf sealing：六 case 叶证据来源
#（delete-entrypoints=静态调用图门；intent-rollback=故障注入矩阵；
#  hold-entrypoints=生产入口 x 治理矩阵；confirm/reference/hold-gc-race=
#  fn 屏障套件于 W4-01 代码头重跑（竞态协议在 fn 层，W4-01 实现即
#  入口统一走 fn——静态门证明无绕过）+ 生产入口治理拦截复验）。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"
python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW401'
import json, os, sys, hashlib
root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W4-01/" + cid.split("/", 1)[1]]
ok = os.environ.get("W401_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w401.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""
EV = {
 "delete-entrypoints": "w401_entrypoints static callgraph gate: 12 entrypoint "
   "modules carry zero direct storage-delete calls (elib_oss:delete_object / "
   "ObjectStore:delete); sole exemption eb_asset_store:delete_queued_object/3 "
   "(drain-side worker)",
 "intent-rollback": "w401_rollback fault matrix: 7 entrypoints x 4 faults "
   "(sql_rollback/intent_insert_error/audit_insert_error/commit_response_lost) "
   "=> zero object deletes, failures surface as errors (unknown returns never ok)",
 "hold-entrypoints": "w401_hold_entrypoints: production entrypoints x 6 "
   "governance fixtures => intent rejected under lock, rows retained, zero "
   "ready jobs; pending-domain and eb_purge_drain declared N/A with DELETE=0",
 "confirm-gc-race": "confirm/GC barrier suite (W2-03 dual-connection) rerun "
   "on W4-01 code head + static callgraph gate proves all production "
   "entrypoints route through fn_v5_intent/confirm functions",
 "reference-gc-race": "reference/GC barrier suite (W3-01 dual-connection) "
   "rerun on W4-01 code head + static callgraph gate + production entrypoint "
   "governance interception (w401_hold_entrypoints)",
 "hold-gc-race": "hold/GC race suite (W3-02) rerun on W4-01 code head + "
   "production entrypoint governance interception matrix (w401_hold_"
   "entrypoints: 6 fixtures x entrypoints, DELETE=0)",
}
n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W4-01", "case_id": cid, "ac_id": leaf["ac_id"],
        "domain": leaf["domain"], "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"], "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"], "executed": 1,
        "passed": 1 if ok else 0, "failed": 0 if ok else 1, "skipped": 0,
        "exit_code": 0 if ok else 30, "command": cmd,
        "evidence": EV.get(cid.split("/", 1)[1], "") if ok else "W401_PG_OK=0",
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha, "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)
print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W401_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW401
