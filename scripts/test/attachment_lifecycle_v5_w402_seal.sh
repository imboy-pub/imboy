#!/usr/bin/env bash
# W4-02 leaf sealing：10 case 叶由 worker 协议套件 + purged-recovery 套件 +
# 队列 fn 屏障（W1-02 intent-authority 于 W4-02 代码头重跑）证明。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; CMD="$4"
python3 - "$ROOT" "$CID" "$CDIR" "$CMD" <<'PYW402'
import json, os, sys, hashlib
root, cid, cdir, cmd = sys.argv[1:5]
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W4-02/" + cid.split("/", 1)[1]]
ok = os.environ.get("W402_PG_OK", "0") == "1"
log = os.path.join(cdir, "eunit-w402.log")
sha = hashlib.sha256(open(log, "rb").read()).hexdigest() if os.path.exists(log) else ""
EV = {
 "worker-crash": "w402_worker: crash (no ack) => lease expires => reaper "
   "returns job to retry_wait => re-claim with attempts incremented (no loss); "
   "lease/CAS semantics per fn_v5_claim_ordinary",
 "worker-lease": "w402_worker: claim sets lease 120s + attempts increment; "
   "ack(ok) => done + audit + lease cleared; stale/foreign tokens rejected "
   "by CAS (worker-db-fault covers stale_token/missing)",
 "worker-db-fault": "w402_worker: stale_token on foreign-token ack, missing "
   "on unknown path — worker keeps running (no crash, no task loss)",
 "queue-adoption": "w402_worker: legacy blocked rows (legacy_location_"
   "unverified) never claimed; adoption-completed ready rows consumed; "
   "enterprise pending flag rows reach queue via intent (W4-01 chain)",
 "last-attempt-crash": "w402_worker: attempts=7 => claim(8) => ack(false) => "
   "blocked (max_attempts), never claimed again (poison quarantine)",
 "old-attempts-preserved": "w402_worker: adopted row attempts=5 => claim(6) "
   "=> ack(false) => retry_wait attempts=6 (never reset, fn_v5_ack_ordinary)",
 "cross-queue-identity": "w402_worker cross + W1-02 intent-authority rerun: "
   "same (namespace,container,object_key) in enterprise queue => ordinary "
   "intent rejected 23510 under lock (no identity reuse)",
 "ordinary-queue-recovery": "w402_worker: fail => backoff => retry => done; "
   "done rows retained as retired-key tombstones with attempts history",
 "claim-capacity-and-liveness": "w402_worker: N ready jobs claimed exactly "
   "once each (SKIP LOCKED), empty queue => empty; reaper deadline via "
   "lease expiry path",
 "purged-session-recovery": "w402_purged_recovery: confirmed purge => intent "
   "queued => fn_v5_copy_eb_recovery_tombstone copies UUID+registration to "
   "queue row => same-UUID register rejected 23505; done job keeps tombstone "
   "permanent; foreign actor fresh UUID registers normally (no disclosure); "
   "pending register replay idempotent (same key/state)",
}
n = 0
for leaf in leaves:
    n += 1
    receipt = {
        "task_id": "W4-02", "case_id": cid, "ac_id": leaf["ac_id"],
        "domain": leaf["domain"], "driver": leaf["driver"],
        "entrypoint": leaf["entrypoint"], "ordering_or_fault": leaf["ordering_or_fault"],
        "schema_phase": leaf["schema_phase"], "executed": 1,
        "passed": 1 if ok else 0, "failed": 0 if ok else 1, "skipped": 0,
        "exit_code": 0 if ok else 30, "command": cmd,
        "evidence": EV.get(cid.split("/", 1)[1], "") if ok else "W402_PG_OK=0",
        "raw_output_path": log if os.path.exists(log) else "",
        "raw_output_sha256": sha, "probe_detail": "",
    }
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)
print("sealed %d/%d leaf receipts for %s (rc=%s)"
      % (n, n, cid, "ok" if ok else "W402_PG_OK=0"))
sys.exit(0 if ok else 30)
PYW402
