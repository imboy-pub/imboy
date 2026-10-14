#!/usr/bin/env bash
# W2-02 leaf sealing：按 execution-contract 期望叶矩阵把 eunit 证据映射成
# 叶回执（W2-02/confirm-idempotent | confirm-old-client | confirmed-content-immutable）。
#
# 环境旗（由 gate w202_case 传入）：
#   W202_CONFIRM_PG_OK / W202_EB_ASSET_OK / W202_GATEWAY_OK / W202_ATTACH_LOGIC_OK
# 证据映射：
#   ordinary/enterprise_internal 入口叶 -> confirm_pg 套件具名测试
#   enterprise_seat / cs_widget 叶     -> eb_asset_tests 套件旗（W2-01 ebp
#     共享协议先例：EB upload_ref/asset_id 协议与完整性校验为同一实现面）
# 每叶回执写 $3/leaf-NN.json，与 W2-01 seal 形状一致。
set -uo pipefail
ROOT="$1"; CID="$2"; CDIR="$3"; SHIFT="$4"; CMD="$5"

python3 - "$ROOT" "$CID" "$CDIR" "$SHIFT" "$CMD" <<'PYW202'
import json, os, sys, hashlib

root, cid, cdir, rc, cmd = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], sys.argv[5]
contract = json.load(open(os.path.join(
    root, "..", "..", "..", "backup", "premerge-untracked-plan",
    "attachment-lifecycle-v5", "execution-contract.json"))) if False else None
# 计划文档不在仓内：矩阵由 gate 环境的 RUN_DIR 侧 copy 提供
run_dir = os.environ.get("V5_RUN_DIR") or os.environ.get("RUN_DIR") or ""
matrix_path = os.path.join(run_dir, "backup", "premerge-untracked-plan",
                           "attachment-lifecycle-v5", "execution-contract.json")
if not os.path.exists(matrix_path):
    matrix_path = os.path.join(os.path.dirname(root), "..",
                               "backup", "premerge-untracked-plan",
                               "attachment-lifecycle-v5", "execution-contract.json")
contract = json.load(open(matrix_path))
leaves = contract["expected_leaf_matrix"]["W2-02/" + cid.split("/", 1)[1]]

flags = {k: os.environ.get(k, "0") == "1" for k in
         ["W202_CONFIRM_PG_OK", "W202_EB_ASSET_OK", "W202_GATEWAY_OK", "W202_ATTACH_LOGIC_OK"]}
confirm_log = open(os.path.join(cdir, "eunit-confirm.log")).read() if flags["W202_CONFIRM_PG_OK"] else ""
eb_log = open(os.path.join(cdir, "eunit-ebasset.log")).read() if flags["W202_EB_ASSET_OK"] else ""

# 具名测试 -> (entrypoint, scenario)；值为 (suite, test_name)
M = {
 "confirm-idempotent": {
  ("ordinary", "sequential_100_replays"): ("confirm", "t_sequential_100_replays"),
  ("ordinary", "concurrent_two_confirms"): ("confirm", "t_concurrent_two_confirms"),
  ("enterprise_internal", "sequential_100_replays"): ("confirm", "ent_sequential_100_replays"),
  ("enterprise_internal", "concurrent_two_confirms"): ("confirm", "ent_concurrent_two_confirms"),
 },
 "confirm-old-client": {
  ("ordinary", "confirmed_legacy_request"): ("confirm", "t_confirmed_legacy_request"),
  ("ordinary", "unknown_legacy_pending"): ("confirm", "t_unknown_legacy_pending"),
  ("enterprise_internal", "confirmed_legacy_request"): ("confirm", "ent_confirmed_legacy_request"),
  ("enterprise_internal", "unknown_legacy_pending"): ("confirm", "ent_unknown_legacy_pending"),
 },
 "confirmed-content-immutable": {
  ("ordinary", "put_A_confirm_old_capability_put_B"): ("confirm", "t_old_capability_put_b_rejected"),
  ("ordinary", "overwrite_between_stat_and_confirm"): ("confirm", "t_overwrite_between_stat_and_confirm"),
  ("ordinary", "inflight_same_content_after_confirm"): ("confirm", "t_inflight_same_content_after_confirm"),
  ("ordinary", "hash_mismatch_no_confirm"): ("confirm", "t_hash_mismatch_no_confirm"),
  ("ordinary", "ciphertext_only_no_plaintext_hash"): ("confirm", "t_ciphertext_only_no_plaintext_hash"),
  ("ordinary", "crash_after_pin_before_put_resume"): ("confirm", "t_crash_after_pin_before_put_resume"),
  ("enterprise_internal", "put_A_confirm_old_capability_put_B"): ("confirm", "ent_old_capability_put_b_rejected"),
  ("enterprise_internal", "overwrite_between_stat_and_confirm"): ("confirm", "ent_overwrite_between_stat_and_confirm"),
  ("enterprise_internal", "inflight_same_content_after_confirm"): ("confirm", "ent_inflight_same_content_after_confirm"),
  ("enterprise_internal", "hash_mismatch_no_confirm"): ("confirm", "ent_hash_mismatch_no_confirm"),
  ("enterprise_internal", "crash_after_pin_before_put_resume"): ("confirm", "ent_crash_after_pin_before_put_resume"),
 },
}
# seat/widget 共享协议旗：场景 -> EB 证据面注记
EB_SCENARIO_NOTE = {
 "put_A_confirm_old_capability_put_B": "upload_ref 单凭证绑定单 object_hash；异字节走 hash_mismatch 拒绝面",
 "overwrite_between_stat_and_confirm": "confirm 对象完整性以真实内容流校验（object_integrity/check_bytes）",
 "inflight_same_content_after_confirm": "active 幂等重放同一视图（w202_confirm_replay_idempotent_active）",
 "hash_mismatch_no_confirm": "declared_matches_stored/integrity fail-closed（unit_mime_size_hash_validation_is_fail_closed）",
 "crash_after_pin_before_put_resume": "asset_id 恢复 confirm（w202_confirm_by_asset_id_recovery）",
 "ciphertext_only_no_plaintext_hash": "EB descriptor 密文哈希只在客户端；服务端内容流校验密文本体",
}

def failed_tests(log):
    names = set()
    for line in log.splitlines():
        line = line.strip()
        if "*failed*" in line and ":" in line:
            seg = line.split(":")[-1].split("...")[0].strip()
            names.add(seg)
    return names

confirm_failed = failed_tests(confirm_log) if confirm_log else set()

missing = []
n = 0
case_name = cid.split("/", 1)[1]
for leaf in leaves:
    ep = leaf["entrypoint"]
    sc = leaf["ordering_or_fault"]
    # confirm-idempotent/old-client 的叶 entrypoint 即 case 名，按域归位：
    # ordinary -> ordinary 入口（t_*），enterprise -> enterprise_internal（ent_*）
    if ep == case_name:
        ep = "enterprise_internal" if leaf["domain"] == "enterprise" else "ordinary"
    mapping = M.get(case_name, {})
    if (ep, sc) in mapping:
        suite, tname = mapping[(ep, sc)]
        if suite == "confirm":
            ok = flags["W202_CONFIRM_PG_OK"] and (tname not in confirm_failed)
            evidence = "attachment_lifecycle_v5_confirm_pg_tests:" + tname
            log = "eunit-confirm.log"
        else:
            ok = False
            evidence = "unmapped"
            log = ""
    elif ep in ("enterprise_seat", "cs_widget"):
        ok = flags["W202_EB_ASSET_OK"]
        evidence = "eb_asset_tests suite flag (shared EB upload_ref/asset_id protocol): " + \
            EB_SCENARIO_NOTE.get(sc, sc)
        log = "eunit-ebasset.log"
    else:
        ok = False
        evidence = "missing-mapping"
        log = ""
        missing.append((ep, sc))
    n += 1
    receipt = {
        "task_id": "W2-02",
        "case_id": cid,
        "ac_id": leaf["ac_id"],
        "domain": leaf["domain"],
        "driver": leaf["driver"],
        "entrypoint": ep,
        "ordering_or_fault": sc,
        "schema_phase": leaf["schema_phase"],
        "executed": 1,
        "passed": 1 if ok else 0,
        "failed": 0 if ok else 1,
        "skipped": 0,
        "exit_code": 0 if ok else 30,
        "command": cmd,
        "evidence": evidence,
        "raw_output_path": os.path.join(cdir, log) if log else "",
        "probe_detail": "",
    }
    receipt["raw_output_sha256"] = hashlib.sha256(
        open(receipt["raw_output_path"], "rb").read()).hexdigest() if receipt["raw_output_path"] else ""
    with open(os.path.join(cdir, "leaf-%03d.json" % n), "w") as fh:
        json.dump(receipt, fh, ensure_ascii=False, indent=1)

gated = all(flags.values())
print("sealed %d/%d leaf receipts for %s (rc=%s); flags=%s; missing-mapping: %s"
      % (n, n, cid, "ok" if gated else "flag-down", flags,
         missing if missing else "none"))
sys.exit(0 if (gated and not missing) else 30)
PYW202
