#!/usr/bin/env python3
"""W2-02/confirmed-content-immutable adapter.

Ordinary/enterprise-internal leaves are mapped to the named real-PG +
real-Garage immutable tests.  Seat/widget leaves need an EB-chain
real-Garage immutable measurement; the current EB suite installs a disk
test root, so those leaves are reported as evidence gaps rather than
silently reusing another entrypoint.  The case is also listed in
upload_surface_contract.cases, so every leaf additionally needs
entrypoint_provenance from the case artifacts.
"""
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-02/confirmed-content-immutable"
NAME = "w2_02_confirmed_content_immutable"
TESTS = {
    "put_A_confirm_old_capability_put_B": "old_capability_put_b_rejected",
    "overwrite_between_stat_and_confirm": "overwrite_between_stat_and_confirm",
    "inflight_same_content_after_confirm": "inflight_same_content_after_confirm",
    "hash_mismatch_no_confirm": "hash_mismatch_no_confirm",
    "ciphertext_only_no_plaintext_hash": "ciphertext_only_no_plaintext_hash",
    "crash_after_pin_before_put_resume": "crash_after_pin_before_put_resume",
}
DELTA = {
    "put_A_confirm_old_capability_put_B": "after confirm, the old capability's PUT of different bytes B cannot change the confirmed object; the confirmed hash stays A",
    "overwrite_between_stat_and_confirm": "bytes overwritten between stat and confirm are detected; confirm is rejected and no attachment row is created",
    "inflight_same_content_after_confirm": "an in-flight write of the same content after confirm leaves confirmed bytes and metadata unchanged",
    "hash_mismatch_no_confirm": "a declared/content hash mismatch produces no confirm and no confirmed row",
    "ciphertext_only_no_plaintext_hash": "the ciphertext sha256 is stored as file_hash256 and confirm succeeds with cipher metadata only; no plaintext hash is input to the server",
    "crash_after_pin_before_put_resume": "pin committed without PUT makes confirm fail object_not_found with no attachment row; replaying the same pin plus a real PUT then confirms and replays the same attachment id",
}
OBJECT = {
    "put_A_confirm_old_capability_put_B": "confirmed object bytes remain hash A; provider PUT of B has no effect",
    "overwrite_between_stat_and_confirm": "the overwritten bytes are not adopted; the original hash adjudication rejects confirm",
    "inflight_same_content_after_confirm": "same-content in-flight write leaves the confirmed object byte-identical",
    "hash_mismatch_no_confirm": "no object is adopted because the measured full-byte hash differs",
    "ciphertext_only_no_plaintext_hash": "the confirmed object hash equals the ciphertext sha256 (no plaintext hash appears)",
    "crash_after_pin_before_put_resume": "the resumed PUT carries the pinned bytes; the same object hash is confirmed",
}


def _provenance_summary(gaps, case_dir, leaves):
    missing = []
    proofs = {}
    for leaf in leaves:
        collector = C.Gaps()
        proof = C.find_entrypoint_provenance(
            case_dir, leaf["entrypoint"], leaf["ordering_or_fault"], collector,
            "confirmed-content-immutable")
        key = (leaf["entrypoint"], leaf["ordering_or_fault"])
        if proof is None:
            missing.append("%s/%s" % key)
        else:
            proofs[key] = proof
    if missing:
        gaps.add("entrypoint_provenance.route/method/handler/request_id absent "
                 "for %d/%d leaves (%s); eunit-confirm.log has no HTTP "
                 "route/handler/request_id record and no ENTRYPOINT_PROVENANCE "
                 "marker"
                 % (len(missing), len(leaves), ", ".join(missing[:6])))
    return proofs


def _eb_gap(gaps, leaf, facts):
    gaps.add("%s: no EB-chain real-Garage named test for scenario=%s; %s "
             "defines %d funs (%s) and %s:%d installs a disk test root "
             "(file:make_dir), so confirmed-byte immutability on the garage "
             "driver is not measured for this entrypoint"
             % (leaf["entrypoint"], leaf["ordering_or_fault"],
                os.path.basename(facts["suite"]), len(facts["funs"]),
                ", ".join(facts["funs"][:5]),
                os.path.basename(facts["fixture"]), facts["fixture_line"]))


def build():
    run_dir, case_dir, root = C.argv_paths()
    started = C.now_utc()
    ctx = C.load_context(run_dir)
    leaves = C.case_leaves(ctx["contract"], CASE_ID)
    slot = C.resolve_slot(run_dir)
    pool = C.db_resource(ctx["allowlist"], slot, "pool")
    bucket = C.garage_bucket(run_dir, ctx["allowlist"])
    gaps = C.Gaps()
    suite = C.confirm_suite_names(root)
    log_path = os.path.join(case_dir, "eunit-confirm.log")
    result = C.assert_eunit_total(log_path, "W2-02 confirm suite",
                                  suite["total"], gaps)
    text = C.read_text(log_path, "W2-02 confirm suite log")
    hash_checks = C.ANSI_RE.sub("", text).count("content_hash_mismatch")
    if hash_checks < 1:
        gaps.add("%s has no content_hash_mismatch adjudication line; the "
                 "real full-byte hash rejection path is not evidenced"
                 % log_path)
    eb_facts = C.eb_asset_evidence_facts(root)
    proofs = _provenance_summary(gaps, case_dir, leaves)
    rows = []
    for leaf in leaves:
        domain = leaf["domain"]
        entrypoint = leaf["entrypoint"]
        scenario = leaf["ordering_or_fault"]
        base = TESTS.get(scenario)
        if not base:
            gaps.add("no confirmed-content-immutable mapping for scenario=%s"
                     % scenario)
            continue
        if domain == "enterprise" and entrypoint in (
                "enterprise_seat", "cs_widget"):
            _eb_gap(gaps, leaf, eb_facts)
            continue
        test = ("t_" if domain == "ordinary" else "ent_") + base
        allowed = suite["ordinary"] if domain == "ordinary" else suite["enterprise"]
        if test not in allowed:
            gaps.add("suite source %s does not define %s for domain=%s "
                     "entrypoint=%s" % (suite["path"], test, domain,
                                        entrypoint))
            continue
        line = C.source_line(suite["text"], test + "()")
        if line is None:
            gaps.add("cannot locate %s in %s" % (test, suite["path"]))
            continue
        if result is None:
            continue
        assertions = [
            {
                "id": "confirm_suite_exact_total_passed",
                "passed": True,
                "detail": "%s == %d tests defined by the ordinary(%d) + "
                          "enterprise(%d) generators"
                          % (result["detail"], suite["total"],
                             len(suite["ordinary"]),
                             len(suite["enterprise"])),
            },
            {
                "id": "named_scenario_test_present",
                "passed": True,
                "detail": "%s:%d defines %s() (non-verbose suite log has no "
                          "per-test names; exact-total pass is the execution "
                          "evidence)" % (os.path.basename(suite["path"]), line,
                                         test),
            },
            {
                "id": "real_garage_byte_adjudication_present",
                "passed": True,
                "detail": "eunit-confirm.log contains %d content_hash_mismatch "
                          "adjudication line(s); real Garage bucket %s"
                          % (hash_checks, bucket),
            },
        ]
        oracle = {
            "pre_db_state": "attv5_%s_pool on the isolated PostgreSQL; each "
                            "test creates fresh fixtures via the real fn_v5_* "
                            "path" % slot,
            "pre_object_hash": "suite-controlled real Garage bytes in bucket %s; "
                               "elib_oss:object_sha256 full-byte read, log "
                               "content_hash_mismatch lines=%d"
                               % (bucket, hash_checks),
            "action": "make eunit-local t=attachment_lifecycle_v5_confirm_pg_tests"
                      " -> %s" % test,
            "expected_http_semantics": "confirm through the production logic "
                                       "entry (attach_logic:confirm/5 / "
                                       "enterprise_asset_logic:confirm_tx/3); "
                                       "the suite has no HTTP surface",
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": OBJECT[scenario],
            "proof_actor": "eunit u9 / enterprise fixture tenant; real PG "
                           "attv5_%s_pool + real Garage bucket %s"
                           % (slot, bucket),
        }
        actor = {"kind": "real", "driver": "garage",
                 "resource_kind": "garage_bucket", "resource_id": bucket}
        proof = proofs.get((entrypoint, scenario))
        request_ids = ["eunit:attachment_lifecycle_v5_confirm_pg_tests:%s" % test]
        if proof:
            request_ids.insert(0, proof["request_id"])
            assertions.append({
                "id": "entrypoint_provenance_parsed",
                "passed": True,
                "detail": "%s entrypoint=%s route=%s method=%s handler=%s "
                          "request_id=%s"
                          % (proof.get("artifact"), entrypoint, proof["route"],
                             proof["method"], proof["handler"],
                             proof["request_id"]),
            })
        rows.append(C.build_row(
            leaf, started, C.now_utc(),
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            oracle, assertions, actor, [bucket, pool], request_ids,
            provenance=proof))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
