#!/usr/bin/env python3
"""W2-01/upload-content-pin adapter.

The case is in upload_surface_contract.cases, so every leaf also needs
entrypoint_provenance (route/method/handler/request_id) read from the real
case artifacts.  Per-entrypoint probe lines are matched strictly: the
pin section labels only ordinary lines, the ebp section labels seat/widget
lines, and no other entrypoint's line is substituted for a missing one.
"""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w2_support as C  # noqa: E402

CASE_ID = "W2-01/upload-content-pin"
NAME = "w2_01_upload_content_pin"

ORDINARY = {
    "first_content_pin": ["pin_first_content"],
    "two_different_payloads_race": ["pin_different_rejected"],
    "same_payload_retry": ["pin_same_retry"],
    "pin_commit_unknown_no_put": ["pin_persisted_without_put"],
    "pin_commit_recovered_same_hash": ["pin_same_hash_recovered"],
    "body_path_swap_rejected": ["pin_shape_swap_rejected"],
    "old_client_bare_put_gateway": ["pin_legacy_bare_put"],
    "multipart_uses_same_pin": ["pin_multipart_same_entrypoint"],
    "new_url_cannot_bypass_gateway": ["pin_no_url_columns"],
    "frozen_internal_signature_expiry": ["pin_frozen_signature_expiry"],
}
SURFACE = {
    "ordinary": ("pin", "ordinary"),
    "enterprise_internal": ("pin", "internal"),
    "enterprise_seat": ("ebp", "enterprise_seat"),
    "cs_widget": ("ebp", "cs_widget"),
}
DELTA = {
    "first_content_pin": "the full controlled bytes are read and the server-side hash is committed as the pin before any provider PUT",
    "two_different_payloads_race": "only one payload wins the content pin; the losing different-bytes request has provider PUT=0",
    "same_payload_retry": "the same content receipt retries idempotently against the committed pin",
    "pin_commit_unknown_no_put": "the pin is durable in the DB even though no provider PUT happened",
    "pin_commit_recovered_same_hash": "the same hash replay recovers the committed pin without a new capability",
    "body_path_swap_rejected": "a matching hash with a different size/mime body shape is rejected",
    "old_client_bare_put_gateway": "an old client's bare legacy PUT still goes through the gateway/ledger path",
    "multipart_uses_same_pin": "multipart registration shares the same content-pin entrypoint on its own row",
    "new_url_cannot_bypass_gateway": "no URL column exists on the pending table; all upload capability stays on the controlled gateway",
    "frozen_internal_signature_expiry": "the internal signer uses frozen absolute times and rejects an expired window",
}
HTTP = {
    "first_content_pin": "content pin happens before any provider PUT; gateway is the only upload surface",
    "two_different_payloads_race": "one winner; loser receives a rejection with no provider PUT",
    "same_payload_retry": "identical receipt is acknowledged idempotently",
    "pin_commit_unknown_no_put": "ambiguous commit resolves from the durable pin; no new signed URL",
    "pin_commit_recovered_same_hash": "same-hash recovery returns the committed pin state",
    "body_path_swap_rejected": "body/path shape swap is rejected before capability issuance",
    "old_client_bare_put_gateway": "legacy bare PUT cannot bypass the gateway/ledger",
    "multipart_uses_same_pin": "multipart uses the same pin entrypoint, no separate capability",
    "new_url_cannot_bypass_gateway": "no alternative URL can bypass the gateway",
    "frozen_internal_signature_expiry": "expired internal signature is rejected; frozen times are used",
}



def _s3sign_expected(repo_root):
    path = os.path.join(repo_root, "test", "lib", "elib_s3_sign_tests.erl")
    text = C.read_text(path, "elib_s3_sign_tests source")
    names = re.findall(r"^([a-z0-9_]+_test_)\(\) ->", text, re.M)
    if not names:
        C.gap("no elib_s3_sign_tests generators parsed from %s" % path)
    return names



def _provenance_summary(gaps, case_dir, leaves):
    """Collect provenance failures and return (entrypoint, scenario) proofs."""
    missing = []
    proofs = {}
    for leaf in leaves:
        collector = C.Gaps()
        proof = C.find_entrypoint_provenance(
            case_dir, leaf["entrypoint"], leaf["ordering_or_fault"], collector,
            "upload-content-pin")
        key = (leaf["entrypoint"], leaf["ordering_or_fault"])
        if proof is None:
            missing.append("%s/%s" % key)
        else:
            proofs[key] = proof
    if missing:
        gaps.add("entrypoint_provenance.route/method/handler/request_id absent "
                 "for %d/%d leaves (%s); CASE_DIR probe jsonl lines carry only "
                 "name/entrypoint/scenario/ac/pass and no log line carries an "
                 "ENTRYPOINT_PROVENANCE record"
                 % (len(missing), len(leaves), ", ".join(missing[:6])))
    return proofs


def build():
    run_dir, case_dir, root = C.argv_paths()
    started = C.now_utc()
    ctx = C.load_context(run_dir)
    leaves = C.case_leaves(ctx["contract"], CASE_ID)
    slot = C.resolve_slot(run_dir)
    db = C.db_resource(ctx["allowlist"], slot, "w201")
    bucket = C.garage_bucket(run_dir, ctx["allowlist"])
    migrations = C.w201_migration_count(case_dir)
    gaps = C.Gaps()
    pin = C.try_section(case_dir, "pin", gaps)
    ebp = C.try_section(case_dir, "ebp", gaps)
    sections = {"pin": pin, "ebp": ebp}

    s3sign_path = os.path.join(case_dir, "eunit-s3sign.log")
    s3sign = C.assert_eunit_total(s3sign_path, "elib_s3_sign_tests",
                                  len(_s3sign_expected(root)), gaps)
    eb_facts = C.eb_asset_evidence_facts(root)
    proofs = _provenance_summary(gaps, case_dir, leaves)

    rows = []
    for leaf in leaves:
        entrypoint = leaf["entrypoint"]
        scenario = leaf["ordering_or_fault"]
        if entrypoint not in SURFACE:
            gaps.add("unknown pin entrypoint=%r" % entrypoint)
            continue
        section, probe_ep = SURFACE[entrypoint]
        if entrypoint == "enterprise_internal":
            # The pin section currently labels its rows entrypoint=ordinary;
            # accept only a line that is itself labeled internal (forward
            # compatible with a probe that emits the internal entrypoint).
            accepted, names = C.require_probe_any(
                gaps, sections.get(section), section, probe_ep, scenario,
                "%s/%s" % (entrypoint, scenario))
            if not accepted:
                continue
        else:
            if entrypoint == "ordinary":
                names = ORDINARY.get(scenario)
            elif scenario == "new_url_cannot_bypass_gateway":
                names = (["ebp_no_url_columns"] if entrypoint == "enterprise_seat"
                         else ["ebw_no_url_columns"])
            else:
                names = None
            if not names:
                if entrypoint in ("enterprise_seat", "cs_widget"):
                    gaps.add("%s: scenario=%s has no per-scenario EB-chain "
                             "measurement; the only ebp.jsonl artifact is "
                             "ebp_shared_protocol emitted from the "
                             "W201_EB_ASSET_TESTS_OK suite flag, and %s "
                             "defines %d funs (%s) over a disk test root "
                             "(%s:%d)"
                             % (entrypoint, scenario,
                                os.path.basename(eb_facts["suite"]),
                                len(eb_facts["funs"]),
                                ", ".join(eb_facts["funs"][:5]),
                                os.path.basename(eb_facts["fixture"]),
                                eb_facts["fixture_line"]))
                else:
                    gaps.add("%s: no CASE_DIR probe line labeled "
                             "entrypoint=%s for scenario=%s (pin section labels "
                             "pin_* lines entrypoint=ordinary)"
                             % (entrypoint, probe_ep, scenario))
                continue
            accepted = C.require_probe(gaps, sections.get(section), section,
                                       probe_ep, scenario, names,
                                       "%s/%s" % (entrypoint, scenario))
            if len(accepted) != len(names):
                continue
        assertions = list(accepted)
        if sections.get(section) is not None:
            assertions.append(C.section_assertion(
                section, sections[section][1], sections[section][2]))
        assertions.append({
            "id": "migration_chain_replayed",
            "passed": True,
            "detail": "raw.log: w201 replayed %d up files; no SECTION_FAIL"
                      % migrations,
        })
        if scenario == "frozen_internal_signature_expiry" and s3sign:
            assertions.append({
                "id": "s3_sign_suite_named_tests_green",
                "passed": True,
                "detail": "%s reports %d/%d passed; suite defines "
                          "presign_put_ctx_uses_frozen_times_test_ and "
                          "presign_put_ctx_rejects_expired_window_test_"
                          % (s3sign["detail"], s3sign["total"],
                             s3sign["total"]),
            })
        oracle = {
            "pre_db_state": "fresh %s with %d replayed up migrations; %s "
                            "section ok=%d bad=0"
                            % (db, migrations, section,
                               sections[section][1].get("ok")),
            "pre_object_hash": ("n/a before action: the pin is computed over "
                                "the controlled bytes before any provider PUT "
                                "(no pre-existing object)"),
            "action": "psql pin/EB probe(s): %s" % ", ".join(names),
            "expected_http_semantics": HTTP[scenario],
            "expected_db_delta": DELTA[scenario],
            "expected_object_state": ("no provider PUT outside the controlled "
                                      "gateway; real Garage bucket %s only for "
                                      "byte round-trips" % bucket),
            "proof_actor": "psql imboy_user on %s + real garage bucket %s "
                           "(actor driver=garage)" % (db, bucket),
        }
        actor = {"kind": "real", "driver": "garage",
                 "resource_kind": "garage_bucket", "resource_id": bucket}
        proof = proofs.get((entrypoint, scenario))
        request_ids = ["w201:%s:%s" % (section, name) for name in names]
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
            {"exit_code": 0, "executed": len(accepted), "passed": len(accepted),
             "failed": 0, "skipped": 0},
            oracle, assertions, actor, [bucket, db], request_ids,
            provenance=proof))
    gaps.check()
    C.assert_leaf_set(leaves, rows)
    return case_dir, rows


if __name__ == "__main__":
    C.run_adapter(NAME, build)
