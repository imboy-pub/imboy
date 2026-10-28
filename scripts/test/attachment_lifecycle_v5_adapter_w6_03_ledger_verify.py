#!/usr/bin/env python3
"""W6-03/ledger-verify measured adapter.

Five PLAN leaves verified read-only from real artifacts:

  exact_leaf_set              sealed leaf identity set vs frozen matrix for
                              every required case of G0..G5 (+ W6-03
                              final-regression), with the real `runner verify`
                              result for each gate
  candidate_source_config_hash frozen candidate heads/digests vs the recomputed
                              plan manifest and the real worktree HEADs
  actor_provenance            every sealed receipt's actor against the admission
                              allowlist (real driver match + W6-02 live
                              device/browser provenance)
  raw_output_hash             every receipt's raw output re-hashed from bytes
  mutation_battery            the real `runner self-test` result over the frozen
                              validator_mutations list

`final-candidate-summary.json` (a W6-03 task output) is written only after all
five leaves measure green, G0..G5 verify PASS, all prior tasks are PASS and an
independent review conclusion exists.  Otherwise no summary is written.
"""
import datetime
import json
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import attachment_lifecycle_v5_adapter_w6_support as S6  # noqa: E402

CASE_ID = "W6-03/ledger-verify"
NAME = "w6_03_ledger_verify"
GATES = ("G0", "G1", "G2", "G3", "G4", "G5")
OWN_CASE = "W6-03/ledger-verify"
FINAL_REGRESSION = "W6-03/final-regression"
REQUIRED_CANDIDATE = ("heads", "owned_tree_digest", "plan_manifest_sha256",
                      "migration_digest")
REVIEW_ARTIFACT = os.path.join("resources", "independent-review.json")


def _gate_results(case_dir, case_id):
    results = {}
    for gate in GATES:
        path = os.path.join(case_dir, "verify-%s.json" % gate)
        doc = S6.read_json(path, case_id, "runner verify %s" % gate)
        results[gate] = doc
    return results


def _sealed_receipts(run_dir, case_id, contract):
    """Return {case_id: [(filename, receipt)]} for every sealed leaf."""
    sealed = {}
    for task in contract["tasks"]:
        tid = task["task_id"]
        for cid in task.get("case_ids", []):
            if cid == OWN_CASE:
                continue
            if tid == "W6-03" and cid != FINAL_REGRESSION:
                continue
            directory = os.path.join(run_dir, "evidence", tid,
                                     cid.split("/", 1)[1])
            entries = []
            if os.path.isdir(directory):
                for name in sorted(os.listdir(directory)):
                    if not (name.startswith("leaf-") and name.endswith(".json")):
                        continue
                    path = os.path.join(directory, name)
                    try:
                        with open(path, "r", encoding="utf-8") as handle:
                            entries.append((name, json.load(handle), path))
                    except (OSError, ValueError) as exc:
                        S6.die(case_id, "unreadable sealed receipt %s (%s)"
                               % (path, exc))
            sealed[cid] = entries
    return sealed


def _real_head(repo):
    result = subprocess.run(
        ["git", "-C", repo, "rev-parse", "HEAD"],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if result.returncode != 0:
        S6.die(CASE_ID, "git rev-parse failed for %s: %s"
               % (repo, result.stderr.strip()))
    return result.stdout.strip()


def build():
    run_dir, case_dir, root = S6.C.argv_paths()
    ctx = S6.context(run_dir, CASE_ID, root)
    leaves = S6.leaves(ctx, CASE_ID)
    state = ctx["state"]
    allowlist = ctx["allowlist"]
    contract = ctx["contract"]
    candidate = state.get("verification_candidate") or {}
    gaps = []
    checks = {}

    sealed = _sealed_receipts(run_dir, CASE_ID, contract)
    by_task = {task["task_id"]: task for task in contract["tasks"]}

    # ---- exact_leaf_set (also consumes the real verify outputs) -------------
    gates = _gate_results(case_dir, CASE_ID)
    required_cids = []
    for gate in GATES:
        for tid in contract["wave_gates"][gate]["requires_tasks"]:
            for cid in by_task.get(tid, {}).get("case_ids", []):
                if cid not in required_cids:
                    required_cids.append(cid)
    # W0..W6-02 must PASS before W6-03, so their receipts are part of the
    # ledger even though they sit behind the future G6.
    for tid in ("W6-01", "W6-02"):
        for cid in by_task.get(tid, {}).get("case_ids", []):
            if cid not in required_cids:
                required_cids.append(cid)
    if FINAL_REGRESSION not in required_cids:
        required_cids.append(FINAL_REGRESSION)
    missing_cases = []
    total_expected = 0
    total_sealed = 0
    for cid in required_cids:
        if cid == OWN_CASE:
            continue
        expected = contract["expected_leaf_matrix"][cid]
        total_expected += len(expected)
        entries = sealed.get(cid) or []
        total_sealed += len(entries)
        expected_ids = {S6.identity(item) for item in expected}
        actual_ids = {S6.identity(row) for _n, row, _p in entries}
        if len(entries) != len(expected) or expected_ids != actual_ids:
            missing_cases.append("%s: %d/%d sealed"
                                 % (cid, len(entries), len(expected)))
    for gate in GATES:
        doc = gates[gate]
        if doc.get("result") != "PASS":
            gaps.append("runner verify %s result=%r fails=%r"
                        % (gate, doc.get("result"),
                           (doc.get("fails") or [])[:5]))
    if missing_cases:
        gaps.append("sealed leaf set incomplete for %d case(s): %s"
                    % (len(missing_cases), "; ".join(missing_cases[:6])))
    checks["exact_leaf_set"] = {
        "expected": total_expected, "sealed": total_sealed,
        "missing_cases": missing_cases,
    }

    # ---- candidate_source_config_hash --------------------------------------
    if not candidate:
        gaps.append("state.verification_candidate is missing; the candidate "
                    "heads/digests cannot be bound")
    else:
        absent = [field for field in REQUIRED_CANDIDATE
                  if not candidate.get(field)]
        if absent:
            gaps.append("candidate missing fields: %r" % absent)
        repo_heads = {}
        for name in ("imboy", "imboyapp", "imboyadmin"):
            repo = S6.worktree(ctx, name, CASE_ID)
            repo_heads[name] = _real_head(repo)
        heads = candidate.get("heads") or {}
        mismatched = [name for name, head in repo_heads.items()
                      if heads.get(name) != head]
        if mismatched:
            gaps.append("candidate heads differ from real worktree HEADs: %r"
                        % mismatched)
        try:
            sys.path.insert(0, os.path.join(root, "scripts"))
            import attachment_lifecycle_v5_runner as R  # noqa: E402
            manifest_ok, manifest_hash = R.verify_plan_manifest(
                state["plan_dir"])
            if not manifest_ok or manifest_hash != state.get(
                    "plan_manifest_sha256"):
                gaps.append("frozen plan manifest mismatch")
            if candidate.get("plan_manifest_sha256") != manifest_hash:
                gaps.append("candidate plan binding mismatch")
            errors = R.candidate_errors(candidate)
            if errors:
                gaps.append("candidate_errors: %r" % errors[:5])
        except ImportError as exc:
            gaps.append("cannot import the frozen runner for candidate "
                        "verification: %s" % exc)
        checks["candidate"] = {"repo_heads": repo_heads,
                               "candidate_heads": heads}

    # ---- actor_provenance ---------------------------------------------------
    actor_bad = []
    for cid, entries in sealed.items():
        for name, row, _path in entries:
            actor = row.get("actor_provenance") or {}
            kind = actor.get("resource_kind")
            rid = actor.get("resource_id") or ""
            allowed = allowlist.get(kind)
            ok = (actor.get("kind") in ("real", "targeted-contract")
                  and isinstance(allowed, list) and rid in allowed)
            if not ok:
                actor_bad.append("%s/%s actor=%r" % (cid, name, actor))
                continue
            if row.get("driver") in ("garage", "disk"):
                if actor.get("kind") != "real" \
                        or actor.get("driver") != row.get("driver"):
                    actor_bad.append("%s/%s real driver mismatch" % (cid, name))
            need_device = cid in ("W6-02/flutter-recovery-real",
                                  "W6-02/e2e-compat", "W6-02/ac08")
            need_browser = cid in ("W6-02/admin-recovery-real",
                                   "W6-02/e2e-compat", "W6-02/ac08")
            for needed, field, kind_name in (
                    (need_device, "device_provenance", "physical"),
                    (need_browser, "browser_provenance", "real-browser")):
                if not needed:
                    continue
                proof = row.get(field) or {}
                if (proof.get("kind") != kind_name
                        or proof.get("resource_id") not in
                        (allowlist.get(field) or [])
                        or type(proof.get("process_id")) is not int
                        or proof["process_id"] <= 0):
                    actor_bad.append("%s/%s missing live %s"
                                     % (cid, name, field))
    if actor_bad:
        gaps.append("%d sealed receipts have invalid actor provenance (e.g. %r)"
                    % (len(actor_bad), actor_bad[:4]))
    checks["actor_provenance"] = {"checked": total_sealed,
                                  "bad": len(actor_bad)}

    # ---- raw_output_hash ----------------------------------------------------
    hash_bad = []
    for cid, entries in sealed.items():
        for name, row, _path in entries:
            raw = row.get("raw_output_path") or ""
            if not raw or not os.path.isfile(raw):
                hash_bad.append("%s/%s raw output missing" % (cid, name))
                continue
            if os.path.commonpath([os.path.realpath(run_dir),
                                   os.path.realpath(raw)]) != os.path.realpath(
                                       run_dir):
                hash_bad.append("%s/%s raw output outside run dir"
                                % (cid, name))
                continue
            if S6.sha256_file(raw) != row.get("raw_output_sha256"):
                hash_bad.append("%s/%s raw sha mismatch" % (cid, name))
    if hash_bad:
        gaps.append("%d receipts fail raw-output re-hash (e.g. %r)"
                    % (len(hash_bad), hash_bad[:4]))
    checks["raw_output_hash"] = {"checked": total_sealed,
                                 "bad": len(hash_bad)}

    # ---- mutation_battery ---------------------------------------------------
    selftest_path = os.path.join(case_dir, "runner-selftest.json")
    selftest = S6.read_json(selftest_path, CASE_ID, "runner self-test")
    expected_mutations = len(contract["validator_mutations"])
    total = selftest.get("mutations_total")
    rejected = selftest.get("mutations_rejected")
    parallel = selftest.get("parallel_selftests") or {}
    holder = selftest.get("holder_hang_bounded_refusal") or {}
    if total != expected_mutations or rejected != total:
        gaps.append("mutation battery %r/%r rejected, contract declares %d"
                    % (rejected, total, expected_mutations))
    if not parallel or not all(value is True for value in parallel.values()):
        gaps.append("parallel self-tests not all true: %r" % parallel)
    if holder.get("passed") is not True:
        gaps.append("holder hang bounded refusal not proven: %r" % holder)
    checks["mutation_battery"] = {"total": total, "rejected": rejected,
                                  "expected": expected_mutations,
                                  "parallel": parallel, "holder": holder}

    if gaps:
        S6.report_gaps(CASE_ID, 0, {CASE_ID: (len(leaves), gaps)})

    # ---- build the five measured rows --------------------------------------
    selftest_window = S6.step_window(
        S6.step_record(case_dir, "ledger-selftest", CASE_ID), CASE_ID)
    verify_window = S6.step_window(
        S6.step_record(case_dir, "ledger-verify-G5", CASE_ID), CASE_ID)
    repo = S6.worktree(ctx, "imboy", CASE_ID)
    rows = []
    for leaf in leaves:
        scenario = leaf["ordering_or_fault"]
        checks_for = checks.get(scenario)
        if checks_for is None:
            S6.die(CASE_ID, "no ledger check implemented for %r" % scenario)
        started, finished = (selftest_window if scenario == "mutation_battery"
                             else verify_window)
        summary = {
            "exact_leaf_set": "expected=%d sealed=%d missing_cases=%d"
                              % (checks["exact_leaf_set"]["expected"],
                                 checks["exact_leaf_set"]["sealed"],
                                 len(checks["exact_leaf_set"]["missing_cases"])),
            "candidate_source_config_hash":
                "candidate heads %r match real HEADs; plan manifest %s"
                % (checks.get("candidate", {}).get("repo_heads"),
                   candidate.get("plan_manifest_sha256", "missing")),
            "actor_provenance": "checked=%d bad=%d"
                                % (checks["actor_provenance"]["checked"],
                                   checks["actor_provenance"]["bad"]),
            "raw_output_hash": "checked=%d bad=%d"
                               % (checks["raw_output_hash"]["checked"],
                                  checks["raw_output_hash"]["bad"]),
            "mutation_battery": "%r/%r rejected; parallel=%r; holder=%r"
                                % (checks["mutation_battery"]["rejected"],
                                   checks["mutation_battery"]["total"],
                                   checks["mutation_battery"]["parallel"],
                                   checks["mutation_battery"]["holder"]),
        }[scenario]
        assertions = [S6.assertion("ledger_check_%s" % scenario, summary)]
        if scenario == "exact_leaf_set":
            assertions.append(S6.assertion(
                "runner_verify_gates",
                "; ".join("%s=%s" % (gate, gates[gate].get("result"))
                          for gate in GATES)))
        if scenario == "mutation_battery":
            assertions.append(S6.assertion(
                "runner_selftest_artifact",
                "%s sha256=%s" % (os.path.basename(selftest_path),
                                  S6.sha256_file(selftest_path))))
        rows.append(S6.C.build_row(
            leaf, started, finished,
            {"exit_code": 0, "executed": 1, "passed": 1, "failed": 0,
             "skipped": 0},
            {
                "pre_db_state": "run dir %s with %d sealed leaf receipts across "
                                "the required G0..G5 cases" % (run_dir,
                                                               total_sealed),
                "pre_object_hash": "n/a: read-only ledger verification over "
                                   "receipt bytes and digests",
                "action": scenario,
                "expected_http_semantics": "n/a: no HTTP scenario is executed "
                                           "by the ledger verifier",
                "expected_db_delta": "n/a: no database mutation",
                "expected_object_state": "n/a: no storage object is read or "
                                         "written",
                "proof_actor": "repository %s; artifact %s"
                               % (repo, summary),
            },
            assertions, S6.repo_actor(repo), [repo],
            ["ledger:%s" % scenario]))
    S6.C.assert_leaf_set(leaves, rows)

    # ---- guarded W6-03 task output -----------------------------------------
    review_path = os.path.join(run_dir, REVIEW_ARTIFACT)
    task_states = {tid: item.get("state")
                   for tid, item in (state.get("tasks") or {}).items()}
    prior_ok = all(value == "PASS" for tid, value in task_states.items()
                   if tid != "W6-03")
    gates_ok = all(gates[gate].get("result") == "PASS" for gate in GATES)
    if prior_ok and gates_ok and os.path.isfile(review_path):
        summary = {
            "generated_at": datetime.datetime.now(
                datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "run_id": state.get("run_id"),
            "candidate": candidate,
            "plan_manifest_sha256": state.get("plan_manifest_sha256"),
            "baseline_heads": state.get("initial_heads"),
            "gates": {gate: gates[gate].get("result") for gate in GATES},
            "tasks": task_states,
            "ledger": checks,
            "independent_review": S6.read_json(review_path, CASE_ID,
                                               "independent review conclusion"),
            "verdict": "LOCAL_CANDIDATE_PASS",
        }
        target = os.path.join(
            root, "docs", "architecture", "attachment-lifecycle-v5",
            "evidence", "final-candidate-summary.json")
        S6.write_summary(target, summary, CASE_ID)
    return case_dir, rows


if __name__ == "__main__":
    S6.run_adapter(NAME, build)
