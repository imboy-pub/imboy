#!/usr/bin/env python3
"""W0-03/runner-selftest adapter: measured per-leaf evidence from self-test output.

The gate executor runs exactly one command:
    python3 scripts/attachment_lifecycle_v5_runner.py self-test > <case>/raw.log
cmd_selftest emits a single JSON summary (mutation aggregate, 4 parallel
sub-checks, holder-hang result). It does NOT emit per-mutation rows and does not
exercise timeout/crash/restart/lease/budget scenarios at all.

This adapter therefore:
  * verifies the aggregate is complete and the runner's 34 MUTATIONS match the
    contract's validator_mutations in order (so the aggregate entails every
    mutation_N leaf),
  * maps only the named leaves that the summary actually measures,
  * exits 30 listing every leaf the artifact cannot support. It never turns an
    aggregate or a missing scenario into a per-leaf PASS.
"""
import datetime
import hashlib
import json
import os
import re
import sys

CASE_ID = "W0-03/runner-selftest"
RUN_DIR = os.path.abspath(sys.argv[1])
CASE_DIR = os.path.abspath(sys.argv[2])
ROOT = os.path.abspath(sys.argv[3])

STARTED = datetime.datetime.now(datetime.timezone.utc)


def fail(gaps):
    if isinstance(gaps, str):
        gaps = [gaps]
    for gap in gaps:
        sys.stderr.write("w0_03_runner_selftest adapter: %s\n" % gap.rstrip())
    sys.exit(30)


def load_json(path, what):
    try:
        with open(path, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError) as exc:
        fail("cannot read %s at %s: %s" % (what, path, exc))


def read_text(path, what):
    try:
        with open(path, encoding="utf-8", errors="replace") as fh:
            return fh.read()
    except OSError as exc:
        fail("cannot read %s at %s: %s" % (what, path, exc))


def preflight():
    state = load_json(os.path.join(RUN_DIR, "state.json"), "run state")
    candidate = state.get("verification_candidate")
    if not isinstance(candidate, dict) or not candidate.get("heads"):
        fail("run is not frozen: state.json has no verification_candidate/heads")
    allow = state.get("resource_allowlist")
    if not isinstance(allow, dict) or not allow:
        fail("state.json has no resource_allowlist (admission snapshot incomplete)")
    return state, candidate, allow


def main():
    state, candidate, allow = preflight()

    repo_root = candidate.get("repository_paths", {}).get("imboy")
    if not repo_root:
        fail("verification_candidate has no repository_paths.imboy")
    if repo_root not in (allow.get("repository") or []):
        fail("candidate imboy path %r is not in resource_allowlist.repository"
             % (repo_root,))

    contract = load_json(os.path.join(state["plan_dir"], "execution-contract.json"),
                         "frozen execution contract")
    leaves = contract.get("expected_leaf_matrix", {}).get(CASE_ID)
    if not leaves:
        fail("contract expected_leaf_matrix has no %r" % CASE_ID)
    contract_mutations = contract.get("validator_mutations")
    if not isinstance(contract_mutations, list) or not contract_mutations:
        fail("contract has no validator_mutations list")

    runner_path = os.path.join(ROOT, "scripts/attachment_lifecycle_v5_runner.py")
    source = read_text(runner_path, "runner source")

    block = re.search(r"\nMUTATIONS = \[(.*?)\n\]\n", source, re.S)
    if not block:
        fail("runner source has no parsable MUTATIONS list")
    source_mutations = re.findall(r'\(\s*"([^"]+)"', block.group(1))
    par_start = source.find("def _parallel_selftests")
    par_end = source.find("def _holder_hang_selftest")
    if par_start < 0 or par_end <= par_start:
        fail("runner source has no parsable _parallel_selftests block")
    declared_parallel = re.findall(r'record\("([^"]+)"', source[par_start:par_end])

    if source_mutations != contract_mutations:
        fail("runner MUTATIONS (%d) do not match contract validator_mutations (%d) "
             "in order; aggregate rejection cannot be attributed to named mutations"
             % (len(source_mutations), len(contract_mutations)))

    raw_path = os.path.join(CASE_DIR, "raw.log")
    raw = read_text(raw_path, "self-test raw.log")
    start = raw.find("{")
    if start < 0:
        fail("raw.log contains no JSON self-test summary")
    try:
        summary, consumed = json.JSONDecoder().raw_decode(raw[start:])
    except ValueError as exc:
        fail("raw.log self-test summary is not valid JSON: %s" % exc)
    if not isinstance(summary, dict):
        fail("raw.log self-test summary is not a JSON object")
    trailing = raw[start + consumed:].strip()
    for key in ("mutations_total", "mutations_rejected", "parallel_selftests",
                "holder_hang_bounded_refusal"):
        if key not in summary:
            fail("self-test summary lacks required key %r (raw.log truncated or "
                 "cmd_selftest output changed)" % key)

    total = summary["mutations_total"]
    rejected = summary["mutations_rejected"]
    parallel = summary["parallel_selftests"]
    holder = summary["holder_hang_bounded_refusal"]
    if type(total) is not int or type(rejected) is not int:
        fail("mutations_total/mutations_rejected are not integers")
    if not isinstance(parallel, dict) or not isinstance(holder, dict):
        fail("parallel_selftests/holder_hang_bounded_refusal are not objects")

    declared_ok = (total == rejected == len(contract_mutations) == len(source_mutations)
                   and total > 0)
    parallel_missing = [name for name in declared_parallel
                        if parallel.get(name) is not True]
    if parallel_missing:
        fail("self-test parallel sub-checks are missing/false: %r (summary=%r)"
             % (parallel_missing, parallel))
    holder_detail = str(holder.get("detail", ""))
    took_match = re.search(r"took=([0-9.]+)s", holder_detail)
    took = float(took_match.group(1)) if took_match else None
    state_untouched = "state_untouched=True" in holder_detail
    holder_ok = holder.get("passed") is True and took is not None and took <= 2.5

    evidence = {}
    for index, name in enumerate(contract_mutations, 1):
        evidence["mutation_%d" % index] = {
            "id": "validator_mutation_%d_rejected" % index,
            "passed": declared_ok,
            "detail": ("cmd_selftest mutations_total=%d mutations_rejected=%d over "
                       "%d declared validators; contract validator_mutations[%d]=%r "
                       "matches runner MUTATIONS order, so aggregate rejection "
                       "entails this mutation (per-mutation rows are not printed)"
                       % (total, rejected, len(contract_mutations),
                          index - 1, name)),
        }

    if holder_ok:
        evidence["live_holder_sigstop_bounded_refusal"] = {
            "id": "live_holder_sigstop_bounded_refusal_measured",
            "passed": True,
            "detail": "holder_hang_bounded_refusal=%r" % (holder,),
        }
        evidence["writer_lock_held_past_lease"] = {
            "id": "live_holder_lock_refusal_bounded",
            "passed": True,
            "detail": ("SIGSTOPped live holder still owns the run lock; a resume "
                       "attempt is refused rc=75 in %s s (<=2.5s) with "
                       "state_untouched=%s" % (took, state_untouched)),
        }
    for name in declared_parallel:
        if "second-writer" in name and parallel.get(name) is True:
            evidence["parallel_overlap_rejected"] = {
                "id": "parallel_overlap_rejected_measured",
                "passed": True,
                "detail": "parallel_selftests[%r]=True" % (name,),
            }
            break
    if declared_ok:
        evidence["receipt_wrong_plan_or_scope"] = {
            "id": "receipt_wrong_plan_or_scope_rejected",
            "passed": True,
            "detail": ("mutations 3,4,5,6 (rename case_id / swap backend or queue / "
                       "change candidate SHA / change migration or plan hash) are "
                       "inside the %d/%d rejected aggregate" % (rejected, total)),
        }
        evidence["planned_successor_regression_uses_current_budget"] = {
            "id": "budget_relabel_mutation_rejected",
            "passed": True,
            "detail": ("validator mutation 34 %r rejected inside the %d/%d aggregate"
                       % (contract_mutations[33], rejected, total)),
        }

    # 19 contract-leaf scenarios measured by _scenario_selftests (real
    # subprocess/StateStore/lock drills reported in the self-test summary).
    scenarios = summary.get("scenarios")
    if not isinstance(scenarios, dict) or not scenarios:
        fail("self-test summary lacks the scenarios block (cmd_selftest predates "
             "_scenario_selftests?); 19 contract leaves cannot be measured")
    scenario_bad = sorted(name for name, row in scenarios.items()
                          if not (isinstance(row, dict) and row.get("passed") is True))
    if scenario_bad:
        fail("self-test scenarios did not pass: %r" % (scenario_bad,))
    for name, row in sorted(scenarios.items()):
        evidence[name] = {
            "id": "scenario_%s_measured" % name,
            "passed": True,
            "detail": "scenarios[%r].detail=%r" % (name, row.get("detail", "")),
        }

    uncovered = [leaf.get("ordering_or_fault") for leaf in leaves
                 if leaf.get("ordering_or_fault") not in evidence]
    if uncovered:
        gaps = [
            "self-test raw.log cannot support %d of %d leaves: %r"
            % (len(uncovered), len(leaves), uncovered),
            "cmd_selftest only emits: 34-mutation aggregate (%d/%d rejected), "
            "4 parallel sub-checks %r, holder-hang result %r; there is no "
            "per-scenario measurement for valid_receipt, timeout/crash, parent/child "
            "death, deadline restart, state-ahead-of-log, lease expiry, budget "
            "preflight, observation exclusive-create, orphan-group recovery, or "
            "disjoint/worker-crash parallel scenarios"
            % (rejected, total, sorted(parallel), holder),
            "contract case_registry oracle requires 'valid synthetic receipt "
            "accepted' and 'timeout and crash consume persisted attempt', which the "
            "current self-test command does not exercise; a PASS here would be "
            "fabricated",
        ]
        if trailing:
            gaps.append("unexpected non-JSON text after the summary: %r"
                        % (trailing[:200],))
        fail(gaps)

    output_leaves = []
    for leaf in leaves:
        ordering = leaf.get("ordering_or_fault")
        assertion = evidence[ordering]
        if not assertion["passed"]:
            fail("leaf %r has a measured assertion that did not pass" % ordering)
        output_leaves.append({
            "leaf": leaf,
            "started_at": STARTED.strftime("%Y-%m-%dT%H:%M:%SZ"),
            "finished_at": datetime.datetime.now(
                datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "exit_code": 0,
            "executed": 1,
            "passed": 1,
            "failed": 0,
            "skipped": 0,
            "oracle_observations": {
                "pre_db_state": "n/a (plan-domain runner self-test; no database)",
                "pre_object_hash": "runner source sha256=%s (MUTATIONS=%d declared, "
                                   "parallel=%d declared)"
                                   % (hashlib.sha256(source.encode()).hexdigest(),
                                      len(source_mutations), len(declared_parallel)),
                "action": "python3 scripts/attachment_lifecycle_v5_runner.py self-test",
                "expected_http_semantics": "n/a (no HTTP surface)",
                "expected_db_delta": "none (runner state lives in the run workspace)",
                "expected_object_state": "mutations_rejected=%d/%d; parallel=%r; "
                                         "holder=%r"
                                         % (rejected, total, parallel, holder),
                "proof_actor": "main-integrator",
            },
            "assertions": [assertion],
            "actor": {"kind": "targeted-contract", "driver": leaf.get("driver"),
                      "resource_kind": "repository", "resource_id": repo_root},
            "resource_ids": [repo_root],
            "request_ids": ["w0-03-selftest-%s" % ordering],
        })

    output = os.path.join(CASE_DIR, "observations.json")
    with open(output, "w", encoding="utf-8") as fh:
        json.dump({"leaves": output_leaves}, fh, ensure_ascii=False, indent=1)
    print(json.dumps({"adapter": "w0_03_runner_selftest",
                      "leaves": len(output_leaves), "output": output}))


if __name__ == "__main__":
    main()
