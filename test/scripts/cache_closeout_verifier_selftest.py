#!/usr/bin/env python3
"""Isolated positive/negative CLI regressions for the C14 verifier."""
import hashlib
import json
import os
import subprocess
import sys
import tempfile
from datetime import datetime, timedelta, timezone

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "scripts"))
from check_cache_closeout_v1 import (TASK_ID, EXPECTED_CLOSEOUT_PLAN_SHA256,
    EXPECTED_ORIGINAL_PLAN_SHA256, DEFAULT_CLOSEOUT_PLAN, DEFAULT_ORIGINAL_PLAN,
    SCRIPT_PATH, CHECK_ORDER, DEFAULT_REQUIRED_IDS, sha256_file, load_json, _read_file,
    candidate_inputs, parse_utc)

SELFTEST_ROOT = "/tmp/check_cache_closeout_fixtures"
FIXTURE_ROWS = [{"path": "Makefile", "sha256": hashlib.sha256(b"# fixture\n").hexdigest()}]
CAND_A = hashlib.sha256(json.dumps(FIXTURE_ROWS, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
CAND_B = "cd" * 32


def _now_z():
    now = datetime.now(timezone.utc)
    return now.strftime("%Y-%m-%dT%H:%M:%S.") + "%03dZ" % (now.microsecond // 1000)


def _now_offset():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%S+00:00")


def _base_ids_doc():
    return load_json(DEFAULT_REQUIRED_IDS)[0]


def _base_ledger():
    return {"task_id": TASK_ID, "generated_at_utc": _now_offset(),
            "candidate_sha256": CAND_A,
            "entries": [{"id": e["id"], "kind": e["kind"]} for e in _base_ids_doc()["ids"]]}


def _base_baseline():
    return {"task_id": TASK_ID, "generated_at_utc": _now_offset(),
            "plan_binding": {
                "closeout_plan_sha256": EXPECTED_CLOSEOUT_PLAN_SHA256,
                "expected_closeout_sha256": EXPECTED_CLOSEOUT_PLAN_SHA256,
                "hash_match": True,
                "original_plan_sha256": EXPECTED_ORIGINAL_PLAN_SHA256,
                "expected_original_sha256": EXPECTED_ORIGINAL_PLAN_SHA256}}


def _evidence_doc(candidate, statuses):
    scenarios = []
    for i, st in enumerate(statuses, 1):
        scenarios.append({"name": "scenario-%02d" % i, "status": st,
                          "detail": "" if st == "pass" else "induced failure"})
    return {"driver_node": "selftest@nohost", "utc": _now_z(),
            "elapsed_ms": 42100, "candidate_sha256": candidate,
            "counts": {"passed": statuses.count("pass"), "failed": statuses.count("fail"), "cancelled": 0, "skipped": 0},
            "scenarios": scenarios}


def _performance_result(candidate, arm, round_no):
    def scenario(name, **values):
        return dict({"name": name, "status": "pass", "detail": "fixture oracle"}, **values)
    if arm == "env":
        scenarios = [scenario("environment_anchor", artifacts={})]
    else:
        scenarios = [scenario("profile_self_check", checks={
            "memo_single_flight": True, "invalidation_race": True,
            "loader_exception": True, "writer_crash": True,
            "get_wait_handoff": True}),
            scenario("throughput", ops_per_sec=1000 if arm == "base" else 980,
                errors=0, error_rate=0, operations=10000),
            scenario("p99", get_us=1000 if arm == "base" else 1050,
                set_us=2000, get_p50_us=100, get_p95_us=300,
                set_p50_us=100, set_p95_us=300, errors=0, error_rate=0,
                operations=10000),
            scenario("convergence", set_ms=100, errors=0, error_rate=0,
                operations=10000),
            scenario("resource_envelope", mailbox_peak=1, heap_peak_bytes=1,
                metadata_peak_bytes=1, budget_usage={"n_count": 1, "n_bytes": 1,
                    "t_count": 1, "t_bytes": 1, "journal_count": 1,
                    "journal_bytes": 1, "buffer_count": 1, "buffer_bytes": 1,
                    "page_count": 1, "page_bytes": 1, "generation_count": 1,
                    "generation_bytes": 1, "grace_count": 1, "peer_count": 1})]
    profile = {"operations": 10000, "concurrency": [1, 8, 32], "keyspace": 1024,
        "value_bytes": 256, "ttl_seconds": 3600 if arm == "env" else 600}
    return {"candidate_sha256": candidate, "counts": {"passed": len(scenarios),
        "failed": 0, "cancelled": 0, "skipped": 0}, "utc": _now_z(),
        "elapsed_ms": 1, "arm": arm, "round": round_no,
        "profile_sha256": hashlib.sha256(json.dumps(profile, sort_keys=True,
            separators=(",", ":")).encode()).hexdigest(),
        "budget_digest": "budget_v1: Nmax=50000 Nmax_bytes=16MiB Tmax=20000 Tmax_bytes=8MiB Bmax=2048 Bmax_bytes=32MiB Pmax=256 Pmax_bytes=4MiB Rmax=2 Rmax_rounds=3 Dmax=300s MaxValue=32768B MaxTTL=600s HighWater=10000 W=720s",
        "operations": 10000, "operations_per_worker": 10000,
        "concurrency": [1, 8, 32], "keyspace": 1024,
        "value_bytes": 256, "ttl_seconds": 3600 if arm == "env" else 600,
        "load": {"before_1m": 1, "after_1m": 1, "before_15m": 2, "after_15m": 2},
        "parallel_full_load": False, "compile_exit": 0, "run_exit": 0,
        "scenarios": scenarios}


def _write_json(path, obj):
    with open(path, "w", encoding="utf-8") as f:
        json.dump(obj, f, indent=1, ensure_ascii=False)


def _write_case(dest, closeout_bytes, original_bytes, evidences,
                ids_doc=None, ledger=None):
    evidences = dict(evidences)
    for arm, round_no in [("env", 1)] + [(arm, n) for n in range(1, 4)
                                          for arm in ("base", "cand")]:
        name = "perf_%s_r%d.json" % (arm, round_no)
        evidences.setdefault(name, _performance_result(CAND_A, arm, round_no))
    raw = os.path.join(dest, "raw")
    os.makedirs(raw, exist_ok=True)
    _write_json(os.path.join(dest, "required-ids.json"),
                ids_doc if ids_doc is not None else _base_ids_doc())
    _write_json(os.path.join(dest, "ledger-v1.json"),
                ledger if ledger is not None else _base_ledger())
    _write_json(os.path.join(dest, "baseline.json"), _base_baseline())
    with open(os.path.join(dest, "closeout-plan.md"), "wb") as f:
        f.write(closeout_bytes)
    with open(os.path.join(dest, "plan-v2.2.1.md"), "wb") as f:
        f.write(original_bytes)
    anchor_dir = os.path.join(raw, "w0_anchor")
    os.makedirs(anchor_dir)
    for role in ("env", "w0_r7", "w0_r8"):
        role_dir = os.path.join(anchor_dir, role)
        os.makedirs(role_dir)
        baseline = {"compile_exit": 0, "run_exit": 0, "operations": 10000,
            "dependency_hashes": {"deps/%02d.erl" % i: ("12" * 32) for i in range(28)},
            "script_hashes": {"scripts/%s.erl" % name: "34" * 32
                              for name in ("runner", "bench")}}
        kinds = ("get", "get_wait", "set", "mixed", "memo", "syn")
        rows = [{"kind": kind, "workers": workers, "operations": workers * 10000,
                 "throughput_ops_sec": 1000,
                 "p50_us": 100, "p95_us": 300, "p99_us": 500,
                 "errors": 0, "error_rate": 0}
                for kind in kinds for workers in (1, 8, 32)]
        performance = {"operations_per_worker": 10000, "keyspace": 1024,
            "value_bytes": 256, "ttl_seconds": 3600, "single_node": rows,
            "three_node": {"status": "measured", "nodes": 3,
                "results": [{"workers": workers, "operations": workers * 10000,
                    "throughput_ops_sec": 1000,
                    "p50_us": 100, "p95_us": 300, "p99_us": 500,
                    "errors": 0, "error_rate": 0} for workers in (1, 8, 32)]}}
        baseline_path = os.path.join(role_dir, "baseline.json")
        performance_path = os.path.join(role_dir, "performance.json")
        hashes_path = os.path.join(role_dir, "sha256.json")
        _write_json(baseline_path, baseline)
        _write_json(performance_path, performance)
        _write_json(hashes_path, {"baseline.json": sha256_file(baseline_path),
                                  "performance.json": sha256_file(performance_path)})
        artifacts = {}
        for kind, path in (("baseline", baseline_path), ("performance", performance_path),
                           ("sha256", hashes_path)):
            artifacts[kind] = {"path": "w0_anchor/%s/%s.json" % (role, kind),
                               "sha256": sha256_file(path)}
        if role == "env":
            env_result = evidences["perf_env_r1.json"]
            env_anchor = next(s for s in env_result["scenarios"]
                              if s["name"] == "environment_anchor")
            env_anchor["artifacts"][role] = artifacts
        else:
            env_anchor["artifacts"][role] = artifacts
    for fname, obj in evidences.items():
        _write_json(os.path.join(raw, fname), obj)
    source = os.path.join(dest, "source")
    os.makedirs(source)
    subprocess.run(["git", "init", "-q", source], check=True)
    with open(os.path.join(source, "Makefile"), "wb") as f:
        f.write(b"# fixture\n")
    freeze = (datetime.now(timezone.utc) - timedelta(minutes=1)).isoformat()
    _write_json(os.path.join(dest, "candidate-manifest.json"),
                {"files": FIXTURE_ROWS, "candidate_sha256": CAND_A, "frozen_at_utc": freeze})
    refs = []
    fixture_gates = ["compile", "targeted", "three_node", "mixed", "rollback", "qr", "full_eunit", "performance", "independent_review"]
    perf_names = ["perf_env_r1.json"] + ["perf_%s_r%d.json" % (arm, n)
        for n in range(1, 4) for arm in ("base", "cand")]
    performance_start = datetime.now(timezone.utc).replace(microsecond=0) - timedelta(seconds=30)
    performance_intervals = {}
    for position, name in enumerate(perf_names):
        started = performance_start + timedelta(seconds=position * 2)
        finished = started + timedelta(seconds=1)
        evidences[name]["utc"] = (started + timedelta(milliseconds=500)).isoformat()
        performance_intervals[name] = (started, finished)
        _write_json(os.path.join(raw, name), evidences[name])
    for i, gate in enumerate(fixture_gates):
        selected = ([(name, evidences[name]) for name in perf_names]
                    if gate == "performance" else
                    list(evidences.items())[:2][i % 2:i % 2 + 1])
        for fname, obj in selected:
            index = len(refs)
            if gate == "performance":
                started, finished = performance_intervals[fname]
            else:
                started = finished = datetime.now(timezone.utc)
            counts = obj.get("counts", {"passed": 0, "failed": 0, "cancelled": 0, "skipped": 0})
            log = os.path.join(dest, "command-%d.log" % index)
            with open(log, "w") as f:
                if gate == "full_eunit":
                    f.write("All %d tests passed.\n" % counts["passed"])
                f.write("fixture verification\nCLOSEOUT_COUNTS " + json.dumps(counts, sort_keys=True, separators=(",", ":")) + "\n")
            receipt = os.path.join(dest, "receipt-%d.json" % index)
            _write_json(receipt, {"task_id": TASK_ID, "candidate_sha256": obj.get("candidate_sha256"),
                "command": "python3 fixture.py", "exit_code": 0, "oracle": "all scenarios pass",
                "covers": [e["id"] for e in (ledger or _base_ledger())["entries"]] + ["C%02d" % n for n in range(15)],
                "gate": gate, "count_kind": "tests" if gate == "full_eunit" else "scenarios",
                "input_sha256": CAND_A, "started_at_utc": started.isoformat(), "finished_at_utc": finished.isoformat(),
                "counts": counts, "log": {"path": os.path.basename(log), "sha256": sha256_file(log)},
                "result": {"path": "raw/" + fname, "sha256": sha256_file(os.path.join(raw, fname))}})
            refs.append({"path": os.path.basename(receipt), "sha256": sha256_file(receipt)})
    ld, _ = load_json(os.path.join(dest, "ledger-v1.json"))
    for entry in ld["entries"]:
        entry.setdefault("status", "PASS")
        entry["receipts"] = refs
    ld["closeout_tasks"] = [{"id": "C%02d" % i, "status": "PASS", "receipts": refs} for i in range(15)]
    _write_json(os.path.join(dest, "ledger-v1.json"), ld)




def _cli_paths(case_dir):
    return ["--candidate-manifest", os.path.join(case_dir, "candidate-manifest.json"),
            "--source-root", os.path.join(case_dir, "source"),
            "--required-ids", os.path.join(case_dir, "required-ids.json"),
            "--ledger", os.path.join(case_dir, "ledger-v1.json"),
            "--closeout-plan", os.path.join(case_dir, "closeout-plan.md"),
            "--original-plan", os.path.join(case_dir, "plan-v2.2.1.md"),
            "--baseline", os.path.join(case_dir, "baseline.json"),
            "--evidence-dir", os.path.join(case_dir, "raw")]


def _run_case(cmd):
    return subprocess.run(cmd, capture_output=True, text=True)


def self_test():
    lines = []

    def emit(text):
        print(text)
        lines.append(text)

    for plan in (DEFAULT_CLOSEOUT_PLAN, DEFAULT_ORIGINAL_PLAN):
        if not os.path.isfile(plan):
            print("[FAIL] self-test prerequisite missing: %s" % plan)
            return 1
    closeout_bytes = _read_file(DEFAULT_CLOSEOUT_PLAN)
    original_bytes = _read_file(DEFAULT_ORIGINAL_PLAN)

    global SELFTEST_ROOT
    SELFTEST_ROOT = tempfile.mkdtemp(prefix="cache-closeout-verifier-")

    ok_evidence = {"verify_r1.json": _evidence_doc(CAND_A, ("pass",) * 3),
                   "verify_r2.json": _evidence_doc(CAND_A, ("pass",) * 3)}
    perf_runs = [("env", 1)] + [(arm, n) for n in range(1, 4)
                                  for arm in ("base", "cand")]
    for arm, round_no in perf_runs:
        ok_evidence["perf_%s_r%d.json" % (arm, round_no)] = _performance_result(CAND_A, arm, round_no)
    cases = {}  # label -> (case_dir, flags, expect_fail_check or None)

    def case(label, dirname, flags, expect_fail, **kw):
        dest = os.path.join(SELFTEST_ROOT, dirname)
        _write_case(dest, kw.get("closeout_bytes", closeout_bytes),
                    original_bytes, kw["evidences"],
                    ids_doc=kw.get("ids_doc"), ledger=kw.get("ledger"))
        cases[label] = (dest, flags, expect_fail)

    case("positive: valid fixture passes all six checks", "positive",
         ["--all"], None, evidences=ok_evidence)

    ledger_missing = _base_ledger()
    ledger_missing["entries"] = [e for e in ledger_missing["entries"]
                                 if e["id"] != "W0-02"]
    case("negative-1 missing ID in ledger", "neg_missing_id",
         ["--ids"], "ids", evidences=ok_evidence, ledger=ledger_missing)

    ledger_dup = _base_ledger()
    ledger_dup["entries"].append(dict(ledger_dup["entries"][0]))
    case("negative-2 duplicate ID in ledger", "neg_dup_id",
         ["--ids"], "ids", evidences=ok_evidence, ledger=ledger_dup)

    case("negative-3 tampered plan sha256", "neg_tampered_sha",
         ["--sha256"], "sha256",
         closeout_bytes=closeout_bytes + b"\n# tampered for self-test\n",
         evidences=ok_evidence)

    fail_evidence = {"exitfail_r1.json": _evidence_doc(
        CAND_A, ("pass", "fail", "pass"))}
    case("negative-4 scenario status=fail (non-zero exit)", "neg_exit_fail",
         ["--exits"], "exits", evidences=fail_evidence)

    mixed = {"mixed_r1.json": _evidence_doc(CAND_A, ("pass",) * 3),
             "mixed_r2.json": _evidence_doc(CAND_B, ("pass",) * 3)}
    case("negative-5 mixed candidates in one series", "neg_mixed_candidate",
         ["--candidate"], "candidate", evidences=mixed)

    fake = {"fake_minimal.json": {"status": "pass", "scenarios": []},
            "fake_no_detail.json": {"utc": _now_z(), "elapsed_ms": 5,
                                    "candidate_sha256": CAND_A,
                                    "scenarios": [{"name": "hand-written",
                                                   "status": "pass"}]}}
    case("negative-6 forged PASS structure", "neg_fake_pass",
         ["--exits"], "exits", evidences=fake)

    # Change actual receipt exit while retaining pass scenario JSON; update the
    # reference hash so this exercises the exit oracle, not only tamper checks.
    def receipt_mutation(label, dirname, mutation, expect_detail=None):
        expected = ("receipts", expect_detail) if expect_detail else "receipts"
        case(label, dirname, ["--all"], expected, evidences=ok_evidence)
        dest = cases[label][0]
        mutation(dest)

    def mutate_exit(dest):
        path = os.path.join(dest, "receipt-0.json")
        receipt, _ = load_json(path)
        receipt["exit_code"] = 7
        _write_json(path, receipt)
        ledger, _ = load_json(os.path.join(dest, "ledger-v1.json"))
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"][0]["sha256"] = sha256_file(path)
        _write_json(os.path.join(dest, "ledger-v1.json"), ledger)

    def mutate_status(dest):
        path = os.path.join(dest, "ledger-v1.json")
        ledger, _ = load_json(path)
        ledger["entries"][0]["status"] = "FAIL"
        _write_json(path, ledger)

    def mutate_evidence(dest):
        with open(os.path.join(dest, "raw/verify_r1.json"), "a") as f:
            f.write(" ")

    def mutate_source(dest):
        with open(os.path.join(dest, "source/Makefile"), "a") as f:
            f.write("# changed candidate\n")

    def mutate_log_failure(dest):
        log = os.path.join(dest, "command-0.log")
        with open(log, "a") as f:
            f.write("Failed: 1\n")
        path = os.path.join(dest, "receipt-0.json")
        receipt, _ = load_json(path)
        receipt["log"]["sha256"] = sha256_file(log)
        _write_json(path, receipt)
        ledger, _ = load_json(os.path.join(dest, "ledger-v1.json"))
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"][0]["sha256"] = sha256_file(path)
        _write_json(os.path.join(dest, "ledger-v1.json"), ledger)

    receipt_mutation("negative-7 required FAIL", "neg_required", mutate_status)
    receipt_mutation("negative-8 actual command exit nonzero", "neg_real_exit", mutate_exit)
    receipt_mutation("negative-9 result evidence tampered", "neg_evidence", mutate_evidence)
    receipt_mutation("negative-10 current source changed", "neg_source", mutate_source)
    receipt_mutation("negative-11 failure log despite PASS", "neg_log", mutate_log_failure)

    def mutate_unrelated(dest):
        ledger_path = os.path.join(dest, "ledger-v1.json")
        ledger, _ = load_json(ledger_path)
        path = os.path.join(dest, "receipt-0.json")
        receipt, _ = load_json(path)
        receipt["covers"] = ["W0-01"]
        _write_json(path, receipt)
        ref = {"path": "receipt-0.json", "sha256": sha256_file(path)}
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"] = [ref]
        _write_json(ledger_path, ledger)

    receipt_mutation("negative-12 unrelated receipt reused for every item", "neg_unrelated", mutate_unrelated)

    def mutate_covers_string(dest):
        path = os.path.join(dest, "receipt-0.json")
        receipt, _ = load_json(path)
        receipt["covers"] = " ".join(receipt["covers"])
        _write_json(path, receipt)
        ledger, _ = load_json(os.path.join(dest, "ledger-v1.json"))
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"][0]["sha256"] = sha256_file(path)
        _write_json(os.path.join(dest, "ledger-v1.json"), ledger)

    def mutate_missing_scenarios(dest):
        path = os.path.join(dest, "raw/verify_r1.json")
        result, _ = load_json(path)
        del result["scenarios"]
        _write_json(path, result)
        ledger, _ = load_json(os.path.join(dest, "ledger-v1.json"))
        for i in range(9):
            rp = os.path.join(dest, "receipt-%d.json" % i)
            receipt, _ = load_json(rp)
            if receipt["result"]["path"] == "raw/verify_r1.json":
                receipt["result"]["sha256"] = sha256_file(path)
                _write_json(rp, receipt)
                for entry in ledger["entries"] + ledger["closeout_tasks"]:
                    entry["receipts"][i]["sha256"] = sha256_file(rp)
        _write_json(os.path.join(dest, "ledger-v1.json"), ledger)

    receipt_mutation("negative-13 covers string rejected", "neg_covers_string", mutate_covers_string)
    receipt_mutation("negative-14 missing scenarios with rebound digest", "neg_scenarios_missing", mutate_missing_scenarios)

    reduced_ids = _base_ids_doc()
    reduced_ids["ids"] = reduced_ids["ids"][:4]
    reduced_ids["total"] = 4
    case("negative-15 required ID set silently reduced", "neg_reduced_contract",
         ["--ids"], "ids", evidences=ok_evidence, ids_doc=reduced_ids)

    def rebind_receipt(dest, index, receipt):
        path = os.path.join(dest, "receipt-%d.json" % index)
        _write_json(path, receipt)
        ledger_path = os.path.join(dest, "ledger-v1.json")
        ledger, _ = load_json(ledger_path)
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"][index]["sha256"] = sha256_file(path)
        _write_json(ledger_path, ledger)

    def mutate_full_eunit_kind(dest):
        receipt, _ = load_json(os.path.join(dest, "receipt-6.json"))
        receipt["count_kind"] = "checks"
        rebind_receipt(dest, 6, receipt)

    receipt_mutation("negative-16 full EUnit mislabeled checks", "neg_full_eunit_kind", mutate_full_eunit_kind)

    def mutate_cancelled_count(dest):
        result_path = os.path.join(dest, "raw/verify_r1.json")
        result, _ = load_json(result_path)
        counts = dict(result["counts"], cancelled=1)
        result["counts"] = counts
        _write_json(result_path, result)
        result_sha = sha256_file(result_path)
        for index in range(9):
            path = os.path.join(dest, "receipt-%d.json" % index)
            receipt, _ = load_json(path)
            if receipt["result"]["path"] != "raw/verify_r1.json":
                continue
            log_path = os.path.join(dest, receipt["log"]["path"])
            with open(log_path, encoding="utf-8") as f:
                body = f.read()
            old_footer = "CLOSEOUT_COUNTS " + json.dumps(
                receipt["counts"], sort_keys=True, separators=(",", ":"))
            assert body.count(old_footer) == 1
            new_footer = "CLOSEOUT_COUNTS " + json.dumps(
                counts, sort_keys=True, separators=(",", ":"))
            with open(log_path, "w", encoding="utf-8") as f:
                f.write(body.replace(old_footer, new_footer))
            receipt["counts"] = counts
            receipt["result"]["sha256"] = result_sha
            receipt["log"]["sha256"] = sha256_file(log_path)
            rebind_receipt(dest, index, receipt)

    receipt_mutation(
        "negative-17 cancelled count despite passing scenarios",
        "neg_cancelled_count",
        mutate_cancelled_count,
    )

    def mutate_result_exit(dest):
        result_path = os.path.join(dest, "raw/verify_r1.json")
        result, _ = load_json(result_path)
        result["exit"] = 2
        _write_json(result_path, result)
        result_sha = sha256_file(result_path)
        for index in range(9):
            path = os.path.join(dest, "receipt-%d.json" % index)
            receipt, _ = load_json(path)
            if receipt["result"]["path"] == "raw/verify_r1.json":
                receipt["result"]["sha256"] = result_sha
                rebind_receipt(dest, index, receipt)

    def mutate_performance_single_arm(dest):
        ledger_path = os.path.join(dest, "ledger-v1.json")
        ledger, _ = load_json(ledger_path)
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            retained = []
            for ref in entry["receipts"]:
                path = os.path.join(dest, ref["path"])
                receipt, _ = load_json(path)
                if receipt["gate"] != "performance":
                    retained.append(ref)
                    continue
                result, _ = load_json(os.path.join(dest, receipt["result"]["path"]))
                if result.get("arm") == "cand" and result.get("round") == 1:
                    retained.append(ref)
            entry["receipts"] = retained
        _write_json(ledger_path, ledger)

    receipt_mutation(
        "negative-18 result reports nonzero exit despite successful receipt",
        "neg_result_exit",
        mutate_result_exit,
        "result reports non-zero exit",
    )

    receipt_mutation(
        "negative-19 single-arm performance cannot satisfy interleaved window",
        "neg_perf_single_arm",
        mutate_performance_single_arm,
        "performance",
    )

    def mutate_perf_result(dest, filename, mutation):
        result_path = os.path.join(dest, "raw", filename)
        result, _ = load_json(result_path)
        mutation(result)
        _write_json(result_path, result)
        for index in range(7, 14):
            receipt_path = os.path.join(dest, "receipt-%d.json" % index)
            receipt, _ = load_json(receipt_path)
            if receipt["result"]["path"] == "raw/" + filename:
                receipt["result"]["sha256"] = sha256_file(result_path)
                rebind_receipt(dest, index, receipt)

    def mutate_w0_env_drift(dest):
        env_dir = os.path.join(dest, "raw/w0_anchor/env")
        performance_path = os.path.join(env_dir, "performance.json")
        performance, _ = load_json(performance_path)
        next(row for row in performance["single_node"]
             if row["kind"] == "get" and row["workers"] == 1)["throughput_ops_sec"] = 1300
        _write_json(performance_path, performance)
        hashes_path = os.path.join(env_dir, "sha256.json")
        hashes, _ = load_json(hashes_path)
        hashes["performance.json"] = sha256_file(performance_path)
        _write_json(hashes_path, hashes)
        result_path = os.path.join(dest, "raw/perf_env_r1.json")
        result, _ = load_json(result_path)
        anchor = next(s for s in result["scenarios"] if s["name"] == "environment_anchor")
        anchor["artifacts"]["env"]["performance"]["sha256"] = sha256_file(performance_path)
        anchor["artifacts"]["env"]["sha256"]["sha256"] = sha256_file(hashes_path)
        _write_json(result_path, result)
        receipt, _ = load_json(os.path.join(dest, "receipt-7.json"))
        receipt["result"]["sha256"] = sha256_file(result_path)
        rebind_receipt(dest, 7, receipt)

    def mutate_w0_duplicate_row(dest):
        env_dir = os.path.join(dest, "raw/w0_anchor/env")
        performance_path = os.path.join(env_dir, "performance.json")
        performance, _ = load_json(performance_path)
        performance["single_node"].append(dict(performance["single_node"][0]))
        _write_json(performance_path, performance)
        hashes_path = os.path.join(env_dir, "sha256.json")
        hashes, _ = load_json(hashes_path)
        hashes["performance.json"] = sha256_file(performance_path)
        _write_json(hashes_path, hashes)
        result_path = os.path.join(dest, "raw/perf_env_r1.json")
        result, _ = load_json(result_path)
        anchor = next(s for s in result["scenarios"] if s["name"] == "environment_anchor")
        anchor["artifacts"]["env"]["performance"]["sha256"] = sha256_file(performance_path)
        anchor["artifacts"]["env"]["sha256"]["sha256"] = sha256_file(hashes_path)
        _write_json(result_path, result)
        receipt, _ = load_json(os.path.join(dest, "receipt-7.json"))
        receipt["result"]["sha256"] = sha256_file(result_path)
        rebind_receipt(dest, 7, receipt)

    def mutate_w0_baseline_without_manifest_update(dest):
        baseline_path = os.path.join(dest, "raw/w0_anchor/env/baseline.json")
        baseline, _ = load_json(baseline_path)
        baseline["dependency_hashes"]["deps/00.erl"] = "ff" * 32
        _write_json(baseline_path, baseline)
        result_path = os.path.join(dest, "raw/perf_env_r1.json")
        result, _ = load_json(result_path)
        receipt, _ = load_json(os.path.join(dest, "receipt-7.json"))
        receipt["result"]["sha256"] = sha256_file(result_path)
        rebind_receipt(dest, 7, receipt)

    receipt_mutation(
        "negative-20 performance error count cannot PASS",
        "neg_perf_errors",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: next(s for s in result["scenarios"] if s["name"] == "throughput").update(errors=99)),
        "throughput missing",
    )
    receipt_mutation(
        "negative-21 C01 resource budget is enforced numerically",
        "neg_perf_budget",
        lambda dest: mutate_perf_result(dest, "perf_base_r2.json",
            lambda result: next(s for s in result["scenarios"] if s["name"] == "resource_envelope")["budget_usage"].update(n_count=50001)),
        "resource usage",
    )
    receipt_mutation(
        "negative-22 W0 environment drift cannot PASS",
        "neg_perf_env_drift",
        lambda dest: mutate_w0_env_drift(dest),
        "W0 environment metric",
    )
    receipt_mutation(
        "negative-23 boolean compile exit is not integer zero",
        "neg_perf_bool_exit",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: result.update(compile_exit=False)),
        "compile/run exit",
    )
    receipt_mutation(
        "negative-24 boolean error count is not integer zero",
        "neg_perf_bool_errors",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: next(s for s in result["scenarios"] if s["name"] == "throughput").update(errors=False)),
        "throughput missing",
    )
    receipt_mutation(
        "negative-25 malformed profile digest is rejected without crashing",
        "neg_perf_bad_profile",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: result.update(profile_sha256=[])),
        "profile digest",
    )
    receipt_mutation(
        "negative-26 missing P95 metric is rejected",
        "neg_perf_missing_p95",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: next(s for s in result["scenarios"] if s["name"] == "p99").pop("get_p95_us")),
        "P99 missing",
    )
    receipt_mutation(
        "negative-27 nonzero error rate is rejected even with zero error count",
        "neg_perf_error_rate",
        lambda dest: mutate_perf_result(dest, "perf_cand_r1.json",
            lambda result: next(s for s in result["scenarios"] if s["name"] == "throughput").update(error_rate=0.1)),
        "throughput missing",
    )
    receipt_mutation(
        "negative-28 raw W0 baseline tampering breaks sha256.json binding",
        "neg_perf_w0_tamper",
        mutate_w0_baseline_without_manifest_update,
        "evidence digest mismatch",
    )
    receipt_mutation(
        "negative-29 duplicated W0 workload row is rejected",
        "neg_perf_duplicate_w0_row",
        mutate_w0_duplicate_row,
        "W0 env/r7/r8 performance rows incomplete",
    )

    def mutate_perf_intervals_out_of_order(dest):
        start = datetime.now(timezone.utc).replace(microsecond=0) - timedelta(seconds=30)
        for index, source_index in enumerate(reversed(range(7))):
            receipt_path = os.path.join(dest, "receipt-%d.json" % (7 + index))
            receipt, _ = load_json(receipt_path)
            result_path = os.path.join(dest, "raw", receipt["result"]["path"].split("raw/", 1)[1])
            result, _ = load_json(result_path)
            interval_start = start + timedelta(seconds=source_index * 2)
            interval_end = interval_start + timedelta(seconds=1)
            receipt["started_at_utc"] = interval_start.isoformat()
            receipt["finished_at_utc"] = interval_end.isoformat()
            result["utc"] = (interval_start + timedelta(milliseconds=500)).isoformat()
            _write_json(result_path, result)
            receipt["result"]["sha256"] = sha256_file(result_path)
            rebind_receipt(dest, 7 + index, receipt)

    receipt_mutation(
        "negative-30 out-of-order real performance intervals are rejected",
        "neg_perf_interval_order",
        mutate_perf_intervals_out_of_order,
        "performance receipt intervals are not strictly chronological",
    )

    def mutate_perf_result_outside_interval(dest):
        receipt_path = os.path.join(dest, "receipt-13.json")
        receipt, _ = load_json(receipt_path)
        result_path = os.path.join(dest, "raw", receipt["result"]["path"].split("raw/", 1)[1])
        result, _ = load_json(result_path)
        receipt_start, _ = parse_utc(receipt["started_at_utc"])
        result["utc"] = (receipt_start - timedelta(seconds=1)).isoformat()
        _write_json(result_path, result)
        receipt["result"]["sha256"] = sha256_file(result_path)
        rebind_receipt(dest, 13, receipt)

    receipt_mutation(
        "negative-31 performance result time outside command interval is rejected",
        "neg_perf_result_time",
        mutate_perf_result_outside_interval,
        "performance result UTC is outside command receipt interval",
    )

    def mutate_perf_zero_duration(dest):
        start = datetime.now(timezone.utc).replace(microsecond=0) - timedelta(seconds=20)
        for index in range(7):
            receipt_path = os.path.join(dest, "receipt-%d.json" % (7 + index))
            receipt, _ = load_json(receipt_path)
            instant = start + timedelta(microseconds=index + 1)
            result_path = os.path.join(dest, "raw", receipt["result"]["path"].split("raw/", 1)[1])
            result, _ = load_json(result_path)
            receipt["started_at_utc"] = instant.isoformat()
            receipt["finished_at_utc"] = instant.isoformat()
            result["utc"] = instant.isoformat()
            result["elapsed_ms"] = 0
            _write_json(result_path, result)
            receipt["result"]["sha256"] = sha256_file(result_path)
            rebind_receipt(dest, 7 + index, receipt)

    receipt_mutation(
        "negative-32 zero-duration performance commands are rejected",
        "neg_perf_zero_duration",
        mutate_perf_zero_duration,
        "performance receipt interval must have positive duration",
    )

    def mutate_perf_zero_elapsed(dest):
        for index in range(7):
            receipt_path = os.path.join(dest, "receipt-%d.json" % (7 + index))
            receipt, _ = load_json(receipt_path)
            result_path = os.path.join(dest, "raw", receipt["result"]["path"].split("raw/", 1)[1])
            result, _ = load_json(result_path)
            result["elapsed_ms"] = 0
            _write_json(result_path, result)
            receipt["result"]["sha256"] = sha256_file(result_path)
            rebind_receipt(dest, 7 + index, receipt)

    receipt_mutation(
        "negative-33 zero elapsed performance results are rejected",
        "neg_perf_zero_elapsed",
        mutate_perf_zero_elapsed,
        "performance result elapsed_ms must be positive",
    )

    def add_runtime_failure(dest, marker):
        log = os.path.join(dest, "command-6.log")
        with open(log, "a") as f:
            f.write(marker + "\n")
        receipt, _ = load_json(os.path.join(dest, "receipt-6.json"))
        receipt["log"]["sha256"] = sha256_file(log)
        rebind_receipt(dest, 6, receipt)

    for number, name, marker in [
        (19, "OTP generic server terminating", "** Generic server cache_actor terminating"),
        (20, "OTP reason for termination", "** Reason for termination =="),
        (21, "component liveness false", "COMPONENT_LIVENESS false"),
    ]:
        receipt_mutation("negative-%d %s" % (number, name), "neg_runtime_%d" % number,
                         lambda dest, marker=marker: add_runtime_failure(dest, marker))

    def mutate_migration(dest):
        source = os.path.join(dest, "source")
        relative = "priv/migrations/00000170_qr_login_session.up.sql"
        path = os.path.join(source, relative)
        os.makedirs(os.path.dirname(path))
        with open(path, "w") as f:
            f.write("-- changed authority schema fixture\nSELECT 1;\n")
        subprocess.run(["git", "-C", source, "add", relative], check=True)

    def mutate_ignored_profile(dest):
        source = os.path.join(dest, "source")
        os.makedirs(os.path.join(source, "config"))
        with open(os.path.join(source, ".gitignore"), "w") as f:
            f.write("/config/sys.closeout-local.config\n")
        with open(os.path.join(source, "config/sys.closeout-local.config"), "w") as f:
            f.write("[{imboy, [{synthetic_fixture, changed}]}].\n")

    def mutate_monitoring_inputs(dest):
        source = os.path.join(dest, "source")
        paths = [
            "deploy/docker-compose.prod.yml",
            "deploy/prometheus/prometheus.yml",
            "deploy/prometheus/rules/imboy-alerts.yml",
            "deploy/prometheus/rules/imboy-alerts-test.yml",
            "deploy/grafana/dashboards/imboy-cache-sync.json",
            "deploy/grafana/provisioning/dashboards/default.yml",
            "deploy/grafana/provisioning/datasources/prometheus.yml",
        ]
        for relative in paths:
            path = os.path.join(source, relative)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                f.write("# monitoring candidate fixture\n")

    def mutate_ignored_source(dest):
        source = os.path.join(dest, "source")
        ignored_inputs = [
            "src/local_only.erl",
            "src/local_only.xrl",
            "src/local_only.yrl",
            "src/local_only.app.src",
            "asn1/local_only.asn1",
            "mibs/local_only.mib",
            "lib/local_only.ex",
        ]
        with open(os.path.join(source, ".gitignore"), "w") as f:
            f.write("".join("/%s\n" % path for path in ignored_inputs))
        for relative in ignored_inputs:
            path = os.path.join(source, relative)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                f.write("ignored build input\n")

    def mutate_dependency_inputs(dest):
        source = os.path.join(dest, "source")
        inputs = {
            "deps/fs/priv/mac_listener": "runtime helper\n",
            "deps/fs/priv/inotifywait.exe": "runtime helper\n",
            "deps/fs/c_src/mac/main.c": "build source\n",
            "deps/fs/c_src/mac/common.h": "build header\n",
            "deps/fs/c_src/Makefile.erlang.mk": "build rules\n",
            "deps/fs/rebar.config": "build config\n",
        }
        for relative, contents in inputs.items():
            path = os.path.join(source, relative)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w") as f:
                f.write(contents)

    def mutate_dependency_directory_symlink(dest):
        source = os.path.join(dest, "source")
        dependency = os.path.join(source, "deps/fs")
        os.makedirs(dependency, exist_ok=True)
        os.symlink(".", os.path.join(dependency, "cycle"))

    def mutate_dependency_root_symlink(dest):
        source = os.path.join(dest, "source")
        outside = os.path.join(dest, "outside-deps")
        os.makedirs(outside, exist_ok=True)
        with open(os.path.join(outside, "outside.c"), "w") as f:
            f.write("outside build input\n")
        os.symlink(outside, os.path.join(source, "deps"))

    def mutate_dependency_file_symlink_escape(dest):
        source = os.path.join(dest, "source")
        external = os.path.join(dest, "outside-helper")
        with open(external, "w") as f:
            f.write("outside runtime helper\n")
        helper = os.path.join(source, "deps/fs/priv/mac_listener")
        os.makedirs(os.path.dirname(helper), exist_ok=True)
        os.symlink(external, helper)

    def mutate_receipt_result_utc(dest, utc_value=None):
        source = os.path.join(dest, "raw/verify_r1.json")
        result, _ = load_json(source)
        if utc_value is None:
            del result["utc"]
        else:
            result["utc"] = utc_value
        external = os.path.join(dest, "receipt-result-outside-evidence-dir.json")
        _write_json(external, result)
        receipt_path = os.path.join(dest, "receipt-0.json")
        receipt, _ = load_json(receipt_path)
        receipt["result"] = {"path": os.path.basename(external),
                             "sha256": sha256_file(external)}
        _write_json(receipt_path, receipt)
        ledger_path = os.path.join(dest, "ledger-v1.json")
        ledger, _ = load_json(ledger_path)
        ref = {"path": "receipt-0.json", "sha256": sha256_file(receipt_path)}
        for entry in ledger["entries"] + ledger["closeout_tasks"]:
            entry["receipts"][0] = ref
        _write_json(ledger_path, ledger)

    receipt_mutation("negative-22 migration input added after freeze", "neg_migration", mutate_migration)
    receipt_mutation("negative-23 ignored runtime profile added after freeze", "neg_ignored_profile", mutate_ignored_profile)
    receipt_mutation("negative-24 monitoring runtime inputs added after freeze", "neg_monitoring_inputs", mutate_monitoring_inputs)
    receipt_mutation("negative-25 ignored Erlang source after freeze", "neg_ignored_source", mutate_ignored_source)
    receipt_mutation(
        "negative-26 dependency runtime and build inputs added after freeze",
        "neg_dependency_inputs",
        mutate_dependency_inputs,
        "candidate manifest input set/order != current source inventory",
    )
    receipt_mutation(
        "negative-27 dependency root symlink escapes source root",
        "neg_dependency_root_symlink_escape",
        mutate_dependency_root_symlink,
        "dependency root symlink is unsupported",
    )
    receipt_mutation(
        "negative-28 dependency directory symlink cycle",
        "neg_dependency_symlink_cycle",
        mutate_dependency_directory_symlink,
        "dependency directory symlinks are unsupported",
    )
    receipt_mutation(
        "negative-29 dependency file symlink escapes source root",
        "neg_dependency_file_symlink_escape",
        mutate_dependency_file_symlink_escape,
        "dependency file symlink escapes source root",
    )
    receipt_mutation(
        "negative-30 receipt result UTC outside evidence dir",
        "neg_receipt_result_utc",
        mutate_receipt_result_utc,
        "result has missing/invalid UTC",
    )
    for number, utc_value in [
        (31, "2026-10-13T12:00:00+01:60"),
        (32, "0001-01-01T00:00:00+23:59"),
        (33, "9999-12-31T23:59:59-23:59"),
    ]:
        receipt_mutation(
            "negative-%d receipt result invalid UTC boundary" % number,
            "neg_receipt_result_utc_%d" % number,
            lambda dest, value=utc_value: mutate_receipt_result_utc(dest, value),
            "result has missing/invalid UTC",
        )

    results = []
    for label, (dest, flags, expect_fail) in cases.items():
        cmd = [sys.executable, SCRIPT_PATH] + flags + _cli_paths(dest)
        proc = _run_case(cmd)
        if expect_fail is None:
            good = proc.returncode == 0 and "[FAIL]" not in proc.stdout \
                and proc.stdout.count("[PASS]") == len(CHECK_ORDER)
        elif isinstance(expect_fail, tuple):
            check, detail = expect_fail
            good = proc.returncode != 0 and ("[FAIL] %s" % check) in proc.stdout \
                and detail in proc.stdout
        else:
            good = proc.returncode != 0 and \
                ("[FAIL] %s" % expect_fail) in proc.stdout
        results.append((label, good, proc.returncode))
        status = "PASS" if good else "FAIL"
        emit("[%s] self-test %s (subprocess exit=%d)" % (status, label,
                                                         proc.returncode))
        if not good:
            for outline in proc.stdout.rstrip().splitlines():
                emit("    | %s" % outline)
            if proc.stderr.strip():
                emit("    ! stderr: %s" % proc.stderr.strip()[:400])

    internal_link_root = tempfile.mkdtemp(prefix="cache-closeout-deps-link-")
    subprocess.run(["git", "init", "-q", internal_link_root], check=True)
    target = os.path.join(internal_link_root, "deps/fs/shared/mac_listener")
    alias = os.path.join(internal_link_root, "deps/fs/priv/mac_listener")
    os.makedirs(os.path.dirname(target), exist_ok=True)
    os.makedirs(os.path.dirname(alias), exist_ok=True)
    with open(target, "w") as f:
        f.write("in-repository runtime helper\n")
    os.symlink("../shared/mac_listener", alias)
    try:
        internal_paths = candidate_inputs(internal_link_root)
        rows = [{"path": path, "sha256": sha256_file(os.path.join(internal_link_root, path))}
                for path in internal_paths]
        before = hashlib.sha256(json.dumps(
            rows, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        with open(target, "w") as f:
            f.write("changed in-repository runtime helper\n")
        after_rows = [{"path": path, "sha256": sha256_file(os.path.join(internal_link_root, path))}
                      for path in candidate_inputs(internal_link_root)]
        after = hashlib.sha256(json.dumps(
            after_rows, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
        good = "deps/fs/shared/mac_listener" in internal_paths and \
            "deps/fs/priv/mac_listener" in internal_paths and before != after
    except (OSError, ValueError, subprocess.CalledProcessError):
        good = False
    results.append(("internal dependency file symlink remains bound", good, 0 if good else 1))
    emit("[%s] self-test internal dependency file symlink remains bound" %
         ("PASS" if good else "FAIL"))

    passed = sum(1 for _, good, _ in results if good)
    emit("SELF-TEST SUMMARY: %d/%d fixture expectations met "
         "(1 positive + %d negative)" % (passed, len(results), len(results) - 1))
    log_path = os.path.join(SELFTEST_ROOT, "selftest.log")
    os.makedirs(SELFTEST_ROOT, exist_ok=True)
    with open(log_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines) + "\n")
    print("self-test log written to %s" % log_path)
    return 0 if passed == len(results) else 1


if __name__ == "__main__":
    sys.exit(self_test())
