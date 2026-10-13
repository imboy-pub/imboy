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
    SCRIPT_PATH, CHECK_ORDER, DEFAULT_REQUIRED_IDS, sha256_file, load_json, _read_file)

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


def _write_json(path, obj):
    with open(path, "w", encoding="utf-8") as f:
        json.dump(obj, f, indent=1, ensure_ascii=False)


def _write_case(dest, closeout_bytes, original_bytes, evidences,
                ids_doc=None, ledger=None):
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
    for i, gate in enumerate(fixture_gates):
        fname, obj = list(evidences.items())[i % len(evidences)]
        counts = obj.get("counts", {"passed": 0, "failed": 0, "cancelled": 0, "skipped": 0})
        log = os.path.join(dest, "command-%d.log" % i)
        with open(log, "w") as f:
            if gate == "full_eunit":
                f.write("All %d tests passed.\n" % counts["passed"])
            f.write("fixture verification\nCLOSEOUT_COUNTS " + json.dumps(counts, sort_keys=True, separators=(",", ":")) + "\n")
        receipt = os.path.join(dest, "receipt-%d.json" % i)
        _write_json(receipt, {"task_id": TASK_ID, "candidate_sha256": obj.get("candidate_sha256"),
            "command": "python3 fixture.py", "exit_code": 0, "oracle": "all scenarios pass",
            "covers": [e["id"] for e in (ledger or _base_ledger())["entries"]] + ["C%02d" % n for n in range(15)],
            "gate": gate, "count_kind": "tests" if gate == "full_eunit" else "scenarios",
            "input_sha256": CAND_A, "started_at_utc": _now_z(), "finished_at_utc": _now_z(),
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
    def receipt_mutation(label, dirname, mutation):
        case(label, dirname, ["--all"], "receipts", evidences=ok_evidence)
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

    def add_runtime_failure(dest, marker):
        log = os.path.join(dest, "command-6.log")
        with open(log, "a") as f:
            f.write(marker + "\n")
        receipt, _ = load_json(os.path.join(dest, "receipt-6.json"))
        receipt["log"]["sha256"] = sha256_file(log)
        rebind_receipt(dest, 6, receipt)

    for number, name, marker in [
        (18, "OTP generic server terminating", "** Generic server cache_actor terminating"),
        (19, "OTP reason for termination", "** Reason for termination =="),
        (20, "component liveness false", "COMPONENT_LIVENESS false"),
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

    receipt_mutation("negative-21 migration input added after freeze", "neg_migration", mutate_migration)
    receipt_mutation("negative-22 ignored runtime profile added after freeze", "neg_ignored_profile", mutate_ignored_profile)

    results = []
    for label, (dest, flags, expect_fail) in cases.items():
        cmd = [sys.executable, SCRIPT_PATH] + flags + _cli_paths(dest)
        proc = _run_case(cmd)
        if expect_fail is None:
            good = proc.returncode == 0 and "[FAIL]" not in proc.stdout \
                and proc.stdout.count("[PASS]") == len(CHECK_ORDER)
            results.append((label, good, proc.returncode))
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
