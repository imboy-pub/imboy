#!/usr/bin/env python3
"""W4-01 leaf models (delete entrypoints, rollback, races, hold matrix)."""
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from attachment_lifecycle_v5_adapter_w4_models_common import (  # noqa: E402
    FAULT_SUFFIX,
    HOLD_ENTRYPOINT_CALLS,
    HOLD_RACE_DOMAIN,
    HOLD_RACE_TOKENS,
    ROLLBACK_ENTRYPOINT,
    S,
    _oracle,
)

def model_delete_entrypoints(f):
    _path, text = f.src("test/repo/attachment_lifecycle_v5_w401_entrypoints_tests.erl")
    macro = S.block(text, "-define(ENTRYPOINT_MODULES, [", f.case_id,
                    "?ENTRYPOINT_MODULES")
    scanned = re.findall(r"^\s{4}([a-z][a-z0-9_]*),?\s*$", macro["text"], re.M)
    inventory = {
        item["id"]: item
        for item in f.ctx["contract"].get("delete_entrypoint_inventory", [])}
    unit = f.test("attachment_lifecycle_v5_w401_entrypoints_tests", func="run")
    unit_desc = S.describe_test(unit) if unit else "no parsed entrypoints run/0 unit"

    def gaps(leaf):
        missing = []
        item = inventory.get(leaf["entrypoint"])
        if not item:
            missing.append("entrypoint %r absent from contract delete_entrypoint_inventory"
                           % leaf["entrypoint"])
        else:
            module = os.path.basename(item["path"])[:-4]
            if module not in scanned:
                missing.append(
                    "entrypoint module %s (%s:%s) absent from the executed "
                    "?ENTRYPOINT_MODULES static scan (%d modules: %s)"
                    % (module, item["path"], item["function_or_branch"],
                       len(scanned), ", ".join(scanned)))
            else:
                missing.append(
                    "per-leaf receipt: only %s exists; w401_static_callgraph/0 "
                    "scans module %s for direct elib_oss:delete_object/"
                    "ObjectStore:delete calls once for all entrypoints, with no "
                    "per-entrypoint status line"
                    % (unit_desc, module))
            missing.append(
                "entrypoint behavior %s:%s is not executed (source-text scan "
                "only; no database/object action and no evidence request id)"
                % (item["path"], item["function_or_branch"]))
        missing.append(_oracle())
        return missing

    notes = [
        "?ENTRYPOINT_MODULES scans %d modules (source %s:%d-%d): %s"
        % (len(scanned), "test/repo/attachment_lifecycle_v5_w401_entrypoints_tests.erl",
           macro["start"], macro["end"], ", ".join(scanned)),
        "contract requires 12 delete entrypoints; the scan covers their "
        "declared modules only for those listed above",
        "w401_case also executes pg/hold/hold-entrypoints/rollback suites, but "
        "none emits a per-entrypoint physical-delete status",
    ]
    return gaps, notes


def model_intent_rollback(f):
    src_rel = "test/repo/attachment_lifecycle_v5_w401_rollback_tests.erl"
    _path, text = f.src(src_rel)
    cases_block = S.block(text, "cases() ->", f.case_id, "cases/0")
    names = re.findall(r"fun ([a-z0-9_]+)/0", cases_block["text"])
    fault_result = S.line_of(text, "fault_result(rollback)")
    impl_eps = sorted({ep for ep in ROLLBACK_ENTRYPOINT if any(
        name.startswith(ROLLBACK_ENTRYPOINT[ep] + "_") for name in names)})
    unit = f.test("attachment_lifecycle_v5_w401_rollback_tests")
    missing_eps = sorted({
        leaf["entrypoint"] for leaf in f.leaves} - set(impl_eps))

    def gaps(leaf):
        ep = leaf["entrypoint"]
        scenario = leaf["ordering_or_fault"]
        missing = []
        prefix = ROLLBACK_ENTRYPOINT.get(ep)
        if prefix is None:
            missing.append(
                "no rollback case for entrypoint %r; cases/0 implements only %d "
                "of 12 entrypoints (%s); missing %s"
                % (ep, len(impl_eps), ", ".join(impl_eps),
                   ", ".join(missing_eps)))
        else:
            suffix = FAULT_SUFFIX.get(scenario)
            name = "%s_%s" % (prefix, suffix) if suffix else None
            test = S.find_test(f.parsed,
                               "attachment_lifecycle_v5_w401_rollback_tests",
                               func=name) if name else None
            if test is None:
                missing.append(
                    "no parsed eunit unit %s:%s (parsed rollback funs: %s)"
                    % ("attachment_lifecycle_v5_w401_rollback_tests", name,
                       ", ".join(names)))
            else:
                missing.append(S.per_leaf_gap(
                    test, "rollback matrix case %s" % name))
            missing.append(
                "fault injection is shape-only: fault_result/1 (%s:%d) maps "
                "sql_rollback/intent_insert_error/audit_insert_error/"
                "commit_response_lost to the same {error,{db,...}} at the intent "
                "call, so the four contract faults are not distinct runtime "
                "failures" % (src_rel, fault_result))
            missing.append(
                "suite is meck-only unit testing (setup/0 mecks elib_pg, "
                "elib_oss, attachment_repo, ...); driver=garage has no real "
                "garage call, no namespace stat and no database visibility "
                "receipt")
        missing.append(_oracle())
        return missing

    notes = [
        "cases/0 lists %d meck units (%s:%d-%d)"
        % (len(names), src_rel, cases_block["start"], cases_block["end"]),
        "contract requires 12 entrypoints x 4 faults = 48 units; cases/0 "
        "implements %d entrypoints x 4 = %d" % (len(impl_eps), len(impl_eps) * 4),
        "aggregate rollback module line: %s" % (S.describe_test(unit)
                                                if unit else "no run unit"),
    ]
    return gaps, notes


def model_confirm_gc_race(f):
    src_rel = "test/repo/attachment_lifecycle_v5_pg_tests.erl"
    _path, text = f.src(src_rel)
    unit = f.test("attachment_lifecycle_v5_pg_tests", func="w203_run")
    intent_line = S.line_of(text, "w203_intent_first(Conn) ->")
    confirm_line = S.line_of(text, "w203_confirm_first(Conn, Server, State) ->")

    def gaps(leaf):
        missing = [
            S.per_leaf_gap(unit, "w203_run/0"),
            "w203_run/0 invokes w203_intent_first/1 (%s:%d) and "
            "w203_confirm_first/3 (%s:%d) inside one eunit unit; eunit reports "
            "no status per commit order" % (src_rel, intent_line, src_rel,
                                            confirm_line),
        ]
        if leaf["domain"] == "enterprise":
            missing.append(
                "domain=enterprise: w203_run/0 seeds private scope/user 101 "
                "(w203_seed/2, w203_seed_open/2); no enterprise scope/scope_ref "
                "fixture exists in this suite")
        missing.append(_oracle())
        return missing

    notes = [
        "w203_run is the only runtime unit for both commit orders",
        "enterprise leaves share the same restricted fn_v5_confirm_pending "
        "guard but no enterprise fixture is executed",
    ]
    return gaps, notes


def model_reference_gc_race(f):
    src_rel = "test/repo/attachment_lifecycle_v5_pg_tests.erl"
    _path, text = f.src(src_rel)
    unit = f.test("attachment_lifecycle_v5_pg_tests", func="w301_run")
    race_line = S.line_of(text, "w301_reference_gc_race(Conn) ->")
    has_enterprise = "enterprise" in S.block(
        text, "w301_reference_gc_race(Conn) ->", f.case_id,
        "w301_reference_gc_race/1")["text"]
    raw_log = os.path.join(f.case_dir, "raw.log")
    guards_applied = False
    if os.path.isfile(raw_log):
        with open(raw_log, "r", encoding="utf-8", errors="replace") as handle:
            guards_applied = "guards-contract" in handle.read()

    def gaps(leaf):
        missing = [
            S.per_leaf_gap(unit, "w301_run/0"),
            "w301_run/0 calls w301_reference_gc_race/1 (%s:%d) which asserts "
            "bind/intent and direct-DML orderings in one unit; w301_run/0 also "
            "runs shared_owner/history_unknown; no per-ordering status line"
            % (src_rel, race_line),
        ]
        if leaf["schema_phase"] == "guards-contract":
            missing.append(
                "schema_phase=guards-contract: w401_case replays "
                "priv/migrations only (no guards-contract.sql application); "
                "CASE_DIR/raw.log %s"
                % ("contains a guards-contract marker" if guards_applied
                   else "is absent or has no guards-contract marker"))
        if leaf["domain"] == "enterprise":
            missing.append(
                "domain=enterprise: w301_reference_gc_race/1 seeds group "
                "scope_ref 66 and private rows only%s; no enterprise fixture"
                % ("" if has_enterprise else " (source has no 'enterprise' token)"))
        missing.append(_oracle())
        return missing

    notes = [
        "w301_run is one aggregate unit for four contract orderings",
        "guards-contract phase is not applied by w401_case",
    ]
    return gaps, notes


def model_hold_gc_race(f):
    src_rel = "test/repo/attachment_lifecycle_v5_hold_protocol_tests.erl"
    _path, text = f.src(src_rel)
    unit = f.test("attachment_lifecycle_v5_hold_protocol_tests", func="run")
    race = S.block(text, "w302_hold_gc_race(Conn) ->", f.case_id,
                   "w302_hold_gc_race/1")
    token_lines = {}
    for scenario, token in HOLD_RACE_TOKENS.items():
        if token is not None:
            token_lines[scenario] = S.line_of(race["text"], token)

    def gaps(leaf):
        scenario = leaf["ordering_or_fault"]
        missing = []
        token = HOLD_RACE_TOKENS.get(scenario)
        if token is None:
            missing.append(
                "scenario %r has no fixture or assertion in "
                "w302_hold_gc_race/1 (source %s:%d-%d) or in the six "
                "install_governance/3 fixtures"
                % (scenario, src_rel, race["start"], race["end"]))
        else:
            missing.append(S.per_leaf_gap(
                unit, "w302_hold_gc_race/1 ordering %r" % scenario))
            missing.append(
                "%r is asserted inside w302_hold_gc_race/1 with token %r at "
                "relative line %s; no per-ordering status line"
                % (scenario, token, token_lines.get(scenario)))
            seeded = HOLD_RACE_DOMAIN.get(scenario)
            if seeded and leaf["domain"] != seeded:
                missing.append(
                    "domain=%s: w302_hold_gc_race/1 seeds %r for this ordering"
                    % (leaf["domain"], seeded))
        missing.append(_oracle())
        return missing

    implemented = [s for s, t in HOLD_RACE_TOKENS.items() if t]
    return gaps, [
        "w302_hold_gc_race/1 implements %d of 4 orderings: %s"
        % (len(implemented), ", ".join(implemented)),
        "the hold protocol suite is shared with W3-02; w401_case re-executes "
        "it but adds no per-ordering receipt",
    ]


def model_hold_entrypoints(f):
    src_rel = "test/repo/attachment_lifecycle_v5_w401_hold_entrypoints_tests.erl"
    _path, text = f.src(src_rel)
    unit = f.test("attachment_lifecycle_v5_w401_hold_entrypoints_tests", func="run")
    fixtures_block = S.block(text, "fixtures() ->", f.case_id, "fixtures/0")
    fixtures = re.findall(r"^\s{8}([a-z][a-z0-9_]*),?$",
                          fixtures_block["text"], re.M)
    matrix = S.block(text, "run_matrix(Conn, Fixture) ->", f.case_id,
                     "run_matrix/2")
    invoked = []
    for entrypoint, (token, _what) in HOLD_ENTRYPOINT_CALLS.items():
        if token in matrix["text"]:
            invoked.append(entrypoint)

    def gaps(leaf):
        ep = leaf["entrypoint"]
        missing = []
        if ep not in invoked:
            missing.append(
                "entrypoint %r is not invoked by run_matrix/2 (source %s:%d-%d "
                "invokes %d: %s)"
                % (ep, src_rel, matrix["start"], matrix["end"], len(invoked),
                   ", ".join(invoked)))
        else:
            missing.append(S.per_leaf_gap(
                unit, "run_matrix/2 entrypoint %r fixture %r"
                % (ep, leaf["ordering_or_fault"])))
            missing.append(
                "run_matrix/2 exercises %s under fixture %s inside a "
                "lists:foreach loop; no per-entrypoint/per-scope status line"
                % (HOLD_ENTRYPOINT_CALLS[ep][1], leaf["ordering_or_fault"]))
        if leaf.get("governance_scope"):
            missing.append(
                "governance_scope=%s is installed by install_governance/3 but "
                "its per-scope effect is only asserted inside the same "
                "aggregate run/0" % leaf["governance_scope"])
        missing.append(_oracle())
        return missing

    return gaps, [
        "fixtures/0 declares %d governance fixtures (%s); contract requires 6"
        % (len(fixtures), ", ".join(fixtures)),
        "run_matrix/2 invokes %d of 12 contract entrypoints: %s"
        % (len(invoked), ", ".join(invoked)),
        "source comments declare pending-domain N/A semantics but no "
        "reject_size/reject_mime/reject_teaching/teaching_unbound/"
        "app_confirm_reject/eb_purge_drain call exists",
    ]

W401_MODELS = {
    "W4-01/delete-entrypoints": model_delete_entrypoints,
    "W4-01/intent-rollback": model_intent_rollback,
    "W4-01/confirm-gc-race": model_confirm_gc_race,
    "W4-01/reference-gc-race": model_reference_gc_race,
    "W4-01/hold-gc-race": model_hold_gc_race,
    "W4-01/hold-entrypoints": model_hold_entrypoints,
}
