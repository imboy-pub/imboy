#!/usr/bin/env python3
"""Agent Hub 任务证据 verifier（EVID-00 交付）。

实现 docs/testing/agent-hub-evidence.schema.json 的全部关键约束，并追加
JSON Schema 无法表达的交叉一致性规则。fail-closed：任何缺失、不一致、
冒充均使结论非 PASS；结构非法返回退出码 2，结构合法但任务未通过返回 1，
单任务 verified-PASS / gate 全 PASS 返回 0。

只用 Python 标准库；不扫描、不回显 secret 内容（错误信息只含字段路径与
违规类型，不含字段值；hash 属于计划允许记录的摘要）。

用法：
    verify_agent_hub_task_evidence.py --task <path/to/evidence.json>
    verify_agent_hub_task_evidence.py --gate <dir-with-TASK_ID-subdirs>
"""

import hashlib
import json
import os
import re
import sys

SCHEMA_PATH = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "..", "docs", "testing", "agent-hub-evidence.schema.json",
)

STATUS_ENUM = ("PASS", "FAIL", "BLOCKED", "PARTIAL")
VERIFICATION_ENUM = ("draft", "verified", "superseded")
ACCEPTANCE_ENUM = ("PASS", "FAIL", "SKIP")
REPOS = ("imboy", "imboyapp", "imboyadmin")

RE_TASK_ID = re.compile(r"^[A-Z][A-Z0-9]*(-[A-Z0-9]+)*$")
RE_ACCEPTANCE_ID = re.compile(r"^[A-Z][A-Z0-9]*(-[A-Z0-9]+)*-A[0-9]{2}$")
RE_CMD_ID = re.compile(r"^cmd-[0-9]{2,}$")
RE_ARTIFACT_ID = re.compile(r"^artifact-[a-z0-9][a-z0-9-]*$")
RE_SHA256 = re.compile(r"^[0-9a-f]{64}$")
RE_SHA_OR_NA = re.compile(r"^(na|[0-9a-f]{40})$")
RE_COMMIT = re.compile(r"^(not-created-no-identity-approval|[0-9a-f]{7,40})$")
RE_FINAL_DIFF = re.compile(r"^[a-z0-9_.-]+:.+$")


class Invalid(Exception):
    """证据结构/一致性违规（fail-closed 条件）。"""


def _require(cond, code):
    if not cond:
        raise Invalid(code)


def _check_type(value, types, code):
    _require(isinstance(value, types), code)


def _is_uint(value):
    return isinstance(value, int) and not isinstance(value, bool) and value >= 0


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as handle:
        for chunk in iter(lambda: handle.read(131072), b""):
            digest.update(chunk)
    return digest.hexdigest()


def validate_shape(ev):
    """对应 schema 的结构校验；code 即 machine-readable 违规码。"""
    _check_type(ev, dict, "top.not_object")
    for field in ("schema_version", "task_id", "status", "verification_status",
                  "base_sha", "final_diff", "commands", "tests", "acceptance",
                  "artifacts", "residual_risks", "commit"):
        _require(field in ev, "top.missing:" + field)
    _require(ev["schema_version"] == 1, "schema_version.not_1")
    _check_type(ev["task_id"], str, "task_id.not_string")
    _require(RE_TASK_ID.match(ev["task_id"]), "task_id.bad_pattern")
    _require(ev["status"] in STATUS_ENUM, "status.bad_enum")
    _require(ev["verification_status"] in VERIFICATION_ENUM,
             "verification_status.bad_enum")

    _check_type(ev["base_sha"], dict, "base_sha.not_object")
    for repo in REPOS:
        _require(repo in ev["base_sha"], "base_sha.missing:" + repo)
        _check_type(ev["base_sha"][repo], str, "base_sha.%s.not_string" % repo)
        _require(RE_SHA_OR_NA.match(ev["base_sha"][repo]),
                 "base_sha.%s.bad_pattern" % repo)

    _check_type(ev["final_diff"], list, "final_diff.not_array")
    for item in ev["final_diff"]:
        _check_type(item, str, "final_diff.item_not_string")
        _require(RE_FINAL_DIFF.match(item), "final_diff.item_bad_pattern")

    _check_type(ev["commands"], list, "commands.not_array")
    _require(len(ev["commands"]) >= 1, "commands.empty")
    for cmd in ev["commands"]:
        _check_type(cmd, dict, "commands.item_not_object")
        for field in ("id", "command", "exit_code"):
            _require(field in cmd, "commands.item_missing:" + field)
        _require(RE_CMD_ID.match(cmd["id"]) if isinstance(cmd["id"], str)
                 else False, "commands.id_bad_pattern")
        _check_type(cmd["command"], str, "commands.command_not_string")
        _require(cmd["command"], "commands.command_empty")
        _require(_is_uint(cmd["exit_code"]), "commands.exit_code_not_uint")

    _check_type(ev["tests"], dict, "tests.not_object")
    for field in ("passed", "failed", "skipped"):
        _require(field in ev["tests"], "tests.missing:" + field)
        _require(_is_uint(ev["tests"][field]), "tests.%s.not_uint" % field)

    _check_type(ev["acceptance"], list, "acceptance.not_array")
    _require(len(ev["acceptance"]) >= 1, "acceptance.empty")
    for acc in ev["acceptance"]:
        _check_type(acc, dict, "acceptance.item_not_object")
        for field in ("acceptance_id", "status", "command_ids", "artifact_ids"):
            _require(field in acc, "acceptance.item_missing:" + field)
        _require(RE_ACCEPTANCE_ID.match(acc["acceptance_id"])
                 if isinstance(acc["acceptance_id"], str) else False,
                 "acceptance.id_bad_pattern")
        _require(acc["status"] in ACCEPTANCE_ENUM, "acceptance.status_bad_enum")
        _require(acc["acceptance_id"].startswith(ev["task_id"] + "-"),
                 "acceptance.id_not_prefixed_by_task")
        for field in ("command_ids", "artifact_ids"):
            _check_type(acc[field], list, "acceptance.%s_not_array" % field)
            for ref in acc[field]:
                _check_type(ref, str, "acceptance.%s.ref_not_string" % field)
        if "assertions" in acc:
            _check_type(acc["assertions"], list, "acceptance.assertions_not_array")
            for a in acc["assertions"]:
                _check_type(a, str, "acceptance.assertion_not_string")
        if "counts" in acc:
            _check_type(acc["counts"], dict, "acceptance.counts_not_object")
            for v in acc["counts"].values():
                _require(_is_uint(v), "acceptance.count_value_not_uint")

    _check_type(ev["artifacts"], list, "artifacts.not_array")
    _require(len(ev["artifacts"]) >= 1, "artifacts.empty")
    for art in ev["artifacts"]:
        _check_type(art, dict, "artifacts.item_not_object")
        for field in ("id", "path", "sha256"):
            _require(field in art, "artifacts.item_missing:" + field)
        _require(RE_ARTIFACT_ID.match(art["id"]) if isinstance(art["id"], str)
                 else False, "artifacts.id_bad_pattern")
        _check_type(art["path"], str, "artifacts.path_not_string")
        _require(art["path"].startswith("/"), "artifacts.path_not_absolute")
        _require(RE_SHA256.match(art["sha256"])
                 if isinstance(art["sha256"], str) else False,
                 "artifacts.sha256_bad_pattern")

    _check_type(ev["residual_risks"], list, "residual_risks.not_array")
    for risk in ev["residual_risks"]:
        _check_type(risk, str, "residual_risks.item_not_string")
    _check_type(ev["commit"], str, "commit.not_string")
    _require(RE_COMMIT.match(ev["commit"]), "commit.bad_pattern")

    # 可选 superseded_by：仅 superseded 证据允许携带，指向取代它的任务。
    if "superseded_by" in ev:
        _check_type(ev["superseded_by"], str, "superseded_by.not_string")
        _require(RE_TASK_ID.match(ev["superseded_by"]),
                 "superseded_by.bad_pattern")
        _require(ev["verification_status"] == "superseded",
                 "superseded_by.requires_superseded_status")
        _require(ev["superseded_by"] != ev["task_id"],
                 "superseded_by.self_reference")


def validate_cross(ev, evidence_path):
    """JSON Schema 表达不了的交叉一致性。"""
    cmd_ids = [c["id"] for c in ev["commands"]]
    _require(len(set(cmd_ids)) == len(cmd_ids), "commands.duplicate_id")
    art_ids = [a["id"] for a in ev["artifacts"]]
    _require(len(set(art_ids)) == len(art_ids), "artifacts.duplicate_id")
    acc_ids = [a["acceptance_id"] for a in ev["acceptance"]]
    _require(len(set(acc_ids)) == len(acc_ids), "acceptance.duplicate_id")

    cmd_by_id = {c["id"]: c for c in ev["commands"]}
    art_by_id = {a["id"]: a for a in ev["artifacts"]}
    for acc in ev["acceptance"]:
        for ref in acc["command_ids"]:
            _require(ref in cmd_by_id, "acceptance.unknown_command_ref")
        for ref in acc["artifact_ids"]:
            _require(ref in art_by_id, "acceptance.unknown_artifact_ref")
        # 非零命令不得支撑 PASS（无论任务整体状态）。
        if acc["status"] == "PASS":
            for ref in acc["command_ids"]:
                _require(cmd_by_id[ref]["exit_code"] == 0,
                         "acceptance.pass_backed_by_nonzero_command")

    # task_id 必须与 $EVIDENCE_ROOT/<TASK_ID>/evidence.json 布局一致。
    parent = os.path.basename(os.path.dirname(os.path.abspath(evidence_path)))
    _require(parent == ev["task_id"], "task_id.mismatch_with_directory")

    # artifact 必须真实存在且 hash 匹配。
    for art in ev["artifacts"]:
        real = os.path.realpath(art["path"])
        _require(os.path.isfile(real), "artifacts.file_missing")
        _require(sha256_file(real) == art["sha256"], "artifacts.hash_mismatch")

    # 冒充检测：status=PASS 必须 acceptance 全 PASS 且零失败。
    if ev["status"] == "PASS":
        for acc in ev["acceptance"]:
            _require(acc["status"] == "PASS", "status.pass_with_non_pass_acceptance")
        _require(ev["tests"]["failed"] == 0, "status.pass_with_failed_tests")


def verify_task_file(evidence_path):
    """返回单任务机器结论 dict。结构非法 → decision=INVALID + errors。"""
    result = {"mode": "task", "task_id": None, "decision": "INVALID", "errors": []}
    try:
        with open(evidence_path, "r", encoding="utf-8") as handle:
            ev = json.load(handle)
    except (OSError, ValueError) as exc:
        result["errors"].append("evidence.unreadable:%s" % type(exc).__name__)
        return result
    result["task_id"] = ev.get("task_id") if isinstance(ev, dict) else None
    try:
        validate_shape(ev)
        validate_cross(ev, evidence_path)
    except Invalid as exc:
        result["errors"].append(str(exc))
        return result
    status = ev["status"]
    verification = ev["verification_status"]
    if verification == "superseded":
        result["decision"] = "SUPERSEDED"
    elif status == "PASS":
        # draft 是合法中间态：结构一致、验收全 PASS，等待结算。
        result["decision"] = "DRAFT" if verification == "draft" else "PASS"
    else:
        result["decision"] = status
    result.pop("errors")
    return result


def verify_gate(root):
    """扫描 root/<TASK_ID>/evidence.json 汇总。

    SUPERSEDED 任务不参与聚合（其证据已被后续任务取代，如 BUILD-00 →
    BUILD-00R），在 superseded_skipped 中如实上报；其余任一非 PASS
    （BLOCKED/PARTIAL/DRAFT/FAIL）→ NOT_PASS；INVALID（含已 superseded
    但结构损坏的）→ INVALID；全部被 superseded（无活跃任务）→ NOT_PASS
    而非 PASS（fail-closed：空 gate 不得放行）。"""
    result = {"mode": "gate", "tasks": {}, "superseded_skipped": [],
              "decision": "INVALID", "errors": []}
    try:
        entries = sorted(os.listdir(root))
    except OSError as exc:
        result["errors"].append("gate.unreadable_root:%s" % type(exc).__name__)
        return result
    task_dirs = [e for e in entries
                 if os.path.isfile(os.path.join(root, e, "evidence.json"))]
    if not task_dirs:
        result["errors"].append("gate.no_task_evidence_found")
        return result
    decisions = []
    for task_id in task_dirs:
        single = verify_task_file(os.path.join(root, task_id, "evidence.json"))
        result["tasks"][task_id] = {"decision": single["decision"]}
        if "errors" in single:
            result["tasks"][task_id]["errors"] = single["errors"]
            result["errors"].extend("%s:%s" % (task_id, e)
                                    for e in single["errors"])
        if single["decision"] == "SUPERSEDED":
            result["superseded_skipped"].append(task_id)
        else:
            decisions.append(single["decision"])
    if "INVALID" in decisions:
        result["decision"] = "INVALID"
    elif not decisions:
        result["errors"].append("gate.no_active_tasks")
        result["decision"] = "NOT_PASS"
    elif all(d == "PASS" for d in decisions):
        result["decision"] = "PASS"
    else:
        result["decision"] = "NOT_PASS"
    if not result["errors"]:
        result.pop("errors")
    return result


def main(argv):
    usage = "usage: verify_agent_hub_task_evidence.py --task FILE | --gate DIR"
    if len(argv) != 2 or argv[0] not in ("--task", "--gate"):
        print(usage, file=sys.stderr)
        return 2
    if argv[0] == "--task":
        result = verify_task_file(argv[1])
    else:
        result = verify_gate(argv[1])
    print(json.dumps(result, sort_keys=True))
    decision = result["decision"]
    if decision == "INVALID":
        return 2
    # 单任务 draft/合法未过 → 1（证据合法，任务未结算/未通过）。
    # gate NOT_PASS → 1。PASS（含单任务 verified-PASS）→ 0。
    return 0 if decision == "PASS" else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
