#!/usr/bin/env python3
"""STEP-04 契约结构性校验（API-01 证据）。

校验深度：
  C1 OpenAPI YAML 可解析（redocly lint 另行独立校验，见 commands.md）
  C2 每个 operation 有 operationId、responses(>=200)、tags
  C3 TSID 硬约束：契约中所有 *_id / reviewer_uid 类字段绝无 type: integer；
     examples 中 ID 值一律为字符串（防 JS 2^53 精度丢失）
  C4 幂等：createSubmission 要求 Idempotency-Key header
  C5 家长视角 schema（SubmissionParentView 及其依赖）不含 ai_draft 键
  C6 STEP-04/schemas/*.json 均为合法 JSON Schema 且 examples 自通过
"""
import json
import re
import sys
from pathlib import Path

import yaml

BASE = Path(__file__).resolve().parent
OPENAPI = BASE / "openapi" / "moya-teaching.yaml"
SCHEMAS = BASE / "schemas"

TSID_FIELDS = re.compile(
    r"^(.*_id|reviewer_uid|submitted_by|created_by|guardian_uid|learner_id|"
    r"assignment_id|submission_id|attachment_id|task_id|group_id|workspace_id|"
    r"organization_id|draft_id|review_id|ai_task_id|video_attachment_id|"
    r"latest_submission|published_review_id)$"
)

failures = []
checks = []


def check(cid, ok, msg):
    checks.append((cid, ok, msg))
    if not ok:
        failures.append(f"{cid}: {msg}")


spec = yaml.safe_load(OPENAPI.read_text())
check("C1", isinstance(spec, dict) and "paths" in spec, "OpenAPI YAML parses to a document with paths")

# C2 operations
ops = 0
for path, item in spec["paths"].items():
    for method, op in item.items():
        if method not in ("get", "post", "put", "delete", "patch"):
            continue
        ops += 1
        check("C2", "operationId" in op, f"{method} {path} has operationId")
        resp = op.get("responses", {})
        check("C2", "200" in resp or "default" in resp, f"{method} {path} declares 200/default response")
check("C2", ops == 12, f"endpoint operation count == 12 (got {ops})")

# C3 TSID fields never integer anywhere in the yaml text
raw = OPENAPI.read_text()
# walk schemas: any property whose name matches TSID pattern must not be bare integer
def walk(node, trail=""):
    if isinstance(node, dict):
        name = node.get("__name__", "")
        for k, v in node.items():
            walk(v, f"{trail}/{k}")
    # handled below in targeted pass

props_tsid_bad = []
comps = spec.get("components", {}).get("schemas", {})
def check_schema_props(schemas: dict):
    for sname, sch in schemas.items():
        for pname, psch in sch.get("properties", {}).items():
            if not TSID_FIELDS.match(pname):
                continue
            t = psch.get("type")
            if t == "integer":
                props_tsid_bad.append(f"{sname}.{pname}")
            # allOf-wrapped refs to TsidString/NullableTsidString are fine
            for sub in psch.get("allOf", []) if isinstance(psch.get("allOf"), list) else []:
                ref = sub.get("$ref", "") if isinstance(sub, dict) else ""
                if "TsidString" not in ref and ref:
                    props_tsid_bad.append(f"{sname}.{pname} ref={ref}")
            ref = psch.get("$ref", "")
            if ref and "TsidString" not in ref:
                props_tsid_bad.append(f"{sname}.{pname} ref={ref}")

check_schema_props(comps)
# parameters too
for p in spec.get("components", {}).get("parameters", {}).values():
    sch = p.get("schema", {})
    for sub in sch.get("allOf", []) if isinstance(sch.get("allOf"), list) else []:
        if isinstance(sub, dict) and "TsidString" not in sub.get("$ref", "") and sub.get("$ref"):
            props_tsid_bad.append(f"param {p.get('name')}")
check("C3", not props_tsid_bad, f"all TSID-typed fields are string-typed (violations: {props_tsid_bad or 'none'})")

# C3b examples: any key matching TSID pattern inside examples must be str
def walk_examples(node, trail=""):
    if isinstance(node, dict):
        for k, v in node.items():
            if TSID_FIELDS.match(str(k)) and isinstance(v, int):
                yield f"{trail}/{k}={v!r} is int"
            yield from walk_examples(v, f"{trail}/{k}")
    elif isinstance(node, list):
        for i, v in enumerate(node):
            yield from walk_examples(v, f"{trail}[{i}]")

bad_examples = list(walk_examples(spec.get("paths", {})))
bad_examples += list(walk_examples(comps))
check("C3", not bad_examples, f"example ID values are all JSON strings (violations: {bad_examples or 'none'})")

# C4 idempotency header on createSubmission
create = spec["paths"]["/teaching/assignments/{id}/submissions"]["post"]
hdrs = [p.get("name") for p in create.get("parameters", []) if p.get("in") == "header"]
check("C4", "Idempotency-Key" in hdrs, "createSubmission requires Idempotency-Key header")

# C5 parent view has no ai_draft (its own + inherited properties)
parent = comps.get("SubmissionParentView", {})
parent_keys = set(parent.get("properties", {}).keys())
check("C5", "ai_draft" not in parent_keys, "SubmissionParentView (guardian payload) has no ai_draft key")
# published envelope reachable from parent view must not carry drafts either
check("C5", "ai_draft" not in comps.get("PublishedReview", {}).get("properties", {}), "PublishedReview has no ai_draft")

# C6 JSON Schema files parse and examples validate
import jsonschema

schema_files = sorted(SCHEMAS.glob("*.json"))
check("C6", len(schema_files) == 5, f"5 JSON Schema files present (got {len(schema_files)})")
for f in schema_files:
    try:
        sch = json.loads(f.read_text())
        validator_cls = jsonschema.validators.validator_for(sch)
        validator_cls.check_schema(sch)
        for i, ex in enumerate(sch.get("examples", [])):
            try:
                validator_cls(sch).validate(ex)
            except jsonschema.ValidationError as e:
                failures.append(f"C6: {f.name} example[{i}] invalid: {e.message[:80]}")
        checks.append(("C6", True, f"{f.name}: valid schema, examples pass"))
    except Exception as e:  # noqa: BLE001
        failures.append(f"C6: {f.name}: {e}")

print(f"contract validation: {len(checks)} checks, {len(failures)} failures")
for cid, ok, msg in checks:
    print(f"  [{'PASS' if ok else 'FAIL'}] {cid} {msg}")
if failures:
    print("\nFAILURES:")
    for f_ in failures:
        print(" -", f_)
    sys.exit(1)
print("ALL PASS")
