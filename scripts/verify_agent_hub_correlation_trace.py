#!/usr/bin/env python3
"""Agent Hub correlation 审计链 verifier（TRACE-00 交付）。

实现 docs/api-contracts/agent_hub_correlation_contract.md 的机器判定：
输入审计导出 JSON（实体数组，字段白名单只含 ID/类型/父引用/时间戳/状态），
按 correlation_id 重建 request -> task -> event -> approval -> execution ->
delivery -> outcome 链，fail-closed 检查唯一根、必需边、时间顺序、孤儿、
重复根、entity_id 全局唯一、outcome 唯一、客户端覆盖与敏感字段。

退出码：0=合法链；2=任何违规（含文件不可读/结构非法）；1=用法错误。
错误输出只含字段路径与违规码，不回显字段值（secret 安全）。
只用 Python 标准库（3.9+，无 match 语法）。

用法：
    verify_agent_hub_correlation_trace.py <audit-export.json>
"""

import json
import re
import sys
from datetime import datetime

# --- 契约常量（与 agent_hub_correlation_contract.md §4/§5 一一对应） ---

ENTITY_TYPES = (
    "request", "task", "event", "approval", "execution", "delivery", "outcome",
)

ALLOWED_PARENTS = {
    "task": ("request",),
    "event": ("request", "task", "event"),
    "approval": ("task", "event"),
    "execution": ("approval", "task"),
    "delivery": ("execution", "event"),
    "outcome": ("execution", "delivery"),
}

REQUIRED_FIELDS = (
    "correlation_id", "entity_type", "entity_id",
    "parent_entity_id", "timestamp", "status",
)
OPTIONAL_FIELDS = ("client_override",)

# 隐私红线：链数据只允许 ID/类型/父引用/时间戳/状态（+ client_override 布尔）。
RECORD_FIELDS = frozenset(REQUIRED_FIELDS + OPTIONAL_FIELDS)

SENSITIVE_FIELDS = frozenset((
    "message_body", "body", "content", "payload", "text", "plaintext",
    "secret", "token", "password", "authorization", "api_key",
    "private_key", "url", "full_url", "pii",
))

RE_ID = re.compile(r"^[A-Za-z0-9_-]{16,64}$")
RE_STATUS = re.compile(r"^[a-z0-9_]{1,32}$")
RE_TIMESTAMP = re.compile(
    r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{1,6})?Z$"
)


class Violation(Exception):
    """链违规（fail-closed 条件），code 即 machine-readable 违规码。"""


def _require(cond, code):
    if not cond:
        raise Violation(code)


def parse_timestamp(value):
    """契约 §5 冻结形状：UTC ISO-8601，Z 结尾。返回可比 datetime。"""
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def validate_record(record, index):
    """单条记录的结构校验（契约 §5 白名单）。"""
    _require(isinstance(record, dict), "record[%d].not_object" % index)
    for field in REQUIRED_FIELDS:
        _require(field in record, "record[%d].missing:%s" % (index, field))
    for field in record:
        if field in RECORD_FIELDS:
            continue
        if field in SENSITIVE_FIELDS:
            raise Violation("record[%d].sensitive_field:%s" % (index, field))
        raise Violation("record[%d].unknown_field:%s" % (index, field))

    for field in ("correlation_id", "entity_id"):
        value = record[field]
        _require(isinstance(value, str), "record[%d].%s.not_string" % (index, field))
        _require(RE_ID.match(value) is not None,
                 "record[%d].%s.bad_pattern" % (index, field))

    _require(record["entity_type"] in ENTITY_TYPES,
             "record[%d].bad_entity_type" % index)

    parent = record["parent_entity_id"]
    if parent is not None:
        _require(isinstance(parent, str),
                 "record[%d].parent_entity_id.not_string_nor_null" % index)
        _require(RE_ID.match(parent) is not None,
                 "record[%d].parent_entity_id.bad_pattern" % index)
    if record["entity_type"] == "request":
        _require(parent is None, "record[%d].request_with_parent" % index)
    else:
        _require(parent is not None, "record[%d].missing_parent" % index)
        _require(parent != record["entity_id"], "record[%d].self_parent" % index)

    timestamp = record["timestamp"]
    _require(isinstance(timestamp, str),
             "record[%d].timestamp.not_string" % index)
    _require(RE_TIMESTAMP.match(timestamp) is not None,
             "record[%d].timestamp.bad_format" % index)

    status = record["status"]
    _require(isinstance(status, str), "record[%d].status.not_string" % index)
    _require(RE_STATUS.match(status) is not None,
             "record[%d].status.bad_pattern" % index)

    if "client_override" in record:
        override = record["client_override"]
        _require(isinstance(override, bool),
                 "record[%d].client_override.not_bool" % index)
        _require(not override, "record[%d].client_override" % index)


def check_chains(records):
    """链级校验：唯一根、必需边闭合、时间单调、孤儿/重复/entity_id 唯一。

    违规码只带记录索引（@N），不回显 ID 字段值（契约 §9）。
    """
    seen_ids = set()
    for index, record in enumerate(records):
        entity_id = record["entity_id"]
        _require(entity_id not in seen_ids,
                 "chain.duplicate_entity_id@%d" % index)
        seen_ids.add(entity_id)

    by_correlation = {}
    for index, record in enumerate(records):
        by_correlation.setdefault(
            record["correlation_id"], []).append((index, record))

    for correlation_id, group in sorted(by_correlation.items()):
        roots = [r for _, r in group if r["entity_type"] == "request"]
        _require(len(roots) == 1,
                 "chain.no_request_root" if not roots
                 else "chain.duplicate_request_root")
        index_of = {r["entity_id"]: (i, r) for i, r in group}
        outcomes = 0
        for index, record in group:
            if record["entity_type"] == "outcome":
                outcomes += 1
            parent_id = record["parent_entity_id"]
            if parent_id is None:
                continue
            _require(parent_id in index_of,
                     "chain.orphan_record@%d" % index)
            parent_index, parent = index_of[parent_id]
            _require(parent["entity_type"] in ALLOWED_PARENTS[record["entity_type"]],
                     "chain.bad_parent_type@%d" % index)
            _require(parse_timestamp(record["timestamp"])
                     >= parse_timestamp(parent["timestamp"]),
                     "chain.timestamp_regression@%d" % index)
        _require(outcomes <= 1, "chain.duplicate_outcome")

    return {
        "correlations": len(by_correlation),
        "entity_counts": {
            entity_type: sum(
                1 for r in records if r["entity_type"] == entity_type
            )
            for entity_type in ENTITY_TYPES
        },
    }


def verify_export(data):
    """校验导出对象；合法返回摘要 dict，违规抛 Violation。"""
    _require(isinstance(data, list), "export.not_array")
    _require(len(data) > 0, "export.empty")
    for index, record in enumerate(data):
        validate_record(record, index)
    return check_chains(records=data)


def load_export(path):
    with open(path, "r", encoding="utf-8") as handle:
        return json.load(handle)


def main(argv):
    if len(argv) != 1:
        print("usage: verify_agent_hub_correlation_trace.py "
              "<audit-export.json>", file=sys.stderr)
        return 1
    try:
        summary = verify_export(load_export(argv[0]))
    except Violation as exc:
        print(json.dumps(
            {"decision": "VIOLATION", "errors": [str(exc)]}, sort_keys=True))
        return 2
    except (OSError, ValueError) as exc:
        print(json.dumps({"decision": "VIOLATION", "errors": [
            "export.unreadable:%s" % type(exc).__name__]}, sort_keys=True))
        return 2
    summary["decision"] = "OK"
    print(json.dumps(summary, sort_keys=True))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
