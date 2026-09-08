"""TRACE-00 verifier 测试：正负例固定契约语义。

对应 docs/api-contracts/agent_hub_correlation_contract.md §4/§5/§9：
合法链（完整/进行中/部分链）、孤儿、双根、无根、客户端覆盖、时间倒序、
断链（task 无 request 父）、敏感字段、结构非法与 CLI 退出码。
"""

import contextlib
import importlib.util
import io
import json
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "verify_agent_hub_correlation_trace.py"
FIXTURES = ROOT / "test" / "fixtures" / "agent_hub" / "correlation"

SPEC = importlib.util.spec_from_file_location("correlation_trace", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)

CID = "corr-0123456789abcdef0123456789abcdef"
CID_OTHER = "corr-fedcba9876543210fedcba9876543210"


def record(entity_type, entity_id, parent, timestamp,
           status="ok", correlation_id=CID, **extra):
    payload = {
        "correlation_id": correlation_id,
        "entity_type": entity_type,
        "entity_id": entity_id,
        "parent_entity_id": parent,
        "timestamp": timestamp,
        "status": status,
    }
    payload.update(extra)
    return payload


def full_chain():
    """契约 §4.2 主链：request->task->event->approval->execution->delivery->outcome。"""
    return [
        record("request", "req-20260907-000001", None,
               "2026-09-07T08:00:00.000000Z", "accepted"),
        record("task", "task-20260907-000001", "req-20260907-000001",
               "2026-09-07T08:00:01.000000Z", "created"),
        record("event", "evt-20260907-000001", "task-20260907-000001",
               "2026-09-07T08:00:02.000000Z", "awaiting_approval"),
        record("approval", "apr-20260907-000001", "task-20260907-000001",
               "2026-09-07T08:05:00.000000Z", "approved"),
        record("execution", "exe-20260907-000001", "apr-20260907-000001",
               "2026-09-07T08:05:01.000000Z", "succeeded"),
        record("delivery", "dlv-20260907-000001", "exe-20260907-000001",
               "2026-09-07T08:05:02.000000Z", "delivered"),
        record("outcome", "out-20260907-000001", "dlv-20260907-000001",
               "2026-09-07T08:05:03.000000Z", "succeeded"),
    ]


class ValidChainTest(unittest.TestCase):
    """正例：A01 六类实体 + outcome 由一个 correlation 重建。"""

    def test_full_chain_passes_and_rebuilds_all_entities(self):
        summary = MODULE.verify_export(full_chain())
        self.assertEqual(summary["correlations"], 1)
        self.assertEqual(summary["entity_counts"], {
            "request": 1, "task": 1, "event": 1, "approval": 1,
            "execution": 1, "delivery": 1, "outcome": 1,
        })

    def test_in_progress_chain_without_outcome_passes(self):
        summary = MODULE.verify_export(full_chain()[:-1])
        self.assertEqual(summary["entity_counts"]["outcome"], 0)

    def test_bot_mention_partial_chain_passes(self):
        """Bot mention：request->event->delivery（无 task/approval/execution）。"""
        chain = [
            record("request", "req-20260907-000002", None,
                   "2026-09-07T09:00:00.000000Z", "accepted"),
            record("event", "evt-20260907-000002", "req-20260907-000002",
                   "2026-09-07T09:00:01.000000Z", "bot_mentioned"),
            record("delivery", "dlv-20260907-000002", "evt-20260907-000002",
                   "2026-09-07T09:00:02.000000Z", "delivered"),
        ]
        self.assertEqual(MODULE.verify_export(chain)["correlations"], 1)

    def test_event_cascade_and_direct_request_parent_pass(self):
        chain = full_chain() + [
            record("event", "evt-20260907-000003", "req-20260907-000001",
                   "2026-09-07T08:06:00.000000Z", "notified"),
            record("event", "evt-20260907-000004", "evt-20260907-000003",
                   "2026-09-07T08:06:01.000000Z", "acknowledged"),
        ]
        self.assertEqual(MODULE.verify_export(chain)["entity_counts"]["event"], 3)

    def test_execution_failed_outcome_on_execution_passes(self):
        """执行失败：outcome 挂 execution（免审批路径 execution.parent=task）。"""
        chain = [
            record("request", "req-20260907-000003", None,
                   "2026-09-07T10:00:00.000000Z", "accepted"),
            record("task", "task-20260907-000003", "req-20260907-000003",
                   "2026-09-07T10:00:01.000000Z", "created"),
            record("execution", "exe-20260907-000003", "task-20260907-000003",
                   "2026-09-07T10:00:02.000000Z", "failed"),
            record("outcome", "out-20260907-000003", "exe-20260907-000003",
                   "2026-09-07T10:00:03.000000Z", "failed"),
        ]
        MODULE.verify_export(chain)

    def test_equal_parent_child_timestamp_passes(self):
        chain = full_chain()
        chain[1]["timestamp"] = chain[0]["timestamp"]
        MODULE.verify_export(chain)

    def test_second_independent_correlation_passes(self):
        other = [
            record("request", "req-20260908-000001", None,
                   "2026-09-08T08:00:00.000000Z", "accepted",
                   correlation_id=CID_OTHER),
            record("task", "task-20260908-000001", "req-20260908-000001",
                   "2026-09-08T08:00:01.000000Z", "created",
                   correlation_id=CID_OTHER),
        ]
        summary = MODULE.verify_export(full_chain() + other)
        self.assertEqual(summary["correlations"], 2)


class ChainViolationTest(unittest.TestCase):
    """负例：A02 客户端覆盖、异步丢失、孤儿/重复根等全部拒绝。"""

    def test_orphan_delivery_rejected(self):
        chain = full_chain()
        chain[5]["parent_entity_id"] = "exe-20260907-999999"
        with self.assertRaisesRegex(MODULE.Violation, "chain.orphan_record"):
            MODULE.verify_export(chain)

    def test_async_loss_task_parent_missing_rejected(self):
        """异步丢失：task 的 request 父不在导出中（worker 丢 ctx）。"""
        chain = full_chain()
        chain[1]["parent_entity_id"] = "req-20260907-999999"
        with self.assertRaisesRegex(MODULE.Violation, "chain.orphan_record"):
            MODULE.verify_export(chain)

    def test_broken_chain_task_parent_wrong_type_rejected(self):
        """断链：task 的父指向 event 实体（非允许父类型）。"""
        chain = full_chain()
        chain[1]["parent_entity_id"] = "evt-20260907-000001"
        with self.assertRaisesRegex(MODULE.Violation, "chain.bad_parent_type"):
            MODULE.verify_export(chain)

    def test_task_with_null_parent_rejected(self):
        chain = full_chain()
        chain[1]["parent_entity_id"] = None
        with self.assertRaisesRegex(MODULE.Violation, "record\\[1\\].missing_parent"):
            MODULE.verify_export(chain)

    def test_duplicate_request_root_rejected(self):
        chain = full_chain() + [
            record("request", "req-20260907-000009", None,
                   "2026-09-07T08:00:00.000000Z", "accepted"),
        ]
        with self.assertRaisesRegex(MODULE.Violation,
                                    "chain.duplicate_request_root"):
            MODULE.verify_export(chain)

    def test_missing_request_root_rejected(self):
        with self.assertRaisesRegex(MODULE.Violation, "chain.no_request_root"):
            MODULE.verify_export(full_chain()[1:])

    def test_client_override_rejected(self):
        """契约 §3.3：导出中出现客户端覆盖事件即违规。"""
        chain = full_chain()
        chain[0]["client_override"] = True
        with self.assertRaisesRegex(MODULE.Violation, "client_override$"):
            MODULE.verify_export(chain)

    def test_client_override_non_bool_rejected(self):
        chain = full_chain()
        chain[0]["client_override"] = "yes"
        with self.assertRaisesRegex(MODULE.Violation,
                                    "client_override.not_bool"):
            MODULE.verify_export(chain)

    def test_timestamp_regression_rejected(self):
        chain = full_chain()
        chain[3]["timestamp"] = "2026-09-07T07:59:59.000000Z"  # 早于父 task
        with self.assertRaisesRegex(MODULE.Violation,
                                    "chain.timestamp_regression"):
            MODULE.verify_export(chain)

    def test_cross_correlation_parent_rejected(self):
        """同一实体不得跨链引用父（禁止静默第二条链共享实体）。"""
        other_root = record("request", "req-20260908-000001", None,
                            "2026-09-08T08:00:00.000000Z", "accepted",
                            correlation_id=CID_OTHER)
        chain = full_chain()
        chain[1]["parent_entity_id"] = "req-20260908-000001"
        with self.assertRaisesRegex(MODULE.Violation, "chain.orphan_record"):
            MODULE.verify_export(chain + [other_root])

    def test_duplicate_entity_id_rejected(self):
        chain = full_chain() + [
            record("task", "task-20260907-000001", "req-20260907-000001",
                   "2026-09-07T08:00:09.000000Z", "retried",
                   correlation_id=CID_OTHER),
        ]
        with self.assertRaisesRegex(MODULE.Violation,
                                    "chain.duplicate_entity_id"):
            MODULE.verify_export(chain)

    def test_duplicate_outcome_rejected(self):
        chain = full_chain() + [
            record("outcome", "out-20260907-000002", "dlv-20260907-000001",
                   "2026-09-07T08:05:04.000000Z", "failed"),
        ]
        with self.assertRaisesRegex(MODULE.Violation, "chain.duplicate_outcome"):
            MODULE.verify_export(chain)

    def test_self_parent_rejected(self):
        chain = full_chain()
        chain[2]["parent_entity_id"] = "evt-20260907-000001"
        with self.assertRaisesRegex(MODULE.Violation, "record\\[2\\].self_parent"):
            MODULE.verify_export(chain)

    def test_request_with_parent_rejected(self):
        chain = full_chain()
        chain[0]["parent_entity_id"] = "req-20260907-000001"
        with self.assertRaisesRegex(MODULE.Violation,
                                    "record\\[0\\].request_with_parent"):
            MODULE.verify_export(chain)


class RecordShapeTest(unittest.TestCase):
    """负例：记录级结构/格式/隐私红线。"""

    def test_entity_id_integer_rejected(self):
        chain = full_chain()
        chain[0]["entity_id"] = 12345678901234578901
        with self.assertRaisesRegex(MODULE.Violation,
                                    "entity_id.not_string"):
            MODULE.verify_export(chain)

    def test_entity_id_too_short_rejected(self):
        chain = full_chain()
        chain[0]["entity_id"] = "req-1"
        with self.assertRaisesRegex(MODULE.Violation, "entity_id.bad_pattern"):
            MODULE.verify_export(chain)

    def test_correlation_id_integer_rejected(self):
        chain = full_chain()
        chain[0]["correlation_id"] = 12345678901234567890
        with self.assertRaisesRegex(MODULE.Violation,
                                    "correlation_id.not_string"):
            MODULE.verify_export(chain)

    def test_bad_entity_type_rejected(self):
        chain = full_chain()
        chain[1]["entity_type"] = "job"
        with self.assertRaisesRegex(MODULE.Violation, "bad_entity_type"):
            MODULE.verify_export(chain)

    def test_timestamp_without_z_rejected(self):
        chain = full_chain()
        chain[0]["timestamp"] = "2026-09-07T16:00:00+08:00"
        with self.assertRaisesRegex(MODULE.Violation, "timestamp.bad_format"):
            MODULE.verify_export(chain)

    def test_status_uppercase_rejected(self):
        chain = full_chain()
        chain[0]["status"] = "Accepted"
        with self.assertRaisesRegex(MODULE.Violation, "status.bad_pattern"):
            MODULE.verify_export(chain)

    def test_missing_required_field_rejected(self):
        chain = full_chain()
        del chain[4]["status"]
        with self.assertRaisesRegex(MODULE.Violation,
                                    "record\\[4\\].missing:status"):
            MODULE.verify_export(chain)

    def test_sensitive_fields_rejected(self):
        """A03：导出 schema 不含消息正文/secret/URL 等字段。"""
        for field in ("message_body", "secret", "token", "url", "payload"):
            with self.subTest(field=field):
                chain = full_chain()
                chain[2][field] = "x"
                with self.assertRaisesRegex(
                        MODULE.Violation, "sensitive_field:%s" % field):
                    MODULE.verify_export(chain)

    def test_unknown_field_rejected(self):
        chain = full_chain()
        chain[0]["retry_count"] = 3
        with self.assertRaisesRegex(MODULE.Violation,
                                    "record\\[0\\].unknown_field:retry_count"):
            MODULE.verify_export(chain)

    def test_non_object_record_rejected(self):
        chain = full_chain()
        chain[2] = 42
        with self.assertRaisesRegex(MODULE.Violation,
                                    "record\\[2\\].not_object"):
            MODULE.verify_export(chain)

    def test_empty_export_rejected(self):
        with self.assertRaisesRegex(MODULE.Violation, "export.empty"):
            MODULE.verify_export([])

    def test_non_array_export_rejected(self):
        with self.assertRaisesRegex(MODULE.Violation, "export.not_array"):
            MODULE.verify_export({"records": full_chain()})


class CliTest(unittest.TestCase):
    """契约 §9 退出码：0=合法链，2=违规，1=用法错误。"""

    def run_cli(self, argv):
        stdout = io.StringIO()
        with contextlib.redirect_stdout(stdout):
            code = MODULE.main(argv)
        return code, stdout.getvalue()

    def test_valid_fixture_exit_zero(self):
        code, out = self.run_cli([str(FIXTURES / "valid.json")])
        self.assertEqual(code, 0)
        summary = json.loads(out)
        self.assertEqual(summary["decision"], "OK")
        self.assertEqual(summary["entity_counts"]["outcome"], 1)

    def assert_violation_fixture(self, name, expected_code):
        code, out = self.run_cli([str(FIXTURES / name)])
        self.assertEqual(code, 2, out)
        result = json.loads(out)
        self.assertEqual(result["decision"], "VIOLATION")
        self.assertTrue(any(code_ == expected_code
                            for code_ in result["errors"]), result)

    def test_orphan_delivery_fixture_exit_two(self):
        self.assert_violation_fixture(
            "orphan-delivery.json", "chain.orphan_record@5")

    def test_client_override_fixture_exit_two(self):
        self.assert_violation_fixture(
            "client-override.json", "record[0].client_override")

    def test_duplicate_root_fixture_exit_two(self):
        self.assert_violation_fixture(
            "duplicate-root.json", "chain.duplicate_request_root")

    def test_time_regression_fixture_exit_two(self):
        self.assert_violation_fixture(
            "time-regression.json", "chain.timestamp_regression@3")

    def test_broken_parent_type_fixture_exit_two(self):
        self.assert_violation_fixture(
            "broken-parent-type.json", "chain.bad_parent_type@1")

    def test_usage_error_exit_one(self):
        stdout = io.StringIO()
        with contextlib.redirect_stdout(stdout):
            self.assertEqual(MODULE.main([]), 1)

    def test_missing_file_exit_two(self):
        code, out = self.run_cli([str(FIXTURES / "no-such-file.json")])
        self.assertEqual(code, 2)
        self.assertIn("export.unreadable", out)

    def test_invalid_json_exit_two(self):
        with tempfile.TemporaryDirectory() as directory:
            bad = Path(directory) / "bad.json"
            bad.write_text("{not-json", encoding="utf-8")
            code, out = self.run_cli([str(bad)])
        self.assertEqual(code, 2)
        self.assertIn("export.unreadable", out)


if __name__ == "__main__":
    unittest.main()
