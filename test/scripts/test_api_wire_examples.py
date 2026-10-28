"""Synthetic public responses that must not be rejected by the API schemas."""
from pathlib import Path
import unittest
from urllib.parse import unquote, urlparse

from jsonschema import Draft202012Validator, FormatChecker
from referencing import Registry, Resource
from referencing.jsonschema import DRAFT202012
import yaml

ROOT = Path(__file__).resolve().parents[2]
API = ROOT / "api"


def retrieve(uri):
    path = Path(unquote(urlparse(uri).path))
    document = yaml.safe_load(path.read_text(encoding="utf-8"))
    return Resource.from_contents(document, default_specification=DRAFT202012)


def validator(path, schema=None):
    path = path.resolve()
    if schema is None:
        schema = yaml.safe_load(path.read_text(encoding="utf-8"))
    return Draft202012Validator({"$id": path.as_uri(), **schema},
                                registry=Registry(retrieve=retrieve))


class WireExamplesTest(unittest.TestCase):
    def test_client_summary_matches_generated_and_not_generated_responses(self):
        path = API / "paths/api/v1/talk-records/summary_item.yaml"
        schema = yaml.safe_load(path.read_text())["get"]["responses"]["200"]["content"]["application/json"]["schema"]
        check = validator(path, schema)
        envelope = {"code": 0, "msg": "success", "sv_ts": 1792713600000}
        generated = {"id": 101, "status": "draft", "overview": "summary", "decisions": [],
                     "actions": [], "source_transcript_version": 1, "source_content_revision": 1,
                     "model_version": None, "confirmed_by": None, "revision": 1,
                     "etag": "summary:1", "created_at": 1792713600000, "updated_at": None}
        missing = {"status": "not_generated", "summary_status": "none", "summary": None}
        for payload in [generated, missing]:
            self.assertTrue(check.is_valid({**envelope, "payload": payload}))
        for key in generated:
            with self.subTest(missing_generated_field=key):
                payload = {k: v for k, v in generated.items() if k != key}
                self.assertFalse(check.is_valid({**envelope, "payload": payload}))
        for payload in [{}, {"status": "not_generated"}, {**missing, "summary": {}}]:
            self.assertFalse(check.is_valid({**envelope, "payload": payload}))

    def test_client_processing_request_dispatch_matches_execution_mode(self):
        path = API / "paths/api/v1/talk-records/processing_requests.yaml"
        schema = yaml.safe_load(path.read_text())["post"]["requestBody"]["content"]["application/json"]["schema"]
        check = validator(path, schema)
        legacy = {"mode": "transcribe", "processing_consent": {
            "policy_version": "v1", "processor_config_version": "v1"}}
        self.assertTrue(check.is_valid(legacy))
        for execution_mode in [None, "server_managed", "unknown", "client_byok"]:
            self.assertFalse(check.is_valid({**legacy, "execution_mode": execution_mode}))
        pins = {"execution_mode": "client_byok", "provider": "minimax", "model": "asr-1.0",
                "policy_version": "v1", "config_version": "a" * 64}
        client = {"mode": "transcribe", **pins, "processing_consent": {**pins, "confirmed": True}}
        self.assertTrue(check.is_valid(client))

    def test_business_error_is_distinct_from_success(self):
        check = validator(API / "components/schemas/BusinessErrorEnvelope.yaml")
        response = {"code": 403, "msg": "forbidden", "sv_ts": 1792713600000, "payload": {}}
        self.assertTrue(check.is_valid(response))
        self.assertFalse(check.is_valid({**response, "code": 0}))

    def test_visitor_session_list_does_not_require_private_contact_id(self):
        check = validator(API / "components/schemas/customer-service/VisitorSession.yaml")
        response = {"id": "101", "organization_id": "102", "workspace_id": "103",
                    "conversation_id": "104", "status": "queued", "version": 1}
        self.assertTrue(check.is_valid(response), list(check.iter_errors(response)))

    def test_masked_name_and_minimal_context_history(self):
        check = validator(API / "components/schemas/customer-service/CustomerContext.yaml")
        response = {"session_id": "101", "workspace_id": "103", "source": "widget",
                    "contact": {"masked_name": "guest#12345", "first_seen": 1792713600,
                                "last_seen": 1792713600},
                    "history": {"sessions": [{"id": "105", "workspace_id": "103",
                                              "status": "closed", "version": 1}],
                                "next_after_id": None}, "notes": []}
        self.assertTrue(check.is_valid(response), list(check.iter_errors(response)))

    def test_presence_clear_requires_omission_not_null(self):
        path = API / "paths/api/v1/customer-service/seat-presence-status.yaml"
        document = yaml.safe_load(path.read_text())
        schema = document["put"]["requestBody"]["content"]["application/json"]["schema"]
        check = validator(path, schema)
        self.assertTrue(check.is_valid({"workspace_id": "103"}))
        self.assertTrue(check.is_valid({"workspace_id": "103", "manual_status": "away"}))
        self.assertFalse(check.is_valid({"workspace_id": "103", "manual_status": None}))

    def test_internal_upload_request_selects_registration_or_existing_session(self):
        path = API / "paths/internal/v1/file/presign.yaml"
        operation = yaml.safe_load(path.read_text())["post"]
        schema = operation["requestBody"]["content"]["application/json"]["schema"]
        check = validator(path, schema)
        declaration = {"file_name": "example.txt", "mime_type": "text/plain"}
        client_id = "00000000-0000-4000-8000-000000000001"
        valid = [declaration, {**declaration, "operation": "register"}]
        invalid = [{}, {"operation": "register", "file_name": "example.txt"},
                   {**declaration, "operation": "unknown"}]
        for action in ("status", "renew"):
            valid.extend(({"operation": action, "client_upload_id": client_id},
                          {"operation": action, "object_key": "eoa/example.txt"}))
            invalid.extend(({"operation": action}, {**declaration, "operation": action},
                            {"operation": action, "client_upload_id": None},
                            {"operation": action, "object_key": None}))
        for response in valid:
            with self.subTest(valid=response):
                self.assertTrue(check.is_valid(response), list(check.iter_errors(response)))
        for response in invalid:
            with self.subTest(invalid=response):
                self.assertFalse(check.is_valid(response))

    def test_internal_upload_response_allows_capability_and_current_status(self):
        path = API / "paths/internal/v1/file/presign.yaml"
        operation = yaml.safe_load(path.read_text())["post"]
        schema = operation["responses"]["200"]["content"]["application/json"]["schema"]
        check = validator(path, schema)
        identity = {"object_key": "eoa/example.txt",
                    "client_upload_id": "00000000-0000-4000-8000-000000000001",
                    "session_deadline": 1792717200}
        capability = {**identity, "session_state": "registered", "expires_at": 1792713900,
                      "put_url": "/api/v1/attachment/gateway/example?capability=synthetic",
                      "upload_capability": "synthetic", "gateway": True,
                      "bucket": "example", "mime_type": "text/plain"}
        status = {**identity, "session_state": "registered", "upload_expires_at": 1792713900,
                  "can_resume": True, "retry_action": "resume_same_id"}
        confirmed = {**status, "session_state": "confirmed", "file_id": 101, "size": 12,
                     "mime_type": "text/plain", "lifecycle_state": "live",
                     "can_resume": False, "retry_action": "none"}
        for response in (capability, status, confirmed):
            with self.subTest(valid=response):
                self.assertTrue(check.is_valid(response), list(check.iter_errors(response)))
        for response in ({"expires_at": 1792713900}, identity,
                         {**capability, "expires_at": None},
                         {**status, "upload_expires_at": "1792713900"},
                         {**confirmed, "file_id": "101"},
                         {**status, "undocumented_field": True}):
            with self.subTest(invalid=response):
                self.assertFalse(check.is_valid(response))


class HistoryTimezoneContractTest(unittest.TestCase):
    def history_operation(self):
        path = API / "paths/api/v1/msg/history.yaml"
        return path, yaml.safe_load(path.read_text())["get"]

    def backfill_response(self):
        root = yaml.safe_load((API / "openapi-api.yaml").read_text())
        operation_path = API / root["paths"]["/api/v1/msg"]["$ref"]
        operation = yaml.safe_load(operation_path.read_text())["get"]
        path = (operation_path.parent / operation["responses"]["200"]["$ref"]).resolve()
        return path, yaml.safe_load(path.read_text())["content"]["application/json"]

    def test_archived_acceptance_is_required_nullable_epoch_milliseconds(self):
        path = API / "components/schemas/client/ArchivedMessage.yaml"
        check = validator(path).evolve(
            format_checker=FormatChecker())
        message = {"msg_id": "合成-msg_1", "chat_type": "c2c", "conv_seq": 1,
                   "msg_type": "text", "from": "101", "to": "102", "sender_did": None,
                   "e2ee": None, "payload": {"text": "你好 🌏"},
                   "created_at": 1792713600000, "server_ts": None}
        for accepted_at in (None, 0, 1792713600123):
            with self.subTest(valid=accepted_at):
                self.assertTrue(check.is_valid({**message, "accepted_at": accepted_at}))
        for accepted_at in ("2026-10-23T00:00:00.123456Z",
                            "2026-10-23T08:00:00.123456+08:00",
                            "1792713600000", "", {}, [], True, False, 1792713600000.5,
                            "2026-10-23T00:00:00", "2026-02-30T00:00:00Z"):
            with self.subTest(invalid=accepted_at):
                self.assertFalse(check.is_valid({**message, "accepted_at": accepted_at}))
        self.assertFalse(check.is_valid(message))
        schema = yaml.safe_load(path.read_text())["properties"]["accepted_at"]
        self.assertEqual(schema["format"], "int64")

    def test_history_requires_safe_progress_and_explicit_gap_reasons(self):
        path, operation = self.history_operation()
        check = validator(path, operation["responses"]["200"]["content"]["application/json"]["schema"])
        envelope = {"code": 0, "msg": "success", "sv_ts": 1792713600000}
        payload = {"messages": [], "next_seq": 0, "has_more": False,
                   "conv_key": "c2c:101:102", "safe_head": 0, "blocked": False, "gaps": []}
        self.assertTrue(check.is_valid({**envelope, "payload": payload}))
        for field in ("safe_head", "blocked", "gaps"):
            with self.subTest(missing=field):
                self.assertFalse(check.is_valid({**envelope, "payload": {
                    key: value for key, value in payload.items() if key != field}}))
        for reason in ("authorization", "terminal_failure"):
            page = {**payload, "safe_head": 9, "next_seq": 9, "blocked": True,
                    "gaps": [{"from_seq": 1, "to_seq": 2, "reason": reason}]}
            self.assertTrue(check.is_valid({**envelope, "payload": page}))
        for invalid in ({"safe_head": -1}, {"safe_head": None}, {"safe_head": "0"},
                        {"blocked": 1}, {"gaps": None},
                        {"gaps": [{"from_seq": 0, "to_seq": 1, "reason": "terminal_failure"}]},
                        {"gaps": [{"from_seq": 1, "to_seq": 2, "reason": "timeout"}]},
                        {"gaps": [{"from_seq": 1, "reason": "authorization"}]}):
            with self.subTest(invalid=invalid):
                self.assertFalse(check.is_valid({**envelope, "payload": {**payload, **invalid}}))

    def test_backfill_payload_has_scan_limit_without_new_wire_keys(self):
        path, operation = self.history_operation()
        action_path = API / "paths/api/v1/msg/backfill.yaml"
        action = yaml.safe_load(action_path.read_text())["get"]
        self.assertEqual(action["x-action"], "history_backfill")
        self.assertEqual(action["x-handler"], "msg_handler")
        self.assertEqual(action["security"], operation["security"])
        response_path, response = self.backfill_response()
        check = validator(response_path, response["schema"])
        envelope = {"code": 0, "msg": "success", "sv_ts": 1792713600000}
        payload = {"messages": [], "next_seq": 1000, "has_more": True,
                   "conv_key": "c2c:101:102", "range_limit": 1000}
        for next_seq, has_more in ((1000, True), (2501, False), (0, False)):
            self.assertTrue(check.is_valid({**envelope, "payload": {
                **payload, "next_seq": next_seq, "has_more": has_more}}))
        for invalid in ({"range_limit": 0}, {"range_limit": None}, {"next_seq": -1},
                        {"target_seq": 2501}, {"safe_head": 2501}):
            with self.subTest(invalid=invalid):
                self.assertFalse(check.is_valid({**envelope, "payload": {**payload, **invalid}}))
        self.assertFalse(check.is_valid({**envelope, "payload": {
            key: value for key, value in payload.items() if key != "range_limit"}}))
        names = {parameter["name"] for parameter in operation["parameters"]}
        self.assertEqual(names, {"chat_type", "peer_id", "after_seq", "limit", "did", "target_msg_id"})
        names = {parameter["name"] for parameter in action["parameters"]}
        self.assertEqual(names, {"action", "chat_type", "peer_id", "after_seq", "limit", "did"})
        selector = next(parameter for parameter in action["parameters"] if parameter["name"] == "action")
        self.assertFalse(selector["required"])
        self.assertEqual(selector["schema"]["enum"], ["history_backfill"])

    def test_history_and_backfill_documented_wire_examples_validate(self):
        path, operation = self.history_operation()
        history = operation["responses"]["200"]["content"]["application/json"]
        for response_path, response in ((path, history), self.backfill_response()):
            check = validator(response_path, response["schema"]).evolve(format_checker=FormatChecker())
            self.assertGreaterEqual(len(response["examples"]), 3)
            for name, example in response["examples"].items():
                with self.subTest(contract=str(response_path), example=name):
                    self.assertTrue(check.is_valid(example["value"]),
                                    list(check.iter_errors(example["value"])))


if __name__ == "__main__":
    unittest.main()
