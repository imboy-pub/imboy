"""Synthetic public responses that must not be rejected by the API schemas."""
from pathlib import Path
import unittest
from urllib.parse import unquote, urlparse

from jsonschema import Draft202012Validator
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


if __name__ == "__main__":
    unittest.main()
