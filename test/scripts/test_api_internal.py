"""Keep the delivered Internal contract aligned with the registered runtime API."""
import copy
import json
from pathlib import Path
import re
import unittest

from jsonschema import Draft202012Validator, FormatChecker
import yaml

ROOT = Path(__file__).resolve().parents[2]
API = ROOT / "api"
CHANNEL_PATH = "/api/internal/v1/channels/{channel_id}/messages"
METHODS = {"get", "post", "put", "patch", "delete", "head", "options", "trace"}


def read_yaml(path):
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def registered_routes():
    source = (ROOT / "src/api/enterprise_internal_routes.erl").read_text(encoding="utf-8")
    blocks = re.findall(r'#\{\s*id => <<"(INT-\d+)">>,(.*?)\n\s*\}', source, re.S)
    routes = {}
    for identifier, block in blocks:
        route = {}
        for name in ("method", "path"):
            route[name] = re.search(rf'{name} => <<"([^"]+)">>', block).group(1)
        for name in ("rate_bucket", "idempotency", "sender_mode"):
            route[name] = re.search(rf'{name} => ([a-z_]+)', block).group(1)
        if identifier in routes:
            raise AssertionError(f"duplicate runtime route {identifier}")
        routes[identifier] = route
    # Refuse to silently miss a registered route if the Erlang source shape changes.
    declared = re.findall(r'id => <<"(INT-\d+)">>', source)
    if sorted(routes) != sorted(declared):
        raise AssertionError("not every runtime route was parsed")
    return routes


def collection_requests(items):
    for item in items:
        if "request" in item:
            yield item
        yield from collection_requests(item.get("item", []))


class InternalInventoryTest(unittest.TestCase):
    def test_runtime_manifest_and_openapi_operations_match(self):
        runtime = registered_routes()
        manifest = read_yaml(API / "internal/v1/manifest.yaml")
        entries = [r for r in manifest["routes"] if r["id"].startswith("INT-")]
        self.assertEqual(len(entries), len({r["id"] for r in entries}))
        self.assertEqual(set(runtime), {r["id"] for r in entries})
        root = read_yaml(API / "openapi-internal.yaml")
        bundle = read_yaml(API / "openapi-internal.bundle.yaml")
        self.assertEqual(set(root["paths"]), set(bundle["paths"]))
        documented = {(method.upper(), path)
                      for path, node in bundle["paths"].items()
                      for method in node if method in METHODS}
        self.assertEqual({(r["method"], r["path"]) for r in runtime.values()}, documented)
        for entry in entries:
            with self.subTest(route=entry["id"]):
                actual = runtime[entry["id"]]
                for field in ("method", "path", "idempotency"):
                    self.assertEqual(actual[field], entry[field])
                self.assertEqual(actual["rate_bucket"], entry["rate"])
                self.assertEqual(actual["sender_mode"].replace("application_human", "application|human"),
                                 entry["sender_mode"])
                operation = bundle["paths"][actual["path"]][actual["method"].lower()]
                self.assertEqual(actual["rate_bucket"], operation["x-imboy-rate-bucket"])
                self.assertEqual(actual["idempotency"], operation["x-imboy-idempotency"])
        self.assertEqual("POST", runtime["INT-50"]["method"])
        self.assertEqual(CHANNEL_PATH, runtime["INT-50"]["path"])

    def test_postman_covers_registered_routes_and_channel_example(self):
        document = json.loads((API / "internal/v1/IMBoy-Internal-API-v1.postman_collection.json")
                              .read_text(encoding="utf-8"))
        entries = list(collection_requests(document["item"]))
        operations = []
        for item in entries:
            request = item["request"]
            url = request["url"]
            raw = url["raw"] if isinstance(url, dict) else url
            if not raw.startswith("{{base_url}}/api/internal/v1/"):
                continue
            path = raw.removeprefix("{{base_url}}").split("?", 1)[0]
            path = re.sub(r"\{\{([^{}]+)\}\}", r"{\1}", path)
            operations.append((request["method"], path))
        runtime = {(r["method"], r["path"]) for r in registered_routes().values()}
        self.assertEqual(len(operations), len(set(operations)))
        self.assertEqual(runtime, set(operations))
        request = next(item["request"] for item in entries if item["name"].startswith("INT-50 "))
        operation = read_yaml(API / "paths/internal/v1/channel/messages.yaml")["post"]
        schema = operation["requestBody"]["content"]["application/json"]["schema"]
        Draft202012Validator(schema).validate(json.loads(request["body"]["raw"]))
        self.assertIn({"key": "Idempotency-Key", "value": "{{idempotency_key}}"}, request["header"])


class ChannelMessageContractTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.operation = read_yaml(API / "paths/internal/v1/channel/messages.yaml")["post"]
        cls.request_schema = cls.operation["requestBody"]["content"]["application/json"]["schema"]
        cls.response_schema = cls.operation["responses"]["200"]["content"]["application/json"]["schema"]
        Draft202012Validator.check_schema(cls.request_schema)
        Draft202012Validator.check_schema(cls.response_schema)
        cls.request_validator = Draft202012Validator(cls.request_schema)
        cls.response_validator = Draft202012Validator(cls.response_schema, format_checker=FormatChecker())

    def test_hmac_and_required_idempotency_are_preserved(self):
        self.assertEqual([{
            "ibIntHmacCredential": [], "ibIntHmacTimestamp": [],
            "ibIntHmacNonce": [], "ibIntHmacSignature": []
        }], self.operation["security"])
        header = next(p for p in self.operation["parameters"] if p["name"] == "Idempotency-Key")
        self.assertEqual("header", header["in"])
        self.assertTrue(header["required"])

    def test_all_message_types_and_both_sender_modes(self):
        payloads = [
            {"msg_type": "text", "content": "project update"},
            {"msg_type": "file", "object_key": "synthetic/confirmed-file"},
            {"msg_type": "image", "object_key": "synthetic/confirmed-image", "width": 640, "height": 480},
            {"msg_type": "link_card", "title": "Project", "url": "https://oa.example.com/project"},
            {"msg_type": "business_action_card", "title": "Review", "action_type": "review_project",
             "action_params": {"project": "synthetic-project"}},
        ]
        for payload in payloads:
            for sender in ({"sender_mode": "application"},
                           {"sender_mode": "human", "sender_user_id": "oa-user-001"}):
                with self.subTest(payload=payload, sender=sender):
                    self.request_validator.validate(dict(payload, **sender))

    def test_invalid_required_fields_and_sender_aliases_are_rejected(self):
        valid = {"sender_mode": "application", "msg_type": "text", "content": "hello"}
        invalid = [
            {}, dict(valid, msg_type="rtc"), dict(valid, content=""), dict(valid, content=123),
            dict(valid, sender_mode="human"), dict(valid, sender_mode="human", sender_user_id=123),
            dict(valid, sender_mode="human", sender_user_id=""),
            {"sender_mode": "application", "msg_type": "file"},
            {"sender_mode": "application", "msg_type": "image", "object_key": ""},
            {"sender_mode": "application", "msg_type": "link_card", "title": "Project", "url": "https:///path"},
            {"sender_mode": "application", "msg_type": "business_action_card", "title": "Review"},
            {"sender_mode": "application", "msg_type": "business_action_card", "title": "Review",
             "action_type": "review", "action_params": []},
            {"sender_mode": "application", "msg_type": "business_action_card", "title": "Review",
             "action_type": "review", "url": None},
        ]
        for alias in ("actor_user_id", "as_user_id"):
            invalid.extend(dict(valid, **{alias: value}) for value in (None, "oa-user-001", 123))
        for payload in invalid:
            with self.subTest(payload=payload):
                self.assertFalse(self.request_validator.is_valid(payload))

    def test_ignored_optional_values_and_link_url_match_runtime(self):
        # These are ignored by the handler; documenting them as hard failures
        # would reject currently accepted clients. Required fields stay strict.
        self.request_validator.validate({
            "sender_mode": "application", "sender_user_id": None,
            "msg_type": "image", "object_key": "synthetic/confirmed-image",
            "width": -1, "height": "ignored", "extra": {"ignored": True},
        })
        self.request_validator.validate({
            "sender_mode": "application", "msg_type": "link_card", "title": "Project",
            "url": "https://oa.example.com/" + "p" * 2048,
            "description": None, "thumb": "ignored-invalid-url",
        })
        self.assertFalse(self.request_validator.is_valid({
            "sender_mode": "application", "msg_type": "business_action_card", "title": "Review",
            "action_type": "review", "url": "https://oa.example.com/" + "p" * 2048,
        }))

    def test_response_contains_bare_numeric_principal_or_human_id(self):
        response = {
            "msg_id": "019a5500-0000-7000-8000-000000000001",
            "webhook_resource_id": 9100000000000000701,
            "sender_kind": "application", "sender_user_id": 9100000000000000702,
            "origin_kind": "enterprise_application", "origin_application_id": 9100000000000000703,
            "accepted_at": 1793836800000,
        }
        self.response_validator.validate(response)
        self.response_validator.validate(dict(response, sender_kind="human"))
        for field in ("sender_user_id", "origin_application_id", "webhook_resource_id"):
            for value in (None, str(response[field]), 0):
                with self.subTest(field=field, value=value):
                    self.assertFalse(self.response_validator.is_valid(dict(response, **{field: value})))
        self.assertFalse(self.response_validator.is_valid({"code": 0, "payload": response}))
        missing_row_id = copy.deepcopy(response)
        del missing_row_id["webhook_resource_id"]
        self.assertFalse(self.response_validator.is_valid(missing_row_id))


if __name__ == "__main__":
    unittest.main()
