import hashlib
import hmac
import importlib.util
import json
import pathlib
import time
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "agent_hub_bot_webhook_fixture",
    ROOT / "scripts" / "agent_hub_bot_webhook_fixture.py",
)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class BotWebhookFixtureTest(unittest.TestCase):
    def request(self, secret="fixture-verify-secret", delivery_id="dlv-1", now=None):
        current = int(time.time()) if now is None else int(now)
        body = json.dumps(
            {"event": "message.c2g_mention", "delivery_id": delivery_id}
        ).encode("utf-8")
        timestamp = str(current)
        return body, {
            "x-imboy-timestamp": timestamp,
            "x-imboy-delivery": delivery_id,
            "x-imboy-event": "message.c2g_mention",
            "x-imboy-signature": MODULE.signature_for(timestamp, body, secret),
        }

    def test_valid_request_matches_all_headers(self):
        body, headers = self.request(now=1000)
        checks, envelope = MODULE.verify_request(
            headers, body, "fixture-verify-secret", now=1001
        )
        self.assertTrue(all(ok for _, ok in checks))
        self.assertEqual("dlv-1", envelope["delivery_id"])

    def test_stale_timestamp_is_rejected(self):
        body, headers = self.request(now=1000)
        checks, _ = MODULE.verify_request(
            headers, body, "fixture-verify-secret", now=1301
        )
        self.assertFalse(dict(checks)["timestamp within 300 seconds"])

    def test_delivery_header_mismatch_is_rejected(self):
        body, headers = self.request()
        headers["x-imboy-delivery"] = "dlv-other"
        checks, _ = MODULE.verify_request(headers, body, "fixture-verify-secret")
        self.assertFalse(dict(checks)["delivery header matches body"])

    def test_forged_context_uses_only_bot_secret(self):
        payload = json.dumps({"group_id": 7, "exp": 1}).encode("utf-8")
        token = (
            MODULE.base64.b64encode(payload).decode("ascii")
            + "."
            + MODULE.base64.b64encode(b"old-signature").decode("ascii")
        )
        forged = MODULE.forge_with_bot_secret(token, 8, "bot-secret")
        encoded, signature = forged.split(".", 1)
        raw = MODULE.base64.b64decode(encoded)
        expected = hmac.new(b"bot-secret", raw, hashlib.sha256).digest()
        self.assertTrue(hmac.compare_digest(expected, MODULE.base64.b64decode(signature)))
        self.assertEqual(8, json.loads(raw)["group_id"])

    def test_response_classification(self):
        self.assertTrue(MODULE.api_success((200, {"code": 0, "payload": {}})))
        self.assertTrue(MODULE.api_rejected((200, {"code": 1, "payload": {}})))
        self.assertFalse(MODULE.api_rejected((500, {"code": 1})))


if __name__ == "__main__":
    unittest.main()
