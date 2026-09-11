#!/usr/bin/env python3

import importlib.util
import json
import threading
import unittest
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/agent_hub_channel_webhook_smoke.py"
SPEC = importlib.util.spec_from_file_location("channel_webhook_smoke", SCRIPT)
CLIENT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CLIENT)


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *_args):
        pass

    def do_POST(self):
        length = int(self.headers.get("Content-Length") or 0)
        self.rfile.read(length)
        if self.path.endswith("/active-token-fixture"):
            status, body = 200, b'{"ok":true}'
        else:
            status, body = 404, b'{"ok":false,"error":"not_found"}'
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


class ChannelWebhookSmokeTest(unittest.TestCase):
    def setUp(self):
        self.server = HTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()

    def test_three_request_contract(self):
        base = f"http://127.0.0.1:{self.server.server_address[1]}"
        active_token = "active-token-fixture"
        disabled_token = "disabled-token-fixture"
        result = CLIENT.run(base, active_token, disabled_token, "fixture-message")
        self.assertEqual(4, result["passed"])
        self.assertEqual(0, result["failed"])
        serialized = json.dumps(result)
        self.assertNotIn(active_token, serialized)
        self.assertNotIn(disabled_token, serialized)

    def test_non_loopback_is_rejected(self):
        with self.assertRaises(ValueError):
            CLIENT.guard("https://example.com")


if __name__ == "__main__":
    unittest.main()
