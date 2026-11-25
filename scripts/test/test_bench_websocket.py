#!/usr/bin/env python3
import asyncio
import base64
import json
import sys
import tempfile
import time
import unittest
from pathlib import Path
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

import bench_websocket as bench  # noqa: E402


def fake_jwt(uid, did=""):
    payload = json.dumps({"uid": uid, "did": did}).encode()
    encoded = base64.urlsafe_b64encode(payload).decode().rstrip("=")
    return f"header.{encoded}.signature"


class JwtCredentialTests(unittest.TestCase):
    def test_decode_bound_claims(self):
        self.assertEqual(
            bench.decode_jwt_claims(fake_jwt(42, "device-a")),
            {"uid": "42", "did": "device-a"},
        )

    def test_single_legacy_token_gets_unique_synthetic_dids(self):
        token = fake_jwt(42)
        credentials, mode = bench.build_credentials(3, token, None, run_id="run123")
        self.assertEqual(mode, "legacy-unbound")
        self.assertEqual([c.token for c in credentials], [token] * 3)
        self.assertEqual(len({c.header_did for c in credentials}), 3)
        self.assertTrue(all(c.header_did.startswith("imboy-bench-run123-") for c in credentials))

    def test_single_bound_token_cannot_be_reused_for_multiple_connections(self):
        with self.assertRaisesRegex(ValueError, "bound to one DID"):
            bench.build_credentials(2, fake_jwt(42, "device-a"), None)

    def test_single_bound_token_is_paired_with_its_claimed_did(self):
        credentials, mode = bench.build_credentials(1, fake_jwt(42, "device-a"), None)
        self.assertEqual(mode, "bound")
        self.assertEqual(credentials[0].header_did, "device-a")

    def test_token_file_requires_enough_tokens(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "tokens.txt"
            path.write_text(fake_jwt(42, "device-a") + "\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "provide at least one token per connection"):
                bench.build_credentials(2, None, str(path))

    def test_token_file_rejects_duplicate_bound_device(self):
        token = fake_jwt(42, "device-a")
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "tokens.txt"
            path.write_text(token + "\n" + token + "\n", encoding="utf-8")
            with self.assertRaisesRegex(ValueError, "repeats the same bound device"):
                bench.build_credentials(2, None, str(path))

    def test_token_file_accepts_distinct_bound_devices(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "tokens.txt"
            path.write_text(
                fake_jwt(42, "device-a") + "\n" + fake_jwt(42, "device-b") + "\n",
                encoding="utf-8",
            )
            credentials, mode = bench.build_credentials(2, None, str(path))
            self.assertEqual(mode, "bound")
            self.assertEqual([c.header_did for c in credentials], ["device-a", "device-b"])


class MetricsTests(unittest.TestCase):
    def test_derive_metrics_url_uses_current_metrics_route(self):
        self.assertEqual(
            bench.derive_metrics_url("ws://127.0.0.1:9800/api/v1/ws?x=1"),
            "http://127.0.0.1:9800/metrics",
        )
        self.assertEqual(
            bench.derive_metrics_url("wss://example.test:443/custom/ws"),
            "https://example.test:443/metrics",
        )

    def test_parse_r0_prometheus_metrics(self):
        raw = """# HELP imboy_ws_admission_current Current admitted WS slots
# TYPE imboy_ws_admission_current gauge
imboy_ws_admission_current 17
imboy_ws_admission_limit 20000
imboy_ws_admission_rejected_total 3
"""
        self.assertEqual(
            bench.parse_prometheus_metrics(raw),
            {"current": 17, "limit": 20000, "rejected_total": 3},
        )

    def test_missing_r0_metrics_fail_closed(self):
        with self.assertRaisesRegex(ValueError, "missing"):
            bench.parse_prometheus_metrics("imboy_ws_admission_current 1\n")

    def test_server_delta_must_match_client_live_count(self):
        baseline = {"current": 10, "limit": 20000, "rejected_total": 0}
        exact = {"current": 13, "limit": 20000, "rejected_total": 0}
        drift = {"current": 12, "limit": 20000, "rejected_total": 0}
        self.assertEqual(bench.metrics_delta_matches(baseline, exact, 3), (True, 3, 0))
        self.assertEqual(bench.metrics_delta_matches(baseline, drift, 3), (False, 2, -1))
        self.assertEqual(bench.metrics_delta_matches(baseline, drift, 3, tolerance=1), (True, 2, -1))

    def test_default_ws_path_is_current_v1_route(self):
        with patch.dict("os.environ", {}, clear=True):
            cfg = bench.load_config()
        self.assertEqual(cfg.ws_url, "ws://127.0.0.1:9800/api/v1/ws")
        self.assertEqual(cfg.metrics_url, "http://127.0.0.1:9800/metrics")
        self.assertTrue(cfg.metrics_required)
        self.assertEqual(cfg.ping_interval, 15.0)

    def test_client_only_mode_must_be_explicit(self):
        with patch.dict("os.environ", {"METRICS_URL": "off", "METRICS_REQUIRED": "0"}, clear=True):
            cfg = bench.load_config()
        self.assertIsNone(cfg.metrics_url)
        self.assertFalse(cfg.metrics_required)

    def test_metrics_off_without_explicit_opt_out_is_rejected(self):
        with patch.dict("os.environ", {"METRICS_URL": "off"}, clear=True):
            with self.assertRaisesRegex(ValueError, "requires METRICS_REQUIRED=0"):
                bench.load_config()



class FakeWebSocket:
    def __aiter__(self):
        return self

    async def __anext__(self):
        await asyncio.Future()


class FakeWebSocketsModule:
    def __init__(self):
        self.kwargs = None
        self.ws = FakeWebSocket()

    async def connect(self, *_args, **kwargs):
        self.kwargs = kwargs
        return self.ws


class RampKeepaliveTests(unittest.IsolatedAsyncioTestCase):
    async def test_ping_keepalive_is_enabled_during_ramp(self):
        with patch.dict("os.environ", {"PING_INTERVAL": "0.25", "PING_TIMEOUT": "1"}, clear=True):
            cfg = bench.load_config()
        credential = bench.Credential("fake.jwt.token", "42", "", "bench-run-000001")
        fake_module = FakeWebSocketsModule()
        conn, error = await bench._connect_one(
            0, credential, time.monotonic(), cfg, asyncio.Semaphore(1), fake_module
        )
        self.assertIsNone(error)
        self.assertIsNotNone(conn)
        self.assertEqual(fake_module.kwargs["ping_interval"], 0.25)
        self.assertEqual(fake_module.kwargs["ping_timeout"], 1.0)
        conn.receiver_task.cancel()
        await asyncio.gather(conn.receiver_task, return_exceptions=True)


if __name__ == "__main__":
    unittest.main(verbosity=2)
