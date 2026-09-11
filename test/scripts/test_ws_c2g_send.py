#!/usr/bin/env python3

import importlib.util
import os
import unittest
from pathlib import Path
from unittest.mock import patch


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/smoke/ws_c2g_send.py"


def load_script():
    env = {
        "WS_URL": "ws://127.0.0.1:1/api/v1/ws",
        "WS_TOKEN": "fixture-token",
        "WS_FROM_UID": "42",
        "WS_GID": "1",
        "WS_MSG_ID": "fixture-message",
        "WS_TEXT": "fixture-text",
    }
    with patch.dict(os.environ, env, clear=False):
        spec = importlib.util.spec_from_file_location("ws_c2g_send", SCRIPT)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module


class MentionsTest(unittest.TestCase):
    def test_optional_mentions(self):
        module = load_script()
        self.assertIsNone(module.parse_mentions(None))
        self.assertEqual(module.parse_mentions('["42", 43]'), ["42", 43])
        with self.assertRaises(ValueError):
            module.parse_mentions('{"uid": 42}')

    def test_protocol_errors(self):
        module = load_script()
        self.assertTrue(module.is_error_frame('{"type":"C2G_ERROR"}'))
        self.assertTrue(module.is_error_frame(
            '{"type":"S2C","action":"invalid_message"}'))
        self.assertFalse(module.is_error_frame(
            '{"type":"C2G_SERVER_ACK"}'))


if __name__ == "__main__":
    unittest.main()
