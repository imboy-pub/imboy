import importlib.util
import json
import socket
import tempfile
import threading
import time
import unittest
from http.client import HTTPConnection
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "agent_hub_http_status_fixture.py"
SPEC = importlib.util.spec_from_file_location("status_fixture", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)


class StatusFixtureTests(unittest.TestCase):
    def test_returns_fixed_status_and_exact_count(self):
        with socket.socket() as sock:
            sock.bind(("127.0.0.1", 0))
            port = sock.getsockname()[1]
        with tempfile.TemporaryDirectory() as tmp:
            ready = Path(tmp) / "ready.json"
            out = Path(tmp) / "out.json"
            result = []
            thread = threading.Thread(
                target=lambda: result.append(MODULE.main([
                    "--port", str(port), "--status", "503", "--requests", "2",
                    "--ready", str(ready), "--out", str(out),
                ]))
            )
            thread.start()
            for _ in range(100):
                if ready.exists():
                    break
                time.sleep(0.01)
            for _ in range(2):
                connection = HTTPConnection("127.0.0.1", port, timeout=2)
                connection.request("POST", "/hook", body=b"{}")
                self.assertEqual(503, connection.getresponse().status)
                connection.close()
            thread.join(2)
            self.assertEqual([0], result)
            self.assertEqual(2, json.loads(out.read_text())["requests"])


if __name__ == "__main__":
    unittest.main()
