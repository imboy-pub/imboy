import os
from pathlib import Path
import socket
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[2]
SUPERVISOR = ROOT / "test/common/pg_relay_supervisor.py"


class RelaySupervisorTest(unittest.TestCase):
    def run_case(self, wait_for_relay):
        with tempfile.TemporaryDirectory(prefix="relay-supervisor-test-") as directory:
            fifo = Path(directory) / "control"
            os.mkfifo(fifo)
            control = os.open(fifo, os.O_RDWR | os.O_NONBLOCK)
            target = socket.socket()
            target.bind(("127.0.0.1", 0))
            target.listen()
            probe = socket.socket()
            probe.bind(("127.0.0.1", 0))
            relay_port = probe.getsockname()[1]
            probe.close()
            unrelated = subprocess.Popen(["sleep", "30"], start_new_session=True)
            supervisor = subprocess.Popen(
                ["python3", str(SUPERVISOR), str(fifo), str(relay_port), str(target.getsockname()[1])],
                cwd=ROOT, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE,
            )
            client = None
            upstream = None
            try:
                if wait_for_relay:
                    deadline = time.monotonic() + 5
                    while True:
                        try:
                            client = socket.create_connection(("127.0.0.1", relay_port), timeout=0.1)
                            target.settimeout(2)
                            upstream, _ = target.accept()
                            break
                        except OSError:
                            if time.monotonic() >= deadline:
                                self.fail("relay did not start")
                            time.sleep(0.02)
                os.close(control)
                control = None
                self.assertEqual(supervisor.wait(timeout=5), 0)
                self.assertIsNone(unrelated.poll())
                if client is not None:
                    client.settimeout(2)
                    self.assertEqual(client.recv(1), b"")
                with socket.socket() as port:
                    port.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                    port.bind(("127.0.0.1", relay_port))
            finally:
                if control is not None:
                    os.close(control)
                if supervisor.poll() is None:
                    supervisor.terminate()
                    supervisor.wait(timeout=5)
                unrelated.terminate()
                unrelated.wait(timeout=5)
                if client is not None:
                    client.close()
                if upstream is not None:
                    upstream.close()
                target.close()
                supervisor.stderr.close()

    def test_active_relay_is_reaped_without_touching_unrelated_process(self):
        self.run_case(True)

    def test_owner_closes_during_startup(self):
        self.run_case(False)


if __name__ == "__main__":
    unittest.main()
