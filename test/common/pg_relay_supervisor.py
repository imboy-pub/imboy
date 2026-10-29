#!/usr/bin/env python3
"""Stop only this invocation's relay group when its owner's FIFO closes."""
import fcntl
import os
import signal
import subprocess
import sys


def stop(_signum, _frame):
    raise SystemExit(1)


def main():
    fifo, listen, target = sys.argv[1:]
    os.setsid()
    signal.signal(signal.SIGTERM, stop)
    signal.signal(signal.SIGINT, stop)
    relay = None
    try:
        fd = os.open(fifo, os.O_RDONLY | os.O_NONBLOCK)
        fcntl.fcntl(fd, fcntl.F_SETFL, 0)
        relay = subprocess.Popen([
            "sh", "-c", 'while :; do python3 test/common/pg_relay.py "$1" "$2"; sleep 1; done',
            "relay", listen, target,
        ], stdin=subprocess.DEVNULL)
        with os.fdopen(fd, "rb") as control:
            while control.read(1):
                pass
    finally:
        if relay is not None:
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            group = os.getpgrp()
            killer = os.fork()
            if killer == 0:
                os.setsid()
                try:
                    # Freeze the group before signalling so a relay cannot fork
                    # another worker between the signal and process exit.
                    os.killpg(group, signal.SIGSTOP)
                    try:
                        os.killpg(group, signal.SIGTERM)
                    finally:
                        os.killpg(group, signal.SIGCONT)
                finally:
                    os._exit(0)
            os.waitpid(killer, 0)
            try:
                relay.wait(timeout=3)
            except subprocess.TimeoutExpired:
                os.killpg(os.getpgrp(), signal.SIGKILL)


if __name__ == "__main__":
    main()
