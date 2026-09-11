#!/usr/bin/env python3
"""Loopback HTTP fixture returning one fixed status for webhook retry checks."""

import argparse
import json
import time
from http.server import BaseHTTPRequestHandler, HTTPServer


class Handler(BaseHTTPRequestHandler):
    def log_message(self, *_args):
        pass

    def do_POST(self):
        length = int(self.headers.get("Content-Length", "0"))
        self.rfile.read(length)
        self.server.requests += 1
        self.send_response(self.server.response_status)
        self.send_header("Content-Length", "0")
        self.send_header("Connection", "close")
        self.end_headers()


def write_json(path, value):
    with open(path, "w", encoding="utf-8") as handle:
        json.dump(value, handle, ensure_ascii=True, indent=2)
        handle.write("\n")


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", type=int, required=True)
    parser.add_argument("--status", type=int, required=True)
    parser.add_argument("--requests", type=int, required=True)
    parser.add_argument("--ready", required=True)
    parser.add_argument("--out", required=True)
    args = parser.parse_args(argv)
    if not 400 <= args.status <= 599 or not 1 <= args.requests <= 10:
        return 2

    server = HTTPServer(("127.0.0.1", args.port), Handler)
    server.response_status = args.status
    server.requests = 0
    server.timeout = 1
    write_json(args.ready, {"host": "127.0.0.1", "port": server.server_port})
    deadline = time.monotonic() + 30
    while server.requests < args.requests and time.monotonic() < deadline:
        server.handle_request()
    server.server_close()
    result = {
        "status": args.status,
        "expected_requests": args.requests,
        "requests": server.requests,
        "passed": server.requests == args.requests,
    }
    write_json(args.out, result)
    return 0 if result["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
