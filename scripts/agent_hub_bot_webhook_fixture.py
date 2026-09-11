#!/usr/bin/env python3
"""Loopback-only Bot webhook receiver for the Agent Hub golden flow."""

import argparse
import base64
import binascii
import hashlib
import hmac
import json
import os
import sys
import time
import urllib.parse
from http.client import HTTPConnection
from http.server import BaseHTTPRequestHandler, HTTPServer


MAX_BODY_BYTES = 1024 * 1024
LOOPBACK_HOSTS = {"127.0.0.1", "localhost", "::1"}
SEND_PATH = "/api/v1/bot/send_message"


def loopback_target(base_url):
    target = urllib.parse.urlsplit(base_url)
    if (
        target.scheme != "http"
        or target.hostname not in LOOPBACK_HOSTS
        or target.username
        or target.password
        or target.query
        or target.fragment
        or target.path not in ("", "/")
    ):
        raise ValueError("Bot fixture requires a plain loopback HTTP base URL")
    return target.hostname, target.port or 80


def signature_for(timestamp, body, secret):
    base = timestamp.encode("ascii") + b"\n" + body
    digest = hmac.new(secret.encode("utf-8"), base, hashlib.sha256).hexdigest()
    return "sha256=" + digest


def verify_request(headers, body, secret, now=None):
    timestamp = headers.get("x-imboy-timestamp", "")
    delivery_id = headers.get("x-imboy-delivery", "")
    event = headers.get("x-imboy-event", "")
    signature = headers.get("x-imboy-signature", "")
    current = int(time.time()) if now is None else int(now)
    try:
        age = abs(current - int(timestamp))
    except ValueError:
        age = 10**9
    expected = signature_for(timestamp, body, secret) if timestamp else ""
    signed = bool(signature) and hmac.compare_digest(expected, signature)
    envelope = None
    if signed and age <= 300:
        try:
            envelope = json.loads(body)
        except (UnicodeDecodeError, ValueError):
            envelope = None
    body_delivery = envelope.get("delivery_id") if isinstance(envelope, dict) else None
    body_event = envelope.get("event") if isinstance(envelope, dict) else None
    checks = [
        ("timestamp within 300 seconds", age <= 300),
        ("signature matches raw body", signed),
        ("delivery header matches body", bool(delivery_id) and delivery_id == body_delivery),
        ("event header matches body", bool(event) and event == body_event),
    ]
    return checks, envelope


def forge_with_bot_secret(token, group_id, bot_secret):
    payload_b64, _signature_b64 = token.split(".", 1)
    payload = json.loads(base64.b64decode(payload_b64, validate=True))
    payload["group_id"] = group_id
    payload["exp"] = int(time.time()) + 60
    raw = json.dumps(payload, separators=(",", ":")).encode("utf-8")
    signature = hmac.new(bot_secret.encode("utf-8"), raw, hashlib.sha256).digest()
    return (
        base64.b64encode(raw).decode("ascii")
        + "."
        + base64.b64encode(signature).decode("ascii")
    )


def post_bot(base_url, api_token, reply_context, text, group_override=None):
    host, port = loopback_target(base_url)
    body = {"reply_context": reply_context, "text": text}
    if group_override is not None:
        body["group_id"] = group_override
    raw = json.dumps(body, separators=(",", ":")).encode("utf-8")
    connection = HTTPConnection(host, port, timeout=15)
    try:
        connection.request(
            "POST",
            SEND_PATH,
            body=raw,
            headers={
                "Accept": "application/json",
                "Authorization": "Bearer " + api_token,
                "Content-Type": "application/json",
            },
        )
        response = connection.getresponse()
        response_raw = response.read()
        try:
            parsed = json.loads(response_raw) if response_raw else None
        except ValueError:
            parsed = None
        return response.status, parsed
    finally:
        connection.close()


def api_success(response):
    status, body = response
    return status == 200 and isinstance(body, dict) and body.get("code") == 0


def api_rejected(response):
    status, body = response
    return status == 200 and isinstance(body, dict) and body.get("code") != 0


class BotFixtureHandler(BaseHTTPRequestHandler):
    def log_message(self, *_args):
        pass

    def _reply(self, status):
        self.send_response(status)
        self.send_header("Content-Length", "0")
        self.send_header("Connection", "close")
        self.end_headers()

    def do_POST(self):
        server = self.server
        try:
            length = int(self.headers.get("Content-Length", "0"))
            if length <= 0 or length > MAX_BODY_BYTES:
                raise ValueError("invalid body length")
            raw = self.rfile.read(length)
            headers = {key.lower(): value for key, value in self.headers.items()}
            checks, envelope = verify_request(headers, raw, server.verify_token)
            if not all(ok for _, ok in checks) or not isinstance(envelope, dict):
                write_json(
                    server.out_path,
                    {
                        "task": "E2E-01-A02-bot-dialog",
                        "passed": sum(1 for _, ok in checks if ok),
                        "failed": sum(1 for _, ok in checks if not ok),
                        "checks": [
                            {"label": label, "ok": ok} for label, ok in checks
                        ],
                    },
                )
                server.done = True
                self._reply(401)
                return
            data = envelope.get("data") or {}
            reply_context = data.get("reply_context", "")
            checks.extend(
                [
                    ("expected group is bound", data.get("group_id") == str(server.group_id)),
                    ("expected trigger is bound", data.get("trigger_msg_id") == server.trigger_msg_id),
                    ("expected sender is bound", data.get("from_uid") == str(server.from_uid)),
                    ("reply context is present", bool(reply_context)),
                ]
            )
            forged = forge_with_bot_secret(
                reply_context, server.group_id + 1, server.verify_token
            )
            forged_response = post_bot(
                server.backend_url, server.api_token, forged, "forged reply"
            )
            malformed_response = post_bot(
                server.backend_url, server.api_token, "not-a-reply-context", "malformed reply"
            )
            valid_response = post_bot(
                server.backend_url,
                server.api_token,
                reply_context,
                server.reply_text,
                group_override=server.group_id + 1,
            )
            replay_response = post_bot(
                server.backend_url, server.api_token, reply_context, "replayed reply"
            )
            checks.extend(
                [
                    ("Bot verify secret cannot forge context", api_rejected(forged_response)),
                    ("malformed context is rejected", api_rejected(malformed_response)),
                    ("valid context replies through HTTP API", api_success(valid_response)),
                    ("consumed context replay is rejected", api_rejected(replay_response)),
                ]
            )
            valid_payload = (valid_response[1] or {}).get("payload") or {}
            result = {
                "task": "E2E-01-A02-bot-dialog",
                "passed": sum(1 for _, ok in checks if ok),
                "failed": sum(1 for _, ok in checks if not ok),
                "checks": [{"label": label, "ok": ok} for label, ok in checks],
                "delivery_id": envelope.get("delivery_id"),
                "event": envelope.get("event"),
                "group_id": data.get("group_id"),
                "bot_id": envelope.get("bot_id"),
                "reply_msg_id": valid_payload.get("msg_id"),
                "responses": {
                    "forged": response_summary(forged_response),
                    "malformed": response_summary(malformed_response),
                    "valid": response_summary(valid_response),
                    "replay": response_summary(replay_response),
                },
            }
            write_json(server.out_path, result)
            server.done = True
            self._reply(200 if result["failed"] == 0 else 500)
        except (OSError, ValueError, KeyError, binascii.Error) as error:
            write_json(
                server.out_path,
                {
                    "task": "E2E-01-A02-bot-dialog",
                    "passed": 0,
                    "failed": 1,
                    "checks": [{"label": str(error), "ok": False}],
                },
            )
            server.done = True
            self._reply(500)


def response_summary(response):
    status, body = response
    payload = body.get("payload") if isinstance(body, dict) else None
    return {
        "status": status,
        "code": body.get("code") if isinstance(body, dict) else None,
        "has_msg_id": isinstance(payload, dict) and bool(payload.get("msg_id")),
    }


def write_json(path, value):
    with open(path, "w", encoding="utf-8") as handle:
        json.dump(value, handle, ensure_ascii=True, indent=2)
        handle.write("\n")


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--port", type=int, required=True)
    parser.add_argument("--ready", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--expected-group-id", type=int, required=True)
    parser.add_argument("--expected-trigger-msg-id", required=True)
    parser.add_argument("--expected-from-uid", type=int, required=True)
    parser.add_argument("--reply-text", required=True)
    args = parser.parse_args(argv)
    backend_url = os.environ.get("IMBOY_BASE_URL", "")
    api_token = os.environ.get("BOT_API_TOKEN", "")
    verify_token = os.environ.get("BOT_VERIFY_TOKEN", "")
    if not backend_url or not api_token or not verify_token:
        print("missing Bot fixture environment", file=sys.stderr)
        return 2
    try:
        loopback_target(backend_url)
        server = HTTPServer(("127.0.0.1", args.port), BotFixtureHandler)
    except (OSError, ValueError) as error:
        print(f"Bot fixture start failed: {error}", file=sys.stderr)
        return 2
    server.backend_url = backend_url
    server.api_token = api_token
    server.verify_token = verify_token
    server.group_id = args.expected_group_id
    server.trigger_msg_id = args.expected_trigger_msg_id
    server.from_uid = args.expected_from_uid
    server.reply_text = args.reply_text
    server.out_path = args.out
    server.done = False
    write_json(args.ready, {"host": "127.0.0.1", "port": server.server_port})
    server.timeout = 1
    while not server.done:
        server.handle_request()
    server.server_close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
