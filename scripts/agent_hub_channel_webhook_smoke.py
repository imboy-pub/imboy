#!/usr/bin/env python3
"""Exercise the real channel incoming-webhook HTTP boundary on loopback."""

import argparse
import json
import os
import sys
import urllib.parse
from http.client import HTTPConnection


LOOPBACK_HOSTS = {"127.0.0.1", "localhost", "::1"}


def guard(base_url):
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
        raise ValueError("channel webhook smoke requires a plain loopback HTTP base URL")
    return target.hostname, target.port or 80


def post(base_url, token, text, timeout=10):
    host, port = guard(base_url)
    path = "/api/v1/webhook/channel/" + urllib.parse.quote(token, safe="")
    payload = json.dumps({"text": text}).encode("utf-8")
    connection = HTTPConnection(host, port, timeout=timeout)
    try:
        connection.request(
            "POST",
            path,
            body=payload,
            headers={"Accept": "application/json", "Content-Type": "application/json"},
        )
        response = connection.getresponse()
        raw = response.read()
        try:
            body = json.loads(raw) if raw else None
        except ValueError:
            body = None
        return response.status, body
    finally:
        connection.close()


def run(base_url, active_token, disabled_token, text):
    responses = {
        "active": post(base_url, active_token, text),
        "invalid": post(base_url, "invalid-local-token", text),
        "disabled": post(base_url, disabled_token, text),
    }
    checks = [
        ("active token publishes", responses["active"] == (200, {"ok": True})),
        (
            "invalid token is hidden",
            responses["invalid"] == (404, {"ok": False, "error": "not_found"}),
        ),
        (
            "disabled token is hidden",
            responses["disabled"] == (404, {"ok": False, "error": "not_found"}),
        ),
        (
            "invalid and disabled responses match",
            responses["invalid"] == responses["disabled"],
        ),
    ]
    return {
        "task": "E2E-01-A02-channel-webhook",
        "base_url": base_url,
        "passed": sum(1 for _, ok in checks if ok),
        "failed": sum(1 for _, ok in checks if not ok),
        "checks": [{"label": label, "ok": ok} for label, ok in checks],
        "responses": {
            name: {"status": status, "body": body}
            for name, (status, body) in responses.items()
        },
    }


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--out", required=True)
    args = parser.parse_args(argv)
    base_url = os.environ.get("IMBOY_BASE_URL", "http://127.0.0.1:9800")
    active_token = os.environ.get("CHANNEL_WEBHOOK_TOKEN", "")
    disabled_token = os.environ.get("CHANNEL_WEBHOOK_DISABLED_TOKEN", "")
    text = os.environ.get("CHANNEL_WEBHOOK_TEXT", "")
    if not active_token or not disabled_token or not text:
        print("missing channel webhook smoke fixture input", file=sys.stderr)
        return 2
    try:
        result = run(base_url, active_token, disabled_token, text)
    except (OSError, ValueError) as error:
        print(f"channel webhook smoke failed: {error}", file=sys.stderr)
        return 1
    with open(args.out, "w", encoding="utf-8") as handle:
        json.dump(result, handle, ensure_ascii=True, indent=2)
        handle.write("\n")
    for check in result["checks"]:
        print(f"[channel-webhook] {'PASS' if check['ok'] else 'FAIL'}: {check['label']}")
    print(
        f"[channel-webhook] {result['passed']} PASS / {result['failed']} FAIL"
    )
    return 0 if result["failed"] == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
