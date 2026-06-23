#!/usr/bin/env python3
"""
c2c_ws_smoke.py — Tier-0 WebSocket round-trip 冒烟

流程：
  1. Bob (env BOB_UID / BOB_TOKEN) 连 ws://127.0.0.1:9800/api/ws 订阅
  2. 启动后作为子进程调用 imboy_ctl msg send，从 Alice (BOB 外的 FROM uid) 向 Bob 发 1 条 C2C
  3. 在 TIMEOUT 秒内等 Bob WS 收到含 MSG_ID 的文本帧
  4. 退出码：
     0 PASS；非 0 表示 FAIL（详见 stderr）

环境变量（必填）：
  BOB_TOKEN      Bob 的 JWT
  BOB_UID        Bob 的 uid（目标）
  FROM_UID       Alice 的 uid（发送方）
  ESCRIPT_PATH   imboy_ctl 绝对路径

可选环境变量：
  WS_URL         默认 ws://127.0.0.1:9800/api/ws
  TIMEOUT_SEC    默认 5
  DID            默认 smoke-bob-ws
"""
import asyncio
import json
import os
import subprocess
import sys
import time
from typing import Optional

try:
    import websockets
except ImportError:
    print("ERROR: python3 websockets package missing. "
          "Install: pip3 install websockets", file=sys.stderr)
    sys.exit(127)


WS_URL = os.environ.get("WS_URL", "ws://127.0.0.1:9800/api/ws")
TIMEOUT_SEC = float(os.environ.get("TIMEOUT_SEC", "5"))
DID = os.environ.get("DID", "smoke-bob-ws")

BOB_TOKEN = os.environ.get("BOB_TOKEN", "")
BOB_UID = os.environ.get("BOB_UID", "")
FROM_UID = os.environ.get("FROM_UID", "")
ESCRIPT_PATH = os.environ.get("ESCRIPT_PATH", "")


def require(name: str, value: str) -> str:
    if not value:
        print(f"ERROR: env {name} is required", file=sys.stderr)
        sys.exit(2)
    return value


def headers_for(token: str, did: str):
    return [
        ("authorization", f"Bearer {token}"),
        ("did", did),
        ("cos", "mac"),
        ("vsn", "1.0.0"),
    ]


def send_via_escript(from_uid: str, to_uid: str) -> Optional[str]:
    """Spawn imboy_ctl msg send to send one C2C. Return MSG_ID or None."""
    cmd = [ESCRIPT_PATH, "msg", "send", from_uid, to_uid]
    try:
        res = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=10,
        )
    except subprocess.TimeoutExpired:
        print("ERROR: imboy_ctl msg send timed out", file=sys.stderr)
        return None
    if res.returncode != 0:
        print(f"ERROR: escript exit={res.returncode} stderr={res.stderr.strip()}",
              file=sys.stderr)
        return None
    for line in res.stdout.splitlines():
        line = line.strip()
        if line.startswith("MSG_ID="):
            return line[len("MSG_ID="):]
    print(f"ERROR: escript output missing MSG_ID: {res.stdout!r}", file=sys.stderr)
    return None


def frame_contains_msg_id(raw: str, msg_id: str) -> bool:
    """True iff the WS text frame contains msg_id.

    First try JSON-aware match on id/msg_id fields (robust against substring
    collisions); fall back to plain substring scan if not JSON.
    """
    try:
        parsed = json.loads(raw)
    except (ValueError, TypeError):
        return msg_id in raw
    if isinstance(parsed, dict):
        for key in ("id", "msg_id", "message_id"):
            if str(parsed.get(key, "")) == msg_id:
                return True
        # Nested payload/body?
        for key in ("payload", "body", "data"):
            inner = parsed.get(key)
            if isinstance(inner, dict):
                for k in ("id", "msg_id", "message_id"):
                    if str(inner.get(k, "")) == msg_id:
                        return True
    return msg_id in raw


async def run() -> int:
    require("BOB_TOKEN", BOB_TOKEN)
    require("BOB_UID", BOB_UID)
    require("FROM_UID", FROM_UID)
    require("ESCRIPT_PATH", ESCRIPT_PATH)

    loop = asyncio.get_event_loop()

    try:
        ws = await asyncio.wait_for(
            websockets.connect(
                WS_URL,
                additional_headers=headers_for(BOB_TOKEN, DID),
                subprotocols=["imboy-json", "text"],
            ),
            timeout=5,
        )
    except Exception as e:
        print(f"ERROR: Bob WS connect failed: {e}", file=sys.stderr)
        return 3

    print(f"[bob] connected subprotocol={ws.subprotocol}")

    # Step 2: trigger C2C send in a thread so WS recv can run concurrently
    msg_id_holder: list = []

    def do_send():
        mid = send_via_escript(FROM_UID, BOB_UID)
        msg_id_holder.append(mid)

    send_task = loop.run_in_executor(None, do_send)

    # Give escript a moment to start before we begin waiting
    await asyncio.sleep(0.2)

    deadline = time.monotonic() + TIMEOUT_SEC
    matched = False
    msg_id: Optional[str] = None

    try:
        while time.monotonic() < deadline:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                break
            try:
                raw = await asyncio.wait_for(ws.recv(), timeout=min(remaining, 0.5))
            except asyncio.TimeoutError:
                # Poll whether escript finished producing MSG_ID yet
                if send_task.done() and msg_id is None:
                    msg_id = msg_id_holder[0] if msg_id_holder else None
                continue

            print(f"[bob<-] {raw}")

            if msg_id is None and send_task.done():
                msg_id = msg_id_holder[0] if msg_id_holder else None

            if msg_id and frame_contains_msg_id(raw, msg_id):
                matched = True
                break

        # Make sure escript completed so we can report MSG_ID in all paths
        await send_task
        if msg_id is None:
            msg_id = msg_id_holder[0] if msg_id_holder else None
    finally:
        try:
            await ws.close()
        except Exception:
            pass

    if msg_id is None:
        print("FAIL: escript did not produce MSG_ID", file=sys.stderr)
        return 4

    print(f"MSG_ID={msg_id}")
    if matched:
        print(f"WS_RECV=ok (msg_id={msg_id})")
        return 0
    print(f"WS_RECV=timeout (msg_id={msg_id}, timeout={TIMEOUT_SEC}s)",
          file=sys.stderr)
    return 5


if __name__ == "__main__":
    sys.exit(asyncio.run(run()))
