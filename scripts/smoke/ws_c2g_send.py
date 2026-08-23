#!/usr/bin/env python3
"""ws_c2g_send.py — WebSocket C2G（群消息）发送器。

golden_upgrade.sh 数据生成段专用（P2-U1）：向 imboy WS 发一帧 C2G 文本消息。
发送方必须是群成员（msg_c2g_logic:c2g 的 is_member 检查），帧契约来自
src/api/websocket_handler.erl handle_json_message → message_router_logic →
msg_c2g_logic:c2g（id/type/to/msg_type 必填；do_send_c2g 另读 created_at）。

环境变量（必填）：
  WS_URL     WS 基址，如 ws://127.0.0.1:9800/api/v1/ws
  WS_TOKEN   发送者 JWT（golden_upgrade.sh 用账号 A 的登录 token）
  WS_GID     目标群 ID（TSID 字符串）
  WS_MSG_ID  客户端消息 ID（golden_upgrade.sh 用 golden-up-c2g-<ts>）
  WS_TEXT    消息文本
可选：
  WS_WAIT_SEC  发送后收帧观察窗口，默认 3 秒

退出码：0 = 已发送且窗口内无 C2G_ERROR 帧（消息是否归档由调用方轮询
        /api/v1/msg/history 断言）；1 = 连接失败 / 收到 C2G_ERROR（禁言、
        非成员、限流等，帧内含 code 与 error 说明）。
"""
import asyncio
import json
import os
import sys
import time

try:
    import websockets
except ImportError:
    print("ERROR: python3 websockets package missing. "
          "Install: pip3 install websockets", file=sys.stderr)
    sys.exit(127)

WS_URL = os.environ["WS_URL"]
TOKEN = os.environ["WS_TOKEN"]
GID = os.environ["WS_GID"]
MSG_ID = os.environ["WS_MSG_ID"]
TEXT = os.environ["WS_TEXT"]
WAIT = float(os.environ.get("WS_WAIT_SEC", "3"))


def headers_for(token: str):
    # 与 scripts/smoke/c2c_ws_smoke.py 同款连接头（authorization/did/cos/vsn）
    return [
        ("authorization", f"Bearer {token}"),
        ("did", "golden-upgrade"),
        ("cos", "mac"),
        ("vsn", "1.0.0"),
    ]


async def run() -> int:
    try:
        ws = await asyncio.wait_for(
            websockets.connect(
                WS_URL,
                additional_headers=headers_for(TOKEN),
                subprotocols=["imboy-json", "text"],
            ),
            timeout=5,
        )
    except Exception as e:  # noqa: BLE001 - 连接失败统一转 rc=1 与 stderr
        print(f"ERROR: WS connect failed: {e}", file=sys.stderr)
        return 1

    try:
        frame = {
            "id": MSG_ID,
            "type": "C2G",
            "to": GID,
            "msg_type": "text",
            "created_at": int(time.time() * 1000),
            "payload": {"msg_type": "text", "text": TEXT},
        }
        await ws.send(json.dumps(frame))
        print(f"[ws->] {json.dumps(frame, ensure_ascii=False)}", flush=True)

        deadline = time.monotonic() + WAIT
        while time.monotonic() < deadline:
            try:
                raw = await asyncio.wait_for(
                    ws.recv(), timeout=max(0.1, deadline - time.monotonic())
                )
            except asyncio.TimeoutError:
                break
            print(f"[ws<-] {raw}", flush=True)
            if "C2G_ERROR" in str(raw):
                return 1
        return 0
    finally:
        try:
            await ws.close()
        except Exception:  # noqa: BLE001 - 关闭失败不影响退出码
            pass


if __name__ == "__main__":
    sys.exit(asyncio.run(run()))
