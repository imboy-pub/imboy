#!/usr/bin/env python3
"""Q08 / AC21-W14 — 本地 Webhook 接收端替身【仅测试用途】。

独立于仓内 Erlang 设施的第二个接收端（Python），供 phase1_delivery 隔离节点
的 bot_delivery 投递链验证使用：

  POST /hook   → 按 RUN_DIR/receiver_mode 文件返回：
                   retry → 第 1 次 500，其后 200（W1 退避重试）
                   dead  → 恒 410（W2 一次尝试即死信）
                   hang  → 睡 1.6s 后 200（W3 发送端 1s 超时 {recv,timeout}）
                 每次到达独立复算 HMAC-SHA256 签名（secret 从
                 RUN_DIR/receiver_secret 读取，支持 INT-12 rotate 轮换），
                 结果连同原始头写入 RUN_DIR/webhook_receptions.jsonl；
  GET  /evidence → 返回全部到达记录 JSON（供脚本断言）；
  GET  /healthz  → 200。

安全边界：只绑 127.0.0.1；secret 只在内存/指定 RUN_DIR，绝不打印。

用法: webhook_receiver_stub.py <port> <run_dir>
"""
import hashlib
import hmac
import json
import os
import sys
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

RUN_DIR = "."


def read_secret():
    try:
        with open(os.path.join(RUN_DIR, "receiver_secret")) as f:
            return f.read().strip()
    except OSError:
        return None


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt, *args):
        pass

    def _reply(self, code, body=b"", ctype="text/plain"):
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path == "/healthz":
            self._reply(200, b"ok")
            return
        if self.path == "/evidence":
            path = os.path.join(RUN_DIR, "webhook_receptions.jsonl")
            rows = []
            if os.path.exists(path):
                with open(path) as f:
                    rows = [json.loads(x) for x in f if x.strip()]
            self._reply(200, json.dumps(rows).encode(), "application/json")
            return
        self._reply(404, b"not found")

    def do_POST(self):
        if self.path != "/hook":
            self._reply(404, b"not found")
            return
        n = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(n)
        ts = self.headers.get("x-imboy-timestamp", "")
        sig = self.headers.get("x-imboy-signature", "")
        secret = read_secret() or ""
        expected = "sha256=" + hmac.new(
            secret.encode(), (ts + "." + body.decode("utf-8", "replace")).encode(),
            hashlib.sha256).hexdigest()
        row = {
            "ts_arrival": time.time(),
            "event": self.headers.get("x-imboy-event", ""),
            "delivery": self.headers.get("x-imboy-delivery", ""),
            "hmac_ok": hmac.compare_digest(sig, expected),
            "signature": sig,
            "expected": expected,
            "body_len": len(body),
            "body": body.decode("utf-8", "replace"),
        }
        with open(os.path.join(RUN_DIR, "webhook_receptions.jsonl"), "a") as f:
            f.write(json.dumps(row, ensure_ascii=False) + "\n")
        mode_path = os.path.join(RUN_DIR, "receiver_mode")
        mode = "retry"
        if os.path.exists(mode_path):
            with open(mode_path) as f:
                mode = f.read().strip()
        if mode == "dead":
            self._reply(410)
            return
        if mode == "hang":
            time.sleep(1.6)
            self._reply(200)
            return
        # retry：第 1 次（对该 delivery）500，其后 200
        path = os.path.join(RUN_DIR, "webhook_receptions.jsonl")
        seen = 0
        with open(path) as f:
            seen = sum(
                1 for line in f
                if line.strip() and json.loads(line).get("delivery") == row["delivery"]
            )
        self._reply(500 if seen == 1 else 200)


def main():
    global RUN_DIR
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(2)
    port, RUN_DIR = int(sys.argv[1]), sys.argv[2]
    httpd = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    print(f"RECEIVER-READY http://127.0.0.1:{port} run_dir={RUN_DIR}", flush=True)
    httpd.serve_forever()


if __name__ == "__main__":
    main()
