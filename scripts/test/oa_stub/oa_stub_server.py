#!/usr/bin/env python3
"""Q08 / AC20 — 本地 OA 页面替身 HTTPS 服务【仅测试用途】。

单进程、仅回环（127.0.0.1）、stdlib 实现，配合 scripts/test/oa_stub/oa_stub.html
与 gen_self_signed_cert.sh 使用：

  GET /            → 返回同目录 oa_stub.html（OA 侧工作台替身页）
  GET /cb?code=..&state=.. → SSO redirect_uri 落点：记录 code/state 到 JSONL
                            （回放证据），页面显示“登录跳转完成”；
  GET /evidence    → 返回已记录的落点 JSON（供脚本断言）；
  GET /healthz     → 200（ readiness 探测）。

安全边界：
  - 只绑 127.0.0.1；证书为测试自签（调用方需 -k / verify=False）；
  - 不鉴权、不落敏感明文以外的任何数据（code/nonce 本身就是一次性凭据，
    仅写入调用方指定的 RUN_DIR，用后即弃）；
  - 绝不对外网监听。

用法: oa_stub_server.py <port> <cert.pem> <key.pem> <run_dir>
"""
import json
import os
import ssl
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlparse

HERE = os.path.dirname(os.path.abspath(__file__))
RUN_DIR = "."


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt, *args):  # 静默默认访问日志（防 code 进日志）
        pass

    def _reply(self, code, body, ctype="text/html; charset=utf-8"):
        data = body.encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        u = urlparse(self.path)
        if u.path == "/healthz":
            self._reply(200, "ok", "text/plain")
            return
        if u.path == "/":
            with open(os.path.join(HERE, "oa_stub.html"), "rb") as f:
                data = f.read()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
            return
        if u.path == "/cb":
            q = parse_qs(u.query)
            row = {
                "ts": time.time(),
                "code": q.get("code", [""])[0],
                "state": q.get("state", [""])[0],
            }
            with open(os.path.join(RUN_DIR, "oa_stub_landings.jsonl"), "a") as f:
                f.write(json.dumps(row, ensure_ascii=False) + "\n")
            self._reply(
                200,
                "<h3>OA-SSO landing OK</h3><p>code recorded (test stub). state="
                + row["state"]
                + "</p>",
            )
            return
        if u.path == "/evidence":
            path = os.path.join(RUN_DIR, "oa_stub_landings.jsonl")
            rows = []
            if os.path.exists(path):
                with open(path) as f:
                    rows = [json.loads(line) for line in f if line.strip()]
            self._reply(200, json.dumps(rows, ensure_ascii=False), "application/json")
            return
        self._reply(404, "not found", "text/plain")


def main():
    global RUN_DIR
    if len(sys.argv) != 5:
        print(__doc__)
        sys.exit(2)
    port, cert, key, RUN_DIR = int(sys.argv[1]), sys.argv[2], sys.argv[3], sys.argv[4]
    httpd = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
    ctx.load_cert_chain(cert, key)
    httpd.socket = ctx.wrap_socket(httpd.socket, server_side=True)
    print(f"OA-STUB-READY https://127.0.0.1:{port} run_dir={RUN_DIR}", flush=True)
    httpd.serve_forever()


if __name__ == "__main__":
    main()
