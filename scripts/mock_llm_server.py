#!/usr/bin/env python3
"""本地 mock LLM 服务（OpenAI Chat Completions 兼容，固定应答）。

用途：本机没有 ARK/BAILIAN key 时，验证 agent 对话全链（C2C 明文豁免 →
限流 → dispatch → LLM → 回投落库 → WS 推送）。配套 sys.local.config 里的
`mock` provider（base_url=http://127.0.0.1:9911/v1）。

用法: python3 scripts/mock_llm_server.py   # 监听 127.0.0.1:9911
不入仓自动化；仅本地联调（scripts 目录内的开发辅助，同 smoke 系列定位）。
"""
import json
import re
from http.server import BaseHTTPRequestHandler, HTTPServer

REPLY_TMPL = "你好，我是 mock AI 助手。已收到你的消息：「{q}」（共 {n} 字）。"


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get("Content-Length", 0))
        body = self.rfile.read(length) if length else b"{}"
        try:
            data = json.loads(body or b"{}")
        except json.JSONDecodeError:
            data = {}
        # 取最后一条 user 消息文本做回显
        question = ""
        for msg in reversed(data.get("messages", [])):
            if msg.get("role") == "user":
                question = re.sub(r"<.*?>", "", str(msg.get("content", "")))
                break
        answer = REPLY_TMPL.format(q=question[:80], n=len(question))
        resp = {
            "id": "chatcmpl-mock",
            "object": "chat.completion",
            "model": data.get("model", "mock-chat-1"),
            "choices": [
                {
                    "index": 0,
                    "finish_reason": "stop",
                    "message": {"role": "assistant", "content": answer},
                }
            ],
            "usage": {"prompt_tokens": 1, "completion_tokens": 1, "total_tokens": 2},
        }
        payload = json.dumps(resp, ensure_ascii=False).encode("utf-8")
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def log_message(self, fmt, *args):
        print("[mock_llm] " + fmt % args)


if __name__ == "__main__":
    print("[mock_llm] listening on 127.0.0.1:9911")
    HTTPServer(("127.0.0.1", 9911), Handler).serve_forever()
