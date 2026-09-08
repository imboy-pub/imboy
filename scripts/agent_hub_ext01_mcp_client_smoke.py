#!/usr/bin/env python3
"""EXT-01-A02 — 真实 MCP 客户端 smoke：connect → grant → task → revoke。

以独立 HTTP 客户端（非测试 fixture）走通 MCP 治理全链：
  admin 创建 client（secret 一次返回）→ approve → 显式授权 tool →
  MCP initialize/tools-list → tools/call 建任务/读任务 →
  负例（错误凭证 fail-closed）→ revoke → 负例（撤销后凭证失效）。

用法（本地 loopback 默认）:
  ADM_UID=1 ADM_SIG=<签名cookie> python3 scripts/agent_hub_ext01_mcp_client_smoke.py

非 loopback 目标（隔离环境）必须显式授权:
  EXT01_AUTHORIZED=yes IMBOY_BASE_URL=https://<隔离环境> ADM_UID=.. ADM_SIG=.. \
    python3 scripts/agent_hub_ext01_mcp_client_smoke.py --out /tmp/ext01-a02.json

凭证获取（ADM_UID/ADM_SIG cookie 对）见
docs/runbooks/agent-hub-ext01-external-verification.md §0。
secret/cookie 只经环境变量传入，脚本不落盘、输出脱敏。
"""

import argparse
import hashlib
import json
import os
import sys
import threading
import time
import urllib.parse
from http.client import HTTPConnection, HTTPSConnection
from http.server import BaseHTTPRequestHandler, HTTPServer

LOOPBACK_HOSTS = {"127.0.0.1", "localhost", "::1"}
MCP_PATH = "/api/v1/mcp"


def die(msg, code=2):
    print(f"[ext01-a02] FAIL: {msg}", file=sys.stderr)
    sys.exit(code)


class Api:
    def __init__(self, base, adm_uid, adm_sig, timeout=15):
        u = urllib.parse.urlparse(base)
        self.host, self.port = u.hostname, u.port or (443 if u.scheme == "https" else 80)
        self.tls = u.scheme == "https"
        self.adm_cookie = f"adm_user_id={adm_uid}; adm_user_sig={adm_sig}"
        self.timeout = timeout

    def _conn(self):
        cls = HTTPSConnection if self.tls else HTTPConnection
        return cls(self.host, self.port, timeout=self.timeout)

    def request(self, method, path, body=None, headers=None):
        h = {"Accept": "application/json"}
        if headers:
            h.update(headers)
        data = None
        if body is not None:
            data = json.dumps(body).encode()
            h["Content-Type"] = "application/json"
        conn = self._conn()
        try:
            conn.request(method, path, body=data, headers=h)
            resp = conn.getresponse()
            raw = resp.read()
            try:
                parsed = json.loads(raw) if raw else None
            except ValueError:
                parsed = None
            return resp.status, dict(resp.getheaders()), parsed
        finally:
            conn.close()

    def adm(self, method, path, body=None):
        return self.request(method, path, body, {"Cookie": self.adm_cookie})

    def mcp(self, payload, secret, session=None):
        headers = {"Authorization": f"Bearer {secret}"}
        if session:
            headers["mcp-session-id"] = session
        return self.request("POST", MCP_PATH, payload, headers)


def jsonrpc(mid, method, params=None):
    p = {"jsonrpc": "2.0", "id": mid, "method": method}
    if params is not None:
        p["params"] = params
    return p


def rpc_result(status, body, mid):
    if status != 200 or not isinstance(body, dict):
        die(f"JSON-RPC HTTP {status}: {str(body)[:200]}")
    if "error" in body:
        die(f"JSON-RPC error: {body['error']}")
    return body.get("result")


def guard(base_url):
    """非 loopback 目标必须 EXT01_AUTHORIZED=yes（外部动作显式授权门）。"""
    host = urllib.parse.urlparse(base_url).hostname or ""
    if host in LOOPBACK_HOSTS:
        return
    if os.environ.get("EXT01_AUTHORIZED", "") != "yes":
        die(
            f"目标 {base_url} 非 loopback。执行外部/隔离环境动作需显式授权：\n"
            "  export EXT01_AUTHORIZED=yes  # 确认已获用户授权设备/账号/外部动作\n"
            "并确保目标为授权的隔离环境（禁止生产）。",
            code=3,
        )


def expect(cond, label, results):
    results.append((label, bool(cond)))
    mark = "PASS" if cond else "FAIL"
    print(f"[ext01-a02] {mark}: {label}")
    return cond


def mask(secret):
    return (secret[:6] + "…") if len(secret) > 6 else "…"


# ---- --self-test：loopback stub 按真实契约回放，仅验证脚本客户端逻辑。 ----
# 这是工具自检，不是产品证据；EXT-01 证据必须来自授权隔离环境的真实后端。
SECRET = "ext01stub-secret-0123456789abcdef"  # gitleaks:allow self-test stub fake


class Stub(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def _json(self, obj, status=200, headers=None):
        raw = json.dumps(obj).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        for k, v in (headers or {}).items():
            self.send_header(k, v)
        self.send_header("Content-Length", str(len(raw)))
        self.end_headers()
        self.wfile.write(raw)

    def do_GET(self):
        if self.path.startswith("/api/adm/mcp/clients/grants"):
            self._json({"code": 0, "payload": {"tools": [{"name": STUB_TOOL, "enabled": True}]}})
        else:
            self._json({"code": 1}, 404)

    def do_POST(self):
        n = int(self.headers.get("Content-Length") or 0)
        try:
            body = json.loads(self.rfile.read(n) or b"{}")
        except ValueError:
            body = {}

        def adm_ok():
            self._json({"code": 0, "payload": {}})

        if self.path == "/api/adm/mcp/clients/create":
            self._json({"code": 0, "payload": {
                "client_id": 42, "client_key": "mck-stub", "secret": SECRET}})
        elif self.path.startswith("/api/adm/mcp/"):
            if self.path.endswith("/revoke"):
                global STUB_REVOKED
                STUB_REVOKED = True
            adm_ok()
        elif self.path == MCP_PATH:
            if STUB_REVOKED or self.headers.get("Authorization") != f"Bearer {SECRET}":
                self._json({"error": "credential_invalid"}, 401)
                return
            method = body.get("method")
            mid = body.get("id")
            if method == "initialize":
                self._json({"jsonrpc": "2.0", "id": mid, "result": {"serverInfo": {"name": "stub"}}},
                           headers={"mcp-session-id": "stub-session"})
            elif method == "tools/list":
                self._json({"jsonrpc": "2.0", "id": mid,
                            "result": {"tools": [{"name": STUB_TOOL}]}})
            elif method == "tools/call":
                args = (body.get("params") or {}).get("arguments") or {}
                if (body.get("params") or {}).get("name") == "create_agent_task":
                    self._json({"jsonrpc": "2.0", "id": mid, "result": {"structuredContent": {
                        "task_id": "1880000000000000001", "status": "submitted",
                        "correlation_id": "c-1", "created": True}}})
                else:
                    self._json({"jsonrpc": "2.0", "id": mid, "result": {"structuredContent": {
                        "task_id": args.get("task_id", ""), "status": "submitted"}}})
            else:
                self._json({"jsonrpc": "2.0", "id": mid,
                            "error": {"code": -32601, "message": "no method"}}, 400)
        else:
            self._json({"code": 1}, 404)


STUB_TOOL = "create_agent_task"
STUB_REVOKED = False


def self_test():
    srv = HTTPServer(("127.0.0.1", 0), Stub)
    port = srv.server_address[1]
    t = threading.Thread(target=srv.serve_forever, daemon=True)
    t.start()
    os.environ["IMBOY_BASE_URL"] = f"http://127.0.0.1:{port}"
    os.environ["ADM_UID"] = "1"
    os.environ["ADM_SIG"] = "stub-signature"
    print(f"[ext01-a02] SELF-TEST against loopback stub :{port}")
    try:
        import argparse as _ap
        flow(_ap.Namespace(out=""), os.environ["IMBOY_BASE_URL"], "1", "stub-signature",
             1, STUB_TOOL, 0)
    finally:
        srv.shutdown()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="", help="结果 JSON 写入路径")
    ap.add_argument("--self-test", action="store_true",
                    help="对内嵌 loopback stub 自检客户端逻辑（非产品证据）")
    args = ap.parse_args()

    base = os.environ.get("IMBOY_BASE_URL", "http://127.0.0.1:9800")
    adm_uid = os.environ.get("ADM_UID", "")
    # 签名是 32 字节原始 HMAC；env 传不进任意字节，优先 ADM_SIG_HEX（hex 编码）。
    if os.environ.get("ADM_SIG_HEX"):
        adm_sig = bytes.fromhex(os.environ["ADM_SIG_HEX"]).decode("latin-1")
    else:
        adm_sig = os.environ.get("ADM_SIG", "")
    owner_uid = int(os.environ.get("EXT01_OWNER_UID", "1"))
    tool = os.environ.get("EXT01_TOOL", "create_agent_task")
    group_id = int(os.environ.get("EXT01_GROUP_ID", "0"))

    if args.self_test:
        self_test()
        return

    guard(base)
    if not adm_uid or not adm_sig:
        die("缺 ADM_UID / ADM_SIG（admin 签名 cookie 对），获取方式见 EXT-01 runbook §0")
    flow(args, base, adm_uid, adm_sig, owner_uid, tool, group_id)


def flow(args, base, adm_uid, adm_sig, owner_uid, tool, group_id):
    api = Api(base, adm_uid, adm_sig)
    results = []
    run_tag = f"ext01-a02-{int(time.time())}"

    # ---- admin 治理链：create → approve → grant → grants 复核 ----
    st, _, body = api.adm("POST", "/api/adm/mcp/clients/create", {
        "owner_uid": owner_uid, "name": run_tag, "description": "EXT-01 A02 smoke"})
    payload = (body or {}).get("payload") or (body or {}).get("data") or {}
    client_id = payload.get("client_id") or payload.get("id")
    secret = payload.get("secret") or ""
    client_key = payload.get("client_key") or ""
    if not expect(st == 200 and client_id and secret,
                  f"create client (client_key={client_key}, secret={mask(secret)})",
                  results):
        die("create 失败，中止（无残留可达凭证）")
    print(f"[ext01-a02] client_id={client_id}")

    st, _, _ = api.adm("POST", "/api/adm/mcp/clients/approve", {"client_id": client_id})
    expect(st == 200, "approve", results)

    st, _, _ = api.adm("POST", "/api/adm/mcp/clients/grants/set",
                       {"client_id": client_id, "tool": tool, "enabled": True})
    expect(st == 200, f"grant {tool}=enabled", results)

    st, _, body = api.adm("GET", f"/api/adm/mcp/clients/grants?client_id={client_id}")
    grants = ((body or {}).get("payload") or {}).get("tools") or []
    enabled = [g.get("name") for g in grants if g.get("enabled")]
    expect(tool in enabled, f"grants 复核 enabled={enabled}", results)

    # ---- MCP 连接与调用 ----
    st, hdr, body = api.mcp(jsonrpc(1, "initialize", {
        "protocolVersion": "2024-11-05",
        "capabilities": {},
        "clientInfo": {"name": "ext01-smoke", "version": "1.0"}}), secret)
    session = hdr.get("mcp-session-id") or hdr.get("Mcp-Session-Id")
    rpc_result(st, body, 1)
    expect(bool(session), f"initialize（session={bool(session)}）", results)

    st, _, body = api.mcp(jsonrpc(2, "tools/list"), secret, session)
    names = [t.get("name") for t in (rpc_result(st, body, 2).get("tools") or [])]
    expect(tool in names, f"tools/list 含 {tool}", results)

    idem = run_tag
    st, _, body = api.mcp(jsonrpc(3, "tools/call", {
        "name": "create_agent_task",
        "arguments": {"group_id": group_id, "idempotency_key": idem,
                      "tool": "ext01_smoke",
                      "params_digest": hashlib.sha256(idem.encode()).hexdigest()},
    }), secret, session)
    r3 = rpc_result(st, body, 3) or {}
    content = r3.get("structuredContent") or r3.get("structured_content") or {}
    task_id = content.get("task_id")
    expect(bool(task_id), f"tools/call create_agent_task（task_id={task_id}）", results)

    if task_id:
        st, _, body = api.mcp(jsonrpc(4, "tools/call", {
            "name": "get_agent_task", "arguments": {"task_id": str(task_id)},
        }), secret, session)
        r4 = rpc_result(st, body, 4) or {}
        sc = r4.get("structuredContent") or r4.get("structured_content") or {}
        expect(sc.get("status") in ("submitted", "working", "awaiting_approval"),
               f"get_agent_task status={sc.get('status')}", results)

    # ---- 负例：错误凭证 fail-closed（handler 401 + {"error":"credential_invalid"}）----
    st, _, body = api.mcp(jsonrpc(5, "tools/list"), "wrong-secret-000")
    expect(st == 401, f"负例：错误凭证 401 fail-closed（HTTP {st}）", results)

    # ---- revoke 后凭证立即失效 ----
    st, _, _ = api.adm("POST", "/api/adm/mcp/clients/revoke",
                       {"client_id": client_id, "reason": "ext01 smoke done"})
    expect(st == 200, "revoke", results)

    st, _, body = api.mcp(jsonrpc(6, "tools/list"), secret, session)
    expect(st in (401, 403), f"负例：撤销后凭证失效（HTTP {st}）", results)

    failed = [label for label, ok in results if not ok]
    summary = {
        "task": "EXT-01-A02", "base_url": base, "run_tag": run_tag,
        "client_key": client_key, "task_id": str(task_id or ""),
        "passed": sum(1 for _, ok in results if ok),
        "failed": len(failed), "checks": [{"label": l, "ok": o} for l, o in results],
    }
    print(f"[ext01-a02] ===== {summary['passed']} PASS / {summary['failed']} FAIL =====")
    if args.out:
        with open(args.out, "w") as f:
            json.dump(summary, f, ensure_ascii=False, indent=2)
        print(f"[ext01-a02] 结果已写入 {args.out}")
    sys.exit(0 if not failed else 1)


if __name__ == "__main__":
    main()
