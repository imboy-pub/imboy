#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Q04 / AC07 / AC08 真 WS 双连接端到端验收（BE-WS-E2E，2026-10-07）.

前置：imboy 节点已以 config/sys.p1-boot.config（phase1_delivery profile，
隔离库 imboy_p1_20261007t003716z@127.0.0.1:4323）在 127.0.0.1:9800 启动，
由同目录 run_ws_e2e.sh 负责 boot/stop；本脚本只做 seeding + 协议验证。

拓扑（与 AC07/AC08 oracle 对齐）：
  用户 R（接收者，复用 AC02 测试用户 13900007806）
    ├─ 设备 R1：真 WS 连接（did=p1wse2e-did-r1）
    └─ 设备 R2：真 WS 连接（did=p1wse2e-did-r2）+ HTTP 离线补拉
  用户 S（发送者，13900007807；不存在则经 imboy_ctl user create 建号）
    └─ 设备 S1：真 WS 连接（did=p1wse2e-did-s）
  群 G=9876770：S(owner)+R 两成员（SQL 直插 fixture；role 必须按生产语义
    赋值——owner=4、受邀成员=1，见 group_ds 建群/邀请入群；缺省 0 会被
    msg_store_repo staging 事务内 caller.role >= 1 重验拒绝→forbidden）

四条链（行为合同 control/designs/Q04-contract.md 冻结版 v1）：
  E2E-1 (I1) 送达 ACK 纯标记：R1 CLIENT_ACK 后 client_ack=true，但游标=0、
          未读=3、R2 的 HTTP 补拉仍含全部消息
  E2E-2 (I2) 已读游标 GREATEST 单调：读到 N 后逆序/重复上报不回退，
          S2C message_read_sync 同时下发 R1/R2 双连接；N+1 到达仍计未读
  E2E-3 (I3) R1 ACK + offline_ack 后 R2 离线补拉完整取回（不删行）
  E2E-4 (I4) 未读 = count(seq > 游标)：发 N 读 M 后 SQL 复算（与服务端
          msg_c2g_timeline_repo:count_unread_by_uid 同一 SQL 文本）

依赖：python3 标准库 + websocket-client（本机已装 1.8.0）+ psql +
      escript（imboy_ctl）。所有网络目标仅 127.0.0.1。

用法：
  python3 scripts/test/ws_e2e/ws_e2e_test.py \
      --repo /path/to/imboy --base-url http://127.0.0.1:9800 \
      --evidence <RUN_ROOT>/control/q04-ws-e2e-evidence.log

退出码：0=四链全 PASS；1=有 FAIL；2=环境 BLOCKED（未跑协议链）。
"""

import argparse
import json
import os
import re
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.parse
import urllib.request

try:
    import websocket  # websocket-client
except ImportError:
    print("FATAL: 缺少 websocket-client（pip install websocket-client）", file=sys.stderr)
    sys.exit(2)

# ----------------------------------------------------------------------------
# 常量
# ----------------------------------------------------------------------------

WS_PATH = "/api/v1/ws"
GID = 9876770                       # e2e fixture 群（单测段 9876541，避开）
UID_R_MOBILE = "13900007806"        # 接收者：复用 AC02 测试用户
S_MOBILE = "13900007807"            # 发送者：不存在则 imboy_ctl 建号
S_NICKNAME = "p1wse2e-sender"
S_PASSWORD = "P1pass123456"         # 合成测试值（S3 安全门已判定）：仅本地隔离测试库
                                    # 临时建号用，非任何真实系统凭据
DID_R1 = "p1wse2e-did-r1"
DID_R2 = "p1wse2e-did-r2"
DID_S = "p1wse2e-did-s"
MSG_PREFIX = "p1wse2e"              # 消息 id 前缀（幂等清理锚点）
RECV_TIMEOUT_DEFAULT = 15.0         # 单帧等待上限（秒）

# 未读复算 SQL：与 src/repo/msg_c2g_timeline_repo.erl count_unread_by_uid/2
# 同一 SQL 文本（同源交叉核验，非近似）
SQL_UNREAD = (
    "SELECT count(DISTINCT tl.msg_id)::integer AS count FROM msg_c2g_timeline tl "
    "JOIN public.\"group\" grp ON grp.id = tl.to_gid AND grp.status = 1 "
    "JOIN public.group_member gm ON gm.group_id = tl.to_gid "
    "AND gm.user_id = tl.to_uid AND gm.status = 1 "
    "JOIN public.msg_c2g m ON m.msg_id = tl.msg_id "
    "AND m.created_at = tl.created_at AND m.to_id = tl.to_gid "
    "JOIN public.group_member_generation gmg ON gmg.group_id = tl.to_gid "
    "AND gmg.user_id = tl.to_uid AND gmg.end_seq IS NULL "
    "LEFT JOIN public.workspace_group_read_cursor r ON r.generation_id = gmg.id "
    "WHERE tl.to_uid = {uid} AND tl.to_gid = {gid} "
    "AND tl.conv_seq IS NOT NULL AND tl.conv_seq >= gmg.start_seq "
    "AND tl.conv_seq > COALESCE(r.read_seq, 0) AND m.from_id <> {uid} "
    "AND (m.expire_at IS NULL OR m.expire_at > NOW()) "
    "AND m.payload->>'action' IS DISTINCT FROM 'message_revoke_ack'"
)

# 已读游标：与单测 read_seq_of/2 同源
SQL_CURSOR = (
    "SELECT COALESCE(r.read_seq, 0) AS rs FROM group_member_generation gmg "
    "LEFT JOIN workspace_group_read_cursor r ON r.generation_id = gmg.id "
    "WHERE gmg.group_id = {gid} AND gmg.user_id = {uid} AND gmg.end_seq IS NULL"
)

# per-uid 送达标记：与单测 timeline_client_ack/1 同源
SQL_CLIENT_ACK = (
    "SELECT client_ack FROM msg_c2g_timeline WHERE to_uid = {uid} AND msg_id = '{mid}'"
)

# timeline 行（msg_id -> conv_seq，全量核对用）
SQL_TIMELINE_SEQS = (
    "SELECT msg_id, conv_seq FROM msg_c2g_timeline "
    "WHERE to_uid = {uid} AND to_gid = {gid} AND msg_id LIKE '{pfx}%' "
    "ORDER BY conv_seq"
)


# ----------------------------------------------------------------------------
# 证据落盘
# ----------------------------------------------------------------------------

class Evidence:
    def __init__(self, path):
        self._fh = open(path, "a", encoding="utf-8", buffering=1)
        self.lock = threading.Lock()
        self.t0 = time.time()

    def log(self, kind, conn, payload):
        ts = time.time() - self.t0
        line = json.dumps(
            {"t": round(ts, 3), "kind": kind, "conn": conn, "data": payload},
            ensure_ascii=False, default=str,
        )
        with self.lock:
            self._fh.write(line + "\n")

    def text(self, msg):
        with self.lock:
            self._fh.write(msg + "\n")

    def close(self):
        self._fh.close()


# ----------------------------------------------------------------------------
# PG / imboy_ctl 助手
# ----------------------------------------------------------------------------

class Env:
    def __init__(self, repo, base_url, ev, node):
        self.repo = repo
        self.base = base_url.rstrip("/")
        self.ev = ev
        self.node = node
        self.pg_env = dict(os.environ)
        # 口令从 gitignored 的 config/sys.local.config 的 pg_conf 块运行时解析
        # （绝不写入提交物）；库名从本 run boot 配置（sys.p1-boot.config）取。
        local_cfg = os.path.join(repo, "config", "sys.local.config")
        boot_cfg = os.path.join(repo, "config", "sys.p1-boot.config")
        local_text = open(local_cfg, encoding="utf-8").read()
        boot_text = open(boot_cfg, encoding="utf-8").read()
        mpw = re.search(r"pg_conf.*?password\s*=>\s*\"([^\"]+)\"", local_text, re.S)
        mdb = re.search(r"database\s*=>\s*\"(imboy_p1_[^\"]+)\"", boot_text)
        if not mpw or not mdb:
            raise RuntimeError("无法解析隔离库口令（sys.local.config pg_conf）/库名（sys.p1-boot.config）")
        self.pg_env["PGPASSWORD"] = mpw.group(1)
        self.dbname = mdb.group(1)

    # ---- psql ----
    def sql(self, query):
        cmd = [
            "psql", "-h", "127.0.0.1", "-p", "4323", "-U", "imboy_user",
            "-d", self.dbname, "-tA", "-c", query,
        ]
        p = subprocess.run(cmd, capture_output=True, text=True, env=self.pg_env, timeout=30)
        self.ev.log("sql", "-", {"query": query, "rc": p.returncode,
                                 "out": p.stdout.strip(), "err": p.stderr.strip()[:300]})
        if p.returncode != 0:
            raise RuntimeError(f"SQL 失败: {p.stderr.strip()[:200]}")
        return p.stdout.strip()

    def sql_int(self, query):
        out = self.sql(query)
        return int(out) if out else 0

    # ---- imboy_ctl ----
    def ctl(self, *args):
        cmd = ["escript", "scripts/imboy_ctl"] + list(args)
        p = subprocess.run(cmd, capture_output=True, text=True, cwd=self.repo, timeout=60)
        self.ev.log("ctl", "-", {"args": list(args), "rc": p.returncode,
                                 "out": p.stdout.strip()[:400], "err": p.stderr.strip()[:400]})
        return p.returncode, p.stdout

    # ---- HTTP（Bearer JWT）----
    def http(self, method, path, token, body=None, qs=None):
        url = self.base + path + (qs or "")
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(url, data=data, method=method)
        req.add_header("Authorization", "Bearer " + token)
        if data:
            req.add_header("Content-Type", "application/json")
        try:
            with urllib.request.urlopen(req, timeout=20) as resp:
                raw, rc = resp.read().decode(), resp.getcode()
        except urllib.error.HTTPError as e:
            raw, rc = e.read().decode(), e.code
        try:
            parsed = json.loads(raw)
        except ValueError:
            parsed = {"_raw": raw[:500]}
        self.ev.log("http", path, {"method": method, "status": rc, "resp": parsed})
        return rc, parsed


# ----------------------------------------------------------------------------
# WS 客户端（后台 reader + keepalive）
# ----------------------------------------------------------------------------

class WSClient:
    def __init__(self, ws_base, did, token, name, ev):
        url = "{}{}?{}".format(
            ws_base, WS_PATH,
            urllib.parse.urlencode({"did": did, "cos": "web", "token": token}),
        )
        self.name = name
        self.did = did
        self.ev = ev
        self.ws = websocket.create_connection(
            url, timeout=8,
            # 服务端 websocket_ds:check_subprotocols 要求握手必带子协议
            #（缺失回 400）；imboy-json = 文本 JSON 帧（framing=none）
            subprotocols=["imboy-json"],
        )
        self.frames = []           # [{"t":..., "obj":...|None, "raw":...}]
        self.dead = False
        self._lock = threading.Lock()
        self.ev.log("ws_open", name, {"did": did})
        threading.Thread(target=self._reader, daemon=True).start()
        threading.Thread(target=self._keepalive, daemon=True).start()

    def _reader(self):
        while not self.dead:
            try:
                raw = self.ws.recv()
            except Exception:
                self.dead = True
                return
            if not raw:
                continue
            obj = None
            try:
                obj = json.loads(raw)
            except ValueError:
                pass
            with self._lock:
                self.frames.append({"t": time.time(), "obj": obj, "raw": raw[:2000]})

    def _keepalive(self):
        n = 0
        while not self.dead:
            time.sleep(20)
            if self.dead:
                return
            n += 1
            try:
                self.send_text("ping")
                self.ev.log("ws_ka", self.name, {"seq": n})
            except Exception:
                self.dead = True
                return

    def send_text(self, text):
        with self._lock:
            self.ws.send(text)
        self.ev.log("ws_send", self.name, {"raw": text[:1500]})

    def send_json(self, obj):
        self.send_text(json.dumps(obj, ensure_ascii=False))

    def _match(self, f, pred):
        obj = f.get("obj")
        if obj is None:
            return None
        try:
            # 注意：必须返回命中的帧对象本身而非 pred 真值——
            # 调用方（如 message_read_ack）会用返回值继续取字段
            return obj if pred(obj) else None
        except Exception:
            return None

    def wait_for(self, pred, timeout=RECV_TIMEOUT_DEFAULT, desc="", after=0):
        """等待满足 pred(obj) 的帧；after=起始扫描下标（跳过更早的帧）。"""
        deadline = time.time() + timeout
        seen = after
        while time.time() < deadline:
            if self.dead:
                raise RuntimeError(f"[{self.name}] 连接已断（等待 {desc}）")
            with self._lock:
                frames = self.frames[:]
            for f in frames[seen:]:
                obj = self._match(f, pred)
                if obj is not None:
                    self.ev.log("ws_match", self.name, {"desc": desc, "obj": obj})
                    return obj
            seen = len(frames)
            time.sleep(0.05)
        raise TimeoutError(f"[{self.name}] {timeout}s 内未等到 {desc}")

    def frame_count(self):
        with self._lock:
            return len(self.frames)

    def received_ids(self, msg_ids):
        with self._lock:
            frames = self.frames[:]
        got = set()
        for f in frames:
            o = f.get("obj")
            if isinstance(o, dict) and o.get("type") == "C2G" and o.get("id") in msg_ids:
                got.add(o.get("id"))
        return got

    def close(self):
        self.dead = True
        try:
            self.ws.close()
        except Exception:
            pass


# ----------------------------------------------------------------------------
# 断言与结果收集
# ----------------------------------------------------------------------------

class Chain:
    def __init__(self, chain_id, invariant, ev):
        self.chain_id = chain_id
        self.invariant = invariant
        self.ev = ev
        self.checks = []

    def check(self, name, ok, detail):
        self.checks.append({"name": name, "ok": bool(ok), "detail": detail})
        self.ev.log("check", self.chain_id, {"name": name, "ok": bool(ok), "detail": detail})
        if not ok:
            print(f"  ✗ [{self.chain_id}] {name}: {detail}")
        else:
            print(f"  ✓ [{self.chain_id}] {name}: {detail}")

    @property
    def passed(self):
        return self.checks and all(c["ok"] for c in self.checks)


# ----------------------------------------------------------------------------
# fixture（与单测 setup_fixture 同构，真链路额外走 staging 事务产生的数据）
# ----------------------------------------------------------------------------

def fixture_cleanup(env, uid_r, uid_s):
    env.sql(f"DELETE FROM msg_c2g_timeline WHERE to_gid = {GID}")
    env.sql(f"DELETE FROM msg_c2g WHERE to_id = {GID} AND msg_id LIKE '{MSG_PREFIX}%'")
    env.sql(
        "DELETE FROM workspace_group_read_cursor WHERE generation_id IN "
        f"(SELECT id FROM group_member_generation WHERE group_id = {GID})"
    )
    env.sql(f"DELETE FROM group_member_generation WHERE group_id = {GID}")
    env.sql(f"DELETE FROM group_member WHERE group_id = {GID}")
    env.sql(f"DELETE FROM \"group\" WHERE id = {GID}")
    env.sql(f"DELETE FROM msg_store_seq WHERE conv_key = 'c2g:{GID}'")
    # message_read_sync 的 save 行（时间窗 + gid 双重限定，避免误删他人 s2c）
    env.sql(
        f"DELETE FROM msg_s2c WHERE to_id = {uid_r} AND action = 'message_read_sync' "
        f"AND created_at > NOW() - INTERVAL '2 hours' "
        f"AND payload::text LIKE '%{GID}%'"
    )


def fixture_setup(env, uid_r, uid_s):
    env.sql(
        f"INSERT INTO \"group\" (id, owner_uid, creator_uid, title, status) "
        f"VALUES ({GID}, {uid_s}, {uid_s}, 'p1wse2e_fixture', 1)"
    )
    env.sql(
        f"INSERT INTO group_member (id, group_id, user_id, status, role) "
        f"VALUES (98768001, {GID}, {uid_s}, 1, 4), (98768002, {GID}, {uid_r}, 1, 1)"
    )
    env.sql(
        f"INSERT INTO group_member_generation (group_id, user_id, generation_no, start_seq) "
        f"VALUES ({GID}, {uid_s}, 1, 1), ({GID}, {uid_r}, 1, 1)"
    )


# ----------------------------------------------------------------------------
# 帧构造（v2.0 信封，与 imboyapp chat_network_service.buildReadReceiptItem /
# 消息发送同形态）
# ----------------------------------------------------------------------------

_seq_box = [0]


def gen_id():
    _seq_box[0] += 1
    return f"{MSG_PREFIX}{int(time.time() * 1000) % 10**10}{_seq_box[0]:03d}"


def now_ms():
    return int(time.time() * 1000)


def build_c2g(uid_s, content):
    return {
        "id": gen_id(),
        "type": "C2G",
        "from": str(uid_s),
        "to": str(GID),
        "msg_type": "text",
        "payload": {"content": content},
        "created_at": now_ms(),
    }


def build_read(uid_r, msg_ids):
    return {
        "id": gen_id(),
        "type": "C2G",
        "from": str(uid_r),
        "to": str(GID),
        "msg_type": "custom",
        "action": "message_read",
        "payload": {"msg_ids": msg_ids},
        "created_at": now_ms(),
    }


# ----------------------------------------------------------------------------
# 四条链
# ----------------------------------------------------------------------------

def timeline_row_exists(env, uid, mid):
    return env.sql(f"SELECT count(*) FROM msg_c2g_timeline WHERE to_uid = {uid} AND msg_id = '{mid}'") == "1"


def wait_timeline_row(env, uid, mid, timeout=10.0):
    """timeline 行由 msg_store_worker 异步落库：等行出现再走 CLIENT_ACK/read/SQL。"""
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            if timeline_row_exists(env, uid, mid):
                return
        except RuntimeError:
            pass
        time.sleep(0.2)
    raise TimeoutError(f"timeline 行 {mid[-8:]} {timeout}s 内未落库（worker 滞后）")


def send_and_confirm(env, sconn, content, r1, r2):
    """S 发 1 条 C2G；等待 SERVER_ACK + R1/R2 双连接实时帧 + timeline 落库；返回 msg_id。"""
    frame = build_c2g(env.uid_s, content)
    mid = frame["id"]
    sconn.send_json(frame)
    sconn.wait_for(
        lambda o: o.get("type") == "C2G_SERVER_ACK" and o.get("in_reply_to") == mid,
        desc=f"C2G_SERVER_ACK({mid})",
    )
    if r1 is not None:
        r1.wait_for(
            lambda o: o.get("type") == "C2G" and o.get("id") == mid and o.get("from") == str(env.uid_s),
            desc=f"R1 实时 C2G({mid})",
        )
    if r2 is not None:
        r2.wait_for(
            lambda o: o.get("type") == "C2G" and o.get("id") == mid and o.get("from") == str(env.uid_s),
            desc=f"R2 实时 C2G({mid})",
        )
    wait_timeline_row(env, env.uid_r, mid)
    return mid


def run_chain_1(env, clients, ctx):
    """E2E-1 (I1)：送达 ACK 纯标记——不动游标/未读，不影响 R2 补拉。"""
    r1, r2, sconn = clients["R1"], clients["R2"], clients["S"]
    c = Chain("E2E-1", "I1 送达 ACK 永不推进已读游标、永不清/减未读", env.ev)

    mids = [send_and_confirm(env, sconn, f"e2e1-msg-{i}", r1, r2) for i in (1, 2, 3)]
    ctx["mids"] = mids

    # R1 对全部消息回送达 ACK（CLIENT_ACK,C2G,<mid>,<did>）
    for mid in mids:
        r1.send_text(f"CLIENT_ACK,C2G,{mid},{DID_R1}")
        r1.wait_for(
            lambda o, m=mid: o.get("type") == "CLIENT_ACK_CONFIRM" and o.get("id") == m,
            desc=f"CLIENT_ACK_CONFIRM({mid})",
        )
    time.sleep(0.8)  # 留出重试管道收敛窗口

    for mid in mids:
        flag = env.sql(SQL_CLIENT_ACK.format(uid=env.uid_r, mid=mid))
        c.check(f"client_ack[{mid[-6:]}]==true（per-uid 送达标记）", flag == "t", f"db={flag}")

    cursor = env.sql_int(SQL_CURSOR.format(gid=GID, uid=env.uid_r))
    c.check("已读游标未被送达 ACK 推进（=0）", cursor == 0, f"cursor={cursor}")

    unread = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("未读=3（送达 ACK 后未读不清零）", unread == 3, f"unread={unread}")

    # R2 视角 HTTP 离线补拉：A(R1) 已 ACK，R2 仍能取回全部
    rc, resp = env.http("GET", "/api/v1/msg/offline", env.token_r, qs="?limit=1000&did=" + DID_R2)
    c2g = (resp.get("payload") or {}).get("c2g") or {}
    got = {it.get("msg_id") for it in c2g.get("list") or []}
    missing = [m for m in mids if m not in got]
    c.check(
        "R2 离线补拉完整含 3 条（不受 R1 送达 ACK 影响）",
        rc == 200 and not missing,
        f"status={rc} missing={missing}",
    )
    return c


def run_chain_2(env, clients, ctx):
    """E2E-2 (I2)：游标 GREATEST 单调 + message_read_sync 双连接下发。"""
    r1, r2, sconn = clients["R1"], clients["R2"], clients["S"]
    c = Chain("E2E-2", "I2 游标单调；逆序/重复已读不回退、不误清 N+1 未读", env.ev)
    mids = ctx["mids"]

    def sync_pred(o):
        if o.get("type") == "S2C" and o.get("action") == "message_read_sync":
            p = o.get("payload") or {}
            if p.get("gid") == GID:
                return o
        return None

    # R1 阅读到 m2（seq=2）
    frame = build_read(env.uid_r, [mids[1]])
    r1.send_json(frame)
    ack = r1.wait_for(
        lambda o: o.get("type") == "C2G" and o.get("action") == "message_read_ack"
        and o.get("id") == frame["id"],
        desc="message_read_ack(读到 m2)",
    )
    read_seq = ((ack.get("payload") or {}).get("read_seq"))
    c.check("message_read_ack 回显 read_seq=2", read_seq == 2, f"read_seq={read_seq}")

    sync1 = r1.wait_for(sync_pred, desc="R1 收 message_read_sync")
    sync2 = r2.wait_for(sync_pred, desc="R2 收 message_read_sync（跨设备同步）")
    for name, s in (("R1", sync1), ("R2", sync2)):
        p = s.get("payload") or {}
        ok = p.get("read_seq") == 2 and p.get("conversation_type") == "c2g" and mids[1] in (p.get("msg_ids") or [])
        c.check(f"{name} 收 message_read_sync(read_seq=2,c2g)", ok, json.dumps(p, ensure_ascii=False)[:200])

    cursor = env.sql_int(SQL_CURSOR.format(gid=GID, uid=env.uid_r))
    unread = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("游标=2 且未读=1", cursor == 2 and unread == 1, f"cursor={cursor} unread={unread}")

    # 逆序/重复已读（回退到 m1）：游标不动、未读不变；R2 收到新 sync（不回退值）
    mark_after = r2.frame_count()
    frame2 = build_read(env.uid_r, [mids[0]])
    r1.send_json(frame2)
    sync2r2 = r2.wait_for(sync_pred, desc="R2 再收 sync（逆序上报）", after=mark_after)
    p2 = sync2r2.get("payload") or {}
    c.check("逆序上报后 R2 新 sync read_seq 仍=2（不回退）", p2.get("read_seq") == 2,
            json.dumps(p2, ensure_ascii=False)[:200])
    cursor = env.sql_int(SQL_CURSOR.format(gid=GID, uid=env.uid_r))
    unread = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("逆序已读后游标仍=2、未读仍=1（不回退）", cursor == 2 and unread == 1,
            f"cursor={cursor} unread={unread}")

    # N+1 到达仍未读；重复旧已读不误清它
    m4 = send_and_confirm(env, sconn, "e2e2-n-plus-1", r1, r2)
    ctx["mids"].append(m4)
    unread = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("N+1(m4) 到达后未读=2", unread == 2, f"unread={unread}")

    r1.send_json(build_read(env.uid_r, [mids[0]]))  # 重复上报 m1
    time.sleep(0.8)
    cursor = env.sql_int(SQL_CURSOR.format(gid=GID, uid=env.uid_r))
    unread = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("重复旧已读后游标=2、未读=2（N+1 未读不误清）",
            cursor == 2 and unread == 2, f"cursor={cursor} unread={unread}")
    return c


def run_chain_3(env, clients, ctx):
    """E2E-3 (I3)：R1 ACK / offline_ack 后 R2 补拉仍完整取回。"""
    r1 = clients["R1"]
    c = Chain("E2E-3", "I3 设备 B 离线补拉完整取回——不因设备 A 的确认删行", env.ev)
    mids = ctx["mids"]

    # m4 尚未 CLIENT_ACK；先由 R1 补 ACK（全量 per-uid 置位）
    r1.send_text(f"CLIENT_ACK,C2G,{mids[3]},{DID_R1}")
    r1.wait_for(
        lambda o: o.get("type") == "CLIENT_ACK_CONFIRM" and o.get("id") == mids[3],
        desc=f"CLIENT_ACK_CONFIRM({mids[3][-6:]})",
    )
    flags = [env.sql(SQL_CLIENT_ACK.format(uid=env.uid_r, mid=m)) for m in mids]
    c.check("4 条 client_ack 全 true（A 设备已全量送达确认）",
            all(f == "t" for f in flags), f"flags={flags}")

    rc, resp = env.http("GET", "/api/v1/msg/offline", env.token_r, qs="?limit=1000&did=" + DID_R2)
    c2g = (resp.get("payload") or {}).get("c2g") or {}
    got = {it.get("msg_id") for it in c2g.get("list") or []}
    missing = [m for m in mids if m not in got]
    c.check("ACK 全量置位后 R2 补拉仍完整含 4 条", rc == 200 and not missing,
            f"status={rc} missing={missing}")

    # A(R1) 再走 REST offline_ack 批量确认——不得删行
    rc, resp = env.http(
        "POST", "/api/v1/msg/offline_ack", env.token_r,
        body={"type": "c2g", "msg_ids": mids, "did": DID_R1},
    )
    c.check("offline_ack(c2g,4条,R1did) 返回成功",
            rc == 200 and resp.get("code") in (0, "0"),
            f"status={rc} resp_code={resp.get('code')}")

    flags2 = [env.sql(SQL_CLIENT_ACK.format(uid=env.uid_r, mid=m)) for m in mids]
    c.check("offline_ack 后 timeline 行仍在（client_ack 保持 true）",
            all(f == "t" for f in flags2), f"flags={flags2}")

    rc, resp = env.http("GET", "/api/v1/msg/offline", env.token_r, qs="?limit=1000&did=" + DID_R2)
    c2g = (resp.get("payload") or {}).get("c2g") or {}
    got = {it.get("msg_id") for it in c2g.get("list") or []}
    missing = [m for m in mids if m not in got]
    c.check("offline_ack 后 R2 补拉仍完整含 4 条（I3 主 oracle）",
            rc == 200 and not missing, f"status={rc} missing={missing}")
    return c


def run_chain_4(env, clients, ctx):
    """E2E-4 (I4)：未读 = count(seq > 游标)——发 N 读 M 后双口径复算。"""
    r1, r2, sconn = clients["R1"], clients["R2"], clients["S"]
    c = Chain("E2E-4", "I4 未读计数=已投递且未被已读游标覆盖的消息数", env.ev)

    # 清零基线：把游标推到最新
    r1.send_json(build_read(env.uid_r, ctx["mids"]))
    time.sleep(0.8)
    unread0 = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("基线：全部已读后未读=0", unread0 == 0, f"unread={unread0}")

    n_new = [send_and_confirm(env, sconn, f"e2e4-n{i}", r1, r2) for i in range(1, 6)]
    unread1 = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check(f"发 N=5 条后未读=5", unread1 == 5, f"unread={unread1}")

    # 读 M=3 条
    r1.send_json(build_read(env.uid_r, n_new[:3]))
    time.sleep(0.8)
    unread2 = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("读 M=3 条后未读=2（N-M）", unread2 == 2, f"unread={unread2}")

    # 双口径交叉：count_unread_by_uid SQL vs 手工 count(seq>游标)
    cursor = env.sql_int(SQL_CURSOR.format(gid=GID, uid=env.uid_r))
    manual = env.sql_int(
        f"SELECT count(*) FROM msg_c2g_timeline tl "
        f"JOIN public.msg_c2g m ON m.msg_id = tl.msg_id AND m.created_at = tl.created_at "
        f"AND m.to_id = tl.to_gid "
        f"WHERE tl.to_uid = {env.uid_r} AND tl.to_gid = {GID} "
        f"AND tl.conv_seq > {cursor} AND m.from_id <> {env.uid_r} "
        f"AND (m.expire_at IS NULL OR m.expire_at > NOW())"
    )
    c.check("双口径一致（服务端 SQL == 手工 count(seq>游标)）", manual == unread2 == 2,
            f"manual={manual} service={unread2} cursor={cursor}")

    # 读剩余 2 条 → 归零
    r1.send_json(build_read(env.uid_r, n_new[3:]))
    time.sleep(0.8)
    unread3 = env.sql_int(SQL_UNREAD.format(uid=env.uid_r, gid=GID))
    c.check("读剩余 2 条后未读=0", unread3 == 0, f"unread={unread3}")

    # 补充：conv_seq 严格递增（真实链路由 staging 事务分配）
    rows = env.sql(SQL_TIMELINE_SEQS.format(uid=env.uid_r, gid=GID, pfx=MSG_PREFIX))
    seqs = [int(x.split("|")[1]) for x in rows.splitlines() if "|" in x] if rows else []
    strictly_inc = all(a < b for a, b in zip(seqs, seqs[1:]))
    c.check(f"timeline conv_seq 严格递增（{len(seqs)} 行）", len(seqs) >= 9 and strictly_inc,
            f"seqs={seqs}")
    return c


# ----------------------------------------------------------------------------
# 主流程
# ----------------------------------------------------------------------------

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", required=True, help="imboy 仓绝对路径")
    ap.add_argument("--base-url", default="http://127.0.0.1:9800")
    ap.add_argument("--evidence", required=True, help="证据落盘路径（追加写）")
    args = ap.parse_args()

    ev = Evidence(args.evidence)
    ev.text(f"\n===== ws_e2e_test start {time.strftime('%Y-%m-%dT%H:%M:%S%z')} =====")
    env = None
    clients = {}
    chains = []
    blocked = None
    ctx = {}
    rc = 2
    try:
        env = Env(args.repo, args.base_url, ev, None)

        # ---- 节点探活 ----
        rc_probe, resp = env.http("GET", "/api/v1/conversation/mine", "invalid-token")
        if rc_probe not in (200, 401):
            raise RuntimeError(f"节点未就绪：/api/v1/conversation/mine -> {rc_probe}")
        ev.text(f"node alive: status={rc_probe}")

        # ---- seeding ----
        uid_r_s = env.sql(f"SELECT id FROM \"user\" WHERE mobile = '{UID_R_MOBILE}' LIMIT 1")
        if not uid_r_s:
            raise RuntimeError(f"隔离库缺接收者 {UID_R_MOBILE}（AC02 测试用户未 seed）")
        uid_r = int(uid_r_s)

        uid_s_s = env.sql(f"SELECT id FROM \"user\" WHERE mobile = '{S_MOBILE}' LIMIT 1")
        if uid_s_s:
            uid_s = int(uid_s_s)
            ev.text(f"sender exists uid={uid_s}")
        else:
            code, out = env.ctl("user", "create", "0", "-a", S_MOBILE, "-n", S_NICKNAME, "-p", S_PASSWORD)
            m = re.search(r"UID=(\d+)", out)
            if code != 0 or not m:
                raise RuntimeError(f"imboy_ctl user create 失败: rc={code} out={out[:200]}")
            uid_s = int(m.group(1))
            ev.text(f"sender created uid={uid_s}")
        env.uid_r, env.uid_s = uid_r, uid_s

        code, tok = env.ctl("user", "token", str(uid_r))
        if code != 0 or not tok.strip():
            raise RuntimeError(f"user token(R) 失败: {tok[:200]}")
        env.token_r = tok.strip()
        code, tok = env.ctl("user", "token", str(uid_s))
        if code != 0 or not tok.strip():
            raise RuntimeError(f"user token(S) 失败: {tok[:200]}")
        env.token_s = tok.strip()

        fixture_cleanup(env, uid_r, uid_s)
        fixture_setup(env, uid_r, uid_s)
        ev.text(f"fixture ready: gid={GID} uid_r={uid_r} uid_s={uid_s}")

        # ---- 真双连接 ----
        ws_base = args.base_url.replace("http", "ws").replace("https", "wss")
        clients["R1"] = WSClient(ws_base, DID_R1, env.token_r, "R1", ev)
        clients["R2"] = WSClient(ws_base, DID_R2, env.token_r, "R2", ev)
        clients["S"] = WSClient(ws_base, DID_S, env.token_s, "S", ev)
        time.sleep(1.0)  # 三连接 online 注册窗口
        conn_cnt = env.sql(
            f"SELECT count(*) FROM group_member WHERE group_id = {GID}"
        )
        ev.text(f"ws connected x3, members={conn_cnt}")

        chains.append(run_chain_1(env, clients, ctx))
        chains.append(run_chain_2(env, clients, ctx))
        chains.append(run_chain_3(env, clients, ctx))
        chains.append(run_chain_4(env, clients, ctx))

        rc = 0 if all(ch.passed for ch in chains) else 1
    except Exception as e:
        import traceback
        ev.log("fatal", "-", {"error": str(e), "trace": traceback.format_exc()[-2000:]})
        blocked = str(e)
        rc = 2
    finally:
        for cl in clients.values():
            cl.close()
        if env is not None:
            try:
                fixture_cleanup(env, env.uid_r, env.uid_s)
                ev.text("fixture cleaned")
            except Exception as e:
                ev.log("cleanup_failed", "-", {"error": str(e)})

    # ---- summary ----
    summary = {
        "ts": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
        "agent": "BE-WS-E2E",
        "acceptance": ["AC07", "AC08"],
        "chains": [
            {
                "chain": ch.chain_id,
                "invariant": ch.invariant,
                "status": "PASS" if ch.passed else "FAIL",
                "checks": ch.checks,
            }
            for ch in chains
        ],
        "blocked": blocked,
        "exit_code": rc,
    }
    if blocked and not chains:
        for cid, inv in (
            ("E2E-1", "I1"), ("E2E-2", "I2"), ("E2E-3", "I3"), ("E2E-4", "I4"),
        ):
            summary["chains"].append({"chain": cid, "invariant": inv, "status": "BLOCKED",
                                      "checks": [], "reason": blocked})

    ev.text("===== ws_e2e_test summary =====")
    ev.text(json.dumps(summary, ensure_ascii=False, indent=1))
    print(json.dumps({k: v for k, v in summary.items() if k != "chains"},
                     ensure_ascii=False))
    for ch in summary["chains"]:
        print(f"  {ch['chain']}: {ch['status']}")
    ev.close()
    sys.exit(rc)


if __name__ == "__main__":
    main()
