#!/usr/bin/env python3
"""IMBOY_HIRD_AUTH_REPLAY_POC_V1 — 从 sys.local.config 提取 pg_conf 连接
参数，输出 HIRDPOC_PG_* export 行（stdout 供 bash eval；密码只进 env 值，
不打印任何敏感内容到日志）。"""
import re
import sys

s = open(sys.argv[1]).read()
m = re.search(r'\{pg_conf.*?\}\}', s, re.S)
frag = m.group(0) if m else ''
vals = {}
for k, v1, v2, v3 in re.findall(r'(\w+)\s*=>\s*(?:"([^"]*)"|\'([^\']*)\'|([0-9]+))', frag):
    cand = v1 or v2 or v3
    if cand:
        vals[k] = cand

print(f'export HIRDPOC_PG_HOST="{vals.get("host", "127.0.0.1")}"')
print(f'export HIRDPOC_PG_PORT="{vals.get("port", "5432")}"')
print(f'export HIRDPOC_PG_USER="{vals.get("username", "")}"')
print(f'export HIRDPOC_PG_DB="{vals.get("database", "")}"')
print('export HIRDPOC_PG_MAINT_DB="postgres"')
print(f'export HIRDPOC_PG_PASSWORD="{vals.get("password", "")}"')
