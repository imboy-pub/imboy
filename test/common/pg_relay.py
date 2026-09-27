#!/usr/bin/env python3
"""A1c (CP-TD-A02) fork-per-connection TCP relay for eunit-local PG access.

背景：全量 eunit 长跑中，长命 eunit VM 的新建 TCP 连接会被 macOS Docker
端口转发层（com.docker.backend @127.0.0.1:<pg_port>）按调用方进程楔死——
持续 econnrefused 且不恢复，而同一时刻 pg_isready / 全新进程连接正常
（evidence/CP-TD-A02 run1-4；防火墙关闭、无内容过滤器、排除并发/累计
连接数与 PG 上限成因）。

对策：本 relay 以「每连接 fork 子进程」模式把 127.0.0.1:<listen_port>
转发到 127.0.0.1:<target_port>。eunit VM 只连 relay（15432）；真正触达
4323 的 connect 全部来自 fork 出的全新子进程——按进程楔死机制无法命中。
父进程只 accept+fork，不做任何数据面工作。

用法：python3 test/common/pg_relay.py <listen_port> <target_port>
"""
import os
import sys
import socket

LISTEN = ("127.0.0.1", int(sys.argv[1]))
TARGET = ("127.0.0.1", int(sys.argv[2]))


def pipe(src, dst):
    try:
        while True:
            data = src.recv(65536)
            if not data:
                break
            dst.sendall(data)
    except OSError:
        pass
    finally:
        try:
            dst.shutdown(socket.SHUT_WR)
        except OSError:
            pass


def handle(client):
    try:
        upstream = socket.create_connection(TARGET, timeout=10)
    except OSError:
        client.close()
        return
    p1 = os.fork()
    if p1 == 0:
        # 子进程 1：client -> upstream
        try:
            pipe(client, upstream)
        finally:
            os._exit(0)
    p2 = os.fork()
    if p2 == 0:
        # 子进程 2：upstream -> client
        try:
            pipe(upstream, client)
        finally:
            os._exit(0)
    # 父进程：关掉已交接的 fd，等两个双向管道子进程收尾
    client.close()
    upstream.close()
    for pid in (p1, p2):
        try:
            os.waitpid(pid, 0)
        except ChildProcessError:
            pass


def main():
    srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    srv.bind(LISTEN)
    srv.listen(256)
    print("pg_relay ready %s:%d -> %s:%d" % (LISTEN[0], LISTEN[1],
                                             TARGET[0], TARGET[1]),
          flush=True)
    # A1c：accept 循环永不退出——父进程一死，所有经中继的 PG 连接立即
    # econnrefused（run12 实证：瞬态 accept 异常可致父进程静默退场）。
    while True:
        try:
            client, _addr = srv.accept()
        except OSError:
            continue
        pid = os.fork()
        if pid == 0:
            srv.close()
            try:
                handle(client)
            finally:
                os._exit(0)
        client.close()
        # 父进程回收子进程（非阻塞兜底，避免僵尸堆积）
        try:
            while True:
                done, _ = os.waitpid(-1, os.WNOHANG)
                if done == 0:
                    break
        except ChildProcessError:
            pass


if __name__ == "__main__":
    main()
