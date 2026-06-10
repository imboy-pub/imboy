#!/usr/bin/env bash
# WebSocket 并发连接基准 / WebSocket concurrent-connection benchmark
# 建 N 条带鉴权的真实 WS 连接，测握手延迟分布与稳定保持能力。
# Opens N authenticated WS connections; reports handshake latency
# percentiles and hold stability. 连接参数与 smoke/c2c_ws_smoke.py 一致。
#
# ⚠️ 仅限本地/压测环境使用，严禁对生产环境运行。
# ⚠️ Local/load-test environments ONLY. Never run against production.
#
# 依赖 / Requires: python3 + websockets（pip install websockets，smoke 同款）
#
# 用法 / Usage:
#   TOKEN=<jwt> bash scripts/bench_websocket.sh                  # 默认 100 连接，保持 10s
#   TOKEN=<jwt> CONNS=500 HOLD=30 bash scripts/bench_websocket.sh
#   WS_URL=ws://host:9800/ws TOKEN=<jwt> bash scripts/bench_websocket.sh
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

: "${TOKEN:?需要 TOKEN（取法：escript scripts/imboy_ctl user token <uid>）}"
export WS_URL="${WS_URL:-ws://127.0.0.1:9800/ws}"
export CONNS="${CONNS:-100}"
export HOLD="${HOLD:-10}"
export TOKEN

python3 -c 'import websockets' 2>/dev/null \
  || { echo "✗ 缺少 python websockets 库：pip install websockets" >&2; exit 1; }

exec python3 - <<'PY'
import asyncio, os, statistics, sys, time

import websockets

WS_URL = os.environ["WS_URL"]
TOKEN = os.environ["TOKEN"]
CONNS = int(os.environ["CONNS"])
HOLD = int(os.environ["HOLD"])

# 与 smoke/c2c_ws_smoke.py 相同的鉴权头 / Same auth headers as the smoke script
def headers(i: int):
    return [
        ("authorization", f"Bearer {TOKEN}"),
        ("did", f"bench-{i:06d}"),
        ("cos", "mac"),
        ("vsn", "1.0.0"),
    ]

latencies: list[float] = []
errors: dict[str, int] = {}
held = 0

async def one(i: int, stagger: float):
    global held
    await asyncio.sleep(stagger)
    t0 = time.monotonic()
    try:
        ws = await asyncio.wait_for(
            websockets.connect(WS_URL, additional_headers=headers(i),
                               subprotocols=["imboy-json", "text"]),
            timeout=10,
        )
    except Exception as e:
        errors[type(e).__name__] = errors.get(type(e).__name__, 0) + 1
        return
    latencies.append((time.monotonic() - t0) * 1000)
    try:
        await asyncio.sleep(HOLD)  # 保持连接，靠服务端/WS 层心跳维持
        held += 1
    except Exception as e:
        errors[f"hold:{type(e).__name__}"] = errors.get(f"hold:{type(e).__name__}", 0) + 1
    finally:
        try:
            await ws.close()
        except Exception:
            pass

async def main():
    global held
    print(f"目标 {WS_URL} | 并发 {CONNS} | 保持 {HOLD}s")
    # 50/s 阶梯爬坡，避免瞬时 SYN 洪峰把指标打歪 / ramp at 50 conns/s
    await asyncio.gather(*(one(i, i / 50.0) for i in range(CONNS)))

    ok = len(latencies)
    print("\n========== 结果 / Results ==========")
    print(f"握手成功 / connected : {ok}/{CONNS}")
    print(f"保持成功 / held {HOLD}s: {held}/{ok}")
    if latencies:
        ls = sorted(latencies)
        q = lambda p: ls[min(int(len(ls) * p), len(ls) - 1)]
        print(f"握手延迟 ms: min={ls[0]:.1f} p50={q(.5):.1f} p95={q(.95):.1f} "
              f"p99={q(.99):.1f} max={ls[-1]:.1f} avg={statistics.mean(ls):.1f}")
    if errors:
        print("错误分布 / errors:")
        for k, v in sorted(errors.items()):
            print(f"  {k}: {v}")
    sys.exit(0 if ok == CONNS and held == ok else 1)

asyncio.run(main())
PY
