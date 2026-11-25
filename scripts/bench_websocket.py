#!/usr/bin/env python3
"""IMBoy WebSocket capacity benchmark.

The benchmark keeps every successful socket open until the complete ramp has
finished, then observes all sockets for a full HOLD window. It validates
liveness with WebSocket ping/pong and, by default, reconciles client sockets
against the server's R0 imboy_ws_admission_current gauge.

JWT payloads are decoded locally only to select the correct DID header. The
server remains responsible for signature, expiry, device and epoch validation.
"""
from __future__ import annotations

import asyncio
import base64
import json
import math
import os
import statistics
import sys
import time
import urllib.error
import urllib.request
import uuid
from collections import Counter
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any
from urllib.parse import urlsplit, urlunsplit

METRIC_KEYS = (
    "imboy_ws_admission_current",
    "imboy_ws_admission_limit",
    "imboy_ws_admission_rejected_total",
)


@dataclass(frozen=True)
class Credential:
    token: str = field(repr=False)
    uid: str
    token_did: str
    header_did: str


@dataclass
class Connection:
    index: int
    credential: Credential = field(repr=False)
    ws: Any = field(default=None, repr=False)
    receiver_task: Any = field(default=None, repr=False)
    latency_ms: float = 0.0
    pings_ok: int = 0
    frames_received: int = 0
    held: bool = False
    failure: str = ""
    peer_closed: bool = False
    close_code: Any = None
    close_reason: str = ""


@dataclass(frozen=True)
class Config:
    ws_url: str
    conns: int
    hold: float
    ramp_rate: float
    connect_parallelism: int
    connect_timeout: float
    ping_interval: float
    ping_timeout: float
    metrics_url: str | None
    metrics_required: bool
    metrics_interval: float
    metrics_timeout: float
    metrics_settle: float
    metrics_tolerance: int
    cleanup_timeout: float
    close_timeout: float
    did_prefix: str


def _env_int(name: str, default: int, minimum: int = 1) -> int:
    raw = os.environ.get(name, str(default)).strip()
    try:
        value = int(raw)
    except ValueError as exc:
        raise ValueError(f"{name} must be an integer, got {raw!r}") from exc
    if value < minimum:
        raise ValueError(f"{name} must be >= {minimum}")
    return value


def _env_float(name: str, default: float, minimum: float = 0.001) -> float:
    raw = os.environ.get(name, str(default)).strip()
    try:
        value = float(raw)
    except ValueError as exc:
        raise ValueError(f"{name} must be a number, got {raw!r}") from exc
    if not math.isfinite(value) or value < minimum:
        raise ValueError(f"{name} must be finite and >= {minimum}")
    return value


def _env_bool(name: str, default: bool) -> bool:
    raw = os.environ.get(name, "1" if default else "0").strip().lower()
    if raw in {"1", "true", "yes", "on"}:
        return True
    if raw in {"0", "false", "no", "off"}:
        return False
    raise ValueError(f"{name} must be one of 1/0, true/false, yes/no, on/off")


def decode_jwt_claims(token: str) -> dict[str, str]:
    """Decode untrusted JWT payload fields needed to pair token and DID."""
    parts = token.strip().split(".")
    if len(parts) != 3 or not parts[1]:
        raise ValueError("token is not a three-part JWT")
    payload_part = parts[1]
    try:
        payload_bytes = base64.urlsafe_b64decode(
            payload_part + "=" * (-len(payload_part) % 4)
        )
        payload = json.loads(payload_bytes)
    except (ValueError, json.JSONDecodeError) as exc:
        raise ValueError("JWT payload is not valid base64url JSON") from exc
    if not isinstance(payload, dict) or payload.get("uid") is None:
        raise ValueError("JWT payload must contain the IMBoy uid claim")
    uid = str(payload["uid"])
    did = payload.get("did", "")
    if did is None:
        did = ""
    if not isinstance(did, str):
        raise ValueError("JWT did claim must be a string")
    if len(did) > 128:
        raise ValueError("JWT did claim exceeds the server's 128-character limit")
    return {"uid": uid, "did": did}


def _synthetic_did(prefix: str, run_id: str, index: int) -> str:
    did = f"{prefix}-{run_id}-{index:06d}"
    if len(did) > 128:
        raise ValueError("DID_PREFIX produces a DID longer than 128 characters")
    return did


def _read_token_file(path_value: str) -> list[str]:
    path = Path(path_value).expanduser()
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except OSError as exc:
        raise ValueError(f"cannot read TOKENS_FILE {path}: {exc}") from exc
    tokens = []
    for line in lines:
        value = line.strip()
        if not value or value.startswith("#"):
            continue
        if value.lower().startswith("bearer "):
            value = value[7:].strip()
        tokens.append(value)
    if not tokens:
        raise ValueError(f"TOKENS_FILE {path} contains no JWT tokens")
    return tokens


def build_credentials(
    conns: int,
    token: str | None,
    tokens_file: str | None,
    did_prefix: str = "imboy-bench",
    run_id: str | None = None,
) -> tuple[list[Credential], str]:
    """Build per-connection credentials without reusing a bound device token."""
    token = token.strip() if token else None
    tokens_file = tokens_file.strip() if tokens_file else None
    if bool(token) == bool(tokens_file):
        raise ValueError("set exactly one of TOKEN (single token) or TOKENS_FILE")

    run_id = run_id or uuid.uuid4().hex[:12]
    if token:
        claims = decode_jwt_claims(token)
        if claims["did"]:
            if conns > 1:
                raise ValueError(
                    "TOKEN is bound to one DID; reusing it would create same-device "
                    "sessions and kick earlier sockets. Supply TOKENS_FILE with one "
                    "valid active-device JWT per connection, or use an explicitly "
                    "did-less legacy token for synthetic multi-device testing."
                )
            return [Credential(token, claims["uid"], claims["did"], claims["did"])], "bound"
        credentials = [
            Credential(token, claims["uid"], "", _synthetic_did(did_prefix, run_id, i + 1))
            for i in range(conns)
        ]
        return credentials, "legacy-unbound"

    assert tokens_file is not None
    tokens = _read_token_file(tokens_file)
    if len(tokens) < conns:
        raise ValueError(
            f"TOKENS_FILE has {len(tokens)} token(s), but CONNS={conns}; "
            "provide at least one token per connection"
        )

    credentials: list[Credential] = []
    seen_bound_devices: set[tuple[str, str]] = set()
    bound_count = 0
    for index, current_token in enumerate(tokens[:conns], start=1):
        claims = decode_jwt_claims(current_token)
        token_did = claims["did"]
        if token_did:
            identity = (claims["uid"], token_did)
            if identity in seen_bound_devices:
                raise ValueError(
                    f"TOKENS_FILE repeats the same bound device identity uid={claims['uid']} "
                    f"did={token_did!r}; every bound connection needs a distinct active device"
                )
            seen_bound_devices.add(identity)
            header_did = token_did
            bound_count += 1
        else:
            header_did = _synthetic_did(did_prefix, run_id, index)
        credentials.append(Credential(current_token, claims["uid"], token_did, header_did))

    if bound_count == conns:
        auth_mode = "bound"
    elif bound_count == 0:
        auth_mode = "legacy-unbound"
    else:
        auth_mode = "mixed"
    return credentials, auth_mode


def derive_metrics_url(ws_url: str) -> str:
    parsed = urlsplit(ws_url)
    if parsed.scheme not in {"ws", "wss"} or not parsed.netloc:
        raise ValueError("WS_URL must be an absolute ws:// or wss:// URL")
    scheme = "https" if parsed.scheme == "wss" else "http"
    return urlunsplit((scheme, parsed.netloc, "/metrics", "", ""))


def parse_prometheus_metrics(text: str) -> dict[str, int]:
    found: dict[str, int] = {}
    wanted = set(METRIC_KEYS)
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split()
        if len(parts) < 2:
            continue
        name = parts[0].split("{", 1)[0]
        if name not in wanted:
            continue
        try:
            value = float(parts[1])
        except ValueError:
            continue
        if not math.isfinite(value):
            continue
        found[name] = int(value)
    missing = wanted - found.keys()
    if missing:
        raise ValueError("Prometheus response is missing: " + ", ".join(sorted(missing)))
    return {
        "current": found["imboy_ws_admission_current"],
        "limit": found["imboy_ws_admission_limit"],
        "rejected_total": found["imboy_ws_admission_rejected_total"],
    }


def fetch_metrics(url: str, timeout: float = 3.0) -> dict[str, int]:
    request = urllib.request.Request(
        url,
        headers={"Accept": "text/plain", "User-Agent": "imboy-ws-bench/2"},
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            if response.status != 200:
                raise ValueError(f"metrics endpoint returned HTTP {response.status}")
            body = response.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as exc:
        raise ValueError(f"metrics endpoint returned HTTP {exc.code}") from exc
    except urllib.error.URLError as exc:
        raise ValueError(f"metrics endpoint request failed: {exc.reason}") from exc
    return parse_prometheus_metrics(body)


def metrics_delta_matches(
    baseline: dict[str, int],
    sample: dict[str, int],
    client_live: int,
    tolerance: int = 0,
) -> tuple[bool, int, int]:
    server_delta = sample["current"] - baseline["current"]
    difference = server_delta - client_live
    return abs(difference) <= tolerance, server_delta, difference


def load_config() -> Config:
    ws_url = os.environ.get("WS_URL", "ws://127.0.0.1:9800/api/v1/ws").strip()
    parsed = urlsplit(ws_url)
    if parsed.scheme not in {"ws", "wss"} or not parsed.netloc:
        raise ValueError("WS_URL must be an absolute ws:// or wss:// URL")

    metrics_raw = os.environ.get("METRICS_URL", "auto").strip() or "auto"
    metrics_url = None if metrics_raw.lower() == "off" else (
        derive_metrics_url(ws_url) if metrics_raw.lower() == "auto" else metrics_raw
    )
    metrics_required = _env_bool("METRICS_REQUIRED", True)
    if metrics_required and metrics_url is None:
        raise ValueError("METRICS_URL=off requires METRICS_REQUIRED=0")

    return Config(
        ws_url=ws_url,
        conns=_env_int("CONNS", 100),
        hold=_env_float("HOLD", 10.0),
        ramp_rate=_env_float("RAMP_RATE", 50.0),
        connect_parallelism=_env_int("CONNECT_PARALLELISM", 100),
        connect_timeout=_env_float("CONNECT_TIMEOUT", 10.0),
        ping_interval=_env_float("PING_INTERVAL", 15.0),
        ping_timeout=_env_float("PING_TIMEOUT", 5.0),
        metrics_url=metrics_url,
        metrics_required=metrics_required,
        metrics_interval=_env_float("METRICS_INTERVAL", 2.0),
        metrics_timeout=_env_float("METRICS_TIMEOUT", 3.0),
        metrics_settle=_env_float("METRICS_SETTLE", 1.0, minimum=0.0),
        metrics_tolerance=_env_int("METRICS_TOLERANCE", 0, minimum=0),
        cleanup_timeout=_env_float("CLEANUP_TIMEOUT", 10.0),
        close_timeout=_env_float("CLOSE_TIMEOUT", 5.0),
        did_prefix=os.environ.get("DID_PREFIX", "imboy-bench").strip() or "imboy-bench",
    )


def _status_error(exc: BaseException) -> str:
    response = getattr(exc, "response", None)
    status = getattr(response, "status_code", None) or getattr(response, "status", None)
    status = status or getattr(exc, "status_code", None)
    if status:
        return f"HTTP_{status}"
    return type(exc).__name__


def _headers(credential: Credential) -> list[tuple[str, str]]:
    return [
        ("authorization", f"Bearer {credential.token}"),
        ("did", credential.header_did),
        ("cos", "mac"),
        ("vsn", "1.0.0"),
    ]


async def _drain_frames(conn: Connection) -> None:
    try:
        async for _frame in conn.ws:
            conn.frames_received += 1
    except asyncio.CancelledError:
        raise
    except Exception as exc:  # connection close/errors are reported by the monitor
        if not conn.failure:
            conn.failure = f"recv:{type(exc).__name__}"
    finally:
        conn.peer_closed = True
        conn.close_code = getattr(conn.ws, "close_code", None)
        conn.close_reason = getattr(conn.ws, "close_reason", "") or ""


async def _connect_one(
    index: int,
    credential: Credential,
    started_at: float,
    cfg: Config,
    semaphore: asyncio.Semaphore,
    websockets_module: Any,
) -> tuple[Connection | None, str | None]:
    scheduled_at = started_at + index / cfg.ramp_rate
    delay = scheduled_at - time.monotonic()
    if delay > 0:
        await asyncio.sleep(delay)

    async with semaphore:
        conn = Connection(index=index + 1, credential=credential)
        started = time.monotonic()
        try:
            conn.ws = await asyncio.wait_for(
                websockets_module.connect(
                    cfg.ws_url,
                    additional_headers=_headers(credential),
                    subprotocols=["imboy-json", "text"],
                    open_timeout=cfg.connect_timeout,
                    close_timeout=cfg.close_timeout,
                    # Keep sockets alive during long ramp-up as well as HOLD.
                    ping_interval=cfg.ping_interval,
                    ping_timeout=cfg.ping_timeout,
                    max_queue=16,
                ),
                timeout=cfg.connect_timeout + 1.0,
            )
        except Exception as exc:
            return None, _status_error(exc)

        conn.latency_ms = (time.monotonic() - started) * 1000.0
        conn.receiver_task = asyncio.create_task(_drain_frames(conn))
        return conn, None


async def _ping_once(conn: Connection, timeout: float) -> None:
    if conn.receiver_task.done():
        raise ConnectionError(
            f"peer_closed:{conn.close_code}:{conn.close_reason}".rstrip(":")
        )
    pong_waiter = await conn.ws.ping()
    await asyncio.wait_for(pong_waiter, timeout=timeout)
    conn.pings_ok += 1
    if conn.receiver_task.done():
        raise ConnectionError(
            f"peer_closed:{conn.close_code}:{conn.close_reason}".rstrip(":")
        )


async def _monitor_connection(
    conn: Connection,
    deadline: float,
    ping_interval: float,
    ping_timeout: float,
) -> None:
    try:
        # websockets' keepalive task sends Ping/Pong throughout ramp-up and HOLD.
        # Explicit boundary pings prove the socket is alive at both ends of HOLD.
        await _ping_once(conn, ping_timeout)
        remaining = deadline - time.monotonic()
        if remaining > 0:
            done, _pending = await asyncio.wait(
                {conn.receiver_task}, timeout=remaining
            )
            if done:
                raise ConnectionError(
                    f"peer_closed:{conn.close_code}:{conn.close_reason}".rstrip(":")
                )
        await _ping_once(conn, ping_timeout)
        conn.held = True
    except Exception as exc:
        if not conn.failure:
            conn.failure = f"{type(exc).__name__}:{exc}"
        conn.held = False


async def _metrics_sample(
    label: str,
    cfg: Config,
    connections: list[Connection],
    samples: list[tuple[str, dict[str, int], int]],
    errors: list[str],
) -> None:
    if cfg.metrics_url is None:
        return
    try:
        metrics = await asyncio.to_thread(fetch_metrics, cfg.metrics_url, cfg.metrics_timeout)
        client_live = sum(
            1 for conn in connections
            if conn.receiver_task is not None and not conn.receiver_task.done()
        )
        samples.append((label, metrics, client_live))
    except Exception as exc:
        errors.append(f"{label}: {exc}")


async def _sample_metrics_during_hold(
    cfg: Config,
    connections: list[Connection],
    deadline: float,
    samples: list[tuple[str, dict[str, int], int]],
    errors: list[str],
) -> None:
    if cfg.metrics_url is None:
        return
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            return
        await asyncio.sleep(min(cfg.metrics_interval, remaining))
        if time.monotonic() >= deadline:
            return
        elapsed = cfg.hold - max(0.0, deadline - time.monotonic())
        await _metrics_sample(
            f"hold+{elapsed:.1f}s", cfg, connections, samples, errors
        )


async def _close_connections(connections: list[Connection], timeout: float) -> None:
    async def close_one(conn: Connection) -> None:
        if conn.ws is None:
            return
        try:
            await asyncio.wait_for(conn.ws.close(), timeout=timeout)
        except Exception:
            pass
        task = conn.receiver_task
        if task is not None and not task.done():
            try:
                await asyncio.wait_for(task, timeout=timeout)
            except Exception:
                task.cancel()
                await asyncio.gather(task, return_exceptions=True)

    await asyncio.gather(*(close_one(conn) for conn in connections))


async def _wait_for_cleanup(
    cfg: Config,
    baseline: dict[str, int] | None,
) -> tuple[dict[str, int] | None, bool | None, str | None]:
    if cfg.metrics_url is None or baseline is None:
        return None, None, None
    deadline = time.monotonic() + cfg.cleanup_timeout
    last: dict[str, int] | None = None
    last_error: str | None = None
    while True:
        try:
            last = await asyncio.to_thread(fetch_metrics, cfg.metrics_url, cfg.metrics_timeout)
            last_error = None
            if abs(last["current"] - baseline["current"]) <= cfg.metrics_tolerance:
                return last, True, None
        except Exception as exc:
            last_error = str(exc)
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            return last, False, last_error
        await asyncio.sleep(min(0.5, remaining))


def _percentile(values: list[float], fraction: float) -> float:
    ordered = sorted(values)
    index = max(0, math.ceil(fraction * len(ordered)) - 1)
    return ordered[index]


def _print_results(
    cfg: Config,
    auth_mode: str,
    baseline: dict[str, int] | None,
    connections: list[Connection],
    connect_errors: Counter[str],
    metrics_samples: list[tuple[str, dict[str, int], int]],
    metrics_errors: list[str],
    live_at_hold_end: int,
    cleanup_snapshot: dict[str, int] | None,
    cleanup_ok: bool | None,
    cleanup_error: str | None,
) -> bool:
    connected = len(connections)
    held = sum(1 for conn in connections if conn.held)
    latencies = [conn.latency_ms for conn in connections]
    ping_ok = sum(conn.pings_ok for conn in connections)
    failed_hold = Counter(
        conn.failure.split(":", 1)[0] for conn in connections if conn.failure
    )

    print("\n========== WebSocket benchmark v2 / 结果 ==========")
    print(f"目标 / target          : {cfg.ws_url}")
    print(f"请求连接 / requested   : {cfg.conns}")
    print(f"握手成功 / connected   : {connected}/{cfg.conns}")
    print(f"保持成功 / held {cfg.hold:g}s : {held}/{connected}")
    print(f"保持窗口结束时存活 / live-at-hold-end: {live_at_hold_end}/{connected}")
    print(f"保持窗口边界 Ping/Pong 成功次数: {ping_ok}")
    if latencies:
        print(
            "握手延迟 ms            : "
            f"min={min(latencies):.1f} p50={_percentile(latencies, .50):.1f} "
            f"p95={_percentile(latencies, .95):.1f} "
            f"p99={_percentile(latencies, .99):.1f} "
            f"max={max(latencies):.1f} avg={statistics.mean(latencies):.1f}"
        )
    if connect_errors:
        print("握手失败分布 / connect errors:")
        for key, count in sorted(connect_errors.items()):
            print(f"  {key}: {count}")
    if failed_hold:
        print("保持失败分布 / hold errors:")
        for key, count in sorted(failed_hold.items()):
            print(f"  {key}: {count}")

    metrics_ok: bool | None = True
    if baseline is None:
        print("服务端对账 / server reconciliation: NOT AVAILABLE (client-only; not capacity evidence)")
        metrics_ok = False if cfg.metrics_required else None
    else:
        print(
            "服务端基线 / baseline  : "
            f"current={baseline['current']} limit={baseline['limit']} "
            f"rejected_total={baseline['rejected_total']}"
        )
        if metrics_samples:
            values = [sample[1]["current"] for sample in metrics_samples]
            print(
                "服务端保持窗口 / hold  : "
                f"samples={len(metrics_samples)} min={min(values)} max={max(values)} "
                f"last={values[-1]}"
            )
            for label, metrics, client_live in metrics_samples:
                matches, server_delta, difference = metrics_delta_matches(
                    baseline, metrics, client_live, cfg.metrics_tolerance
                )
                if not matches:
                    metrics_ok = False
                    print(
                        f"  MISMATCH {label}: server_delta={server_delta} "
                        f"client_live={client_live} delta_error={difference}"
                    )
            last_metrics = metrics_samples[-1][1]
            _matches, server_delta, difference = metrics_delta_matches(
                baseline, last_metrics, metrics_samples[-1][2], cfg.metrics_tolerance
            )
            print(
                "服务端连接差值 / delta : "
                f"server={server_delta} client_live={metrics_samples[-1][2]} "
                f"error={difference} rejected_delta="
                f"{last_metrics['rejected_total'] - baseline['rejected_total']}"
            )
        else:
            metrics_ok = False
            print("服务端保持窗口 / hold  : NO SAMPLES")
        if cleanup_snapshot is not None:
            cleanup_delta = cleanup_snapshot["current"] - baseline["current"]
            print(
                "关闭后服务端 / cleanup: "
                f"current={cleanup_snapshot['current']} baseline={baseline['current']} "
                f"delta={cleanup_delta}"
            )
        if cleanup_ok is False:
            metrics_ok = False
            print(f"清理对账失败 / cleanup mismatch: {cleanup_error or 'admission count did not return to baseline'}")
        elif cleanup_ok is True:
            print("清理对账 / cleanup     : PASS (admission count returned to baseline)")
        else:
            metrics_ok = False
            print("清理对账 / cleanup     : NOT AVAILABLE")

    if metrics_errors:
        # In explicitly client-only mode, a missing metrics endpoint is disclosed
        # but does not turn the client-only diagnostic into a false server claim.
        if baseline is not None or cfg.metrics_required:
            metrics_ok = False
        print("指标采样错误 / metrics errors:")
        for error in metrics_errors:
            print(f"  {error}")

    handshake_ok = connected == cfg.conns
    hold_ok = held == connected
    success = handshake_ok and hold_ok and (metrics_ok is not False) and (cleanup_ok is not False)
    print(f"握手完整性 / all connected: {'PASS' if handshake_ok else 'FAIL'}")
    print(f"保持完整性 / all held     : {'PASS' if hold_ok else 'FAIL'}")
    metrics_label = "PASS" if metrics_ok is True else ("FAIL" if metrics_ok is False else "CLIENT-ONLY")
    print(f"服务端对账 / metrics       : {metrics_label}")
    result_label = "PASS (CLIENT-ONLY; NOT CAPACITY EVIDENCE)" if success and metrics_ok is None else ("PASS" if success else "FAIL")
    print(f"最终结果 / RESULT         : {result_label}")
    return success


async def run_benchmark() -> int:
    try:
        import websockets
    except ImportError:
        print("ERROR: python3 websockets package missing; install: python3 -m pip install websockets", file=sys.stderr)
        return 2

    try:
        cfg = load_config()
        token = os.environ.get("TOKEN")
        tokens_file = os.environ.get("TOKENS_FILE")
        credentials, auth_mode = build_credentials(
            cfg.conns, token, tokens_file, did_prefix=cfg.did_prefix
        )
    except ValueError as exc:
        print(f"ERROR: {exc}", file=sys.stderr)
        return 2

    print(
        f"WebSocket 压测 / target={cfg.ws_url} requested={cfg.conns} "
        f"ramp={cfg.ramp_rate:g}/s hold_after_ramp={cfg.hold:g}s "
        f"keepalive={cfg.ping_interval:g}s/{cfg.ping_timeout:g}s "
        f"connect_parallelism={cfg.connect_parallelism} auth={auth_mode}"
    )
    if auth_mode in {"legacy-unbound", "mixed"}:
        print("AUTH NOTE: did-less legacy token(s) use unique synthetic DID headers; this does not measure bound-device token validation.")
    if auth_mode in {"bound", "mixed"}:
        print("AUTH NOTE: bound token DID claims are used as-is; each bound (uid,did) must be a distinct active device on the server.")

    baseline: dict[str, int] | None = None
    metrics_errors: list[str] = []
    if cfg.metrics_url is not None:
        try:
            baseline = await asyncio.to_thread(fetch_metrics, cfg.metrics_url, cfg.metrics_timeout)
            print(
                f"SERVER METRICS: {cfg.metrics_url} "
                f"baseline_current={baseline['current']} limit={baseline['limit']}"
            )
        except Exception as exc:
            message = f"baseline: {exc}"
            if cfg.metrics_required:
                print(f"ERROR: required server metrics unavailable ({message}); refusing client-only capacity run", file=sys.stderr)
                return 2
            metrics_errors.append(message)
            print(f"WARNING: {message}; continuing in explicitly client-only mode", file=sys.stderr)
    else:
        print("WARNING: server metrics disabled; this run cannot be used as capacity evidence", file=sys.stderr)

    semaphore = asyncio.Semaphore(cfg.connect_parallelism)
    connections: list[Connection] = []
    connect_errors: Counter[str] = Counter()
    metrics_samples: list[tuple[str, dict[str, int], int]] = []
    cleanup_snapshot: dict[str, int] | None = None
    cleanup_ok: bool | None = None
    cleanup_error: str | None = None
    live_at_hold_end = 0

    started_at = time.monotonic()
    tasks = [
        asyncio.create_task(
            _connect_one(i, credential, started_at, cfg, semaphore, websockets)
        )
        for i, credential in enumerate(credentials)
    ]
    try:
        results = await asyncio.gather(*tasks)
        for conn, error in results:
            if conn is not None:
                connections.append(conn)
            elif error:
                connect_errors[error] += 1

        if connections:
            if cfg.metrics_settle:
                await asyncio.sleep(cfg.metrics_settle)
            await _metrics_sample("hold-start", cfg, connections, metrics_samples, metrics_errors)
            hold_deadline = time.monotonic() + cfg.hold
            monitor_tasks = [
                asyncio.create_task(
                    _monitor_connection(conn, hold_deadline, cfg.ping_interval, cfg.ping_timeout)
                )
                for conn in connections
            ]
            sampler_task = asyncio.create_task(
                _sample_metrics_during_hold(
                    cfg, connections, hold_deadline, metrics_samples, metrics_errors
                )
            )
            await asyncio.gather(*monitor_tasks)
            live_at_hold_end = sum(
                1 for conn in connections
                if conn.receiver_task is not None and not conn.receiver_task.done()
            )
            await sampler_task
            await _metrics_sample("hold-end", cfg, connections, metrics_samples, metrics_errors)

    except asyncio.CancelledError:
        raise
    except Exception as exc:
        print(f"ERROR: benchmark execution failed: {type(exc).__name__}: {exc}", file=sys.stderr)
        metrics_errors.append(f"benchmark: {type(exc).__name__}: {exc}")
    finally:
        await _close_connections(connections, cfg.close_timeout)
        cleanup_snapshot, cleanup_ok, cleanup_error = await _wait_for_cleanup(cfg, baseline)

    success = _print_results(
        cfg,
        auth_mode,
        baseline,
        connections,
        connect_errors,
        metrics_samples,
        metrics_errors,
        live_at_hold_end,
        cleanup_snapshot,
        cleanup_ok,
        cleanup_error,
    )
    return 0 if success else 1


def main() -> int:
    try:
        return asyncio.run(run_benchmark())
    except KeyboardInterrupt:
        print("Interrupted; closing the benchmark run.", file=sys.stderr)
        return 130


if __name__ == "__main__":
    sys.exit(main())
