#!/usr/bin/env python3
"""macOS integration_test 截图采集器（N2 原生验收取证）。

用法（stdin 接 flutter test --machine 的原始输出，含非 JSON 的 VM Service 行）：
  flutter test <file> -d macos --machine ... | python3 n2_capture_macos.py <out_dir>

监听：
  - stdout 里的 "Dart VM Service listening on http://127.0.0.1:PORT" → VM 服务
  - --machine 流中 print 事件的 [AI_SCREENSHOT] name → 经 VM 服务
    ext.flutter.screenshot 抓真实渲染帧存 <out_dir>/<seq>_<name>.png

截图是 Flutter 引擎渲染的真实帧（与设备所见一致），非布局示意图。
"""
import base64
import json
import os
import re
import sys
import threading

import websocket

OUT = sys.argv[1] if len(sys.argv) > 1 else 'screenshots'
os.makedirs(OUT, exist_ok=True)

VM_RE = re.compile(r'Dart VM Service listening on (http://127\.0\.0\.1:\d+)')
MARK_RE = re.compile(r'\[AI_SCREENSHOT\]\s+(\S+)')

state = {'uri': None, 'ws': None, 'seq': 0, 'lock': threading.Lock()}
pending = []


def connect(uri):
    ws_uri = uri.replace('http://', 'ws://') + '/ws'
    ws = websocket.create_connection(ws_uri, timeout=20)
    state['ws'] = ws
    print(f'[capture] connected {ws_uri}', file=sys.stderr)
    try:
        ws.send(json.dumps({'id': 900001, 'method': 'getVM'}))
        vm = json.loads(ws.recv())
        for iso in vm['result']['isolates']:
            ws.send(json.dumps({
                'id': 900002, 'method': 'getIsolate',
                'params': {'isolateId': iso['id']},
            }))
            detail = json.loads(ws.recv())
            exts = detail['result'].get('extensionRPCs', [])
            print(
                '[capture] extensions: '
                + str([e for e in exts if 'screen' in e or 'shot' in e])
                + f' (total {len(exts)})',
                file=sys.stderr,
            )
            break
    except Exception as exc:  # noqa: BLE001
        print(f'[capture] probe failed: {exc}', file=sys.stderr)
    return ws


def shoot(name):
    ws = state['ws']
    if ws is None:
        print(f'[capture] SKIP {name}: no vm service yet', file=sys.stderr)
        return
    with state['lock']:
        state['seq'] += 1
        seq = state['seq']
        try:
            ws.send(json.dumps({'id': seq, 'method': 'ext.flutter.screenshot'}))
            while True:
                raw = ws.recv()
                msg = json.loads(raw)
                if msg.get('id') == seq:
                    break
            if 'result' not in msg or 'screenshot' not in msg.get('result', {}):
                print(
                    f'[capture] RESP {name}: {json.dumps(msg)[:300]}',
                    file=sys.stderr,
                )
                return
            b64 = msg['result']['screenshot']
            path = os.path.join(OUT, f'{seq:02d}_{name}.png')
            with open(path, 'wb') as fh:
                fh.write(base64.b64decode(b64))
            print(f'[capture] saved {path} ({len(b64) // 1024}KB b64)',
                  file=sys.stderr)
        except Exception as exc:  # noqa: BLE001
            print(f'[capture] FAIL {name}: {exc}', file=sys.stderr)


def worker():
    while True:
        name = pending.pop(0) if pending else None
        if name:
            shoot(name)
        else:
            import time
            time.sleep(0.05)


def main():
    threading.Thread(target=worker, daemon=True).start()
    for line in sys.stdin:
        m = VM_RE.search(line)
        if m and not state['ws']:
            try:
                connect(m.group(1))
            except Exception as exc:  # noqa: BLE001
                print(f'[capture] connect failed: {exc}', file=sys.stderr)
        if line.startswith('{') or line.startswith('['):
            try:
                event = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(event, list):
                for item in event:
                    if isinstance(item, dict) and item.get(
                        'event'
                    ) == 'test.startedProcess':
                        uri = (item.get('params') or {}).get('vmServiceUri')
                        if uri and not state['ws']:
                            try:
                                connect(uri)
                            except Exception as exc:  # noqa: BLE001
                                print(
                                    f'[capture] connect failed: {exc}',
                                    file=sys.stderr,
                                )
                continue
            if isinstance(event, dict) and event.get('type') == 'print':
                mm = MARK_RE.search(str(event.get('message', '')))
                if mm:
                    pending.append(mm.group(1))
    # 排空
    import time
    while pending:
        time.sleep(0.3)


if __name__ == '__main__':
    main()
