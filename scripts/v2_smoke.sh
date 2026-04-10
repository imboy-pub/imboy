#!/usr/bin/env bash
# imboy.v2 分层二进制帧协议全链路联调 smoke 脚本
#
# 运行 Erlang + Dart 两侧涉及 v2 帧、子协议协商、消息分派和 ACK 的所有测试。
# 不启动真实服务器，全部通过 eunit / flutter test 和 meck / FakeChannel 完成。
#
# 用法：
#   bash imboy/scripts/v2_smoke.sh
#
# 依赖：
#   - imboy 仓库在 ../imboy，imboyapp 仓库在 ../imboyapp（同一父目录下）
#   - make、erlang、flutter 在 PATH 中

set -euo pipefail

cd "$(dirname "$0")/.."
IMBOY_DIR="$(pwd)"
APP_DIR="$(cd "${IMBOY_DIR}/../imboyapp" && pwd)"

echo "=== Erlang: imboy_frame (26 tests) ==="
make eunit t=imboy_frame_tests

echo "=== Erlang: imboy_codec (v2 framing wrap/unwrap) ==="
make eunit t=imboy_codec_tests

echo "=== Erlang: websocket_ds (子协议协商) ==="
make eunit t=websocket_ds_tests

echo "=== Erlang: websocket_handler (30 tests, 含 v1/v2) ==="
make eunit t=websocket_handler_tests

echo "=== Erlang: v2_frame_e2e (9 端到端集成用例) ==="
make eunit t=v2_frame_e2e_tests

echo "=== Dart: imboy_frame (26 tests) ==="
(cd "${APP_DIR}" && flutter test test/service/imboy_frame_test.dart)

echo "=== Dart: websocket_v2 + websocket_v2_e2e ==="
(cd "${APP_DIR}" && flutter test \
    test/service/websocket_v2_test.dart \
    test/service/websocket_v2_e2e_test.dart)

echo ""
echo "================================================================"
echo "  imboy.v2 smoke passed."
echo "  (Erlang 9 e2e + 既有 v2 用例；Dart 7 + 12 + 26 跨语言用例)"
echo "================================================================"
