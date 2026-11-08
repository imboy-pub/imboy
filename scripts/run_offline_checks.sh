#!/usr/bin/env bash
# Reuse upstream regression tests; no real Docker or network deployment.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
bash scripts/check_delivery.sh
bash scripts/test/install_one_click_test.sh
bash deploy/preflight.sh --self-test
bash scripts/test/delivery_guard_test.sh
printf '\n[PASS] 离线测试结束；真实服务器验收仍须另行执行。\n'
