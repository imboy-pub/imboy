#!/usr/bin/env bash
# V3-07 外部内容 Worker gate：独立 VM 直跑端到端断言（真 Garage fixture）。
# 环境：A0_ENDPOINT/A0_AK/A0_AK 指向一次性 Garage；PGPASSWORD/IMBOYENV 沿 eunit-local。
# 退出：0 全绿；1 断言失败；2 环境缺失。
set -uo pipefail
B="$(cd "$(dirname "$0")/../.." && pwd)"
[ -n "${A0_ENDPOINT:-}" ] || { echo "ENV_MISSING: A0_ENDPOINT"; exit 2; }
PA=""
for d in "$B"/deps/*/ebin; do PA="$PA -pa $d"; done
exec erl -noinput -pa "$B/ebin" $PA -pa "$B/test/lib" -run attachment_worker_port_gate main
