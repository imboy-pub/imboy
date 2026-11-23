#!/bin/bash
# R0 capacity probe orchestrator.
# Isolated by construction: loopback only, scratch dir only, reads compiled
# cowboy/ranch/gun beams from the main checkout (read-only), never touches
# any running node, any repo file or any configuration.
#
# usage: run_probe.sh <name> <shape> <N> <phase1_timeout_ms> <close_k> <phase2_wait_ms> "<erl +P flag or empty>"
set -u
D=/tmp/r0-capacity-probe-20261123
DEPS=/Users/leeyi/project/imboy.pub/imboy/deps
name="$1"; shape="$2"; n="$3"; tmo="$4"; closek="$5"; ph2="$6"; pflag="$7"
stamp=$(date +%H%M%S)
run="$D/run/${name}-${stamp}"
mkdir -p "$run"
port=$(python3 -c 'import socket;s=socket.socket();s.bind(("127.0.0.1",0));print(s.getsockname()[1])')
echo "run_dir=$run port=$port shape=$shape N=$n erl_flag='${pflag}'"
(
  erl -noshell $pflag \
    -pa "$D/ebin" -pa "$DEPS/cowlib/ebin" -pa "$DEPS/ranch/ebin" -pa "$DEPS/cowboy/ebin" \
    -s probe_srv main "$DEPS" "$port" "$shape" "$run/ready" 900000 > "$run/server.log" 2>&1
) &
for i in $(seq 1 300); do [ -f "$run/ready" ] && break; sleep 0.05; done
if [ ! -f "$run/ready" ]; then
  echo "SERVER FAILED TO START"; cat "$run/server.log"; exit 1
fi
erl -noshell +P 400000 +Q 65536 +A 8 \
  -pa "$D/ebin" -pa "$DEPS/cowlib/ebin" -pa "$DEPS/gun/ebin" \
  -s probe_client main "$port" "$n" "$tmo" "$closek" "$ph2" "${8:-0}" "${9:-100}" > "$run/client.log" 2>&1
echo "client_exit=$?"
curl -s --max-time 5 "http://127.0.0.1:$port/stats/stop" >/dev/null
wait
echo "=== server.log ==="
cat "$run/server.log"
echo "=== client report ==="
grep -E "^(SERVER|CLIENT)" "$run/client.log"
echo "--- client non-report lines (first 12) ---"
grep -v -E "^(SERVER|CLIENT)" "$run/client.log" | head -12
