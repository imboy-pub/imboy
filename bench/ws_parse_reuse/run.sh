#!/usr/bin/env bash
# Explicit synthetic loopback benchmark; never invoked by normal EUnit.
set -euo pipefail
cd "$(dirname "$0")/../.."
BASE=${WS_PERF_BASE:-5941171b673b2167b77db80611d42d36c5d491f5}
OUT=${WS_PERF_OUT:-$(mktemp -d /tmp/ws-parse-bench.XXXXXX)}
mkdir -p "$OUT"; OUT=$(cd "$OUT" && pwd)
[[ ! -e "$OUT/.compiled" ]] || { echo 'Refusing existing compiled output' >&2; exit 1; }
mkdir -p "$OUT"/{baseline,candidate,runtime/ebin,ebin,logs}
FLAGS=(+debug_info +nowarn_unused_function +'{parse_transform,lager_transform}' -I include)
DEPS=(); for path in deps/*/ebin; do DEPS+=(-pa "$path"); done
for module in websocket_handler websocket_delivery_guard; do
 git show "$BASE:src/api/$module.erl" > "$OUT/baseline/$module.erl"
 erlc "${FLAGS[@]}" "${DEPS[@]}" -o "$OUT/baseline" "$OUT/baseline/$module.erl"
 erlc "${FLAGS[@]}" "${DEPS[@]}" -o "$OUT/candidate" "src/api/$module.erl"
done
for module in message_ds imboy_syn ack_retry_cache elib_metric imboy_codec imboy_frame auth_ds micro_community_access_ds micro_community_access_logic websocket_logic elib_log log_redact imboy_pb; do
 source=$(find src -name "$module.erl" -print -quit)
 erlc "${FLAGS[@]}" "${DEPS[@]}" -o "$OUT/runtime/ebin" "$source"
done
erlc +debug_info -I include "${DEPS[@]}" -o "$OUT/ebin" bench/ws_parse_reuse/ws_*.erl
sha256sum "$OUT/"{baseline,candidate}/*.beam "$OUT/runtime/ebin/"*.beam "$OUT/ebin/"*.beam > "$OUT/logs/beam-sha256.txt"
printf '%s\n' "$BASE" > "$OUT/logs/base-sha.txt"
touch "$OUT/.compiled"
export ERL_AFLAGS='+S 4:4 +A 4'
ARGS=("${DEPS[@]}" -pa ebin -pa "$OUT/runtime/ebin" -pa "$OUT/ebin" -noinput -boot no_dot_erlang -noshell)
case ${1:-short} in
 short) erl "${ARGS[@]}" -s ws_short main | tee "$OUT/logs/short.log" ;;
 retry) erl "${ARGS[@]}" -s ws_retry main | tee "$OUT/logs/retry.log" ;;
 stress)
  export WS_STRESS_SECONDS=${WS_STRESS_SECONDS:-600} WS_STRESS_BUSINESS_RATE=${WS_STRESS_BUSINESS_RATE:-500} WS_STRESS_STOP_FILE="$OUT/STOP"
  for variant in baseline candidate; do
   WS_STRESS_VARIANT=$variant erl "${ARGS[@]}" -s ws_stress main | tee "$OUT/logs/stress-$variant.log"
  done ;;
 *) echo 'Usage: run.sh short|retry|stress' >&2; exit 2 ;;
esac
printf 'Evidence: %s\n' "$OUT"
