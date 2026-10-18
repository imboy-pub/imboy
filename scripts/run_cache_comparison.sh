#!/usr/bin/env bash
# Run identical already-compiled drivers against isolated baseline/candidate ebins.
# Dependencies and OTP must already be installed. No DB or deployment changes.
set -euo pipefail
: "${CACHE_BASELINE_DIR:?absolute baseline directory with ebin and deps required}"
: "${CACHE_CANDIDATE_DIR:?absolute candidate directory with ebin and deps required}"
: "${CACHE_RESULTS_DIR:?absolute evidence output directory required}"
mkdir -p "$CACHE_RESULTS_DIR"
export ERL_EPMD_PORT="${ERL_EPMD_PORT:-43729}"
epmd -daemon -address 127.0.0.1
# NODES KEYS VALUE_BYTES WORKLOAD. Override the list for a focused rerun.
profiles=${CACHE_PROFILES:-'1 10000 64 read
1 10000 64 write
1 100000 1024 read
2 10000 1024 mixed
3 50000 64 mixed'}
while read -r nodes keys bytes workload; do
  [[ -n "$nodes" ]] || continue
  total_schedulers=${CACHE_TOTAL_SCHEDULERS:-$(nproc)}
  schedulers=$((total_schedulers / nodes))
  ((schedulers >= 1)) || schedulers=1
  for arm in baseline candidate; do
    if [[ "$arm" == baseline ]]; then root=$CACHE_BASELINE_DIR; else root=$CACHE_CANDIDATE_DIR; fi
    stem="$CACHE_RESULTS_DIR/$arm-${nodes}n-${keys}keys-${bytes}b-$workload"
    ERL_AFLAGS="+S $schedulers:$schedulers" CACHE_STRESS_NODES="$nodes" \
      CACHE_STRESS_PEER_SCHEDULERS="$schedulers" CACHE_STRESS_KEYS="$keys" \
      CACHE_STRESS_BYTES="$bytes" CACHE_STRESS_WORKLOAD="$workload" \
      CACHE_STRESS_MODE=replicate CACHE_STRESS_SECONDS="${CACHE_SECONDS:-10}" \
      CACHE_STRESS_WORKERS="${CACHE_WORKERS:-32}" CACHE_STRESS_CAPACITY=enlarged \
      CACHE_STRESS_OUTPUT="$stem.json" \
      erl -setcookie imboy_cache_isolated_bench -noshell \
        -pa "$root/ebin" "$root"/deps/*/ebin -s cache_stress_bench run >"$stem.log" 2>&1
  done
done <<< "$profiles"
