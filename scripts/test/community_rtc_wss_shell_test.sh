#!/usr/bin/env bash
# Run the actual container regression with a local Docker stub, stopping before
# networking. This tests CID/error/cleanup control flow, never TLS or WSS.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf -- "$TMP"' EXIT
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
mkdir -p "$TMP/bin" "$TMP/runtime"
cat > "$TMP/model.json" <<'JSON'
{"services":{"imboy_nginx":{"image":"nginx@sha256:fixture","command":["/bin/sh","-c","sleep 0 & wait $${!}"],"environment":{"NGINX_ENVSUBST_FILTER":"^API_DOMAIN$$","LITERAL_DOLLARS":"$$$$","LITERAL_VARIABLE":"$$RTC_TEST_LITERAL"},"volumes":[]},"imboy_livekit":{"image":"livekit@sha256:fixture","environment":{"LIVEKIT_CONFIG":"port: 7880\n# fixture: $$$$"}}}}
JSON
cat > "$TMP/bin/docker" <<'DOCKER'
#!/usr/bin/env bash
set -euo pipefail
STATE="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
[[ "${1:-}" == --host=unix:///var/run/docker.sock ]] || exit 90
shift
case "${1:-}" in
  info) exit 0 ;;
  compose)
    [[ "${2:-}" != version ]] || exit 0
    cat "$STATE/model.json" ;;
  network)
    printf 'network %s\n' "$2" >> "$STATE/events" ;;
  run)
    shift
    args=("$@")
    cidfile=
    while [[ $# -gt 0 ]]; do
      if [[ "$1" == --cidfile ]]; then cidfile=$2; break; fi
      shift
    done
    [[ -n "$cidfile" ]] || exit 91
    role="${cidfile##*/}"
    case "$role" in upstream.cid) number=1 ;; livekit.cid) number=2 ;; nginx.cid) number=3 ;; *) exit 92 ;; esac
    printf -v id '%064d' "$number"
    # Match Docker CLI's WriteString(id): exactly 64 bytes, no newline.
    printf '%s' "$id" > "$cidfile"
    printf '%s\n' "${cidfile%/*}" > "$STATE/runtime-dir"
    printf 'run %s\n' "$role" >> "$STATE/events"
    if [[ "$(<"$STATE/mode")" == partial-start && "$role" == livekit.cid ]]; then exit 125; fi
    if [[ "$role" == nginx.cid ]]; then
      # Execute the synthetic Compose command with a real POSIX shell. A leaked
      # $$ expands to its PID and makes wait fail, just as in the real CI job.
      count=${#args[@]}
      "${args[count-3]}" "${args[count-2]}" "${args[count-1]}"
      fixture_dir="${cidfile%/*}"
      grep -Fxq 'NGINX_ENVSUBST_FILTER=^API_DOMAIN$' "$fixture_dir/nginx.env"
      grep -Fxq 'LITERAL_DOLLARS=$$' "$fixture_dir/nginx.env"
      grep -Fxq 'LITERAL_VARIABLE=$RTC_TEST_LITERAL' "$fixture_dir/nginx.env"
      grep -Fxq '# fixture: $$' "$fixture_dir/livekit.yaml"
      printf 'compose runtime values decoded once\n' >> "$STATE/events"
    fi
    printf '%s\n' "$id" ;;
  port)
    printf 'port %s %s\n' "$2" "$3" >> "$STATE/events"
    exit 42 ;;
  logs)
    printf '%s\n' "$*" >> "$STATE/events"
    printf 'fixture container log\n'
    exit 9 ;; # Diagnostics can fail; cleanup must still run and retain status.
  inspect)
    printf 'inspect %s\n' "${@: -1}" >> "$STATE/events"
    exit 9 ;;
  rm)
    printf '%s\n' "$*" >> "$STATE/events" ;;
  *) printf 'Unexpected Docker stub call: %s\n' "$1" >&2; exit 93 ;;
esac
DOCKER
chmod +x "$TMP/bin/docker"
for scenario in after-start partial-start; do
  printf '%s\n' "$scenario" > "$TMP/mode"
  : > "$TMP/events"
  rc=0
  # A separate Bash process retains its own errexit even when the parent records
  # its exit status. Only Docker is replaced; no production functions are copied.
  PATH="$TMP/bin:$PATH" TMPDIR="$TMP/runtime" RTC_TEST_LITERAL=must-not-expand \
    bash "$ROOT/scripts/test/community_rtc_wss_test.sh" --docker > "$TMP/output" 2>&1 || rc=$?
  if [[ "$scenario" == after-start ]]; then expected_rc=42; count=3; else expected_rc=125; count=2; fi
  if [[ "$rc" -ne "$expected_rc" ]]; then
    cat "$TMP/output" >&2
    fail "$scenario: expected injected exit $expected_rc, got $rc"
  fi
  if [[ "$scenario" == after-start ]]; then
    printf -v nginx_id '%064d' 3
    grep -Fxq "port $nginx_id 80/tcp" "$TMP/events" || fail 'No-newline Nginx CID stopped port lookup'
    grep -Fxq 'compose runtime values decoded once' "$TMP/events" || fail 'Compose serialization leaked into runtime values'
  else
    ! grep -q '^run nginx.cid$' "$TMP/events" || fail 'Nginx started after the injected LiveKit failure'
  fi
  for ((number=1; number<=count; number++)); do
    printf -v id '%064d' "$number"
    grep -Fxq "inspect $id" "$TMP/events" || fail "Missing state diagnostics for container $number"
    grep -Fxq "logs --tail 30 $id" "$TMP/events" || fail "Missing diagnostics for container $number"
    grep -Fxq "rm -f $id" "$TMP/events" || fail "Container $number was not cleaned up"
  done
  grep -Fxq 'network rm' "$TMP/events" || fail 'Disposable network was not cleaned up'
  [[ ! -e "$(<"$TMP/runtime-dir")" ]] || fail 'Private fixture directory was not removed'
  grep -Fq "exited with status $expected_rc" "$TMP/output" || fail 'Original exit status was not diagnosed'
  printf '[PASS] %s: no-newline CIDs, diagnostics failure, cleanup and exit %s preserved\n' "$scenario" "$expected_rc"
done
printf '\n2 PASS; Compose decoding and Docker stub control flow only. Container/TLS/WSS results are NOT verified.\n'
