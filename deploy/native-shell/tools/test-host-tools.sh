#!/usr/bin/env bash
# Offline synthetic host-dependency contracts. Never runs real apt-get.
set -Eeuo pipefail
umask 077
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
. "$TOOLS_DIR/lib-payload.sh"
. "$TOOLS_DIR/lib-host-packages.sh"
scratch=$(mktemp -d /tmp/imboy-host-test.XXXXXXXX)
trap 'rm -rf -- "$scratch"' EXIT
kit=$scratch/kit
mkdir -p "$kit/tools" "$kit/metadata" "$kit/payload/postgresql/lib" "$scratch/bin" "$scratch/state"
cp "$TOOLS_DIR/"{prepare-host.sh,lib-payload.sh,lib-host-packages.sh} "$kit/tools/"
payload_os
cat > "$kit/metadata/release.env" <<EOF
PAYLOAD_READY=yes
OS_ID=$PAYLOAD_OS_ID
OS_VERSION_ID=$PAYLOAD_OS_VERSION_ID
ARCH=$PAYLOAD_ARCH
EOF
{
  printf 'package\tversion\tarchitecture\n'
  for pkg in "${HOST_BASE_PACKAGES[@]}"; do
    arch=amd64; [[ $pkg != ca-certificates ]] || arch=all
    printf '%s\t1.0\t%s\n' "$pkg" "$arch"
  done
} > "$kit/metadata/host-runtime-packages.tsv"
cp "$kit/metadata/host-runtime-packages.tsv" "$scratch/state/lock.tsv"
printf 'fixture only\n' > "$kit/payload/postgresql/lib/probe.so"
export IMBOY_HOST_TEST_STATE=$scratch/state
export IMBOY_HOST_TEST_ROOT=$kit
export IMBOY_HOST_TEST_REAL_FILE
IMBOY_HOST_TEST_REAL_FILE=$(command -v file)
cat > "$scratch/bin/dpkg-query" <<'EOF'
#!/usr/bin/env bash
set -eu
[[ $1 == -W ]] || exit 77
query=${!#}; name=${query%:*}
row=$(awk -F '\t' -v name="$name" '$1==name {print}' "$IMBOY_HOST_TEST_STATE/lock.tsv")
[[ -n $row ]] || exit 1
IFS=$'\t' read -r pkg version arch <<< "$row"
[[ $query != *:* || ${query#*:} == "$arch" ]] || exit 1
mode=$(cat "$IMBOY_HOST_TEST_STATE/mode")
if [[ $pkg == curl && ! -f $IMBOY_HOST_TEST_STATE/installed ]]; then
  case "$mode" in
    missing|unavailable|extra|remove) exit 1;;
    newer) version=2.0;;
    older) version=0.5;;
  esac
fi
printf '%s\t%s\t%s\tinstalled\n' "$pkg" "$version" "$arch"
EOF
cat > "$scratch/bin/apt-cache" <<'EOF'
#!/usr/bin/env bash
set -eu
[[ $1 == madison ]] || exit 77
mode=$(cat "$IMBOY_HOST_TEST_STATE/mode")
[[ $mode != unavailable ]] || exit 0
printf '%s | 1.0 | fixture-only\n' "$2"
EOF
cat > "$scratch/bin/apt-get" <<'EOF'
#!/usr/bin/env bash
set -eu
printf '%s\n' "$*" >> "$IMBOY_HOST_TEST_STATE/actions"
mode=$(cat "$IMBOY_HOST_TEST_STATE/mode")
case "$1" in
  update) exit 0;;
  --simulate)
    printf 'Inst curl (1.0 fixture-only [amd64])\n'
    case "$mode" in
      extra) printf 'Inst unlisted-dependency (1.0 fixture-only [amd64])\n';;
      remove) printf 'Remv unrelated-package [1.0]\n';;
    esac
    exit 0;;
  --yes)
    [[ " $* " == *' --no-remove '* && " $* " == *' curl:amd64=1.0 '* ]] || exit 77
    touch "$IMBOY_HOST_TEST_STATE/installed"; exit 0;;
  *) exit 77;;
esac
EOF
cat > "$scratch/bin/file" <<'EOF'
#!/usr/bin/env bash
[[ ${!#} != *'/probe.so' ]] || { printf 'ELF 64-bit LSB shared object, fixture\n'; exit 0; }
exec "$IMBOY_HOST_TEST_REAL_FILE" "$@"
EOF
cat > "$scratch/bin/ldd" <<'EOF'
#!/usr/bin/env bash
[[ $1 == *'/probe.so' ]] || exit 77
if [[ ${LD_LIBRARY_PATH:-} == "$IMBOY_HOST_TEST_ROOT/payload/postgresql/lib"* ]]; then
  printf 'libpq.so.5 => %s/payload/postgresql/lib/libpq.so.5 (fixture)\n' "$IMBOY_HOST_TEST_ROOT"
else
  printf 'libpq.so.5 => not found\n'; exit 1
fi
EOF
cat > "$scratch/bin/id" <<'EOF'
#!/usr/bin/env bash
[[ $1 == -u ]] || exit 77
printf '0\n'
EOF
chmod 755 "$scratch/bin/"*
payload_hash_tree "$kit" CHECKSUMS.sha256
passed=0
pass() { passed=$((passed+1)); printf 'PASS %s\n' "$1"; }
reset_state() { printf '%s\n' "$1" > "$scratch/state/mode"; rm -f "$scratch/state/installed" "$scratch/state/actions"; }
run_host() { PATH="$scratch/bin:$PATH" bash "$kit/tools/prepare-host.sh" "$@" > "$scratch/result.txt" 2>&1; }
fail_expected() { if run_host "$1"; then cat "$scratch/result.txt" >&2; exit 1; fi; }
reset_state ready
run_host --check
[[ ! -f $scratch/state/actions ]]
grep -q HOST_PACKAGE_CHECK=PASS "$scratch/result.txt"
pass 'host-ready-check-and-bundled-pg-library-path'
reset_state missing
rc=0; run_host --check || rc=$?
[[ $rc == 3 && ! -f $scratch/state/actions ]]
pass 'host-missing-check-read-only'
for state in missing older; do
  reset_state "$state"
  run_host --apply
  grep -q '^update$' "$scratch/state/actions"
  grep -q '^--simulate .*--no-remove install' "$scratch/state/actions"
  grep -q '^--yes .*--no-remove .*curl:amd64=1.0' "$scratch/state/actions"
  [[ -f $scratch/state/installed ]]
  pass "host-$state-apply-exact-lock"
done
reset_state newer
fail_expected --apply
[[ ! -f $scratch/state/actions ]]
grep -q BLOCKED_NEWER_VERSION "$scratch/result.txt"
pass 'host-newer-refuses-before-apt-update'
for state in unavailable extra remove; do
  reset_state "$state"
  fail_expected --apply
  [[ ! -f $scratch/state/installed ]]
  ! grep -q '^--yes ' "$scratch/state/actions"
  pass "host-$state-refuses-before-install"
done
reset_state ready
printf 'curl\t1.0\tamd64\n' >> "$kit/metadata/host-runtime-packages.tsv"
payload_hash_tree "$kit" CHECKSUMS.sha256
fail_expected --check
grep -q '重复包' "$scratch/result.txt"
pass 'host-duplicate-lock-refused'
cp "$scratch/state/lock.tsv" "$kit/metadata/host-runtime-packages.tsv"
payload_hash_tree "$kit" CHECKSUMS.sha256
printf 'tampered\n' >> "$kit/metadata/host-runtime-packages.tsv"
fail_expected --check
[[ ! -f $scratch/state/actions ]]
pass 'host-tampered-lock-refused'
HOST_NATIVE_ARCH=$(dpkg --print-architecture)
libc=$(ldd /usr/bin/bash | awk '/libc\.so.*=> \// {print $3; exit}')
owner=$(host_owner_of_library "$libc")
[[ $owner == libc6:amd64 ]]
pass 'host-real-read-only-library-package-mapping'
printf 'unowned fixture\n' > "$scratch/unowned.so"
if (host_owner_of_library "$scratch/unowned.so") > "$scratch/result.txt" 2>&1; then exit 1; fi
pass 'host-unowned-library-is-engineering-blocker'
printf 'TOTAL_PASS=%s\nSCOPE=offline_synthetic_contracts_and_read_only_system_package_queries\n' "$passed"
