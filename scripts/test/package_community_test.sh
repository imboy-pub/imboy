#!/usr/bin/env bash
# Offline export/extraction tests in a disposable Git fixture. No Docker or network.
set -euo pipefail
REPO="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
for tool in git zip unzip sha256sum stat cmp find sort realpath mktemp; do
    command -v "$tool" >/dev/null 2>&1 || { printf '[FAIL] Missing test tool: %s\n' "$tool" >&2; exit 1; }
done
test_dir=$(mktemp -d "${TMPDIR:-/tmp}/imboy-package-test.XXXXXX")
trap 'rm -rf -- "$test_dir"' EXIT
fixture="$test_dir/source with spaces"
mkdir -p "$fixture"
passes=0
failures=0
check() {
    local label=$1
    shift
    if "$@" > "$test_dir/last-check.log" 2>&1; then
        printf '[PASS] %s\n' "$label"
        passes=$((passes + 1))
    else
        printf '[FAIL] %s\n' "$label"
        cat "$test_dir/last-check.log"
        failures=$((failures + 1))
    fi
}
fails() { ! "$@" > "$test_dir/expected-failure.log" 2>&1; }
fixture_commit() {
    GIT_AUTHOR_DATE=2026-01-01T00:00:00Z GIT_COMMITTER_DATE=2026-01-01T00:00:00Z \
        git -C "$fixture" -c user.name='Package test' -c user.email='package-test@example.invalid' \
        -c commit.gpgsign=false commit -qm "$1"
}

# Snapshot candidate source files so subsequent cases cannot affect the caller's checkout.
sources=(scripts/package_community.sh deploy/standalone/files.tsv)
while IFS=$'\t' read -r source target extra; do
    [[ -n "$source" && "$source" != \#* ]] || continue
    [[ -f "$REPO/$source" && ! -L "$REPO/$source" ]] \
        || { printf '[FAIL] Missing candidate source: %s\n' "$source" >&2; exit 1; }
    sources+=("$source")
done < "$REPO/deploy/standalone/files.tsv"
for source in "${sources[@]}"; do
    mkdir -p "$fixture/$(dirname -- "$source")"
    cp -p -- "$REPO/$source" "$fixture/$source"
done
mkdir -p "$fixture/priv/migrations" "$fixture/src"
printf '%s\n' '-- Synthetic migration identity; never executed.' > "$fixture/priv/migrations/00000001_fixture.up.sql"
printf '%s\n' '-- Another synthetic identity.' > "$fixture/priv/migrations/00000003_fixture.up.sql"
printf '%s\n' '-- Excluded down migration identity.' > "$fixture/priv/migrations/00000001_fixture.down.sql"
printf '%s\n' 'not part of the customer distribution' > "$fixture/src/not-shipped.erl"
printf '%s\n' 'stream {}' > "$fixture/deploy/nginx/templates/livekit-l4-sni-stream.conf.template"
printf '%s\n' 'rule_files: []' > "$fixture/deploy/prometheus/rules/imboy-alerts-test.yml"
git -C "$fixture" init -q
git -C "$fixture" add -- "${sources[@]}" priv/migrations src/not-shipped.erl \
    deploy/nginx/templates/livekit-l4-sni-stream.conf.template \
    deploy/prometheus/rules/imboy-alerts-test.yml
fixture_commit 'Standalone package fixture'
commit=$(git -C "$fixture" rev-parse HEAD)
builder="$fixture/scripts/package_community.sh"
version=$(cat "$fixture/VERSION")
name="IMBoy_Community_Standalone_${version}_${commit:0:12}"

# These are intentionally untracked fake credentials/state, never real secrets.
mkdir -p "$fixture/deploy/data/pg18"
printf '%s\n' 'TEST_ONLY_UNTRACKED_CREDENTIAL=do-not-package' > "$fixture/deploy/.env"
printf '%s\n' 'do-not-package' > "$fixture/deploy/data/pg18/sentinel"
printf '\n# Uncommitted work must not enter a fixed-ref package.\n' >> "$fixture/deploy/.env.example"
first="$test_dir/first export.zip"
second="$test_dir/second export.zip"
check 'Build from the committed source with untracked .env/data present' bash "$builder" --output "$first"
[[ -f "$first" ]] || { printf '[FAIL] Cannot inspect a missing archive\n'; exit 1; }
check 'ZIP structure is valid' unzip -tq "$first"
check 'A second export of the same ref succeeds' bash "$builder" --ref "$commit" --output "$second"
check 'Same commit produces identical ZIP bytes' cmp -s "$first" "$second"
extraction="$test_dir/extracted with spaces"
mkdir -p "$extraction"
unzip -q "$first" -d "$extraction"
package="$extraction/$name"
check 'Extracted package validates from an unrelated current directory' bash "$package/scripts/check_delivery.sh"
check 'Read-only entry succeeds without a customer configuration' bash "$package/install.sh" --check
check 'Shell executable permissions survive ZIP extraction' test "$(stat -c '%a' "$package/install.sh")" = 755
check 'Required sibling scripts/sanity_check.sh is executable' test -x "$package/scripts/sanity_check.sh"
git -C "$fixture" show "$commit:deploy/.env.example" > "$test_dir/expected.env.example"
check 'Export uses committed bytes instead of a dirty working tree' cmp -s "$test_dir/expected.env.example" "$package/deploy/.env.example"
check 'SOURCE_COMMIT identifies the selected immutable ref' test "$(cat "$package/SOURCE_COMMIT")" = "$commit"
check 'Only up migration identities are recorded' test "$(wc -l < "$package/review/SOURCE_MIGRATIONS.tsv")" -eq 3
no_runtime_files() {
    [[ ! -e "$package/deploy/.env" && ! -e "$package/deploy/data" \
       && ! -e "$package/.git" && ! -e "$package/src" && ! -e "$package/priv" \
       && ! -e "$package/.delivery-install.lock" ]]
}
check 'No customer state, application source or installation side effects are shipped' no_runtime_files
check 'Host stream template is excluded from HTTP templates' test ! -e "$package/deploy/nginx/templates/livekit-l4-sni-stream.conf.template"
check 'Prometheus test definitions are excluded from runtime rules' test ! -e "$package/deploy/prometheus/rules/imboy-alerts-test.yml"

original_hash=$(sha256sum "$first")
check 'Existing output is rejected' fails bash "$builder" --output "$first"
check 'Rejected overwrite preserves the existing archive' test "$(sha256sum "$first")" = "$original_hash"
ln -s "$test_dir/no-target.zip" "$test_dir/dangling.zip"
check 'An existing output symlink is rejected' fails bash "$builder" --output "$test_dir/dangling.zip"
check 'Rejecting a symlink creates no destination archive' test ! -e "$test_dir/no-target.zip"

cp -p "$package/scripts/sanity_check.sh" "$test_dir/sanity.backup"
rm "$package/scripts/sanity_check.sh"
check 'Missing sanity script is rejected after extraction' fails bash "$package/scripts/check_delivery.sh"
cp -p "$test_dir/sanity.backup" "$package/scripts/sanity_check.sh"
printf '\n# altered\n' >> "$package/scripts/sanity_check.sh"
check 'Changed dependency bytes are rejected' fails bash "$package/scripts/check_delivery.sh"
cp -p "$test_dir/sanity.backup" "$package/scripts/sanity_check.sh"
chmod 644 "$package/install.sh"
check 'Lost executable permissions are rejected' fails bash "$package/scripts/check_delivery.sh"
chmod 755 "$package/install.sh"
printf '%s\n' 'unexpected template' > "$package/deploy/nginx/templates/unreviewed.conf.template"
check 'Unexpected Nginx template is rejected' fails bash "$package/scripts/check_delivery.sh"
rm "$package/deploy/nginx/templates/unreviewed.conf.template"
printf '\nOperator evidence: NOT_RUN\n' >> "$package/records/SITE_INPUTS.md"
check 'Customer record updates do not invalidate immutable file checks' bash "$package/scripts/check_delivery.sh"

git -C "$fixture" rm -q -- scripts/sanity_check.sh
fixture_commit 'Remove a required fixture dependency'
check 'Missing dependency in the selected commit rejects the build' fails bash "$builder" --output "$test_dir/incomplete.zip"
check 'Failed build leaves no output archive' test ! -e "$test_dir/incomplete.zip"
check 'Explicit older ref remains usable after a newer incomplete commit' bash "$builder" --ref "$commit" --output "$test_dir/older-ref.zip"
check 'Ref selection is independent of the current checkout' cmp -s "$first" "$test_dir/older-ref.zip"

printf '\nPackage regression: %s PASS, %s FAIL\n' "$passes" "$failures"
printf 'Scope: local Git export, ZIP contents, extraction and integrity guards only.\n'
[[ "$failures" -eq 0 ]]
