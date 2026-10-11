#!/usr/bin/env bash
# Regression tests use an owned temporary Git repository, never a live runtime.
set -euo pipefail
umask 077
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
TASK_DIR="$(mktemp -d "${TMPDIR:-/tmp}/imboy-pack-whitelist.XXXXXX")"
trap 'rm -rf "$TASK_DIR"' EXIT
FIXTURE="$TASK_DIR/repo"
mkdir -p "$FIXTURE/scripts/deliver" "$FIXTURE/config" "$FIXTURE/src"
cp "$ROOT/scripts/deliver/make_delivery_pack.sh" "$FIXTURE/scripts/deliver/"
cp "$ROOT/scripts/deliver/backend_allowlist.regex" "$FIXTURE/scripts/deliver/"
git -C "$FIXTURE" init -q

commit_fixture() {
    git -C "$FIXTURE" add -- .
    git -C "$FIXTURE" -c user.name=leeyi -c user.email=leeyisoft@qq.com \
        commit -q -s -m "$1"
}

run_pack() {
    local name="$1"
    shift
    (cd "$FIXTURE" && bash scripts/deliver/make_delivery_pack.sh \
        --out-dir "$TASK_DIR/$name" "$@") > "$TASK_DIR/$name.log" 2>&1
}

expect_member() {
    grep -qxF "$2" "$TASK_DIR/$1/manifest.txt" || {
        echo "FAIL: missing allowed path $2" >&2; exit 1;
    }
}

expect_absent() {
    if grep -qxF "$2" "$TASK_DIR/$1/manifest.txt"; then
        echo "FAIL: unexpected path $2" >&2; exit 1
    fi
}

mkdir -p "$FIXTURE/docs/product" "$FIXTURE/docs/brand" \
    "$FIXTURE/docs/brand/logo" \
    "$FIXTURE/docs/design" "$FIXTURE/deploy/nginx/templates" \
    "$FIXTURE/deploy/nginx/prod-vhosts" "$FIXTURE/unknown-new-directory"
printf '{"profile":"full-selected"}\n' > "$FIXTURE/config/product-feature-manifest.json"
printf 'committed source\n' > "$FIXTURE/src/example.erl"
printf 'approved product guide\n' > "$FIXTURE/docs/product/example.md"
printf 'deployment entry\n' > "$FIXTURE/deploy/README.md"
printf 'public template\n' > "$FIXTURE/deploy/nginx/templates/example.conf"
printf 'debug configuration\n' > "$FIXTURE/config/debug.config"
printf 'brand media\n' > "$FIXTURE/docs/brand/example.svg"
printf 'entry logo\n' > "$FIXTURE/docs/brand/logo/imboy_logo_2474E5_0512.png"
printf 'optional uptrace compose\n' > "$FIXTURE/deploy/docker-compose.uptrace.yml"
printf 'execution plan\n' > "$FIXTURE/docs/design/example.md"
printf 'production snapshot\n' > "$FIXTURE/deploy/nginx/prod-vhosts/example.conf"
printf 'unknown content\n' > "$FIXTURE/unknown-new-directory/example.txt"
commit_fixture 'test fixture: committed full profile'
if run_pack default-full --no-tar; then
    echo 'FAIL: default accepted full-selected profile' >&2; exit 1
fi
grep -q 'profile mismatch' "$TASK_DIR/default-full.log"
run_pack explicit-full --no-tar --require-profile full-selected
expect_absent explicit-full config/debug.config
printf '{"profile":"phase1_delivery"}\n' > "$FIXTURE/config/product-feature-manifest.json"
commit_fixture 'test fixture: committed phase1 profile'
printf 'dirty source must not enter snapshot\n' > "$FIXTURE/src/example.erl"
printf 'untracked file\n' > "$FIXTURE/src/untracked.erl"
run_pack phase1
for path in src/example.erl docs/product/example.md deploy/README.md \
    deploy/nginx/templates/example.conf scripts/deliver/backend_allowlist.regex \
    docs/brand/logo/imboy_logo_2474E5_0512.png deploy/docker-compose.uptrace.yml; do
    expect_member phase1 "$path"
done
for path in config/debug.config docs/brand/example.svg docs/design/example.md \
    deploy/nginx/prod-vhosts/example.conf unknown-new-directory/example.txt src/untracked.erl; do
    expect_absent phase1 "$path"
done
grep -qx 'committed source' "$TASK_DIR/phase1/.work/source/src/example.erl"
grep -qx 'dirty source must not enter snapshot' "$FIXTURE/src/example.erl"
PACK_TAR="$(find "$TASK_DIR/phase1" -maxdepth 1 -name '*.tar' -print)"
tar -xOf "$PACK_TAR" src/example.erl | grep -qx 'committed source'
test "$(tar -tf "$PACK_TAR" | wc -l | tr -d ' ')" = \
    "$(wc -l < "$TASK_DIR/phase1/manifest.txt" | tr -d ' ')"
if run_pack wrong-profile --no-tar --require-profile nonexistent; then
    echo 'FAIL: accepted wrong profile' >&2; exit 1
fi
if (cd "$FIXTURE" && bash scripts/deliver/make_delivery_pack.sh \
    --out-dir "$FIXTURE/nested-output" --no-tar) > "$TASK_DIR/nested.log" 2>&1; then
    echo 'FAIL: accepted repository-local output' >&2; exit 1
fi
echo 'PASS: default profile, positive categories, exclusions, immutable bytes, tar and output boundary'
