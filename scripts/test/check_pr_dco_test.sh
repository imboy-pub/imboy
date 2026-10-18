#!/usr/bin/env bash
# 临时 Git 仓库回归：真实提交必须签署，GitHub 合成 merge 不在 PR 范围内。
# Temporary-repository regression for the exact PR commit range and author match.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CHECKER="$ROOT/scripts/check_pr_dco.sh"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/imboy-pr-dco.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT
export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE
unset GIT_AUTHOR_NAME GIT_AUTHOR_EMAIL GIT_AUTHOR_DATE GIT_COMMITTER_NAME GIT_COMMITTER_EMAIL GIT_COMMITTER_DATE
git init -q "$TMP/repo"
cd "$TMP/repo"
git config user.name 'DCO Fixture'
git config user.email 'fixture@example.invalid'
git config commit.gpgsign false
mkdir "$TMP/hooks"
git config core.hooksPath "$TMP/hooks"

pass=0
expect_ok() {
  local label="$1" base="$2" head="$3" count="$4"
  if ! PR_BASE_SHA="$base" PR_HEAD_SHA="$head" bash "$CHECKER" > "$TMP/output" 2>&1; then
    cat "$TMP/output"; echo "FAIL: $label" >&2; exit 1
  fi
  grep -qF "DCO passed for $count PR commits" "$TMP/output"
  pass=$((pass + 1)); echo "PASS: $label"
}
expect_fail() {
  local label="$1" base="$2" head="$3"
  if PR_BASE_SHA="$base" PR_HEAD_SHA="$head" bash "$CHECKER" > "$TMP/output" 2>&1; then
    cat "$TMP/output"; echo "FAIL: $label unexpectedly passed" >&2; exit 1
  fi
  pass=$((pass + 1)); echo "PASS: $label"
}

git commit -q --allow-empty -m 'Existing base, outside PR range'
base="$(git rev-parse HEAD)"
git checkout -qb contribution
git commit -q --allow-empty -s -m 'Signed contributor commit'
signed="$(git rev-parse HEAD)"
git -c user.name='Dependency Bot' -c user.email='49699333+dependabot[bot]@users.noreply.github.com' \
  commit -q --allow-empty -s -m 'Signed bot commit'
head="$(git rev-parse HEAD)"
git checkout -qb target "$base"
git commit -q --allow-empty -s -m 'Target branch advanced'
target="$(git rev-parse HEAD)"
git merge -q --no-ff --no-edit contribution
synthetic="$(git rev-parse HEAD)"
[[ "$synthetic" != "$head" ]]
! git log -1 --format='%b' | grep -qi '^signed-off-by:'
expect_ok 'unsigned synthetic checkout merge excluded' "$target" "$head" 2
expect_ok 'signed real contributor accepted' "$base" "$signed" 1
expect_ok 'literal bot metacharacter email accepted' "$signed" "$head" 1
expect_fail 'unsigned merge included when it is the real PR head' "$target" "$synthetic"

git checkout -qb unsigned "$head"
git commit -q --allow-empty -m 'Unsigned real contributor commit'
unsigned="$(git rev-parse HEAD)"
expect_fail 'unsigned real contributor rejected' "$target" "$unsigned"
git commit -q --allow-empty -s -m 'Signed tip after unsigned ancestor'
expect_fail 'unsigned ancestor rejected despite signed PR tip' "$target" "$(git rev-parse HEAD)"
git checkout -qb wrong-email "$head"
git -c user.name='Dependency Bot' -c user.email='49699333+dependabot[bot]@users.noreply.github.com' \
  commit -q --allow-empty -m 'Wrong literal signoff' \
  -m 'Signed-off-by: Dependency Bot <49699333+dependabotb@usersXnoreplyXgithubXcom>'
expect_fail 'lookalike bot email rejected' "$head" "$(git rev-parse HEAD)"
expect_fail 'missing base rejected' '' "$head"
expect_fail 'missing head rejected' "$base" ''
expect_fail 'symbolic head rejected' "$base" HEAD
expect_fail 'invalid base rejected' main "$head"
expect_fail 'missing object rejected' "$base" 1111111111111111111111111111111111111111
expect_fail 'missing base object rejected' 1111111111111111111111111111111111111111 "$head"
blob="$(printf 'not a commit' | git hash-object -w --stdin)"
expect_fail 'non-commit object rejected' "$base" "$blob"
expect_fail 'empty PR range rejected' "$head" "$head"
echo "All $pass PR DCO regression checks passed."
