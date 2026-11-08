#!/usr/bin/env bash
# Golden 镜像参数回归；在 Docker 前置处故意失败，不执行安装或发布。
# Exercise the real CLI; a failing Docker dependency stops before clone/deploy.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/bin"

cat > "$TMP/bin/docker" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$GOLDEN_TEST_DOCKER_LOG"
exit 99
EOF
# A regression must not accidentally start a network clone in this offline test.
printf '#!/usr/bin/env bash\nexit 99\n' > "$TMP/bin/git"
chmod +x "$TMP/bin/docker" "$TMP/bin/git"

# Observe parsed values without adding a production bypass or changing its gates.
cat > "$TMP/observe.sh" <<'EOF'
trap 'printf "OBSERVED_STAGE=%s\nOBSERVED_WIDGET=%s\nOBSERVED_FROM_REF=%s\nOBSERVED_FROM_IMAGE=%s\n" "${CURRENT_STAGE:-arg}" "${WIDGET_IMAGE_REF:-}" "${FROM_REF:-}" "${FROM_IMAGE:-}"' EXIT
EOF

digest="sha256:$(printf '%064d' 0)"
backend="ghcr.io/imboy-pub/imboy-backend@$digest"
widget="ghcr.io/imboy-pub/imboy-widget@$digest"
checks=0
fail() { printf 'FAIL: %s\n' "$*" >&2; cat "$TMP/output" >&2; exit 1; }

run_case() {
  local script="$1" stage="$2" rc=0
  shift 2
  : > "$TMP/docker.calls"
  PATH="$TMP/bin:$PATH" BASH_ENV="$TMP/observe.sh" \
    GOLDEN_TEST_DOCKER_LOG="$TMP/docker.calls" \
    bash "$ROOT/scripts/$script" \
      --profile ci --admin-phone 13800138000 --admin-password CI_Only_Test_1234 \
      --workdir "$TMP/work" "$@" > "$TMP/output" 2>&1 || rc=$?
  [ "$rc" -eq 1 ] || fail "$script: expected the real failure path, got $rc"
  grep -Fxq "OBSERVED_STAGE=$stage" "$TMP/output" || fail "$script: unexpected stage"
  if [ "$stage" = arg ]; then
    [ ! -s "$TMP/docker.calls" ] || fail "$script: invalid argument reached Docker"
  else
    grep -Fxq 'info' "$TMP/docker.calls" || fail "$script: cleanroom Docker gate not reached"
  fi
  checks=$((checks + 1))
}

for script in golden_install.sh golden_upgrade.sh; do
  if [ "$script" = golden_install.sh ]; then
    args=(--image-ref "$backend" --git-ref v1.0.0-alpha.84)
  else
    args=(--to-image "$backend" --to-ref v1.0.0-alpha.84 --from v1.2.3)
  fi

  run_case "$script" cleanroom-assert "${args[@]}" --widget-image-ref "$widget"
  grep -Fxq "OBSERVED_WIDGET=$widget" "$TMP/output" || fail 'Widget digest lost'
  if [ "$script" = golden_upgrade.sh ]; then
    grep -Fxq 'OBSERVED_FROM_REF=v1.2.3' "$TMP/output" || fail 'Git tag changed'
    grep -Fxq 'OBSERVED_FROM_IMAGE=ghcr.io/imboy-pub/imboy-backend:1.2.3' "$TMP/output" \
      || fail 'Image tag must omit the Git v prefix'
  fi

  run_case "$script" cleanroom-assert "${args[@]}" "--widget-image-ref=$widget"
  grep -Fxq "OBSERVED_WIDGET=$widget" "$TMP/output" || fail 'Equals option lost'
  run_case "$script" cleanroom-assert "${args[@]}"
  grep -Fxq 'OBSERVED_WIDGET=' "$TMP/output" || fail 'Optional default changed'
  run_case "$script" arg "${args[@]}" --widget-image-ref ghcr.io/imboy-pub/imboy-widget:1.0.0-alpha.84
  run_case "$script" arg "${args[@]}" --widget-image-ref ghcr.io/imboy-pub/imboy-widget@sha256:abc
  run_case "$script" arg "${args[@]}" --widget-image-ref=
  run_case "$script" arg "${args[@]}" --widget-image-ref
done

run_case golden_upgrade.sh cleanroom-assert \
  --to-image "$backend" --to-ref v1.0.0-alpha.84 --from v1.0.0-alpha.83
grep -Fxq 'OBSERVED_FROM_IMAGE=ghcr.io/imboy-pub/imboy-backend:1.0.0-alpha.83' "$TMP/output" \
  || fail 'Prerelease suffix was stripped'
run_case golden_upgrade.sh cleanroom-assert \
  --to-image "$backend" --to-ref v1.0.0-alpha.84 --from-image "$backend" --from-ref v1.2.3
grep -Fxq "OBSERVED_FROM_IMAGE=$backend" "$TMP/output" || fail 'Explicit baseline digest changed'

printf 'PASS: %s Golden image CLI cases; no deployment or Golden acceptance executed.\n' "$checks"
