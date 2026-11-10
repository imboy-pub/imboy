#!/usr/bin/env bash
# Shared non-PostgreSQL regression selection for Ubuntu and Debian CI.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../.."

log_file=$(mktemp "${TMPDIR:-/tmp}/imboy-core-eunit.XXXXXXXX")
trap 'rm -f "$log_file"' EXIT
erl -noshell -pa ebin test deps/*/ebin \
  -eval 'Tests = [
    {generator, fun eb_direct_purge_mixed_pg_tests:environment_restore_test_/0},
    {generator, fun enterprise_asset_driver_pg_tests:environment_isolation_test_/0},
    elib_s3_sign_tests,
    imboy_app_registry_validate_tests,
    e2ee_presign_mime_binding_tests
  ], case eunit:test(Tests, [verbose]) of ok -> halt(0); _ -> halt(1) end.' \
  2>&1 | tee "$log_file"

# eunit:test/2 may return ok for an empty selection; require a positive summary.
grep -Eq '^[[:space:]]*(All [1-9][0-9]* tests passed\.|Test passed\.)$' "$log_file" || {
  echo 'ERROR: core EUnit produced no successful nonempty test summary' >&2
  exit 1
}
