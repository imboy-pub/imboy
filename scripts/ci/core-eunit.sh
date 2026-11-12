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

# IMBOY_QUALITY_PHASE0_EUNIT: exercise the actual admission/lock helpers on
# both hosted OTP29 runners. Older OTP versions cannot execute OTP29 JSON paths.
otp_release=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt(0).' 2>/dev/null)
case "$otp_release" in
  29|30|31)
    (
      quality_beams=$(mktemp -d "${TMPDIR:-/tmp}/imboy-quality-phase0.XXXXXXXX")
      trap 'rm -rf -- "$quality_beams"' EXIT
      erlc -o "$quality_beams" \
        tools/imboy_quality/imboy_quality_guard.erl \
        tools/imboy_quality/imboy_quality_task_owner.erl \
        tools/imboy_quality/imboy_quality_budget.erl \
        tools/imboy_quality/imboy_quality_build.erl \
        tools/imboy_quality/imboy_quality_state.erl \
        tools/imboy_quality/imboy_quality_cli.erl \
        tools/imboy_quality/imboy_quality_full_budget.erl \
        tools/imboy_quality/imboy_quality_resources.erl
      erlc -o "$quality_beams" \
        test/quality/imboy_quality_admission_tests.erl \
        test/quality/imboy_quality_cloud_phase0_tests.erl
      erl -noshell -pa "$quality_beams" ebin test deps/*/ebin \
        -eval 'case eunit:test([imboy_quality_admission_tests, imboy_quality_cloud_phase0_tests], [verbose]) of ok -> halt(0); _ -> halt(1) end.'
    ) 2>&1 | tee "$log_file"
    grep -Eq '^[[:space:]]*(All [1-9][0-9]* tests passed\.|Test passed\.)$' "$log_file" || {
      echo 'ERROR: Phase0 quality EUnit emitted no positive summary' >&2
      exit 1
    }
    ;;
  *) echo "quality Phase0 EUnit deferred for OTP $otp_release (<29)" ;;
esac
