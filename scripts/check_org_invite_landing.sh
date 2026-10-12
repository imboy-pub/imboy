#!/usr/bin/env bash
# Local rendered-page regression; optionally also check the public HTTPS entry.
set -euo pipefail
cd "$(dirname "$0")/.."
work_dir=$(mktemp -d)
trap 'rm -rf "$work_dir"' EXIT
erlc -o "$work_dir" src/api/org_invite_landing_handler.erl test/api/org_invite_landing_handler_tests.erl
INVITE_HTML="$work_dir/local.html" erl -noshell -pa "$work_dir" -eval '
  ok = eunit:test(org_invite_landing_handler_tests),
  ok = file:write_file(os:getenv("INVITE_HTML"), org_invite_landing_handler:landing_html(<<"ABCD2345">>)),
  halt().'
INVITE_HTML="$work_dir/local.html" node --test test/web/org_invite_landing.test.mjs
if [[ $# -gt 0 ]]; then
  base_url=${1%/}
  [[ "$base_url" == https://* ]] || { echo 'HTTPS public origin required' >&2; exit 1; }
  status=$(curl --silent --show-error --max-time 20 -D "$work_dir/headers" -o "$work_dir/public.html" -w '%{http_code}' "$base_url/invite/org/ABCD2345")
  [[ "$status" == 200 ]] || { echo "Invitation entry returned HTTP $status" >&2; exit 1; }
  grep -qi '^cache-control:.*no-store' "$work_dir/headers"
  INVITE_HTML="$work_dir/public.html" node --test test/web/org_invite_landing.test.mjs
  status=$(curl --silent --show-error --max-time 20 -o "$work_dir/invalid.html" -w '%{http_code}' "$base_url/invite/org/INVALID")
  [[ "$status" == 404 ]]
  grep -q '邀请链接无效' "$work_dir/invalid.html"
fi
