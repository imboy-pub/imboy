#!/usr/bin/env bash
# Real-container permission/release smoke; synthetic files, no DB/business UAT.
# 真实容器权限与 release 冒烟；仅合成文件，不代表完整业务 Golden 验收。
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
image="${1:?Usage: backend_runtime_test.sh BUILT_IMAGE}"
work="$(mktemp -d)"
probe_name="imboy-runtime-probe-$$"
cleanup() {
  docker rm -f "$probe_name" >/dev/null 2>&1 || true
  docker run --rm --network none --user 0:0 --entrypoint /bin/sh \
    --mount "type=bind,source=$work,target=/fixtures" "$image" \
    -c 'rm -rf /fixtures/private /fixtures/log' >/dev/null 2>&1 || true
  rm -rf -- "$work"
}
trap cleanup EXIT
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
[[ "$(docker image inspect --format '{{.Config.User}}' "$image")" == 10001:10001 ]] \
  || fail 'The image must default to the unprivileged runtime identity.'
root_fixture() {
  docker run --rm --network none --user 0:0 --entrypoint /bin/sh \
    --mount "type=bind,source=$work,target=/fixtures" "$image" -eu -c "$1"
}
root_fixture '
  mkdir -p /fixtures/private/keys /fixtures/log
  chmod 700 /fixtures/private /fixtures/private/keys /fixtures/log
  printf "synthetic-private-fixture\n" > /fixtures/private/keys/login_rsa_priv.pem
  printf "synthetic-optional-pem\n" > /fixtures/private/optional.pem
  printf "%s\n" "-name runtime_probe@127.0.0.1" "-setcookie runtime_fixture_only" "+S 1" "+A 2" > /fixtures/private/golden-vm.args
  printf "existing-root-log\n" > /fixtures/log/existing.log
  chmod 600 /fixtures/private/keys/login_rsa_priv.pem /fixtures/private/optional.pem \
    /fixtures/private/golden-vm.args /fixtures/log/existing.log
  chown 20002:20002 /fixtures/private/optional.pem
'
jq -n --arg image "$image" --arg private "$work/private" --arg log "$work/log" '
  {services: {imboy_backend: {image: $image, volumes: [
    {type:"bind",source:$private,target:"/opt/imboy/priv_runtime",read_only:true},
    {type:"bind",source:$log,target:"/opt/imboy/log"}
  ]}}}
' > "$work/compose.json"
prepare() { bash "$ROOT/deploy/prepare-backend-runtime.sh" -f "$work/compose.json"; }
mounts=(--mount "type=bind,source=$work/private,target=/opt/imboy/priv_runtime,readonly"
        --mount "type=bind,source=$work/log,target=/opt/imboy/log")

# Existing root:600 files are initially inaccessible to the actual image user.
if docker run --rm --network none "${mounts[@]}" --entrypoint /bin/sh "$image" \
     -c 'test -r /opt/imboy/priv_runtime/keys/login_rsa_priv.pem'; then
  fail 'Root-owned fixture unexpectedly readable before preparation.'
fi
prepare
docker run --rm --network none "${mounts[@]}" --entrypoint /bin/sh "$image" -eu -c '
  test "$(id -u):$(id -g)" = 10001:10001
  test "$(cat /opt/imboy/priv_runtime/keys/login_rsa_priv.pem)" = synthetic-private-fixture
  test "$(cat /opt/imboy/priv_runtime/optional.pem)" = synthetic-optional-pem
  test ! -w /opt/imboy/priv_runtime/keys/login_rsa_priv.pem
  test "$(cat /opt/imboy/log/existing.log)" = existing-root-log
  printf "nonroot-append\n" >> /opt/imboy/log/existing.log
  mkdir /opt/imboy/log/new-directory
  : > /opt/imboy/log/new-directory/new.log
  test "$HOME" = /home/imboy
  (umask 077; printf "runtime_fixture_only\n" > "$HOME/.erlang.cookie")
  test "$(stat -c %a "$HOME/.erlang.cookie")" = 600
  temporary="$(mktemp)"; rm "$temporary"
  test ! -w /opt/imboy/bin/imboy
  for directory in /opt/imboy/erts-*/bin /opt/imboy/lib/imboy-*/ebin; do
    test ! -w "$directory"
  done
'
if docker run --rm --network none --user 10002:10002 "${mounts[@]}" \
     --entrypoint /bin/sh "$image" \
     -c 'test -r /opt/imboy/priv_runtime/keys/login_rsa_priv.pem'; then
  fail 'Preparation granted secret access to an unrelated user.'
fi
root_fixture '
  test "$(stat -c %u:%g /fixtures/private/keys/login_rsa_priv.pem)" = 0:0
  test "$(stat -c %u:%g /fixtures/private/optional.pem)" = 20002:20002
  test "$(cat /fixtures/private/keys/login_rsa_priv.pem)" = synthetic-private-fixture
  chmod 600 /fixtures/private/keys/login_rsa_priv.pem
'
prepare # Installer re-run restores the ACL mask after its existing chmod 600.

# Execute the actual relx script and bundled ERTS/crypto NIF as the default user.
# OS-variable replacement exercises writable relx output with a read-only input.
timeout 120 docker run --rm --name "$probe_name" --network none "${mounts[@]}" \
  -e VMARGS_PATH=/opt/imboy/priv_runtime/golden-vm.args -e RELX_REPLACE_OS_VARS=true \
  "$image" /opt/imboy/bin/imboy console_clean -noshell -eval '
    {ok, _} = application:ensure_all_started(crypto),
    {ok, <<"synthetic-private-fixture\n">>} = file:read_file("/opt/imboy/priv_runtime/keys/login_rsa_priv.pem"),
    ok = file:write_file("/opt/imboy/log/release-io-probe", <<"ok">>),
    io:format("RUNTIME_PROBE_OK~n"), halt(0).
  ' | tee "$work/release.log"
grep -Fxq RUNTIME_PROBE_OK "$work/release.log" || fail 'The real release probe did not finish.'

jq '.services.imboy_backend.user="0:0"' "$work/compose.json" > "$work/root.json"
if bash "$ROOT/deploy/prepare-backend-runtime.sh" -f "$work/root.json"; then
  fail 'A root Compose override was accepted for the non-root image.'
fi
root_fixture 'ln -s /etc/passwd /fixtures/private/unexpected-link'
if prepare; then fail 'A symlink in the private tree was accepted.'; fi
root_fixture 'rm /fixtures/private/unexpected-link'
prepare
printf '[PASS] Non-root image, root-owned upgrade fixtures, ACL re-run, read-only secrets and real release smoke.\n'
