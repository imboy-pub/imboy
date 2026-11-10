#!/usr/bin/env bash
# Prepare only the backend's two bind mounts; never start the application as root.
# 仅准备后端两个 bind 目录；保留宿主所有权/密钥内容，不以 root 启动应用。
set -euo pipefail
set +x
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }

# Pass the same Compose options as pull/up, from the same working directory.
# config JSON may contain secrets: retain only non-secret mount/image metadata.
metadata="$(docker compose "$@" config --format json | jq -ces '
  if length != 1 then error("invalid Compose document") else
    .[0].services.imboy_backend |
    if (.image | type) != "string" or (.image | length) == 0 then
      error("missing backend image")
    else {image, user, volumes} end
  end
' 2>/dev/null)" || fail 'Cannot resolve backend runtime metadata; no permissions changed.'
image="$(jq -r .image <<< "$metadata")"
image_user="$(docker image inspect --format '{{.Config.User}}' "$image")" \
  || fail 'Pull the backend image before preparing its runtime mounts.'

# Old releases run as root and do not need this migration. Keep rollback/install
# of those immutable images possible; never override the user of a new image.
case "$image_user" in
  ''|0|0:0|root|root:root)
    printf '[INFO] Legacy image: runtime permission migration is not required.\n'
    exit 0 ;;
  10001:10001) ;;
  *) fail 'Unsupported backend image user; review custom UID/GID mount permissions.' ;;
esac
configured_user="$(jq -r '.user // ""' <<< "$metadata")"
case "$configured_user" in
  ''|10001:10001) ;;
  *) fail 'Remove the backend Compose user override; this image runs as 10001:10001.' ;;
esac

source_for() {
  jq -er --arg target "$1" '
    [.volumes[]? | select(.target == $target)] |
    if length != 1 or .[0].type != "bind" then
      error("expected one bind mount")
    else .[0].source |
      if type != "string" or (startswith("/") | not) or . == "/"
         or contains("\n") or contains(",") then error("invalid bind source")
      else . end
    end
  ' <<< "$metadata" 2>/dev/null
}
private_dir="$(source_for /opt/imboy/priv_runtime)" \
  || fail 'Expected a backend private bind mount; review custom mounts before upgrading.'
log_dir="$(source_for /opt/imboy/log)" \
  || fail 'Expected a backend log bind mount; review custom mounts before upgrading.'
[[ "$private_dir" != "$log_dir" && ! -L "$private_dir" && ! -L "$log_dir" ]] \
  || fail 'Runtime bind roots must be distinct real directories.'
[[ "$(realpath -m -- "$private_dir")" == "$private_dir" \
   && "$(realpath -m -- "$log_dir")" == "$log_dir" ]] \
  || fail 'Runtime bind roots must be canonical paths without symlink parents.'
case "$private_dir/" in "$log_dir/"*) fail 'Runtime bind directories must not overlap.' ;; esac
case "$log_dir/" in "$private_dir/"*) fail 'Runtime bind directories must not overlap.' ;; esac
jq -e '.volumes[] | select(.target == "/opt/imboy/priv_runtime") | .read_only == true' \
  <<< "$metadata" >/dev/null || fail 'The backend private mount must remain read-only.'
jq -e '.volumes[] | select(.target == "/opt/imboy/log") | .read_only != true' \
  <<< "$metadata" >/dev/null || fail 'The backend log mount must be writable.'
mkdir -p -- "$private_dir" "$log_dir"

# ACLs preserve the host owner's access, including installer chmod 600/re-runs.
# The temporary root process has no network and can write only these two binds.
# Refuse symlink entries instead of following them outside the approved trees.
docker run --rm --network none --read-only --user 0:0 \
  --cap-drop ALL --cap-add FOWNER --cap-add DAC_OVERRIDE \
  --mount "type=bind,source=$private_dir,target=/runtime-secrets" \
  --mount "type=bind,source=$log_dir,target=/runtime-log" \
  --entrypoint /bin/sh "$image" -eu -c '
    for directory in /runtime-secrets /runtime-log; do
      if [ -n "$(find -P "$directory" -xdev -type l -print -quit)" ]; then
        printf "%s\n" "Runtime permission preparation refuses symlinks; review their targets." >&2
        exit 1
      fi
    done
    find -P /runtime-secrets -xdev -type d -exec setfacl -m u:10001:rx {} +
    find -P /runtime-secrets -xdev -type f -exec setfacl -m u:10001:r {} +
    find -P /runtime-log -xdev -type d -exec setfacl -m u:10001:rwx,d:u:10001:rwx {} +
    find -P /runtime-log -xdev -type f -exec setfacl -m u:10001:rw {} +
  ' || fail 'Cannot prepare backend mount permissions; service startup is blocked.'
printf '[PASS] Backend bind permissions prepared for UID/GID 10001:10001.\n'
