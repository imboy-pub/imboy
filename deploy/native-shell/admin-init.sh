#!/usr/bin/env bash
set -euo pipefail
HERE=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
source "$HERE/lib/common.sh"
if [[ ${1:-} == --help || ${1:-} == -h ]]; then
    printf 'Usage: sudo bash admin-init.sh\nCreate the first admin interactively. Password is hidden and never placed in argv. Existing admin is preserved.\n'; exit 0
fi
[[ $# == 0 ]] || die 2 ARG 'No command-line credentials are accepted.'
require_root; lock_operation
[[ -f $CONF_DIR/release.env ]] || die 3 INSTALL 'Install the native package first.'
count=$(psql_admin imboy -Atqc 'SELECT count(*) FROM public.adm_user')
[[ $count =~ ^[0-9]+$ ]] || die 5 ADMIN 'Unable to determine administrator state.'
if (( count > 0 )); then info 'Administrator already initialized; no account or password changed.'; exit 0; fi
[[ -t 0 ]] || die 6 ADMIN_REQUIRED 'Service is ready; run sudo bash admin-init.sh from an interactive terminal to complete first-admin initialization.'
printf 'Administrator email or mobile: '; IFS= read -r account
[[ $account =~ ^[a-zA-Z0-9@._+-]{5,128}$ ]] || die 2 ADMIN_ACCOUNT 'Enter a valid email or mobile account.'
printf 'Administrator nickname: '; IFS= read -r nickname
[[ -n $nickname && ${#nickname} -le 40 ]] || die 2 ADMIN_NAME 'Nickname must contain 1-40 characters.'
printf 'Password (12-64 characters; letters and digits): '; IFS= read -r -s password; printf '\n'
printf 'Repeat password: '; IFS= read -r -s confirmation; printf '\n'
[[ $password == "$confirmation" && $password =~ ^[[:graph:]]{12,64}$ && $password =~ [a-zA-Z] && $password =~ [0-9] ]] || die 2 ADMIN_PASSWORD 'Use 12-64 printable ASCII characters without spaces, including letters and digits; confirmation must match.'
private=$(mktemp -d /run/imboy-admin.XXXXXX)
cleanup() { unset password confirmation digest; rm -f "$private/account" "$private/password" "$private/nickname" "$private/request.json" "$private/response.json"; rmdir "$private"; }
trap cleanup EXIT
chmod 700 "$private"
# Client login contract hashes the entered password with MD5 before server hashing.
digest=$(printf '%s' "$password" | openssl dgst -md5 | awk '{print $NF}')
unset password confirmation
printf '%s' "$account" > "$private/account"
printf '%s' "$digest" | openssl pkeyutl -encrypt -pubin -inkey "$CONF_DIR/keys/login_rsa_pub.pem" \
    -pkeyopt rsa_padding_mode:oaep -pkeyopt rsa_oaep_md:sha256 -pkeyopt rsa_mgf1_md:sha256 \
    | openssl base64 -A > "$private/password"
printf '%s' "$nickname" > "$private/nickname"
jq -n --rawfile account "$private/account" --rawfile password "$private/password" --rawfile nickname "$private/nickname" '{account:$account,password:$password,nickname:$nickname}' > "$private/request.json"
curl --noproxy '*' --fail --silent --show-error --connect-timeout 3 --max-time 30 \
    -H 'Content-Type: application/json' --data-binary "@$private/request.json" \
    "http://127.0.0.1:$HTTP_PORT/api/adm/setup/init" -o "$private/response.json" || die 5 ADMIN_HTTP 'Local first-admin request failed.'
jq -e '.code==0' "$private/response.json" >/dev/null || die 5 ADMIN_INIT 'Admin initialization was rejected; inspect the local response code without sharing credentials.'
curl --noproxy '*' --fail --silent --show-error --max-time 8 \
    "http://127.0.0.1:$HTTP_PORT/api/adm/setup/status" -o "$private/response.json"
jq -e '.code==0 and .payload.initialized==true' "$private/response.json" >/dev/null || die 5 ADMIN_INIT 'Admin state did not confirm initialization.'
info 'First administrator created. Store the password in your approved password manager.'
