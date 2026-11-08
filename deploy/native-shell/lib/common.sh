#!/usr/bin/env bash
# Shared Bash functions. Configuration is parsed as data; never sourced/eval'ed.
set +x
set -euo pipefail
umask 077
export LC_ALL=C
PACKAGE_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd -P)
INSTALL_DIR=/opt/imboy
CONF_DIR=/etc/imboy
DATA_DIR=/var/lib/imboy
STATE_DIR=/var/lib/imboy-install
BACKUP_DIR=/var/backups/imboy
PG_ROOT=$INSTALL_DIR/runtime/postgresql
GARAGE_BIN=$INSTALL_DIR/runtime/garage
HTTP_PORT=9800
PG_PORT=5432
PG_USER=imboy
PG_DATABASE=imboy
PRIVATE_BUCKET=imboy
PUBLIC_BUCKET=imboy-public
GARAGE_REGION=garage
declare -A SITE=() META=() SECRET=()
SITE_KEYS='SITE_NAME|API_DOMAIN|S3_DOMAIN|FILES_DOMAIN|TLS_DIR|GARAGE_CAPACITY|CORS_ORIGINS'
META_KEYS='PACKAGE_FORMAT|RELEASE_ID|SOURCE_SHA|BACKEND_VERSION|OS_ID|OS_VERSION_ID|ARCH|OTP_RELEASE|PG_MAJOR|PG_VERSION|GARAGE_VERSION|PRODUCT_PROFILE|MIGRATION_HEAD|PAYLOAD_READY|BUILD_RECEIPT_SHA256|BUILD_DATE_UTC|CONFIG_SHA256|PG_PREFIX|PG_PAYLOAD_SHA256|GARAGE_SHA256|BACKEND_PAYLOAD_SHA256'
SECRET_KEYS='PG_PASSWORD|JWT_SECRET|ERLANG_COOKIE|GARAGE_ACCESS_KEY|GARAGE_SECRET_KEY|GARAGE_RPC_SECRET|GARAGE_ADMIN_TOKEN|CS_SIGNING_SECRET|POSTGRE_AES_KEY|ADM_COOKIE_SECRET|SOLIDIFIED_KEY|SOLIDIFIED_KEY_IV|PASSWORD_SALT'

die() { local code=$1 tag=$2; shift 2; printf 'ERROR [%s] %s\n' "$tag" "$*" >&2; exit "$code"; }
info() { printf '[%s] %s\n' "$(date -u +%FT%TZ)" "$*"; }
need() { command -v "$1" >/dev/null 2>&1 || die 3 DEPENDENCY "Missing command: $1"; }
require_root() { [[ $(id -u) == 0 ]] || die 3 ROOT 'Run this operation with sudo.'; }
sha() { sha256sum -- "$1" | cut -d ' ' -f1; }
safe_path() { [[ $1 =~ ^/[a-zA-Z0-9_./-]+$ && $1 != *'/../'* && $1 != *'/./'* && $1 != */.. && $1 != / ]] || die 2 PATH 'Use an absolute path without spaces, shell syntax or parent traversal.'; }

read_kv() {
    local file=$1 target=$2 allowed=$3 line key value n=0
    local -n dest=$target
    [[ -f $file && ! -L $file ]] || die 3 CONFIG "Missing regular config file: $file"
    dest=()
    while IFS= read -r line || [[ -n $line ]]; do
        n=$((n+1)); line=${line%$'\r'}
        [[ -z $line || $line == \#* ]] && continue
        [[ $line =~ ^([A-Z][A-Z0-9_]*)=(.*)$ ]] || die 2 CONFIG "Invalid KEY=value syntax at line $n; do not use export or quotes."
        key=${BASH_REMATCH[1]}; value=${BASH_REMATCH[2]}
        [[ $key =~ ^($allowed)$ ]] || die 2 CONFIG "Unknown key at line $n: $key"
        [[ ! -v dest[$key] ]] || die 2 CONFIG "Duplicate key: $key"
        [[ $value != *$'\t'* && $value != *$'\r'* ]] || die 2 CONFIG "Invalid control character at line $n"
        dest[$key]=$value
    done < "$file"
}

valid_domain() {
    local d=$1 label
    [[ ${#d} -le 253 && $d == *.* && $d =~ ^[a-z0-9.-]+$ && $d != *..* ]] || return 1
    [[ ! $d =~ ^[0-9.]+$ && $d != example.com && $d != *.example.com && $d != *.invalid && $d != *.localhost ]] || return 1
    local -a labels; IFS=. read -r -a labels <<< "$d"
    for label in "${labels[@]}"; do [[ $label =~ ^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$ ]] || return 1; done
}

load_site() {
    read_kv "$1" SITE "$SITE_KEYS"
    local key origin
    for key in SITE_NAME API_DOMAIN S3_DOMAIN FILES_DOMAIN TLS_DIR GARAGE_CAPACITY CORS_ORIGINS; do
        [[ -v SITE[$key] ]] || die 2 CONFIG "Missing site key: $key"
        printf -v "$key" '%s' "${SITE[$key]}"
    done
    [[ $SITE_NAME =~ ^[a-z0-9][a-z0-9-]{0,40}$ ]] || die 2 CONFIG 'Invalid SITE_NAME.'
    for key in API_DOMAIN S3_DOMAIN FILES_DOMAIN; do valid_domain "${!key}" || die 2 DOMAIN "Set a real lowercase hostname for $key."; done
    [[ $API_DOMAIN != "$S3_DOMAIN" && $S3_DOMAIN != "$FILES_DOMAIN" && $API_DOMAIN != "$FILES_DOMAIN" ]] || die 2 DOMAIN 'API/S3/FILES domains must be distinct.'
    safe_path "$TLS_DIR"
    [[ $GARAGE_CAPACITY =~ ^[1-9][0-9]*[GMT]$ ]] || die 2 CAPACITY 'GARAGE_CAPACITY must be e.g. 10G, 100G or 1T.'
    if [[ -n $CORS_ORIGINS ]]; then
        local -a origins; IFS=, read -r -a origins <<< "$CORS_ORIGINS"
        for origin in "${origins[@]}"; do
            [[ $origin == https://* ]] || die 2 CORS 'Use comma-separated HTTPS origins without paths or wildcards.'
            valid_domain "${origin#https://}" || die 2 CORS 'Invalid CORS origin.'
        done
    fi
}

load_metadata() {
    read_kv "$1/metadata/release.env" META "$META_KEYS"
    local key
    for key in PACKAGE_FORMAT RELEASE_ID SOURCE_SHA BACKEND_VERSION OS_ID OS_VERSION_ID ARCH OTP_RELEASE PG_MAJOR PG_VERSION GARAGE_VERSION PRODUCT_PROFILE MIGRATION_HEAD PAYLOAD_READY; do
        [[ -n ${META[$key]:-} ]] || die 3 PAYLOAD "Missing release metadata: $key"
    done
    [[ ${META[PACKAGE_FORMAT]} == 1 ]] || die 3 FORMAT 'Unsupported package format.'
    [[ ${META[PAYLOAD_READY]} == yes ]] || die 3 PAYLOAD 'Native binaries are not assembled. Publisher must run tools/prepare-payload.sh before customer installation.'
    [[ ${META[RELEASE_ID]} =~ ^[a-zA-Z0-9][a-zA-Z0-9._-]{0,90}$ && ${META[RELEASE_ID]} != *..* ]] || die 2 METADATA 'Invalid RELEASE_ID.'
    [[ ${META[SOURCE_SHA]} =~ ^[0-9a-f]{40}$ && ${META[MIGRATION_HEAD]} =~ ^[0-9]+$ ]] || die 2 METADATA 'Invalid source/migration identity.'
    [[ ${META[BACKEND_VERSION]} =~ ^[0-9][a-zA-Z0-9.+_-]*$ && ${META[PG_VERSION]} =~ ^18\.[0-9]+$ ]] || die 2 METADATA 'Invalid runtime version string.'
    [[ ${META[OTP_RELEASE]} == 29 && ${META[PG_MAJOR]} == 18 && ${META[GARAGE_VERSION]} == 2.4.1 && ${META[PRODUCT_PROFILE]} == phase1_delivery ]] || die 3 CONTRACT 'Payload runtime/profile is outside this installer contract.'
}

verify_manifest() {
    local dir=$1 digest path line
    [[ -s $dir/CHECKSUMS.sha256 ]] || die 3 CHECKSUM 'Missing CHECKSUMS.sha256.'
    while IFS= read -r line || [[ -n $line ]]; do
        digest=${line:0:64}; path=${line:66}
        [[ $digest =~ ^[a-f0-9]{64}$ && ${line:64:2} == '  ' && $path == ./* && $path != *'/../'* && $path != */.. ]] || die 3 CHECKSUM 'Invalid checksum entry.'
        [[ -f $dir/${path#./} ]] || die 3 CHECKSUM 'Missing checksum member.'
    done < "$dir/CHECKSUMS.sha256"
    [[ -z $(find "$dir" -type l -print -quit) ]] || die 3 CHECKSUM 'Delivery directory must contain regular files, without symlinks.'
    cmp -s <(cd "$dir" && find . -type f ! -path './CHECKSUMS.sha256' ! -path './site.env' | sort) \
        <(cut -c 67- "$dir/CHECKSUMS.sha256" | sort) || die 3 CHECKSUM 'Checksum file does not cover the exact package file set. Keep runtime logs outside the package directory.'
    (cd -- "$dir" && sha256sum --check --status CHECKSUMS.sha256) || die 3 CHECKSUM 'Package content changed or is incomplete.'
    for path in install.sh lib/common.sh lib/render.sh lib/services.sh metadata/release.env; do
        grep -Eq "^[a-f0-9]{64}  \./${path//./\.}$" "$dir/CHECKSUMS.sha256" || die 3 CHECKSUM "Required file not covered: $path"
    done
}

load_secrets() {
    local file=${1:-$CONF_DIR/secrets.env} key mode
    read_kv "$file" SECRET "$SECRET_KEYS"
    mode=$(stat -c %a -- "$file")
    (( (8#$mode & 077) == 0 )) || die 3 SECRET_PERMS 'secrets.env must be mode 600.'
    for key in PG_PASSWORD JWT_SECRET ERLANG_COOKIE GARAGE_ACCESS_KEY GARAGE_SECRET_KEY GARAGE_RPC_SECRET GARAGE_ADMIN_TOKEN CS_SIGNING_SECRET POSTGRE_AES_KEY ADM_COOKIE_SECRET SOLIDIFIED_KEY SOLIDIFIED_KEY_IV PASSWORD_SALT; do
        [[ -n ${SECRET[$key]:-} && ${SECRET[$key]} =~ ^[a-zA-Z0-9]+$ ]] || die 3 SECRET "Missing or invalid secret key: $key"
        printf -v "$key" '%s' "${SECRET[$key]}"
    done
    [[ ${#SOLIDIFIED_KEY_IV} == 16 && ${#SOLIDIFIED_KEY} == 32 && ${#POSTGRE_AES_KEY} == 32 ]] || die 3 SECRET 'Invalid encryption key length.'
}

generate_secrets() {
    local dir=$1 key bytes tmp
    if [[ -f $dir/secrets.env ]]; then load_secrets "$dir/secrets.env"; touch "$dir/secrets.initialized"; return; fi
    [[ ! -e $dir/secrets.initialized && ! -f $DATA_DIR/postgres/PG_VERSION && ! -e $dir/keys/login_rsa_priv.pem ]] || die 4 SECRET_LOST 'Secrets are missing for an initialized instance. Restore the original secrets from backup; do not generate replacements.'
    mkdir -p -- "$dir"; chmod 700 -- "$dir"
    tmp=$(mktemp "$dir/.secrets.XXXXXX")
    for key in PG_PASSWORD JWT_SECRET ERLANG_COOKIE GARAGE_SECRET_KEY GARAGE_RPC_SECRET GARAGE_ADMIN_TOKEN CS_SIGNING_SECRET POSTGRE_AES_KEY ADM_COOKIE_SECRET SOLIDIFIED_KEY SOLIDIFIED_KEY_IV PASSWORD_SALT; do
        bytes=32
        case $key in JWT_SECRET|POSTGRE_AES_KEY|SOLIDIFIED_KEY) bytes=16;; SOLIDIFIED_KEY_IV) bytes=8;; esac
        printf '%s=%s\n' "$key" "$(openssl rand -hex "$bytes")" >> "$tmp"
    done
    printf 'GARAGE_ACCESS_KEY=GK%s\n' "$(openssl rand -hex 12)" >> "$tmp"
    chmod 600 "$tmp"; mv -T -- "$tmp" "$dir/secrets.env"
    touch "$dir/secrets.initialized"
    load_secrets "$dir/secrets.env"
}

validate_tls() {
    local role domain cert key cert_hash key_hash
    for role in api s3 files; do
        case $role in api) domain=$API_DOMAIN;; s3) domain=$S3_DOMAIN;; files) domain=$FILES_DOMAIN;; esac
        cert=$TLS_DIR/$role/fullchain.pem; key=$TLS_DIR/$role/privkey.pem
        [[ -s $cert && -s $key ]] || die 3 TLS "Missing fullchain.pem/privkey.pem in $TLS_DIR/$role"
        openssl x509 -in "$cert" -noout -checkend 604800 >/dev/null 2>&1 || die 3 TLS "Certificate expires within 7 days or is invalid: $role"
        # OpenSSL x509 can exit 0 even when checkhost prints a mismatch.
        openssl x509 -in "$cert" -noout -checkhost "$domain" 2>/dev/null | grep -q 'does match certificate' || die 3 TLS "Certificate hostname does not match $role domain."
        cert_hash=$(openssl x509 -in "$cert" -pubkey -noout | openssl pkey -pubin -outform DER 2>/dev/null | sha256sum | cut -d ' ' -f1)
        key_hash=$(openssl pkey -in "$key" -passin pass: -pubout -outform DER 2>/dev/null | sha256sum | cut -d ' ' -f1) || die 3 TLS "Invalid/encrypted TLS key for $role."
        [[ $cert_hash == "$key_hash" ]] || die 3 TLS "Certificate and key do not match for $role."
    done
}

check_host() {
    local apply=${1:-no} os_id os_version cmd
    [[ $(uname -s) == Linux && $(uname -m) == "${META[ARCH]}" ]] || die 3 PLATFORM 'Payload architecture differs from host.'
    os_id=$(sed -n 's/^ID=//p' /etc/os-release | tr -d '"')
    os_version=$(sed -n 's/^VERSION_ID=//p' /etc/os-release | tr -d '"')
    [[ $os_id == "${META[OS_ID]}" && $os_version == "${META[OS_VERSION_ID]}" ]] || die 3 PLATFORM 'Use a payload built and certified for this exact OS release.'
    for cmd in bash curl openssl jq tar sha256sum awk sed grep flock stat install runuser useradd getent systemctl nginx ss ldd file timeout; do need "$cmd"; done
    curl --help all | grep -q -- --aws-sigv4 || die 3 CURL 'curl with --aws-sigv4 is required.'
    [[ -d /run/systemd/system ]] || die 3 SYSTEMD 'A running systemd host is required.'
    if [[ $apply == yes ]]; then require_root; fi
    # Preinstalled hosts, upgrades and restores obey the same frozen OS package lock.
    bash "$PACKAGE_DIR/tools/prepare-host.sh" --check || die 3 HOST_PACKAGES 'Host package versions differ from the candidate lock. Use its documented host preparation procedure.'
}

validate_payload() {
    local dir=$1 f output expected_config actual_config
    for f in backend/bin/imboy postgresql/bin/postgres postgresql/bin/initdb postgresql/bin/pg_ctl postgresql/bin/psql postgresql/bin/pg_dump postgresql/bin/pg_restore postgresql/bin/pg_config garage/garage; do
        [[ -x $dir/payload/$f ]] || die 3 PAYLOAD "Missing executable: payload/$f"
    done
    [[ -d $dir/payload/backend/lib && -d $dir/payload/backend/releases ]] || die 3 PAYLOAD 'Incomplete relx release.'
    [[ -f $dir/payload/backend/releases/${META[BACKEND_VERSION]}/sys.config ]] || die 3 PAYLOAD 'Missing release sys.config template.'
    expected_config=$(sed -n 's/^IMBOY_NATIVE_CONFIG_SHA256=\([0-9a-f]\{64\}\)$/\1/p' "$dir/lib/render.sh")
    actual_config=$(sha "$dir/payload/backend/releases/${META[BACKEND_VERSION]}/sys.config")
    [[ $expected_config =~ ^[0-9a-f]{64}$ && $actual_config == "$expected_config" && ${META[CONFIG_SHA256]:-} == "$actual_config" ]] || die 3 RENDERER 'Runtime config differs from the reviewed renderer contract.'
    while IFS= read -r -d '' f; do
        if file -b -- "$f" | grep -q '^ELF'; then
            output=$(LD_LIBRARY_PATH="$dir/payload/postgresql/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" ldd "$f" 2>&1 || true)
            [[ $output != *'not found'* ]] || die 3 SHARED_LIBRARY "Missing shared library for ${f#"$dir"/}; publisher must supply the matching OS dependency bundle."
        fi
    done < <(find "$dir/payload" -type f \( -perm /111 -o -name '*.so' -o -name '*.so.*' \) -print0)
}

lock_operation() {
    mkdir -p "$STATE_DIR"; chmod 700 "$STATE_DIR"
    exec 9>"$STATE_DIR/operation.lock"
    flock -n 9 || die 4 BUSY 'Another install/upgrade/backup/restore operation is running.'
}

check_install_resume_state() {
    local expected=$1 status
    [[ -f $STATE_DIR/install.identity && $(cat "$STATE_DIR/install.identity") == "$expected" ]] || die 4 RESUME 'Resume requires exactly the same package and site.env. Do not delete state to bypass this check.'
    status=$(cat "$STATE_DIR/status" 2>/dev/null || true)
    case $status in
        ''|FAILED|SERVICE_READY|INITIALIZED) ;;
        *) die 4 RESUME_PHASE 'This instance has entered upgrade or restore. install --resume cannot switch it back to an earlier release; use the documented recovery procedure.' ;;
    esac
}

psql_admin() { runuser -u imboy-pg -- "$PG_ROOT/bin/psql" -X -h /run/imboy-pg -p "$PG_PORT" -U postgres -d "${1:-postgres}" -v ON_ERROR_STOP=1 "${@:2}"; }
garage_cmd() { runuser -u imboy-garage -- "$GARAGE_BIN" -c "$CONF_DIR/garage.toml" "$@"; }
wait_postgres() {
    local n
    for ((n=0;n<60;n++)); do if psql_admin postgres -Atqc 'SELECT 1' >/dev/null 2>&1; then return 0; fi; sleep 1; done
    printf 'ERROR [PG_READY] PostgreSQL did not become ready.\n' >&2; return 1
}
wait_garage() {
    local n
    for ((n=0;n<60;n++)); do if garage_cmd status >/dev/null 2>&1; then return 0; fi; sleep 1; done
    printf 'ERROR [GARAGE_READY] Garage did not become ready.\n' >&2; return 1
}
wait_health() {
    local n body
    for ((n=0;n<90;n++)); do
        if body=$(curl --noproxy '*' -fsS --connect-timeout 2 --max-time 3 "http://127.0.0.1:$HTTP_PORT/healthz" 2>/dev/null) &&
           jq -e --arg v "${META[BACKEND_VERSION]}" '.status=="ok" and .db=="up" and .tsid=="ready" and .version==$v' <<< "$body" >/dev/null; then return; fi
        sleep 2
    done
    printf 'ERROR [HEALTH] Backend did not reach expected version/readiness. Check journalctl -u imboy-backend locally.\n' >&2
    return 1
}
