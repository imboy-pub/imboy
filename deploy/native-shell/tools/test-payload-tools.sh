#!/usr/bin/env bash
# Offline synthetic contract tests. Does not compile Erlang or start a service.
set -Eeuo pipefail
umask 077
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck source=lib-payload.sh
. "$TOOLS_DIR/lib-payload.sh"
# shellcheck source=lib-host-packages.sh
. "$TOOLS_DIR/lib-host-packages.sh"
scratch=$(mktemp -d /tmp/imboy-payload-test.XXXXXXXX)
trap 'rm -rf -- "$scratch"' EXIT
passed=0
pass() { passed=$((passed + 1)); printf 'PASS %s\n' "$1"; }
fail_expected() {
  local name=$1; shift
  if "$@" > "$scratch/expected-output.txt" 2>&1; then
    printf 'FAIL expected refusal: %s\n' "$name" >&2; exit 1
  fi
  pass "$name"
}
check_tree() { bash -c '. "$1"; payload_check_tree "$2"' _ "$TOOLS_DIR/lib-payload.sh" "$1"; }
check_hashes() { bash -c '. "$1"; payload_verify_hashes "$2" CHECKSUMS.sha256' _ "$TOOLS_DIR/lib-payload.sh" "$1"; }

mkdir "$scratch/tree"
printf 'plain\n' > "$scratch/tree/file.txt"
payload_hash_tree "$scratch/tree" CHECKSUMS.sha256
check_hashes "$scratch/tree"
pass 'hash-valid'
printf 'mutated\n' > "$scratch/tree/file.txt"
fail_expected 'hash-modified-byte' check_hashes "$scratch/tree"
payload_hash_tree "$scratch/tree" CHECKSUMS.sha256
printf 'unlisted\n' > "$scratch/tree/extra.txt"
fail_expected 'hash-unlisted-file' check_hashes "$scratch/tree"
rm "$scratch/tree/extra.txt"
printf 'SITE_NAME=local-fixture\n' > "$scratch/tree/site.env"
check_hashes "$scratch/tree"
pass 'site-env-intentionally-mutable'
printf '%064d  ../outside.txt\n' 0 > "$scratch/tree/CHECKSUMS.sha256"
fail_expected 'hash-parent-traversal' check_hashes "$scratch/tree"
mkdir "$scratch/secret"
printf '%s\n' '-----BEGIN PRIVATE KEY-----' 'FIXTURE-NOT-A-KEY' '-----END PRIVATE KEY-----' > "$scratch/secret/private.pem"
fail_expected 'private-pem' check_tree "$scratch/secret"
rm "$scratch/secret/private.pem"
touch "$scratch/secret/.env"
fail_expected 'dotenv' check_tree "$scratch/secret"
rm "$scratch/secret/.env"
touch "$scratch/secret/postmaster.pid"
fail_expected 'pg-runtime-state' check_tree "$scratch/secret"
rm "$scratch/secret/postmaster.pid"
ln -s /etc/hosts "$scratch/secret/outside"
fail_expected 'external-symlink' check_tree "$scratch/secret"
rm "$scratch/secret/outside"
printf 'KEY=value\nKEY=second\n' > "$scratch/values.env"
fail_expected 'duplicate-metadata' bash -c '. "$1"; payload_value "$2" KEY' _ "$TOOLS_DIR/lib-payload.sh" "$scratch/values.env"
printf 'KEY=$(touch %s)\n' "$scratch/must-not-exist" > "$scratch/values.env"
value=$(payload_value "$scratch/values.env" KEY)
[[ $value == '$('* && ! -e $scratch/must-not-exist ]]
pass 'metadata-is-data-not-shell'
mkdir -m 700 "$scratch/permissions"
printf 'public artifact\n' > "$scratch/permissions/data"
printf '#!/usr/bin/env bash\nexit 0\n' > "$scratch/permissions/program"
chmod 600 "$scratch/permissions/data"
chmod 700 "$scratch/permissions/program"
payload_normalize_permissions "$scratch/permissions"
payload_check_permissions "$scratch/permissions"
[[ $(stat -c %a "$scratch/permissions/data") == 644 && $(stat -c %a "$scratch/permissions/program") == 755 ]]
pass 'payload-service-readable-permissions'

# This sandbox lacks nginx. Fake only that package, never install host software.
# All other package/library ownership checks use the real read-only tools.
export PAYLOAD_TEST_REAL_DPKG_QUERY=$(command -v dpkg-query)
export PAYLOAD_TEST_REAL_APT_CACHE=$(command -v apt-cache)
mkdir "$scratch/mock-bin"
cat > "$scratch/mock-bin/dpkg-query" <<'EOF'
#!/usr/bin/env bash
query=${!#}
if [[ $1 == -W && ( $query == nginx || $query == nginx:amd64 ) ]]; then
  printf 'nginx\t1.0\tamd64\tinstalled\n'; exit 0
fi
exec "$PAYLOAD_TEST_REAL_DPKG_QUERY" "$@"
EOF
cat > "$scratch/mock-bin/apt-cache" <<'EOF'
#!/usr/bin/env bash
query=${!#}
if [[ $1 == depends && ( $query == nginx=1.0 || $query == nginx:amd64=1.0 ) ]]; then
  printf 'nginx\n'; exit 0
fi
exec "$PAYLOAD_TEST_REAL_APT_CACHE" "$@"
EOF
chmod 755 "$scratch/mock-bin/"*
export PATH="$scratch/mock-bin:$PATH"

# Build a tiny immutable fixture repository and already-built artifact contracts.
# These are stubs; success here is never reported as a native install/ERTS test.
payload_os
kit=$scratch/kit
repo=$scratch/repo
backend=$scratch/backend
pg=$scratch/pg
mkdir -p "$kit/tools" "$kit/lib" "$kit/templates" "$repo/config" "$repo/scripts/ci" "$repo/priv/migrations" "$repo/docker"
cp "$TOOLS_DIR"/*.sh "$kit/tools/"
for item in install.sh verify.sh backup.sh restore.sh upgrade.sh admin-init.sh README.md lib/common.sh lib/render.sh lib/services.sh templates/site.env.example; do
  printf 'fixture\n' > "$kit/$item"
done
printf '1.0.0-alpha.83\n' > "$repo/VERSION"
printf '[{imboy,[]}].\n' > "$repo/config/sys.config.example"
fixture_config_hash=$(sha256sum "$repo/config/sys.config.example" | cut -d ' ' -f1)
printf 'IMBOY_NATIVE_CONFIG_SHA256=%s\n' "$fixture_config_hash" > "$kit/lib/render.sh"
printf '%s\n' '-name imboy@127.0.0.1' '-setcookie placeholder' > "$repo/config/vm.args"
printf '{"profile":"phase1_delivery"}\n' > "$repo/config/product-feature-manifest.json"
printf '#!/usr/bin/env bash\nexit 0\n' > "$repo/scripts/imboy_ctl"
printf 'SELECT 1;\n' > "$repo/priv/migrations/00000243_fixture.up.sql"
printf 'SELECT 1;\n' > "$repo/priv/migrations/00000243_fixture.down.sql"
printf 'fixture dictionary\n' > "$repo/docker/pg_jieba_userdict.txt.big"
printf 'fixture license\n' > "$repo/LICENSE"
printf '#!/usr/bin/env bash\nprintf "garage v2.4.1\\n"\n' > "$scratch/garage"
chmod 755 "$scratch/garage"
garage_hash=$(sha256sum "$scratch/garage" | cut -d ' ' -f1)
printf '{"garage":{"sha256":"%s","version":"2.4.1"}}\n' "$garage_hash" > "$repo/scripts/ci/native-eunit-pins.json"
git -C "$repo" init -q
git -C "$repo" add .
git -C "$repo" -c user.name=Fixture -c user.email=fixture@example.invalid commit -qm fixture
source_sha=$(git -C "$repo" rev-parse HEAD)
mkdir -p "$backend/bin" "$backend/erts-fixture/bin" "$backend/releases/1.0.0-alpha.83" "$backend/lib/imboy-1.0.0-alpha.83/priv/migrations" "$backend/delivery-build"
cp "$repo/config/sys.config.example" "$backend/releases/1.0.0-alpha.83/sys.config"
cp "$repo/config/vm.args" "$backend/releases/1.0.0-alpha.83/vm.args"
cp "$repo/config/product-feature-manifest.json" "$backend/delivery-build/product-feature-manifest.json"
cp "$repo/priv/migrations/"*.sql "$backend/lib/imboy-1.0.0-alpha.83/priv/migrations/"
cp "$repo/scripts/imboy_ctl" "$backend/bin/imboy_ctl"
cp "$repo/scripts/imboy_ctl" "$backend/bin/imboy"
cp "$repo/scripts/imboy_ctl" "$backend/erts-fixture/bin/erlexec"
chmod 755 "$backend/bin/"* "$backend/erts-fixture/bin/erlexec"
printf 'fixture\t%s\n' "$source_sha" > "$backend/delivery-build/dependency-commits.tsv"
cat > "$backend/delivery-build/build.env" <<EOF
BUILD_FORMAT=1
SOURCE_SHA=$source_sha
BACKEND_VERSION=1.0.0-alpha.83
OS_ID=$PAYLOAD_OS_ID
OS_VERSION_ID=$PAYLOAD_OS_VERSION_ID
ARCH=$PAYLOAD_ARCH
OTP_RELEASE=29
PRODUCT_PROFILE=phase1_delivery
BUILD_TEST_STATUS=NOT_RUN
EOF
payload_hash_tree "$backend" BUILD_FILES.sha256
mkdir -p "$pg/bin" "$pg/lib" "$pg/share/extension" "$pg/share/tsearch_data"
for bin in postgres initdb pg_ctl psql pg_dump pg_restore createdb pg_isready; do
  printf '#!/usr/bin/env bash\nexit 0\n' > "$pg/bin/$bin"
done
cat > "$pg/bin/pg_config" <<EOF
#!/usr/bin/env bash
case \$1 in
 --version) printf 'PostgreSQL 18.6\\n';;
 --configure) printf "'%s'\\n" '--prefix=/opt/imboy/runtime/postgresql';;
 --sharedir) printf '%s\\n' '$pg/share';;
 --pkglibdir) printf '%s\\n' '$pg/lib';;
 *) exit 1;;
esac
EOF
chmod 755 "$pg/bin/"*
for ext in pgcrypto pg_jieba timescaledb vector postgis pg_trgm btree_gin btree_gist citext unaccent intarray pg_stat_statements; do
  printf "default_version = '1.0'\n" > "$pg/share/extension/$ext.control"
  printf 'SELECT 1;\n' > "$pg/share/extension/$ext--1.0.sql"
done
cp "$repo/docker/pg_jieba_userdict.txt.big" "$pg/share/tsearch_data/jieba_user.dict"
args=(--repo "$repo" --sha "$source_sha" --release-dir "$backend" --pg-prefix "$pg" --garage-bin "$scratch/garage" --release-id fixture)
bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/customer" > "$scratch/prepare.txt"
[[ $(payload_value "$scratch/customer/metadata/release.env" PAYLOAD_READY) == yes ]]
payload_verify_hashes "$scratch/customer" CHECKSUMS.sha256
pass 'prepare-synthetic-happy-path'
[[ -s $scratch/customer/metadata/host-runtime-packages.tsv ]]
payload_check_permissions "$scratch/customer/payload"
pass 'prepare-records-runtime-lock-and-safe-permissions'
printf 'IMBOY_NATIVE_CONFIG_SHA256=%064d\n' 0 > "$kit/lib/render.sh"
fail_expected 'prepare-renderer-contract-mismatch' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-renderer"
[[ ! -e $scratch/no-renderer ]]
printf 'IMBOY_NATIVE_CONFIG_SHA256=%s\n' "$fixture_config_hash" > "$kit/lib/render.sh"
fail_expected 'prepare-refuses-existing-output' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/customer"
cp "$backend/delivery-build/build.env" "$scratch/receipt-save"
sed -i 's/^SOURCE_SHA=.*/SOURCE_SHA=0000000000000000000000000000000000000000/' "$backend/delivery-build/build.env"
fail_expected 'prepare-source-sha-mismatch' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-sha"
cp "$scratch/receipt-save" "$backend/delivery-build/build.env"
printf 'extra\n' >> "$backend/releases/1.0.0-alpha.83/sys.config"
fail_expected 'prepare-config-tampered' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-config"
cp "$repo/config/sys.config.example" "$backend/releases/1.0.0-alpha.83/sys.config"
rm "$pg/share/extension/timescaledb.control"
fail_expected 'prepare-required-extension-missing' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-ext"
printf "default_version = '1.0'\n" > "$pg/share/extension/timescaledb.control"
cp "$pg/bin/pg_config" "$scratch/pg-config-save"
sed -i 's|/opt/imboy/runtime/postgresql|/wrong/compiled/prefix|' "$pg/bin/pg_config"
fail_expected 'prepare-pg-compiled-prefix-mismatch' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-prefix"
cp "$scratch/pg-config-save" "$pg/bin/pg_config"
printf 'corrupt\n' >> "$scratch/garage"
fail_expected 'prepare-garage-hash-mismatch' bash "$kit/tools/prepare-payload.sh" "${args[@]}" --out "$scratch/no-garage"
printf 'TOTAL_PASS=%s\nSCOPE=offline_synthetic_contracts_only\n' "$passed"
