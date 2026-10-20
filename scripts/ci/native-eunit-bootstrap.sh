#!/usr/bin/env bash
# Adapted from audited native rebuild scripts; no PostgreSQL binary packages.
set -euo pipefail
[[ ${NATIVE_EPHEMERAL_KEYS_APPROVED:-false} == true ]] || { echo 'Disposable fixture-key approval required before provisioning'; exit 2; }
[[ ${GITHUB_ACTIONS:-} == true && $(uname -m) == x86_64 ]] || { echo 'Requires approved GitHub Ubuntu x86_64 VM'; exit 2; }
source /etc/os-release
[[ "$ID" == ubuntu && "$VERSION_ID" == 24.04 ]] || { echo 'Requires Ubuntu24.04'; exit 2; }
: "${NATIVE_CI_ROOT:?}" "${NATIVE_CI_ARTIFACTS:?}"
REPO=$(pwd)
JOBS=${JOBS:-2}
[[ ! -e "$NATIVE_CI_ROOT" ]] || { echo 'Refusing to reuse a native build directory'; exit 2; }
mkdir -m 700 "$NATIVE_CI_ROOT"
mkdir -p "$NATIVE_CI_ROOT/prefix" "$NATIVE_CI_ARTIFACTS"
sudo -n apt-get update
sudo -n apt-get install -y --no-install-recommends \
  build-essential make pkg-config curl ca-certificates git perl python3 \
  bison flex m4 autoconf automake libtool cmake libncurses-dev libreadline-dev \
  libssl-dev zlib1g-dev libicu-dev libxml2-dev uuid-dev libgeos-dev libproj-dev \
  libjson-c-dev libprotobuf-c-dev protobuf-c-compiler protobuf-compiler libpcre2-dev liblz4-dev \
  libzstd-dev libedit-dev xsltproc unzip xz-utils
python3 scripts/ci/native-eunit-fetch.py
PREFIX="$NATIVE_CI_ROOT/prefix"
export PATH="$PREFIX/otp/bin:$PREFIX/pg/bin:$PREFIX/rust/bin:$PREFIX/bin:$PATH:/usr/sbin"
export LD_LIBRARY_PATH="$PREFIX/pg/lib:${LD_LIBRARY_PATH:-}"
export PG_CONFIG="$PREFIX/pg/bin/pg_config"
export ERL_FLAGS='+S 4:4 +A 4'
cd "$NATIVE_CI_ROOT/sources/otp_src_29.1.2"
./configure --prefix="$PREFIX/otp" --without-javac --without-wx --without-odbc --with-ssl=/usr
make -j"$JOBS"
make install
cd "$NATIVE_CI_ROOT/sources/rebar3-3.27.3"
env -u http_proxy -u https_proxy ./bootstrap
cp rebar3 "$PREFIX/bin/rebar3"
cd "$NATIVE_CI_ROOT/sources/postgresql-18.6"
./configure --prefix="$PREFIX/pg" --with-openssl --with-libxml --with-icu --with-uuid=e2fs --without-llvm
make -j"$JOBS"
make install
make -C contrib -j"$JOBS"
make -C contrib install
cd "$NATIVE_CI_ROOT/sources/pgvector-0.8.7"
make -j"$JOBS" PG_CONFIG="$PG_CONFIG" OPTFLAGS=''
make install PG_CONFIG="$PG_CONFIG"
cd "$NATIVE_CI_ROOT/sources/pg_jieba"
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
  -DPostgreSQL_ROOT="$PREFIX/pg" -DPostgreSQL_PG_CONFIG="$PG_CONFIG" \
  -DPostgreSQL_INCLUDE_DIR="$PREFIX/pg/include" -DPostgreSQL_LIBRARY="$PREFIX/pg/lib/libpq.so" \
  -DPostgreSQL_TYPE_INCLUDE_DIR="$("$PG_CONFIG" --includedir-server)"
cmake --build build -j"$JOBS"
cmake --install build
cd "$NATIVE_CI_ROOT/sources/timescaledb-2.23.1"
./bootstrap -DPG_CONFIG="$PG_CONFIG" -DCMAKE_BUILD_TYPE=Release -DREGRESS_CHECKS=OFF -DWARNINGS_AS_ERRORS=OFF
cmake --build build -j"$JOBS"
cmake --install build
cd "$NATIVE_CI_ROOT/sources/postgis-3.6.4"
./configure --prefix="$PREFIX/postgis" --with-pgconfig="$PG_CONFIG" \
  --with-geosconfig=/usr/bin/geos-config --without-raster --without-sfcgal --without-gui
make -j"$JOBS"
make install
for component in rustc cargo rust-std; do
  bash "$NATIVE_CI_ROOT/sources/$component-1.97.1-x86_64-unknown-linux-gnu/install.sh" \
    --prefix="$PREFIX/rust" --disable-ldconfig
done
cd "$REPO"
python3 - "$REPO" <<'PY'
import hashlib,json,os,pathlib,sys
repo=pathlib.Path(sys.argv[1]); pins=json.loads((repo/'scripts/ci/native-eunit-pins.json').read_text())
p=repo/pins['dictionary']['path']
assert hashlib.file_digest(p.open('rb'),'sha256').hexdigest()==pins['dictionary']['sha256'], 'Dictionary differs from audited input'
PY
install -m644 docker/pg_jieba_userdict.txt.big "$(pg_config --sharedir)/tsearch_data/jieba_user.dict"
IMBOYENV=local make test-build
python3 - "$REPO" <<'PY'
import hashlib,json,pathlib,subprocess,sys
repo=pathlib.Path(sys.argv[1]); pins=json.loads((repo/'scripts/ci/native-eunit-pins.json').read_text())
hird=repo/'deps/hird_runtime'
assert subprocess.check_output(['git','-C',str(hird),'rev-parse','HEAD'],text=True).strip()==pins['hird']['commit']
assert hashlib.file_digest((hird/'Cargo.lock').open('rb'),'sha256').hexdigest()==pins['hird']['cargo_lock_sha256']
PY
# Use the exact native cargo/rustc, bypassing any runner rustup override.
RUSTC="$PREFIX/rust/bin/rustc" "$PREFIX/rust/bin/cargo" build --locked --release --manifest-path deps/hird_runtime/Cargo.toml -p hird-cli
TEXTGUARD_SOURCE="$NATIVE_CI_ROOT/sources/imboy-textguard"
python3 - "$REPO" "$TEXTGUARD_SOURCE" <<'PY'
import hashlib,json,pathlib,subprocess,sys
repo,source=map(pathlib.Path,sys.argv[1:]); pin=json.loads((repo/'scripts/ci/native-eunit-pins.json').read_text())['textguard']
assert subprocess.check_output(['git','-C',str(source),'rev-parse','HEAD'],text=True).strip()==pin['commit']
assert hashlib.file_digest((source/'Cargo.lock').open('rb'),'sha256').hexdigest()==pin['cargo_lock_sha256']
PY
RUSTC="$PREFIX/rust/bin/rustc" "$PREFIX/rust/bin/cargo" build --locked --release \
  --manifest-path "$TEXTGUARD_SOURCE/Cargo.toml" --target-dir "$NATIVE_CI_ROOT/build/textguard"
{
  printf 'export PATH=%q\n' "$REPO/deps/hird_runtime/target/release:$PATH"
  printf 'export TEXTGUARD_TEST_BINARY=%q\n' "$NATIVE_CI_ROOT/build/textguard/release/imboy-textguard"
  printf 'export LD_LIBRARY_PATH=%q\n' "$LD_LIBRARY_PATH"
  printf 'export PG_CONFIG=%q\n' "$PG_CONFIG"
  printf 'export ERL_FLAGS=%q\n' "$ERL_FLAGS"
} > "$NATIVE_CI_ROOT/activate.sh"
{
  erl -noshell -eval 'io:format("~s~n",[erlang:system_info(system_version)]), {ok,_}=application:ensure_all_started(ssl), halt().'
  rebar3 version
  pg_config --version
  rustc --version --verbose
  cargo --version
  garage --version
  deps/hird_runtime/target/release/hird --version
  dpkg-query -W -f='${Package} ${Version}\n' | sort
} > "$NATIVE_CI_ARTIFACTS/toolchain-versions.txt"
cp scripts/ci/native-eunit-pins.json "$NATIVE_CI_ARTIFACTS/public-input-pins.json"
echo 'NATIVE_SOURCE_BUILDS_COMPLETE'
