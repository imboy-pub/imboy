#!/usr/bin/env bash
# Publisher-only source recipe translated from the fixed native CI input pins.
# No server is started; no database is created. This recipe is not an install test.
set -Eeuo pipefail
umask 077
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck source=lib-payload.sh
. "$TOOLS_DIR/lib-payload.sh"

usage() {
  cat <<'EOF'
发布方专用，需独立构建机；不要在已部署的服务器上编译。

bash tools/build-native-dependencies.sh --repo /abs/imboy \
  --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
  --work /abs/new-native-build --otp-prefix /abs/new-otp29 \
  --pg-prefix /opt/imboy/runtime/postgresql [--jobs 2]

所有输出目录必须不存在，并且调用者有创建权限。工具不会sudo、安装apt包，
也不会覆盖已安装的PostgreSQL。预计为较长的源码构建；日志只保留在work内。
PG前缀固定为 /opt/imboy/runtime/postgresql，不支持编译后更换安装前缀。
先在构建机通过可信发行版仓库安装（无需安装Python）：
  build-essential make pkg-config curl ca-certificates git perl jq file \
  bison flex m4 autoconf automake libtool cmake libncurses-dev libreadline-dev \
  libssl-dev zlib1g-dev libicu-dev libxml2-dev uuid-dev libgeos-dev libproj-dev \
  libjson-c-dev libprotobuf-c-dev protobuf-c-compiler protobuf-compiler \
  libpcre2-dev liblz4-dev libzstd-dev libedit-dev xsltproc unzip xz-utils bzip2

构建OTP29、PG18和12项扩展，下载固定Garage2.4.1，并校验源码pin/子模块/词典。
未运行实装、SQL迁移、登录、真实消息、附件或恢复验收。
EOF
}

repo= sha= work= otp_prefix= pg_prefix= jobs=2
while (($#)); do
  case "$1" in
    --repo|--sha|--work|--otp-prefix|--pg-prefix|--jobs)
      (($# >= 2)) || payload_die "选项缺参数: $1"
      case "$1" in
        --repo) repo=$2;; --sha) sha=$2;; --work) work=$2;;
        --otp-prefix) otp_prefix=$2;; --pg-prefix) pg_prefix=$2;; --jobs) jobs=$2;;
      esac
      shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) payload_die "未知参数: $1" ;;
  esac
done
[[ -n $repo && -n $sha && -n $work && -n $otp_prefix && -n $pg_prefix ]] || { usage >&2; exit 2; }
[[ $sha =~ ^[0-9a-f]{40}$ && $jobs =~ ^[1-9][0-9]?$ ]] || payload_die 'SHA或jobs非法'
for cmd in bash git curl jq make cc c++ cmake tar bzip2 xz perl pkg-config bison flex geos-config sha256sum file ldd realpath; do payload_need "$cmd"; done
payload_os
repo=$(payload_absolute_existing "$repo")
work=$(realpath -m "$work")
otp_prefix=$(realpath -m "$otp_prefix")
pg_prefix=$(realpath -m "$pg_prefix")
[[ $pg_prefix == /opt/imboy/runtime/postgresql ]] || payload_die '--pg-prefix 必须为 /opt/imboy/runtime/postgresql，与目标运行路径完全一致'
for path in "$repo" "$work" "$otp_prefix" "$pg_prefix"; do payload_safe_path "$path"; done
for path in "$work" "$otp_prefix" "$pg_prefix"; do
  [[ ! -e $path && ! -L $path ]] || payload_die "输出已存在，拒绝覆盖: $path"
  [[ $path != "$repo" && $path != "$repo"/* ]] || payload_die '输出不能放在源码仓中'
done
[[ $otp_prefix != "$pg_prefix" && $otp_prefix != "$pg_prefix"/* && $pg_prefix != "$otp_prefix"/* ]] || payload_die 'OTP和PG前缀不能相同或互相嵌套'
[[ $work != "$otp_prefix" && $work != "$otp_prefix"/* && $work != "$pg_prefix" && $work != "$pg_prefix"/* ]] || payload_die 'work不能嵌套在产物prefix中'
[[ $(git -C "$repo" rev-parse --verify "$sha^{commit}") == "$sha" ]] || payload_die '指定commit不存在'
mkdir -p "$work/sources" "$work/downloads" "$work/no-interpreters"
trap 'rc=$?; if ((rc)); then printf "依赖构建失败（%s），目录保留在 %s；不完整prefix禁止交付。\n" "$rc" "$work" >&2; fi' EXIT
git -C "$repo" show "$sha:scripts/ci/native-eunit-pins.json" > "$work/pins.json"
for forbidden in python python2 python3 docker docker-compose; do
  cat > "$work/no-interpreters/$forbidden" <<'EOF'
#!/usr/bin/env bash
printf '源码构建拒绝调用禁止的解释器/容器工具: %s\n' "${0##*/}" >&2
exit 91
EOF
  chmod 755 "$work/no-interpreters/$forbidden"
done
export PATH="$work/no-interpreters:$PATH"

fetch_archive() {
  local name=$1 dir=$2 url hash
  url=$(jq -er --arg name "$name" '.sources[] | select(.file==$name) | .url' "$work/pins.json")
  hash=$(jq -er --arg name "$name" '.sources[] | select(.file==$name) | .sha256' "$work/pins.json")
  [[ $url == https://* && $hash =~ ^[0-9a-f]{64}$ ]] || payload_die "源码pin非法: $name"
  curl --fail --location --proto '=https' --tlsv1.2 --retry 2 "$url" -o "$work/downloads/$name"
  printf '%s  %s\n' "$hash" "$work/downloads/$name" | sha256sum --quiet -c -
  tar -tf "$work/downloads/$name" | awk -v root="$dir/" '
    index($0,root)!=1 || /(^|\/)\.\.(\/|$)/ { bad=1 } END { exit bad ? 1 : 0 }
  ' || payload_die "源码归档路径非法: $name"
  tar -xf "$work/downloads/$name" -C "$work/sources" --no-same-owner
}
fetch_git() {
  local dir=$1 url commit subpath subcommit
  url=$(jq -er --arg dir "$dir" '.sources[] | select(.directory==$dir) | .url' "$work/pins.json")
  commit=$(jq -er --arg dir "$dir" '.sources[] | select(.directory==$dir) | .commit' "$work/pins.json")
  [[ $url == https://* && $commit =~ ^[0-9a-f]{40}$ ]] || payload_die "git pin非法: $dir"
  git clone --no-checkout "$url" "$work/sources/$dir" > "$work/git-$dir.txt" 2>&1
  git -C "$work/sources/$dir" checkout --detach "$commit" >> "$work/git-$dir.txt" 2>&1
  [[ $(git -C "$work/sources/$dir" rev-parse HEAD) == "$commit" ]] || payload_die "git commit不同: $dir"
  git -C "$work/sources/$dir" submodule update --init --recursive >> "$work/git-$dir.txt" 2>&1
  while IFS=$'\t' read -r subpath subcommit; do
    [[ $subpath != /* && $subpath != *..* && $subcommit =~ ^[0-9a-f]{40}$ ]] || payload_die '子模块pin非法'
    [[ $(git -C "$work/sources/$dir/$subpath" rev-parse HEAD) == "$subcommit" ]] || payload_die "子模块commit不同: $dir/$subpath"
  done < <(jq -r --arg dir "$dir" '.sources[] | select(.directory==$dir) | (.submodule_commits // {}) | to_entries[] | [.key,.value] | @tsv' "$work/pins.json")
}
run_build() {
  local label=$1 directory=$2; shift 2
  printf '构建 %s；日志 %s\n' "$label" "$work/$label.txt"
  (cd "$directory" && "$@") > "$work/$label.txt" 2>&1 || payload_die "构建失败: $label；查看对应日志（分享前脱敏）"
}

# Versions deliberately match the pinned native CI; never silently use latest.
fetch_archive otp_src_29.1.2.tar.gz otp_src_29.1.2
fetch_archive postgresql-18.6.tar.bz2 postgresql-18.6
fetch_archive postgis-3.6.4.tar.gz postgis-3.6.4
fetch_git pgvector-0.8.7
fetch_git timescaledb-2.23.1
fetch_git pg_jieba
run_build otp-configure "$work/sources/otp_src_29.1.2" ./configure --prefix="$otp_prefix" --without-javac --without-wx --without-odbc --with-ssl=/usr
run_build otp-build "$work/sources/otp_src_29.1.2" make -j "$jobs"
run_build otp-install "$work/sources/otp_src_29.1.2" make install
export PATH="$otp_prefix/bin:$PATH"
run_build pg-configure "$work/sources/postgresql-18.6" ./configure --prefix="$pg_prefix" --with-openssl --with-libxml --with-icu --with-uuid=e2fs --without-llvm
run_build pg-build "$work/sources/postgresql-18.6" make -j "$jobs"
run_build pg-install "$work/sources/postgresql-18.6" make install
run_build pg-contrib-build "$work/sources/postgresql-18.6" make -C contrib -j "$jobs"
run_build pg-contrib-install "$work/sources/postgresql-18.6" make -C contrib install
export PATH="$pg_prefix/bin:$PATH"
export PG_CONFIG="$pg_prefix/bin/pg_config"
export LD_LIBRARY_PATH="$pg_prefix/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
run_build pgvector-build "$work/sources/pgvector-0.8.7" make -j "$jobs" PG_CONFIG="$PG_CONFIG" OPTFLAGS=
run_build pgvector-install "$work/sources/pgvector-0.8.7" make install PG_CONFIG="$PG_CONFIG"
run_build jieba-configure "$work/sources/pg_jieba" cmake -S . -B build -DCMAKE_BUILD_TYPE=Release \
  -DPostgreSQL_ROOT="$pg_prefix" -DPostgreSQL_PG_CONFIG="$PG_CONFIG" \
  -DPostgreSQL_INCLUDE_DIR="$pg_prefix/include" -DPostgreSQL_LIBRARY="$pg_prefix/lib/libpq.so" \
  -DPostgreSQL_TYPE_INCLUDE_DIR="$("$PG_CONFIG" --includedir-server)"
run_build jieba-build "$work/sources/pg_jieba" cmake --build build -j "$jobs"
run_build jieba-install "$work/sources/pg_jieba" cmake --install build
run_build timescale-configure "$work/sources/timescaledb-2.23.1" ./bootstrap -DPG_CONFIG="$PG_CONFIG" -DCMAKE_BUILD_TYPE=Release -DREGRESS_CHECKS=OFF -DWARNINGS_AS_ERRORS=OFF
run_build timescale-build "$work/sources/timescaledb-2.23.1" cmake --build build -j "$jobs"
run_build timescale-install "$work/sources/timescaledb-2.23.1" cmake --install build
run_build postgis-configure "$work/sources/postgis-3.6.4" ./configure --prefix="$pg_prefix" --with-pgconfig="$PG_CONFIG" --with-geosconfig="$(command -v geos-config)" --without-raster --without-sfcgal --without-gui
run_build postgis-build "$work/sources/postgis-3.6.4" make -j "$jobs"
run_build postgis-install "$work/sources/postgis-3.6.4" make install
git -C "$repo" show "$sha:docker/pg_jieba_userdict.txt.big" > "$work/jieba_user.dict"
dictionary_hash=$(jq -er '.dictionary.sha256' "$work/pins.json")
printf '%s  %s\n' "$dictionary_hash" "$work/jieba_user.dict" | sha256sum --quiet -c -
install -m 644 "$work/jieba_user.dict" "$pg_prefix/share/tsearch_data/jieba_user.dict"
garage_url=$(jq -er '.garage.url' "$work/pins.json")
garage_hash=$(jq -er '.garage.sha256' "$work/pins.json")
[[ $garage_url == https://garagehq.deuxfleurs.fr/* && $garage_hash =~ ^[0-9a-f]{64}$ ]] || payload_die 'Garage pin非法'
curl --fail --location --proto '=https' --tlsv1.2 --retry 2 "$garage_url" -o "$work/garage"
printf '%s  %s\n' "$garage_hash" "$work/garage" | sha256sum --quiet -c -
chmod 755 "$work/garage"
"$work/garage" --version
mkdir -p "$pg_prefix/delivery-build/licenses"
cp "$work/pins.json" "$pg_prefix/delivery-build/native-pins.json"
cp "$work/sources/postgresql-18.6/COPYRIGHT" "$pg_prefix/delivery-build/licenses/PostgreSQL-COPYRIGHT"
for source in "$work/sources/postgis-3.6.4/COPYING" "$work/sources/timescaledb-2.23.1/LICENSE" "$work/sources/timescaledb-2.23.1/tsl/LICENSE-TIMESCALE" "$work/sources/pgvector-0.8.7/LICENSE" "$work/sources/pg_jieba/LICENSE"; do
  [[ -f $source ]] || continue
  component=$(basename "$(dirname "$source")")
  cp "$source" "$pg_prefix/delivery-build/licenses/$component-$(basename "$source")"
done
printf 'SOURCE_SHA=%s\nOS_ID=%s\nOS_VERSION_ID=%s\nARCH=%s\nPG_VERSION=18.6\nBUILD_TEST_STATUS=NOT_RUN\n' \
  "$sha" "$PAYLOAD_OS_ID" "$PAYLOAD_OS_VERSION_ID" "$PAYLOAD_ARCH" > "$pg_prefix/delivery-build/build.env"
payload_check_dynamic_libraries "$pg_prefix"
printf '依赖构建完成。OTP: %s；PG prefix: %s；Garage: %s/garage\n' "$otp_prefix" "$pg_prefix" "$work"
printf '接下来把OTP的bin加入PATH，执行 build-backend.sh，再执行 prepare-payload.sh。未运行真实安装验收。\n'
