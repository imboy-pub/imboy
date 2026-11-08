#!/usr/bin/env bash
# Build a native relx artifact from an immutable local Git commit.
# The source checkout is read-only. All writes occur in a new output directory.
set -Eeuo pipefail
umask 077
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
# shellcheck source=lib-payload.sh
. "$TOOLS_DIR/lib-payload.sh"

usage() {
  cat <<'EOF'
用法:
  bash tools/build-backend.sh --repo /abs/path/imboy \
    --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af --out /abs/path/new-build [--jobs 4]

发布方在目标同发行版/架构的 Linux 主机运行；需事先装好 Erlang/OTP 29、
Git、GNU make、编译工具、curl、jq、file、binutils 等依赖。
不运行 Python 或容器、不修改源仓，不启动应用或连接数据库。
会在新 out/work/imboy 内构建，输出 out/backend（含 ERTS、CLI、构建收据）。
首次构建按固定源码 include/deps.mk 拉取依赖；网络/代理由发布方准备。
工作目录名固定为 imboy，避免 relx 的 app_not_found。
失败保留构建目录和日志；重新运行须选择新的 --out，不清理已有目录。
EOF
}

repo= sha= out= jobs=2
while (($#)); do
  case "$1" in
    --repo|--sha|--out|--jobs)
      (($# >= 2)) || payload_die "选项缺参数: $1"
      case "$1" in --repo) repo=$2;; --sha) sha=$2;; --out) out=$2;; --jobs) jobs=$2;; esac
      shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) payload_die "未知参数: $1" ;;
  esac
done
[[ -n $repo && -n $sha && -n $out ]] || { usage >&2; exit 2; }
[[ $sha =~ ^[0-9a-f]{40}$ ]] || payload_die '--sha 必须为完整小写40位 commit SHA'
[[ $jobs =~ ^[1-9][0-9]?$ ]] || payload_die '--jobs 必须为 1..99'
for cmd in bash git make erl erlc tar cp sed awk grep find sort sha256sum realpath readlink file ldd jq curl cmp install cut; do payload_need "$cmd"; done
payload_os
repo=$(payload_absolute_existing "$repo")
out=$(realpath -m -- "$out")
payload_safe_path "$repo"
payload_safe_path "$out"
[[ ! -e $out && ! -L $out ]] || payload_die "输出目录已存在，拒绝覆盖: $out"
[[ $out != "$repo" && $out != "$repo"/* ]] || payload_die '输出不能放在源仓内'
[[ $(git -C "$repo" rev-parse --verify "$sha^{commit}") == "$sha" ]] || payload_die '本地仓找不到指定 commit'
otp=$(erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' 2>/dev/null)
[[ $otp == 29 ]] || payload_die "需要 OTP 29；实际 $otp"

mkdir -p -- "$out/work/imboy" "$out/no-interpreters"
trap 'rc=$?; if ((rc)); then printf "构建失败（退出码 %s），已保留 %s；未修改源仓。\n" "$rc" "$out" >&2; fi' EXIT
git -C "$repo" archive --format=tar "$sha" | tar -xf - -C "$out/work/imboy"
build_dir=$out/work/imboy
version=$(tr -d '\r\n' < "$build_dir/VERSION")
[[ $version =~ ^[A-Za-z0-9][A-Za-z0-9._-]{0,79}$ ]] || payload_die 'VERSION 非法'
grep -Fq "{release, {imboy, \"$version\"}" "$build_dir/relx.config" || payload_die 'VERSION 与 relx.config 不一致；不要假设 RELX_REL_VSN 能覆盖'
grep -Eq '^\{dev_mode,[[:space:]]*false\}\.' "$build_dir/relx.config" || payload_die 'relx 必须 dev_mode=false'
grep -Eq '^\{include_erts,[[:space:]]*true\}\.' "$build_dir/relx.config" || payload_die 'relx 必须 include_erts=true'
[[ $(jq -er .profile "$build_dir/config/product-feature-manifest.json") == phase1_delivery ]] || payload_die '只支持已冻结的 phase1_delivery 编译档'
grep -Fq '<<"phase1_delivery">>' "$build_dir/include/generated/imboy_product_features.hrl" || payload_die '编译档与已生成 Erlang 特性头不一致'
cp "$build_dir/config/sys.config.example" "$build_dir/config/sys.config"

# These guards make forbidden transitive calls fail instead of silently adding
# an interpreter/container prerequisite. No upstream gate is disabled.
for forbidden in python python2 python3 docker docker-compose; do
  cat > "$out/no-interpreters/$forbidden" <<'EOF'
#!/usr/bin/env bash
printf '发布构建拒绝调用禁止的解释器/容器工具: %s\n' "${0##*/}" >&2
exit 91
EOF
  chmod 755 "$out/no-interpreters/$forbidden"
done

# Gitee smart-HTTP may redirect to a sign-in page. Reuse the exact public
# source archive pin recorded by this fixed source's Dockerfile, without Docker.
pay_commit=$(sed -n 's/^ARG ERLANG_PAY_COMMIT=//p' "$build_dir/Dockerfile")
pay_hash=$(sed -n 's/^ARG ERLANG_PAY_ARCHIVE_SHA256=//p' "$build_dir/Dockerfile")
[[ $pay_commit =~ ^[0-9a-f]{40}$ && $pay_hash =~ ^[0-9a-f]{64}$ ]] || payload_die '源码未提供可核对的 erlang_pay 归档 pin'
mkdir -p "$build_dir/deps" "$out/downloads"
curl --fail --location --proto '=https' --tlsv1.2 --retry 2 \
  "https://gitee.com/imboy-pub/erlang_pay/repository/archive/$pay_commit.tar.gz" \
  -o "$out/downloads/erlang_pay.tar.gz"
printf '%s  %s\n' "$pay_hash" "$out/downloads/erlang_pay.tar.gz" | sha256sum --quiet -c -
tar -tzf "$out/downloads/erlang_pay.tar.gz" | awk -v root="erlang_pay-$pay_commit/" '
  index($0,root)!=1 || /(^|\/)\.\.(\/|$)/ { bad=1 } END { exit bad ? 1 : 0 }
' || payload_die 'erlang_pay 归档路径不合法'
tar -xzf "$out/downloads/erlang_pay.tar.gz" -C "$out/downloads" --no-same-owner
mv "$out/downloads/erlang_pay-$pay_commit" "$build_dir/deps/erlang_pay"
{
  printf '%s\n' '.PHONY: all' 'all:'
  printf '\t%s\n' '../../.erlang.mk/rebar3/rebar3 compile' 'mkdir -p ebin' 'cp _build/default/lib/erlang_pay/ebin/* ebin/'
} > "$build_dir/deps/erlang_pay/Makefile"

printf '构建固定 commit %s，版本 %s；日志保存在新输出目录。\n' "$sha" "$version"
(
  cd "$build_dir"
  export PATH="$out/no-interpreters:$PATH"
  # This archived tree contains only the public example, never local config.
  unset IMBOYENV
  make -j "$jobs" rel RELX_TAR=0
) > "$out/build.log" 2>&1 || payload_die "make rel 失败；查阅 $out/build.log（分享前脱敏）"

[[ -x $build_dir/_rel/imboy/bin/imboy ]] || payload_die '构建没有生成 bin/imboy'
payload_check_tree "$build_dir/_rel/imboy"
mkdir -p "$out/backend"
# Relx dev_mode=false should be self-contained. Expand only checked internal
# links so every delivered byte is directly covered by the hash manifest.
cp -aL "$build_dir/_rel/imboy/." "$out/backend/"
backend=$out/backend
cmp -s "$build_dir/config/sys.config.example" "$backend/releases/$version/sys.config" || payload_die 'release sys.config 不等于无秘密模板，拒绝出包'
cmp -s "$build_dir/config/vm.args" "$backend/releases/$version/vm.args" || payload_die 'release vm.args 与源码模板不一致'
[[ -d $backend/lib/imboy-$version/priv/migrations ]] || payload_die 'release 缺少 priv/migrations'
install -m 755 "$build_dir/scripts/imboy_ctl" "$backend/bin/imboy_ctl"
mkdir -p "$backend/delivery-build"
cp "$build_dir/config/product-feature-manifest.json" "$backend/delivery-build/product-feature-manifest.json"
cp "$build_dir/include/generated/imboy_product_features.hrl" "$backend/delivery-build/imboy_product_features.hrl"
cp "$build_dir/LICENSE" "$backend/delivery-build/LICENSE"
git -C "$repo" show -s --format='%H%n%cs%n%s' "$sha" > "$backend/delivery-build/source.txt"
for dep in "$build_dir"/deps/*; do
  [[ -d $dep ]] || continue
  if [[ -d $dep/.git || -f $dep/.git ]]; then
    printf '%s\t%s\n' "${dep##*/}" "$(git -C "$dep" rev-parse HEAD)"
  elif [[ ${dep##*/} == erlang_pay ]]; then
    printf 'erlang_pay\t%s\tarchive_sha256=%s\n' "$pay_commit" "$pay_hash"
  else
    printf '%s\thex-or-transitive-content-pinned-by-BUILD_FILES\n' "${dep##*/}"
  fi
done > "$backend/delivery-build/dependency-commits.tsv"
cat > "$backend/delivery-build/build.env" <<EOF
BUILD_FORMAT=1
SOURCE_SHA=$sha
BACKEND_VERSION=$version
OS_ID=$PAYLOAD_OS_ID
OS_VERSION_ID=$PAYLOAD_OS_VERSION_ID
ARCH=$PAYLOAD_ARCH
OTP_RELEASE=$otp
PRODUCT_PROFILE=phase1_delivery
BUILD_TEST_STATUS=NOT_RUN
EOF
payload_check_tree "$backend"
payload_check_dynamic_libraries "$backend"
erts_bins=("$backend"/erts-*/bin/erlexec)
[[ ${#erts_bins[@]} == 1 && -x ${erts_bins[0]} ]] || payload_die 'ERTS 缺失或存在多个版本'
erts_bindir=$(dirname "${erts_bins[0]}")
env ROOTDIR="$backend" BINDIR="$erts_bindir" EMU=beam PROGNAME=erl \
  "${erts_bins[0]}" -version > "$backend/delivery-build/erts-version.txt" 2>&1
payload_normalize_permissions "$backend"
payload_hash_tree "$backend" BUILD_FILES.sha256
payload_verify_hashes "$backend" BUILD_FILES.sha256
payload_check_permissions "$backend"
printf '原生后端构建完成: %s\n未运行真实数据库/业务/干净机验收。下一步用 prepare-payload.sh 封包。\n' "$backend"
