#!/usr/bin/env bash
# Assemble a native customer candidate from trusted publisher builds.
set -Eeuo pipefail
umask 077
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
PACKAGE_ROOT=$(cd "$TOOLS_DIR/.." && pwd -P)
# shellcheck source=lib-payload.sh
. "$TOOLS_DIR/lib-payload.sh"
# shellcheck source=lib-host-packages.sh
. "$TOOLS_DIR/lib-host-packages.sh"

usage() {
  cat <<'EOF'
用法:
  bash tools/prepare-payload.sh --repo /abs/imboy \
    --sha cb95fa72f9bc617d0f30ddd6b98da51f39cfd2af \
    --release-dir /abs/new-build/backend \
    --pg-prefix /opt/imboy/runtime/postgresql --garage-bin /abs/garage \
    --release-id native-20261105-cb95fa72 --out /abs/customer-candidate

发布方操作。只读源仓和三个输入产物，写入全新的客户候选目录。
release-dir 必须有 build-backend.sh 生成的收据。PG prefix 必须包含完整
bin/lib/share（含12项必要扩展和 jieba词典），构建时 --prefix 必须为
/opt/imboy/runtime/postgresql，与目标安装路径一致。
只接受同发行版/架构的可信产物，不接受数据库数据目录。
Garage 固定2.4.1，SHA256按源码 native-eunit-pins.json校验。
输出包含部署工具/手册、payload、metadata和CHECKSUMS.sha256。
不复制源仓、build.log、私有配置、密钥、数据或现场演练证据。
现场 site.env 不进入完整性清单；候选必须完成干净机演练才可正式交付。
EOF
}

repo= sha= release_dir= pg_prefix= garage_bin= release_id= out=
while (($#)); do
  case "$1" in
    --repo|--sha|--release-dir|--pg-prefix|--garage-bin|--release-id|--out)
      (($# >= 2)) || payload_die "选项缺参数: $1"
      case "$1" in
        --repo) repo=$2;; --sha) sha=$2;; --release-dir) release_dir=$2;;
        --pg-prefix) pg_prefix=$2;; --garage-bin) garage_bin=$2;;
        --release-id) release_id=$2;; --out) out=$2;;
      esac
      shift 2 ;;
    --help|-h) usage; exit 0 ;;
    *) payload_die "未知参数: $1" ;;
  esac
done
[[ -n $repo && -n $sha && -n $release_dir && -n $pg_prefix && -n $garage_bin && -n $release_id && -n $out ]] || { usage >&2; exit 2; }
[[ $sha =~ ^[0-9a-f]{40}$ ]] || payload_die '--sha 必须为完整小写40位 commit SHA'
[[ $release_id =~ ^[A-Za-z0-9][A-Za-z0-9._-]{0,79}$ && $release_id != *..* ]] || payload_die 'release-id 仅允许字母数字 ._-，禁止连续点'
for cmd in bash git tar cp install awk grep find sort sha256sum realpath readlink file ldd jq cmp cut sed date dpkg dpkg-query apt-cache stat chmod mktemp; do payload_need "$cmd"; done
payload_os
repo=$(payload_absolute_existing "$repo")
release_dir=$(payload_absolute_existing "$release_dir")
pg_prefix=$(payload_absolute_existing "$pg_prefix")
garage_bin=$(payload_absolute_existing "$garage_bin")
out=$(realpath -m -- "$out")
for path in "$repo" "$release_dir" "$pg_prefix" "$garage_bin" "$out"; do payload_safe_path "$path"; done
[[ ! -e $out && ! -L $out ]] || payload_die "输出已存在，拒绝覆盖: $out"
for src in "$PACKAGE_ROOT" "$repo" "$release_dir" "$pg_prefix"; do
  [[ $out != "$src" && $out != "$src"/* ]] || payload_die '输出不得嵌套在工具包或输入目录中'
done
for required in install.sh verify.sh backup.sh restore.sh upgrade.sh admin-init.sh lib/common.sh lib/render.sh lib/services.sh templates/site.env.example README.md; do
  [[ -f $PACKAGE_ROOT/$required ]] || payload_die "交付工具包缺少必需文件: $required"
done
[[ $(git -C "$repo" rev-parse --verify "$sha^{commit}") == "$sha" ]] || payload_die '源仓没有固定 commit'
receipt=$release_dir/delivery-build/build.env
[[ -f $receipt ]] || payload_die 'release 缺少 build-backend.sh 构建收据；请在干净目录重建'
[[ $(payload_value "$receipt" BUILD_FORMAT) == 1 ]] || payload_die '构建收据版本不支持'
[[ $(payload_value "$receipt" SOURCE_SHA) == "$sha" ]] || payload_die 'release 与源仓 SHA 不同'
for key in OS_ID OS_VERSION_ID ARCH; do
  case "$key" in OS_ID) expected=$PAYLOAD_OS_ID;; OS_VERSION_ID) expected=$PAYLOAD_OS_VERSION_ID;; ARCH) expected=$PAYLOAD_ARCH;; esac
  [[ $(payload_value "$receipt" "$key") == "$expected" ]] || payload_die "release 构建平台不同: $key"
done
[[ $(payload_value "$receipt" OTP_RELEASE) == 29 ]] || payload_die 'release 不为 OTP29'
[[ $(payload_value "$receipt" PRODUCT_PROFILE) == phase1_delivery ]] || payload_die 'release 产品档不同'
version=$(git -C "$repo" show "$sha:VERSION" | tr -d '\r\n')
[[ $(payload_value "$receipt" BACKEND_VERSION) == "$version" ]] || payload_die 'release版本与源码不同'
[[ -x $release_dir/bin/imboy ]] || payload_die 'release缺少可执行bin/imboy'
erts_bins=("$release_dir"/erts-*/bin/erlexec)
[[ ${#erts_bins[@]} == 1 && -x ${erts_bins[0]} ]] || payload_die 'release必须含唯一的ERTS运行时'
payload_check_tree "$release_dir"
payload_verify_hashes "$release_dir" BUILD_FILES.sha256
cmp -s <(git -C "$repo" show "$sha:config/sys.config.example") "$release_dir/releases/$version/sys.config" || payload_die 'release sys.config 不是源码无秘密模板'
config_sha=$(sha256sum "$release_dir/releases/$version/sys.config" | cut -d ' ' -f1)
renderer_sha=$(payload_value "$PACKAGE_ROOT/lib/render.sh" IMBOY_NATIVE_CONFIG_SHA256)
[[ $renderer_sha =~ ^[0-9a-f]{64}$ && $config_sha == "$renderer_sha" ]] || payload_die '原始sys.config不符合当前渲染器合同；发布方必须先适配并验证lib/render.sh，再生成客户候选'
cmp -s <(git -C "$repo" show "$sha:config/vm.args") "$release_dir/releases/$version/vm.args" || payload_die 'release vm.args 不是源码占位模板'
cmp -s <(git -C "$repo" show "$sha:scripts/imboy_ctl") "$release_dir/bin/imboy_ctl" || payload_die 'release CLI与源码不同'
cmp -s <(git -C "$repo" show "$sha:config/product-feature-manifest.json") "$release_dir/delivery-build/product-feature-manifest.json" || payload_die 'release产品manifest与源码不同'
while IFS= read -r path; do
  [[ $path == priv/migrations/*.sql ]] || continue
  cmp -s <(git -C "$repo" show "$sha:$path") "$release_dir/lib/imboy-$version/$path" || payload_die "release迁移缺失或字节不同: $path"
done < <(git -C "$repo" ls-tree -r --name-only "$sha" -- priv/migrations)
cmp -s <(git -C "$repo" ls-tree -r --name-only "$sha" -- priv/migrations | LC_ALL=C sort) \
  <(cd "$release_dir/lib/imboy-$version" && find priv/migrations -type f | LC_ALL=C sort) || payload_die 'release迁移集合与源码不同'
head_migration=$(git -C "$repo" ls-tree -r --name-only "$sha" -- priv/migrations | sed -n 's|^priv/migrations/\([0-9][0-9]*\)_.*\.up\.sql$|\1|p' | sort -n | tail -1)
[[ $head_migration =~ ^[0-9]+$ ]] || payload_die '找不到迁移HEAD'
head_migration=$((10#$head_migration))

for bin in postgres initdb pg_ctl psql pg_dump pg_restore createdb pg_config pg_isready; do
  [[ -x $pg_prefix/bin/$bin ]] || payload_die "PG payload缺少 bin/$bin"
done
pg_version=$("$pg_prefix/bin/pg_config" --version)
[[ $pg_version == 'PostgreSQL 18.'* ]] || payload_die "PG必须为18.x；实际 $pg_version"
pg_version=${pg_version#PostgreSQL }
[[ $pg_version =~ ^18\.[0-9]+$ ]] || payload_die '无法识别PG版本'
pg_configured_prefix=$("$pg_prefix/bin/pg_config" --configure | tr ' ' '\n' | sed "s/^'//;s/'$//" | sed -n 's/^--prefix=//p')
[[ $pg_configured_prefix == /opt/imboy/runtime/postgresql ]] || payload_die 'PG构建--prefix必须为/opt/imboy/runtime/postgresql；不得把其他编译前缀直接重定位为交付物'
pg_share=$("$pg_prefix/bin/pg_config" --sharedir)
pg_lib=$("$pg_prefix/bin/pg_config" --pkglibdir)
[[ $pg_share == "$pg_prefix/share" && $pg_lib == "$pg_prefix/lib" ]] || payload_die 'PG不是完整prefix；bin/lib/share须同一prefix，不能直接复制分散的apt目录'
extensions=(pgcrypto pg_jieba timescaledb vector postgis pg_trgm btree_gin btree_gist citext unaccent intarray pg_stat_statements)
for ext in "${extensions[@]}"; do
  [[ -f $pg_share/extension/$ext.control ]] || payload_die "PG缺少扩展控制文件: $ext"
  compgen -G "$pg_share/extension/$ext--*.sql" >/dev/null || payload_die "PG缺少扩展SQL: $ext"
  module_path=$(sed -n "s/^[[:space:]]*module_pathname[[:space:]]*=[[:space:]]*'\([^']*\)'.*/\1/p" "$pg_share/extension/$ext.control")
  if [[ $module_path == '$libdir/'* ]]; then
    module=${module_path#*/}
    [[ $module =~ ^[A-Za-z0-9_.-]+$ && -s $pg_lib/$module.so ]] || payload_die "PG缺少扩展动态库: $ext"
  elif [[ -n $module_path ]]; then
    payload_die "扩展使用了非可迁移的库路径: $ext"
  fi
done
[[ -s $pg_share/tsearch_data/jieba_user.dict ]] || payload_die 'PG缺少jieba自定义词典'
expected_dictionary=$(git -C "$repo" show "$sha:docker/pg_jieba_userdict.txt.big" | sha256sum | cut -d ' ' -f1)
actual_dictionary=$(sha256sum "$pg_share/tsearch_data/jieba_user.dict" | cut -d ' ' -f1)
[[ $actual_dictionary == "$expected_dictionary" ]] || payload_die 'PG的jieba词典与固定源码不同'
payload_check_tree "$pg_prefix"
payload_check_dynamic_libraries "$release_dir"
payload_check_dynamic_libraries "$pg_prefix"
[[ -x $garage_bin ]] || payload_die 'Garage输入不可执行'
pins=$(git -C "$repo" show "$sha:scripts/ci/native-eunit-pins.json")
expected_garage=$(printf '%s\n' "$pins" | jq -er '.garage.sha256')
garage_version=$(printf '%s\n' "$pins" | jq -er '.garage.version')
[[ $expected_garage =~ ^[0-9a-f]{64}$ && $garage_version == 2.4.1 ]] || payload_die '源码Garage pin不为已支持的2.4.1'
[[ $(sha256sum "$garage_bin" | cut -d ' ' -f1) == "$expected_garage" ]] || payload_die 'Garage二进制与固定源码SHA256不符'
"$garage_bin" --version | grep -Eq '(^|[[:space:]])v?2\.4\.1([[:space:]]|$)' || payload_die 'Garage执行版本不符'

host_manifest_tmp=$(mktemp)
trap 'rm -f -- "${host_manifest_tmp:-}"' EXIT
host_collect_runtime_packages "$host_manifest_tmp" "$release_dir" "$pg_prefix" "$garage_bin"
HOST_NATIVE_ARCH=$(dpkg --print-architecture)
host_load_lock "$host_manifest_tmp"

# Explicit allow-list: no site.env, runtime state, execution logs or old payload.
mkdir -p "$out/payload/backend" "$out/payload/postgresql" "$out/payload/garage" "$out/metadata"
trap 'rc=$?; rm -f -- "${host_manifest_tmp:-}"; if ((rc)); then printf "封包失败（退出码 %s）；不完整输出保留在 %s，禁止交付。\n" "$rc" "$out" >&2; fi' EXIT
for item in README.md MANUAL.zh-CN.md install.sh verify.sh backup.sh restore.sh upgrade.sh rollback.sh status.sh admin-init.sh diagnose.sh renew-tls.sh preflight.sh init.sh tools lib templates docs examples tests; do
  [[ -e $PACKAGE_ROOT/$item ]] || continue
  cp -a -- "$PACKAGE_ROOT/$item" "$out/"
done
cp -a "$release_dir/." "$out/payload/backend/"
# Internal symlinks were checked above; expand them for complete byte hashing.
cp -aL "$pg_prefix/." "$out/payload/postgresql/"
install -m 755 "$garage_bin" "$out/payload/garage/garage"
printf '%s\n' "$pins" > "$out/metadata/upstream-native-pins.json"
cp "$release_dir/delivery-build/product-feature-manifest.json" "$out/metadata/product-feature-manifest.json"
cp "$release_dir/delivery-build/dependency-commits.tsv" "$out/metadata/dependency-commits.tsv"
cp "$host_manifest_tmp" "$out/metadata/host-runtime-packages.tsv"
git -C "$repo" show "$sha:LICENSE" > "$out/metadata/IMBoy-LICENSE"
payload_hash_tree "$out/payload/postgresql" FILES.sha256
receipt_sha=$(sha256sum "$receipt" | cut -d ' ' -f1)
backend_sha=$(sha256sum "$out/payload/backend/BUILD_FILES.sha256" | cut -d ' ' -f1)
pg_sha=$(sha256sum "$out/payload/postgresql/FILES.sha256" | cut -d ' ' -f1)
config_sha=$(sha256sum "$out/payload/backend/releases/$version/sys.config" | cut -d ' ' -f1)
cat > "$out/metadata/release.env" <<EOF
PACKAGE_FORMAT=1
RELEASE_ID=$release_id
SOURCE_SHA=$sha
BACKEND_VERSION=$version
OS_ID=$PAYLOAD_OS_ID
OS_VERSION_ID=$PAYLOAD_OS_VERSION_ID
ARCH=$PAYLOAD_ARCH
OTP_RELEASE=29
PG_MAJOR=18
PG_VERSION=$pg_version
GARAGE_VERSION=$garage_version
PRODUCT_PROFILE=phase1_delivery
MIGRATION_HEAD=$head_migration
PAYLOAD_READY=yes
BUILD_RECEIPT_SHA256=$receipt_sha
BUILD_DATE_UTC=$(date -u +%FT%TZ)
CONFIG_SHA256=$config_sha
PG_PREFIX=/opt/imboy/runtime/postgresql
PG_PAYLOAD_SHA256=$pg_sha
GARAGE_SHA256=$expected_garage
BACKEND_PAYLOAD_SHA256=$backend_sha
EOF
printf 'check\tstatus\nclean_host_install\tNOT_RUN\nbusiness_uat\tNOT_RUN\nbackup_restore\tNOT_RUN\nupgrade_rollback\tNOT_RUN\n' > "$out/metadata/validation-status.tsv"
printf '%s\n' "${extensions[@]}" > "$out/metadata/required-pg-extensions.txt"
payload_check_tree "$out/payload"
payload_normalize_permissions "$out"
payload_hash_tree "$out" CHECKSUMS.sha256
payload_verify_hashes "$out" CHECKSUMS.sha256
payload_check_permissions "$out/payload"
printf '客户候选目录已生成: %s\n' "$out"
printf '先在同OS/架构的干净主机完成首装/业务/备份恢复/升级演练，再正式交付。\n'
printf 'site.env由现场填写。SHA256证明文件一致性，不代替发布者签名或验收。\n'
