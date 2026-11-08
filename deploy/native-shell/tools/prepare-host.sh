#!/usr/bin/env bash
# Install only the publisher's exact runtime package lock from existing APT sources.
set -Eeuo pipefail
umask 077
export LC_ALL=C
TOOLS_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd -P)
PACKAGE_ROOT=$(cd "$TOOLS_DIR/.." && pwd -P)
# shellcheck source=lib-payload.sh
. "$TOOLS_DIR/lib-payload.sh"
# shellcheck source=lib-host-packages.sh
. "$TOOLS_DIR/lib-host-packages.sh"
usage() {
  cat <<'EOF'
Usage: bash tools/prepare-host.sh --check
       sudo bash tools/prepare-host.sh --apply

--check  Read-only check of package integrity, OS/architecture and exact installed
         versions in metadata/host-runtime-packages.tsv. No network or APT update.
--apply  If packages are missing/older, update indexes from already configured,
         site-approved Ubuntu24.04/Debian13 APT sources, verify exact availability,
         simulate, then install only the locked versions and dependency closure.

Does not add/change repositories, run a whole-host upgrade, remove packages,
downgrade newer packages, start IMBoy, or invoke Python/containers.
Existing config files are preserved by dpkg. Normal OS package maintainer actions
(for example Nginx package setup) may occur. Requires working approved APT access;
this is not an offline dependency bundle. If a pinned version is unavailable,
publisher must supply a matching approved repository/offline package collection
and redo validation. Never edit the version lock merely to bypass a failure.
EOF
}
mode=check; selected=no
while (($#)); do
  case "$1" in
    --check|--apply)
      [[ $selected == no ]] || payload_die '只选择一个--check或--apply'
      mode=${1#--}; selected=yes; shift;;
    --help|-h) usage; exit 0;;
    *) payload_die "未知参数: $1";;
  esac
done
for cmd in dpkg dpkg-query awk sed grep cut sort find sha256sum realpath readlink cmp stat; do payload_need "$cmd"; done
payload_os
[[ $(payload_value "$PACKAGE_ROOT/metadata/release.env" PAYLOAD_READY) == yes ]] || payload_die '此目录尚未组装原生payload；发布方须先运行prepare-payload.sh'
[[ $(payload_value "$PACKAGE_ROOT/metadata/release.env" OS_ID) == "$PAYLOAD_OS_ID" && $(payload_value "$PACKAGE_ROOT/metadata/release.env" OS_VERSION_ID) == "$PAYLOAD_OS_VERSION_ID" && $(payload_value "$PACKAGE_ROOT/metadata/release.env" ARCH) == "$PAYLOAD_ARCH" ]] || payload_die '目标系统与依赖锁构建平台不同'
payload_verify_hashes "$PACKAGE_ROOT" CHECKSUMS.sha256
HOST_NATIVE_ARCH=$(dpkg --print-architecture)
host_load_lock "$PACKAGE_ROOT/metadata/host-runtime-packages.tsv"
declare -a pending=()
newer=no
check_installed() {
  local key pkg expected arch record installed_name installed_version installed_arch
  pending=(); newer=no
  for key in "${HOST_LOCK_KEYS[@]}"; do
    pkg=${key%:*}; arch=${key#*:}; expected=${HOST_LOCK_VERSION[$key]}
    if record=$(host_dpkg_record "$key"); then
      IFS=$'\t' read -r installed_name installed_version installed_arch <<< "$record"
      if [[ $installed_version == "$expected" && $installed_arch == "$arch" ]]; then continue; fi
      if dpkg --compare-versions "$installed_version" gt "$expected"; then
        printf 'BLOCKED_NEWER_VERSION\t%s\tinstalled=%s\tpinned=%s\n' "$pkg" "$installed_version" "$expected" >&2
        newer=yes
      else
        printf 'NEED_INSTALL\t%s\t%s\t%s\n' "$pkg" "$expected" "$arch" >&2
        pending+=("$key")
      fi
    else
      printf 'NEED_INSTALL\t%s\t%s\t%s\n' "$pkg" "$expected" "$arch" >&2
      pending+=("$key")
    fi
  done
}
check_installed
[[ $newer == no ]] || payload_die '目标已有更高版本；拒绝降级。请发布方重新生成并验证匹配的候选'
if ((${#pending[@]})); then
  [[ $mode == apply ]] || { printf 'HOST_PACKAGE_CHECK=NEEDS_INSTALL\n' >&2; exit 3; }
  [[ $(id -u) == 0 ]] || payload_die '--apply需要root；请使用sudo'
  for cmd in apt-get apt-cache mktemp; do payload_need "$cmd"; done
  apt-get update
  for key in "${pending[@]}"; do
    pkg=${key%:*}; arch=${key#*:}; version=${HOST_LOCK_VERSION[$key]}
    query=$key; [[ $arch != all ]] || query=$pkg
    available=$(apt-cache madison "$query") || payload_die "无法查询已批准APT源: $query"
    awk -v expected="$version" '$3==expected {found=1} END {exit found ? 0 : 1}' <<< "$available" || payload_die "已配置源不提供固定版本 $query=$version；请发布方补齐依赖供应，不自动换版本"
  done
  plan=$(mktemp)
  trap 'rm -f -- "${plan:-}"' EXIT
  apt-get --simulate --no-install-recommends --no-remove install "${HOST_LOCK_SPECS[@]}" > "$plan"
  host_validate_apt_plan "$plan"
  DEBIAN_FRONTEND=noninteractive apt-get --yes --no-install-recommends --no-remove \
    -o Dpkg::Options::=--force-confold install "${HOST_LOCK_SPECS[@]}"
  check_installed
  [[ $newer == no && ${#pending[@]} == 0 ]] || payload_die 'APT结束后实际版本仍不符合锁；停止后续安装'
fi
for cmd in file ldd; do payload_need "$cmd"; done
LD_LIBRARY_PATH="$PACKAGE_ROOT/payload/postgresql/lib${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
  payload_check_dynamic_libraries "$PACKAGE_ROOT/payload"
printf 'HOST_PACKAGE_CHECK=PASS\nLOCKED_PACKAGE_COUNT=%s\n' "${#HOST_LOCK_KEYS[@]}"
printf '主机依赖满足。未启动IMBoy、数据库或对象存储；继续安装前仍须完成现场/TLS预检。\n'
