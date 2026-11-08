#!/usr/bin/env bash
# Standalone community entry point; deployment remains owned by deploy/install.sh.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
fail() { printf '[ERROR] %s\n' "$*" >&2; exit 2; }
mode=install
args=()
while [ "$#" -gt 0 ]; do
  case "$1" in
    --check) mode=check; shift ;;
    -h|--help) mode=help; shift ;;
    --edition)
      [ "$#" -ge 2 ] && [ "$2" = community ] || fail '此包仅支持 --edition community'
      shift 2 ;;
    --edition=community) shift ;;
    --edition=*) fail '此包仅支持 --edition community' ;;
    --yes|-y|--admin-phone=*|--admin-password=*) args+=("$1"); shift ;;
    --admin-phone|--admin-password)
      [ "$#" -ge 2 ] || fail "$1 缺少参数"
      args+=("$1" "$2"); shift 2 ;;
    *) fail '未知参数；使用 bash install.sh --help 查看支持范围' ;;
  esac
done
if [ "$mode" = help ]; then
  printf '%s\n' 'IMBoy 社区版独立交付入口' \
    '  bash install.sh --check  只检查包完整性和安装保护，不部署、不创建锁' \
    '  bash install.sh          检查后调用现有社区安装器' \
    '  --yes/-y                 会同意原安装器的 Docker 自动安装；默认不启用' \
    '  原安装器的管理员参数可透传；不支持商务版或跳过保护。'
  bash deploy/install.sh --help
  exit 0
fi
[ "$mode" != check ] || [ "${#args[@]}" -eq 0 ] || fail '--check 不接受部署参数'
bash scripts/check_delivery.sh
if [ "$mode" = check ]; then
  bash scripts/install_guard.sh
  exit 0
fi
command -v flock >/dev/null 2>&1 || fail '需要 flock（Debian/Ubuntu: util-linux）'
umask 077
exec 9>.delivery-install.lock
flock -n 9 || fail '本目录已有安装进程，请等待其结束；不要并行安装'
bash scripts/install_guard.sh
for arg in "${args[@]}"; do
  case "$arg" in --yes|-y) printf '%s\n' '[INFO] --yes 将同意原安装器的 Docker 自动安装。' ;; esac
done
bash deploy/install.sh --edition community "${args[@]}"
