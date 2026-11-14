#!/usr/bin/env bash
# Independent Flutter Web deployment; no backend, migrations or TLS mutations.
web_error() { printf 'Web: %s\n' "$*" >&2; return 1; }

web_validate() {
  [[ "$1" == 1 ]] || { web_error '必须使用 -l；不自动选择远端或其他 Flutter 源码'; return 1; }
  local key label
  for key in SERVER_HOST SERVER_PORT SERVER_USER WEB_BUILD_DIR; do
    [[ -n "${!key:-}" ]] || { web_error "缺少 $key"; return 1; }
  done
  [[ "$SERVER_HOST" =~ ^[a-zA-Z0-9][a-zA-Z0-9.-]*$ && "$SERVER_USER" =~ ^[a-zA-Z_][a-zA-Z0-9_-]*$ ]] || return 1
  [[ "$SERVER_PORT" =~ ^[0-9]{1,5}$ ]] && ((10#$SERVER_PORT > 0 && 10#$SERVER_PORT < 65536)) || return 1
  WEB_DOMAIN=${WEB_DOMAIN:-web.imboy.pub}
  [[ "$WEB_DOMAIN" =~ ^[a-z0-9.-]+$ && "$WEB_DOMAIN" == *.* ]] || return 1
  local IFS=.
  for label in $WEB_DOMAIN; do
    [[ "$label" =~ ^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$ ]] || return 1
  done
  [[ "$WEB_DOMAIN" != *. && "$WEB_DOMAIN" != *..* ]] || return 1
  WEB_REMOTE_ROOT=${WEB_REMOTE_ROOT:-/www/wwwroot/$WEB_DOMAIN}
  [[ "$WEB_REMOTE_ROOT" =~ ^/www/wwwroot/[a-zA-Z0-9][a-zA-Z0-9._/-]*$ && "$WEB_REMOTE_ROOT" != */ ]] || return 1
  [[ "/$WEB_REMOTE_ROOT/" != *'/../'* && "/$WEB_REMOTE_ROOT/" != *'/./'* && "$WEB_REMOTE_ROOT" != *//* ]] || return 1
  [[ "$WEB_BUILD_DIR" == /* && "$WEB_BUILD_DIR" != *$'\n'* && "$WEB_BUILD_DIR" != *$'\r'* ]] || return 1
  WEB_SOURCE=$(cd "$WEB_BUILD_DIR" && pwd -P) || return 1
  for key in pubspec.yaml lib/main.dart web/index.html .dart_tool/package_config.json lib/config/env_factory_web.dart; do
    [[ -f "$WEB_SOURCE/$key" ]] || { web_error "源码或已安装依赖缺失: $key"; return 1; }
  done
  grep -q WEB_API_BASE_URL "$WEB_SOURCE/lib/config/env_factory_web.dart" || return 1
  for key in flutter git ssh rsync curl cmp; do command -v "$key" >/dev/null || return 1; done
}

# Every argument used in this remote command has passed the local allowlist.
web_remote() {
  local op=$1 previous=${2:--}
  ssh -p "$SERVER_PORT" -o BatchMode=yes -o StrictHostKeyChecking=yes -o ConnectTimeout=10 \
    "$SERVER_USER@$SERVER_HOST" \
    "bash -s -- $WEB_REMOTE_ROOT $WEB_DOMAIN $WEB_RELEASE $op $previous" <<'REMOTE'
set -euo pipefail
root=$1 domain=$2 release=$3 op=$4 previous=$5
[[ $(realpath -e "$root") == "$root" && -d "$root" && ! -L "$root" ]]
[[ -f "$root/.imboy-web-root" && ! -L "$root/.imboy-web-root" && $(cat "$root/.imboy-web-root") == "$domain" ]]
[[ -d "$root/releases" && ! -L "$root/releases" && $(realpath -e "$root/releases") == "$root/releases" ]]
lock="$root/.web-deploy-lock"
target="releases/$release"
valid_target() {
  [[ "$1" =~ ^releases/[a-zA-Z0-9_-]+$ && -d "$root/$1" && ! -L "$root/$1" && $(realpath -e "$root/$1") == "$root/$1" ]]
}
case "$op" in
  prepare)
    old=-
    if [[ -e "$root/current" || -L "$root/current" ]]; then
      [[ -L "$root/current" ]]; old=$(readlink "$root/current"); valid_target "$old"
    fi
    mkdir "$lock"
    trap 'rmdir "$lock/$release" 2>/dev/null || true; rmdir "$lock" 2>/dev/null || true' ERR
    mkdir "$lock/$release"
    [[ ! -e "$root/$target" && ! -L "$root/$target" ]]
    mkdir "$root/$target"
    printf '%s\n' "$old"
    ;;
  activate)
    [[ -d "$lock/$release" ]]; valid_target "$target"
    [[ -s "$root/$target/index.html" && -s "$root/$target/main.dart.js" ]]
    [[ $(cat "$root/$target/.imboy-web-release") == "$release" ]]
    [[ ! -e "$root/.current-$release" && ! -L "$root/.current-$release" ]]
    ln -s "$target" "$root/.current-$release"
    mv -Tf "$root/.current-$release" "$root/current"
    ;;
  rollback)
    [[ -d "$lock/$release" && -L "$root/current" && $(readlink "$root/current") == "$target" ]]
    if [[ "$previous" == - ]]; then
      unlink "$root/current"
    else
      valid_target "$previous"
      ln -s "$previous" "$root/.rollback-$release"
      mv -Tf "$root/.rollback-$release" "$root/current"
    fi
    ;;
  unlock)
    [[ -d "$lock/$release" ]]
    rmdir "$lock/$release"; rmdir "$lock"
    ;;
  *) exit 1 ;;
esac
REMOTE
}

web_cleanup() {
  local result=$?
  trap - EXIT
  if [[ "$prepared" == 1 ]]; then
    if [[ "$activated" == 1 && "$success" != 1 ]]; then
      web_remote rollback "$previous" || { printf 'Web: 自动回滚失败，保留锁供人工核对 current\n' >&2; exit 1; }
    fi
    web_remote unlock || result=1
  fi
  # Only our mktemp build output; never the Flutter tree or remote release.
  [[ "$work" == "${TMPDIR:-/tmp}/imboy-web-deploy."* ]] && rm -rf -- "$work"
  exit "$result"
}

web_deploy() (
  set -Eeuo pipefail
  web_validate "$1" || { web_error '配置/源码校验失败，未建立 SSH'; exit 1; }
  local work prepared=0 activated=0 success=0 previous=- head
  work=$(mktemp -d "${TMPDIR:-/tmp}/imboy-web-deploy.XXXXXXXX")
  trap web_cleanup EXIT
  WEB_RELEASE="web_$(date -u +%Y%m%dT%H%M%SZ)_${work##*.}"
  head=$(git -C "$WEB_SOURCE" rev-parse HEAD)
  [[ "$head" =~ ^[0-9a-f]{40}$ ]] || exit 1
  printf 'Web: source=%s head=%s origin=https://%s release=%s\n' "$WEB_SOURCE" "$head" "$WEB_DOMAIN" "$WEB_RELEASE"
  (cd "$WEB_SOURCE" && env -u DART_DEFINES -u DART_DEFINES_FROM_FILE \
    flutter build web --release --no-pub --output "$work/dist" \
    --dart-define=APP_ENV=pro --dart-define="WEB_API_BASE_URL=https://$WEB_DOMAIN")
  [[ -s "$work/dist/index.html" && -s "$work/dist/main.dart.js" ]]
  [[ -z $(find "$work/dist" -type l -print -quit) ]] || exit 1
  printf '{"release_id":"%s","source_head":"%s","api_origin":"https://%s"}\n' \
    "$WEB_RELEASE" "$head" "$WEB_DOMAIN" > "$work/dist/__web_release.json"
  printf '%s\n' "$WEB_RELEASE" > "$work/dist/.imboy-web-release"
  previous=$(web_remote prepare)
  prepared=1
  [[ "$previous" == - || "$previous" =~ ^releases/[a-zA-Z0-9_-]+$ ]] || exit 1
  rsync -a --safe-links -e "ssh -p $SERVER_PORT -o BatchMode=yes -o StrictHostKeyChecking=yes -o ConnectTimeout=10" \
    "$work/dist/" "$SERVER_USER@$SERVER_HOST:$WEB_REMOTE_ROOT/releases/$WEB_RELEASE/"
  web_remote activate
  activated=1
  curl --fail --silent --show-error --connect-timeout 10 --max-time 30 \
    "https://$WEB_DOMAIN/__web_release.json?release=$WEB_RELEASE" > "$work/smoke.json"
  cmp -s "$work/dist/__web_release.json" "$work/smoke.json"
  curl --fail --silent --show-error --head --connect-timeout 10 --max-time 30 \
    "https://$WEB_DOMAIN/main.dart.js?release=$WEB_RELEASE" >/dev/null
  success=1
  printf 'Web: 发布静态候选成功；登录/WS/附件需独立真实验收 https://%s\n' "$WEB_DOMAIN"
)
