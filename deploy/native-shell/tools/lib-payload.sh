#!/usr/bin/env bash
# Publisher checks: Bash 4.4+ and GNU userland. No Python/container dependency.

payload_die() { printf 'ERROR: %s\n' "$*" >&2; exit 2; }
payload_need() { command -v "$1" >/dev/null 2>&1 || payload_die "缺少发布工具: $1"; }
payload_absolute_existing() {
  [[ -e $1 ]] || payload_die "路径不存在: $1"
  realpath -e -- "$1"
}
payload_safe_path() {
  [[ $1 =~ ^/[A-Za-z0-9._/-]+$ && $1 != / && $1 != *'/../'* && $1 != */.. ]] || payload_die "路径须为仅含字母数字、下划线、点、横线的绝对路径: $1"
}
payload_value() {
  # Parse KEY=value as data. Never source publisher/site input.
  local file=$1 key=$2 line value= count=0
  while IFS= read -r line || [[ -n $line ]]; do
    if [[ $line == "$key="* ]]; then value=${line#*=}; count=$((count + 1)); fi
  done < "$file"
  [[ $count == 1 && -n $value ]] || payload_die "$file 缺少或重复字段 $key"
  printf '%s\n' "$value"
}
payload_os() {
  local ID= VERSION_ID=
  [[ -r /etc/os-release ]] || payload_die '缺少 /etc/os-release'
  # OS-owned file, unlike publisher/site input.
  # shellcheck disable=SC1091
  . /etc/os-release
  case "$ID:$VERSION_ID" in
    debian:13|ubuntu:24.04) ;;
    *) payload_die "发布机仅支持 Debian 13 / Ubuntu 24.04；实际 $ID $VERSION_ID" ;;
  esac
  [[ $(uname -m) == x86_64 ]] || payload_die '本包只封装 Linux x86_64；其他架构需独立构建和演练'
  PAYLOAD_OS_ID=$ID
  PAYLOAD_OS_VERSION_ID=$VERSION_ID
  PAYLOAD_ARCH=$(uname -m)
}
payload_check_tree() {
  # Conservative state/secret check; opaque binaries still require publisher review.
  local root=$1 f rel target
  while IFS= read -r -d '' f; do
    rel=${f#"$root"/}
    [[ $rel != *$'\n'* && $rel != *$'\r'* && $rel != *$'\t'* ]] || payload_die '归档文件名含控制字符'
    case "$rel" in
      .git|.git/*|*/.git|*/.git/*|.env|.env.*|*/.env|*/.env.*|.erlang.cookie|*/.erlang.cookie|*.key|*.p12|*.pfx|*.jks|*.keystore|*.dump|*.sql.gz|*.log|*.log.*|*/PG_VERSION|PG_VERSION|*/postmaster.pid|postmaster.pid|log/*|logs/*|*/log/*|*/logs/*|Mnesia.*|*/Mnesia.*|pgdata/*|*/pgdata/*|*/pg_wal/*|pg_wal/*)
        payload_die "拒绝封装疑似秘密、数据或日志路径: $rel" ;;
    esac
    if [[ -L $f ]]; then
      target=$(readlink -- "$f")
      [[ $target != /* ]] || payload_die "不可迁移的绝对符号链接: $rel"
      target=$(realpath -e -- "$f") || payload_die "失效符号链接: $rel"
      [[ $target == "$root"/* ]] || payload_die "越出产物根目录的符号链接: $rel"
    elif [[ -f $f ]]; then
      if LC_ALL=C grep -IqlE '^-----BEGIN ([A-Z0-9]+ )*PRIVATE KEY-----[[:space:]]*$' "$f"; then
        payload_die "拒绝封装私钥文件: $rel"
      fi
    elif [[ ! -d $f ]]; then
      payload_die "不允许设备、管道、套接字等特殊文件: $rel"
    fi
  done < <(find "$root" -mindepth 1 -print0)
}
payload_verify_hashes() {
  local root=$1 manifest=$2
  [[ -f $root/$manifest ]] || payload_die "缺少产物校验表: $manifest"
  # Reject external paths even when a modified receipt is supplied.
  awk '
    { h=substr($0,1,64); p=substr($0,67);
      if(length(h)!=64 || h ~ /[^0-9a-f]/ || substr($0,65,2)!="  " || p !~ /^\.\// || p ~ /(^|\/)\.\.(\/|$)/ || p ~ /[\r\t]/) bad=1 }
    END { if(NR==0) bad=1; exit bad ? 1 : 0 }
  ' "$root/$manifest" || payload_die "校验表格式非法: $manifest"
  [[ -z $(find "$root" -type l -print -quit) ]] || payload_die '带哈希收据的产物不得包含符号链接'
  cmp -s <(cd "$root" && find . -type f ! -path "./$manifest" ! -path './site.env' ! -path './.git/*' | LC_ALL=C sort) \
    <(cut -c 67- "$root/$manifest" | LC_ALL=C sort) || payload_die "校验表未精确覆盖文件集合: $manifest"
  (cd "$root" && sha256sum --quiet -c "$manifest") || payload_die "产物完整性失败: $manifest"
}
payload_hash_tree() {
  local root=$1 target=$2 f
  [[ -z $(find "$root" -type l -print -quit) ]] || payload_die '封包前请把内部符号链接展开为实体文件'
  (
    cd "$root"
    while IFS= read -r -d '' f; do sha256sum "$f"; done < <(
      find . -type f ! -path "./$target" ! -path './site.env' ! -path './.git/*' -print0 | LC_ALL=C sort -z
    )
  ) > "$root/$target"
  chmod 644 -- "$root/$target"
}

payload_normalize_permissions() {
  local root=$1 entry mode
  [[ -z $(find "$root" -type l -print -quit) ]] || payload_die '权限规范只接受无符号链接的已验证产物'
  while IFS= read -r -d '' entry; do
    if [[ -d $entry ]]; then
      chmod u=rwx,go=rx,u-s,g-s,o-t -- "$entry"
    elif [[ -f $entry ]]; then
      mode=$(stat -c %a -- "$entry")
      if (( (8#$mode & 0111) != 0 )); then
        chmod 755 -- "$entry"
      else
        chmod 644 -- "$entry"
      fi
    else
      payload_die '产物含特殊文件，拒绝调整权限'
    fi
  done < <(find "$root" -print0)
}

payload_check_permissions() {
  local root=$1 entry mode
  while IFS= read -r -d '' entry; do
    mode=$(stat -c %a -- "$entry")
    if [[ -d $entry && ! -L $entry ]]; then
      [[ $mode == 755 ]] || payload_die "产物目录必须可由服务用户遍历(755): $entry"
    elif [[ -f $entry && ! -L $entry ]]; then
      [[ $mode == 644 || $mode == 755 ]] || payload_die "无秘密产物文件须为644或755: $entry"
    else
      payload_die "产物含链接或特殊文件: $entry"
    fi
  done < <(find "$root" -print0)
}
payload_check_dynamic_libraries() {
  local root=$1 f output
  while IFS= read -r -d '' f; do
    file -b -- "$f" | grep -q '^ELF ' || continue
    output=$(ldd "$f" 2>&1) || :
    if [[ $output == *'not found'* ]]; then
      printf '%s\n' "$output" >&2
      payload_die "发布机动态库不全: ${f#"$root"/}"
    fi
  done < <(find "$root" -type f \( -perm /111 -o -name '*.so' -o -name '*.so.*' \) -print0)
}
