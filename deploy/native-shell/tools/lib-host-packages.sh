#!/usr/bin/env bash
# Runtime package locks; parse data only, never execute manifests.
HOST_BASE_PACKAGES=(bash curl jq openssl ca-certificates tar coreutils util-linux passwd iproute2 file nginx imagemagick fonts-dejavu-core)

host_dpkg_record() {
  local requested=$1 candidate output pkg version arch status selected= count=0
  case "$requested" in *:any|*:native) requested=${requested%:*};; esac
  local -a candidates=("$requested")
  [[ $requested == *:* ]] || candidates=("$requested:$HOST_NATIVE_ARCH" "$requested")
  for candidate in "${candidates[@]}"; do
    output=$(dpkg-query -W -f='${Package}\t${Version}\t${Architecture}\t${db:Status-Status}\n' -- "$candidate" 2>/dev/null) || continue
    selected=; count=0
    while IFS=$'\t' read -r pkg version arch status; do
      [[ $status == installed && ( $arch == "$HOST_NATIVE_ARCH" || $arch == all ) ]] || continue
      selected=$pkg$'\t'$version$'\t'$arch
      count=$((count+1))
    done <<< "$output"
    [[ $count == 1 ]] || continue
    printf '%s\n' "$selected"
    return 0
  done
  return 1
}

host_owner_of_library() {
  local original=$1 resolved candidate output line owner record pkg version arch
  resolved=$(realpath -e -- "$original") || payload_die "工程阻塞：动态库不存在: $original"
  local -a candidates=("$original" "$resolved")
  case "$resolved" in /usr/lib/*|/usr/lib64/*|/usr/bin/*|/usr/sbin/*) candidates+=("${resolved#/usr}");; esac
  case "$original" in /lib/*|/lib64/*|/bin/*|/sbin/*) candidates+=("/usr$original");; esac
  for candidate in "${candidates[@]}"; do
    output=$(dpkg-query -S -- "$candidate" 2>/dev/null) || continue
    while IFS= read -r line; do
      [[ $line == *": $candidate" ]] || continue
      owner=${line%": $candidate"}
      [[ $owner =~ ^[a-z0-9][a-z0-9+.-]*(:[a-z0-9-]+)?$ ]] || continue
      record=$(host_dpkg_record "$owner") || continue
      IFS=$'\t' read -r pkg version arch <<< "$record"
      printf '%s:%s\n' "$pkg" "$arch"
      return 0
    done <<< "$output"
  done
  payload_die "工程阻塞：库 $original 不属于已安装系统包，也未随payload供应；须先补齐原生依赖"
}

host_collect_runtime_packages() (
  set -euo pipefail
  local output=$1; shift
  local -a roots=("$@") queue=("${HOST_BASE_PACKAGES[@]}")
  local input bundle_root f kind ldd_output ldd_rc lib resolved bundled owner query record pkg version arch key deps dependency index=0
  local -A rows=()
  HOST_NATIVE_ARCH=$(dpkg --print-architecture)
  [[ $HOST_NATIVE_ARCH == amd64 ]] || payload_die '只支持amd64主机依赖锁'
  for input in "${roots[@]}"; do
    while IFS= read -r -d '' f; do
      kind=$(file -b -- "$f")
      [[ $kind == ELF* ]] || continue
      ldd_rc=0
      ldd_output=$(ldd "$f" 2>&1) || ldd_rc=$?
      [[ $ldd_output != *'not found'* ]] || payload_die "工程阻塞：ldd发现缺库: $f"
      if ((ldd_rc)); then
        [[ $kind == *'statically linked'* || $kind == *'static-pie linked'* ]] || payload_die "工程阻塞：ldd无法检查动态产物: $f"
      fi
      while IFS= read -r lib; do
        [[ -n $lib ]] || continue
        resolved=$(realpath -e -- "$lib") || payload_die "工程阻塞：动态库路径无效: $lib"
        bundled=no
        for bundle_root in "${roots[@]}"; do
          if [[ -d $bundle_root && $resolved == "$bundle_root"/* ]]; then bundled=yes; fi
        done
        [[ $bundled == no ]] || continue
        owner=$(host_owner_of_library "$lib")
        queue+=("$owner")
      done < <(printf '%s\n' "$ldd_output" | awk '/=> \// {print $3} /^[[:space:]]*\// {print $1}' | LC_ALL=C sort -u)
    done < <(find "$input" -type f -print0)
  done
  # Continue below with the exact installed dependency closure.
  while ((index < ${#queue[@]})); do
    query=${queue[$index]}; index=$((index+1))
    record=$(host_dpkg_record "$query") || payload_die "工程阻塞：发布机缺少必需已安装包 $query"
    IFS=$'\t' read -r pkg version arch <<< "$record"
    key=$pkg:$arch
    [[ ! -v rows[$key] ]] || continue
    rows[$key]=$record
    query=$key; [[ $arch != all ]] || query=$pkg
    deps=$(apt-cache depends --installed --no-recommends --no-suggests --no-conflicts --no-breaks --no-replaces --no-enhances "$query=$version" 2>/dev/null) || payload_die "工程阻塞：APT无法解析已安装版本依赖: $query=$version"
    while IFS= read -r dependency; do
      [[ -n $dependency ]] || continue
      queue+=("$dependency")
    done < <(printf '%s\n' "$deps" | awk '
      /^[[:space:]]*\|?(Pre)?Depends:[[:space:]]*/ {
        sub(/^[[:space:]]*\|?(Pre)?Depends:[[:space:]]*/, "");
        if($0 ~ /^[a-z0-9][a-z0-9+.:_-]*$/) print; next
      }
      /^[[:space:]]+[a-z0-9][a-z0-9+.:_-]*[[:space:]]*$/ {
        sub(/^[[:space:]]*/, ""); sub(/[[:space:]]*$/, ""); print
      }
    ')
  done
  { printf 'package\tversion\tarchitecture\n'; printf '%s\n' "${rows[@]}" | LC_ALL=C sort -t $'\t' -k1,1 -k3,3; } > "$output"
)

host_load_lock() {
  local lock_file=$1 pkg version arch extra key line=0 base
  [[ -s $lock_file && ! -L $lock_file ]] || payload_die '缺少主机依赖版本锁；发布方须先运行prepare-payload.sh'
  awk -F '\t' 'NR==1 {if($0!="package\tversion\tarchitecture") exit 1; next} NF!=3 {exit 1} END {if(NR<2) exit 1}' "$lock_file" || payload_die '主机依赖锁必须为package/version/architecture三列TSV'
  declare -gA HOST_LOCK_VERSION=() HOST_LOCK_ARCH=()
  declare -ga HOST_LOCK_KEYS=() HOST_LOCK_SPECS=()
  while IFS=$'\t' read -r pkg version arch extra; do
    line=$((line+1)); ((line > 1)) || continue
    [[ $pkg =~ ^[a-z0-9][a-z0-9+.-]*$ && $version =~ ^[0-9][0-9A-Za-z.+:~_-]*$ && ( $arch == "$HOST_NATIVE_ARCH" || $arch == all ) && -z $extra ]] || payload_die "主机依赖锁非法条目: 行$line"
    key=$pkg:$arch
    [[ ! -v HOST_LOCK_VERSION[$key] && ! -v HOST_LOCK_ARCH[$pkg] ]] || payload_die "主机依赖锁重复包: $pkg"
    HOST_LOCK_VERSION[$key]=$version
    HOST_LOCK_ARCH[$pkg]=$arch
    HOST_LOCK_KEYS+=("$key")
    if [[ $arch == all ]]; then HOST_LOCK_SPECS+=("$pkg=$version"); else HOST_LOCK_SPECS+=("$key=$version"); fi
  done < "$lock_file"
  for base in "${HOST_BASE_PACKAGES[@]}"; do
    [[ -v HOST_LOCK_ARCH[$base] ]] || payload_die "主机依赖锁缺基础工具: $base"
  done
}

host_validate_apt_plan() {
  local plan=$1 line name version key arch
  while IFS= read -r line; do
    case "$line" in
      Remv\ *) payload_die 'APT计划包含删除软件包，拒绝执行';;
      Inst\ *)
        name=${line#Inst }; name=${name%% *}
        version=${line#*\(}; [[ $version != "$line" ]] || payload_die '无法解析APT安装计划'
        version=${version%% *}; version=${version%)}
        if [[ $name == *:* ]]; then arch=${name#*:}; name=${name%:*}; else arch=${HOST_LOCK_ARCH[$name]:-}; fi
        key=$name:$arch
        [[ -n $arch && ${HOST_LOCK_VERSION[$key]:-} == "$version" ]] || payload_die "APT计划需要未锁定包或不同版本: $name；发布方须重新补齐依赖锁"
        ;;
    esac
  done < "$plan"
}
