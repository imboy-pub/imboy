#!/usr/bin/env bash
# ============================================================
# check_l4_sni_listen.sh — L4 SNI nginx vhost listen 漂移检测（Task 1）
# ------------------------------------------------------------
# 合同来源:
#   docs/plans/2026-10-01-l4-sni-hardening-relay-verification-plan-v1.md (Task 1)
#   .Codex/runs/20261002T131801Z-l4-sni-hardening/control/interface-contract.md
#
# 边界：本脚本只检查受检配置文件的文本（listen 指令与 server 结构），
# 不证明实际运行配置、端口归属或媒体健康。
#
# 换行按 nginx 官方语义视为纯空白（合同 v1.1 P1）：指令/块仅在 ';' '}'
# 处终结，允许跨行（宝塔 `server\n{` 书写风格）；EOF 处仍有未完结指令 =
# 可能截断的文件，退出 2。
# include 递归解析（合同 v1.1 P2）：绝对路径默认解析；相对路径按搜索目录
# （--include-path > $L4_SNI_INCLUDE_PATH > env 文件 NGINX_INCLUDE_PATH）
# 顺序命中；glob 无匹配跳过（nginx 语义）；非 glob 缺失/环/嵌套超 8 层
# 退出 2。
# fail-closed 原则：不能可靠解析的结构（未闭合块、EOF 未完结指令、无法
# 解析的 include、未知 listen 参数、顶层非 server/upstream 块）一律退出 2，
# 绝不猜测通过（顶层 server 与 upstream 块受支持，其余顶层块 fail-closed）。
# 受检文件若不含任何受管 HTTPS server 定义（空文件、纯注释、只有 HTTP
# server、没有任何 server 块），视为受管配置缺失（漂移），退出 1；
# --strict 与 --pre-switch 两种模式同样适用。
#
# 兼容 bash 3.2（macOS /bin/bash）：不使用关联数组、nameref、空数组
# 在 set -u 下的裸展开等新特性。
# ============================================================

set -Eeuo pipefail

PROG="${0##*/}"
DEFAULT_ENV_FILE=/etc/imboy/livekit-l4-sni.env

# ── 选项状态 ────────────────────────────────────────────────
MODE=strict
MODE_SET=0
WANT_METRICS=0
WANT_PUSH=0
PUSH_URL="${PUSHGATEWAY_URL:-}"
CONFIGS=()

# ── 检测结果全局状态 ─────────────────────────────────────────
PARSE_OK=1        # 1=输入与解析完整成功；0=输入/解析错误（exit 2 路径）
VIOLATIONS=0
PUSH_FAILED=0
NSRV_TOTAL=0
METRICS_PAYLOAD=''

# 单文件解析状态（parse_stream 内跨行/跨 include 保持）
DEPTH=0; NSRV=0; N_DIR=0; HAS_COMMENT=0; FILE_HAS_HTTPS_SRV=0
SRV_OPEN=0; SRV_LINE=0; SRV_DEPTH=0; SRV_CERT=0; SRV_NAME=''; SRV_FILE=''
S_LLINE=(); S_LFILE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()

# 词法状态（lex_line 输出）
TOKENS=(); LEX_COMMENT=0; LEX_ERROR=0
# 当前未完结指令的 token 缓冲。换行即空白（v1.1 P1）：BUF 跨行（乃至跨
# include 文件）保持，直到 ';' '{' '}' 终结；BUF_FILE/BUF_LINE 记录指令
# 起始位置，用于诊断（含 EOF 未完结诊断）。
BUF=(); BUF_FILE=''; BUF_LINE=0

# 顶层受检文件（drift 序列 config_file 标签取值，也是 server 标签去重的作用域）
CUR_FILE=''

# include 解析状态：搜索目录（合并自 --include-path / $L4_SNI_INCLUDE_PATH /
# env 文件 NGINX_INCLUDE_PATH）、当前递归深度、活动 include 链（环检测）
INCLUDE_PATHS=(); INC_DEPTH=0; INC_CHAIN=()
INCLUDE_DEPTH_LIMIT=8
GLOB_MATCHES=()
LAST_LINENO=0

# 指标序列（每个已完成判定的 server 块一条）
M_FILE=(); M_SERVER=(); M_DRIFT=()

err() { printf '%s\n' "$*" >&2; }

viol() {
  err "$1"
  VIOLATIONS=$((VIOLATIONS + 1))
}

# 输入/解析类失败：打印诊断（file:line: message），指标以 check_success=0
# 收尾后退出 2（若同时要求推送且推送失败，则退出 3，见 finalize）。
parse_fatal() {
  PARSE_OK=0
  err "$1"
  finalize 2
}

usage_basics() {
  cat <<'USAGE'
Usage:
  check_l4_sni_listen.sh [--strict|--pre-switch] [--metrics] [--push]
                         [--pushgateway-url URL]
                         [--include-path DIR1[:DIR2...]] [config_file ...]

Check nginx vhost text files for L4 SNI listen drift. This inspects
configuration TEXT only; it does not prove the running config, port
ownership, or media health.

Modes:
  --strict      Default. Post-switch contract: in every managed HTTPS
                server, HTTPS listeners must be 127.0.0.1:10443 or
                [::1]:10443 with a standalone `proxy_protocol` token.
                Any direct :443 listener (bare 443, IPv4 addr:443,
                0.0.0.0:443, *:443, [::]:443, [::1]:443, ...) is drift.
  --pre-switch  Pre-switch state: direct :443 listeners are allowed,
                but files must still parse with decidable structure.
                loopback:10443 listeners, if present, must still be
                well-formed (loopback + proxy_protocol); an ssl
                listener on any other port is still reported as drift.

  In BOTH modes every checked file must contain at least one managed
  HTTPS server definition (an ssl listener, a listener on port 443
  or 10443, or an ssl_certificate directive). A file without one --
  empty, comments-only, HTTP-only server blocks, or no server block
  at all -- is missing managed config and is reported as drift
  (exit 1).

Options:
  --metrics          Print Pushgateway text metrics to stdout (stdout
                     carries ONLY metrics; diagnostics go to stderr).
  --push             Push metrics to Pushgateway, job name
                     imboy_l4_sni_check. Missing URL or failed push
                     exits 3. Without --push nothing is pushed.
  --pushgateway-url URL
                     Pushgateway base URL (wins over $PUSHGATEWAY_URL).
  --include-path DIR1[:DIR2...]
                     Add colon-separated directories to the include
                     search path; the option may be repeated. Search
                     order (first hit wins): --include-path arguments,
                     then $L4_SNI_INCLUDE_PATH, then NGINX_INCLUDE_PATH
                     from the discovery env file. Absolute-path
                     includes resolve without any search path; relative
                     includes are searched in order; glob includes
                     (* ?) with no match are skipped (nginx semantics);
                     a missing non-glob include exits 2. Include
                     nesting is limited to depth 8; include cycles
                     exit 2.
  -h, --help         Show this help.

Config selection (deterministic precedence):
  1. Explicit config_file arguments are checked as given; built-in
     discovery is skipped entirely (explicit wins).
  2. Without arguments, files are discovered from the env file
     $L4_SNI_ENV_FILE (default /etc/imboy/livekit-l4-sni.env) by
     reading NGINX_VHOST_DIR and HTTPS_VHOST_FILES (space separated,
     surrounding quotes stripped). Missing/unreadable/empty discovery
     is an input error (exit 2).
USAGE
}

usage_reference() {
  cat <<'USAGE'
Environment:
  PUSHGATEWAY_URL   Default push base URL used by --push.
  L4_SNI_ENV_FILE   Discovery env file (default /etc/imboy/livekit-l4-sni.env).
                    NGINX_INCLUDE_PATH lines in this file (colon
                    separated, quotes stripped) also feed the include
                    search path; the env file is read for this even
                    when config files are passed explicitly.
  L4_SNI_INCLUDE_PATH
                    Default include search path (colon separated),
                    applied after --include-path values and before the
                    env file's NGINX_INCLUDE_PATH.
  PUSH_TIMEOUT_SEC  Push timeout in seconds, default 10.

Exit codes (precedence 3 > 2 > 1 > 0):
  0  healthy (no drift)
  1  drift/violation found; this includes a checked file with no
     managed HTTPS server definition (empty file, comments-only,
     HTTP-only server blocks, or no server block at all)
  2  input error: missing/unreadable file, empty list, unknown option,
     missing option value, or unparsable structure (unresolvable
     include — relative include with no search path, missing
     non-glob target, include cycle, or nesting deeper than 8;
     unterminated directive at end of file; unbalanced/unclosed
     braces; listen outside a server block; unknown listen parameter;
     non-server/non-upstream top-level block)
  3  --push requested but Pushgateway URL missing or push failed

Metrics (Pushgateway text format, frozen names):
  imboy_l4_sni_listen_drift{config_file,server} gauge  1=drift 0=healthy
  imboy_l4_sni_check_success gauge  1=check fully ran 0=input/parse error
  imboy_l4_sni_last_check_timestamp_seconds gauge  unix epoch
  config_file label = top-level path exactly as checked (servers found
  via include are attributed to the checking file); server label =
  first server_name argument, else server-<n> (per-file ordinal).
  Drift series are emitted only for managed HTTPS server blocks; within
  one checked file, duplicate HTTPS server labels get #2, #3, ...
  suffixes in order of appearance.

Known limits (fail-closed, never guessed as pass):
  - Newlines are plain whitespace (nginx semantics): directives may
    span lines (e.g. `server` and `{` on separate lines); a directive
    still unterminated at EOF exits 2 as a possibly truncated file.
  - include directives are resolved recursively with shared parser
    state (nesting depth <= 8). A relative include without any
    configured search path exits 2 rather than guessing. Glob
    includes with no match are skipped (nginx semantics).
  - A checked file with no managed HTTPS server definition (empty,
    comments-only, HTTP-only server blocks, or no server block) is
    missing managed config and exits 1 as drift.
  - server blocks must sit at file top level (vhost file convention);
    top-level upstream blocks are also supported (parsed structurally
    only — their inner `server host:port;` directives are not server
    blocks, and a listen inside one is still fatal); any other
    top-level block (http/events/stream/map/geo/unknown) fails closed.
  - Unquoted regex braces inside location patterns can confuse block
    accounting (typically surfacing as an exit 2, not a wrong pass).
  - Include cycle detection compares path strings; different path
    spellings of the same file are not unified.
USAGE
}

usage() {
  usage_basics
  usage_reference
}

# 追加冒号分隔的 include 搜索目录（PATH 风格；空段跳过）
add_include_paths() {
  local list="$1" entry rest
  rest="$list"
  while [ -n "$rest" ]; do
    entry="${rest%%:*}"
    if [ "$entry" = "$rest" ]; then
      rest=''
    else
      rest="${rest#*:}"
    fi
    if [ -n "$entry" ]; then
      INCLUDE_PATHS+=("$entry")
    fi
  done
  return 0
}

parse_args() {
  while [ $# -gt 0 ]; do
    case "$1" in
      --strict|--pre-switch)
        if [ "$MODE_SET" -eq 1 ] && [ "$1" != "--$MODE" ]; then
          parse_fatal "$PROG:0: choose exactly one mode (--strict or --pre-switch)"
        fi
        MODE="${1#--}"
        MODE_SET=1
        ;;
      --metrics) WANT_METRICS=1 ;;
      --push) WANT_PUSH=1 ;;
      --pushgateway-url)
        if [ $# -lt 2 ]; then
          parse_fatal "$PROG:0: --pushgateway-url requires a URL"
        fi
        PUSH_URL="$2"
        shift
        ;;
      --include-path)
        if [ $# -lt 2 ]; then
          err "$PROG:0: --include-path requires a value (DIR1[:DIR2...])"
          exit 2
        fi
        add_include_paths "$2"
        shift
        ;;
      -h|--help) usage; exit 0 ;;
      --*|-*)
        parse_fatal "$PROG:0: unknown option: $1"
        ;;
      *) CONFIGS+=("$1") ;;
    esac
    shift
  done
  return 0
}

strip_quotes() {
  local s="$1"
  s="${s#\"}"; s="${s%\"}"
  s="${s#\'}"; s="${s%\'}"
  printf '%s' "$s"
}

discover_configs() {
  local env_file="${L4_SNI_ENV_FILE:-$DEFAULT_ENV_FILE}" dir files f
  if [ ! -f "$env_file" ]; then
    parse_fatal "$env_file:0: no config files given and discovery env file is missing"
  fi
  if [ ! -r "$env_file" ]; then
    parse_fatal "$env_file:0: discovery env file is not readable"
  fi
  dir="$(sed -n 's/^NGINX_VHOST_DIR=//p' "$env_file" | tail -n 1)"
  files="$(sed -n 's/^HTTPS_VHOST_FILES=//p' "$env_file" | tail -n 1)"
  dir="$(strip_quotes "$dir")"
  files="$(strip_quotes "$files")"
  if [ -z "$dir" ] || [ -z "$files" ]; then
    parse_fatal "$env_file:0: discovery needs both NGINX_VHOST_DIR and HTTPS_VHOST_FILES"
  fi
  # shellcheck disable=SC2086  # HTTPS_VHOST_FILES 本就是空格分隔列表
  for f in $files; do
    CONFIGS+=("$dir/$f")
  done
  if [ "${#CONFIGS[@]}" -eq 0 ]; then
    parse_fatal "$env_file:0: HTTPS_VHOST_FILES is empty; no config files to check"
  fi
  err "$PROG:0: discovered ${#CONFIGS[@]} config file(s) from $env_file (explicit arguments take precedence over discovery)"
  return 0
}

# 从发现 env 文件读取 NGINX_INCLUDE_PATH（沿 NGINX_VHOST_DIR 的解析约定：
# 取该前缀最后一次出现、去首尾引号；冒号分隔）。env 文件路径与配置文件
# 参数独立：显式传配置文件时仍读取（v1.1 P2）；文件缺失/不可读/无该行
# 时静默跳过——它只是可选的 include 搜索路径来源，不是受检对象。
load_env_include_paths() {
  local env_file="${L4_SNI_ENV_FILE:-$DEFAULT_ENV_FILE}" line
  if [ ! -f "$env_file" ] || [ ! -r "$env_file" ]; then
    return 0
  fi
  line="$(sed -n 's/^NGINX_INCLUDE_PATH=//p' "$env_file" | tail -n 1)"
  if [ -z "$line" ]; then
    return 0
  fi
  line="$(strip_quotes "$line")"
  if [ -n "$line" ]; then
    add_include_paths "$line"
  fi
  return 0
}

# 把一行配置切成 token。引号内不切分；行内 # 之后是注释直接截断；
# ; { } 作为独立 token。输出 TOKENS；LEX_COMMENT/LEX_ERROR 置位。
lex_line() {
  local line="$1" i=0 len=${#1} ch q='' tok=''
  TOKENS=()
  LEX_COMMENT=0
  LEX_ERROR=0
  while [ "$i" -lt "$len" ]; do
    ch="${line:i:1}"
    if [ -n "$q" ]; then
      tok="$tok$ch"
      if [ "$ch" = "$q" ]; then q=''; fi
    elif [ "$ch" = '"' ] || [ "$ch" = "'" ]; then
      if [ -n "$tok" ]; then TOKENS+=("$tok"); tok=''; fi
      tok="$ch"
      q="$ch"
    elif [ "$ch" = ' ' ] || [ "$ch" = $'\t' ] || [ "$ch" = $'\r' ]; then
      if [ -n "$tok" ]; then TOKENS+=("$tok"); tok=''; fi
    elif [ "$ch" = '#' ]; then
      LEX_COMMENT=1
      break
    else
      case "$ch" in
        ';'|'{'|'}')
          if [ -n "$tok" ]; then TOKENS+=("$tok"); tok=''; fi
          TOKENS+=("$ch")
          ;;
        *) tok="$tok$ch" ;;
      esac
    fi
    i=$((i + 1))
  done
  if [ -n "$tok" ]; then TOKENS+=("$tok"); fi
  if [ -n "$q" ]; then LEX_ERROR=1; fi
  return 0
}

check_listen_param() {
  local file="$1" lineno="$2" p="$3"
  local n=$(( ${#S_PORT[@]} - 1 ))
  case "$p" in
    ssl)            S_SSL[$n]=1 ;;
    proxy_protocol) S_PP[$n]=1 ;;
    http2|default|default_server|bind|deferred|reuseport|quic|http3|ssl_reject_handshake) : ;;
    backlog=*|rcvbuf=*|sndbuf=*|ipv6only=*|so_keepalive=*|fastopen=*|accept_filter=*) : ;;
    *)
      parse_fatal "$file:$lineno: unknown listen parameter '$p' (refusing to guess)"
      ;;
  esac
  return 0
}

record_listen() {
  local file="$1" lineno="$2" first="${BUF[1]:-}" addr port p
  if [ "${#BUF[@]}" -lt 2 ] || [ -z "$first" ]; then
    parse_fatal "$file:$lineno: listen directive has no address/port"
  fi
  case "$first" in
    \[*\]:*) port="${first##*:}"; addr="${first%:*}" ;;
    unix:*)  addr="$first"; port='unix' ;;
    *:*)     port="${first##*:}"; addr="${first%:*}" ;;
    *)       addr='*'; port="$first" ;;
  esac
  if [ "$port" != unix ]; then
    case "$port" in
      ''|*[!0-9]*)
        parse_fatal "$file:$lineno: cannot parse listen port from '$first'"
        ;;
    esac
  fi
  S_LLINE+=("$lineno"); S_LFILE+=("$file"); S_ADDR+=("$addr"); S_PORT+=("$port"); S_SSL+=("0"); S_PP+=("0")
  if [ "${#BUF[@]}" -gt 2 ]; then
    for p in "${BUF[@]:2}"; do
      check_listen_param "$file" "$lineno" "$p"
    done
  fi
  return 0
}

# ── include 解析（合同 v1.1 P2）─────────────────────────────

# 当前活动 include 链的诊断文本：a.conf -> b.conf -> c.conf
chain_text() {
  local t='' c
  for c in ${INC_CHAIN[@]+"${INC_CHAIN[@]}"}; do
    if [ -z "$t" ]; then t="$c"; else t="$t -> $c"; fi
  done
  printf '%s' "$t"
}

has_glob_chars() {
  case "$1" in
    *\**|*\?*) return 0 ;;
    *) return 1 ;;
  esac
}

# 展开一个已绝对化的 glob 模式到 GLOB_MATCHES（compgen -G，bash 3.2 可用；
# 无匹配时结果为空数组）。文件名含换行的病态情形不支持。
expand_glob() {
  local pattern="$1" m
  GLOB_MATCHES=()
  while IFS= read -r m; do
    if [ -n "$m" ]; then
      GLOB_MATCHES+=("$m")
    fi
  done < <(compgen -G "$pattern")
  return 0
}

# 进入一个已解析出具体路径的 include 目标：存在性/可读检查、环检测、
# 深度防护，然后递归 parse_stream（共享全部解析状态：server/listen 正确
# 归位到包含它的块上下文；顶层 include 的内容按顶层处理）。
include_enter() {
  local file="$1" lineno="$2" target="$3" c
  if [ ! -e "$target" ]; then
    parse_fatal "$file:$lineno: include target not found: $target"
  fi
  if [ ! -f "$target" ]; then
    parse_fatal "$file:$lineno: include target is not a regular file: $target"
  fi
  if [ ! -r "$target" ]; then
    parse_fatal "$file:$lineno: include target is not readable: $target"
  fi
  for c in ${INC_CHAIN[@]+"${INC_CHAIN[@]}"}; do
    if [ "$c" = "$target" ]; then
      parse_fatal "$file:$lineno: include cycle detected: '$target' is already being parsed (chain: $(chain_text))"
    fi
  done
  if [ "$INC_DEPTH" -ge "$INCLUDE_DEPTH_LIMIT" ]; then
    parse_fatal "$file:$lineno: include nesting exceeds depth limit $INCLUDE_DEPTH_LIMIT (chain: $(chain_text))"
  fi
  INC_DEPTH=$((INC_DEPTH + 1))
  INC_CHAIN+=("$target")
  parse_stream "$target"
  INC_DEPTH=$((INC_DEPTH - 1))
  unset "INC_CHAIN[$(( ${#INC_CHAIN[@]} - 1 ))]"
  return 0
}

# include 指令解析：以 / 开头 → 绝对路径直接解析（无需搜索路径）；
# 相对 → 依次尝试各搜索目录，第一个命中为准；含 glob（* 或 ?）→ 展开匹配
# （无匹配 = OK，nginx 语义，跳过）；非 glob 且全部搜索目录未命中 → exit 2
# （无任何搜索路径配置时同样 exit 2，不猜）。
do_include() {
  local file="$1" lineno="$2" raw="$3" path d m tried='' ndirs=0
  path="$(strip_quotes "$raw")"
  if [ -z "$path" ]; then
    parse_fatal "$file:$lineno: include directive has an empty argument"
  fi
  if [ "${path#/}" != "$path" ]; then
    if has_glob_chars "$path"; then
      expand_glob "$path"
      if [ "${#GLOB_MATCHES[@]}" -eq 0 ]; then
        return 0
      fi
      for m in ${GLOB_MATCHES[@]+"${GLOB_MATCHES[@]}"}; do
        include_enter "$file" "$lineno" "$m"
      done
      return 0
    fi
    include_enter "$file" "$lineno" "$path"
    return 0
  fi
  if has_glob_chars "$path"; then
    for d in ${INCLUDE_PATHS[@]+"${INCLUDE_PATHS[@]}"}; do
      ndirs=$((ndirs + 1))
      expand_glob "$d/$path"
      if [ "${#GLOB_MATCHES[@]}" -gt 0 ]; then
        for m in ${GLOB_MATCHES[@]+"${GLOB_MATCHES[@]}"}; do
          include_enter "$file" "$lineno" "$m"
        done
        return 0
      fi
    done
    return 0
  fi
  for d in ${INCLUDE_PATHS[@]+"${INCLUDE_PATHS[@]}"}; do
    ndirs=$((ndirs + 1))
    if [ -f "$d/$path" ]; then
      include_enter "$file" "$lineno" "$d/$path"
      return 0
    fi
    tried="$tried $d"
  done
  if [ "$ndirs" -eq 0 ]; then
    parse_fatal "$file:$lineno: relative include '$path' cannot be resolved: no include search path is configured (use --include-path, \$L4_SNI_INCLUDE_PATH, or NGINX_INCLUDE_PATH in the env file)"
  fi
  parse_fatal "$file:$lineno: relative include '$path' not found in any include search dir (tried:$tried)"
}

handle_directive() {
  local file="$1" lineno="$2" word="${BUF[0]:-}" ipath=''
  if [ "${#BUF[@]}" -eq 0 ]; then return 0; fi
  case "$word" in
    include)
      if [ "${#BUF[@]}" -ne 2 ]; then
        parse_fatal "$file:$lineno: include directive takes exactly one argument (got $(( ${#BUF[@]} - 1 )))"
      fi
      ipath="${BUF[1]}"
      # 先清空缓冲再递归：被 include 文件的 token 不得拼进 include 指令本身
      BUF=()
      do_include "$file" "$lineno" "$ipath"
      ;;
    listen)
      if [ "$SRV_OPEN" -eq 1 ]; then
        record_listen "$file" "$lineno"
      else
        parse_fatal "$file:$lineno: listen directive outside any server block"
      fi
      ;;
    server_name)
      if [ "$SRV_OPEN" -eq 1 ] && [ -z "$SRV_NAME" ] && [ "${#BUF[@]}" -ge 2 ]; then
        SRV_NAME="${BUF[1]}"
      fi
      ;;
    ssl_certificate|ssl_certificate_key)
      if [ "$SRV_OPEN" -eq 1 ]; then SRV_CERT=1; fi
      ;;
  esac
  return 0
}

handle_block_open() {
  local file="$1" lineno="$2" word="${BUF[0]:-}"
  if [ "${#BUF[@]}" -eq 0 ]; then
    parse_fatal "$file:$lineno: unexpected '{' without a directive name"
  fi
  if [ "$word" = include ]; then
    parse_fatal "$file:$lineno: include directive must be terminated by ';' (found '{')"
  fi
  if [ "$word" = server ]; then
    if [ "$DEPTH" -ne 0 ] || [ "$SRV_OPEN" -eq 1 ]; then
      parse_fatal "$file:$lineno: server block must be at file top level (vhost file convention)"
    fi
    SRV_OPEN=1; SRV_LINE=$lineno; SRV_FILE="$file"; SRV_CERT=0; SRV_NAME=''
    S_LLINE=(); S_LFILE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()
    DEPTH=$((DEPTH + 1))
    SRV_DEPTH=$DEPTH
    return 0
  fi
  # v1.2（缺口 #5）：顶层 upstream 块与 server 同级合法（宝塔 vhost 常态），
  # 仅做结构化解析（DEPTH 配平）；块内 `server host:port;` 是指令，走
  # handle_directive 的无匹配忽略路径，不与 server 块混淆；块内 listen 在
  # SRV_OPEN=0 下仍 fatal。upstream 的 `}` 只减 DEPTH（SRV_OPEN=0 时不触发
  # evaluate_server）。其余顶层块维持 fail-closed。
  if [ "$DEPTH" -eq 0 ] && [ "$word" != upstream ]; then
    parse_fatal "$file:$lineno: unexpected top-level block '$word' (only server and upstream blocks are supported)"
  fi
  DEPTH=$((DEPTH + 1))
  return 0
}

handle_block_close() {
  local file="$1" lineno="$2"
  DEPTH=$((DEPTH - 1))
  if [ "$DEPTH" -lt 0 ]; then
    parse_fatal "$file:$lineno: unbalanced '}' (no matching open block)"
  fi
  if [ "$SRV_OPEN" -eq 1 ] && [ "$DEPTH" -eq $((SRV_DEPTH - 1)) ]; then
    evaluate_server "$file"
    SRV_OPEN=0
  fi
  return 0
}

# token 消费。换行即空白（v1.1 P1）：BUF 跨行保持，指令/块仅在 ';' '}'
# 处终结；BUF 非空时 handle_directive/handle_block_open 收到的是指令起始
# 位置（BUF_FILE:BUF_LINE，诊断口径）。
consume_tokens() {
  local file="$1" lineno="$2" tok bl_file bl_line
  for tok in ${TOKENS[@]+"${TOKENS[@]}"}; do
    case "$tok" in
      ';')
        N_DIR=$((N_DIR + 1))
        if [ "${#BUF[@]}" -gt 0 ]; then
          handle_directive "$BUF_FILE" "$BUF_LINE"
        else
          handle_directive "$file" "$lineno"
        fi
        BUF=()
        ;;
      '{')
        if [ "${#BUF[@]}" -gt 0 ]; then
          bl_file="$BUF_FILE"; bl_line="$BUF_LINE"
        else
          bl_file="$file"; bl_line="$lineno"
        fi
        handle_block_open "$bl_file" "$bl_line"
        BUF=()
        ;;
      '}')
        if [ "${#BUF[@]}" -gt 0 ]; then
          parse_fatal "$file:$lineno: unexpected tokens before '}' (malformed directive; expected ';' first)"
        fi
        handle_block_close "$file" "$lineno"
        ;;
      *)
        if [ "${#BUF[@]}" -eq 0 ]; then
          BUF_FILE="$file"
          BUF_LINE="$lineno"
        fi
        BUF+=("$tok")
        ;;
    esac
  done
  return 0
}

# server 块收口判定：产出违规诊断（file:line）与 drift 指标序列。
# v1.1 P3 去重：序列仅对受管 HTTPS（is_https）server 产出，HTTP-only 块
# 不再生成序列（文件级 fail-closed 违规判定不变）；同一受检文件内 HTTPS
# server 标签冲突按出现顺序追加 #2、#3。config_file 标签 = 受检顶层文件
# （经 include 发现的 server 归入包含它的受检文件）。
evaluate_server() {
  local file="$1" i=0 n="${#S_PORT[@]}" is_https=0 has10443=0 drift=0
  local label base suffix clash j m
  while [ "$i" -lt "$n" ]; do
    if [ "${S_SSL[$i]}" = 1 ] || [ "${S_PORT[$i]}" = 443 ] || [ "${S_PORT[$i]}" = 10443 ]; then
      is_https=1
    fi
    i=$((i + 1))
  done
  if [ "$SRV_CERT" = 1 ]; then is_https=1; fi
  if [ "$is_https" = 1 ]; then FILE_HAS_HTTPS_SRV=1; fi
  i=0
  while [ "$i" -lt "$n" ]; do
    if [ "${S_PORT[$i]}" = 443 ]; then
      if [ "$MODE" = strict ]; then
        viol "${S_LFILE[$i]}:${S_LLINE[$i]}: direct :443 listener '${S_ADDR[$i]}:443' is drift (post-switch contract requires loopback:10443 with proxy_protocol)"
        drift=1
      fi
    elif [ "${S_PORT[$i]}" = 10443 ]; then
      has10443=1
      if [ "${S_ADDR[$i]}" != 127.0.0.1 ] && [ "${S_ADDR[$i]}" != '[::1]' ]; then
        viol "${S_LFILE[$i]}:${S_LLINE[$i]}: :10443 listener must bind 127.0.0.1 or [::1] (found '${S_ADDR[$i]}')"
        drift=1
      fi
      if [ "${S_PP[$i]}" != 1 ]; then
        viol "${S_LFILE[$i]}:${S_LLINE[$i]}: :10443 listener is missing a standalone proxy_protocol token"
        drift=1
      fi
    elif [ "${S_SSL[$i]}" = 1 ]; then
      viol "${S_LFILE[$i]}:${S_LLINE[$i]}: HTTPS(ssl) listener on unexpected '${S_ADDR[$i]}:${S_PORT[$i]}' (expected loopback:10443 with proxy_protocol)"
      drift=1
    fi
    i=$((i + 1))
  done
  if [ "$MODE" = strict ] && [ "$is_https" = 1 ] && [ "$has10443" = 0 ]; then
    viol "$SRV_FILE:$SRV_LINE: managed HTTPS server has no loopback:10443 listener"
    drift=1
  fi
  if [ "$is_https" = 1 ]; then
    if [ -n "$SRV_NAME" ]; then base="$SRV_NAME"; else base="server-$((NSRV + 1))"; fi
    label="$base"
    suffix=2
    m="${#M_FILE[@]}"
    while :; do
      clash=0
      j=0
      while [ "$j" -lt "$m" ]; do
        if [ "${M_FILE[$j]}" = "$CUR_FILE" ] && [ "${M_SERVER[$j]}" = "$label" ]; then
          clash=1
          break
        fi
        j=$((j + 1))
      done
      if [ "$clash" -eq 0 ]; then
        break
      fi
      label="$base#$suffix"
      suffix=$((suffix + 1))
    done
    M_FILE+=("$CUR_FILE")
    M_SERVER+=("$label")
    M_DRIFT+=("$drift")
  fi
  NSRV=$((NSRV + 1))
  return 0
}

# 解析一段文件文本流：词法 + token 消费。include 递归进入目标文件时共享
# 全部全局解析状态（DEPTH/SRV_OPEN/BUF 等），token 流跨文件连续（nginx
# 语义：include 文件末尾未完结的指令由包含文件的后续 token 续上）。
# EOF 级检查（未完结指令/未闭合块）只在顶层 parse_file 做。
parse_stream() {
  local file="$1" line='' lineno=0 content=''
  if ! content="$(LC_ALL=C cat "$file" 2>/dev/null)"; then
    parse_fatal "$file:0: cannot open file for reading"
  fi
  while IFS= read -r line; do
    lineno=$((lineno + 1))
    lex_line "$line"
    if [ "$LEX_ERROR" -eq 1 ]; then
      parse_fatal "$file:$lineno: unterminated quote (cannot tokenize reliably)"
    fi
    if [ "$LEX_COMMENT" -eq 1 ]; then HAS_COMMENT=1; fi
    consume_tokens "$file" "$lineno"
  done <<< "$content"
  LAST_LINENO=$lineno
  return 0
}

# 顶层受检文件入口：重置状态、解析（含 include 递归）、EOF fail-closed
# 检查与受管配置缺失判定。
parse_file() {
  local file="$1"
  DEPTH=0; NSRV=0; N_DIR=0; HAS_COMMENT=0; FILE_HAS_HTTPS_SRV=0
  SRV_OPEN=0; SRV_LINE=0; SRV_DEPTH=0; SRV_CERT=0; SRV_NAME=''; SRV_FILE="$file"
  S_LLINE=(); S_LFILE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()
  BUF=(); BUF_FILE="$file"; BUF_LINE=0
  CUR_FILE="$file"
  INC_DEPTH=0; INC_CHAIN=("$file")
  parse_stream "$file"
  # EOF fail-closed（v1.1 P1）：指令未完结（token 缓冲非空）= 可能截断的
  # 文件；诊断定位到指令起始位置（含 include 片段中的起始文件与行号）。
  if [ "${#BUF[@]}" -gt 0 ]; then
    parse_fatal "$BUF_FILE:$BUF_LINE: unterminated directive at end of file (missing ';' — file may be truncated)"
  fi
  if [ "$DEPTH" -ne 0 ]; then
    parse_fatal "$file:$LAST_LINENO: unclosed block at end of file (brace depth $DEPTH)"
  fi
  # fail-closed 口径（计划 Task 1：只有 HTTP/注释/空文件不能通过）：
  # 受检文件必须含至少一个受管 HTTPS server 定义（ssl listener、443/10443
  # 端口或 ssl_certificate）。缺失即受管配置缺失（漂移），报违规退出 1；
  # --strict 与 --pre-switch 均适用。
  if [ "$NSRV" -eq 0 ]; then
    if [ "$N_DIR" -eq 0 ] && [ "$HAS_COMMENT" -eq 1 ]; then
      viol "$file:1: no managed HTTPS server block found in managed config (comments-only file)"
    elif [ "$N_DIR" -eq 0 ]; then
      viol "$file:1: no managed HTTPS server block found in managed config (empty file)"
    else
      viol "$file:1: no managed HTTPS server block found in managed config (no server block)"
    fi
  elif [ "$FILE_HAS_HTTPS_SRV" -eq 0 ]; then
    viol "$file:1: no managed HTTPS server block found in managed config (HTTP-only server blocks)"
  fi
  return 0
}

metric_escape() {
  local s="$1"
  s="${s//\\/\\\\}"
  s="${s//\"/\\\"}"
  printf '%s' "$s"
}

metrics_text() {
  local i=0 n="${#M_DRIFT[@]}" cf sb
  printf '# TYPE imboy_l4_sni_listen_drift gauge\n'
  while [ "$i" -lt "$n" ]; do
    cf="$(metric_escape "${M_FILE[$i]}")"
    sb="$(metric_escape "${M_SERVER[$i]}")"
    printf 'imboy_l4_sni_listen_drift{config_file="%s",server="%s"} %s\n' "$cf" "$sb" "${M_DRIFT[$i]}"
    i=$((i + 1))
  done
  printf '# TYPE imboy_l4_sni_check_success gauge\n'
  printf 'imboy_l4_sni_check_success %s\n' "$PARSE_OK"
  printf '# TYPE imboy_l4_sni_last_check_timestamp_seconds gauge\n'
  printf 'imboy_l4_sni_last_check_timestamp_seconds %s\n' "$(date -u +%s)"
  return 0
}

do_push() {
  local url out
  if [ -z "$PUSH_URL" ]; then
    err "$PROG:0: --push requested but no Pushgateway URL is configured (set PUSHGATEWAY_URL or pass --pushgateway-url)"
    PUSH_FAILED=1
    return 0
  fi
  url="${PUSH_URL%/}/metrics/job/imboy_l4_sni_check"
  if ! out="$(printf '%s\n' "$METRICS_PAYLOAD" \
      | curl -sS --fail --max-time "${PUSH_TIMEOUT_SEC:-10}" --data-binary @- "$url" 2>&1)"; then
    err "$PROG:0: push to Pushgateway failed (url=$url): $out"
    PUSH_FAILED=1
  fi
  return 0
}

# 收尾：按需输出指标/摘要、执行推送；退出码优先级 3 > 2 > 1 > 0。
finalize() {
  local rc="$1"
  METRICS_PAYLOAD="$(metrics_text)"
  if [ "$WANT_METRICS" -eq 1 ]; then
    printf '%s\n' "$METRICS_PAYLOAD"
  elif [ "$rc" -eq 0 ]; then
    printf 'CHECK_OK: mode=%s files=%s servers=%s violations=0\n' \
      "$MODE" "${#CONFIGS[@]}" "$NSRV_TOTAL"
  else
    printf 'CHECK_%s: mode=%s files=%s servers=%s violations=%s\n' \
      "$(if [ "$rc" -eq 2 ]; then printf 'INPUT_ERROR'; else printf 'DRIFT'; fi)" \
      "$MODE" "${#CONFIGS[@]}" "$NSRV_TOTAL" "$VIOLATIONS"
  fi
  if [ "$WANT_PUSH" -eq 1 ]; then
    do_push
  fi
  if [ "$PUSH_FAILED" -eq 1 ]; then
    exit 3
  fi
  exit "$rc"
}

main() {
  local f rc=0
  parse_args "$@"
  if [ "${#CONFIGS[@]}" -eq 0 ]; then
    discover_configs
  fi
  # include 搜索路径合并（v1.1 P2，先到先搜）：--include-path（parse_args
  # 已按出现顺序收集）> $L4_SNI_INCLUDE_PATH > 发现 env 文件内的
  # NGINX_INCLUDE_PATH（env 文件与配置文件参数独立，显式传配置文件时仍读）。
  if [ -n "${L4_SNI_INCLUDE_PATH:-}" ]; then
    add_include_paths "$L4_SNI_INCLUDE_PATH"
  fi
  load_env_include_paths
  for f in ${CONFIGS[@]+"${CONFIGS[@]}"}; do
    if [ ! -f "$f" ]; then
      parse_fatal "$f:0: config file not found (checked files must exist)"
    fi
    if [ ! -r "$f" ]; then
      parse_fatal "$f:0: config file is not readable"
    fi
  done
  NSRV_TOTAL=0
  for f in ${CONFIGS[@]+"${CONFIGS[@]}"}; do
    parse_file "$f"
    NSRV_TOTAL=$((NSRV_TOTAL + NSRV))
  done
  if [ "$VIOLATIONS" -gt 0 ]; then rc=1; fi
  if [ "$PARSE_OK" -eq 0 ]; then rc=2; fi
  finalize "$rc"
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  main "$@"
fi
