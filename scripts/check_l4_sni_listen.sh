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
# fail-closed 原则：不能可靠解析的结构（include、跨行指令、未闭合块、
# 未知 listen 参数、顶层非 server 块）一律退出 2，绝不猜测通过。
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

# 单文件解析状态（parse_file 内跨行保持）
DEPTH=0; NSRV=0; N_DIR=0; HAS_COMMENT=0; FILE_HAS_HTTPS_SRV=0
SRV_OPEN=0; SRV_LINE=0; SRV_DEPTH=0; SRV_CERT=0; SRV_NAME=''
S_LLINE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()

# 词法状态（lex_line 输出）
TOKENS=(); LEX_COMMENT=0; LEX_ERROR=0
# 当前未完结指令的 token 缓冲（仅限单行内）
BUF=()

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
                         [--pushgateway-url URL] [config_file ...]

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
  PUSH_TIMEOUT_SEC  Push timeout in seconds, default 10.

Exit codes (precedence 3 > 2 > 1 > 0):
  0  healthy (no drift)
  1  drift/violation found; this includes a checked file with no
     managed HTTPS server definition (empty file, comments-only,
     HTTP-only server blocks, or no server block at all)
  2  input error: missing/unreadable file, empty list, unknown option,
     missing option value, or unparsable structure (include directive,
     multi-line directive, unbalanced/unclosed braces, listen outside
     a server block, unknown listen parameter, non-server top-level block)
  3  --push requested but Pushgateway URL missing or push failed

Metrics (Pushgateway text format, frozen names):
  imboy_l4_sni_listen_drift{config_file,server} gauge  1=drift 0=healthy
  imboy_l4_sni_check_success gauge  1=check fully ran 0=input/parse error
  imboy_l4_sni_last_check_timestamp_seconds gauge  unix epoch
  config_file label = path exactly as checked; server label = first
  server_name argument, else server-<n> (per-file ordinal).

Known limits (fail-closed, never guessed as pass):
  - include directives abort with exit 2; pass included files explicitly.
  - A directive spanning multiple lines aborts with exit 2.
  - A checked file with no managed HTTPS server definition (empty,
    comments-only, HTTP-only server blocks, or no server block) is
    missing managed config and exits 1 as drift.
  - server blocks must sit at file top level (vhost file convention).
  - Unquoted regex braces inside location patterns can confuse block
    accounting (typically surfacing as an exit 2, not a wrong pass).
USAGE
}

usage() {
  usage_basics
  usage_reference
}

parse_args() {
  while [ $# -gt 0 ]; do
    case "$1" in
      --strict|--pre-switch)
        if [ "$MODE_SET" -eq 1 ] && [ "$1" != "--$MODE" ]; then
          err "$PROG:0: choose exactly one mode (--strict or --pre-switch)"
          exit 2
        fi
        MODE="${1#--}"
        MODE_SET=1
        ;;
      --metrics) WANT_METRICS=1 ;;
      --push) WANT_PUSH=1 ;;
      --pushgateway-url)
        if [ $# -lt 2 ]; then
          err "$PROG:0: --pushgateway-url requires a URL"
          exit 2
        fi
        PUSH_URL="$2"
        shift
        ;;
      -h|--help) usage; exit 0 ;;
      --*|-*)
        err "$PROG:0: unknown option: $1"
        exit 2
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
  S_LLINE+=("$lineno"); S_ADDR+=("$addr"); S_PORT+=("$port"); S_SSL+=("0"); S_PP+=("0")
  if [ "${#BUF[@]}" -gt 2 ]; then
    for p in "${BUF[@]:2}"; do
      check_listen_param "$file" "$lineno" "$p"
    done
  fi
  return 0
}

handle_directive() {
  local file="$1" lineno="$2" word="${BUF[0]:-}"
  if [ "${#BUF[@]}" -eq 0 ]; then return 0; fi
  case "$word" in
    include)
      parse_fatal "$file:$lineno: include directive is not supported; pass the included file(s) explicitly instead"
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
  if [ "$word" = server ]; then
    if [ "$DEPTH" -ne 0 ] || [ "$SRV_OPEN" -eq 1 ]; then
      parse_fatal "$file:$lineno: server block must be at file top level (vhost file convention)"
    fi
    SRV_OPEN=1; SRV_LINE=$lineno; SRV_CERT=0; SRV_NAME=''
    S_LLINE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()
    DEPTH=$((DEPTH + 1))
    SRV_DEPTH=$DEPTH
    return 0
  fi
  if [ "$DEPTH" -eq 0 ]; then
    parse_fatal "$file:$lineno: unexpected top-level block '$word' (only server blocks are supported)"
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

consume_tokens() {
  local file="$1" lineno="$2" tok
  for tok in ${TOKENS[@]+"${TOKENS[@]}"}; do
    case "$tok" in
      ';')
        N_DIR=$((N_DIR + 1))
        handle_directive "$file" "$lineno"
        BUF=()
        ;;
      '{')
        handle_block_open "$file" "$lineno"
        BUF=()
        ;;
      '}')
        if [ "${#BUF[@]}" -gt 0 ]; then
          parse_fatal "$file:$lineno: unexpected tokens before '}' (malformed or multi-line directive)"
        fi
        handle_block_close "$file" "$lineno"
        ;;
      *) BUF+=("$tok") ;;
    esac
  done
  if [ "${#BUF[@]}" -gt 0 ]; then
    parse_fatal "$file:$lineno: directive spans multiple lines (not supported; refusing to guess)"
  fi
  return 0
}

# server 块收口判定：产出违规诊断（file:line）与一条 drift 指标序列。
evaluate_server() {
  local file="$1" i=0 n="${#S_PORT[@]}" is_https=0 has10443=0 drift=0 label addr port
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
    addr="${S_ADDR[$i]}"
    port="${S_PORT[$i]}"
    if [ "$port" = 443 ]; then
      if [ "$MODE" = strict ]; then
        viol "$file:${S_LLINE[$i]}: direct :443 listener '$addr:443' is drift (post-switch contract requires loopback:10443 with proxy_protocol)"
        drift=1
      fi
    elif [ "$port" = 10443 ]; then
      has10443=1
      if [ "$addr" != 127.0.0.1 ] && [ "$addr" != "[::1]" ]; then
        viol "$file:${S_LLINE[$i]}: :10443 listener must bind 127.0.0.1 or [::1] (found '$addr')"
        drift=1
      fi
      if [ "${S_PP[$i]}" != 1 ]; then
        viol "$file:${S_LLINE[$i]}: :10443 listener is missing a standalone proxy_protocol token"
        drift=1
      fi
    elif [ "${S_SSL[$i]}" = 1 ]; then
      viol "$file:${S_LLINE[$i]}: HTTPS(ssl) listener on unexpected '$addr:$port' (expected loopback:10443 with proxy_protocol)"
      drift=1
    fi
    i=$((i + 1))
  done
  if [ "$MODE" = strict ] && [ "$is_https" = 1 ] && [ "$has10443" = 0 ]; then
    viol "$file:$SRV_LINE: managed HTTPS server has no loopback:10443 listener"
    drift=1
  fi
  if [ -n "$SRV_NAME" ]; then label="$SRV_NAME"; else label="server-$((NSRV + 1))"; fi
  M_FILE+=("$file")
  M_SERVER+=("$label")
  M_DRIFT+=("$drift")
  NSRV=$((NSRV + 1))
  return 0
}

parse_file() {
  local file="$1" line='' lineno=0
  DEPTH=0; NSRV=0; N_DIR=0; HAS_COMMENT=0; FILE_HAS_HTTPS_SRV=0
  SRV_OPEN=0; SRV_LINE=0; SRV_DEPTH=0; SRV_CERT=0; SRV_NAME=''
  S_LLINE=(); S_ADDR=(); S_PORT=(); S_SSL=(); S_PP=()
  if ! { exec 3<"$file"; } 2>/dev/null; then
    parse_fatal "$file:0: cannot open file for reading"
  fi
  while IFS= read -r line <&3 || [ -n "$line" ]; do
    lineno=$((lineno + 1))
    lex_line "$line"
    if [ "$LEX_ERROR" -eq 1 ]; then
      parse_fatal "$file:$lineno: unterminated quote (cannot tokenize reliably)"
    fi
    if [ "$LEX_COMMENT" -eq 1 ]; then HAS_COMMENT=1; fi
    consume_tokens "$file" "$lineno"
  done
  # 注意：这里绝不能带 2>/dev/null——对 exec 的重定向会在当前 shell
  # 持久生效，把后续所有 stderr 诊断吞掉。
  exec 3<&-
  if [ "$DEPTH" -ne 0 ]; then
    parse_fatal "$file:$lineno: unclosed block at end of file (brace depth $DEPTH)"
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
