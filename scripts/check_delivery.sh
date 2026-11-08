#!/usr/bin/env bash
# Offline package integrity check. No Docker, network, .env sourcing or deployment.
set -euo pipefail
ROOT="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"
fail() { printf '[FAIL] %s\n' "$*" >&2; exit 1; }
for tool in sha256sum stat bash; do
    command -v "$tool" >/dev/null 2>&1 || fail "缺少离线检查工具: $tool"
done
for path in MANIFEST.sha256 FILE_MODES.tsv SOURCE_COMMIT install.sh \
    deploy/install.sh deploy/preflight.sh deploy/.env.example \
    deploy/docker-compose.community.yml deploy/nginx/init-letsencrypt.sh \
    deploy/nginx/templates/imboy.conf.template \
    deploy/nginx/templates/cs-widget.conf.template \
    deploy/nginx/rtc.conf.template scripts/sanity_check.sh \
    scripts/install_guard.sh; do
    [[ -f "$path" && -r "$path" && ! -L "$path" ]] || fail "缺少、不可读或异常符号链接: $path"
done
while IFS= read -r record; do
    [[ "$record" =~ ^[0-9a-f]{64}\ \ (.+)$ ]] || fail '校验清单格式异常'
    path="${BASH_REMATCH[1]}"
    case "$path" in /*|../*|*/../*|*/..|*\\*) fail "清单路径异常: $path" ;; esac
    [[ -f "$path" && -r "$path" && ! -L "$path" ]] || fail "交付文件缺失或异常: $path"
done < MANIFEST.sha256
sha256sum --check --quiet MANIFEST.sha256 || fail '文件校验值不一致；请恢复原包，不要修改清单来绕过检查'
shell_count=0
while IFS=$'\t' read -r mode path; do
    [[ -n "$path" ]] || fail '权限清单格式异常'
    [[ -f "$path" && -r "$path" ]] || fail "文件不可读: $path"
    actual="$(stat -c '%a' -- "$path")"
    [[ "$actual" == "$mode" ]] || fail "文件权限应为 $mode，实际为 $actual: $path"
    case "$path" in
        *.sh)
            bash -n "$path" || fail "Shell 语法错误: $path"
            shell_count=$((shell_count + 1))
            ;;
    esac
done < FILE_MODES.tsv
for path in deploy/nginx/templates/*; do
    case "$path" in
        deploy/nginx/templates/imboy.conf.template|deploy/nginx/templates/cs-widget.conf.template) ;;
        *) fail "Nginx 模板目录出现未审核文件: $path" ;;
    esac
done
for path in deploy/prometheus/rules/*; do
    [[ "$path" == deploy/prometheus/rules/imboy-alerts.yml ]] || fail "Prometheus 规则目录出现未审核文件: $path"
done
printf '[PASS] 文件字节、相对路径、权限与 %s 个 Shell 文件语法通过。\n' "$shell_count"
printf '[INFO] 此检查不连接 Docker，不证明镜像、TLS、数据库或客户业务已通过。\n'
