#!/usr/bin/env bash
# ---------------------------------------------------------------------------
# plugin_install.sh — 从 Git 仓库安装插件到 imboy
# Install a plugin from a Git repository into imboy
#
# 用法 / Usage:
#   ./scripts/plugin_install.sh <git-url> <version> [options]
#
# 选项 / Options:
#   -d, --plugins-dir DIR   插件目标目录（默认 priv/plugins）
#   -k, --public-key FILE   签名公钥文件（用于签名插件）
#   -n, --name NAME         插件目录名（默认从 git URL 推断）
#   --no-sign               跳过签名校验
#   --no-restart            不触发 loader 热扫描
#   -h, --help              显示帮助
#
# 示例 / Examples:
#   ./scripts/plugin_install.sh https://gitee.com/imboy-pub/imboy-plugin-channel.git v1.2.0
#   ./scripts/plugin_install.sh https://gitee.com/imboy-pub/imboy-plugin-channel.git v1.2.0 -k /path/to/pub.key
#   ./scripts/plugin_install.sh ./local-plugin-dir v1.0.0 -n my_plugin
#
# 退出码 / Exit codes:
#   0 = 成功
#   1 = 参数错误
#   2 = git clone/checkout 失败
#   3 = 插件清单校验失败
#   4 = 签名校验失败
#   5 = 热扫描触发失败
# ---------------------------------------------------------------------------

set -euo pipefail

# ---------------------------------------------------------------------------
# 默认值
# ---------------------------------------------------------------------------
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
PLUGINS_DIR="${PROJECT_DIR}/priv/plugins"
PUBLIC_KEY_FILE=""
PLUGIN_NAME=""
NO_SIGN=false
NO_RESTART=false
GIT_URL=""
VERSION=""

# ---------------------------------------------------------------------------
# 帮助信息
# ---------------------------------------------------------------------------
usage() {
    sed -n '3,28p' "$0" | sed 's/^# //' >&2
    exit 1
}

# ---------------------------------------------------------------------------
# 参数解析
# ---------------------------------------------------------------------------
while [[ $# -gt 0 ]]; do
    case "$1" in
        -d|--plugins-dir) PLUGINS_DIR="$2"; shift 2 ;;
        -k|--public-key)  PUBLIC_KEY_FILE="$2"; shift 2 ;;
        -n|--name)        PLUGIN_NAME="$2"; shift 2 ;;
        --no-sign)        NO_SIGN=true; shift ;;
        --no-restart)     NO_RESTART=true; shift ;;
        -h|--help)        usage ;;
        -*)
            echo "ERROR: Unknown option: $1" >&2
            usage
            ;;
        *)
            if [[ -z "$GIT_URL" ]]; then
                GIT_URL="$1"
            elif [[ -z "$VERSION" ]]; then
                VERSION="$1"
            else
                echo "ERROR: Unexpected argument: $1" >&2
                usage
            fi
            shift
            ;;
    esac
done

if [[ -z "$GIT_URL" ]] || [[ -z "$VERSION" ]]; then
    echo "ERROR: git-url and version are required" >&2
    usage
fi

# ---------------------------------------------------------------------------
# 辅助函数
# ---------------------------------------------------------------------------

log()  { echo "[plugin_install] $*"; }
warn() { echo "[plugin_install] WARN: $*" >&2; }
die()  { echo "[plugin_install] ERROR: $*" >&2; exit "${2:-1}"; }

# 从 git URL 推断插件目录名
infer_name_from_url() {
    local url="$1"
    local base
    base="$(basename "$url" .git)"
    # imboy-plugin-channel -> channel
    echo "$base" | sed 's/^imboy-plugin-//'
}

# 临时目录（自动清理）
TMPDIR=""
cleanup() {
    if [[ -n "$TMPDIR" ]] && [[ -d "$TMPDIR" ]]; then
        rm -rf "$TMPDIR"
    fi
}
trap cleanup EXIT
TMPDIR="$(mktemp -d /tmp/imboy_plugin_install.XXXXXX)"

# ---------------------------------------------------------------------------
# 主流程
# ---------------------------------------------------------------------------

# 1. 确定插件名
if [[ -z "$PLUGIN_NAME" ]]; then
    PLUGIN_NAME="$(infer_name_from_url "$GIT_URL")"
fi
log "Plugin name: $PLUGIN_NAME"
log "Version: $VERSION"

# 2. 获取插件源码
CLONE_DIR="$TMPDIR/plugin_src"
if [[ -d "$GIT_URL" ]]; then
    # 本地路径：直接复制
    log "Copying from local path: $GIT_URL"
    cp -r "$GIT_URL" "$CLONE_DIR"
else
    # 远程 git：clone + checkout
    log "Cloning $GIT_URL ..."
    if ! git clone --depth 1 --branch "$VERSION" "$GIT_URL" "$CLONE_DIR" 2>&1; then
        die "git clone failed (branch=$VERSION)" 2
    fi
fi

# 3. 验证插件清单
CONFIG_FILE="$CLONE_DIR/plugin.config"
if [[ ! -f "$CONFIG_FILE" ]]; then
    die "plugin.config not found in source" 3
fi
log "Found plugin.config"

# 简单校验：确保文件非空且包含 name 字段
if ! grep -q 'name' "$CONFIG_FILE"; then
    die "plugin.config missing 'name' field" 3
fi

# 4. 签名校验（如果启用）
SIGNATURE_FILE="$CLONE_DIR/SIGNATURE"
if [[ "$NO_SIGN" == "false" ]] && [[ -n "$PUBLIC_KEY_FILE" ]]; then
    if [[ ! -f "$SIGNATURE_FILE" ]]; then
        die "SIGNATURE file not found (signing enforced with -k)" 4
    fi
    if [[ ! -f "$PUBLIC_KEY_FILE" ]]; then
        die "Public key file not found: $PUBLIC_KEY_FILE" 4
    fi

    log "Verifying Ed25519 signature..."
    SIG_BYTES="$(wc -c < "$SIGNATURE_FILE" | tr -d ' ')"
    if [[ "$SIG_BYTES" -ne 64 ]]; then
        die "SIGNATURE file must be exactly 64 bytes (got $SIG_BYTES)" 4
    fi

    PUB_BYTES="$(wc -c < "$PUBLIC_KEY_FILE" | tr -d ' ')"
    if [[ "$PUB_BYTES" -ne 32 ]]; then
        die "Public key file must be exactly 32 bytes (got $PUB_BYTES)" 4
    fi

    # 真正的 crypto 验证在 imboy_plugin_loader 加载时完成
    # 这里做基本格式校验
    log "Signature: 64 bytes OK, Public key: 32 bytes OK"
    log "Signature will be re-verified by imboy_plugin_loader on start"
fi

# 5. 安装到目标目录
TARGET_DIR="$PLUGINS_DIR/$PLUGIN_NAME"
if [[ -d "$TARGET_DIR" ]]; then
    warn "Target directory already exists, replacing: $TARGET_DIR"
    rm -rf "$TARGET_DIR"
fi

mkdir -p "$PLUGINS_DIR"
cp -r "$CLONE_DIR" "$TARGET_DIR"
log "Installed to: $TARGET_DIR"

# 6. 列出安装的文件
log "Installed files:"
find "$TARGET_DIR" -type f | sed "s|$TARGET_DIR/|  |" | head -20

# 7. 触发热扫描（如果节点在线）
if [[ "$NO_RESTART" == "false" ]]; then
    if [[ -x "$SCRIPT_DIR/imboy_ctl" ]]; then
        log "Triggering plugin loader rescan..."
        if escript "$SCRIPT_DIR/imboy_ctl" plugin scan 2>/dev/null; then
            log "Rescan triggered successfully"
        else
            warn "Could not trigger rescan (node may be offline). Plugins will load on next restart."
        fi
    else
        warn "imboy_ctl not found. Plugins will load on next restart."
    fi
fi

log "Done. Plugin '$PLUGIN_NAME' ($VERSION) installed."
