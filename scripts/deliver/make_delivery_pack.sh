#!/usr/bin/env bash
# make_delivery_pack.sh — 一期交付包生成（AC27：tracked 白名单打包）
# Generate the Phase-1 delivery pack from the git-tracked whitelist.
#
# 用法 / Usage:
#   scripts/deliver/make_delivery_pack.sh [--out-dir DIR] [--no-tar]
#                                        [--secret-fingerprints FILE]
#
#   --out-dir DIR             manifest 与 tar 输出目录（默认 /tmp，绝不写仓内）
#   --no-tar                  只生成 manifest 并跑自检，不产 tar
#   --secret-fingerprints F   环境特定凭据指纹清单（每行一个字面量，仓外维护，
#                             绝不提交）；命中即 FAIL。未提供时跳过并提示。
#
# 设计原则（详见 docs/deliver/deliver-pack.md）：
#   1. 白名单 = `git ls-files`。未 tracked 的工作区产物（_build/、deps/、
#      erl_crash.dump、config/sys.local*、顶层 evidence/、运行日志）天然不进包。
#   2. tracked 集将来若混入禁项，第 2 步排除 + 第 3 步自检双保险拦截。
#   3. tar 只输出到 --out-dir（默认 /tmp），仓库内不落任何生成物。
#
# AC27 禁项 ↔ 排除规则对照：
#   X1 真实配置      config/sys.{local,dev,prod,eunit}*（任意层级防御）
#   X2 真实 env      .env 及 .env.<variant>；*.example 模板是"配置入口"豁免保留
#   X3 日志/crash    *.log（含 docs 归档中的 evidence 日志原件——归档可追溯性由
#                    仓内 git 历史与结论文档承担）；erl_crash.dump / crash.dump
#   X4 任意层级 evidence/test-results/playwright-report（执行材料不交付）
#   X5 缓存          node_modules/
#   X6 凭据命名      basename 含 private[_-]?key（一律拦）；password 作为词元
#                    （^ _ - . 分界）且扩展非源码/契约/文档类
#                    （.erl/.hrl/.app.src/.yaml/.yml/.md/.py/.sh/.ts/.js 豁免——
#                    elib_password.erl、change-password.yaml 属业务语义非凭据）
#   内容级（硬失败）  真实私钥 PEM 块（私钥头 + 200+ 连续 base64 高熵体）；
#                    --secret-fingerprints 提供时的已知环境口令指纹。
#
# 退出码：0 成功；1 自检失败或参数错误。

set -euo pipefail

# ---------------------------------------------------------------------------
# 参数解析
# ---------------------------------------------------------------------------
OUT_DIR=""
NO_TAR=0
FINGERPRINTS=""

while [ $# -gt 0 ]; do
    case "$1" in
        --out-dir)             OUT_DIR="${2:?--out-dir needs a value}"; shift 2 ;;
        --no-tar)              NO_TAR=1; shift ;;
        --secret-fingerprints) FINGERPRINTS="${2:?--secret-fingerprints needs a value}"; shift 2 ;;
        -h|--help)             sed -n '2,12p' "$0"; exit 0 ;;
        *) echo "ERROR: unknown arg: $1" >&2; exit 1 ;;
    esac
done

# ---------------------------------------------------------------------------
# 前置：必须在 git 仓内运行；输出目录默认 /tmp（绝不写仓内）
# ---------------------------------------------------------------------------
REPO_ROOT="$(git rev-parse --show-toplevel 2>/dev/null)" || {
    echo "ERROR: not inside a git repository" >&2; exit 1;
}
cd "$REPO_ROOT"

GIT_SHA="$(git rev-parse HEAD)"
GIT_SHA8="$(git rev-parse --short HEAD)"
STAMP="$(date +%Y%m%d%H%M%S)"
: "${OUT_DIR:=/tmp/imboy-delivery-${GIT_SHA8}-${STAMP}}"
mkdir -p "$OUT_DIR"
# 输出目录若在仓库内则拒绝（防止生成物污染 tracked 集与文件域）
case "$OUT_DIR/" in
    "$REPO_ROOT"/*) echo "ERROR: --out-dir must be outside the repository: $OUT_DIR" >&2; exit 1 ;;
esac

MANIFEST="$OUT_DIR/manifest.txt"
META="$OUT_DIR/manifest.meta.md"
TAR_OUT="$OUT_DIR/imboy-delivery-${GIT_SHA8}-${STAMP}.tar"
WORK="$OUT_DIR/.work"
mkdir -p "$WORK"

sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{print $1}';
           else shasum -a 256 "$1" | awk '{print $1}'; fi; }

log()  { printf '[deliver] %s\n' "$*"; }
fail() { printf '[deliver] FAIL: %s\n' "$*" >&2; exit 1; }

# ---------------------------------------------------------------------------
# 规则函数：输入路径列表文件，输出命中"凭据命名特征"的路径。
# 排除阶段与自检阶段共用，保证两者逻辑永不漂移。
# ---------------------------------------------------------------------------
credential_named_paths() {
    local list="$1"
    # R6a private[_-]?key 字样 —— 任何目录/扩展一律视为凭据命名
    awk -F/ '{print $NF"\t"$0}' "$list" \
        | grep -iE 'private[_-]?key' | cut -f2
    # R6b password 词元（basename 内以 ^ _ - . 为界）且扩展非源码/契约/文档
    awk -F/ '{print $NF"\t"$0}' "$list" \
        | grep -iE '(^|[._-])password([._-]|$)' \
        | grep -ivE '\.(erl|hrl|app\.src|yaml|yml|md|py|sh|ts|js)(\.gz)?$' | cut -f2
}

# ---------------------------------------------------------------------------
# 第 1 步：tracked 白名单
# ---------------------------------------------------------------------------
log "step 1/5: collecting git-tracked whitelist (git ls-files)"
# core.quotepath=off：非 ASCII 文件名原样输出；默认的八进制转义+引号形式
# 会让下游 tar -T 按字面匹配而 stat 失败（中文测试图片文件名实测踩坑）。
git -c core.quotepath=off ls-files | grep . > "$WORK/all.list" || true
TRACKED_N=$(grep -c . "$WORK/all.list" || true)
log "  tracked files: $TRACKED_N"

# ---------------------------------------------------------------------------
# 第 2 步：排除规则（AC27 禁项；pattern 与第 3 步自检一一对应）
# ---------------------------------------------------------------------------
log "step 2/5: applying AC27 exclusion rules"
: > "$WORK/exclude.list"

# X1 真实环境配置：config/sys.{local,dev,prod,eunit}*（任意层级防御）
grep -E '(^|/)config/sys\.(local|dev|prod|eunit)' "$WORK/all.list" >> "$WORK/exclude.list" || true
# X3a 日志原件：*.log
grep -E '\.log$'                                    "$WORK/all.list" >> "$WORK/exclude.list" || true
# X3b crash dump
grep -E '(^|/)(erl_)?crash\.dump$'                  "$WORK/all.list" >> "$WORK/exclude.list" || true
# X2 真实 env 文件：.env 及 .env.<variant>；*.example 豁免（配置入口模板）
grep -E '(^|/)\.env' "$WORK/all.list" | grep -v '\.example$' >> "$WORK/exclude.list" || true
# X5 缓存 / 第三方包目录
grep -E '(^|/)node_modules/'                        "$WORK/all.list" >> "$WORK/exclude.list" || true
# X4 Execution artifacts are excluded at every depth.
grep -E '(^|/)(evidence|test-results|playwright-report)/' "$WORK/all.list" >> "$WORK/exclude.list" || true
# X6 凭据命名（见 credential_named_paths 注释）
credential_named_paths "$WORK/all.list"             >> "$WORK/exclude.list" || true

sort -u "$WORK/exclude.list" -o "$WORK/exclude.list"
EXCLUDED_N=$(grep -c . "$WORK/exclude.list" || true)

# 白名单 = tracked - 排除集（整行精确差集）
grep -vxF -f "$WORK/exclude.list" "$WORK/all.list" > "$MANIFEST" || true
MANIFEST_N=$(grep -c . "$MANIFEST" || true)
log "  excluded: $EXCLUDED_N  ->  manifest entries: $MANIFEST_N"

# ---------------------------------------------------------------------------
# 第 3 步：自检（对最终 manifest 断言；任一命中即 FAIL，双保险）
# ---------------------------------------------------------------------------
log "step 3/5: running exclusion self-checks on manifest"
VIOLATIONS="$WORK/violations.txt"; : > "$VIOLATIONS"

grep -E '(^|/)config/sys\.(local|dev|prod|eunit)' "$MANIFEST" >> "$VIOLATIONS" || true
grep -E '\.log$'                                   "$MANIFEST" >> "$VIOLATIONS" || true
grep -E '(^|/)(erl_)?crash\.dump$'                 "$MANIFEST" >> "$VIOLATIONS" || true
grep -E '(^|/)\.env' "$MANIFEST" | grep -v '\.example$' >> "$VIOLATIONS" || true
grep -E '(^|/)node_modules/'                       "$MANIFEST" >> "$VIOLATIONS" || true
grep -E '(^|/)(evidence|test-results|playwright-report)/' "$MANIFEST" >> "$VIOLATIONS" || true
credential_named_paths "$MANIFEST" | sed 's/^/[cred-name] /' >> "$VIOLATIONS" || true

# 内容级 A：真实私钥 PEM 块 = 私钥头 + 200+ 连续 base64 高熵体。
# 文档注释、占位串（测试 fixture 里的 "test"/"key"）不会命中。
PEM_HITS=$(tr '\n' '\0' < "$MANIFEST" | xargs -0 grep -IlE -- \
    '-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----' 2>/dev/null || true)
for f in $PEM_HITS; do
    if grep -qE '[A-Za-z0-9+/]{200,}' "$f" 2>/dev/null; then
        echo "[real-pem] $f" >> "$VIOLATIONS"
    fi
done

# 内容级 B：已知环境凭据指纹（指纹文件仓外维护，本脚本绝不内嵌任何真实凭据）
if [ -n "$FINGERPRINTS" ] && [ -s "$FINGERPRINTS" ]; then
    while IFS= read -r fp; do
        [ -n "$fp" ] || continue
        HITS=$(tr '\n' '\0' < "$MANIFEST" | xargs -0 grep -IlF -- "$fp" 2>/dev/null || true)
        for f in $HITS; do echo "[fingerprint] $f" >> "$VIOLATIONS"; done
    done < "$FINGERPRINTS"
else
    log "  NOTE: no --secret-fingerprints given, environment-specific secret scan skipped"
fi

if [ -s "$VIOLATIONS" ]; then
    cat "$VIOLATIONS" >&2
    fail "self-check found $(grep -c . "$VIOLATIONS") violation(s) — see above"
fi
log "  self-check PASS: 0 violations"

# WARN（不阻断，供交付 owner 复核）：PEM 文件清点
PEM_LIST=$(grep -E '\.pem$' "$MANIFEST" || true)
if [ -n "$PEM_LIST" ]; then
    log "  WARN: .pem files in pack (verify they are public keys / dev fixtures):"
    printf '    %s\n' $PEM_LIST
fi

# ---------------------------------------------------------------------------
# 第 4 步：manifest 元信息（可追溯：SHA、时间、规则、规模）
# ---------------------------------------------------------------------------
{
    echo "# imboy phase-1 delivery manifest"
    echo ""
    echo "- generated_at: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "- git_sha: $GIT_SHA"
    echo "- tracked_files: $TRACKED_N"
    echo "- excluded_files: $EXCLUDED_N"
    echo "- manifest_entries: $MANIFEST_N"
    echo "- rules: AC27 exclusions (see scripts/deliver/make_delivery_pack.sh header)"
    echo "  * config/sys.{local,dev,prod,eunit}* ; real .env (non-example) kept out"
    echo "  * *.log ; crash dumps ; node_modules/"
    echo "  * evidence/, test-results/, playwright-report/ at every depth"
    echo "  * credential-looking filenames (private-key always; password as a"
    echo "    filename token with non-source/contract/doc extension)"
    echo "  * content: real PEM private-key blocks; optional secret fingerprints"
    echo "- docs: docs/deliver/deliver-pack.md"
} > "$META"
log "step 4/5: wrote $MANIFEST ($MANIFEST_N entries) and $META"

# ---------------------------------------------------------------------------
# 第 5 步：tar（仅输出到 OUT_DIR，默认 /tmp）+ 完整性验证
# ---------------------------------------------------------------------------
if [ "$NO_TAR" -eq 1 ]; then
    log "step 5/5: --no-tar given, skip tar; done."
    exit 0
fi
log "step 5/5: creating tar (output only under $OUT_DIR)"
tar -cf "$TAR_OUT" -C "$REPO_ROOT" -T "$MANIFEST"
TAR_N=$(tar -tf "$TAR_OUT" | grep -c . || true)
[ "$TAR_N" -eq "$MANIFEST_N" ] || fail "tar entries ($TAR_N) != manifest entries ($MANIFEST_N)"
TAR_SHA=$(sha256 "$TAR_OUT")
TAR_SIZE=$(du -h "$TAR_OUT" | awk '{print $1}')
log "  tar ok: $TAR_OUT"
log "  entries: $TAR_N  size: $TAR_SIZE  sha256: $TAR_SHA"
echo "$TAR_SHA  $(basename "$TAR_OUT")" > "$OUT_DIR/tar.sha256"
log "done. pack artifacts in: $OUT_DIR"
