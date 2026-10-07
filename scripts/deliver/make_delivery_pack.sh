#!/usr/bin/env bash
# make_delivery_pack.sh — 一期交付包生成（AC27：tracked 白名单打包）
# Generate the Phase-1 delivery pack from an immutable committed whitelist.
#
# 用法 / Usage:
#   scripts/deliver/make_delivery_pack.sh [--out-dir DIR] [--no-tar]
#                                        [--secret-fingerprints FILE] [--require-profile NAME]
#
#   --out-dir DIR             manifest 与 tar 输出目录（默认 /tmp，绝不写仓内）
#   --no-tar                  只生成 manifest 并跑自检，不产 tar
#   --require-profile NAME    要求已提交 manifest 的 profile 匹配；不改写源码
#   --secret-fingerprints F   环境特定凭据指纹清单（每行一个字面量，仓外维护，
#                             绝不提交）；命中即 FAIL。未提供时跳过并提示。
#
# 设计原则（详见 docs/deliver/deliver-pack.md）：
#   1. 白名单与文件字节均来自固定 HEAD；暂存和工作区修改不进入交付包。
#      未提交的工作区产物（_build/、deps/、
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
#   内容级（硬失败）  完整私钥 PEM 块（支持分行 Base64）；
#                    --secret-fingerprints 提供时的已知环境口令指纹。
#
# 退出码：0 成功；1 自检失败或参数错误。

set -euo pipefail

# ---------------------------------------------------------------------------
# 参数解析
# ---------------------------------------------------------------------------
umask 077
OUT_DIR=""
NO_TAR=0
FINGERPRINTS=""
REQUIRED_PROFILE=""

while [ $# -gt 0 ]; do
    case "$1" in
        --out-dir)             OUT_DIR="${2:?--out-dir needs a value}"; shift 2 ;;
        --no-tar)              NO_TAR=1; shift ;;
        --require-profile)     REQUIRED_PROFILE="${2:?--require-profile needs a value}"; shift 2 ;;
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
# Resolve relative paths, parent components and symlinks before creating output.
OUT_DIR="$(python3 - "$REPO_ROOT" "$OUT_DIR" <<'PY_PATH'
from pathlib import Path
import sys
root, output = (Path(value).resolve() for value in sys.argv[1:])
if output == root or root in output.parents:
    sys.exit("ERROR: --out-dir must be outside the repository")
print(output)
PY_PATH
)" || exit 1
mkdir -p "$OUT_DIR"

MANIFEST="$OUT_DIR/manifest.txt"
META="$OUT_DIR/manifest.meta.md"
TAR_OUT="$OUT_DIR/imboy-delivery-${GIT_SHA8}-${STAMP}.tar"
WORK="$OUT_DIR/.work"
mkdir -p "$WORK"
SOURCE="$WORK/source"
[ ! -e "$SOURCE" ] || { echo "ERROR: output already contains a source snapshot" >&2; exit 1; }
mkdir "$SOURCE"
if [ -n "$FINGERPRINTS" ]; then
    FINGERPRINTS="$(python3 - "$FINGERPRINTS" <<'PY_FINGERPRINT'
from pathlib import Path
import sys
print(Path(sys.argv[1]).resolve())
PY_FINGERPRINT
)"
fi
# Snapshot immutable committed bytes; staged and working-tree edits are excluded.
git archive --format=tar "$GIT_SHA" | tar -xf - -C "$SOURCE"
cd "$SOURCE"

sha256() { if command -v sha256sum >/dev/null 2>&1; then sha256sum "$1" | awk '{print $1}';
           else shasum -a 256 "$1" | awk '{print $1}'; fi; }

log()  { printf '[deliver] %s\n' "$*"; }
fail() { printf '[deliver] FAIL: %s\n' "$*" >&2; exit 1; }

# Read the immutable snapshot, never the dirty workspace's manifest.
PROFILE="$(python3 - "$REQUIRED_PROFILE" <<'PY_PROFILE'
import json
from pathlib import Path
import re
import sys
path = Path('config/product-feature-manifest.json')
profile = 'unbound'
if path.exists():
    profile = json.loads(path.read_text()).get('profile')
    if not isinstance(profile, str) or not re.fullmatch(r'[a-zA-Z0-9_-]+', profile):
        sys.exit('ERROR: invalid manifest profile')
if sys.argv[1] and profile != sys.argv[1]:
    sys.exit(f'ERROR: profile mismatch: required {sys.argv[1]}, committed {profile}')
print(profile)
PY_PROFILE
)" || exit 1
MANIFEST_FILE_HASH="unbound"
if [ -f config/product-feature-manifest.json ]; then
    MANIFEST_FILE_HASH="$(sha256 config/product-feature-manifest.json)"
fi

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

# Product delivery manifest section 2: development metadata and environment
# snapshots are not customer deliverables. Share this rule with the final check.
contract_excluded_paths() {
    grep -E '^(deploy/nginx/prod-vhosts/|docs-site/|\.github/|\.githooks/|\.contract/|\.ecc/|\.qa/|\.gitcode(/|$)|docs/(qa|design|plans)/|docs/archive/design/|docs/runbooks/agent-hub-[^/]*\.md$|\.mcp\.json$|\.elp\.toml$|\.gradualizer-baseline\.txt$|lefthook\.yml$|sonar-project\.properties$|index\.html$|AGENTS\.md$|CLAUDE\.md$)' "$1"
}

# ---------------------------------------------------------------------------
# 第 1 步：tracked 白名单
# ---------------------------------------------------------------------------
log "step 1/5: collecting committed whitelist (git ls-tree)"
python3 - "$REPO_ROOT" "$GIT_SHA" "$WORK/all.list" <<'PY_LIST'
from pathlib import Path
import subprocess
import sys
entries = subprocess.check_output([
    "git", "-C", sys.argv[1], "ls-tree", "-rz", "--full-tree", sys.argv[2]
]).split(b"\0")
names = []
for entry in filter(None, entries):
    metadata, name = entry.split(b"\t", 1)
    if metadata.split()[1] != b"blob":
        sys.exit("ERROR: submodule content requires an explicit delivery contract")
    if b"\n" in name or b"\r" in name:
        sys.exit("ERROR: newline in delivery filename")
    names.append(name)
Path(sys.argv[3]).write_bytes(b"\n".join(sorted(names)) + b"\n")
PY_LIST

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
grep -E '(^|/)(node_modules|_build|\.otp)/'                        "$WORK/all.list" >> "$WORK/exclude.list" || true
# X4 Execution artifacts are excluded at every depth.
grep -E '(^|/)(evidence|test-results|playwright-report)/' "$WORK/all.list" >> "$WORK/exclude.list" || true
# X6 凭据命名（见 credential_named_paths 注释）
credential_named_paths "$WORK/all.list"             >> "$WORK/exclude.list" || true
contract_excluded_paths "$WORK/all.list" >> "$WORK/exclude.list" || true

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
grep -E '(^|/)(node_modules|_build|\.otp)/'                       "$MANIFEST" >> "$VIOLATIONS" || true
grep -E '(^|/)(evidence|test-results|playwright-report)/' "$MANIFEST" >> "$VIOLATIONS" || true
credential_named_paths "$MANIFEST" | sed 's/^/[cred-name] /' >> "$VIOLATIONS" || true
contract_excluded_paths "$MANIFEST" | sed 's/^/[delivery-contract] /' >> "$VIOLATIONS" || true

# Content check: decode complete PEM bodies, including conventional wrapped lines.
python3 - "$MANIFEST" "$VIOLATIONS" <<'PY_PEM'
import base64
import binascii
from pathlib import Path
import re
import sys
pattern = re.compile(
    rb"-----BEGIN ((?:RSA |EC |OPENSSH |ENCRYPTED )?PRIVATE KEY)-----"
    rb"(.*?)-----END \1-----", re.S
)
root = Path.cwd().resolve()
names = Path(sys.argv[1]).read_text().splitlines()
selected = set(names)
with Path(sys.argv[2]).open("a") as violations:
    for name in names:
        if Path(name).is_symlink():
            try:
                target = Path(name).resolve(strict=True)
                target_name = str(target.relative_to(root))
                safe = target.is_file() and target_name in selected
            except (OSError, RuntimeError, ValueError):
                safe = False
            if not safe:
                violations.write(f"[symlink] {name}\n")
                continue
        for block in pattern.finditer(Path(name).read_bytes()):
            body = re.sub(rb"\s+", b"", block.group(2))
            try:
                decoded = base64.b64decode(body, validate=True)
            except (ValueError, binascii.Error):
                continue
            if len(decoded) >= 32:
                violations.write(f"[real-pem] {name}\n")
                break
PY_PEM

if grep -q '^\[symlink\]' "$VIOLATIONS"; then
    cat "$VIOLATIONS" >&2
    fail "unsafe delivery symlink"
fi

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
    echo "# imboy source delivery manifest"
    echo ""
    echo "- generated_at: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
    echo "- git_sha: $GIT_SHA"
    echo "- profile: $PROFILE"
    echo "- manifest_file_sha256: $MANIFEST_FILE_HASH"
    echo "- source: immutable git archive snapshot (index/worktree excluded)"
    echo "- tracked_files: $TRACKED_N"
    echo "- excluded_files: $EXCLUDED_N"
    echo "- manifest_entries: $MANIFEST_N"
    echo "- rules: AC27 exclusions (see scripts/deliver/make_delivery_pack.sh header)"
    echo "  * config/sys.{local,dev,prod,eunit}* ; real .env (non-example) kept out"
    echo "  * *.log ; crash dumps ; node_modules/"
    echo "  * evidence/, test-results/, playwright-report/ at every depth"
    echo "  * production vhost snapshots, development metadata and task documents"
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
python3 - "$SOURCE" "$MANIFEST" "$TAR_OUT" <<'PY_TAR'
from pathlib import Path
import sys
import tarfile
root = Path(sys.argv[1])
with tarfile.open(sys.argv[3], "w") as archive:
    for name in Path(sys.argv[2]).read_text().splitlines():
        archive.add(root / name, arcname=name, recursive=False)
PY_TAR
TAR_N=$(tar -tf "$TAR_OUT" | grep -c . || true)
[ "$TAR_N" -eq "$MANIFEST_N" ] || fail "tar entries ($TAR_N) != manifest entries ($MANIFEST_N)"
TAR_SHA=$(sha256 "$TAR_OUT")
TAR_SIZE=$(du -h "$TAR_OUT" | awk '{print $1}')
log "  tar ok: $TAR_OUT"
log "  entries: $TAR_N  size: $TAR_SIZE  sha256: $TAR_SHA"
echo "$TAR_SHA  $(basename "$TAR_OUT")" > "$OUT_DIR/tar.sha256"
log "done. pack artifacts in: $OUT_DIR"
