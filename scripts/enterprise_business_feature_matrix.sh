#!/usr/bin/env bash
# ============================================================================
# enterprise_business feature 三档矩阵（disposable worktree 版）—— EB-10
#
# 目的（plan.snapshot §8 EB-10 / EB-10-A01..A04）：
#   把 enterprise_business 接进**现成**的 feature 机制（manifest → 编译期宏 →
#   erlang.mk ERLC_EXCLUDE 物理裁剪 → imboy_router 的 -ifdef 路由段剔除 →
#   .app modules → relx release），并在**一次性工作树**上跑三档选择矩阵：
#
#     1. neither                     既不选 enterprise_business 也不选 customer_service
#     2. enterprise-only             只选 enterprise_business
#     3. enterprise-customer-service 再叠加 customer_service（依赖 enterprise_business）
#
#   逐档断言五层资产：
#     A01（selected）  宏 / 路由 / beam / .app modules / release 都在
#     A02（unselected）宏 / 路由 / beam / .app modules / release 都不在
#     A03              customer_service 无 enterprise_business ⇒ **生成即失败**
#     A04              往返（neither → enterprise-only → neither）无陈旧 beam，
#                      且 tracked tree hash 复原
#
# 为什么必须一次性工作树（**不得切换共享 manifest**）：
#   生成器把产物写到 `<repo>/include/generated/**` 与 `<workspace>/imboyapp|imboyadmin`
#   ——都是共享主树里的 tracked 文件。本脚本用 `git worktree add` 造副本，再把
#   调用方工作树的 tracked 改动 + 未跟踪（非忽略）文件 **overlay** 进去（本 run 的
#   src/features/enterprise_business/** 尚未 commit，只复刻 HEAD 会没有被测对象）。
#   生成/编译/打包全部发生在副本里；调用方工作树的 tracked tree hash、未跟踪
#   文件清单与 manifest 指纹在开头/结尾各算一次，不等即 FAIL（这就是"跑完
#   tracked tree hash 复原"的直接证据）。
#
# 用法：
#   bash scripts/enterprise_business_feature_matrix.sh                          # 三档全跑
#   bash scripts/enterprise_business_feature_matrix.sh enterprise-only           # 只跑一档（调试）
#   EB_FEATURE_MATRIX_EVIDENCE_DIR=<dir> bash scripts/enterprise_business_feature_matrix.sh
#
# 退出码：0 = 三档全部断言通过；1 = 有断言红（逐条 FAIL 行点名被测能力）。
#
# 已知环境性质（不是本脚本的缺陷，见 ORDER §环境坑）：
#   erlang.mk 按 mtime 判新鲜。副本里 overlay 进来的 .erl 与生成物必须比既有构建
#   标记新，否则"新加入的模块"会被静默跳过编译。故每次 generate 后对生成物与
#   企业源做 touch（**只改 mtime，不改内容**，且只作用于副本）。
# ============================================================================
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PRESET_FILTER="${1:-all}"
# 调用方脚本路径仅用于“这个仓确实有生成器”的自检；实际生成一律用副本内的脚本。
PY_GENERATOR="$ROOT/scripts/generate_product_features.py"
MANIFEST="$ROOT/config/product-feature-manifest.json"

PRESET_NAMES="neither enterprise-only enterprise-customer-service"

TMPBASE="$(mktemp -d "${TMPDIR:-/tmp}/imboy-eb-feature-matrix.XXXXXX")"
EVIDENCE_DIR="${EB_FEATURE_MATRIX_EVIDENCE_DIR:-$TMPBASE/evidence}"
WTLIST="$TMPBASE/worktrees.txt"
OVERLAY_LIST="$TMPBASE/overlay-list.txt"
: > "$WTLIST"
FAILED=0

case "$PRESET_FILTER" in
  all | neither | enterprise-only | enterprise-customer-service) ;;
  *)
    echo "用法: $0 [all|neither|enterprise-only|enterprise-customer-service]" >&2
    echo "未知档位: $PRESET_FILTER" >&2
    exit 2
    ;;
esac

log()  { printf '%s\n' "$*"; }
ok()   { printf '  [ok]   %s\n' "$*"; }
fail() { printf '  [FAIL] %s\n' "$*" >&2; FAILED=$((FAILED + 1)); }

cleanup() {
  local status=$?
  trap - EXIT
  while IFS= read -r wt; do
    [ -n "$wt" ] || continue
    git -C "$ROOT" worktree remove --force "$wt" >/dev/null 2>&1 || rm -rf "$wt"
  done < "$WTLIST"
  git -C "$ROOT" worktree prune >/dev/null 2>&1 || true
  rm -rf "$TMPBASE"
  exit "$status"
}
trap cleanup EXIT

# ---------------------------------------------------------------------------
# 工具
# ---------------------------------------------------------------------------

# tracked tree hash：tracked 改动的 binary diff + 未跟踪（非忽略）文件内容，
# 与 scripts/verify_product_feature_artifacts.py:worktree_hash 同一算法。
tree_hash() {
  python3 - "$1" <<'PY'
import hashlib
import pathlib
import subprocess
import sys

repo = pathlib.Path(sys.argv[1])


def git(*args):
    return subprocess.check_output(["git", "-C", str(repo), *args])


digest = hashlib.sha256()
digest.update(git("diff", "--binary", "HEAD"))
names = subprocess.check_output(
    ["git", "-C", str(repo), "ls-files", "--others", "--exclude-standard", "-z"]
).decode()
for rel in sorted(name for name in names.split("\0") if name):
    digest.update(rel.encode())
    path = repo / rel
    if path.is_file():
        digest.update(path.read_bytes())
print(digest.hexdigest())
PY
}

file_sha256() {
  python3 -c 'import hashlib,sys;print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest())' "$1"
}

# 企业 feature 的模块清单：**从被测源码目录独立枚举**（-module 属性），
# 不读生成器的 FEATURE_BACKEND_MODULES —— 断言必须独立于被断言对象，否则
# 「物理裁剪映射行丢失」会让清单一起变空、断言退化成 0/0 的假绿。
# 「生成器映射 == 目录清单」由 test/scripts/test_generate_product_features.py
# 的 test_enterprise_backend_modules_cover_feature_directory 保证。
enterprise_modules() {
  find "$ROOT/src/features/enterprise_business" -name '*.erl' -exec \
    sed -n 's/^-module(\([a-z][a-z0-9_]*\)).*/\1/p' {} + | sort -u | tr '\n' ' '
}

modules_in_app() {
  python3 - "$1" <<'PY'
import pathlib
import re
import sys

text = pathlib.Path(sys.argv[1]).read_text()
match = re.search(r"\{modules\s*,\s*\[(.*?)\]\}", text, re.S)
if match is None:
    raise SystemExit(f"no modules entry in {sys.argv[1]}")
for item in match.group(1).split(","):
    name = item.strip().strip("'")
    if name:
        print(name)
PY
}

# 依赖目录状态（路径+mtime+size 摘要）：用来证明矩阵**没有写调用方的 deps**。
deps_state() {
  find "$1/deps" -type f -exec stat -f '%N %m %z' {} + 2>/dev/null | sort | shasum -a 256 | cut -d' ' -f1
}

# ---------------------------------------------------------------------------
# 一次性工作树
# ---------------------------------------------------------------------------

new_worktree() {   # $1 = 名字；stdout = 副本路径
  local name="$1" wt
  # 目录名必须是 `imboy`：erlang.mk 的 `relx-rel` 用 `erl -pa ebin/` 调 relx，
  # relx 经 code:lib_dir(imboy) 解析 release 里的本应用 —— code:lib_dir 只认
  # 「父目录名 == 应用名」的路径条目。副本若叫 wt-xxx，relx 会在组装阶段以
  # `{app_not_found,imboy,undefined}` 炸掉（2026-09-14 实测）。
  # 每档各自一个父目录，互不干扰：$TMPBASE/wt-<preset>/imboy。
  wt="$TMPBASE/wt-$name/imboy"
  mkdir -p "$(dirname "$wt")"
  git -C "$ROOT" worktree add --detach "$wt" "$(git -C "$ROOT" rev-parse HEAD)" >/dev/null 2>&1
  printf '%s\n' "$wt" >> "$WTLIST"
  copy_deps "$wt"
  # `imboy -> .` 是 eunit-local/-pa 布局约定（preflight_imboy_symlink.sh）。
  ln -s . "$wt/imboy"
  # relx.config 的 {sys_config, "config/sys.runtime.config"} 不随 git 走（gitignored）
  if [ -f "$ROOT/config/sys.runtime.config" ]; then
    cp "$ROOT/config/sys.runtime.config" "$wt/config/sys.runtime.config"
  fi
  overlay_working_tree "$wt"
  printf '%s\n' "$wt"
}

# deps **不能**用软链共享：make 会在 deps/<dep>/<dep>.d 里写入**绝对路径**，
# 共享会让下一次构建读到指向另一个（甚至已被删除的）工作树的头文件路径，
# 报 `No rule to make target .../deps/cowlib/include/cow_inline.hrl`，并且
# 反向污染调用方的 deps。故每档复制一份（macOS 用 clonefile 免拷贝成本），
# 复制后删掉带进来的 .d（它们记的是源工作树的绝对路径，make 会自行重建）。
copy_deps() {
  local wt="$1"
  mkdir -p "$wt/deps"
  if ! cp -Rcp "$ROOT/deps/." "$wt/deps/" 2>/dev/null; then
    cp -Rp "$ROOT/deps/." "$wt/deps/"
  fi
  rm -f "$wt/deps"/*/*.d
}

overlay_working_tree() {
  local wt="$1" rel
  git -C "$ROOT" status --porcelain -uall | sed 's/^...//' > "$OVERLAY_LIST"
  while IFS= read -r rel; do
    [ -n "$rel" ] || continue
    mkdir -p "$wt/$(dirname "$rel")"
    cp -p "$ROOT/$rel" "$wt/$rel"
  done < "$OVERLAY_LIST"
}

# 三档 manifest：Base = 调用方 manifest 的 selected_features 去掉
# enterprise_business / customer_service（**不改调用方 manifest 一个字节**，
# 只写副本之外的临时 manifest 文件）。
write_preset_manifests() {
  python3 - "$MANIFEST" "$TMPBASE" <<'PY'
import json
import pathlib
import sys

manifest_path = pathlib.Path(sys.argv[1])
out_dir = pathlib.Path(sys.argv[2]) / "manifests"
out_dir.mkdir(parents=True, exist_ok=True)
base = json.loads(manifest_path.read_text())["selected_features"]
base = [f for f in base if f not in ("enterprise_business", "customer_service")]
presets = {
    "neither": [],
    "enterprise-only": ["enterprise_business"],
    "enterprise-customer-service": ["enterprise_business", "customer_service"],
}
for name, extra in presets.items():
    value = {
        "schema_version": 1,
        "product_id": "imboy",
        "profile": name,
        "base_ref": "imboy-feature-inventory-v1",
        "selected_features": base + extra,
    }
    (out_dir / f"{name}.json").write_text(json.dumps(value, indent=2) + "\n")
    print(f"{name}: {len(value['selected_features'])} features")
PY
}

# 生成器必须用**副本自己的** scripts/generate_product_features.py 调用：
# 它内部用 `Path(__file__).resolve().parents[1]` 当 repo 根，把 11 个产物写到
# 「脚本所在仓 + 其父目录的 imboyapp/imboyadmin」。若用调用方的脚本路径，
# 产物会被写回**调用方**（共享主树），副本根本没被生成 —— 矩阵退化成
# 「重复测试调用方当前状态」，而调用方还可能被静默改写。
generate() {   # $1 = 副本, $2 = preset 名
  # bash 3.2：`local a=1 b=$a` 里后一项看不见前一项（set -u 下 "a: unbound variable"）
  local wt="$1" name="$2"
  local manifest="$TMPBASE/manifests/$name.json"
  ( cd "$wt" && python3 "$wt/scripts/generate_product_features.py" --manifest "$manifest" ) || return 1
  # 生成后立刻用 --check 复验「副本自己的 11 个产物 == 本 preset」。这条同时
  # 钉住「产物真的写进了副本」（含副本父目录的 imboyapp/imboyadmin）而不是写回
  # 调用方 —— 后者会让矩阵静默退化成「重复测试调用方当前状态」。
  if ! ( cd "$wt" && python3 "$wt/scripts/generate_product_features.py" \
      --check --manifest "$manifest" ) >/dev/null 2>&1; then
    fail "生成产物与 preset 不一致：副本 $wt 的 include/generated 或 workspace 产物未按 preset 刷新"
    return 1
  fi
  refresh_mtimes "$wt"
}

generate_check() {   # $1 = 副本, $2 = manifest 路径
  local wt="$1" manifest="$2"
  ( cd "$wt" && python3 "$wt/scripts/generate_product_features.py" --check --manifest "$manifest" )
}

refresh_mtimes() {
  local wt="$1"
  find "$wt/include/generated" -type f -exec touch {} +
  find "$wt/src/features/enterprise_business" -name '*.erl' -exec touch {} +
}

build_release() {   # $1 = 副本, $2 = tag
  local wt="$1" tag="$2"
  if ! ( cd "$wt" && IMBOYENV=local make rel ) > "$TMPBASE/build-$tag.log" 2>&1; then
    log "  ---- make rel 失败，末尾 25 行 ----"
    tail -25 "$TMPBASE/build-$tag.log" >&2
    return 1
  fi
  return 0
}

release_ebin_dir() {   # $1 = 副本；stdout = release 内 imboy 的 ebin
  local wt="$1" d
  for d in "$wt"/_rel/imboy/lib/imboy-*/ebin; do
    [ -d "$d" ] || continue
    printf '%s\n' "$d"
    return 0
  done
  return 1
}

# 运行期路由探针：直接读 imboy_router:get_routes/0（EB-09 契约核对同一入口）
route_probe() {   # $1 = 副本；stdout = "tenant=<n> platform=<n>"
  local wt="$1" pa="" d
  for d in "$wt"/deps/*/ebin; do
    [ -d "$d" ] && pa="$pa -pa $d"
  done
  ( cd "$TMPBASE" && ERL_CRASH_DUMP="$TMPBASE/erl_crash.dump" \
      erl -noshell -pa "$wt/ebin" $pa -eval '
        Routes = lists:append([Rs || {_H, Rs} <- imboy_router:get_routes()]),
        Ent = [P || {P, _, _} <- Routes, is_list(P),
                    (lists:prefix("/api/v1/enterprise", P) orelse
                     lists:prefix("/api/adm/enterprise-business", P))],
        T = [P || P <- Ent, lists:prefix("/api/v1/enterprise", P)],
        A = [P || P <- Ent, lists:prefix("/api/adm/enterprise-business", P)],
        io:format("ROUTE_PROBE tenant=~p platform=~p~n", [length(T), length(A)]),
        halt().' 2>/dev/null ) | tr -d '\r' | grep -o 'tenant=[0-9]* platform=[0-9]*' | tail -1
}

# ---------------------------------------------------------------------------
# 五层断言（selected = 应存在 / unselected = 应不存在）
# ---------------------------------------------------------------------------

assert_layer_macro() {
  local wt="$1" expect="$2" hrl="$1/include/generated/imboy_product_features.hrl"
  if [ "$expect" = selected ]; then
    if grep -qF -- '-define(IMBOY_FEATURE_ENTERPRISE_BUSINESS, true).' "$hrl"; then
      ok "宏：include/generated/imboy_product_features.hrl 定义 IMBOY_FEATURE_ENTERPRISE_BUSINESS"
    else
      fail "宏：selected 却缺 -define(IMBOY_FEATURE_ENTERPRISE_BUSINESS, true)"
    fi
  else
    if grep -q 'enterprise_business' "$hrl"; then
      fail "宏：unselected 却仍在 $(basename "$hrl") 出现 enterprise_business（第 $(grep -n 'enterprise_business' "$hrl" | head -1 | cut -d: -f1) 行）"
    else
      ok "宏：unselected 时 hrl 无 enterprise_business（宏 / compiled_features 双无）"
    fi
  fi
}

assert_layer_erlc_exclude() {
  local wt="$1" expect="$2" mk="$1/include/generated/imboy_product_features_erlc.mk" m hits=0
  for m in $ENTERPRISE_MODULES; do
    case " $(sed -n 's/^IMBOY_FEATURE_ERLC_EXCLUDE *:=//p' "$mk") " in
      *" $m "*) hits=$((hits + 1)) ;;
    esac
  done
  if [ "$expect" = selected ]; then
    if [ "$hits" -eq 0 ]; then
      ok "erlc.mk：selected 时 ERLC_EXCLUDE 不含任何企业模块（$ENTERPRISE_MODULE_COUNT 个全在场）"
    else
      fail "erlc.mk：selected 却有 $hits 个企业模块被 ERLC_EXCLUDE 排除"
    fi
  else
    if [ "$hits" -eq "$ENTERPRISE_MODULE_COUNT" ]; then
      ok "erlc.mk：unselected 时 $ENTERPRISE_MODULE_COUNT 个企业模块全部进 ERLC_EXCLUDE"
    else
      fail "erlc.mk：unselected 只排除 $hits/$ENTERPRISE_MODULE_COUNT 个企业模块（漏排即残留编译）"
    fi
  fi
}

assert_layer_router_strings() {
  local wt="$1" expect="$2" beam="$1/ebin/imboy_router.beam" n
  n="$(grep -c -a -- '/api/v1/enterprise/\|/api/adm/enterprise-business/' "$beam" 2>/dev/null || true)"
  if [ "$expect" = selected ]; then
    if [ "$n" -gt 0 ] 2>/dev/null; then
      ok "路由：imboy_router.beam 含企业路径字符串（物理在场）"
    else
      fail "路由：selected 却 imboy_router.beam 里没有企业路径字符串"
    fi
  else
    if [ "$n" -eq 0 ] 2>/dev/null; then
      ok "路由：unselected 时 imboy_router.beam 零企业路径字符串（编译期已剔除，非运行时开关）"
    else
      fail "路由：unselected 却 imboy_router.beam 残留企业路径字符串（匹配行 ${n}）"
    fi
  fi
}

assert_layer_routes() {
  local wt="$1" expect="$2" probe tenant platform
  probe="$(route_probe "$wt")"
  if [ -z "$probe" ]; then
    fail "路由表：route_probe 无输出（erl 探针失败）"
    return
  fi
  tenant="${probe#tenant=}"
  tenant="${tenant%% *}"
  platform="${probe##*platform=}"
  if [ "$expect" = selected ]; then
    if [ "$tenant" = 16 ] && [ "$platform" = 10 ]; then
      ok "路由表：get_routes/0 企业路由租户 16 + 平台 10"
    else
      fail "路由表：selected 期望租户 16/平台 10，实得 $probe"
    fi
  else
    if [ "$tenant" = 0 ] && [ "$platform" = 0 ]; then
      ok "路由表：get_routes/0 零企业路由（${probe}）"
    else
      fail "路由表：unselected 期望 0/0，实得 $probe"
    fi
  fi
}

assert_layer_beams() {
  local wt="$1" expect="$2" m present=0
  for m in $ENTERPRISE_MODULES; do
    [ -f "$wt/ebin/$m.beam" ] && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$ENTERPRISE_MODULE_COUNT" ]; then
      ok "beam：ebin 中 $present/$ENTERPRISE_MODULE_COUNT 个企业 beam 全在"
    else
      fail "beam：selected 只有 $present/$ENTERPRISE_MODULE_COUNT 个企业 beam"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "beam：ebin 中 0/$ENTERPRISE_MODULE_COUNT 个企业 beam（含陈旧 beam）"
    else
      fail "beam：unselected 仍残留 $present 个企业 beam（陈旧 beam 未剪）"
    fi
  fi
}

assert_layer_app_modules() {
  local wt="$1" expect="$2" app="$1/ebin/imboy.app" mods="$TMPBASE/app-modules.txt" m present=0
  if [ ! -f "$app" ]; then
    fail "app modules：$app 不存在"
    return
  fi
  modules_in_app "$app" > "$mods"
  for m in $ENTERPRISE_MODULES; do
    grep -qx "$m" "$mods" && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$ENTERPRISE_MODULE_COUNT" ]; then
      ok "app：ebin/imboy.app {modules,_} 含 $present/$ENTERPRISE_MODULE_COUNT 个企业模块"
    else
      fail "app：selected 只有 $present/$ENTERPRISE_MODULE_COUNT 个企业模块进 .app"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "app：ebin/imboy.app {modules,_} 零企业模块"
    else
      fail "app：unselected 仍有 $present 个企业模块在 .app modules"
    fi
  fi
}

assert_layer_release() {
  local wt="$1" expect="$2" ebin m present=0
  if ! ebin="$(release_ebin_dir "$wt")"; then
    fail "release：未找到 _rel/imboy/lib/imboy-*/ebin"
    return
  fi
  for m in $ENTERPRISE_MODULES; do
    [ -f "$ebin/$m.beam" ] && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$ENTERPRISE_MODULE_COUNT" ]; then
      ok "release：$(basename "$(dirname "$ebin")")/ebin 含 $present/$ENTERPRISE_MODULE_COUNT 个企业 beam"
    else
      fail "release：selected 只有 $present/$ENTERPRISE_MODULE_COUNT 个企业 beam 进 release"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "release：release 包内 0/$ENTERPRISE_MODULE_COUNT 个企业 beam"
    else
      fail "release：unselected 仍有 $present 个企业 beam 进 release"
    fi
  fi
}

# 断言灵敏度对照（非真空证明）：把 1 个 selected 层的 beam 暂时移走，存在性断言
# 必须**立刻报红**。它与 RED 的「unselected 五层全红」互为两侧：RED 证明
# 不存在性断言不恒真，本对照证明存在性断言（A01）不恒真。
probe_assertion_sensitivity() {   # $1 = 已构建的 selected 副本
  local wt="$1" beam="$1/ebin/eb_tenant_handler.beam" out
  if [ ! -f "$beam" ]; then
    fail "灵敏度对照：样本 $beam 不存在（selected 副本未构建？）"
    return 0
  fi
  mv "$beam" "$beam.probe"
  out="$(assert_layer_beams "$wt" selected 2>&1)"
  mv "$beam.probe" "$beam"
  if printf '%s' "$out" | grep -q '\[FAIL\]'; then
    ok "灵敏度对照：移走 1 个 selected beam ⇒ beam 断言立即红（存在性断言非恒真）"
  else
    fail "灵敏度对照：移走 selected beam 后断言仍绿 ⇒ beam 断言恒真，A01 不可信"
  fi
  if [ ! -f "$beam" ]; then
    fail "灵敏度对照还原失败：$beam 未回到原位"
  fi
}

assert_all_layers() {   # $1 = 副本, $2 = expect(selected|unselected), $3 = 档名
  log "  断言 [$3 / $2]"
  assert_layer_macro "$1" "$2"
  assert_layer_erlc_exclude "$1" "$2"
  assert_layer_router_strings "$1" "$2"
  assert_layer_routes "$1" "$2"
  assert_layer_beams "$1" "$2"
  assert_layer_app_modules "$1" "$2"
  assert_layer_release "$1" "$2"
}

write_preset_evidence() {   # $1 = tag, $2 = 说明, $3 = 选中的 feature 列表
  [ -n "$EVIDENCE_DIR" ] || return 0
  mkdir -p "$EVIDENCE_DIR"
  python3 - "$EVIDENCE_DIR/$1.json" "$1" "$2" "$3" "$FAILED" <<'PY'
import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "preset": sys.argv[2],
    "description": sys.argv[3],
    "selected_features": sys.argv[4].split(),
    "assertions_failed_so_far": int(sys.argv[5]),
}, indent=2, sort_keys=True) + "\n")
PY
}

# 逐档「生成产物」证据：manifest 选中的 feature、生成的宏、erlc.mk 排除行、
# ebin/.app/release 的企业模块计数，落成 <tag>-artifacts.txt。
capture_artifacts() {   # $1 = 副本, $2 = tag, $3 = 该 tag 对应的 preset manifest 名
  [ -n "$EVIDENCE_DIR" ] || return 0
  # 注意：bash 3.2 的 `local a=1 b=$a` 里后一项看不见前一项（set -u 下直接
  # "a: unbound variable"），故拆成两行。
  local wt="$1" tag="$2" preset="$3"
  local out="$EVIDENCE_DIR/$tag-artifacts.txt" m count=0 app_mods=0 rel=0 ebin=""
  mkdir -p "$EVIDENCE_DIR"
  {
    printf 'preset: %s\n' "$preset"
    printf 'worktree: %s\n' "$wt"
    printf 'base_commit: %s\n' "$(git -C "$wt" rev-parse HEAD)"
    printf 'manifest_selected: %s\n' \
      "$(python3 -c 'import json,sys;print(" ".join(json.load(open(sys.argv[1]))["selected_features"]))' \
        "$TMPBASE/manifests/$preset.json" 2>/dev/null || echo '(n/a)')"
    printf 'hrl_feature_defines:\n'
    grep -o -- '-define(IMBOY_FEATURE_[A-Z0-9_]*, true)\.' \
      "$wt/include/generated/imboy_product_features.hrl" | sed 's/^/  /' || true
    printf 'erlc_exclude: %s\n' "$(sed -n 's/^IMBOY_FEATURE_ERLC_EXCLUDE *:=//p' \
      "$wt/include/generated/imboy_product_features_erlc.mk")"
  } > "$out"
  for m in $ENTERPRISE_MODULES; do
    [ -f "$wt/ebin/$m.beam" ] && count=$((count + 1))
  done
  if [ -f "$wt/ebin/imboy.app" ]; then
    modules_in_app "$wt/ebin/imboy.app" > "$TMPBASE/app-modules.txt"
    while IFS= read -r m; do
      grep -qx "$m" "$TMPBASE/app-modules.txt" && app_mods=$((app_mods + 1))
    done <<EOF
$(printf '%s\n' $ENTERPRISE_MODULES)
EOF
  fi
  if ebin="$(release_ebin_dir "$wt")"; then
    for m in $ENTERPRISE_MODULES; do
      [ -f "$ebin/$m.beam" ] && rel=$((rel + 1))
    done
  fi
  {
    printf 'enterprise_beams_in_ebin: %s\n' "$count"
    printf 'enterprise_modules_in_app: %s\n' "$app_mods"
    printf 'enterprise_beams_in_release: %s\n' "$rel"
    printf 'route_probe: %s\n' "$(route_probe "$wt")"
  } >> "$out"
}

# ---------------------------------------------------------------------------
# 主流程
# ---------------------------------------------------------------------------

log "=== enterprise_business feature 三档矩阵（disposable worktree） ==="
log "repo            : $ROOT"
log "base commit     : $(git -C "$ROOT" rev-parse HEAD)"
log "manifest        : $MANIFEST"
log "manifest sha256 : $(file_sha256 "$MANIFEST")"
log "evidence dir    : $EVIDENCE_DIR"

ENTERPRISE_MODULES="$(enterprise_modules)"
ENTERPRISE_MODULE_COUNT=0
for _m in $ENTERPRISE_MODULES; do ENTERPRISE_MODULE_COUNT=$((ENTERPRISE_MODULE_COUNT + 1)); done
log "enterprise 模块数: $ENTERPRISE_MODULE_COUNT"
write_preset_manifests

ROOT_TREE_HASH_BEFORE="$(tree_hash "$ROOT")"
ROOT_STATUS_BEFORE="$(git -C "$ROOT" status --porcelain -uall | sort)"
ROOT_MANIFEST_SHA_BEFORE="$(file_sha256 "$MANIFEST")"
ROOT_DEPS_STATE_BEFORE="$(deps_state "$ROOT")"
log "调用方 tracked tree hash（前）: $ROOT_TREE_HASH_BEFORE"
log "调用方 deps 状态（前）      : $ROOT_DEPS_STATE_BEFORE"

should_run() {
  [ "$PRESET_FILTER" = all ] || [ "$PRESET_FILTER" = "$1" ]
}

# --- 档 1：neither（同时承载 A04 往返）------------------------------------
if should_run neither; then
  log ""
  log "---- 档 1/3: neither ----"
  WT_NEITHER="$(new_worktree neither)"
  generate "$WT_NEITHER" neither
  WT_NEITHER_HASH0="$(tree_hash "$WT_NEITHER")"
  if build_release "$WT_NEITHER" neither; then
    assert_all_layers "$WT_NEITHER" unselected neither
  else
    fail "neither：make rel 失败"
  fi
  cp "$TMPBASE/build-neither.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  capture_artifacts "$WT_NEITHER" neither neither
  write_preset_evidence neither "既不选 enterprise_business 也不选 customer_service" \
    "$(python3 -c 'import json,sys;print(" ".join(json.load(open(sys.argv[1]))["selected_features"]))' "$TMPBASE/manifests/neither.json")"

  log ""
  log "---- A04 往返：neither → enterprise-only → neither（同一副本，不删 ebin）----"
  generate "$WT_NEITHER" enterprise-only
  if build_release "$WT_NEITHER" roundtrip-enterprise-only; then
    assert_all_layers "$WT_NEITHER" selected "往返/enterprise-only"
  else
    fail "往返：neither→enterprise-only 的 make rel 失败"
  fi
  capture_artifacts "$WT_NEITHER" roundtrip-enterprise-only enterprise-only
  generate "$WT_NEITHER" neither
  if build_release "$WT_NEITHER" roundtrip-neither; then
    assert_all_layers "$WT_NEITHER" unselected "往返/回到 neither"
  else
    fail "往返：enterprise-only→neither 的 make rel 失败"
  fi
  capture_artifacts "$WT_NEITHER" roundtrip-neither neither
  WT_NEITHER_HASH1="$(tree_hash "$WT_NEITHER")"
  if [ "$WT_NEITHER_HASH1" = "$WT_NEITHER_HASH0" ]; then
    ok "A04 tracked tree hash 复原：${WT_NEITHER_HASH0}（往返前后同一副本同值）"
  else
    fail "A04 tracked tree hash 未复原：前 $WT_NEITHER_HASH0 / 后 $WT_NEITHER_HASH1"
  fi
  cp "$TMPBASE/build-roundtrip-enterprise-only.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  cp "$TMPBASE/build-roundtrip-neither.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  if [ -n "$EVIDENCE_DIR" ]; then
    mkdir -p "$EVIDENCE_DIR"
    python3 - "$EVIDENCE_DIR/roundtrip-a04-hashes.json" "$WT_NEITHER_HASH0" "$WT_NEITHER_HASH1" \
      "$ROOT_TREE_HASH_BEFORE" "$FAILED" <<'PY'
import json
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
path.write_text(json.dumps({
    "preset": "roundtrip-a04",
    "description": ("A04：同一 disposable worktree 内 neither -> enterprise-only -> neither，"
                    "不删 ebin；往返后无陈旧 beam 且 tracked tree hash 复原"),
    "roundtrip_tree_hash_before": sys.argv[2],
    "roundtrip_tree_hash_after": sys.argv[3],
    "roundtrip_tree_hash_restored": sys.argv[2] == sys.argv[3],
    "note": "与 roundtrip-a04.json（选中的 feature 集合）分开存，避免被覆盖",
    "caller_tree_hash_before": sys.argv[4],
    "assertions_failed_so_far": int(sys.argv[5]),
}, indent=2, sort_keys=True) + "\n")
PY
  fi
  write_preset_evidence roundtrip-a04 \
    "A04：same-worktree neither -> enterprise-only -> neither，无陈旧 beam 且 tracked tree hash 复原" \
    "$(python3 -c 'import json,sys;print(" ".join(json.load(open(sys.argv[1]))["selected_features"]))' "$TMPBASE/manifests/neither.json")"
fi

# --- 档 2：enterprise-only（干净副本，A01 新鲜构建证据）-------------------
if should_run enterprise-only; then
  log ""
  log "---- 档 2/3: enterprise-only（干净副本）----"
  WT_ENTERPRISE="$(new_worktree enterprise-only)"
  generate "$WT_ENTERPRISE" enterprise-only
  if build_release "$WT_ENTERPRISE" enterprise-only; then
    assert_all_layers "$WT_ENTERPRISE" selected enterprise-only
    probe_assertion_sensitivity "$WT_ENTERPRISE"
  else
    fail "enterprise-only：make rel 失败"
  fi
  cp "$TMPBASE/build-enterprise-only.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  capture_artifacts "$WT_ENTERPRISE" enterprise-only enterprise-only
  write_preset_evidence enterprise-only "只选 enterprise_business" \
    "$(python3 -c 'import json,sys;print(" ".join(json.load(open(sys.argv[1]))["selected_features"]))' "$TMPBASE/manifests/enterprise-only.json")"
fi

# --- 档 3：enterprise-customer-service（含 A03 负例）---------------------
if should_run enterprise-customer-service; then
  log ""
  log "---- 档 3/3: enterprise-customer-service ----"
  WT_CS="$(new_worktree enterprise-customer-service)"
  generate "$WT_CS" enterprise-customer-service
  generate_check "$WT_CS" "$TMPBASE/manifests/enterprise-customer-service.json" &&
    ok "A03 正例对照：enterprise_business + customer_service 的 manifest 生成并 --check 通过" ||
    fail "A03 正例对照：叠加档生成/--check 失败"

  # A03 负例：customer_service 在、enterprise_business 不在 ⇒ **生成即失败**。
  python3 - "$TMPBASE/manifests/customer-service-only.json" "$TMPBASE/manifests/enterprise-customer-service.json" <<'PY'
import json
import pathlib
import sys

target = pathlib.Path(sys.argv[1])
source = json.loads(pathlib.Path(sys.argv[2]).read_text())
source["profile"] = "customer-service-without-enterprise"
source["selected_features"] = [
    f for f in source["selected_features"] if f != "enterprise_business"
]
target.write_text(json.dumps(source, indent=2) + "\n")
PY
  set +e
  ( cd "$WT_CS" && python3 "$WT_CS/scripts/generate_product_features.py" \
      --manifest "$TMPBASE/manifests/customer-service-only.json" ) \
    > "$TMPBASE/a03-negative.log" 2>&1
  A03_STATUS=$?
  set -e
  if [ "$A03_STATUS" -ne 0 ] &&
    grep -q 'missing dependency for customer_service: enterprise_business' "$TMPBASE/a03-negative.log"; then
    ok "A03 负例：customer_service 无 enterprise_business ⇒ 生成 exit=$A03_STATUS 且点名缺失依赖"
  else
    fail "A03 负例：期望生成失败并点名 missing dependency，实得 exit=${A03_STATUS}：$(head -2 "$TMPBASE/a03-negative.log" | tr '\n' ' ')"
  fi
  # 还原证明：失败的生成不得留下半写产物；同一棵树对叠加档 manifest 仍 --check 通过。
  if generate_check "$WT_CS" "$TMPBASE/manifests/enterprise-customer-service.json" >/dev/null 2>&1; then
    ok "A03 还原证明：负例失败后同一棵树对叠加档 manifest 仍 --check 通过（负例零写入）"
  else
    fail "A03 还原证明：负例后叠加档 --check 不再通过（负例写坏了产物）"
  fi

  if build_release "$WT_CS" enterprise-customer-service; then
    assert_all_layers "$WT_CS" selected enterprise-customer-service
    if grep -qF -- '-define(IMBOY_FEATURE_CUSTOMER_SERVICE, true).' \
      "$WT_CS/include/generated/imboy_product_features.hrl"; then
      ok "叠加档宏：IMBOY_FEATURE_CUSTOMER_SERVICE 随选中出现"
    else
      fail "叠加档宏：缺 -define(IMBOY_FEATURE_CUSTOMER_SERVICE, true)"
    fi
  else
    fail "enterprise-customer-service：make rel 失败"
  fi
  cp "$TMPBASE/build-enterprise-customer-service.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  cp "$TMPBASE/a03-negative.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  capture_artifacts "$WT_CS" enterprise-customer-service enterprise-customer-service
  write_preset_evidence enterprise-customer-service \
    "enterprise_business + customer_service（依赖边 customer_service -> enterprise_business）" \
    "$(python3 -c 'import json,sys;print(" ".join(json.load(open(sys.argv[1]))["selected_features"]))' "$TMPBASE/manifests/enterprise-customer-service.json")"
fi

# --- 调用方工作树未被切换（A04 的另一半）--------------------------------
log ""
log "---- 调用方工作树未改（不得切换共享 manifest）----"
ROOT_TREE_HASH_AFTER="$(tree_hash "$ROOT")"
ROOT_STATUS_AFTER="$(git -C "$ROOT" status --porcelain -uall | sort)"
ROOT_MANIFEST_SHA_AFTER="$(file_sha256 "$MANIFEST")"
if [ "$ROOT_TREE_HASH_AFTER" = "$ROOT_TREE_HASH_BEFORE" ]; then
  ok "调用方 tracked tree hash 复原：$ROOT_TREE_HASH_AFTER"
else
  fail "调用方 tracked tree hash 变了：$ROOT_TREE_HASH_BEFORE -> $ROOT_TREE_HASH_AFTER"
fi
if [ "$ROOT_STATUS_AFTER" = "$ROOT_STATUS_BEFORE" ]; then
  ok "调用方 git status --porcelain -uall 逐字不变（未新增/未删除任何路径）"
else
  fail "调用方 git status 变了"
fi
if [ "$ROOT_MANIFEST_SHA_AFTER" = "$ROOT_MANIFEST_SHA_BEFORE" ]; then
  ok "调用方 config/product-feature-manifest.json 字节未变：$ROOT_MANIFEST_SHA_AFTER"
else
  fail "调用方 manifest 被改写：$ROOT_MANIFEST_SHA_BEFORE -> $ROOT_MANIFEST_SHA_AFTER"
fi
if [ "$(deps_state "$ROOT")" = "$ROOT_DEPS_STATE_BEFORE" ]; then
  ok "调用方 deps/ 状态逐字未变（矩阵未写共享依赖目录）"
else
  fail "调用方 deps/ 被写：$ROOT_DEPS_STATE_BEFORE -> $(deps_state "$ROOT")"
fi
# 一次性工作树残留=0（cleanup 会删，这里先记账）
if git -C "$ROOT" worktree list --porcelain | grep -q "^worktree $TMPBASE"; then
  ok "一次性工作树：$(grep -c . "$WTLIST") 个（退出时由 cleanup 注销）"
fi

log ""
if [ "$FAILED" -eq 0 ]; then
  log "=== enterprise_business feature 三档矩阵 PASS（断言 0 红）==="
  exit 0
fi
log "=== enterprise_business feature 三档矩阵 FAIL（$FAILED 条断言红）===" >&2
exit 1
