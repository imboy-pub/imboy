#!/usr/bin/env bash
# ============================================================================
# customer_service feature 三档矩阵（disposable worktree 版）—— CS-02
#
# 目的（plan.snapshot §8 CS-02 / CS-02-A03 + 裁剪矩阵）：
#   验证 customer_service 已接入**现成**的 feature 机制（manifest → 编译期宏 →
#   erlang.mk ERLC_EXCLUDE 物理裁剪 → imboy_router 的 -ifdef 路由段剔除 →
#   .app modules → relx release），并在**一次性工作树**上跑两档选择矩阵：
#
#     1. neither           既不选 enterprise_business 也不选 customer_service
#     2. customer-service  enterprise_business + customer_service（依赖前者）
#
#   逐档断言五层资产（对象 = customer_service 的全部模块（运行期实测 36，
#   含 CSB-03 的 cs_widget_handler / BE-W01 的 cs_widget_frame_handler 与
#   CSB-02R 的 cs_widget_env / cs_identity_assertion）+
#   35 条路由 = 租户 17 + widget 9 + 平台 9）：
#     A-selected    宏 / 路由 / beam / .app modules / release 都在
#     A-unselected  宏 / 路由 / beam / .app modules / release 都不在
#     A03-负例      customer_service 无 enterprise_business ⇒ 生成即失败
#     A-往返        neither → customer-service → neither 无陈旧 beam 且
#                   tracked tree hash 复原（同一副本，不删 ebin）
#
# 为什么必须一次性工作树（**不得切换共享 manifest**）：与
# scripts/enterprise_business_feature_matrix.sh 同理——生成器产物是共享树里的
# tracked 文件；生成/编译/打包全部发生在副本里，调用方 tracked tree hash 在
# 开头/结尾各算一次，不等即 FAIL。
#
# 用法：
#   bash scripts/customer_service_feature_matrix.sh                       # 两档全跑
#   bash scripts/customer_service_feature_matrix.sh customer-service      # 只跑一档
#   CS_FEATURE_MATRIX_EVIDENCE_DIR=<dir> bash scripts/customer_service_feature_matrix.sh
#
# 退出码：0 = 全部断言通过；1 = 有断言红（逐条 FAIL 行点名被测能力）。
#
# 已知环境性质（同 enterprise 矩阵）：erlang.mk 按 mtime 判新鲜，副本里 overlay
# 进来的 .erl 与生成物必须 touch（只改 mtime，只作用于副本）。
# ============================================================================
set -u

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PRESET_FILTER="${1:-all}"
PY_GENERATOR="$ROOT/scripts/generate_product_features.py"
MANIFEST="$ROOT/config/product-feature-manifest.json"

PRESET_NAMES="neither customer-service"

TMPBASE="$(mktemp -d "${TMPDIR:-/tmp}/imboy-cs-feature-matrix.XXXXXX")"
EVIDENCE_DIR="${CS_FEATURE_MATRIX_EVIDENCE_DIR:-$TMPBASE/evidence}"
WTLIST="$TMPBASE/worktrees.txt"
OVERLAY_LIST="$TMPBASE/overlay-list.txt"
: > "$WTLIST"
FAILED=0

case "$PRESET_FILTER" in
  all | neither | customer-service) ;;
  *)
    echo "用法: $0 [all|neither|customer-service]" >&2
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

# customer_service 的模块清单：**从被测源码目录独立枚举**（-module 属性），
# 不读生成器的 FEATURE_BACKEND_MODULES —— 断言必须独立于被断言对象（同
# enterprise 矩阵的纪律）。「生成器映射 == 目录清单」由 cs_route_contract_tests
# 的 a03_generator_mapping_covers_feature_directory 机械核对。
cs_modules() {
  find "$ROOT/src/features/customer_service" -name '*.erl' -exec \
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

deps_state() {
  find "$1/deps" -type f -exec stat -f '%N %m %z' {} + 2>/dev/null | sort | shasum -a 256 | cut -d' ' -f1
}

# ---------------------------------------------------------------------------
# 一次性工作树
# ---------------------------------------------------------------------------

new_worktree() {   # $1 = 名字；stdout = 副本路径
  local name="$1" wt
  # 目录名必须是 `imboy`（relx 经 code:lib_dir(imboy) 解析本应用；同 enterprise 矩阵）。
  wt="$TMPBASE/wt-$name/imboy"
  mkdir -p "$(dirname "$wt")"
  git -C "$ROOT" worktree add --detach "$wt" "$(git -C "$ROOT" rev-parse HEAD)" >/dev/null 2>&1
  printf '%s\n' "$wt" >> "$WTLIST"
  copy_deps "$wt"
  ln -s . "$wt/imboy"
  if [ -f "$ROOT/config/sys.runtime.config" ]; then
    cp "$ROOT/config/sys.runtime.config" "$wt/config/sys.runtime.config"
  fi
  overlay_working_tree "$wt"
  printf '%s\n' "$wt"
}

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

# 两档 manifest：Base = 调用方 manifest 去掉 enterprise_business /
# customer_service（不改调用方 manifest 一个字节）。
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
    "customer-service": ["enterprise_business", "customer_service"],
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

# 生成器必须用**副本自己的**脚本调用（产物写到「脚本所在仓 + 其父目录」）。
generate() {   # $1 = 副本, $2 = preset 名
  local wt="$1" name="$2"
  local manifest="$TMPBASE/manifests/$name.json"
  ( cd "$wt" && python3 "$wt/scripts/generate_product_features.py" --manifest "$manifest" ) || return 1
  if ! ( cd "$wt" && python3 "$wt/scripts/generate_product_features.py" \
      --check --manifest "$manifest" ) >/dev/null 2>&1; then
    fail "生成产物与 preset 不一致：副本 $wt 的 include/generated 未按 preset 刷新"
    return 1
  fi
  refresh_mtimes "$wt"
}

refresh_mtimes() {
  local wt="$1"
  find "$wt/include/generated" -type f -exec touch {} +
  find "$wt/src/features/customer_service" -name '*.erl' -exec touch {} +
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

# 运行期路由探针：直接读 imboy_router:get_routes/0；只数客服两面。
route_probe() {   # $1 = 副本；stdout = "tenant=<n> platform=<n>"
  local wt="$1" pa="" d
  for d in "$wt"/deps/*/ebin; do
    [ -d "$d" ] && pa="$pa -pa $d"
  done
  ( cd "$TMPBASE" && ERL_CRASH_DUMP="$TMPBASE/erl_crash.dump" \
      erl -noshell -pa "$wt/ebin" $pa -eval '
        Routes = lists:append([Rs || {_H, Rs} <- imboy_router:get_routes()]),
        CS = [P || {P, H, _} <- Routes,
                   (H =:= cs_tenant_handler orelse H =:= cs_widget_handler orelse
                    H =:= cs_widget_frame_handler orelse
                    H =:= cs_platform_handler),
                   is_list(P)],
        T = [P || {P, H, _} <- Routes, H =:= cs_tenant_handler, is_list(P)],
        W = [P || {P, H, _} <- Routes,
                  (H =:= cs_widget_handler orelse H =:= cs_widget_frame_handler),
                  is_list(P)],
        A = [P || {P, H, _} <- Routes, H =:= cs_platform_handler, is_list(P)],
        io:format("ROUTE_PROBE tenant=~p widget=~p platform=~p total=~p~n",
                  [length(T), length(W), length(A), length(CS)]),
        halt().' 2>/dev/null ) | tr -d '\r' | grep -o 'tenant=[0-9]* widget=[0-9]* platform=[0-9]* total=[0-9]*' | tail -1
}

# ---------------------------------------------------------------------------
# 五层断言
# ---------------------------------------------------------------------------

assert_layer_macro() {
  local wt="$1" expect="$2" hrl="$1/include/generated/imboy_product_features.hrl"
  if [ "$expect" = selected ]; then
    if grep -qF -- '-define(IMBOY_FEATURE_CUSTOMER_SERVICE, true).' "$hrl"; then
      ok "宏：include/generated/imboy_product_features.hrl 定义 IMBOY_FEATURE_CUSTOMER_SERVICE"
    else
      fail "宏：selected 却缺 -define(IMBOY_FEATURE_CUSTOMER_SERVICE, true)"
    fi
  else
    if grep -q 'customer_service' "$hrl"; then
      fail "宏：unselected 却仍在 $(basename "$hrl") 出现 customer_service（第 $(grep -n 'customer_service' "$hrl" | head -1 | cut -d: -f1) 行）"
    else
      ok "宏：unselected 时 hrl 无 customer_service（宏 / compiled_features 双无）"
    fi
  fi
}

assert_layer_erlc_exclude() {
  local wt="$1" expect="$2" mk="$1/include/generated/imboy_product_features_erlc.mk" m hits=0
  for m in $CS_MODULES; do
    case " $(sed -n 's/^IMBOY_FEATURE_ERLC_EXCLUDE *:=//p' "$mk") " in
      *" $m "*) hits=$((hits + 1)) ;;
    esac
  done
  if [ "$expect" = selected ]; then
    if [ "$hits" -eq 0 ]; then
      ok "erlc.mk：selected 时 ERLC_EXCLUDE 不含任何客服模块（$CS_MODULE_COUNT 个全在场）"
    else
      fail "erlc.mk：selected 却有 $hits 个客服模块被 ERLC_EXCLUDE 排除"
    fi
  else
    if [ "$hits" -eq "$CS_MODULE_COUNT" ]; then
      ok "erlc.mk：unselected 时 $CS_MODULE_COUNT 个客服模块全部进 ERLC_EXCLUDE"
    else
      fail "erlc.mk：unselected 只排除 $hits/$CS_MODULE_COUNT 个客服模块（漏排即残留编译）"
    fi
  fi
}

assert_layer_router_strings() {
  local wt="$1" expect="$2" beam="$1/ebin/imboy_router.beam" n
  n="$(grep -c -a -- '/api/v1/cs/\|/api/adm/customer-service/' "$beam" 2>/dev/null || true)"
  if [ "$expect" = selected ]; then
    if [ "$n" -gt 0 ] 2>/dev/null; then
      ok "路由：imboy_router.beam 含客服路径字符串（物理在场）"
    else
      fail "路由：selected 却 imboy_router.beam 里没有客服路径字符串"
    fi
  else
    if [ "$n" -eq 0 ] 2>/dev/null; then
      ok "路由：unselected 时 imboy_router.beam 零客服路径字符串（编译期已剔除）"
    else
      fail "路由：unselected 却 imboy_router.beam 残留客服路径字符串（匹配行 ${n}）"
    fi
  fi
}

assert_layer_routes() {
  local wt="$1" expect="$2" probe tenant widget platform
  probe="$(route_probe "$wt")"
  if [ -z "$probe" ]; then
    fail "路由表：route_probe 无输出（erl 探针失败）"
    return
  fi
  tenant="${probe#tenant=}"
  tenant="${tenant%% *}"
  widget="${probe#*widget=}"
  widget="${widget%% *}"
  platform="${probe##*platform=}"
  platform="${platform%% *}"
  if [ "$expect" = selected ]; then
    # CSB-02R：租户面 17（坐席会话详情 + seats/sessions 工作台 + queue 双主体）
    # + widget 接入面 9（8 + BE-W01 frame）+ 平台面 9（含
    # widget-installations 列表/创建/revoke）。
    if [ "$tenant" = 17 ] && [ "$widget" = 9 ] && [ "$platform" = 9 ]; then
      ok "路由表：get_routes/0 客服路由租户 17 + widget 9 + 平台 9"
    else
      fail "路由表：selected 期望租户 17/widget 9/平台 9，实得 $probe"
    fi
  else
    if [ "$tenant" = 0 ] && [ "$widget" = 0 ] && [ "$platform" = 0 ]; then
      ok "路由表：get_routes/0 零客服路由（${probe}）"
    else
      fail "路由表：unselected 期望 0/0/0，实得 $probe"
    fi
  fi
}

assert_layer_beams() {
  local wt="$1" expect="$2" m present=0
  for m in $CS_MODULES; do
    [ -f "$wt/ebin/$m.beam" ] && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$CS_MODULE_COUNT" ]; then
      ok "beam：ebin 中 $present/$CS_MODULE_COUNT 个客服 beam 全在"
    else
      fail "beam：selected 只有 $present/$CS_MODULE_COUNT 个客服 beam"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "beam：ebin 中 0/$CS_MODULE_COUNT 个客服 beam（含陈旧 beam）"
    else
      fail "beam：unselected 仍残留 $present 个客服 beam（陈旧 beam 未剪）"
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
  for m in $CS_MODULES; do
    grep -qx "$m" "$mods" && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$CS_MODULE_COUNT" ]; then
      ok "app：ebin/imboy.app {modules,_} 含 $present/$CS_MODULE_COUNT 个客服模块"
    else
      fail "app：selected 只有 $present/$CS_MODULE_COUNT 个客服模块进 .app"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "app：ebin/imboy.app {modules,_} 零客服模块"
    else
      fail "app：unselected 仍有 $present 个客服模块在 .app modules"
    fi
  fi
}

assert_layer_release() {
  local wt="$1" expect="$2" ebin m present=0
  if ! ebin="$(release_ebin_dir "$wt")"; then
    fail "release：未找到 _rel/imboy/lib/imboy-*/ebin"
    return
  fi
  for m in $CS_MODULES; do
    [ -f "$ebin/$m.beam" ] && present=$((present + 1))
  done
  if [ "$expect" = selected ]; then
    if [ "$present" -eq "$CS_MODULE_COUNT" ]; then
      ok "release：$(basename "$(dirname "$ebin")")/ebin 含 $present/$CS_MODULE_COUNT 个客服 beam"
    else
      fail "release：selected 只有 $present/$CS_MODULE_COUNT 个客服 beam 进 release"
    fi
  else
    if [ "$present" -eq 0 ]; then
      ok "release：release 包内 0/$CS_MODULE_COUNT 个客服 beam"
    else
      fail "release：unselected 仍有 $present 个客服 beam 进 release"
    fi
  fi
}

# 断言灵敏度对照：把 1 个 selected 层的 beam 暂时移走，存在性断言必须立刻红。
probe_assertion_sensitivity() {   # $1 = 已构建的 selected 副本
  local wt="$1" beam="$1/ebin/cs_tenant_handler.beam" out
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
    fail "灵敏度对照：移走 selected beam 后断言仍绿 ⇒ beam 断言恒真，A-selected 不可信"
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

# ---------------------------------------------------------------------------
# 主流程
# ---------------------------------------------------------------------------

log "=== customer_service feature 两档矩阵（disposable worktree） ==="
log "repo            : $ROOT"
log "base commit     : $(git -C "$ROOT" rev-parse HEAD)"
log "manifest        : $MANIFEST"
log "manifest sha256 : $(file_sha256 "$MANIFEST")"
log "evidence dir    : $EVIDENCE_DIR"

CS_MODULES="$(cs_modules)"
CS_MODULE_COUNT=0
for _m in $CS_MODULES; do CS_MODULE_COUNT=$((CS_MODULE_COUNT + 1)); done
log "customer_service 模块数: $CS_MODULE_COUNT"
write_preset_manifests

ROOT_TREE_HASH_BEFORE="$(tree_hash "$ROOT")"
ROOT_MANIFEST_SHA_BEFORE="$(file_sha256 "$MANIFEST")"
ROOT_DEPS_STATE_BEFORE="$(deps_state "$ROOT")"
log "调用方 tracked tree hash（前）: $ROOT_TREE_HASH_BEFORE"
log "调用方 deps 状态（前）      : $ROOT_DEPS_STATE_BEFORE"

should_run() {
  [ "$PRESET_FILTER" = all ] || [ "$PRESET_FILTER" = "$1" ]
}

# --- 档 1：neither（同时承载往返）------------------------------------------
if should_run neither; then
  log ""
  log "---- 档 1/2: neither ----"
  WT_NEITHER="$(new_worktree neither)"
  generate "$WT_NEITHER" neither
  WT_NEITHER_HASH0="$(tree_hash "$WT_NEITHER")"
  if build_release "$WT_NEITHER" neither; then
    assert_all_layers "$WT_NEITHER" unselected neither
  else
    fail "neither：make rel 失败"
  fi
  cp "$TMPBASE/build-neither.log" "$EVIDENCE_DIR/" 2>/dev/null || true

  log ""
  log "---- 往返：neither → customer-service → neither（同一副本，不删 ebin）----"
  generate "$WT_NEITHER" customer-service
  if build_release "$WT_NEITHER" roundtrip-cs; then
    assert_all_layers "$WT_NEITHER" selected "往返/customer-service"
  else
    fail "往返：neither→customer-service 的 make rel 失败"
  fi
  generate "$WT_NEITHER" neither
  if build_release "$WT_NEITHER" roundtrip-neither; then
    assert_all_layers "$WT_NEITHER" unselected "往返/回到 neither"
  else
    fail "往返：customer-service→neither 的 make rel 失败"
  fi
  WT_NEITHER_HASH1="$(tree_hash "$WT_NEITHER")"
  if [ "$WT_NEITHER_HASH1" = "$WT_NEITHER_HASH0" ]; then
    ok "往返 tracked tree hash 复原：${WT_NEITHER_HASH0}"
  else
    fail "往返 tracked tree hash 未复原：前 $WT_NEITHER_HASH0 / 后 $WT_NEITHER_HASH1"
  fi
  cp "$TMPBASE/build-roundtrip-cs.log" "$EVIDENCE_DIR/" 2>/dev/null || true
  cp "$TMPBASE/build-roundtrip-neither.log" "$EVIDENCE_DIR/" 2>/dev/null || true
fi

# --- 档 2：customer-service（干净副本 + A03 负例）--------------------------
if should_run customer-service; then
  log ""
  log "---- 档 2/2: customer-service ----"
  WT_CS="$(new_worktree customer-service)"
  generate "$WT_CS" customer-service
  if build_release "$WT_CS" customer-service; then
    assert_all_layers "$WT_CS" selected customer-service
    probe_assertion_sensitivity "$WT_CS"
  else
    fail "customer-service：make rel 失败"
  fi
  cp "$TMPBASE/build-customer-service.log" "$EVIDENCE_DIR/" 2>/dev/null || true

  # A03 负例：customer_service 在、enterprise_business 不在 ⇒ **生成即失败**
  #（依赖边来自 imboy_policy_catalog:dependencies/1）。
  python3 - "$TMPBASE/manifests/cs-only.json" "$TMPBASE/manifests/customer-service.json" <<'PY'
import json
import pathlib
import sys

target = pathlib.Path(sys.argv[1])
source = json.loads(pathlib.Path(sys.argv[2]).read_text())
source["profile"] = "cs-without-enterprise"
source["selected_features"] = [
    f for f in source["selected_features"] if f != "enterprise_business"
]
target.write_text(json.dumps(source, indent=2) + "\n")
PY
  set +e
  ( cd "$WT_CS" && python3 "$WT_CS/scripts/generate_product_features.py" \
      --manifest "$TMPBASE/manifests/cs-only.json" ) \
    > "$TMPBASE/a03-negative.log" 2>&1
  GEN_RC=$?
  set -e
  if [ "$GEN_RC" -ne 0 ] &&
    grep -q "missing dependency for customer_service: enterprise_business" \
      "$TMPBASE/a03-negative.log"; then
    ok "A03 负例：customer_service 无 enterprise_business ⇒ 生成即失败（依赖边生效）"
  else
    fail "A03 负例：生成器退出码 $GEN_RC（期望非零 + missing dependency 报错）"
  fi
  mkdir -p "$EVIDENCE_DIR"
  cp "$TMPBASE/a03-negative.log" "$EVIDENCE_DIR/" 2>/dev/null || true
fi

# ---------------------------------------------------------------------------
# 收尾校验：调用方工作树零污染
# ---------------------------------------------------------------------------
ROOT_TREE_HASH_AFTER="$(tree_hash "$ROOT")"
ROOT_MANIFEST_SHA_AFTER="$(file_sha256 "$MANIFEST")"
ROOT_DEPS_STATE_AFTER="$(deps_state "$ROOT")"
log ""
log "调用方 tracked tree hash（后）: $ROOT_TREE_HASH_AFTER"
if [ "$ROOT_TREE_HASH_BEFORE" = "$ROOT_TREE_HASH_AFTER" ]; then
  ok "调用方 tracked tree hash 复原（矩阵对共享树零写入）"
else
  fail "调用方 tracked tree hash 变化：矩阵泄漏了对共享树的写入"
fi
if [ "$ROOT_MANIFEST_SHA_BEFORE" = "$ROOT_MANIFEST_SHA_AFTER" ]; then
  ok "共享 manifest 未被改动（sha256 一致）"
else
  fail "共享 manifest 被改动：$ROOT_MANIFEST_SHA_BEFORE -> $ROOT_MANIFEST_SHA_AFTER"
fi
if [ "$ROOT_DEPS_STATE_BEFORE" = "$ROOT_DEPS_STATE_AFTER" ]; then
  ok "调用方 deps 未被矩阵触碰"
else
  fail "调用方 deps 被矩阵改动"
fi

log ""
if [ "$FAILED" -eq 0 ]; then
  log "=== customer_service feature matrix: PASS（$CS_MODULE_COUNT 模块 / 35 路由，两档五层全绿） ==="
else
  log "=== customer_service feature matrix: FAIL（$FAILED 条断言红） ===" >&2
fi
exit $([ "$FAILED" -eq 0 ] && echo 0 || echo 1)
