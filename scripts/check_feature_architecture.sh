#!/usr/bin/env bash
# ============================================================================
# arch-check —— Feature Slice 架构铁律门禁
#
# 依据（规范性）：docs/architecture/feature-slice-rules.md  v1.4（9 条铁律）
# 决策来源：     docs/adr/0007-feature-slice-architecture.md
#
# 与 check_module_boundaries.sh 的分工（两者互补，均被 make security-gate 调用）：
#   check_module_boundaries.sh            —— 旧四层边界：handler → logic → ds → repo 单向依赖
#   check_feature_architecture.sh（本脚本）—— Feature Slice 九铁律：纵切单元结构、facade 分层、
#                                            domain 纯净、Org 贯穿、跨单元依赖方向、Product/Plugin 隔离
#
# 管辖范围（仅纵切单元，不动存量）：
#   src/features/**  src/products/**  src/plugins/**  +  对 src/lib/** 的反向检查
#   存量目录 src/api|logic|ds|repo|adm|mcp 不在管辖内（铁律「存量不迁移」）。
#
# 铁律 6 的越权负例与铁律 7 的并发 CAS 属"评审 + 测试契约"，静态不可判定，
# 本脚本仅做 advisory 提示（warning，不阻断）。
#
# 用法：
#   bash scripts/check_feature_architecture.sh                # 检查本仓
#   bash scripts/check_feature_architecture.sh --root DIR     # 检查指定根（自测用）
#   bash scripts/check_feature_architecture.sh --self-test    # 金丝雀自检（12 条必须全触发）
# ============================================================================
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if ! command -v rg >/dev/null 2>&1; then
  echo "error: rg is required for scripts/check_feature_architecture.sh" >&2
  exit 1
fi

TARGET_ROOT="$ROOT_DIR"
SELF_TEST=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --root)      TARGET_ROOT="$2"; shift 2 ;;
    --self-test) SELF_TEST=1; shift ;;
    -h|--help)   sed -n '2,26p' "$0"; exit 0 ;;
    *)           echo "unknown arg: $1" >&2; exit 2 ;;
  esac
done

VIOLATIONS=0
WARNINGS=0

violation() { echo "arch violation: $*" >&2; VIOLATIONS=1; }
warn()      { echo "arch warning:   $*" >&2; WARNINGS=$((WARNINGS + 1)); }

# ---------------------------------------------------------------------------
# 基础工具
# ---------------------------------------------------------------------------

# 去掉 Erlang 行注释后的代码（保留行号）
code_only() { perl -ne 's/%.*$//; print' "$1" 2>/dev/null; }

# 纵切单元清单：feature|name|relpath / product|name|relpath / plugin|域/impl|relpath
list_units() {
  local root="$1" d p
  for d in "$root"/src/features/*/ ; do
    [[ -d "$d" ]] || continue
    echo "feature|$(basename "$d")|${d#"$root"/}"
  done
  for d in "$root"/src/products/*/ ; do
    [[ -d "$d" ]] || continue
    echo "product|$(basename "$d")|${d#"$root"/}"
  done
  for p in "$root"/src/plugins/*/*/ ; do
    [[ -d "$p" ]] || continue
    echo "plugin|$(basename "$(dirname "$p")")/$(basename "$p")|${p#"$root"/}"
  done
}

# 模块 → 相对路径 索引
module_index() {
  local root="$1" f m
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    m="$(rg --no-filename -o -r '$1' '^-module\(([a-z][a-z0-9_]*)\)' "$f" 2>/dev/null | head -1 || true)"
    [[ -n "$m" ]] && printf '%s\t%s\n' "$m" "${f#"$root"/}"
  done < <(find "$root/src" -type f -name '*.erl' 2>/dev/null)
}

# 文件所属层（注意：Erlang 文件名是 cs_facade.erl 这类后缀形态，不用 */_facade.erl）
layer_of() {
  case "$1" in
    */domain/*)         echo domain ;;
    */application/*)    echo application ;;
    */interfaces/*)     echo interfaces ;;
    */infrastructure/*) echo infrastructure ;;
    src/plugins/*)      echo plugin ;;
    src/lib/*)          echo core ;;
    *_facade.erl)       echo facade ;;
    *_feature.erl)      echo feature_manifest ;;
    *_sup.erl)          echo sup ;;
    *)                  echo root ;;
  esac
}

# 单元归属
owner_of() {
  case "$1" in
    src/features/*) echo "feature:$(echo "$1" | cut -d/ -f3)" ;;
    src/products/*) echo "product:$(echo "$1" | cut -d/ -f3)" ;;
    src/plugins/*)  echo "plugin:$(echo "$1" | cut -d/ -f3)/$(echo "$1" | cut -d/ -f4)" ;;
    src/lib/*)      echo "core" ;;
    *)              echo "legacy" ;;
  esac
}

# 引用的模块名（Mod:fun( 与 -behaviour(Mod)）。rust regex 无 lookahead，故用 -r 分组提取。
refs_in() {
  { code_only "$1" | rg -o -N -r '$1' '\b([a-z][a-z0-9_]*):[a-z_][a-z0-9_]*\(' 2>/dev/null || true
    code_only "$1" | rg -o -N -r '$1' '^-behaviou?r\(([a-z][a-z0-9_]*)\)' 2>/dev/null || true
  } | sort -u
}

has_callback() { rg -q '^-callback' "$1" 2>/dev/null; }

# 已知 product_id 集合（排除平台自身 imboy：它是核心模块前缀，非"外部产品"）
product_ids() {
  local root="$1" f
  { [[ -f "$root/config/product-feature-manifest.json" ]] && printf '%s\n' "$root/config/product-feature-manifest.json"
    find "$root/config/product-feature-manifests" -maxdepth 1 -name '*.json' 2>/dev/null
  } | while IFS= read -r f; do
        rg --no-filename -o -r '$1' '"product_id"[[:space:]]*:[[:space:]]*"([a-z][a-z0-9_-]*)"' "$f" 2>/dev/null || true
      done | sort -u | grep -vx 'imboy' || true
}

plugin_impl_names() {
  local root="$1" p
  for p in "$root"/src/plugins/*/*/ ; do
    [[ -d "$p" ]] || continue
    basename "$p"
  done | sort -u
}

# 标识符级 substring 匹配（_ 视为分隔符，故用非字母数字边界）
substr_hits() {  # $1=file  $2=needle  → 输出 "行号:行内容"
  code_only "$1" | rg -n "([^a-zA-Z0-9]|^)${2}([^a-zA-Z0-9]|\$)" 2>/dev/null || true
}

# ---------------------------------------------------------------------------
# 铁律 1/2：结构白名单
# ---------------------------------------------------------------------------
check_structure() {
  local root="$1" kind rel base d sub f
  while IFS='|' read -r kind _ rel; do
    [[ -n "$kind" ]] || continue
    base="${rel%/}"
    for d in "$root/$base"/*/ ; do
      [[ -d "$d" ]] || continue
      sub="$(basename "$d")"
      case "$kind" in
        feature|product)
          case "$sub" in
            domain|application|interfaces|infrastructure) ;;
            *) violation "[铁律2] $base/ 出现白名单外的层目录：$sub/" ;;
          esac
          ;;
        plugin)
          case "$sub" in
            domain|application|interfaces|infrastructure)
              violation "[铁律9] 插件实现 $base/ 不应含业务层目录 $sub/（Plugin 只实现扩展点，无业务分层）" ;;
          esac
          ;;
      esac
    done
    if [[ "$kind" == "feature" || "$kind" == "product" ]]; then
      while IFS= read -r f; do
        [[ -n "$f" ]] || continue
        case "$(basename "$f")" in
          *_facade.erl|*_feature.erl|*_sup.erl|*_product.erl) ;;
          *) violation "[铁律2] $base/ 根部出现白名单外文件：$(basename "$f")" ;;
        esac
      done < <(find "$root/$base" -maxdepth 1 -type f -name '*.erl' 2>/dev/null)
    fi
  done < <(list_units "$root")
}

# ---------------------------------------------------------------------------
# 铁律 3/5：facade 只进 application；application 不得直连 repo
# ---------------------------------------------------------------------------
check_layering() {
  local root="$1" f rel ref layer
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    rel="${f#"$root"/}"
    layer="$(layer_of "$rel")"
    [[ "$layer" == facade || "$layer" == application ]] || continue
    while IFS= read -r ref; do
      [[ -n "$ref" ]] || continue
      case "$ref" in
        *_repo)
          if [[ "$layer" == facade ]]; then
            violation "[铁律3] ${rel}（facade）直达 ${ref}——应经 application/"
          else
            violation "[铁律5] ${rel}（application）直连 ${ref}——持久化应经扩展点(Port)"
          fi
          ;;
        *_ds)
          [[ "$layer" == facade ]] && violation "[铁律3] ${rel}（facade）直达 ${ref}——应经 application/" ;;
      esac
    done < <(refs_in "$f")
    if rg -q '\belib_pg(_sql)?:' "$f" 2>/dev/null; then
      if [[ "$layer" == facade ]]; then
        violation "[铁律3] ${rel}（facade）直接使用 elib_pg"
      else
        violation "[铁律5] ${rel}（application）直接使用 elib_pg——应经扩展点(Port)"
      fi
    fi
  done < <(find "$root/src/features" "$root/src/products" -type f -name '*.erl' 2>/dev/null)
}

# ---------------------------------------------------------------------------
# 铁律 4：Domain 纯净 + domain 测试零 mock
# ---------------------------------------------------------------------------
DOMAIN_BANNED='\b(cowboy|cowboy_req|epgsql|elib_pg|elib_pg_sql|imboy_syn|imboy_domain_event|httpc|inets):'

check_domain_purity() {
  local root="$1" f rel hit
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    rel="${f#"$root"/}"
    [[ "$rel" == test/* ]] && continue
    hit="$(code_only "$f" | rg -o -N "$DOMAIN_BANNED" 2>/dev/null | sed -n '1,3p' | tr '\n' ' ' || true)"
    if [[ -n "$hit" ]]; then
      violation "[铁律4] $rel 的 domain 依赖外部设施：$hit"
    fi
    if code_only "$f" | rg -q '\b(os:timestamp|erlang:timestamp|calendar:universal_time|rand:uniform|rand:seed)\(' 2>/dev/null; then
      warn "[铁律4] $rel 的 domain 疑似隐式时间/随机依赖（应由调用方传入）"
    fi
  done < <(find "$root/src/features" "$root/src/products" -path '*/domain/*' -name '*.erl' 2>/dev/null)

  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    if code_only "$f" | rg -q '\?WITH_MECKS|meck:' 2>/dev/null; then
      violation "[铁律4] $(basename "$f") 的 domain 测试使用了 meck——domain 单测必须零 mock"
    fi
  done < <(find "$root/test" -path '*domain*' -name '*_tests.erl' 2>/dev/null)
}

# ---------------------------------------------------------------------------
# 铁律 6（可静态判定部分）：repo 的 SQL 必须带 org_id 约束
# ---------------------------------------------------------------------------
check_repo_org_scope() {
  local root="$1" f rel bad
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    [[ "$(basename "$f")" == *_repo.erl ]] || continue
    rel="${f#"$root"/}"
    [[ "$rel" == test/* ]] && continue
    bad="$(perl -0777 -ne '
      while (/<<((?:[^"]|"(?!>)|\n)*?)">>/gs) {
        my $sql = $1;
        next unless $sql =~ /\b(SELECT|UPDATE|DELETE|INSERT)\b/i;
        next if $sql =~ /\borg_id\b/i;
        my $s = $sql; $s =~ s/\s+/ /g; $s = substr($s, 0, 72);
        print "    ", $s, "\n";
      }' "$f" 2>/dev/null || true)"
    if [[ -n "$bad" ]]; then
      violation "[铁律6] $rel 存在未带 org_id 约束的 SQL："
      printf '%s\n' "$bad" >&2
    fi
  done < <(find "$root/src/features" "$root/src/products" -name '*_repo.erl' 2>/dev/null)
}

# ---------------------------------------------------------------------------
# 铁律 5/8/9：跨单元依赖方向 + 假 Plugin 检测
# ---------------------------------------------------------------------------
check_edges() {
  local root="$1"
  local idx; idx="$(mktemp)"
  module_index "$root" > "$idx"
  if [[ ! -s "$idx" ]]; then rm -f "$idx"; return 0; fi

  local f rel ref tp to tl co cname pname
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    rel="${f#"$root"/}"
    [[ "$rel" == test/* ]] && continue
    co="$(owner_of "$rel")"
    case "$co" in
      feature:*|product:*|plugin:*|core) ;;
      *) continue ;;
    esac
    while IFS= read -r ref; do
      [[ -n "$ref" ]] || continue
      tp="$(rg --no-filename -m1 "^${ref}	" "$idx" 2>/dev/null | cut -f2 || true)"
      [[ -n "$tp" ]] || continue
      to="$(owner_of "$tp")"
      tl="$(layer_of "$tp")"

      case "$co" in
        core)
          case "$to" in
            core|legacy) ;;
            *) violation "[铁律5] core 模块 $rel 反向依赖纵切单元 ${to}（${ref}）" ;;
          esac
          ;;
        feature:*)
          cname="${co#feature:}"
          case "$to" in
            core|legacy) ;;
            "feature:$cname") ;;
            feature:*)
              [[ "$tl" == facade ]] || violation "[铁律5] $rel 跨 Feature 引用非 facade 模块 ${ref}（${tp}）" ;;
            product:*)
              violation "[铁律8] Feature $cname 依赖 Product ${to}（${ref}）——Feature 不得知道具体产品" ;;
            plugin:*)
              violation "[铁律9] Feature $cname 依赖具体 Plugin ${to}（${ref}）——应只依赖扩展点" ;;
          esac
          ;;
        product:*)
          pname="${co#product:}"
          case "$to" in
            core|legacy) ;;
            "product:$pname") ;;
            product:*)
              violation "[铁律8] Product $pname 依赖另一 Product ${to}（${ref}）" ;;
            feature:*)
              [[ "$tl" == facade ]] || violation "[铁律5] $rel 跨单元引用非 facade 模块 ${ref}（${tp}）" ;;
            plugin:*)
              violation "[铁律9] Product $pname 依赖具体 Plugin ${to}（${ref}）——应经扩展点装配" ;;
          esac
          ;;
        plugin:*)
          case "$to" in
            core) ;;
            feature:*|product:*)
              if ! has_callback "$root/$tp"; then
                violation "[铁律9] 假 Plugin：$rel 引用 ${ref}（${tp}），该模块未声明 -callback，不是扩展点"
              elif [[ "$to" == feature:* ]]; then
                # 语法事实是"必要非充分"：Feature 扩展点还须在该单元 manifest 中登记为契约。
                # 按规范口径先作软告警，收敛后转硬门（见 feature-slice-rules §铁律9）。
                if ! rg -q --glob '*_feature.erl' "${ref}" "$root/src/features/${to#feature:}" 2>/dev/null; then
                  warn "[铁律9] 扩展点 ${ref} 未在 ${to#feature:} 的 manifest(*_feature.erl) 中登记 extension_points（软告警）"
                fi
              fi
              ;;
          esac
          ;;
      esac
    done < <(refs_in "$f")
  done < <(find "$root/src/features" "$root/src/products" "$root/src/plugins" "$root/src/lib" -name '*.erl' 2>/dev/null)
  rm -f "$idx"
}

# ---------------------------------------------------------------------------
# 铁律 8：Core/Feature 不得出现外部产品名
# ---------------------------------------------------------------------------
check_no_foreign_product() {
  local root="$1" pid f hits
  for pid in $(product_ids "$root"); do
    while IFS= read -r f; do
      [[ -n "$f" ]] || continue
      hits="$(substr_hits "$f" "$pid")"
      if [[ -n "$hits" ]]; then
        violation "[铁律8] $(basename "$f") 出现产品名 '$pid'（Core/Feature 不得依赖具体 Product）："
        printf '%s\n' "$hits" | sed -n '1,3p' | sed 's/^/    /' >&2
      fi
    done < <(find "$root/src/lib" "$root/src/features" -name '*.erl' 2>/dev/null)
  done
}

# ---------------------------------------------------------------------------
# 铁律 9：Core/Feature 不得出现具体 Plugin 名
# ---------------------------------------------------------------------------
check_no_plugin_names() {
  local root="$1" pn f hits
  for pn in $(plugin_impl_names "$root"); do
    while IFS= read -r f; do
      [[ -n "$f" ]] || continue
      hits="$(substr_hits "$f" "$pn")"
      if [[ -n "$hits" ]]; then
        violation "[铁律9] $(basename "$f") 出现具体 Plugin '$pn'（应只依赖扩展点 + Registry 装配）："
        printf '%s\n' "$hits" | sed -n '1,3p' | sed 's/^/    /' >&2
      fi
    done < <(find "$root/src/lib" "$root/src/features" -name '*.erl' 2>/dev/null)
  done
}

# ---------------------------------------------------------------------------
# advisory：铁律 7 并发 CAS 契约（静态不可判定，仅提示）
# ---------------------------------------------------------------------------
advisory_contracts() {
  local root="$1" f rel tdir
  while IFS= read -r f; do
    [[ -n "$f" ]] || continue
    rel="${f#"$root"/}"
    code_only "$f" | rg -q 'status[[:space:]]*=[[:space:]]*\$' 2>/dev/null || continue
    tdir="$root/${rel%/*}"; tdir="${tdir/\/src\//\/test\/}"
    if ! rg -q 'conflict|concurrent' "$tdir" 2>/dev/null; then
      warn "[铁律7] $rel 含状态推进，但同单元测试未见并发/冲突负例（CAS 契约需人工确认）"
    fi
  done < <(find "$root/src/features" "$root/src/products" -name '*_repo.erl' 2>/dev/null)
}

run_all_checks() {
  local root="$1"
  check_structure          "$root"
  check_layering           "$root"
  check_domain_purity      "$root"
  check_repo_org_scope     "$root"
  check_edges              "$root"
  check_no_foreign_product "$root"
  check_no_plugin_names    "$root"
  advisory_contracts       "$root"
  check_core_no_feature_refs "$root"
}

# FND-7（RULING-2026-09-15 §五）：Core -> Feature 禁止依赖的**真实覆盖** ——
# 不只扫 Feature 目录自身，还反向扫 Core（src/lib/**）：Core 模块里出现对
# Feature 模块（eb_*/cs_* 前缀）的远程调用即违规。此前门只看 features/
# products/plugins 内部铁律，Core 里塞一个 eb_* 调用是盲区。
check_core_no_feature_refs() {
  local root="$1" hits=0
  local f line callee
  while IFS= read -r f; do
    while IFS= read -r line; do
      callee="$(printf '%s' "$line" | sed -nE 's/.*[^a-zA-Z0-9_]([be]b_[a-z0-9_]+|cs_[a-z0-9_]+):[a-z_]+.*/\1/p' | head -1)"
      if [ -n "$callee" ]; then
        echo "  铁律[Core→Feature] ${f#$root/}: Core 模块引用 Feature 模块 $callee"
        hits=$((hits + 1))
      fi
    done < <(sed -e 's/%.*$//' "$f" | grep -nE '[be]b_[a-z0-9_]+:|cs_[a-z0-9_]+:' || true)
  done < <(find "$root/src/lib" -name '*.erl' 2>/dev/null || true)
  if [ "$hits" -gt 0 ]; then
    echo "feature architecture check failed：Core 存在 Feature 反向依赖（共 $hits 处）" >&2
    VIOLATIONS=$((VIOLATIONS + hits))
  fi
}

# ===========================================================================
# 金丝雀自检
# ===========================================================================
seed_fixture() {   # 构造"合规基线"：应零违规
  local r="$1"
  mkdir -p "$r/src/lib" "$r/src/features/cs/domain" "$r/src/features/cs/application" \
           "$r/src/features/cs/interfaces" "$r/src/features/cs/infrastructure" \
           "$r/src/products/moya/infrastructure" "$r/src/plugins/payment/stripe" \
           "$r/config/product-feature-manifests" "$r/test/features/cs/domain"
  printf '{"schema_version":1,"product_id":"imboy","profile":"x","selected_features":[]}\n' > "$r/config/product-feature-manifest.json"
  printf '{"schema_version":1,"product_id":"moya","profile":"y","selected_features":[]}\n' > "$r/config/product-feature-manifests/moya.json"

  printf -- '-module(elib_helper).\n-export([f/1]).\nf(X) -> X.\n'                 > "$r/src/lib/elib_helper.erl"
  printf -- '-module(cs_session_agg).\n-export([f/1]).\nf(X) -> X.\n'             > "$r/src/features/cs/domain/cs_session_agg.erl"
  printf -- '-module(cs_dispatch_strategy).\n-callback pick(term()) -> ok.\n'     > "$r/src/features/cs/domain/cs_dispatch_strategy.erl"
  printf -- '-module(cs_session_uc).\n-export([f/1]).\nf(X) -> cs_session_agg:f(X).\n' > "$r/src/features/cs/application/cs_session_uc.erl"
  printf -- '-module(cs_facade).\n-export([f/1]).\nf(X) -> cs_session_uc:f(X).\n'  > "$r/src/features/cs/cs_facade.erl"
  # $1 is an Erlang SQL placeholder in the fixture.
  # shellcheck disable=SC2016
  printf -- '-module(cs_seat_repo).\n-export([f/1]).\nf(O) -> elib_pg:query(<<"SELECT id FROM s WHERE org_id = $1">>, [O]).\n' > "$r/src/features/cs/infrastructure/cs_seat_repo.erl"
  printf -- '-module(moya_grades).\n-export([f/1]).\nf(X) -> X.\n'                > "$r/src/products/moya/infrastructure/moya_grades.erl"
}

run_self_test() {
  local tmp; tmp="$(mktemp -d)"
  local fails=0
  # shellcheck disable=SC2064
  trap "rm -rf '$tmp'" EXIT

  ok()   { echo "  ✓ $*"; }
  bad()  { echo "  ✗ $*"; fails=$((fails + 1)); }

  expect_rule() {   # $1=编号 $2=描述 $3=夹具根 $4=期望规则串
    local out; out="$(run_all_checks "$3" 2>&1 || true)"
    if printf '%s' "$out" | rg -q "$4"; then ok "金丝雀 $1 触发：$2"; else
      bad "金丝雀 $1 未触发：$2（期望匹配 /$4/）"
      printf '%s\n' "$out" | sed -n '1,5p' | sed 's/^/      /'
    fi
  }

  # 基线：零违规
  local a="$tmp/clean"; seed_fixture "$a"
  local abase; abase="$(run_all_checks "$a" 2>&1 || true)"
  if [[ -n "$abase" ]]; then
    bad "基线夹具应零违规，实得："; printf '%s\n' "$abase" | sed -n '1,8p' | sed 's/^/      /'
  else
    ok "基线夹具零违规"
  fi

  # ① facade 直达 repo
  local b="$tmp/facade"; seed_fixture "$b"
  printf -- '-module(cs_facade).\n-export([f/1]).\nf(X) -> cs_seat_repo:f(X).\n' > "$b/src/features/cs/cs_facade.erl"
  expect_rule "①" "facade 直达 repo" "$b" '铁律3'

  # ② application 直连 repo（铁律5）
  local c="$tmp/app"; seed_fixture "$c"
  printf -- '-module(cs_seat_uc).\n-export([f/1]).\nf(X) -> cs_seat_repo:f(X).\n' > "$c/src/features/cs/application/cs_seat_uc.erl"
  expect_rule "②" "application 直连 repo" "$c" '铁律5'

  # ③ core 出现外部产品名 moya（铁律8）
  local d="$tmp/coreprod"; seed_fixture "$d"
  printf -- '-module(elib_moya_bridge).\n-export([f/1]).\nf(X) -> X.\n' > "$d/src/lib/elib_moya_bridge.erl"
  expect_rule "③" "core 出现产品名 moya" "$d" '铁律8'

  # ④ feature 依赖具体 Plugin（铁律9）
  local e="$tmp/featplugin"; seed_fixture "$e"
  printf -- '-module(stripe).\n-export([charge/1]).\ncharge(X) -> X.\n' > "$e/src/plugins/payment/stripe/stripe.erl"
  printf -- '-module(cs_pay_uc).\n-export([f/1]).\nf(X) -> stripe:charge(X).\n' > "$e/src/features/cs/application/cs_pay_uc.erl"
  expect_rule "④" "feature 依赖具体 Plugin" "$e" '铁律9'

  # ⑤ 假 Plugin：引用不含 -callback 的 Feature 模块
  local g="$tmp/fakeplugin"; seed_fixture "$g"
  printf -- '-module(custom_dispatch).\n-export([f/1]).\nf(X) -> cs_session_uc:f(X).\n' > "$g/src/plugins/payment/stripe/custom_dispatch.erl"
  expect_rule "⑤" "假 Plugin（引用非扩展点模块）" "$g" '假 Plugin'

  # ⑥ 真 Plugin：依赖 -callback 扩展点 —— 必须不被误判
  local h="$tmp/realplugin"; seed_fixture "$h"
  printf -- '-module(custom_dispatch).\n-export([f/1]).\nf(X) -> cs_dispatch_strategy:pick(X).\n' > "$h/src/plugins/payment/stripe/custom_dispatch.erl"
  local hout; hout="$(run_all_checks "$h" 2>&1 || true)"
  if printf '%s' "$hout" | rg -q '假 Plugin'; then
    bad "真 Plugin（依赖 -callback 扩展点）被误判为违规"
  else
    ok "真 Plugin（依赖 -callback 扩展点）未被误判"
  fi

  # ⑦ domain 依赖 cowboy（铁律4）
  local i="$tmp/domain"; seed_fixture "$i"
  printf -- '-module(cs_bad_agg).\n-export([f/1]).\nf(X) -> cowboy_req:method(X).\n' > "$i/src/features/cs/domain/cs_bad_agg.erl"
  expect_rule "⑦" "domain 依赖 cowboy" "$i" '铁律4'

  # ⑧ repo SQL 缺 org_id（铁律6）
  local j="$tmp/org"; seed_fixture "$j"
  # $1 is an Erlang SQL placeholder in the fixture.
  # shellcheck disable=SC2016
  printf -- '-module(cs_bad_repo).\n-export([f/1]).\nf(X) -> elib_pg:query(<<"SELECT id FROM s WHERE id = $1">>, [X]).\n' > "$j/src/features/cs/infrastructure/cs_bad_repo.erl"
  expect_rule "⑧" "repo SQL 缺 org_id" "$j" '铁律6'

  # ⑨ 结构白名单外的层目录（铁律2）
  local k="$tmp/struct"; seed_fixture "$k"
  mkdir -p "$k/src/features/cs/services"
  printf -- '-module(cs_misc).\n-export([f/1]).\nf(X) -> X.\n' > "$k/src/features/cs/services/cs_misc.erl"
  expect_rule "⑨" "白名单外层目录 services/" "$k" '铁律2'

  # ⑩ domain 测试用 meck（铁律4）
  local l="$tmp/mock"; seed_fixture "$l"
  printf -- '-module(cs_agg_tests).\n-export([t/0]).\nt() -> meck:new(x).\n' > "$l/test/features/cs/domain/cs_agg_tests.erl"
  expect_rule "⑩" "domain 测试使用 meck" "$l" '铁律4'

  # ⑫ FND-7：Core 插入 Feature 反向引用 -> check_core_no_feature_refs 必须抓红
  local m="$tmp/corefeat"; seed_fixture "$m"
  printf -- '-module(elib_eb_bridge).\n-export([f/1]).\nf(X) -> eb_auth_principal:x(X).\n' > "$m/src/lib/elib_eb_bridge.erl"
  local mout; mout="$(run_all_checks "$m" 2>&1 || true)"
  if printf '%s' "$mout" | rg -q 'Core→Feature'; then
    ok "金丝雀 ⑫ Core 插入 Feature 反向引用被拒"
  else
    bad "金丝雀 ⑫ Core→Feature 检查未触发（门对 Core 是盲区）"
  fi

  # ⑪ 特性裁剪调用门（F-EB10-1）：未保护的跨裁剪调用必须被 prune 门抓红。
  #    门自身带 --selftest（判据红绿两侧），此处验证的是**接线**：arch-check
  #    真的会跑 prune 门且它的红能传导为整体红。
  python3 "$ROOT_DIR/scripts/check_feature_prune_calls.py" --selftest >/dev/null 2>&1 \
    && ok "金丝雀 ⑪a prune 门自测通过（判据红绿两侧）" \
    || bad "金丝雀 ⑪a prune 门自测失败：门不可信"

  echo
  if [[ "$fails" -ne 0 ]]; then
    echo "arch-check 自检失败：$fails 项未通过——门禁不可信" >&2
    exit 1
  fi
  echo "arch-check 自检通过：12 条金丝雀全部按预期触发，基线夹具零违规"
}

main() {
  if [[ "$SELF_TEST" -eq 1 ]]; then
    echo "=== arch-check 金丝雀自检 ==="
    run_self_test
    exit 0
  fi
  cd "$TARGET_ROOT"
  local units; units="$(list_units "$TARGET_ROOT" | wc -l | tr -d ' ')"
  run_all_checks "$TARGET_ROOT"
  if [[ "$VIOLATIONS" -ne 0 ]]; then
    echo "feature architecture check failed（纵切单元 $units 个，警告 $WARNINGS 条）" >&2
    exit 1
  fi
  # F-EB10-1（2026-09-15）：特性裁剪调用门纳入 arch-check 必经清单 ——
  # 「始终编译的模块未加 -ifdef 保护地调用被裁剪模块」在未选中档是运行期 undef
  # （实证：auth_middleware_api_v1 曾致全站 /api/v1 不可用）。baseline 只豁免
  # defensive 既有条目（scripts/feature-prune-baseline.tsv），hard 永不豁免。
  python3 "$ROOT_DIR/scripts/check_feature_prune_calls.py" "$TARGET_ROOT" \
    --baseline "$ROOT_DIR/scripts/feature-prune-baseline.tsv" || exit 1
  echo "feature architecture check passed（纵切单元 $units 个，警告 $WARNINGS 条 + prune 门绿）"
}

main
