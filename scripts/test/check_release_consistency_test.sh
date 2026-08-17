#!/usr/bin/env bash
# ============================================================
# scripts/check_release_consistency.sh 的 fixture 测试
# ------------------------------------------------------------
# 每项检查都跑「正例 + 反例」：只测正例的门禁等于没有门禁 ——
# 必须证明规则被破坏时它真的会失败。
# 运行 / Run:  bash scripts/test/check_release_consistency_test.sh
# ============================================================
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PASS=0
FAIL=0

ok() {
  PASS=$((PASS + 1))
  echo "  ✓ $1"
}
bad() {
  FAIL=$((FAIL + 1))
  echo "  ✗ $1"
}
expect_ok() {
  if [ "$2" -eq 0 ]; then ok "$1"; else bad "$1（期望通过，实际失败）"; fi
}
expect_fail() {
  if [ "$2" -ne 0 ]; then ok "$1"; else bad "$1（期望失败，实际通过 —— 门禁形同虚设）"; fi
}

# shellcheck source=scripts/check_release_consistency.sh
. "${ROOT}/scripts/check_release_consistency.sh"

TMP="$(mktemp -d)"
trap 'rm -rf "${TMP}"' EXIT

# 构造一个「全绿」的最小 fixture 根目录
make_fixture() {
  local d="${TMP}/$1"
  rm -rf "$d"
  mkdir -p "$d/priv/migrations" "$d/scripts/test" "$d/src/lib" "$d/api" "$d/docs/ops" "$d/src"

  echo "9.9.9" >"$d/VERSION"
  echo '{release, {imboy, "9.9.9"}, [imboy]}.' >"$d/relx.config"

  echo "-- up" >"$d/priv/migrations/00000001_a.up.sql"
  echo "-- down" >"$d/priv/migrations/00000001_a.down.sql"
  cp "$ROOT"/priv/migrations/0000006{5_wallet_available_balance_constraint,6_validate_wallet_available_balance_constraint}.{up,down}.sql \
    "$d/priv/migrations/"

  echo "MulanPSL-2.0" >"$d/LICENSE"
  echo "#!/usr/bin/env escript" >"$d/scripts/gen_license.escript"
  cat >"$d/src/lib/imboy_license.erl" <<'ERL'
-module(imboy_license).
public_info() ->
    S = state(),
    #{
        edition => maps:get(edition, S, <<"community">>),
        valid => maps:get(valid, S, false),
        max_users => maps:get(max_users, S, 0)
    }.
ERL

  local f
  for f in backup_pg.sh restore_pg.sh restore_smoke.sh deploy.sh backup_garage.sh; do
    printf '#!/usr/bin/env bash\necho ok\n' >"$d/scripts/$f"
  done
  cp "$ROOT/scripts/verify_wallet_constraint_clone.sh" "$d/scripts/"
  cp "$ROOT/scripts/test/wallet_constraint_clone_guard_test.sh" "$d/scripts/test/"

  # 契约：正例包含全部商业路由；router 同步注册
  : >"$d/api/openapi.yaml"
  : >"$d/src/imboy_router.erl"
  local p
  for p in "${COMMERCIAL_PATHS[@]}"; do
    echo "  ${p}:" >>"$d/api/openapi.yaml"
    [ "$p" = "/brand" ] || echo "            {\"${p}\", some_handler, #{}}," >>"$d/src/imboy_router.erl"
  done

  cat >"$d/docs/ops/support-matrix.md" <<'MD'
| Erlang/OTP | 28+ |
| PostgreSQL | 18+ |
| Flutter | 3.8+ |
| Docker | 24+ |
MD
  echo "$d"
}

run() { # run <fn> <dir> -> 退出码，输出静默
  local fn="$1" dir="$2"
  CRC_PASS=0
  CRC_FAIL=0
  "$fn" "$dir" >/dev/null 2>&1
}

echo "== 版本一致性 =="
D="$(make_fixture version_ok)"
run check_version "$D"
expect_ok "VERSION 与 relx.config 一致时通过" $?

D="$(make_fixture version_bad)"
echo '{release, {imboy, "9.9.10"}, [imboy]}.' >"$D/relx.config"
run check_version "$D"
expect_fail "只 bump 了一处版本号时必须失败" $?

D="$(make_fixture version_missing)"
rm -f "$D/VERSION"
run check_version "$D"
expect_fail "VERSION 缺失时必须失败" $?

echo "== 迁移 up/down =="
D="$(make_fixture mig_ok)"
run check_migrations "$D"
expect_ok "up/down 成对时通过" $?

D="$(make_fixture mig_no_down)"
echo "-- up" >"$D/priv/migrations/00000002_b.up.sql"
run check_migrations "$D"
expect_fail "缺 down 脚本时必须失败" $?

D="$(make_fixture mig_dup)"
echo "-- up" >"$D/priv/migrations/00000001_dup.up.sql"
echo "-- down" >"$D/priv/migrations/00000001_dup.down.sql"
run check_migrations "$D"
expect_fail "迁移序号重复时必须失败" $?

echo "== License 资产与脱敏 =="
D="$(make_fixture lic_ok)"
run check_license "$D"
expect_ok "License 资产齐备且白名单干净时通过" $?

D="$(make_fixture lic_leak)"
# public_info/0 里混入授权原文 —— 正是要挡住的泄漏
python3 - "$D" <<'PY'
# -*- coding: utf-8 -*-
import io, sys
p = sys.argv[1] + '/src/lib/imboy_license.erl'
s = io.open(p, encoding='utf-8').read()
s = s.replace("        max_users => maps:get(max_users, S, 0)",
              "        max_users => maps:get(max_users, S, 0),\n        license_text => maps:get(license_text, S, <<>>)")
io.open(p, 'w', encoding='utf-8').write(s)
PY
run check_license "$D"
expect_fail "public_info/0 泄漏 license_text 时必须失败" $?

D="$(make_fixture lic_missing)"
rm -f "$D/LICENSE"
run check_license "$D"
expect_fail "LICENSE 缺失时必须失败" $?

echo "== 运维脚本 =="
D="$(make_fixture ops_ok)"
run check_ops_scripts "$D"
expect_ok "备份/恢复/部署脚本齐备且语法通过" $?

D="$(make_fixture ops_broken)"
printf '#!/usr/bin/env bash\nif [ 1 -eq 1 ]; then\n' >"$D/scripts/restore_pg.sh"
run check_ops_scripts "$D"
expect_fail "恢复脚本语法错误时必须失败" $?

D="$(make_fixture ops_missing)"
rm -f "$D/scripts/restore_smoke.sh"
run check_ops_scripts "$D"
expect_fail "恢复演练脚本缺失时必须失败" $?

echo "== 商业契约覆盖 =="
D="$(make_fixture contract_ok)"
run check_contract_coverage "$D"
expect_ok "商业路由全部有契约且已注册时通过" $?

D="$(make_fixture contract_missing)"
grep -v "^  /api/adm/sso/config:$" "$D/api/openapi.yaml" >"$D/api/openapi.tmp"
mv "$D/api/openapi.tmp" "$D/api/openapi.yaml"
run check_contract_coverage "$D"
expect_fail "商业路由漏写契约时必须失败" $?

D="$(make_fixture contract_ghost)"
grep -v '"/api/adm/sso/test"' "$D/src/imboy_router.erl" >"$D/src/router.tmp"
mv "$D/src/router.tmp" "$D/src/imboy_router.erl"
run check_contract_coverage "$D"
expect_fail "契约声明了 router 未注册的幽灵端点时必须失败" $?

echo "== 支持矩阵 =="
D="$(make_fixture matrix_ok)"
run check_support_matrix "$D"
expect_ok "支持矩阵声明齐全时通过" $?

D="$(make_fixture matrix_partial)"
grep -v "PostgreSQL" "$D/docs/ops/support-matrix.md" >"$D/docs/ops/m.tmp"
mv "$D/docs/ops/m.tmp" "$D/docs/ops/support-matrix.md"
run check_support_matrix "$D"
expect_fail "支持矩阵漏声明 PostgreSQL 时必须失败" $?

D="$(make_fixture matrix_missing)"
rm -f "$D/docs/ops/support-matrix.md"
run check_support_matrix "$D"
expect_fail "支持矩阵缺失时必须失败" $?

echo "== 钱包约束克隆 Gate =="
D="$(make_fixture wallet_gate_ok)"
run check_wallet_constraint_gate "$D"
expect_ok "隔离克隆 Gate 的离线守卫实际通过时放行" $?

D="$(make_fixture wallet_gate_missing)"
rm -f "$D/scripts/test/wallet_constraint_clone_guard_test.sh"
run check_wallet_constraint_gate "$D"
expect_fail "隔离克隆 Gate 守卫测试缺失时必须失败" $?

D="$(make_fixture wallet_gate_wiring)"
rm -f "$D/scripts/test/wallet_constraint_clone_guard_test.sh"
CRC_PASS=0
CRC_FAIL=0
IMBOY_ROOT="$D" crc_main >/dev/null 2>&1
expect_fail "总发布门禁必须真实调用隔离克隆 Gate" $?

echo ""
echo "通过 ${PASS} 项，失败 ${FAIL} 项"
[ "$FAIL" -eq 0 ]
