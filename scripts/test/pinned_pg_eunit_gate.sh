#!/usr/bin/env bash
# =============================================================================
# pinned_pg_eunit_gate.sh — 钉库真库套件统一执行通道（yield-registry T3-T6）
#
# 背景：nightly 全量轨道（公网 runner 无钉库资源）对这些套件受控让位
# （*_EUNIT_YIELD，见 .qa/yield-registry.md）。让位≠通过：本脚本是它们的
# 独立执行通道——调用方准备好钉库并导出对应钉名 env（TALK_OWNED_PG_* /
# T06_PG_* / DIRECTORY_SYNC_INTTEST_* / <PREFIX>_DB 等）后，逐模块真跑并
# 防零用例假绿（套件让位时 eunit "no tests to run" 会被判失败）。
#
# 用法:
#   bash scripts/test/pinned_pg_eunit_gate.sh MOD [MOD ...]
#   env 参见 scripts/test/group_schedule_remind_pg_gate.sh 同款守卫：
#     PGHOST PGPORT PGUSER PGPASSWORD PGDATABASE
#     IMBOY_TEST_EXCLUSIVE_DATABASE IMBOY_TEST_PG_SYSTEM_ID
#   以及被测套件自身的钉名 env（缺失时套件仍会让位——本脚本会以
#   "yielded" 标注退出码 3，区分于失败）。
#
# 退出码: 0 全部真跑通过；1 用例失败；2 用法/守卫错误；3 全部让位（未执行）。
# =============================================================================
set -euo pipefail
: "${PGHOST:?}" "${PGPORT:?}" "${PGUSER:?}" "${PGPASSWORD:?}" "${PGDATABASE:?}"
: "${IMBOY_TEST_EXCLUSIVE_DATABASE:?}" "${IMBOY_TEST_PG_SYSTEM_ID:?}"
[[ $# -ge 1 ]] || { echo "usage: $0 MOD [MOD ...]" >&2; exit 2; }
[[ "$PGDATABASE" == "$IMBOY_TEST_EXCLUSIVE_DATABASE" ]] || {
  echo 'database identity mismatch' >&2; exit 2; }

root=$(git rev-parse --show-toplevel)
cd "$root"
tmp=$(mktemp -d "${TMPDIR:-/tmp}/imboy-pinned-pg.XXXXXXXX")
trap 'rm -rf -- "$tmp"' EXIT

if command -v gmake >/dev/null 2>&1; then make_bin=gmake; else make_bin=make; fi
# eunit-local（legacy 口径）需要 sys.config 物化；已存在则不动。
[[ -f config/sys.config ]] || cp config/sys.config.example config/sys.config

overall=0
yielded=0
for mod in "$@"; do
  mod_log="$tmp/$mod.log"
  set +e
  IMBOYENV=local "$make_bin" eunit-local t="$mod" >"$mod_log" 2>&1
  rc=$?
  set -e
  tail -40 "$mod_log"
  if grep -q 'There were no tests to run\|test module not found' "$mod_log"; then
    echo "::$mod 未执行（套件让位或模块名有误——核对钉名 env 与模块名）" >&2
    yielded=$((yielded + 1))
    [[ $overall -eq 0 ]] && overall=3
  elif [[ $rc -ne 0 ]] || ! grep -Eq 'Test passed\.|All [1-9][0-9]* tests passed\.' "$mod_log"; then
    overall=1
  fi
done

if [[ $yielded -eq $# ]]; then
  echo "ALL YIELDED: $yielded/$# 套件未执行（缺钉名 env）——本通道不算通过" >&2
fi
exit "$overall"
