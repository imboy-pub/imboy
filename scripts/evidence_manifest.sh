#!/usr/bin/env bash
# 生成发布证据清单（Release Evidence Manifest，审计就绪包附录 B / P5-8）。
#
# 字段集遵循 docs/guides/e2ee/v2/20-implementation-and-acceptance-plan.md §13。
#
# 设计上的一条硬规矩：**拿不到的字段必须打印"未提供 + 原因"，不能省略**。
# 省略会让读者默认"这项没问题"，而真相往往是"这项从没做过"。一份漏报的
# 证据清单比没有清单更危险——它看起来像已经核验过了。
#
# 用法:
#   scripts/evidence_manifest.sh > docs/guides/e2ee/standard/evidence-manifest.generated.md
#
# 不含秘密：只读 git 元数据、lock 文件哈希、测试文件计数；不读 .env、不连 DB、不打网络。
set -uo pipefail

cd "$(dirname "$0")/.." || exit 1
APP="../imboyapp"

h() { printf '\n## %s\n\n' "$1"; }
kv() { printf '| %s | %s |\n' "$1" "$2"; }
th() { printf '| 项 | 值 |\n|---|---|\n'; }
na() { printf '| %s | ⛔ **未提供** —— %s |\n' "$1" "$2"; }

sha_of() { [ -f "$1" ] && shasum -a 256 "$1" | cut -c1-16 || echo "文件不存在"; }
repo_head() { git -C "$1" rev-parse --short HEAD 2>/dev/null || echo "非 git 仓"; }
repo_dirty() {
    local n
    n=$(git -C "$1" status --porcelain 2>/dev/null | grep -c .) || n=0
    [ "$n" -eq 0 ] && echo "clean" || echo "⚠️ $n 个文件未提交（manifest 与工作树不一致）"
}

echo "# Release Evidence Manifest（机器生成）"
echo
echo "> 由 \`scripts/evidence_manifest.sh\` 生成。字段集见 \`../v2/20-implementation-and-acceptance-plan.md\` §13。"
echo "> **⛔ 标记的字段是真的没有，不是懒得填。**"

h "1. 版本与 commit 锚"
th
kv "imboy HEAD" "\`$(repo_head .)\`"
kv "imboy 工作树" "$(repo_dirty .)"
kv "imboyapp HEAD" "\`$(repo_head "$APP")\`"
kv "imboyapp 工作树" "$(repo_dirty "$APP")"
kv "imboyapp 版本" "$(sed -n 's/^version: *//p' "$APP/pubspec.yaml" 2>/dev/null | head -1)"

h "2. 依赖锁与哈希（SHA-256 前 16 位）"
th
kv "imboyapp \`pubspec.lock\`" "\`$(sha_of "$APP/pubspec.lock")\`"
kv "imboy \`Makefile\`（DEPS 真源）" "\`$(sha_of Makefile)\`"
kv "许可证清单（生成态）" "\`$(sha_of docs/legal/third-party-licenses.generated.md)\`"
na "SBOM（CycloneDX/SPDX）" "未生成。\`SBOM Diff Report\` 工作流跑 trivy，但产物未纳入本清单"

h "3. 测试计数"
th
kv "Erlang eunit 测试函数" "$(grep -rhoE '^[a-z_0-9]+_test_?\(\) ->' test/ 2>/dev/null | grep -c . || echo 0)"
kv "Erlang 测试文件" "$(find test -name '*.erl' 2>/dev/null | grep -c . || echo 0)"
kv "Dart 测试用例（test/testWidgets）" "$(grep -rhoE '^[[:space:]]*(test|testWidgets)\(' "$APP/test" 2>/dev/null | grep -c . || echo 0)"
kv "Dart 测试文件" "$(find "$APP/test" -name '*_test.dart' 2>/dev/null | grep -c . || echo 0)"
kv "其中 e2ee 专项测试文件" "$(find "$APP/test/service/e2ee" -name '*_test.dart' 2>/dev/null | grep -c . || echo 0)"
kv "已知 skip / 暂排" "CI 暂排 2 个文件（X16/X17，理由见 known-issues-ledger IMB-2026-026）"

h "4. 互操作与向量"
th
kv "跨实现 golden vectors" "KT profile v1 已核验（ADR 29 §8）；fallback key canonical 双端钉死（含长度 82）"
na "向量文件哈希" "向量以内联形式散在 ADR/测试中，无独立向量文件可哈希"
na "第三方实现互操作测试" "未做。我方两端均为自研客户端，无第三方 Olm 实现对接"

h "5. 真机与性能"
th
na "真机型号 / OS / 性能结果" "**双端真机从未验证**（IMB-2026-021 / X2）"

h "6. 鲁棒性"
th
na "fuzz 语料 / 运行次数 / crash 数" "未做 fuzz"
na "崩溃一致性运行次数 / 失败数" "未做。无跨进程 harness（IMB-2026-022）"
na "故障注入计数" "同上"

h "7. 迁移与回滚"
th
kv "迁移文件数" "$(find priv/migrations -name '*.up.sql' 2>/dev/null | grep -c . || echo 0)"
kv "最新迁移" "$(ls priv/migrations/*.up.sql 2>/dev/null | tail -1 | xargs -n1 basename 2>/dev/null)"
na "回滚演练 id" "未做演练"

h "8. 外部审计"
th
kv "上游 \`vodozemac\`" "Least Authority 2022-03 已审计（建议复用结论，重点审我方胶水层）"
na "我方外部审计报告 id / open findings" "**未采购审计**。按 2026-08-01 决策，本包为「就绪包」而非已完成审计（TT-D5 SHOULD 降级形态）"

h "9. 发布与灰度"
th
na "canary 指标窗口 / stop-trigger 计数" "未做灰度发布流程"

h "10. 签署"
th
kv "负责人" "leeyi（solo）"
na "时间戳" "由发布流程在采纳本 manifest 时填写；脚本不自造时间以免与 git 历史矛盾"

h "11. 门禁现状"
th
kv "分发阻断门" "\`scripts/license_inventory.sh --check\` —— **当前必然退出 1**（AGPL 未解，预期行为）"
kv "许可证判别自检" "\`scripts/license_inventory.sh --selftest\` 10/10"
kv "模块边界门" "\`scripts/check_module_boundaries.sh\`"
echo
echo "---"
echo
echo "本 manifest **不含**密钥、用户数据或生产 PII。"
