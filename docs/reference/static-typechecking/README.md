# 静态类型检查 / Static type checking

本目录提供 Erlang 类型检查器选型与工具局限的维护说明。实际命令、工具版本和 CI 门禁以仓库中的 `Makefile`、`.elp.toml` 和 `.github/workflows/backend-ci.yml` 为准。

This directory documents Erlang type checker selection and known tool limitations. The repository's `Makefile`, `.elp.toml`, and `.github/workflows/backend-ci.yml` define the commands, tool versions, and CI gates.

## 文档 / Documentation

| 文档 / Document | 用途 / Purpose |
|---|---|
| [类型检查器分析 / Type checker analysis](./erlang-static-typecheckers-analysis.md) | Gradualizer、eqWAlizer 与 Dialyzer 的能力和选型依据 / Capabilities and selection criteria |
| [工具局限与处理依据 / Tool limitations and decisions](./gradualizer-upstream-issues.md) | 已知误报模式及处理依据 / Known false positives and handling decisions |

## 使用与维护 / Usage and maintenance

- 使用 `make gradualize`、`make eqwalize` 或分层检查目标检查当前代码；目标参数见 `Makefile`。
- 判断告警时同时核对工具输出和源码。修复真实类型或运行时缺陷；记录工具局限的可复现原因。
- 调整 CI 预算前重新测量当前候选，避免用历史测试数字证明当前质量。
- 执行计划、审查报告、测试结果和一次性材料保存在仓库外。

- Check the current source with `make gradualize`, `make eqwalize`, or the layer-specific targets; consult `Makefile` for parameters.
- Compare diagnostics with source code. Fix real type or runtime defects and document reproducible tool limitations.
- Measure the current candidate before changing CI budgets; historical results do not prove current quality.
- Keep execution plans, review reports, test results, and one-time materials outside the repository.
