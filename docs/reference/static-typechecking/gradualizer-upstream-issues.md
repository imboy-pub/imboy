# 类型检查工具局限与研判 / Type checker limitations and triage

本文提供类型告警的研判方法。工具版本、排除项和 CI 预算以 `Makefile`、`.elp.toml` 与 CI 配置为准。每条告警应在当前源码和工具版本上独立核验。

This document describes how to investigate type diagnostics. `Makefile`, `.elp.toml`, and CI configuration define tool versions, exclusions, and budgets. Verify each diagnostic against the current source and tool version.

## 研判顺序 / Investigation sequence

1. 用 `make gradualize FILE=<source>` 或 `make eqwalize MOD=<module>` 复现，并核对 include 路径、依赖和工具版本。
2. 阅读实现、调用方和 spec，确认返回值、错误分支、输入守卫和运行时契约。
3. 对真实缺陷修复实现或 spec；对疑似工具局限提取最小复现，保留复现依据后再决定豁免。

1. Reproduce with `make gradualize FILE=<source>` or `make eqwalize MOD=<module>` and check include paths, dependencies, and tool versions.
2. Inspect implementation, callers, and specs, including return values, error branches, input guards, and runtime contracts.
3. Fix real implementation or spec defects. Reduce suspected tool limitations to a minimal reproduction before granting an exemption.

## 需要重点核验的模式 / Patterns requiring investigation

| 模式 / Pattern | 核验要点 / Verification |
|---|---|
| map 与错误联合类型 / Map and error unions | 确认调用方真正排除了错误分支；函数返回 map 并不意味着所有调用路径都成功。 / Confirm the caller excludes error branches on every relevant path. |
| binary、位串和 Unicode / Binaries, bitstrings, and Unicode | 核对位宽、编码和转换函数接受的类型，不通过放宽 spec 掩盖不合法输入。 / Check widths, encodings, and accepted conversion inputs. |
| 不可达分支 / Unreachable clauses | 对照守卫及兜底路径，判断分支是否确实不可达。 / Check guards and fallback paths before removing a clause. |
| OTP 与依赖 spec / OTP and dependency specs | 核对实际依赖版本、函数选项及返回类型；不能仅因来自第三方就认定误报。 / Verify versions, options, and return types before classifying a false positive. |
| 工具崩溃或 include 解析失败 / Checker crashes or include failures | 区分工具失败与代码检查通过；检查编译选项并提取最小复现。 / Treat tool failure separately from a successful code check. |

## 维护约束 / Maintenance rules

- 豁免必须有范围、原因和复查条件；修改关联代码或升级工具后重新核验。
- 不通过删掉运行时守卫、任意放宽 spec 或忽略错误输出来追求绿色结果。
- 上游 Issue 草稿、执行计划、测试日志、轮次结论和提交清单保存在仓库外。

- Give exemptions a scope, rationale, and review condition; recheck after related source or tool changes.
- Do not remove runtime guards, arbitrarily widen specs, or ignore diagnostics to obtain a green result.
- Keep upstream issue drafts, execution plans, test logs, run conclusions, and commit inventories outside Git.
