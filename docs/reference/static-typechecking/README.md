# 静态类型检查（Static Type Checking）

> 本目录收录 imboy 引入 **Gradualizer + eqWAlizer** 双引擎静态类型检查的规划、分析、决策日志与 CI 集成验证记录。所有文档均已迁移入仓（2026-07-25，原位于工作区根级游离 `docs/`，对买家 / CI 不可见）。

## 文档索引

| 文档 | 类型 | 内容 |
|------|------|------|
| [erlang-static-typecheckers-analysis.md](./erlang-static-typecheckers-analysis.md) | 分析 | Erlang 主流静态类型检查器（Gradualizer / eqWAlizer / Dialyzer）能力对比与选型依据 |
| [gradualizer-landing-plan.md](./gradualizer-landing-plan.md) | 规划 | Gradualizer 落地路线图：P1 工具骨架 / P2 CI 预算校准 / P3 分层推进 |
| [elp-eqwalizer-landing-plan.md](./elp-eqwalizer-landing-plan.md) | 规划 | eqWAlizer（via ELP）落地路线图与白名单分层 enable 策略 |
| [gradualizer-upstream-issues.md](./gradualizer-upstream-issues.md) | 决策日志 | Gradualizer / eqWAlizer **误报模式决策表 + 已修复真阳性索引（持续维护）** |
| [typecheck-integration-verification.md](./typecheck-integration-verification.md) | 验证 | 双引擎 CI 集成验证：Makefile 目标、CI job、预算 ratchet 行为 |
| [eqwalizer-support-fork-plan.md](./eqwalizer-support-fork-plan.md) | 专项方案 | **eqwalizer_support fork 方案**：覆盖 crypto/uri_string/epgsql 第三方类型缝隙，让 lib 层 eqWAlizer 转阻塞的唯一路径（2026-07-25 R16）|

## 当前状态（截至 2026-07-25）

- **真阳性治理**：Gradualizer 驱动 6 个 + eqWAlizer 驱动 1 个 = **7 个精确提交**（`f56a8cd1` → `31147564`）
- **白名单绿名单**：经双重扫描（R8–R10 全量 + R11 lib 重跑）确认全仓 **185 个 0-error 模块**，可直接 enable 不新增 CI 阻断
- **CI 集成**：`gradualize` / `eqwalize` / `eqwalize-layer` Makefile 目标与 CI job 均已实现；预算 ratchet 待 maintainer 在 GitHub 后台设 `vars.GRADUALIZE_BUDGET` 激活
- **工具行为关键认知**：`elp eqwalize <mod>` 强制检查（忽略 `-eqwalizer(enable).` 属性）；enable 属性仅对 `eqwalize-all` + IDE 生效

## 维护纪律

- 每轮新误报模式 → 追加到 `gradualizer-upstream-issues.md` 决策表
- 每修复真阳性 → 追加索引（commit 反例）
- 真伪判定必须三步：`运行 gradualize/eqwalize` + `读源码` + `确认真伪`，不可凭直觉
- 真阳性（spec 与实现不符 / 真实崩溃路径）→ 修；工具对 OTP29 / re / crypto / map 联合类型推理局限 → 记误报不修
