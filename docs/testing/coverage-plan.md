# 覆盖率计划（Coverage Plan）

> 把"宣称的覆盖率目标"变成"可度量、可 ratchet 的 CI 门禁" · 配合 `testing-strategy.md`

---

## 为什么需要

评审 P1-T1:三仓覆盖率均无阈值门,书面目标(Repo80/Logic70/Handler60/整体65)全不可验证;admin/imboyapp 仅做 `test -f lcov.info` 存在性检查。**没有度量的目标等于不存在。**

## 覆盖率工具

| 仓库 | 工具 | 现状 | 目标 |
|---|---|---|---|
| imboy 后端 | `cover`(OTP 内置)+ covertool 导出 | Makefile 无 cover 目标 | 加 `make cover`,导出 Cobertura |
| imboyapp | `flutter test --coverage` → lcov | 只查文件存在 | 解析 lcov,设阈值 |
| imboyadmin | bun test coverage(仅 bun，无 vitest/c8) | 无配置 | 加 coverage 配置 |

## 分层覆盖率要求（ratchet 地板 = 首次实测,只升不降）

| 层 | 目标 | 理由 |
|---|---|---|
| 后端 Repo | 80% | SQL 正确性是数据完整性根基 |
| 后端 Logic | 70% | 业务规则密集 |
| 后端 Handler | 60% | 参数/鉴权边界 |
| 后端 整体 | 65% | — |
| Flutter service/store | 70% | 状态与数据核心 |
| Flutter 整体 | 55% | UI 部分靠 widget/maestro |
| Admin services/hooks | 70% | API 契约与权限 |
| Admin 整体 | 55% | — |

> **覆盖率不是目的**:高覆盖率 + mock 边界仍可能漏 bug。覆盖率门是"退化警报",契约真边界测试(TEST-04)才是信号质量保证。二者并行,不可替代。

## 落地路线（ratchet 三步）

1. **度量**：三仓加 coverage 采集,首次运行记录实测值作为 baseline 地板。
2. **冻结**：CI 加门,低于地板即失败(存量不补也不许退)。
3. **递增**：每季度按分层目标上调地板,直至达标。

## CI 执行方式

- 后端:`make cover` 在 full-eunit 硬门后运行,导出 Cobertura,门禁比对地板。
- Flutter:`flutter test --coverage` → lcov,`lcov --summary` 解析,阈值门。
- Admin:`bun test --coverage` 阈值配置进 CI（当前用 bun test，非 vitest）。
- 覆盖率报告上传 artifact + SonarQube(三仓已有 sonar-project.properties)。

## 验收标准

- [ ] 三仓覆盖率 CI 可度量、可比对
- [ ] 门禁地板 = 首次实测,ratchet 只升不降
- [ ] 分层目标达标前,地板每季度递增
- [ ] 覆盖率报告进 Sonar,趋势可见
- [ ] 覆盖率门与契约真边界测试并存(不互相替代)
