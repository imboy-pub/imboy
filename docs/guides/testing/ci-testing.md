# CI / CD 测试编排（CI/CD Testing）

> 门禁编排、CD 冒烟、发布验证 · 基于三仓真实 workflow

---

## 为什么需要

CI 是"门禁即真相"的执行者。评审证明现有 CI 结构合理(ratchet 框架已搭)但多数门是软门;CD 侧蓝绿部署有 preflight 但发布后验证靠手动。

## 覆盖范围

- CI:编译门、单元/集成门、覆盖率门、契约 diff 门、E2E 门、安全检查门
- CD:preflight 前置检查、蓝绿切换、发布后冒烟、回滚触发

## 现状（Fact-based）

| 仓库 | 阻塞门 | 非阻塞/缺失 |
|---|---|---|
| imboy | 编译、Moment EUnit、零加密检查、重复模块检查、DCO | 全量 eunit(continue-on-error)、dialyzer、覆盖率、CT |
| imboyapp | analyze、模块边界、new code guard、unit/widget、Moment gate、**integration(ci.yml 已跑)** | 覆盖率阈值门、maestro;独立 integration_test.yml 冗余/路径存疑 |
| imboyadmin | lint、bun test、Channel/Moment gate | Playwright E2E(零引用)、覆盖率 |

## 目标 CI 流水线（分阶段门）

```
Stage 1 快门(每 PR,分钟级)     : 编译 + lint + analyze + 单元 + 边界 + 零加密 + 契约 diff
Stage 2 集成门(每 PR)          : 真 PG 集成 + CT suite + 覆盖率阈值
Stage 3 E2E 门(合并前/nightly) : Flutter integration + maestro + admin Playwright
Stage 4 可靠性(nightly/周)     : 性能基准 + 压力 + chaos 演练
```

## 推荐框架

GitHub Actions(现用)+ matrix(OTP 版本)+ 缓存(deps/PLT)+ artifact(覆盖率/报告/截图/trace)。服务容器用 PG18(pg_jieba/timescaledb 镜像)。

## Mock / Fixture / 数据准备

- Stage 1 全 mock;Stage 2 起真 PG 服务容器 + 迁移到最新 + 幂等播种。
- 播种用 `scripts/seed_demo`(后端)、`setup_test_data.sh`(Flutter)。
- 凭证经 GitHub Secrets 注入,绝不硬编码;PII 绝不进 CI。

## CI 执行方式（收紧路线）

1. 全量 eunit + dialyzer 收紧为 ratchet 硬门(TEST-01)。
2. 覆盖率阈值门(TEST-02)。
3. admin Playwright 进 CI(TEST-03)、修 integration_test.yml 坏死(TEST-05)。
4. 协议 proto/OpenAPI/ws_url diff 门(ARCH-03)。
5. E2E 门在合并前 + nightly 双跑,flaky 隔离(quarantine)。

## CD 测试

- **preflight.sh**(已有):部署前环境/配置/密钥校验。
- **发布后冒烟**:蓝绿切换后自动跑 `smoke`(c2c/ws/ctl)+ `prod-health-check.spec.ts`,失败自动回滚。
- **金丝雀**:新版本先切小流量,监控 SLA(错误率/延迟),达标再全量。
- **回滚验证**:定期演练蓝绿回退 + 迁移 down(见 migration-testing / chaos)。

## 覆盖率要求

CI 编排本身无覆盖率概念;其职责是**执行并强制**各测试类型的覆盖率门(见 coverage-plan)。

## 验收标准

- [ ] 四阶段门就位,快门分钟级
- [ ] 全量 eunit/dialyzer/覆盖率/E2E/契约全部硬门或 ratchet
- [ ] CD 发布后自动冒烟,失败自动回滚
- [ ] 金丝雀 + SLA 监控联动
- [ ] flaky 测试隔离机制,不阻塞主干
- [ ] 回滚季度演练有记录
