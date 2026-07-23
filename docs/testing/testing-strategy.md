# IMBoy 自动化测试体系总纲（Testing Strategy）

> Google SRE + QA + Staff Engineer 视角 · 基于三仓真实测试基础设施 · 日期 2026-07-22
> 本文是总纲,定义测试哲学、分层、门禁与全局约定;各测试类型专项见同目录其余 15 份文档。
> **这是设计文档,不含具体测试代码**;落地节奏见 `docs/roadmap/testing-roadmap.md`。

---

## 0. 现状基线（Fact-based,演进的起点）

测试基础设施**已相当成熟**,演进是收口而非从零搭建:

| 仓库 | 现有资产 | 缺口 |
|---|---|---|
| imboy 后端 | `test/{api,adm,logic,ds,repo,lib}` 380+ EUnit + 9 CT suite + `test/{integration,performance,stress,mcp,domain}` + `test/common/`（meck_helper/test_helper/test_config/cowboy_req_h mock）| 覆盖率无度量;全量 eunit continue-on-error;CT 与 EUnit 职责边界模糊 |
| imboyapp | 433 单测（`test/{smoke,page,unit,widget,service,modules,store,...}`）+ 23 integration_test + `maestro/` 14 条 e2e yaml（含 e2ee_c2c/group）| **更正**：`ci.yml` 已跑 integration job；真实缺口是无覆盖率阈值门 + 独立 `integration_test.yml` 冗余/路径存疑；无头 widget 与异步页不兼容 |
| imboyadmin | 119 bun 单测 + 9 Playwright spec + Channel/Moment regression gate（仅 bun test，无 vitest）| Playwright 零进 CI;无覆盖率门 |

**核心问题不是"没测试"而是"测试信号不可信":** 覆盖率不可度量、E2E 不自动跑、mock 掉协议边界使 5 个真生产 bug 逃过 404 个单测(见 `docs/review/testing-review.md`)。

---

## 1. 测试哲学（5 条原则）

1. **信号真实优先于数字好看**：一个 mock 掉真实边界的绿测试比没有测试更危险(制造安全假象)。契约/存储/协议边界必须用真实依赖测(真 PG、真帧编解码)。
2. **门禁即真相**：书面覆盖率目标若无 CI 阈值门则等于不存在。所有质量目标必须可执行、可拦截。
3. **Ratchet 递增,不准倒退**：存量欠账用 baseline 冻结,新增必须达标,失败数只减不增。后端 xref=0 与"Moment regression gate"是成功样板,推广此模式。
4. **测试金字塔正立**：大量快单元 + 适量集成 + 少量端到端 + 极少手动。反模式是"冰淇淋筒"(重 E2E 轻单元)。
5. **可靠性测试是一等公民**：SRE 视角下 Chaos/Recovery/Migration Rollback 不是可选项——IM 的价值是"消息不丢",必须主动注入故障验证。

---

## 2. 测试维度矩阵（覆盖用户要求的 18 个面）

| 维度 | 主责测试类型 | 专项文档 | 真实依赖 |
|---|---|---|---|
| Server（后端逻辑）| 单元 + 集成 | unit-testing / integration-testing | 真 PG（集成层）|
| Flutter | 单元 + widget + integration + maestro | flutter-testing | 真机（integration）|
| Admin | 单元 + Playwright E2E | 见 ci-testing | 真后端（E2E）|
| API（REST 契约）| 契约 + 集成 | api-testing | 真 PG + 真 handler |
| Database | Repo 集成 + schema | database-testing | 真 PG |
| WebSocket | 帧协议 + 投递 + ACK | websocket-testing | 真帧编解码 + 真连接 |
| E2EE | 密码学正确性 + 端到端 | e2ee-testing | 真 Olm/Megolm |
| Migration | 前滚 + 幂等 + strict 乱序 | migration-testing | 真 PG |
| Upgrade | 灰度 + 双写 + 双读兼容 | migration-testing | 真 PG + 版本矩阵 |
| Rollback | 迁移 down + 发布回退 | migration-testing / chaos | 真 PG |
| CI | 门禁编排 | ci-testing | — |
| CD | 蓝绿 + preflight + 冒烟 | ci-testing | 预发环境 |
| 性能 | 基准 benchmark | performance-testing | 真 PG + 真连接 |
| 压力 | 并发容量 | stress-testing | 负载环境 |
| Chaos | 故障注入 | chaos-testing | 集群环境 |
| Recovery | 恢复验证 | chaos-testing | 集群环境 |
| 安全 | 越权 + 注入 + 密码学 | security-testing | 真 handler |

---

## 3. 测试环境分层

| 环境 | 用途 | 数据 | 依赖 |
|---|---|---|---|
| **L0 单元** | 纯函数/逻辑,毫秒级 | 内存 fixture | meck mock |
| **L1 集成** | Repo/handler + 真 PG | 事务隔离 + 播种 | 本地 PG（docker imboy_pg18）|
| **L2 契约** | 三端协议一致性 | proto/OpenAPI 金标 | 真编解码 |
| **L3 E2E** | 全链路用户流 | 预置账号 | 真后端 + 真机/浏览器 |
| **L4 可靠性** | 性能/压力/chaos | 生成负载 | 隔离集群 |

> 铁律:**含生产数据(PII)的文件严禁进入任何仓库或测试环境**(见根 CLAUDE.md)。测试数据一律合成播种。

---

## 4. 全局 Mock / Fixture / 数据准备约定

- **Mock 边界原则**：只 mock 系统边界之外(第三方 API、时钟、随机源、推送网关);**绝不 mock 自己的协议/存储层**。后端用 `test/common/meck_helper.erl` 统一 mock 生命周期(注意 meck history 是三元组 `{Pid,{M,F,Args},Result}`)。
- **Fixture 分层**：L0 用内存构造器;L1 用 `test/common/test_helper.erl` + 事务回滚隔离;E2E 用幂等播种脚本(`imboy/scripts/seed_demo`、`imboyapp/scripts/setup_test_data.sh`)。
- **数据准备**：每个 L1 测试自带 setup/teardown,事务包裹自动回滚,禁止测试间共享可变状态(评审教训:currentUid 空、contact 未播种致级联登出)。
- **凭证**：测试账号凭证在 `imboyapp/scripts/test.env`(不入库),真机需 `TEST_PHONE`(不能编造)。

---

## 5. 门禁总策略（软门 → 硬门的 ratchet 路线）

| 门禁 | 当前 | 目标 | 收紧任务 |
|---|---|---|---|
| 编译 | 硬门 ✅ | 硬门 | — |
| Moment/Channel regression | 硬门 ✅ | 硬门 | — |
| 零加密检查 | 硬门 ✅ | 硬门 | — |
| 全量 eunit | continue-on-error | ratchet 硬门 | TEST-01 |
| dialyzer | 非阻塞 | ratchet 硬门 | TEST-01 |
| 覆盖率 | 仅文件存在 | 阈值门(地板=实测)| TEST-02 |
| admin E2E | 零进 CI | 进 CI | TEST-03 |
| 协议契约 diff | 无 | 硬门 | ARCH-03 |
| 契约真边界测试 | mock 掩盖 | 真 PG/真帧 | TEST-04 |

---

## 6. 验收标准（体系级）

- [ ] 所有质量目标有对应 CI 门禁(无"宣称但不可验证"的目标)
- [ ] 覆盖率三仓可度量且 ratchet 递增
- [ ] 协议/存储/E2EE 边界用真实依赖测,零 mock 掩盖
- [ ] E2E(admin Playwright + Flutter integration/maestro)全部进 CI
- [ ] Migration 有前滚/幂等/乱序/回滚测试
- [ ] 存在 Chaos/Recovery 定期演练(至少季度)
- [ ] 安全越权/注入有回归测试
- [ ] 性能/压力有基线,退化可拦

---

## 7. 文档索引

test-pyramid · unit-testing · integration-testing · api-testing · websocket-testing · flutter-testing · e2ee-testing · database-testing · migration-testing · performance-testing · stress-testing · chaos-testing · security-testing · ci-testing · coverage-plan
