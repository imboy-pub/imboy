# 自动合并政策（Auto Merge Policy）

> 版本 1.0 · 2026-11-18 · 隶属 [ai-engineering](./)
> 现状声明：**当前唯一已授权的自动合并 = 三仓 dependabot 白名单**（dependabot-auto-merge.yml，属主既定配置）。本文将其成文并定义扩展前提；**不启用新的自动合并，不改分支保护**。

---

## 1. 风险分级

| 级 | 类别 | 自动化许可 |
|---|---|---|
| R0 | 文档修正、格式化、注释/引用漂移修复 | 可探索（仍须过全部门禁） |
| R1 | 独立测试补充、无行为影响的 lint 修复、dependabot patch/minor | dependabot 已运行；其余待授权 |
| R2 | 功能代码、重构、CI 配置变更 | 人工 review 必须 |
| R3 | DB Migration、消息协议、E2EE、RBAC、支付/退款、数据删除、生产配置、基础设施、部署 | **属主审批必须**，永不自动 |

## 2. 自动合并的必要条件（全部满足，缺一不可）

1. 任务范围在 R0/R1 白名单内（扩展白名单须属主批准并记入 §4）。
2. 仓库全部必需 CI checks 通过（以 GitHub Checks 为准）。
3. 独立 Verification PASS（[VERIFICATION_POLICY.md](./VERIFICATION_POLICY.md) 证据齐备）。
4. 无未解决的高优先级 review 意见；无安全门禁失败（secrets-scan/security-scan）。
5. 可追溯：Git SHA + 任务 ID + evidence 路径写入合并记录。
6. 符合仓库分支保护规则（strict 保护下由 merge API 服务端强制）。

## 3. 已固化的工程事实（不得遗忘）

- imboyadmin dependabot 车道曾因 **GITHUB_TOKEN 读不了 branchProtectionRule（admin 专属）+ triage 自身 pending 致 mergeable_state 恒 unstable** 双层死锁而必挂——修复配方是 strict 分支保护（checks 非 contexts）。任何新自动化合并前先核此坑。
- installation API 配额会被多循环并发烧干：同一时刻只允许一个 triage/merge 循环。
- 多 PR 快合时序坑：squash 后分支 run 即死重；concurrency 使 main push run 串行，SKIPPED/queued/cancelled 不是失败。

## 4. 扩展路径（每步均需属主显式授权后另行立任务实施）

1. R0 文档/格式 PR 的 label 触发式自动合并（先在 imboyadmin 试点，因其分支保护配方最成熟）。
2. 合并后主线程冒烟（imboy `make ctl ARGS="smoke all"`）自动回报。
3. 失败自动回滚（revert PR 自动创建，**合并仍人工**）。

## 5. 禁止事项

- 任何 agent 不得为了解除合并阻塞而：修改分支保护、跳过 DCO、重跑/取消他人 CI run、force push。
- 判定不得把 SKIPPED/未执行 checks 当作通过。
