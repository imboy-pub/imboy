# IMBoy 附件生命周期 V5 工程合同

**设计评审通过，实施尚未开始。** 本轮仅新增合同及脱敏取证材料。

1. [真实现状与差距](01-facts-and-gaps.md)
2. [V5 主计划](02-main-plan.md)
3. [迁移合同](03-migration-contract.md)
4. [W0—W6 任务卡](04-tasks.md)
5. [结构化执行状态合同](execution-contract.json)
6. [独立反向 Review 与六维评分](05-independent-review.md)
7. [实际验证摘要](evidence/verification-summary.json)

20 个任务，7 个不可跳过 Gate，8 条验收不变量。真实 PostgreSQL 18.1 scratch 重放 164 个 Up；3 个基线 SQL 观察断言通过。针对性 EUnit 因缺配置取消，exit=2、执行 0 项，不计通过。真实 Garage/Disk、Flutter 真机、浏览器、故障竞态、历史业务数据和生产均 UNVERIFIED。

未来执行先验证 `shasum -a 256 -c manifest.sha256`（在本目录），再核对三仓源码基线及 foreign 清单。后端仅包含本合同目录的文档提交可作为基线后代，但必须证明没有额外源码变动；其他漂移不得自动接受。当前 Flutter 目标文件有外部 WIP，W0 须重绑定并解决所有权，不能接管。

本轮未新增业务迁移、代码或测试 runner；新命令由 W0-03 实现后才可调用。W0 引导任务使用现有工具。Push、部署、联系人变更和生产数据操作不在授权范围。

evidence 被通用 ignore 规则忽略，本次明确纳入文档提交；材料只含源码、空库 schema、合成探针和状态，不含业务数据快照。本 run 创建的 scratch 库保留供复核，不清理其他会话资源。
