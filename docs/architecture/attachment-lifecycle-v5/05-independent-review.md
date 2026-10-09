# 独立反向 Review

日期：2026-10-09。独立 architect Reviewer 只读审阅合同与当前源码，主执行修改后两次回验；没有实施业务代码。

最终结论：**DESIGN_REVIEW_PASS_WITH_EXECUTION_GATES**。本次发现的设计阻断项已关闭；不代表新 SQL、runner、业务修复或真实环境通过。

| 发现 | 原风险 | 设计修订与关闭依据 |
|---|---|---|
| R01 P1 | PG CHECK 的 NULL/UNKNOWN 放过半完整位置 | 显式 driver IS NOT NULL；三字段部分 NULL 全组合列入 G1 |
| R02 P1 | W0 依赖尚未创建的 gate，启动闭环 | 引导任务用现有命令和持久状态；W0-03 追溯验证后才通过 G0 |
| R03 P1 | 旧 queue 位置 NULL，immutable 禁止证明回填 | proof 角色仅 blocked/0attempt/从未 lease 一次填位置，审计和唯一约束 |
| R04 P1 | M3 自动迁移早于业务适配 | W1 只合同/独立 scratch；W4-04 才正式迁移；phase gate 禁旧 worker |
| R05 P1 | 只修 Confirm 就声称 GC 竞态完成 | W2/W3 仅状态协议；W4-01 真实入口竞态；W6 真实字节 |
| R06 P2 | Storage 实现依赖滞后 | 冻结回调/错误类型；W2 固定 Garage，跨驱动验收 W4/W6 |
| R07 P2 | retry_epoch 缺失可能清零无限重试 | V5 不支持已尝试 blocked 重放，8 次终止 |
| R08 P1 | 旧 blocked 首次激活与终止冲突 | 仅 legacy_location_unverified/0attempt/未 lease 且全资格证明后首次 ready |
| R09 P2 | M3 激活缺机器 case | W4-04 强制 guards-activation/all-writer-compat，文档 JSON 同步 |

## 六维评价：设计评分，不是实施评分

| 维度 | 分数/10 | 判断及实际证据边界 |
|---|---:|---|
| 架构正确性 | 8.5 | 五对象与线性化明确；新锁序/触发器实测 UNVERIFIED |
| 最小改动程度 | 8.0 | 新业务表 0、复用队列/治理/引用；Storage Boundary 仍是较大必要依赖 |
| 代码匹配度 | 8.5 | 基线源码与真实 catalog 有证据；Flutter 外部 WIP 须 W0 重绑定 |
| 可执行性 | 8.0 | 20 任务、7 Gate、有限恢复；runner/新迁移/gate 未实施，UNVERIFIED |
| 安全性 | 8.5 | 未知保留、提交后 DELETE、限次队列；真实竞态/存储故障 UNVERIFIED |
| 测试覆盖设计 | 8.5 | 八 AC 与故障矩阵；修复测试未执行，UNVERIFIED |

维持复查时保守分数，不因文字修订加分。实际执行只有基线 Schema 重建、SQL 探针、迁移命名检查；针对性 EUnit 因缺配置取消，执行 0 项，不能算 PASS。业务实施、用户 UAT 和生产验收均未通过。

Reviewer 最终复核确认上述设计阻断项关闭。新增 Flutter 外部 WIP 必须 W0 重绑定，不能将本次 Review 当作允许覆盖外部工作的授权。
