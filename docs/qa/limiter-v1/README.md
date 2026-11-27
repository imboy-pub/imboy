# limiter-v1 验收证据索引 / Limiter V1 Acceptance Evidence Index

> **现行口径（B-3 对齐）**：limiter-v1（IMBOY_LIMITER_V1 验收车道）的正式证据
> 已纳入版本控制，唯一权威位置即本目录 `docs/qa/limiter-v1/<候选提交短哈希>/<运行引用>/`。
> 权威计划 `docs/plans/2026-10-13-limiter-v1-local-evidence-and-plan.md` 附录 E
> 写作时约定证据存**仓外忽略目录**；后续授权任务已将正式证据落版本控制，计划
> 文本按历史档案原则保持原貌不重写——以本页为现行唯一口径。

## 证据目录索引

| 候选（git 可解析） | 内容 |
|---|---|
| `0d28ad4f/baseline/` | AC-16 旧基线冻结输入表（L0，测量前冻结） |
| `124dcbb7/20261014T155741-81062/` | AC-01 REST oracle（api_v1_login_SUITE limiter_real_failures） |
| `417ba55f/20261014T130107-82080/gates/` | 第三轮 main 合入记录（零冲突核查） |
| `7d2239a5/20261015T*/` | L2/L3 混合负载与切换批次运行 ×4 |
| `997aedfb/20261015T*/` | 同批次后续运行 ×4 |
| `fcc9013a/final-eunit/` | 最终形态全量 EUnit 归因（本任务 6 模块零失败；180 失败逐条归并为基线/他域） |

每个候选短哈希均可在本仓 git 历史解析（验证：`git log --oneline -1 <hash>`）。

## 关联

- 权威计划与 AC 合同：`docs/plans/2026-10-13-limiter-v1-local-evidence-and-plan.md`
- 验收入口：`scripts/limiter_v1_acceptance.escript`（按 AC 编号 + 候选清单驱动）
- 现行实现：`login_attempt_ds`（IP 请求桶上移 handler 层，见
  `src/logic/auth_oidc_logic.erl` 与 `src/api/auth_oidc_handler.erl` 注记）
