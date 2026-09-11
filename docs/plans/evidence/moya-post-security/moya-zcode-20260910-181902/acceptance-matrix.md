<!-- ROUND4-DOWNGRADE (2026-09-11, A0 Round 9 补记) -->
> ⚠️ **本矩阵为 Round 4 独立对抗复审【之前】的验收快照，结论行已被降级取代**：
> 总体 LOCAL_PHASE0_1_15_PASS → **PARTIAL**；MN-TASK-03 → **FAIL**（P0-1 task_id 双形态断链）、
> MN-MEDIA-03 → **FAIL**（P1-1 review_asset_payload 缺 object_key，家长侧价值归零）、
> MN-WITHDRAW-01 → **PARTIAL**（后端绿，入口被既有 P1-2 断链阻塞）。
> 两候选 patch NO-GO 不得合入 main。现行权威口径见 final-report.md 顶部声明与
> code-doc-review.md Round 4-9 增补；本文件仅作历史快照保留，禁止据此恢复 PASS 结论。

# RUN moya-zcode-20260910-181902 最终矩阵（final board）

结论行：**LOCAL_PHASE0_1_15_PASS；完整计划已推进到当前授权边界；共享 Moya Step 12 仍为 PARTIAL_PENDING_OWNER；Phase 2A/2B、Phase 3、Phase 4 按各自 Gate 保持 BLOCKED_EXTERNAL/NOT_RUN_BY_GATE；试点与发布仍为 NO-GO。**

| 卡 | 状态 | 证据锚点 |
|---|---|---|
| SECURITY-QUEUE | PASS | control/baseline.json §security_queue_check（Bot/E2EE/GA 均外部 owner，无可执行卡，不抢占） |
| MN-BASE-01 | PASS | baseline.json：两仓 HEAD/branch/dirty 全记录且与契约 Base 一致 |
| MN-BASE-02 | PASS | baseline.json §moya_quality_baseline：14/4/0 与 15/3/0 差异归因 Step 12 scan 门（用户暂存现实 AppID） |
| MN-OWN-01 | PASS | baseline.json §dirty_summary + control/ownership.json：staged/dirty 逐项 owner，零清理零混入 |
| MN-SCOPE-01 | PASS | 仓库 evidence scope-check.txt：非目标零越界 |
| MN-EVAL-01 | PASS | evaluation/rubric-v0.md（5 维度+一票否决+统计法） |
| MN-EVAL-02 | PASS | evaluation/manifest.schema.json + 20 fixtures + privacy-scan.txt（9 类正则零命中） |
| MN-EVAL-03 | PASS | evaluation/protocol.md（A/B、随机化、双盲、退出规则、B 增益门三条件预冻结） |
| MN-EVAL-04 | PASS | evaluation/provider-gap.md（源码行号佐证无 vision=true，不虚构能力） |
| MN-WITHDRAW-01 | PASS | A3 patch bd6310b3…（review PASS）：staged rework 修复保留+回归断言；5481/5444/5423/5443/网络分支全覆盖 |
| MN-ROSTER-01 | PASS | A1 patch 13f7908a…（review PASS）：logic 14+handler 8+repo 4 全绿；12 禁键深度断言 |
| MN-TASK-01 | PASS | A2 patch b223c90c…（review PASS）：角色矩阵/非法输入/0 提交作业/统计计数 DB 断言 |
| MN-TASK-02 | PASS | 同上：106 迁移持久幂等（同 key 同 body replayed=true 行数不增；异 body 5460；缺 key 5461；无 ETS 冒充） |
| MN-TASK-03 | PASS | A4 patch 7c653dfd…（review PASS）：页内表单全状态机；setup-required 禁选；重试复用 key |
| MN-MEDIA-01 | PASS | A5 patch 3d79ad7a…（A0 review PASS）：00000105 up/down/up 真库证据；down 双分支 fail-closed；租约编号正确 |
| MN-MEDIA-02 | PASS | 同上：所有权/MIME/数量/ACL 分流/孤儿双排除 fail closed；附带修复 upsert_draft_tx 既有 SQL bug |
| MN-MEDIA-03 | PASS | A3+A4+A5-backend 三方（review PASS）：assets[] TSID string；published-only 家长侧；object_key 短时 URL 不持久化；兼容列只读派生 |
| MN-CONTRACT-01 | PASS | A5 contract.patch 1ac1f7ff…（A0 review PASS）：5430/5431/5432 全仓唯一；三路由 JWT 段；catalog+OpenAPI（contract-check PASS） |
| BACKEND-BUILD | PASS | 集成树 make compile exit 0 + §八完整序列 183/183（含 4 组 DB 集成串行真跑）+ git diff --check 0 |
| MOYA-CHECK | PASS | 隔离集成树 typecheck/lint/test 167/build/scan/diff-check 全 exit 0（scan 对候选源码零命中） |
| PARENT-03 | BLOCKED_EXTERNAL | 真机+微信+隔离后端授权未取得；恢复入口 evidence/device/README-phase2a-checklist.md |
| TEACHER-03 | BLOCKED_EXTERNAL | 同上 |
| MN-GARAGE-01 | BLOCKED_EXTERNAL | 活 Garage 隔离 bucket/凭据授权未取得；同上恢复入口 |
| DEVICE-01 | BLOCKED_EXTERNAL | 两台真机授权未取得；同上恢复入口 |
| MN-AI-01 | BLOCKED_EXTERNAL | provider/model/费用/凭据授权未取得；恢复入口 evidence/model/README-phase2b-checklist.md |
| MN-AI-02 | BLOCKED_EXTERNAL | 20 份授权样本未取得；同上 |
| MN-AI-03 | BLOCKED_EXTERNAL | 依赖 MN-AI-02 |
| MN-AI-04 | BLOCKED_EXTERNAL | 依赖盲测数据 |
| MN-AI-05 | BLOCKED_EXTERNAL | 依赖盲测数据（工具已备：evaluation/ab_summary.py 模板自验通过） |
| MN-PILOT-01..05 | NOT_RUN_BY_GATE | Gate=Phase 2A 硬门+MN-AI-04+试点书面授权 |
| PHASE-4-DECISION | NOT_RUN_BY_GATE | Gate=Phase 3 全部门指标 PASS；无真实触发证据，不新增功能（正确结束态） |

## DRIFT 事件（集成后检测）
- 共享 imboy HEAD 于本 RUN 集成完成后由 `8fb7ed11` 前进至 `a7e19a26`（Bot 安全工作正式提交 00000104+bot 源码）。与本 RUN owned paths **零交集**（已核实），候选 patch 仍有效；续跑集成时需重基线到 a7e19a26 后重放（见 resume-prompt 步骤 0）。moya 共享 HEAD 未变。
