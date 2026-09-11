# RUN moya-zcode-20260910-181902 最终矩阵（final board）

> ⚠️ **改级声明（2026-09-11）**：原 LOCAL_PHASE0_1_15_PASS 经独立对抗复审证伪为 **PARTIAL**——
> 跨端形状断层（task_id/object_key/图片-only 发布）被双端 mock 互证掩盖；两候选 patch **NO-GO 不得合入**。
> 详见 code-doc-review.md Round 4 与 agents/independent-review/review.md；下表 PASS 声明仅在其注明的本地门范围内成立。

> ✅ **v3 修复轮（2026-09-11，用户授权「发现的所有 patch」后执行）**：九轮 review 台账
> （P0×1/P1×3/P2×12/P3×4）全部修复，门全绿（imboy 183/183 同口径 11 套件+新增 4；moya 168/168+
> typecheck/lint/build/scan；contract-check PASS；00000105 down 真库往返验证）。
> **权威候选升级为 v3**：imboy （67 文件，
> SHA-256 05a3f5011a1042042c4d908e9e389b4af52436bd190a0dfce1106a14056a3e6b）；
> moya （21 文件，
> SHA-256 2e4c415518adbe916c8acdff21d19f6210b7006bb9b256a92dbb390883feaffb）。
> v2 两 patch 作废勿用。总体=LOCAL_ALL_PASS_PENDING_RE_REVIEW（合 main 仍需用户 Git 授权+独立复审确认）。
> 下表 PASS 声明仅在其注明的本地门范围内成立。

> ✅ **v3 修复轮（2026-09-11，用户授权「发现的所有 patch」后执行）**：九轮 review 台账
> （P0×1/P1×3/P2×12/P3×4）全部修复，门全绿（imboy 183/183 同口径 11 套件+新增 4；moya 168/168+
> typecheck/lint/build/scan；contract-check PASS；00000105 down 真库往返验证）。
> **权威候选升级为 v3**：imboy final/candidate-imboy-v3-a7e19a26.patch（67 文件，SHA-256 05a3f5011a1042042c4d908e9e389b4af52436bd190a0dfce1106a14056a3e6b）；
> moya final/candidate-moya-v3-1a636660.patch（21 文件，SHA-256 2e4c415518adbe916c8acdff21d19f6210b7006bb9b256a92dbb390883feaffb）。
> v2 两 patch 作废勿用。总体=LOCAL_ALL_PASS_PENDING_RE_REVIEW（合 main 仍需用户 Git 授权+独立复审确认）。
> 下表 PASS 声明仅在其注明的本地门范围内成立。

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

## 运行要素摘要
- RUN_ID: moya-zcode-20260910-181902 | WORKTREE_ROOT: /Users/leeyi/project/imboy.pub/.worktree/moya-zcode-phase15/moya-zcode-20260910-181902
- RUN_ROOT（仓库外完整证据）: /private/tmp/moya-zcode-phase15.moya-zcode-20260910-181902
- 权威计划: docs/plans/2026-09-10-moya-post-security-product-plan.md SHA-256 b1c9f005…c9b1（与契约期望一致，零漂移）
- Base: imboy 8fb7ed11b76efeb6a5c32fe605897dd10f1a66a5 / moya 1a636660f50419dda0641a7bb3ced87dd1a36021（detached worktree 冻结；共享树后漂移 a7e19a26 见 DRIFT 节）
- 迁移租约: 00000105 teaching_review_asset(A5) / 00000106 group_task_idempotency(A2)；00000104 归 Bot 安全 owner（运行中已由其 owner 提交至 main，本 RUN 未触碰）
- Agent worktrees（持久保留）: agents/{A1,A2,A5}/imboy、agents/{A3,A4}/moya（路径见 WORKTREE_ROOT 下）
- 共享树未动证明: RUN_ROOT/control/shared-*-status-after.txt 与 Wave0 基线逐字一致（moya 三 staged 文件原样；imboy HEAD 前进系外部 owner 提交，非本 RUN）
- scratch DB: moya_zcode_181902@127.0.0.1:4323（口令脱敏；测试后未删，供续跑）
- 已知偏差: ①A4 GET 自动重试仅覆盖 5xx（公共封装不在任何 owned 清单，backlog）；②/tasks 路由由 imboy_router shim 承载 method 分派（cowboy 同路径遮蔽语义等价，收敛项在案）；③A1/A2 handler 整数传码待切宏（值相同断言兼容）；④contract-check.log 因仓级 *.log gitignore 不入 patch，留档 RUN_ROOT
- 外部门与恢复入口: 全部 8 项见 RUN_ROOT/control/external-gates.json；续跑指引=RUN_ROOT/final/resume-prompt.md
- 审查链: A5 media/contract ← A0 独立审查 PASS；A1-A4 ← A5 独立审查全 PASS（RUN_ROOT/agents/A5/review/）；A5 五项契约裁定在案

## 增补（同日续跑：DRIFT 重基线完成，候选 v2）
- **步骤 0 已执行**：新建 `rebaseline/imboy`（detached @ `a7e19a26`，含已进 main 的 00000104），4 个后端 patch 零冲突干净应用；scratch DB 已补 104（现=1→106 全量态）。
- **陈旧 beam 发现（重要）**：原 integration 树的全量绿存在假阳性风险——roster/task 的 handler 测试 beam 编译于宏接线之前，erlang.mk 未重编，掩盖了「A1/A2 测试文件本地 ` -define` 与接线后 error_code.hrl 宏重定义」的编译错误（warnings-as-errors 下 error）。已在两树删除该 4 行冗余定义（A5 handoff 预登记的「集成轮切宏」收尾项；A0 按 A5 声明代执行，留痕本节）。
- **v2 权威候选**：`final/candidate-imboy-v2-a7e19a26.patch`（SHA-256 `d7e15133ca773d48f6c6dc98333bf6f35aaadf4249e1b2a9639c9004963afa21`，32 文件=31 候选文件+evidence 目录），基于当前 main，**从零全量重编+完整门序列 183/183 全绿**、contract-check PASS、git diff --check 0。原 integration/imboy 树（8fb7ed11 基）已被 v2 取代为参考件。
- moya 候选不受影响（其 main 未漂移），integration/moya 仍为权威。
- 重基线 worktree：`.worktree/moya-zcode-phase15/moya-zcode-20260910-181902/rebaseline/imboy`（持久保留）。

- **moya 权威交付物**：`RUN_ROOT/final/candidate-moya-v2-1a636660.patch`（SHA-256 db4aaeff…，21 文件，含 inherited_user_input 继承字节；共享 moya 树三 staged 文件原样未动）。
- **本地可执行工作已耗尽**：剩余推进全部依赖四项人工决策（见 resume-prompt 决策 A-D）。

## 增补（用户指令：review代码和文档）
独立评审报告：`RUN_ROOT/final/code-doc-review.md`（无 P0；2 P1+6 P2+正面确认清单）。
- P1-1 task_id 跨端类型断裂：后端返回 varchar（`task_…`），契约/OpenAPI/前端按数字 TSID——真实联调列表与发布必挂；建议后端改返回 bigint id（一处 SQL+一处 payload）。
- P1-2 00000105 down 缺「单视频已镜像旧列」预检，drop 表可静默丢未镜像关联；建议补第三分支 RAISE。
- 两项均为报告状态，未擅改候选（等用户决定是否出 follow-up patch）；文档漂移（resume-prompt 旧 SHA/文件数）已修正。
