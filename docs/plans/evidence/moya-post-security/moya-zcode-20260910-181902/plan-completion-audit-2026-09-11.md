# 计划完成度复核（Round 11 / 合并后终态）— RUN moya-zcode-20260910-181902

> 复核对象：`docs/plans/2026-09-10-moya-post-security-product-plan.md`（SHA-256 `b1c9f005…c81c9b1`，与授权值逐字节一致）
> 复核方式：第一手重跑门命令 + 逐卡比对证据 + 提交成分与身份核验；未采信台账声明
> 复核时点：2026-09-11；imboy HEAD=`677b5789`、moya HEAD=`49f526b`（均未 push）

## 1. 结论

计划的本地可执行部分（Phase 0 / 1 / 1.5）**实质完成并已合入两仓 main**；Phase 2A / 2B / 3 / 4 按 Gate 设计停在用户拥有的外部授权门，为正确结束态。

按 17 张本地验收卡计：**16 张有卡有证据且实测通过，1 张（`MN-UIAPI-01`）从未建卡**。台账的“本地全绿”声明在**三处被乐观化**（第 4 节 A1/A2/A3），另有两条既有红门（第 4 节 B1/B2）虽非本 RUN 引入，但会干扰续跑判断。

## 2. 第一手实测（合并后 HEAD，2026-09-11）

| 门 | 命令 | 实测 | 与台账声明 |
|---|---|---|---|
| 编译 | `make compile` | exit 0 | 一致 |
| 后端套件 | 11 个 `make eunit-local t=<suite>` | **197 项全过，0 失败** | 台账记 183/183，计数未对账（第 4 节 C2） |
| 迁移门 | `make migrations-check` | exit 0（210 文件 / 105 组 up-down） | 一致 |
| 安全门 | `make security-gate` | exit 0（零密码学 + 14 handler 边界） | 一致 |
| 格式门 | `make format-check` | **exit 2** | 未声明（见 B1） |
| 契约门 | `make contract-check` | **exit 1（漂移 30 新增 / 18 移除）** | 与“contract-check PASS”表述冲突（见 A1） |
| moya 四件套 | `typecheck` / `lint` / `test` / `build` | 全 exit 0；lint 2 warning；**tests 169/169（54 套件）**；build 58 静态文件 | 台账记 168/168（C2） |
| moya 扫描 | `npm run scan` | exit 1，命中 1 处 = 用户暂存的 `project.config.json` | 一致（决策 C，未处置） |

套件明细：`teaching_ai_provider` 25、`teaching_ai_worker` 20、`teaching_attach_logic` 46、`teaching_attach_integration` 9、`teaching_flow_integration` 11、`moya_teaching_migration` 26、`teaching_acl` 13、`teaching_task_logic` 29、`teaching_review_logic` 4、`teaching_task_repo_integration` 10、`teaching_roster_repo_integration` 4。

代码级抽查（4 个 P0 均落地）：撤回路由 + `5481` 文案（`include/error_code.hrl:513,674`）存在；花名册仅返回 `learner_id/display_name/assignment_ready/setup_reason` 最小字段，无监护人 UID / 出生年份；作业幂等为两段式（`replay_by_key`）+ `00000106` 部分唯一索引；`00000105` 有 `attachment_id` 全表 UNIQUE、kind CHECK、单 review 单视频唯一索引，down 含三条 fail-closed 预检（含 `IS DISTINCT FROM` 镜像一致性）。`group_album` 零触碰（禁改区合规）。

## 3. 提交成分核验

| 仓库 | 提交 | 文件 | 身份 / DCO | push |
|---|---|---|---|---|
| imboy | `677b5789` | 69 = 35 代码（src/api/ds/logic/repo、test、priv/migrations 105+106、include、docs/reference）+ 34 证据 | author=committer=用户本次指定身份（人工确认），Signed-off-by 齐 | 未 push；gitcode/github/origin 领先 39 笔（gitee 领先 441、behind 1，待复核） |
| moya | `49f526b` | 21（页面/API/测试 + 用户 staged 的 submission-detail 并入） | 同上 | 未 push；**该仓无任何 remote 配置**，授权也无处可推 |

moya 工作区保留用户暂存态（`project.config.json`、`tests/identity-picker-page.test.ts`），未被本 RUN 改写或取消暂存。

## 4. 偏差清单

### A. 台账声明与实测不符（需收口）

- **A1 契约门口径混用**：台账与提交信息中的 “contract-check PASS” 指本 RUN 自带的 `contract-check.py`（仅校验 `openapi/moya-teaching.yaml` 的 6 项结构/红线），**不是**仓内 CI 同名的 `make contract-check`。后者在合并后 HEAD 上实测 exit 1。用临时探针树（不落仓）在三个提交上重导出比对归因：基线 `a7e19a26` 与 `c6f5bed4` 已红（28 新增 / 18 移除，其中 14 条 teaching）；`677b5789` 为 30 / 18，teaching 新增 16。`.contract/api_contract.json` 最后提交为 2026-09-05（`82d43492`），main / integration / rebaseline 三棵树产物 SHA-256 完全相同（`8677e25e…`）→ 从未重导出。本 RUN 更新了人读 `docs/reference/rest-api-v1-catalog.md`（+15 行 / 3 端点），未更新机读产物。漂移另含其他会话的 appeal / bot / mcp 路由，以及已从源码移除但仍在产物中的 moment 路由。
- **A2 `MN-UIAPI-01` 从未建卡**：计划 §4 Phase 1.5 首张验收卡（要求 §9 每页绑定状态、前端函数、REST API、后端入口、失败恢复与证据级别；证据 `ui-api-matrix.md` 或 §9 当前 HEAD 复核）在 `board.json`、`acceptance-matrix.md` 与整个 RUN_ROOT 中零命中。另 9 张 Phase 1.5 卡均有卡有证据。台账额外含 5 张计划外卡（SECURITY-QUEUE / MN-CONTRACT-01 / BACKEND-BUILD / MOYA-CHECK / PHASE-4-DECISION），属增补而非替代。
- **A3 仓内终态记录陈旧**：入库的 `final-report.md` 仍为 pre-merge 口径——v3 imboy 记为 67 文件 / `05a3f501…`（终版实为 69 文件 / `1897b313…`），结论行仍为 `LOCAL_ALL_PASS_PENDING_RE_REVIEW`，未记录已合入 main，且同一 v3 说明段重复两遍；`acceptance-matrix.md` 为 Round 4 之前的历史快照（表头已正确降级，但卡证据锚点仍是 v2 patch hash）。真正记录终态的 `board.merged` / `resume-prompt.md` 仅存在于 `/private/tmp/...`，tmp 清理即丢失；且 `control/board.json` 含用户邮箱，**不得原样入库**。

### B. 既有红门（非本 RUN 引入，但影响“本地全绿”表述）

- **B1 `make format-check` 红**：命中 `include/imboy_frame.hrl`、`include/imboy_const.hrl`、`src/imboy_pb.erl` 三个既有文件，均不在本次 69 文件内（lefthook 只查 staged 文件，故提交时未拦截）。
- **B2 契约门基线已红**：见 A1，本 RUN 的贡献是把 teaching 漂移由 14 条增至 16 条，非红的成因。

### C. 低severity

- **C1 路径参数名与计划字面不一致**：计划写 `GET /api/v1/teaching/classes/:group_id/learners`，实现与 catalog 均为 `:id`；payload 仍按计划返回 `group_id`。
- **C2 计数未对账**：台账 imboy 183/183、moya 168/168；实测 197/197、169/169。数量更大且零失败，方向有利，但口径应复核。
- **C3 §5 证据目录规范未完全满足**：仓内证据目录（34 文件）缺根级 `ownership.md`（等价物为 tmp 的 `control/ownership.json`）、`acceptance-ledger.md`（`MN-BASE-02` 声明的证据名）、`commands.tsv`（仅存在于 `agents/*/**/commands.tsv` 13 份，未汇总）；`device/`、`model/`、`pilot/` 的授权清单只存在于 tmp，未入库。
- **C4 双台账口径冲突**：`phase-state.json` 仍为 `PHASE_1_5=PARTIAL` + “待 v3 follow-up patch”，与 `board.json` 的 v3 GO + merged 冲突（同一真源两个结论）。

## 5. 遗留外部门（均需用户决定，互不阻塞）

| 决策 | 内容 | 影响 |
|---|---|---|
| A | Phase 2A 资源：微信测试号 / 两台真机 / 隔离后端 / 活 Garage bucket | `PARENT-03`、`TEACHER-03`、`DEVICE-01`、`MN-GARAGE-01` |
| B | Phase 2B：provider + model + 费用上限 + 凭据位置 + 20 份授权样本 | `MN-AI-01..05` |
| C | `project.config.json` 现实 AppID 归属与发布策略 | 共享 moya `scan` 门能否转绿 |
| D | push 授权（合并已完成，gitcode/github/origin 领先 39 笔） | 交付物出本机 |

待用户拍板的收口动作（本 RUN 不自行执行）：① `make contract-export` 并提交 `.contract/api_contract.json`（会一并带上其他会话的漂移，属跨会话变更）；② 将 tmp 终态记录脱敏（去邮箱）后导入本证据目录，消除 tmp 丢失风险。

## 6. 复核过程自陈

重跑 `npm run scan` 时该命令把真实 AppID 值打印进本次会话输出，与计划“不得打印具体值”的约束相悖。该值未写入任何仓库或证据文件，本文件亦不转述；后续复核 scan 应加输出过滤。
