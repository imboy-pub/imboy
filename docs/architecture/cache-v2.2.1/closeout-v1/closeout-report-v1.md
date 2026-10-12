# CACHE_V221_BACKEND_CLOSEOUT_V1 最终收口报告 V1

> task_id：CACHE_V221_BACKEND_CLOSEOUT_V1
> 报告版本：closeout-report-v1（DRAFT_SKELETON）
> 起草时间：2026-10-12（Asia/Shanghai）
> 状态：**DRAFT——骨架已填，终轮数据待协调者（A0）在 C14 签发时填充**
> 绑定：收口计划 `backend-closeout-plan-v1.md`（SHA-256 见 baseline.json）；
> 原计划 `w0/plan-v2.2.1.md` SHA-256 `a5523ee7...c713`；
> 历史运行根 `.Codex/runs/CACHE_V2_2_1_W1_W5_LOCAL_IMPLEMENTATION/zcode-20261010T160733Z/`；
> 本轮证据根 `~/.codex/cache-closeout-runs/zcode-20261011T130725Z/`（仓外 raw/logs）。

## 0. 阅读约定与填充状态

- **⏳ 标记 = IN_PROGRESS / 待协调者最终确认填充**。未标 ⏳ 的内容为已定稿事实
  （取自仓内 closeout-v1/ 文档与 git 只读取证，起草时点成立）。
- 三个结论**并列签发、不得互相推导**（收口计划 §1："本地工程可以闭合而原计划仍为
  BLOCKED。必须并列报告，不得换名称掩盖未完成"）。
- 本报告不覆写历史报告；`w1-w5-report-20261011.md` 等保留历史，本报告是本轮唯一
  状态索引（C14 要求）。

填充状态总览（供协调者终轮核对）：

| 章节 | 状态 |
|---|---|
| §1 三结论 | 框架已填；结论一终值 ⏳、结论二/三已可判定 |
| §2 提交清单 | 4 个已落地已填；计划中提交为占位 ⏳ |
| §3 台账摘要 | C00 版分类计数已填；96 项终态复算 ⏳（C14 verifier） |
| §4 证据摘要 | 仓内文档状态已填；CANDIDATE_SHA 与 C12 raw 证据 ⏳ |
| §5 回滚步骤 | 摘要已填（权威来源 rollback-v1.md）；增补待后续提交落地 ⏳ |
| §6 未闭合项 | 已从四份来源汇总；进行中项终态 ⏳ |
| §7 签发检查单 | 已列；执行 ⏳ |

---

## 1. 三个独立结论（C14 并列签发）

### 1.1 结论一：BACKEND_LOCAL_CLOSEOUT — **DONE_WITH_DOCUMENTED_EXCEPTIONS**（2026-10-12 签发）

C00～C14 全部执行完毕。候选 f1aa1ce0（git 2873dbfd）五步冻结链产品代码零漂移；
绑定绿轮 dcheck 22/22×2 + mixed 13/13×2；全量 EUnit 10762 passed / 6 failed
（6 个全部外部归属：00000150 双胞胎迁移致 inttest 临时库供应失败，按计划纪律
登记不修）；A9 独立审查 APPROVE_WITH_CONCERNS（两条非阻塞，a9-independent-review.md）。

登记的文档化例外（均不阻塞本地收口结论）：
1. 全量 EUnit exit=2——外部归属（inttest marker duplicate_versions）
2. 性能窗口 PARTIAL——计划 19.7 机器态口径（c12-perf-results.md）
3. Prometheus scrape 层消费验证 GAP_EVIDENCE——计划 C11 明文允许
4. sync 模块 1203 行超 800 行规范——收口期不动刀，登记例外
5. W0 census 静态分类器漂移（forwarded_key 断言）——工具债登记

### 1.2 结论二：原计划 LOCAL_CANDIDATE 验收 — **BLOCKED**（结构性判定，可签发）

**判定对象**：原计划（plan-v2.2.1.md）W0～W5 全部 Tasks、AC-01～30、第 19 节全部
必需门禁，含原始 SLO 合同。成立条件要求全部通过；**任何 required 非 PASS 阻止
整体 PASS**，且不得由结论一推导。

**判定**：**BLOCKED**。结构性依据（起草时已终局，不依赖 C12 结果）：

1. AC-26（单节点性能达到冻结 SLO）、AC-27（三节点同步性能及资源预算达标）、
   W0-09（SLO 冻结）为 BLOCKED_EXTERNAL——权威冻结 SLO、目标硬件容量规格、
   峰值负载业务定义在 `docs/`、`deploy/`、`imboy.wiki/`、`scripts/` 中均不存在
   权威版本（external-inputs-c13.md §2/§3 逐项取证：仅合同模板预设值、待执行
   空表、计划内候选值与部署足迹/容器上限）。按 19.7 冻结前置条件不成立。
2. AC-27 另有专属缺口：现有部署权威规格（helm `replicaCount: 1` 单副本 + TSID
   多副本约束）与三节点假设直接冲突；同机房网络带宽规格不存在。
3. 收口计划 §1 明文："没有生产规格、真实业务峰值和冻结 SLO 时，AC-26/27 保留
   BLOCKED_EXTERNAL；原计划总体保持 BLOCKED，禁止承诺无人值守必然达到 100%
   原计划验收。"

96 项台账中其余 93 项的逐项终态（PASS/FAIL 复算）仍由 C14 verifier 机械产出 ⏳；
但无论其余项结果如何，整体判定已为 BLOCKED。**本地工程闭合（结论一 PASS）不得
改写本结论**。

**结论二终值**：BLOCKED（96 项明细复算 ⏳ C14）

### 1.3 结论三：生产资格 PRODUCTION_ACCEPTANCE — **NOT_ISSUED**（本计划不签发）

**判定**：收口计划 §1 明文本计划不签发生产资格；需要目标部署环境、生产资格与
明确外向授权。external-inputs-c13.md §5 判定 AC-26/27 维持 BLOCKED_EXTERNAL：
本地 loopback 三节点结果不代表三机网络或目标硬件容量（C09 oracle 原文约束），
本机结果不得外推生产。

**解除阻塞所需外部输入**（external-inputs-c13.md §5，原文归纳）：

- 产品/运营方提供权威业务峰值模型（峰值会话/消息/附件量、缓存 Key/Value 分布
  与操作速率）；
- 部署决策方冻结目标硬件与集群拓扑（核数/内存/磁盘/带宽、节点数——三节点方案
  需先解决单副本 TSID 约束）；
- 据此经 19.7 冻结程序产出绝对 SLO 与 headroom 后，在对应环境执行 AC-26/27。

**结论三终值**：NOT_ISSUED / BLOCKED_EXTERNAL（终态，无 ⏳ 项）

---

## 2. 本地提交清单

### 2.1 已落地提交（4 个，均未 push——`git branch -r --contains` 为空已核实）

| 提交 | 日期（+0800） | 主题 | 覆盖任务 | 文件面要点 |
|---|---|---|---|---|
| `6217ec8e` | 2026-10-11 21:17 | docs(cache): C00/C01 closeout baseline, ledger and frozen protocol/budget contract | C00/C01 | closeout-v1/ 下 7 个 json/md |
| `7db4e0af` | 2026-10-11 23:57 | feat(cache): C02-C05 sync protocol closeout | C02～C05（含 C07 接入） | imboy_cache_policy / imboy_cache_sync / imboy_cache_sync_proto（新）/ imboy_cache_sync_snapshot（新）/ imboy_cache_sync_tests + protocol-budget-frozen.md |
| `e001fb8e` | 2026-10-11 23:58 | fix(account): G-01 atomic ID claim via dedicated allocator process | C06（G-01） | account_ds（-45 行）/ imboy_account_allocator（新 138 行）/ account_ds_tests |
| `675b0744` | 2026-10-11 23:58 | feat(qr): G-QR atomic consumption via PG authority (closeout C08) | C08 / G-QR | 迁移 `priv/migrations/00000170_qr_login_session.{up,down}.sql` + QR handler/sse_handler/event_ds/session_repo（新 197 行）/ session_repo_tests（新 274 行） |

三代码提交文件面零重叠（rollback-v1.md §0 取证），revert 互不冲突。

### 2.2 计划中提交（占位，⏳ 终轮由协调者替换为实际 SHA）

| 占位 | 主题（暂拟） | 覆盖 | 状态 |
|---|---|---|---|
| P1 | `e903dc65` fix(cache): close out C09 three-node recovery——r2 握手帧 pid 直达（version_request 3 元组 / version_report 9 元组）、take_page 分页守卫、check_incarnation 首现 changed 语义、applied_count maps:values 潜伏 undef、dcheck C9 场景与 candidate 绑定输出、JSON 字节消毒、测试集群禁用 overlapping-partitions | C09 | **DONE**：dcheck 22/22 ×2 独立绿（raw/three_node_c09_r14/r15.json，exit 0）；绑定重跑待窗口 |
| P2 | `9ba11fd6` test(cache): C12 V2 bench driver and C10 mixed-version dcheck（M7-M10 + inject_v2_set_frozen + 真实 UTC）；旧树 worktree 3a01829d（`.ai/c10-legacy-worktree`，make app exit 0，LEGACY_EBIN=worktree 根） | C10 | **DONE**：mixed 13/13 ×2 独立绿（raw/mixed_c10_r1/r2.json，exit 0，各 310s）；绑定重跑待窗口 |
| P3 | `9f863af2` feat(cache): C11 sync observability gauges, panels and alerts——status/epoch/queue/peers/counter gauge + peer 水位系列；面板 7→12（修 4 个重复 id）；告警 +3（StuckRecovering/Degraded/SnapshotAborting）；`4a3f48b4` gauge 暴露断言测试 | C11 | **DONE**：metrics_handler_tests 7/7 绿；Prometheus scrape 层无本地运行能力 → GAP_EVIDENCE（计划 C11 明文允许） |
| P4 | `9ba11fd6` 内 scripts/cache_v2_bench.erl——V2 全链 bench（rep put S/M/L、invalidate、get、memo 回填、mixed），最小组件启动（无 PG/端口），已验证 10000 ops 完整轮（/tmp/cache_v2_bench_runs/verify_r1.json） | C12 前置 | **DONE** |
| P5 | `41aedf3c` docs(cache): closeout evidence pack——11 份文档（registry-w1 / observability / external-inputs-c13 / c10-mixed-plan+legacy-build / c12-window-plan / rollback / qr-wire-check / doc-consistency-review / evidence-index / eunit-inventory）；`4a3f48b4` 内含台账草稿 ledger-update-draft-r2.json 与合入产物 ledger-v2.json（未提交，随终稿批落地） | C14 | **DONE**（ledger-v2 合入 16 条更新；终算随 C12 刷新） |
| P6 | C12 候选终冻结 + 全量 r5 + 绑定绿轮×4 + 性能窗口×6（c12-perf-results.md） | C12/C14 | **DONE**（性能 PARTIAL 为计划内口径） |

### 2.3 HEAD 漂移事实（起草时取证）

4 个收口提交之后 main 已前进（外来无关测试修复系列，至 `626084e0` 等，非本任务
产物、未接管）。**C12 候选冻结时必须以冻结时点实际 HEAD 重新采样**，机械复核
缓存核心零漂移并记录差异 ⏳（c12-window-plan §4.2 流程）。

---

## 3. 验收台账摘要（ledger-v1.json，96 项）

来源：C00 对历史台账 86 项（77 PASS / 6 BLOCKED / 2 NOT_RUN / 1 PARTIAL）逐项
复核 + W0 阶段 10 项；ID 集与原计划机械提取的 required-ids.json（96 项）逐项
相等，alias 26 项显式登记（W4-10↔W4-010、AC-22↔AC-022 等），无重复、无缺失。

| 分类 | 计数 | 语义与关键项 | 终态归属 |
|---|---:|---|---|
| REVERIFY | 63 | 历史 PASS 默认待重新绑定最终候选（W1~W5 任务与 AC-01~16/18/21/23~25/28~29、19.3/19.4/19.5/19.7/19.9-* 等门禁） | 绑定 CANDIDATE_SHA 复验（C12/C14）⏳ |
| REVERIFY_HIST_INHERIT | 9 | W0-01~08、W0-10：W0 阶段证据、受测输入未变 → 可继承 + 抽检 | C14 抽检继承（equivalence 记录）⏳ |
| GAP_IMPLEMENTATION | 13 | W1-06、W3-04、W4-03/04/05/06/09、W5-04、AC-20、AC-22、G-QR、19.6-有界恢复、19.8-混合可观测 | 实施已由 `7db4e0af`/`e001fb8e`/`675b0744` 闭合，待 C09/C12/C14 行为级复验 ⏳ |
| GAP_EVIDENCE | 7 | W4-11（故障矩阵缺口）、W5-02（三节点完整 SET）、W5-07（性能归因 PARTIAL）、W5-10（全量 Failed 4/exit 非零）、AC-17、AC-19、AC-30（全量 exit=0 未达成） | 待 C09/C12 新证据 ⏳ |
| NOT_RUN_CARRIED | 1 | W4-10：缓存性能压测历史 NOT_RUN，旧数据不可继承 | 待 C12 新 V2 路径性能 ⏳ |
| BLOCKED_EXTERNAL | 3 | W0-09、AC-26、AC-27（生产 SLO/硬件/峰值输入缺失） | C13 判定维持，本地不可闭合（终态） |

合计 96。分类规则与逐条 reason 见 ledger-v1.json / review-ledger-v1.md
（C00 版，不覆写）。

**C14 终轮复算要求**：每项新 PASS 需 candidate、输入摘要、命令、exit、oracle、
次数/计数、证据路径/sha256；verifier 拒绝空证据、过期 SHA、1970 时间戳、
混用候选、伪 PASS；历史全量不得继承为新候选全量。终轮 96 项逐项 PASS/FAIL
表与本节计数并置 ⏳。

---

## 4. 证据摘要

### 4.1 仓内证据（docs/architecture/cache-v2.2.1/closeout-v1/）

| 文档 | 任务 | 状态 |
|---|---|---|
| baseline.json | C00 | 已定稿（提交 6217ec8e） |
| required-ids.json | C00 | 已定稿（6217ec8e） |
| ownership.json / resources.json | C00 | 已定稿（6217ec8e） |
| review-ledger-v1.md | C00 | 已定稿（6217ec8e） |
| ledger-v1.json | C00 | C00 版已定稿；顶层候选字段 + 96 项终态复算待 C12/C14 ⏳ |
| protocol-budget-frozen.md | C01 | 冻结版（budget_v1 摘要 + r1/r2 修订正文）；工作树已修改，随 P1 提交落定 ⏳ |
| registry-w1-closeout.md | C06 | 已定稿（未跟踪，随 P5 提交 ⏳）：5 尾巴项 = 1 FIXED-with-test / 2 Documented-Exception / 2 BLOCKED |
| qr-wire-contract-check.md | C08 | 已定稿（未跟踪 ⏳）：MATCH 14 / TOLERATED 6 / BREAK 0 |
| c10-mixed-plan.md | C10 | 计划定稿（未跟踪 ⏳）；M7-M10 编译验证记录在案，执行证据待窗口 ⏳ |
| observability-check.md | C11 | 静态核查定稿（未跟踪 ⏳）；gauge/面板/告警终态待确认 ⏳ |
| external-inputs-c13.md | C13 | 已定稿（未跟踪 ⏳）：AC-26/27 维持 BLOCKED_EXTERNAL |
| c12-window-plan.md | C12 | 计划定稿（未跟踪 ⏳）；执行证据待窗口 ⏳ |
| rollback-v1.md | 回滚 | 已定稿（未跟踪 ⏳，覆盖 4 已落地提交）；后续提交落地后需增补 ⏳ |
| closeout-report-v1.md | C14 | 本报告（骨架 ⏳，随 P5 提交） |

历史证据（保留不覆写）：`w0/`（REVERIFY_HIST_INHERIT 9 项的继承源）、
`w1-w5-report-20261011.md`、`w1-w5-addendum-20261011.md`、
`.Codex/runs/.../zcode-20261010T160733Z/evidence/ledger.json`（历史台账 86 项）。

### 4.2 仓外证据（~/.codex/cache-closeout-runs/zcode-20261011T130725Z/）

| 证据 | 任务 | 状态 |
|---|---|---|
| raw/three_node_c09_r{1,2,3,4,6,7}.json + logs/dcheck_r1..r10.log | C09 | 进行中：r7 仍有失败（F5/C9a/C9d/C9e/C9f）；修复后 19/22 ⏳；终轮需全部 required 场景 ≥2 轮独立通过且补 `candidate_sha256` 绑定（verifier 硬要求）⏳ |
| raw/mixed_c12_r{1,2}.json + logs/mixed_c12_*.log | C10/C12 | 待窗口 ⏳（13 场景两轮） |
| raw/candidate_freeze.json（CANDIDATE_SHA256=f1aa1ce0…=sha256(git archive 2873dbfd)；五步演进链全记录） | C12 | 终冻结（r5，2026-10-12T04:00Z 前后） |
| logs/eunit_full_c12_r1.log + raw/eunit_full_c12_r1.json | C12 | 待窗口 ⏳（要求 exit=0、Failed=0） |
| logs/qr_c12_*_r1.log + raw/qr_c12_r1.json | C12 | 待窗口 ⏳（4 套件） |
| raw/perf_c12_{env,base,cand}_r*/ + raw/perf_c12_r*.json | C12 | 待窗口 ⏳（env 锚 1 轮 + 基线/候选交错 ≥3 组；V2 bench 驱动验证中 ⏳） |

**CANDIDATE_SHA256（r5 终冻结）**：`f1aa1ce016eaa6e4db18e52e6ee6fba51cdb745303c057aaeff650c9b990020c`（= SHA-256 of `git archive 2873dbfd67`；候选演进链 41aedf3c→c4012a88→dbadd5f29→cae95a6d→2873dbfd，全部为测试/清单/驱动修复，产品代码零漂移，supersedes 链与全值在 candidate_freeze.json）。冻结后必须与本报告、台账、全部 raw 证据
一致——verifier 机械校验）。

---

## 5. 回滚步骤（摘要）

权威来源：`rollback-v1.md`（本节仅为索引，执行以该手册为准；手册只描述步骤，
不含任何已执行动作；执行者须对目标环境有写权限并自行确认）。

### 路径 A：全量回滚（revert 三个代码提交，6217ec8e 纯文档保留）

1. 前置：工作树中与 `7db4e0af` 文件面重叠的未提交改动
   （imboy_cache_sync.erl / imboy_cache_sync_proto.erl /
   imboy_cache_sync_tests.erl / protocol-budget-frozen.md）**必须先提交或 stash**，
   否则 revert 被拒。
2. 按倒序逐个 revert，每步一个独立原子提交：
   `git revert 675b0744`（G-QR）→ `git revert e001fb8e`（G-01）→
   `git revert 7db4e0af`（C02-C05）。
3. 各步验证（make compile + 对应套件绿名单见 rollback-v1.md §1.1~§1.3）；
   多节点部署必须全集群同版本回退，禁止混跑（`ImBoyCacheSyncUnknownEvents`
   告警即混跑信号）。
4. deploy/config 零联动（三重取证：指标名父提交已存在 / env 集合零变化 /
   C03 预算为编译期 `-define` 常量）。
5. 接受的风险：G-01 并发 ID 缺陷重现；C02-C05 协议加固消失。
6. DB：生产未 apply 00000170，无需动作；已 apply 的环境按 §2.1 顺序补 down。

### 路径 B：单特性回滚（仅 G-QR，含 DB down）

**顺序强制，不可颠倒**：env `qr_login_authority` 切回 cache（如有环境设过 pg）
→ **代码仍在位时**执行 DB down（`erlang_migrate:down(Config, 1)`，回退 00000170
一步；或 `goto(Config, 169)`）→ `git revert 675b0744`。
先 revert 后 down 会导致 down SQL 无从读取、且 `down(Config,1)` 会误伤
00000169（erlang_migrate strict 语义取证见 rollback-v1.md §2.1）。

token 无复活路径已核对：down SQL 为单条 `DROP TABLE`，无触发器/备份表；
claim 为单语句 `UPDATE...RETURNING` 取出即置 NULL。接受的风险：cache 路径
cancel-after-confirmed 旧地雷重现。

### 增补待办 ⏳

P1~P6 计划中提交落地后，rollback-v1.md 须增补对应回滚条目（尤其 C09 的 sync
协议帧变更与 C11 的 metrics_handler/deploy 变更），并复核工作树前置条件清单。

---

## 6. 精确未闭合项清单

### 6.1 BLOCKED_EXTERNAL（外部输入缺口，本地不可闭合，终态）

| # | 项 | 缺失输入（external-inputs-c13.md） | 恢复途径 |
|---|---|---|---|
| X1 | AC-26 单节点性能达冻结 SLO | I-1 权威冻结 SLO（现仅模板/待执行空表/候选值）、I-2 目标硬件容量规格（现仅部署足迹与容器上限）、I-3 峰值负载业务定义 | 产品/运营方供峰值模型 + 部署方冻结目标机规格 → 19.7 冻结程序 → 对应环境执行 |
| X2 | AC-27 三节点同步性能及资源预算达标 | 同 X1 全部 + 三节点收敛 SLO 未冻结 + helm `replicaCount:1` 与三节点假设冲突（TSID 单副本约束）+ 同机房带宽规格不存在 | 同上，且三节点拓扑须先解决单副本 TSID 方案 |
| X3 | W0-09 SLO 冻结 | 同 X1/X2 输入 | 同上 |

### 6.2 BLOCKED 实施项（registry-w1-closeout.md，2 项，需后续实施轮）

| # | 项 | 精确阻塞原因 | 风险 |
|---|---|---|---|
| B1 | fcm OAuth token 广播：`{fcm_access_token_dyn}` 一元组键在 `namespace_of/1` 无子句 → legacy → `dsync_enabled=true` 时 set/5 仍全组广播（违反 19.5 红线）；同族 `qianfan_access_token` 裸原子键同理 | 修复需 src/ 写变更且方案二选一：(a) `namespace_of/1` 增一元组子句（policy 契约扩展，影响 registry_hash——C01 已冻结，须走冻约流程）；(b) 键形改二元组 + 兼容旧值过期；均须同步补 policy/行为测试并成组处置同族键。本轮 C06 红线只读 | 多节点 + dsync_enabled=true 部署下敏感凭据全值复制；默认配置（dsync_enabled=false）下为潜伏缺陷 |
| B2 | group join/leave 读改写非原子 + join 缓存变更位于事务内（W1-03 历史结论漏扫实证：间接调用链未扫） | 修复需两处 src/ 写变更组合：join 缓存变更移出事务（4 个事务调用方边界重构，参照 leave_execute 既有正确模式）+ 成员列表缓存原子化（invalidate+全量回填或 allocator 模式），须配 40 路并发对照回归测试 | 并发入群交错下 C2G 消息静默漏投成员（不可恢复、监控不可见）；workspace 建群部分失败回滚后脏成员 ≤1h 向无资格者投递 |

### 6.3 观测缺口（observability-check.md：A 类 3 项 + B 类 3 项）

| # | 类 | 缺口 | 状态 |
|---|---|---|---|
| O1 | A（仅 deploy 配置） | 补三条关键告警：degraded_total（critical）/ cold_recovery_total（warning）/ snapshot_abort_total（短窗 warning） | ⏳ 告警已写待终确认（C11 进行中） |
| O2 | A | 专板补面板（snapshot_abort/cold_recovery/degraded/conflict/expired/gap_detected）+ 修复 4 个 stat 面板重复 panel id | ⏳ 面板由另一 agent 补 |
| O3 | B（需代码） | status/epoch/queue_depth gauge 导出（`collect_system_metrics/0` 现不含任何 cache sync 项——"计数可导出、状态不可导出"断链） | ⏳ gauge 已写待终确认；"卡 recovering"告警为后置依赖 |
| O4 | B | peer-lag 派生指标（原料在 ETS，缺计算与导出两环；19.8 明确要求） | 未开始，登记缺口 |
| O5 | B | 19.8 口径细分：receive_total/publish_total 按 protocol（legacy/v2）拆标签；reject_total 加 reason 标签；incarnation 变化计数化；hash_divergent 状态导出 | 未开始，登记缺口 |

### 6.4 进行中项（协调者持有，⏳ 待最终确认）

| # | 项 | 当前状态 | 完成判据 |
|---|---|---|---|
| I1 | ~~C09 修复~~ → **CLOSED**：提交 `e903dc65`；22/22 ×2 绿（r14/r15）；仅剩绿轮证据补 candidate 绑定（转入 C12 窗口） | 22/22 ×2 已达成 | 绑定重跑 ⏳ |
| I2 | C11 gauge 导出与告警 | 代码已写（metrics_handler + deploy 两文件工作树已修改）⏳ | 终态确认；面板（O2）收口 |
| I3 | C12 V2 bench 驱动（A7 缺口，c12-window-plan §2.4 唯一硬前置） | `scripts/cache_v2_bench.erl` 由另一 agent 验证中 ⏳ | 驱动自测通过；否则 base/cand 臂 WAITING_RESOURCE |
| I4 | C10 真实 BEAM 混合组网执行 | 代码就绪，待授权窗口 ⏳ | 13 场景两轮零失败 |
| I5 | C12 候选冻结 + 全量 EUnit + 性能窗口 | 未执行 ⏳ | c12-window-plan §6 五步全过或如实 PARTIAL/WAITING_USER_ACTION |

### 6.5 登记例外与建议项（Documented-Exception，非阻断）

- tag_subtitle_2/3 fill/flush 键失配（TTL 1 天兜底；修复一行待后续轮）；
- config5 双重失效 7.1 残余竞态（彻底闭合依赖 fenced 启用 + 回填改造，计划门控在性能 gate 后；广播半边已由 W1-05 闭合）；
- login_attempt_ip_limit 读改写非原子（账号级原子锁定第一防线）+ 关联
  `login_security_logic.erl:160` depcache 读错存储（fail-safe 方向）；
- apns_jwt = FIXED-with-test（建议后续补 `{apns_jwt,1}` 形态直接断言）；
- 关联发现三条（registry-w1-closeout.md 末节）：一元组/裸原子键失配族登记、
  W1-03 按间接调用链口径重扫（C12/C14 复算时执行）、policy 分类测试抽样补全
  （login_attempt_ip_limit / fcm 一元组 / qianfan 裸原子）。

### 6.6 工程纪律待决项

- `imboy_cache_sync.erl` 1203 行 > 800 行规范：冻结审查前须继续最小拆分或登记
  明确例外 ⏳（禁止借机重写）；
- 机器态：uptime 长期未重启，W5-07 已证同字节构件吞吐退化 -74%~-79%——C12 性能
  窗口 env 锚对照 W0 r7/r8 判定；超出噪声带则窗口记 PARTIAL（机器态归因，哈希
  清单铁证），需用户主机重启时记 WAITING_USER_ACTION ⏳；
- C09 既有 raw 证据全部缺 `candidate_sha256` 绑定（verifier 必拒）——终轮驱动
  侧改造 + 复跑 ⏳。

---

## 7. C14 终轮签发检查单

> **签发后 HEAD 漂移注记**：签发期间并行会话向 main 插入 OA SSO 域提交（e2463757/bc4d769a/edb7973f 等）。候选绑定 f1aa1ce0 = sha256(git archive 2873dbfd) 指向特定 commit，不受 HEAD 前进影响；外来提交不触 cache 代码面（A9 红线核查覆盖 6217ec8e..HEAD 全区间 PASS）。

| # | 检查 | 责任 | 状态 |
|---|---|---|---|
| 1 | C09 两轮全绿 + P1 提交 + candidate 绑定 | A5/A0 | ⏳ |
| 2 | C10 窗口两轮零失败 + LEGACY_EBIN SHA 冻结一致 | A6/A0 | ⏳ |
| 3 | C11 终态确认（gauge/面板/告警）或缺口登记 | A8/A0 | ⏳ |
| 4 | sync 模块行数处置 | A1/A0 | ⏳ |
| 5 | A9 独立审查（协议/资源/QR/数据安全/候选绑定/全部 BLOCKED 与例外）无阻断 | A9 | ⏳ |
| 6 | C12 候选冻结 + 五步窗口（全量 exit=0 / dcheck / mixed / QR / 性能） | A0/A7 | ⏳ |
| 7 | verifier `check_cache_closeout_v1.py` 机械校验 + self-test | A8 | ⏳ |
| 8 | 台账 96 项终态复算并置本报告 §3 | A8/A0 | ⏳ |
| 9 | 三结论终值填写（§1.1 ⏳；§1.2 BLOCKED、§1.3 NOT_ISSUED 已定） | A0 | ⏳ |
| 10 | rollback-v1.md 增补计划中提交条目 | A0 | ⏳ |

> 签发纪律（收口计划 §6 C14 / §7）：任何 required 非 PASS 阻止相应整体 PASS；
> 空证据、过期 SHA、1970 时间戳一律拒绝；阻断发现由 owner 修复并重验，不能靠
> 文档改成 PASS；本地提交均不 push，push/部署/生产启用须用户明确确认。

---

*本报告为 DRAFT_SKELETON，终轮填充完成后由协调者将状态改为 FINAL 并补 CANDIDATE_SHA
与 96 项终态表；所有 ⏳ 项清零或转为明确终态（PASS/FAIL/PARTIAL/BLOCKED/WAITING_*）
后方可作为 C14 签发依据。*

---

## 附录 B：签发后登记项翻案（2026-10-12，收尾后核验）

例外 1（全量 EUnit exit=2，inttest marker duplicate_versions 外部归属）**已消解**：

- 根因修正：并非两份**已提交**的 00000150 孪生迁移（git 全历史仅
  1f293973 一份 `00000150_enterprise_webhook_delivery_keyset`；deorg 迁移以
  `00000171` 随 bc4d769a 落库，从无 00000150_organization）。r5 运行时
  `priv/migrations/` 内存在并行会话**未提交**的同号迁移 WIP，机械扫描
  （版本号去重，与 `erlang_migrate_source:check_duplicates/1` 同谓词）当时命中，
  现为零重复。
- 活体验证：恢复 closeout 测试环境（imboy_pg18:4323 + pg_relay）后重跑
  r5 曾失败套件 `enterprise_human_file_binding_pg_checks`——**PASS**
  （provision_fresh 严格迁移全绿，2026-10-12 本地）。
- 处置：本项从「外部归属登记不修」改为 **RESOLVED（外来 WIP 撞号，随并行会话
  提交自愈，无需本任务修复）**。其余例外 2～5 状态不变。
- 例外 5 同日翻案 **RESOLVED**：根因是 W0 census 的 INFRA 集合只含
  `imboy_cache`/`imboy_cache_sync` 两文件，F5 重构后 `imboy_cache_sync_snapshot`
  直连 depcache 的两处机制自用（page_entry 读/apply_entry 写）落入
  EXCEPTION_UNRESOLVED。已将集合提为 `INFRA_PATHS` 常量并覆盖整个 V2 同步子系统
  （commit 6d4393f0）；scan_artifacts 全绿（543 calls，34 infrastructure，0 未归类），
  snapshot 不泄漏进 mutation/write sites，registry 46 行不变。
- 例外 3 同日闭合 **RESOLVED**（GAP_EVIDENCE → 消费链本地取证）：C11 审计时的缺口
  是"无本地运行能力、核对全为静态"。现已在本地闭合消费链各环：
  ① `promtool check rules` SUCCESS（62 rules）；② 新增规则单测
  `deploy/prometheus/rules/imboy-alerts-test.yml`（promtool test rules SUCCESS：
  5 条告警的不触发边界 + 触发表达式）；③ 活体 Prometheus 抓取 handler 同源渲染
  fixture（format_prometheus/1 真实路径 + 后端同款 Content-Type）：up=1、
  `imboy_cache_sync_status=-1` 入库、5 条规则 health=ok 且 severity/team 接线正确、
  ImBoyCacheSyncDegraded 真实 firing（status=-1 语义正确触发）。
  证据：`<RUN_ROOT>/obs-scrape-evidence/evidence.json`。部署栈内端到端抓取仍属
  部署期验证（residual 如实保留）。
- 例外 4 同日动刀 **RESOLVED**（收口期结束，用户授权后重构）：`imboy_cache_sync.erl`
  1207 行拆为零行为变更三模块——`imboy_cache_sync_apply`（245 行，V2 事件应用 +
  C03 预算 + C05 补偿）、`imboy_cache_sync_recover`（285 行，恢复引擎/快照回合/
  校对清扫）、主文件 712 行（公共 API + gen_server 串行化 + generation 栅栏 +
  发布路径）；共享 `#state{}`/恢复预算 define 移入 `imboy_cache_sync_state.hrl`；
  同步族内部合同以显式 export 块约定。验证：erlfmt/check_module_boundaries/
  arch-check 全过；eunit imboy_cache_sync_tests 29/29 + imboy_cache_tests 11/11 +
  metrics_handler_tests 7/7；census INFRA_PATHS 随新文件更新后 scan 仍全绿
  （commit bc39858b）。**残留**：全量 eunit 确认待安静共享树窗口（并行会话活跃，
  遵守互斥纪律）。
