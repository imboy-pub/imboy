# CACHE_V221_BACKEND_CLOSEOUT_V1 最终收口报告 V1

> task_id：CACHE_V221_BACKEND_CLOSEOUT_V1
> 报告版本：closeout-report-v1（DRAFT_SKELETON）
> 起草时间：2026-10-12（Asia/Shanghai）
> 当前工作树状态：**PARTIAL——旧候选结论不适用于后续未冻结的实现变更**。
> 以下 f1aa1ce0 / git 2873dbfd 及其签发数据作为历史候选记录保留。
> 绑定：收口计划 `backend-closeout-plan-v1.md`（SHA-256 见 baseline.json）；
> 原计划 `w0/plan-v2.2.1.md` SHA-256 `a5523ee7...c713`；
> 历史运行根 `.Codex/runs/CACHE_V2_2_1_W1_W5_LOCAL_IMPLEMENTATION/zcode-20261010T160733Z/`；
> 本轮证据根 `~/.codex/cache-closeout-runs/zcode-20261011T130725Z/`（仓外 raw/logs）。

## 0. 阅读约定与填充状态

### 会话交接与补丁归档（2026-10-14）

用户要求提交、合入 main 并清理本会话 worktree。以下复验段落记录的是当时 WIP，
两个失败测试文件现已按字节封存于仓外 `cache-review-20261014/`，从本任务 worktree
恢复至已提交基线；失败补丁不合入 main。已提交文档与此前通过提交进入本次合并。
接续指令见 [zcode-handoff-20261014.md](zcode-handoff-20261014.md)，其中额外两轮
授权仅在用户将指令发送给 ZCODE 后生效。证据传输包不含专属库凭据或运行配置。

### 当前复验与停止点（2026-10-14）

#### ZCODE 接续轮更新（2026-10-14 下午）

接续执行 zcode-handoff-20261014。六项失败逐项定性并修复（额外两轮窗口）：
F1 same_epoch=fixture 缺真实 imboy_cache 读服务（tombstone 模式进程补齐）；
F2 delete_event=断言违反"相同版本内容核对/同版本不续期"冻结合同（改期望
{reject, revision_content_conflict, _}，保留首次绝对期限断言）；F3/F5=fixture
未入 syn 成员视图（control/metadata fixture 补 ensure/join/leave）；F4=产品缺口
（单事件双 abort 双计 Rmax 预算，apply 侧按回合 id 判别去重）；F6=产品缺口
（coverage_candidate 缺项造零前缀、忽略错代，违反"缺项/旧格式/错代整份拒绝"）。
另修产品 verify_source 比较方向反转 bug（真回退 fail-closed；最终效果更新须有
同来源增量收据证明，manifest_merge 用例约束的"声明与证据矛盾"仍拒绝）。
共享 done/5 重建：origin_target 经 record_origin_target 独立冻结（来源自身
计数器与页前缀较大者），不再清空 retained_obligations、不再手工写目标；
回合改 recovery_round 真实 census。A9 只读复审 APPROVE（三条非阻塞备注）。

验证：七组 147/147 exit=0；11 个纯 fixture 套件 225/225 绿；make eunit-local
cache 13/13、v2 20/20、sync 29/29、capability 15/15；origin_target 7/7、
manifest_merge 18/18、snapshot_coverage 20/20（main 基线对照确认后两者原为
本改动回归、已修复）。metadata_component 两轮预算用尽：18→10/11（1 用例
accepted_snapshot_page_advances_clock 超时未闭合，登记开放项）。

本地提交 `0c441451`（分支 zcode/cache-closeout-v221-20261014，基于 main
78e800c1；main 其后漂移 2 提交均零缓存文件）。候选已冻结：
CANDIDATE_SHA256=bcbb701324832a1c6d7964fecf0edc4eb946c3937ed1fb3d6a141781f400656e
（6200 文件含 deps 全字节，frozen_at 2026-10-14T06:52:21Z，HEAD 0c441451）。
C09×2/C10×2/QR/C11/最终全量/性能七份待安静窗口串行执行。

#### C09 boot 停摆根因闭合与候选重冻结（2026-10-14 晚）

C09 两轮均在 boot_cluster:1682（wait_status(B,400)）超时。单变量 beam 二分
（main 基线 b4810720 38/38 全过为对照）：换入我方 proto（P1）单独复现超时；
换入 apply（P2）或 coverage（P3）均 38/38 全过——P1 为唯一致病改动。

根因链（coverage.erl:212 反模式点）：covered_seq 行仅在 delta 应用时诞生
（note_applied_sequence），化身登记不建行。三节点 boot 中 B/C 加入后，权威方 A
对已登记来源无行，application_coverage_candidate 按冻结合同"缺项整份拒绝"，
send_snapshot 的 cut 携带 {error, incomplete_application_coverage}（该帧仍合法，
policy 允许 error 候选）；恢复方 verify→source_prefixes 的
`{ok, Vector} = snapshot_cut_coverage(Cut)` badmatch，verify_snapshot_coverage
失败→回合中止→重试循环，B/C 永不 READY。

修复 `2c2123cb`：把合同隐含的不变量补进写侧——register_incarnation 新登记/
换 Boot 即初始化 {covered_seq,{Origin,Boot}}={Epoch,0}；bump_epoch、
adopt_epoch（reset 后）与 become_ready_state（floor 安装后）rebase 重建零前缀
行（"高代切换重置前缀，不继承旧代计数"）。读侧缺项/错代/旧格式整份拒绝原样
保留，缺行从此只可能是真异常。metadata_tests 三处断言从"无行"改为"零行"
（语义不变），缺项态改由表手术构造，另增真实登记函数维护不变量的正向断言。

验证：6 步冒烟（登记/推进/换Boot/bump/表手术缺项拒绝）全过；受影响 15 个
cache 套件 14 绿（journal_component 一次 eunit boot coordinator 崩溃为
TEST/非TEST beam 失步瞬时基础设施故障，复跑绿）；metadata_component 维持
既有开放项（修复前代码 round8 同签名 timeout 集 21/492/512/539 全在，
非本修复回归；预算已耗尽，继续登记开放）。

候选重冻结 v2：CANDIDATE_SHA256=
2e30601dcddbfd764410b7e7019e39f948b3aac36cabbde09147dce8d7a1c703
（6200 文件路径集与 v1 逐一相同、恰 4 文件内容变化=3 src+1 test，
HEAD 2c2123cb，manifest 在 runs 目录 candidate-manifest-v2.json）。
v1 bcbb7013… 作废。C09 修复轮 1（fixr1，绑定 2e30601d…）：38 场景 36 过，
boot 停摆消除；2 个 epoch 域场景失败（此前 boot 超时从未执行到，为候选
真实缺口，登记为新首要工作项）：C9k mixed epochs adopt the ready cohort
（converge_timeout，B 期望 cohort_new=fresh_era 未收敛）、unconfirmed
higher-epoch incarnation recovers through member snapshot（badmatch timeout）。
证据：raw/three_node_c09_fixr1.json + logs/dcheck_c09_fixr1.log。
剩余门禁：C09 两轮、C10×2（LEGACY_EBIN 树 904 BEAM 已就位）、QR 完整门禁+
迁移 apply/rollback 专属库、C11 消费链复验、性能七份、最终全量 eunit-local、
C14 台账终轮、三段判定报告。

#### 合并与交接（2026-10-14 晚，358a068）

分支已并入 main（358a068，门禁链 gitleaks/erlfmt/compile-dialyze/arch-check/
conventional 全绿后落提交；git 钩子上下文仍触发 darwin bash fork 崩溃 exit 138，
按既有结论属环境缺陷，缓解为手动完整跑 lefthook 后 --no-verify 提交）。
⚠️ 并集联合复验为下一会话第一要务：main 侧并行会话曾独立修复同域根因
（dd168462 tighten snapshot coverage and dcheck oracles 改 coverage/recover src
与 dcheck oracle；b0db9eae 同修 delete_event 断言），与本轮 2c2123cb 修复
自动合并为并集，两套修复从未同树验证。候选 v2（2e30601d…，绑定 2c2123cb 树）
仅对分支树成立；合并后 main 树须重新冻结 v3 再跑门禁。工作 worktree 与
zcode 分支已删除；证据包保留在 ~/.codex/cache-closeout-runs/zcode-closeout-20261014
（manifest v1/v2、refreeze_candidate.py、run_c09.sh、dcheck JSON/日志、ebin_drv）。


源码 HEAD `f34ca1ee`，worktree 为 `codex/cache-v221-perf-driver-fix`；另有本任务未提交的
`imboy_cache_entry_epoch_tests.erl` / `imboy_cache_source_cut_tests.erl` fixture 修复。
**BACKEND_LOCAL_CLOSEOUT 未通过**：C04/AC-19 fixture 修复达到 §7 自动重试上限，
该修复项记 **NO_GO**，保留失败补丁，不继续自动试改，不提交为通过实现。
原计划 LOCAL_CANDIDATE 继续 **BLOCKED**；PRODUCTION_ACCEPTANCE **NOT_ISSUED**。
这不是仅因生产输入缺失而阻塞：本地正确性、独立审查及最终证据仍有缺口。

最近两轮 fixture 修复验证分别为四组 `77 passed / 1 failed`、七组直接调用方
`141 passed / 6 failed / 0 skipped`，EUnit 原始退出均为 1。
七组为 source_cut、entry_epoch、snapshot_origin、snapshot_tombstone、invalidation、control、metadata。
六个失败用例及实际 oracle：

| 模块 / 用例 | 实际失败 |
|---|---|
| entry_epoch / same_epoch_confirmed_delta_survives_older_snapshot_and_becomes_readable | snapshot_content_conflict，发生于同版本页读取校验 |
| control / delete_event_preserves_absolute_expiry_and_rejects_invalid_expiry | revision_content_conflict |
| control / completed_authority_epoch_change_aborts_round | source_cut_identity_changed |
| control / new_epoch_event_cancels_selected_authority_before_report | rounds_used 期望 1、实际 2 |
| metadata / application_coverage_candidate_requires_every_current_identity | 期望 incomplete_application_coverage、实际返回零序列候选 |
| metadata / cold_recovery_rejects_older_epoch_authority | source_cut_identity_changed |

独立审查阻断：共享 `done/5` 从收到的 manifest 推导 CutSeq，并用同一值覆盖
origin_target，缺少来源独立冻结的目标；既有 `retained_obligations := []` 也使这些
fixture 无法证明保留内容义务。恢复执行前应明确授权重开该根因修复预算，按独立
source cut/target、真实 cache 值读取及实际应用增量重建 oracle；不得弱化产品校验，
不得调整断言强行通过。其余失败须逐项定性，不能全部宣称为 fixture 问题。

独立可执行项已取得新证据：当前 QR 五个产品模块与入口 driver 隔离重编译后，
运行 `cache_v221_qr_entry_dcheck:main/1,2`，专属库 `imboy_cache_closeout_v1`
中的 status/status、SSE/status、SSE/SSE 三种真实双节点 PG 竞争均通过，exit=0，
3 passed / 0 failed / 0 skipped，每场景仅一个消费者取得凭据，合成行清理后为 0。
Cowboy 运输层仍是 observer；这不是 HTTP 端到端、迁移回滚或最终候选验收。
当前 worktree 缺少专属库配置，本次显式使用 main 的既有专属配置，未打印凭据。

C10 旧运行树 `/tmp/cache-v221-legacy-source.7cvuC3` 已从固定提交
`3a01829d32c94bdb15f80150445d0c1b71b88a8b` 恢复；cache/sync/metric/log 由旧源码
编译，syn/depcache 为独立副本。旧版服务启动/加载路径检查 exit=0、V2 proto 不存在。
仅覆盖混合测试所需局部运行树，不是历史 904 模块全应用重建或混合矩阵通过。

`python3 scripts/check_cache_closeout_v1.py --all` 实际 exit=1，2/6 通过；candidate、
exits、utc、receipts 未通过。最终候选未冻结，compile/targeted/three_node/mixed/
rollback/qr/full_eunit/performance/independent_review 九类最终回执仍不齐全。
C09/C10 最终两轮矩阵、C11 冻结后消费复验、QR 完整门禁、最终全量与正式性能不能
越过当前 C04 阻断签发 PASS。后续运行前须重新采样资源；旧全量 PID 72900/97234
已结束，不能继续将其记录为活动租约。

原始日志、QR 结果、未验证 patch 及 SHA256 清单保留于仓外
`~/.codex/cache-closeout-runs/cache-review-20261014/`：

- `epoch-fix-all.log`：`49c0f56af659ba7272d5d8ec05bbad2b363af4523f5246123a52fbdd43ba1e0b`
- `c14-all.log`：`6a239c8a8d7b49b10959fdf94fcfd04397768298e649fa85f478ad76ac77c4f0`
- `qr-entry.json`：`1bfc7b678600db56e63a8e698f6ef1ca1cf4b90db22922aa653e133aa1290d44`

### 当前候选适用范围（2026-10-13）

当前实现已改变协议能力摘要、恢复屏障、资源预算及业务失效路径，没有完成新的
C12 候选冻结和 C14 全项证据绑定。历史 96 项台账中的 `final_evidence_candidate`
只证明其标注的旧候选；`classes` 是明细分类计数，不是 PASS 计数。
当前后端收口结论为 **PARTIAL**：C04/AC-19 已补逐来源完整证书、保留内容义务、
连续增量收据及最终 READY 原子屏障，Registry 失效钩子已补实现与定向回归；
最终候选三节点、混合/回滚、全量及性能门禁仍未完成，不能仅归因于生产输入缺失。
超窗新 epoch 缺少同 epoch READY 权威的可用性问题独立登记；安全降级不能替代恢复通过。
原计划总体资格仍未达成，生产资格仍未签发。当前协议行为与限制以
`protocol-budget-frozen.md` 为准；本地 loopback 场景不能外推三机容量或生产 SLO。

C04 历史接收端边界曾由受控探针核实：冻结 A7/B1，先完成 A 的已知来源
`with_origin(A, BootA, 7)` live 页，再请求并完成 B 的空页集；B cut 候选
覆盖 A7/B1。修复前 `maybe_complete_round` 进入 READY，A 页的值仍存在。
该结果证明候选向量没有缺席裁决能力，不能直接升级为完整内容证明。
探针使用合成身份、手工序列和 depcache 替身；未证明发送端自然产生该历史，
也不是生产复活或 C12 验收证据。当前实现按上述义务拒绝证据不足的回合；
完整真实历史仍需最终三节点验证，禁止只合并 prefix 或将任意空快照当删除。

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

### 1.1 历史候选结论：BACKEND_LOCAL_CLOSEOUT — **DONE_WITH_DOCUMENTED_EXCEPTIONS**（2026-10-12 签发；不适用于当前工作树）

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

- tag_subtitle_2/3 fill/flush 键失配已修复（2026-10-13）：失效对齐实际填充键；
  真实本地缓存回归验证整数/二进制 TagId 失效后重新查询权威。PG 事务、跨节点
  通知与并发 loader 不在本项证据范围内，详见 registry-w1-closeout.md §1；
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
- 例外 4 全量确认已完成（登记项闭环）：安静窗口全量 eunit（10555 通过 / 207 失败）
  中 **cache 域 0 失败**；207 失败全部归因外部——另一会话于 14:30 前后重启 Docker，
  端口转发断裂致 PG 中继批量 `closed`（69 × marker_db_connect_failed）及夹具连锁
  （33 × already_started 等），失败首时与重启时点吻合（用户亲证死因）。cache 三套件
  隔离 47/47 + 全量零失败双证，重构零回归成立。受影响非 cache 套件的复跑待
  imboy_pg18 容器随其编排恢复后进行（日志：logs/eunit_full_postrefactor.log）。

## 第 4 轮（ZCODE 2026-10-15）：合并树 C10 混合矩阵首次真实执行与三层恢复修复

### 背景与新增证据

上一轮遗留的"main 侧 dd168462/b0db9eae 与我方 0c441451/2c2123cb 平行修复、
合并树从未验证"在本轮验证：从 main(0d28ad4f) 建分支
`zcode/cache-closeout-v3-20261014`，先落盘 64dccdf2（covered_seq 重置后
rebase 零下限行，修 C09 双 epoch 场景），15 套件 14 绿 + metadata_component
既有例外，随后进入 C10。

C10 旧基线按 handoff 第 8 条以 `3a01829d`（INTEGRATION_BASE）重建 worktree
+make 构建（旧 /tmp 树 deps 残缺不可继承；首轮"缝合怪"结果无效留档
raw/mixed_c10_v3r1.json）。基线归属判定：`0d28ad4f` 树同版 driver 跑出与
v3 树完全一致的失败集合（8/16 挂，raw/mixed_c10_base1.json）——失败为合并
树固有，非修复引入。C10 16 场景此前仅有"erlc 编译通过"记录
（c10-mixed-plan.md CODE_UPDATED_COMPILE_VERIFIED，"按红线未运行"），
本轮为首次真实执行。

### 三层恢复修复（cf3f1196）与回归修复（efc92a3c）

16 场景 8 挂 → 三层修复 → 12/16（r1/r2/newb 三轮失败集合一致）：

1. **发现屏障误纳 V1 对端**：发现回合把 syn 全员纳入 version_request
   应答义务，而 V1（3a01829d）无该协议、静默丢弃 → 回合卡至 Dmax →
   DEGRADED（M3/M5/U1 时机）。修复：发现屏障与 sources_current 成员
   一致性只覆盖声明 cache_protocol=2 的成员（新增
   `imboy_cache_sync:scope_members_by_capability/0` 一次读取分区）；
   仅存 legacy 对端时空成员恢复轮循环至 Dmax → DEGRADED（M4 fail-closed
   oracle 不变）。
2. **excluded 对端合法性缺"自报未 READY"分支**：升级/恢复中的非空同代
   对端（按合同不提供快照）被 sources_current 与 coverage:verify_target
   判 peer_content_unproven → 0ms 轮×3 → DEGRADED（103ms 快速降级，
   M7 heal 链）。修复：{false,false} 分支记录 `reported_ready`，两处
   合法性判定补 `not reported_ready` 放行；未记录时默认已 READY，
   fail-closed 严格性不变。
3. **cf3f1196 回归（efc92a3c 修复）**：sources_current 成员一致性整体
   切新分区函数连带改变 reported/source pid 资格语义（本地来源
   source_pid=self() 不再命中成员视图），
   simultaneously_recovering_peers_use_ready_authority 挂。资格检查恢复
   沿用 syn_scope_members 全体（含本节点进程）；V2 屏障过滤保持新函数。

验证：invalidation 26/26、snapshot_coverage 20/20、sync 29/29、capability
15/15、metadata 22/22、journal 全过、origin_target 7/7、source_cut 15/15、
entry_epoch 28/28、manifest_merge 18/18、snapshot_origin 14/14、control 全过、
v2/cache 全过；metadata_component 10 过/8 超时（既有登记例外，签名一致）。

### C10 定版：12/16 + 四个开放项（三轮一致）

- **M5**（尾部集群，每轮固定挂）：场景-合同冲突。f93a842a 将 M4 改为
  "破坏所有新版内存副本"（peer_sync_down 杀 sync+depcache），M5 的
  "restores READY" oracle 在 B/C 副本全灭 + 无 READY authority 拓扑下
  不可达；冻结合同要求该态 DEGRADED（"多节点配置但全部 peer 无应答/全
  recovering → DEGRADED"）。修复需协议级"幸存副本自证恢复"能力或场景
  重新设计，超出本轮预算。
- **M8/M9/M10**（主集群，每轮固定挂）：M7 gap 恢复后新键 V2 事件 apply
  成功（trace 实证 apply_v2_validated RET ok、rev 行 live、seq 连续推进、
  B status=ready）但 `imboy_cache:get` 返回 undefined——恢复后副本物理值
  与读屏障不一致，非确定性复现（与 M7 恢复拉取的快照来源相关，从 A 拉
  快照的窗口触发）。诊断证据：logs/diag_m8.log + /tmp/c10_apply_*.log
  （会话内）。
- 失败集合三轮逐字节一致：raw/mixed_c10_v3r1_fix5.json、
  raw/mixed_c10_v3r2_fix5.json、raw/mixed_c10_newb_r1.json。

### C09 新候选绑定双绿 + QR PASS

- C09：`newb_r1`/`newb_r2` 38/38×2（绑定 b0f62c81，含 cf3f1196；恢复屏障
  修复对纯 V2 零回归）。
- QR：3/3 PASS，rows_remaining=0（raw/qr_v3.json）。

### 迁移 apply/rollback：环境阻塞登记

专属库 imboy_cache_closeout_v1 会话中途被外部会话移到 ver=2 dirty（已按
先例从模板重建回 179 clean）；模板 imboy_v1 同时被其他会话占用（克隆窗口
不一致，两次克隆分别得到 121 dirty / 179 clean）；165 干净基座纯 up 在 166
失败——与既有结论一致（部分迁移依赖 app 侧函数，纯迁移不可建）。
apply/rollback 往返需独占干净 179 库或 app 配合环境，登记为环境阻塞开放项。

### 候选冻结链（本轮三次重冻）

a0ea3a69（64dccdf2 后，8745 文件）→ b0f62c81（cf3f1196 后，8746 文件）→
**9b9ed755f8e5879ee12c55a9d78d3e60ca625e9df275224470f747b66e3c67ae**
（efc92a3c 后，8748 文件，head=efc92a3c36077faf601d7462a69f92f0d4060216，
candidate-manifest-v3.json）。C09×2/C10/QR 绑定 b0f62c81（efc92a3c 仅改
成员资格检查语义，全量 15 套件在其后全绿复核）；9b9ed755 为最终冻结。

### 性能七臂：本轮未完成（登记）

七臂回执未产出（会话窗口内被收尾指令打断，性能互斥窗口让位于合并）。
开放：绑定 9b9ed755 的 env_r1/base×3/cand×3 七份回执待补。

### 本轮后未闭合集合（承接上一轮口径增量更新）

1. C10 M8/M9/M10：恢复后副本读屏障不一致（修复窗口未动，证据链齐）。
2. C10 M5：场景-合同冲突（f93a842a 遗留）。
3. 迁移 apply/rollback 往返：环境阻塞（共享 PG 4323 外部占用）。
4. 性能七臂：未产出。
5. PRODUCTION_ACCEPTANCE：NOT_ISSUED（不变）。
