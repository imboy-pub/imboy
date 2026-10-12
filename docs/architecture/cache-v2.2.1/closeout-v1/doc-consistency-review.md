# closeout-v1 文档族交叉一致性审查 — doc-consistency-review.md

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 收口尾声 / 文档族交叉一致性审查
> 审查时间：2026-10-12（Asia/Shanghai，约 09:20–09:45）
> 性质：只读取证；本文件为唯一交付物，未修改任何被审文件，未执行任何 git 操作。
> 审查方法：全量通读 + 机器比对（python3 对 required-ids.json / ledger-v1.json 做 ID
> 集合、分类计数、hist_status 分布核对）+ 源码 file:line 抽查 6 处 + 文档间 grep 交叉引用。

## 0. 审查范围与时点声明

审查对象（任务指定）：`closeout-v1/` 下 protocol-budget-frozen.md（含 r1/r2 修订段）、
ledger-v1.json、review-ledger-v1.md、required-ids.json、baseline.json、
registry-w1-closeout.md、observability-check.md、external-inputs-c13.md、
c10-mixed-plan.md、c12-window-plan.md、rollback-v1.md、qr-wire-contract-check.md、
evidence-index.md（若存在）；以及 backend-closeout-plan-v1.md 与 w0/plan-v2.2.1.md。

**时点事件**：审查开始时 `evidence-index.md` 尚不存在；审查期间（2026-10-12 09:33/09:35）
并行 agent 交付了 `evidence-index.md`，随后又出现 `closeout-report-v1.md`（09:34）与
`ledger-update-draft-r2.json`（09:39，不在指定审查范围内，仅作交叉佐证引用）。
evidence-index.md 已按最新版本纳入审查。

---

## 1. 问题清单（只记录，不修复）

### P1-1【中高】c12 §6.3 混合矩阵 PASS 判据遗漏 M7–M10 与 R1–R3

- **位置**：`c12-window-plan.md` §6.3 PASS 条款（"退出码 0；M1～M6 全场景 pass +
  candidate 绑定…"）vs `c10-mixed-plan.md` §2 场景表与 §4 结语（"预期 13 场景
  （M1-M10 + R1-R3）全绿"）。
- **冲突内容**：c10 定义混合矩阵为 13 场景（既有 M1–M6、R1–R3 共 9 个 + 新增
  M7/M8/M9/M10，M10 即 r2 握手断言场景），且 §6.3 证据行自称"两轮，C10 矩阵口径"；
  但 c12 §6.3 的 PASS 判据字面只覆盖 M1～M6。若 C12 执行者按字面执行门禁，
  M7（hash mismatch）、M8（epoch shift）、M9（r1 旧化身宽限）、M10（r2 三元组握手）
  与 R1–R3 均不在 PASS 判定范围内——恰好漏掉本轮收口新增的全部协议断言场景。
  佐证：晚于两者产出的 `closeout-report-v1.md:57` 已按"13 场景（M1-M10+R1-R3）
  两轮完整零失败"口径表述，c12 成为口径孤例。
- **建议修复方**：c12-window-plan.md owner。将 §6.3 PASS 判据改为
  "M1～M10 + R1–R3 全场景 pass（c10 §2 场景表口径）"。

### P2-1【中低】ledger-v1.json W4-07 reason 符号误用：Tmax=720s

- **位置**：`ledger-v1.json` W4-07 条目 reason 字段——
  `"tombstone TIME upper bound implemented (Tmax=720s formula-derived); ..."`。
- **冲突内容**：protocol-budget-frozen.md §6 冻结符号表中 **Tmax = 墓碑条目上限 =
  20,000 项**，**720 秒是 W（墓碑/化身宽限保留窗口）**；plan-v2.2.1.md 19.6 的
  公式符号也是 `W = max_partition_window + max_recovery_time + skew_margin`。
  ledger 把 W 的数值（720s）安到 Tmax 符号上，与冻结符号表直接冲突，会误导
  C14 复算与读者。核实：`ledger-update-draft-r2.json` 的 updates（16 条）未修正
  W4-07，问题在当前最新状态下仍然成立。
- **建议修复方**：ledger owner（C14 复算时）。reason 更正为
  "tombstone retention window (W) = 720s formula-derived"，Tmax 一词仅用于指
  20,000 条目上限。

### P3-1【低】rollback-v1.md 预算符号大小写：high_water vs HighWater

- **位置**：`rollback-v1.md` §1.4（"C03 预算（Nmax/Tmax/Bmax/Pmax/Rmax/Dmax/
  high_water）"）。
- **冲突内容**：frozen §6 预算表与 budget_digest 字符串（frozen §6 / c12 §4.3）均用
  **HighWater**；rollback-v1 用小写 `high_water`。同义但冻结符号不统一。
- **建议修复方**：rollback-v1.md owner。改为 HighWater。

### P3-2【低】c12 §1 C09 盘点行已被 evidence-index 演进超越（时点差异）

- **位置**：`c12-window-plan.md` §1（"raw/three_node_c09_r{1,2,3,4,6,7}.json（r5 缺号）；
  r7 仍有失败场景（F5/C9a/C9d/C9e/C9f）"）vs `evidence-index.md` §1.1/§1.2
  （至 2026-10-12T01:33Z 快照：系列已有 r1–r4/r6–r9 共 8 个；F5 已在 diag_f5_r16
  收敛，最优 r9 仅剩 C9d/C9e/C9f 失败；C9d 四连 fail 未收敛）。
- **冲突内容**：两文档快照时点不同（c12 = 07:11，evidence-index = 09:33），结论方向
  一致（C09 均未全绿、§4.1 前置均成立），属时点演进而非矛盾；但 c12 作为执行计划
  的现状描述已过期，且 evidence-index 落盘后无人回刷 c12 §1。
- **建议修复方**：c12-window-plan.md owner。在 §1 加注"以 evidence-index.md 最新
  快照为准"或 C12 窗口执行前刷新盘点行。

### P3-3【低】observability-check.md 的 incr/1 行号引用随工作树演进漂移

- **位置**：`observability-check.md` §2（"incr/1 定义于 src/lib/imboy_cache_sync.erl:
  1200-1203"）。实测当前工作树（1206 行版本）中 `-spec incr(atom())` 位于 1203 行、
  函数体 1204–1206；c12 时点（07:11）文件为 1203 行。引用区间与当前实现有约 3 行
  漂移。
- **冲突内容**：`imboy_cache_sync.erl` 属活跃 WIP（c12 §1 明示工作树含未提交修改），
  行号绑定演进中的版本天然漂移。registry-w1-closeout.md 头部声明了"引用行号以核查时
  实际读取的工作树为准"的口径，observability-check.md 缺同等声明。
- **建议修复方**：observability-check.md owner（可选，低优先）。补一行与
  registry-w1-closeout 相同的行号口径声明；C14 复算时对活跃文件行号统一按候选
  冻结版复核。

### 观察-1【澄清，非问题】evidence-index.md 的"缺失引用"情形不存在

任务预设"报告引用 evidence-index.md 但该文件可能还没生成"。实测：全 `docs/` 目录
grep `evidence-index` 仅命中该文件自身——生成前后均无任何其他文档引用它，不存在
悬空引用；该文件于审查期间（09:35）由并行 agent 落盘，内容与 c12 §3 schema、
verifier（scripts/check_cache_closeout_v1.py，实存在于工作树）口径互洽。此项转入
无冲突确认清单（见下 §2.18）。

---

## 2. 无冲突确认清单（正面结论）

1. **96-ID 计数与集合**：required-ids.json `total=96` = 实际 ids 96 项（无重复）=
   ledger-v1.json entries 96 项（无重复）；两文件 ID 集合机器比对**完全相等**，
   无仅单侧存在的 ID；kind 分布一致（task 56 / ac 30 / sec19_gate 10）。
2. **历史台账 86 项口径**：ledger hist_status 实测分布 77 PASS / 6 BLOCKED /
   2 NOT_RUN / 1 PARTIAL（+10 项 W0 不在历史台账），与 backend-closeout-plan-v1.md
   §3、review-ledger-v1.md 概览、ledger `source_hist_ledger` 字段四处一致；
   86 + 10(W0) = 96 闭环。
3. **分类统计**：ledger `classes` 声明与逐条实测完全一致（REVERIFY 63 /
   REVERIFY_HIST_INHERIT 9 / GAP_IMPLEMENTATION 13 / GAP_EVIDENCE 7 /
   NOT_RUN_CARRIED 1 / BLOCKED_EXTERNAL 3）；review-ledger-v1.md 分类统计同值。
4. **alias 26 项**：required-ids.json alias 条目实测 26，与 review-ledger"26 项
   alias 登记"一致；规则（编号 ≥10 两位→三位）应用正确（W0-10/W3-10/W4-10/
   W4-11/W5-10/AC-10…AC-30）。
5. **AC-26/27 口径六处一致**：backend-closeout-plan §1、ledger-v1.json（AC-26/27
   条目）、review-ledger-v1.md（L19/L24）、external-inputs-c13 §5、
   protocol-budget-frozen §10、c12 §6.5 全部为 BLOCKED_EXTERNAL / 维持阻塞，
   无一处吞掉原始阻塞；baseline.json `w0_decision`（W0_BLOCKED / confirmed=false /
   replicate_whitelist=[]）与该口径自洽；并行产出的 ledger-update-draft-r2.json
   亦为 evidence_confirmed_unchanged。
6. **external-inputs-c13 行号引用 7 处全部命中**：plan:1552-1560（11.5 AC 表
   原文逐字）、plan:1637-1651（12.4 候选 SLO 表 + "不是实测结果"声明）、
   backend-closeout-plan-v1.md:263（C13 卡首行原文）、review-ledger-v1.md:19,24、
   w1-w5-addendum-20261011.md:64、w0/report.md:169（W0_BLOCKED 原因第 1 条）、
   protocol-budget-frozen.md:174（自证行逐字）。
7. **提交哈希引用零冲突**：675b0744（G-QR/C08）、e001fb8e（G-01）、7db4e0af
   （C02–C05）、6217ec8e（C00/C01 docs）四个哈希在 rollback-v1.md、
   c12-window-plan.md（含全 40 位 675b07444955…，前 8 位与简写一致）、
   registry-w1-closeout.md、qr-wire-contract-check.md 中的主题映射完全一致。
   ledger-v1.json 本身不含提交哈希（生成于 C00 时点 2026-10-11T13:14，属计划内；
   c12 §4.2 规定执行时补 candidate 字段），不存在 ledger 与 rollback 哈希互斥。
8. **r2 修订三方一致（frozen §4 / c10 M10 / 源码实现）**：`version_request` 3 元组
   `{cache_version_request, FromPid, FromNode}`——`imboy_cache_sync_proto.erl:324-326`
   `version_request_frame/1` 返回 `{cache_version_request, FromPid, node()}`，与
   M10 断言"rpc(B,…) 精确等于 {cache_version_request, SelfPid, B}"一致；旧 2 元组
   兼容走 syn 视图反查——`imboy_cache_sync.erl:431` 分支及其注释"旧格式请求…
   C10 混合矩阵旧节点"；`version_report` 9 元组尾增发送者 sync pid——
   `imboy_cache_sync.erl:426/:434` 实构 `{cache_version_report, node(), counter,
   epoch, config_hash, registry_hash, boot_id, ready, self()}`（恰 9 元，尾元
   self()）。
9. **frozen §4 内部无自相矛盾**：正文快照请求帧 `{cache_snapshot_request,
   RequestorPid, Round}` 与 r2"快照请求直达 report 携带的 pid"是寻址来源的补充
   而非帧形变更（r2 明示"只改帧的寻址方式，不改任何判定逻辑；预算与闭合条件
   不变"）；三重闭合条件（全 peer done + 水位合并 + 无失败页）在正文与 r2 段均
   未被改动。
10. **r1 修订三方一致**：frozen §2（宽限 W=720s、旧值防复活由 Lamport revision +
    epoch 承担、自环过滤保留、快照回放 `<<0:128>>` 合法特例）= c10 M9 场景
    oracle（grace_incarnation 条目 + 旧 revision 不复活 + 高 revision grace_accepted
    收敛）= c10 §3 `inject_v2_set/4` 改造说明（boot_id 用 `<<0:128>>` 特例）。
11. **M7/M8 断言与 frozen §1/§3 逐项对应**：`{reject, hash_mismatch}` /
    reject_total 计数 / hash_divergent 标记 / `{reject, stale_epoch}` / epoch 单调
    与旧 epoch 重放拒绝 / invalidate 不参与 epoch 比较豁免——frozen §1 与 c10
    M7/M8 协议依据列互为原文引用，无出入。
12. **budget_digest 逐字节一致**：frozen §6 摘要串与 c12 §4.3
    `candidate_freeze.json` 模板中的 `budget_v1: Nmax=50000 … W=720s` 完全相同
    （含 Nmax_bytes=16MiB、Tmax_bytes=8MiB、Bmax_bytes=32MiB、Pmax_bytes=4MiB、
    Rmax=2、Rmax_rounds=3、Dmax=300s、MaxValue=32768B、MaxTTL=600s、
    HighWater=10000）。
13. **CANDIDATE_SHA / candidate_sha256 术语分层自洽**：backend-closeout-plan C12
    卡与 c12 用 CANDIDATE_SHA 作概念名，JSON 键统一为 `candidate_sha256`；verifier
    接受 candidate_sha256 / candidate / candidate_digest / working_tree_digest
    任一键（c12 §3.3），evidence-index §4.2 沿用同口径；closeout 文档族中该术语
    仅出现在 c12（16 处）与计划卡，无第三种拼法。Dmax（300s）/ W（720s）符号在
    frozen、c10、c12、observability-check、rollback-v1 全部一致（rollback 的
    high_water 大小写除外，见 P3-1）。
14. **registry 数量 47 三处一致**：baseline.json `w0_decision.registry_count=47`、
    `w0/decision.json` registry_count=47、`w0/report.md:151`"共 47 个业务命名空间"。
15. **INTEGRATION_BASE `3a01829d` 一致**：c10 §1 与 w1-w5-report-20261011.md
    :87/:145/:211（V2 引入前 main，git worktree + `make app` 独立构建）口径一致；
    c12 §2.2 的 LEGACY_EBIN 用法与 c10 §1 相同。
16. **源码 file:line 抽查 6 处（超任务要求 5 处）全部命中**：
    - `src/repo/user_tag_relation_repo.erl:262`（tag_subtitle_2 填充键）/:296
      （memo TTL 86400）/:301-302（tag_subtitle_3 失效键）——registry-w1-closeout
      §1 引用逐行精确；
    - `src/ds/group_member_ds.erl:146`——`group_ds:join(Uid, Gid)` 确在 with_tx
      事务体内，registry-w1-closeout §4 引用精确；
    - `src/lib/imboy_cache_policy.erl:152-158`——`namespace_of/1` 子句集（尺寸
      2–5 元组 + {local_cache,Sub,_}），确无一元组/裸原子子句，registry-w1-closeout
      §3 的"fcm 一元组 → undefined → legacy"论证与源码相符；
    - `src/api/qr_login_handler.erl:84-89`——`authority/0` 实现与 rollback-v1
      §2.3、qr-wire-contract-check §1.1 引用的代码逐字一致（默认 cache）；
    - `scripts/cache_v21_dcheck.erl` L1175 附近确为 C9d 诊断代码（evidence-index
      §1.3 crash 栈定位）；
    - `src/lib/imboy_cache_sync.erl` incr/1（见 P3-3 的 3 行漂移，函数与转发
      语义本身无误）。
17. **894 → 1203 行不构成矛盾**：baseline.json（C00 时点 894 行 + violation 标注）
    与 c12 §1（当前 1203 行仍超限）为不同时点快照；frozen §9"全部 <800 行"是
    C03 目标而非现状声明，c12 §4.1 前置 4 已把行数问题列为候选冻结前必须处理项
    ——时点语义链完整、无相互矛盾。
18. **evidence-index.md 与 c12/verifier 口径互洽**：17 个 raw JSON 分类（0 可用 /
    8 需重跑 / 9 诊断）与 c12 §3 schema（scenarios/status/candidate 绑定/UTC
    规则）及 verifier 两项 FAIL 输出互相印证；52 问题计数逐文件复核成立；
    G1–G7 差距清单对 c12 条款（§3.3、§4.1、§6.2 系列、§7）的引用均准确；
    `scripts/check_cache_closeout_v1.py` 实存于工作树（33KB，2026-10-12 07:07）。
19. **Jmax 处理一致**：plan 19.6 与 backend-closeout-plan C01 卡要求冻结 Jmax 或
    提供无 journal 的等价有界追赶证明；frozen §6 Jmax 行显式"无 journal；等价
    追赶窗口=W，见第 4 节论证"，§4 给出"页内并入 / 延迟事件版本合入 / 三重闭合"
    论证——按"等价证明"路径闭合，与 ledger 19.6 条目 reason 的预算列举口径
    相容。
20. **c10 场景计数自洽**：既有 9（M1–M6 + R1–R3）+ 新增 4（M7–M10）= 13；
    c12 §6.2 三节点 PASS 用"scenarios 全 pass"泛指（与 C09 的 22 场景/轮口径
    不冲突）；问题仅在 c12 §6.3 的显式列举处（见 P1-1）。

---

## 3. 统计与移交

| 严重度 | 条数 | 编号 |
|---|---|---|
| 中高（影响门禁判定完整性） | 1 | P1-1 |
| 中低（冻结符号误用） | 1 | P2-1 |
| 低（术语大小写 / 时点过期 / 行号漂移） | 3 | P3-1、P3-2、P3-3 |
| 观察澄清（非问题） | 1 | 观察-1 |
| **问题合计** | **5** | |
| 无冲突确认 | 20 项 | §2 |

修复责任方归属（按唯一 owner 原则，本审查不代改）：
- P1-1、P3-2 → c12-window-plan.md owner（A0/C12 计划持有者）；
- P2-1 → ledger 持有方（C14 复算时更正 W4-07 reason）；
- P3-1 → rollback-v1.md owner；
- P3-3 → observability-check.md owner（可选加固）。

红线遵守声明：本次审查仅读取上述文件与抽查 src/ 工作树；唯一写入为本文件；
未运行任何测试/节点；未执行任何 git 操作；未触碰外来 WIP。

---

## 补记：引用完整性审计与 w0 §15.3 缺口闭合（2026-10-12，收尾后）

收尾签发后追加引用完整性审计（对 closeout-v1/ 与 w0/ 全部 .md 提取 54 个仓内路径，
逐个核对磁盘存在性与 git 跟踪状态），发现并处置：

1. **校验器未入库**：`scripts/check_cache_closeout_v1.py` 被 5 份交付文档引用但从未
   git 跟踪 → 已入库（提交 8cc04a4e），evidence-index.md 归档注记二同步记录。
2. **w0 §15.3「长期保留文档」三路径全部缺失**（本审查原范围未覆盖 w0 计划文本，
   故前文 P 项未含）：`docs/architecture/imboy-distributed-cache-v2.md`、
   `docs/guides/operations/cache-cluster.md`、`docs/plans/2026-10-10-distributed-cache-v2.2.md`
   在仓库全部 git 历史中均不存在。处置（均为新增，不触碰冻结快照）：
   - 架构篇与运维篇按当前实现（候选 f1aa1ce0）新撰；
   - `docs/plans/` 为仓内刻意不跟踪目录（.gitignore:163 `/docs/plans/`，现存 36 个
     本地计划文件均未跟踪），故不在该目录建桩；路径映射记录于已跟踪文档——
     架构篇 §7 指明该 plans/ 路径即 w0/plan-v2.2.1.md 快照的原工作路径；
   - 对收尾三结论无影响：执行权威为 backend-closeout-plan-v1.md（存在且 SHA 核对通过），
     本缺口属文档交付遗漏而非验收依据缺失。
3. 审计余项定性：`config/sys.closeout-local.config` 与 `deploy/docker-compose.prod.yml`
   为既有 .gitignore 规则（`*local*.config`、显式路径）的刻意忽略，非缺口。
