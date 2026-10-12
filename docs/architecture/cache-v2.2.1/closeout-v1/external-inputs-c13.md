# C13 — AC-26/AC-27 外部输入存在性判定 / External Inputs Existence Verdict

> 任务：CACHE_V221_BACKEND_CLOSEOUT_V1 / C13（原计划 SLO 与外部输入判定）
> 判定对象：AC-26、AC-27（生产资格类验收）所需三类外部输入在仓库/文档中的存在证据
> 日期：2026-10-11 | 性质：只读取证，未运行任何测试/服务/节点
> 原计划快照：`docs/architecture/cache-v2.2.1/w0/plan-v2.2.1.md`（SHA-256
> a5523ee731bc822018831586e3d747ba109c2c66e734daf5633b494d9b18c713）

---

## 1. AC-26 / AC-27 原文与依赖输入推导

### 1.1 原文

原计划 11.5 节（`plan-v2.2.1.md:1552-1560`）：

| AC | 验收内容 |
|---|---|
| AC-26 | 单节点性能达到冻结 SLO |
| AC-27 | 三节点同步性能及资源预算达标 |

关键限定语是**"冻结"**——第 12 节与 19.7 节明确这些 SLO 必须先由外部输入证据化冻结，然后才能验收：

- 12.4 节（绝对性能 SLO）：候选值表（本地 GET P99 ≤ 2 ms、本地 SET P99 ≤ 5 ms、
  三节点 SET 收敛 P99 ≤ 1 s、相对 GET P99 退化 ≤ 10%、持续吞吐 ≥ 冻结基线 95%）
  之后紧跟："**这些不是现有系统实测结果。W0 必须在明确的并发数、请求速率、Key 数量、
  Value 大小及硬件条件下冻结最终 SLO。**"
- 19.7 节（参数冻结时机）："W0 结束时只有**业务峰值/目标硬件已知**，才冻结绝对吞吐、
  P99、错误率和 headroom。本机微基准不反向编造业务 SLO；未知项明确 W0_BLOCKED。"
- W0 报告（`w0/report.md`）与本轮 `ledger-v1.json` 已按此逻辑将 W0-09 / AC-26 /
  AC-27 记为 BLOCKED_EXTERNAL。

### 1.2 依赖输入清单（自原文推导，C13 正文同口径）

C13 要求收集的权威输入（`backend-closeout-plan-v1.md:263`）：
"目标硬件/拓扑、业务峰值/增长、Key/Value 分布、绝对 SLO 与 headroom"。

映射到任务指定的三类：

- **I-1 生产 SLO / 性能规格**：可直接作为验收基准的权威目标 QPS、p99、并发连接数、
  消息吞吐（含缓存层本地 GET/SET P99 与同步收敛目标）。
- **I-2 生产硬件规格**：目标节点数/拓扑、CPU/内存/磁盘、网络带宽。
- **I-3 峰值负载输入**：压测对标用的峰值会话/消息/附件量（及 Key/Value 分布）业务
  定义。

---

## 2. AC-26 依赖输入表

| 输入项 | FOUND/ABSENT | 证据路径 / 搜索范围 |
|---|---|---|
| I-1a 权威冻结的生产 SLO（绝对吞吐/P99/错误率/headroom，含缓存层指标） | **ABSENT**（仅模板级候选值，见附录 A1-A3） | 搜索范围：`docs/` 全部 .md（QPS/p99/SLO/吞吐/并发/规格/峰值/容量）、`deploy/`（README、compose、helm、install.sh、preflight.sh）、`imboy.wiki/` 全部 15 页。物理存在但非权威：`docs/guides/operations/sla-template.md`（合同附件模板，待填占位）、`docs/guides/operations/benchmark.md`（状态"待执行"，实测列全空）、`plan-v2.2.1.md` 12.4（自称"初始候选…不是实测结果"）。三者互不一致且无一经冻结程序确认 |
| I-1b 单节点目标并发连接数（业务权威值） | **ABSENT**（模板级矛盾值存在：sla-template 50,000 vs benchmark 100,000，见附录 A1/A2） | 同上；另查 `README.md`（无"百万并发"现文，benchmark.md:39 所称"头条卖点"在当前 README 中不存在）、`scripts/bench_websocket.sh`（默认 CONNS=100，参数化脚本，无冻结 profile） |
| I-2a 目标硬件规格（CPU 核数/内存/磁盘/网络带宽的容量目标机定义） | **ABSENT**（仅最低足迹与容器上限，见附录 B1-B3） | 搜索范围：`deploy/README.md:89-90`（最低 4 GB/10 GB、建议 8 GB/20 GB——部署足迹，无 CPU/带宽）、`imboy.wiki/System-Requirements.md:12`（同足迹口径）、`deploy/docker-compose.prod.yml`（BACKEND_MEM_LIMIT 8192M 等容器上限）、`deploy/helm/values*.yaml`（limits 4C/8Gi）、`deploy/install.sh`、`deploy/preflight.sh`、`docs/guides/operations/deployment/day1-quickstart.md`（无硬件条目）。无任何文档定义"8 核/16 GiB 目标基准机 + 网络带宽"这类容量规格 |
| I-3a 峰值负载业务定义（峰值会话/消息/附件量 + Key/Value 分布） | **ABSENT** | 搜索范围：`docs/guides/**`（peak/峰值 仅命中 sla-template 模板行与 E2EE 无关项）、`docs/plans/**`、`docs/design/**`、`deploy/`、`imboy.wiki/`、`scripts/`（smoke/bench 脚本清单）。sla-template 2.3 的 3,000 QPS / 10,000 msg/s / 50,000 WS 为模板默认值且注明"实际环境可能因业务模型不同而有差异"，无实测支撑；缓存 Key/Value 分布的权威业务输入不存在（原计划 12.2 的 S/M/L/Stress 为测试等级设计，非业务峰值） |

## 3. AC-27 依赖输入表

| 输入项 | FOUND/ABSENT | 证据路径 / 搜索范围 |
|---|---|---|
| I-1c 三节点同步收敛 SLO（权威冻结值） | **ABSENT** | 同 I-1a 搜索范围。唯一来源为 `plan-v2.2.1.md:1637-1651`（12.4 节）候选值（收敛 P99 ≤ 1 s、相对退化 ≤ 10%），原文自我限定为"V2.2 初始候选…不是现有系统实测结果"，未经 19.7 冻结程序 |
| I-2b 生产集群拓扑（三节点部署的权威定义） | **ABSENT**（且现有部署规格与三节点假设冲突） | 搜索范围：`deploy/helm/values.yaml:23-29`（backend `replicaCount: 1`，注释明确"单副本有状态服务…若未来引入多副本，必须先有 StatefulSet ordinal NodeId 方案"）、`deploy/helm/values.prod.yaml:17`（`replicaCount: 1`）、`deploy/docker-compose.prod.yml`（单 backend 服务）、`docs/guides/operations/clustering.md`（头部标注"适用范围：1.x 中期计划"，正文 3 节点 × 80 连接=240 为连接池规划示例，主机名 `pg.imboy.internal` 为示例值）。无任何文档将"三节点生产拓扑"冻结为交付规格 |
| I-2c 同机房网络带宽/延迟规格 | **ABSENT** | 搜索范围：`deploy/` 全部 yml/sh/md、`docs/guides/operations/**`、`imboy.wiki/`（带宽/bandwidth/network 无容量规格命中；仅 l4-sni 文档 ~2 MB 级内存footprint 等无关项） |
| I-3b 峰值负载输入（同 AC-26 I-3a，另需三节点下的同步流量模型） | **ABSENT** | 同 I-3a；`closeout-v1/protocol-budget-frozen.md:174` 自证："这些值是本地工程预算；生产 SLO 输入仍缺失（AC-26/27 保持 BLOCKED_EXTERNAL）"。Tmax/Jmax/Bmax 等资源预算已按本地工程值冻结（19.6 要求"由 W0 负载与 W2 正确性结果绑定"），但生产侧绑定输入不存在 |

---

## 4. 附录：已检出证据的等级说明

### A. 模板级/待执行数字（物理存在，不构成权威输入）

1. **A1 `docs/guides/operations/sla-template.md`**（文档类型自标"合同附件模板；
   使用说明：签约前需填写 [方括号] 占位内容"，v1.0 / 2026-06-30）：
   - 2.1 API P95/P99 表（消息发送 <100ms/<300ms 等）；
   - 2.2 单节点最大 WS 在线 50,000；
   - 2.3 "以下为单节点（8 核 16GB）基准测试数据，实际环境可能因业务模型不同而有
     差异：HTTP API 峰值 QPS 3,000；WebSocket 消息峰值 10,000 条/秒；并发 WS
     50,000"。
   判定：合同模板预设值，无实测记录支撑（benchmark.md 实测列全空），且未与缓存
   层 SLO 建立任何映射。它提及的"8 核 16GB"是唯一的单节点硬件定量线索，但同样
   属于模板默认而非冻结的目标机规格。
2. **A2 `docs/guides/operations/benchmark.md`**（状态自标"待执行 — 需人工在目标
   硬件跑出数据后填写"）：目标列有 WS 并发 100,000、吞吐 50,000 msg/s、P99
   <100ms、万连接内存 <4GB、CPU 峰值 <80%；**实测值/硬件/日期列全部为空**。与
   A1 的 50,000 连接互相矛盾（100,000 vs 50,000）。
3. **A3 `plan-v2.2.1.md` 12.4**：候选 SLO 表，原文自我否定冻结资格（见 1.1 节引文）。

### B. 足迹级/资源上限规格（存在，但不是 AC-26/27 所需"目标硬件"）

1. **B1 `deploy/README.md:89-90` 与 `imboy.wiki/System-Requirements.md:12`**：
   核心栈最低 4 GB 内存/10 GB 磁盘、建议 8 GB/20 GB（Uptrace 后 8/16 GB）。无 CPU
   核数、无网络带宽、不与任何连接数/QPS 容量挂钩。
2. **B2 `deploy/docker-compose.prod.yml`**：BACKEND_MEM_LIMIT 8192M、PG 4096M、
   Garage 1024M 等容器内存上限。
3. **B3 `deploy/helm/values.yaml` / `values.prod.yaml`**：backend limits 4000m/8Gi、
   requests 500m-1000m/1-2Gi、`replicaCount: 1`（单副本，含 TSID 多副本碰撞警告）。

### C. 既有台账一致性

`closeout-v1/ledger-v1.json`（AC-26: "no production SLO/hardware/peak inputs;
local relative thresholds frozen at C12 but original AC remains blocked"；
AC-27: "same as AC-26"）、`closeout-v1/review-ledger-v1.md:19,24`、
`w1-w5-addendum-20261011.md:64`、`w0/report.md:169` 均已记录同类判定；本次独立
取证结论与之一致。

---

## 5. 结论

**AC-26（单节点性能达到冻结 SLO）：维持 BLOCKED_EXTERNAL。**
其依赖输入（权威冻结 SLO、目标硬件容量规格、峰值负载业务定义）在
`docs/`、`deploy/`、`imboy.wiki/`、`scripts/` 中均不存在权威版本：仅有合同模板
预设值（sla-template.md）、待执行空表（benchmark.md）、计划内候选值
（plan-v2.2.1.md 12.4 自认"不是实测结果"）与部署足迹/容器上限
（deploy/）。按 19.7"W0 结束时只有业务峰值/目标硬件已知，才冻结绝对吞吐、P99、
错误率和 headroom；本机微基准不反向编造业务 SLO"，冻结前置条件不成立，
AC-26 不可解除阻塞。

**AC-27（三节点同步性能及资源预算达标）：维持 BLOCKED_EXTERNAL。**
在 AC-26 同样缺口之上，另有专属缺口：(a) 三节点收敛 SLO 仅有 12.4 候选值未经
冻结；(b) **现有部署权威规格与三节点假设直接冲突**——helm `replicaCount: 1`
（单副本有状态服务，多副本需先解决 TSID NodeId 序号方案），clustering.md 的
三节点内容自标"1.x 中期计划"示例；(c) 同机房网络带宽规格不存在；(d) 生产侧
资源预算绑定输入缺失（protocol-budget-frozen.md:174 自证）。无对应三节点
生产环境定义，按 C13"没有对应环境不能用本机结果外推"，AC-27 不可解除阻塞。

**解除阻塞所需外部输入（供后续恢复）**：由产品/运营方提供权威业务峰值模型
（峰值会话/消息/附件量、缓存 Key/Value 分布与操作速率）；由部署决策方冻结
目标硬件与集群拓扑（核数/内存/磁盘/带宽、节点数——三节点方案需先解决单副本
TSID 约束）；据此经 19.7 冻结程序产出绝对 SLO 与 headroom 后，方可在对应
环境执行 AC-26/27。

---

*C13 输入审查及缺口交付：PASS（本文件）；映射的原 AC-26/AC-27：BLOCKED_EXTERNAL
（未吞掉原始阻塞）。*
