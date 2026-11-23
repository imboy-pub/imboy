# RESUME — IMBoy Erlang Runtime Engineering Audit

> 本文件是本审计车道的**唯一续接入口**。任何后续会话从这里开始。

> 🔁 **R0 已接手（2026-11-23）**：第一轮审查已完成并授权修复，当前续接入口改为 **`RESUME-R0.md`**。本文件及第一轮文档原样保留；P1-1 的勘误见 `r0-capacity-remediation-design.md` §11。

## 续接协议（按任务书第七节）

1. 依序读：本文件 → `erlang-runtime-audit-status.json` → `erlang-runtime-audit-checkpoint.md`。
2. 禁止重复已完成阶段（STATUS.json `phases` 全 DONE 即第一轮收口）。
3. 禁止覆盖未提交修改（main 检出的 `docs/research/` 属属主既有产物）。
4. 第一轮为 review-only：**进入任何代码修复前必须获得属主对 `erlang-runtime-review.md` §5 的明确授权**。

## 一句话现状

第一轮审查完成（2026-11-23）：15 章教程全读、六阶段审查完毕、10/10 交付文件落 `docs/engineering/`；**1×P1（vm.args +P/+Q 20480 × cowboy max_connections=infinity 容量悬崖，release 模板实证同值）+ 6×P2 + 7×P3**，零代码修改，等待授权。

## 文件地图

| 文件 | 内容 |
|---|---|
| `erlang-runtime-review.md` | 主报告：基线/方法/风险台账/章节符合度/修复路径 |
| `erlang-review-checklist.md` | 教程 Ch.14 红旗逐条 × 各章 checklist 符合度 |
| `erlang-process-health-audit.md` | Phase B：监督树/spawn/mailbox/WS 生命周期（PB-1/PB-2） |
| `erlang-memory-ets-audit.md` | Phase C：binary/ETS/persistent_term/数据生命周期 |
| `erlang-testing-gap-analysis.md` | Phase D：资产盘点 + 两缺口（PBT statem、混沌编排） |
| `erlang-observability-audit.md` | Phase E：OBS-1 system_monitor / OBS-2 metrics 维度 |
| `erlang-performance-audit.md` | Phase F：P1-1 容量悬崖全要素 + hibernate 待实证 |
| `erlang-runtime-audit-status.json` | 机器可读状态（findings/证据锚点/下一步） |
| `erlang-runtime-audit-checkpoint.md` | 检查点（阶段/未完成/测试/git 状态） |

## 若获授权，从哪开始

按 `erlang-runtime-review.md` §5 顺序：P1-1（vm.args + max_connections，S 级，**与 +S 同批**）→ OBS 批次（M）→ P2-4（S）→ throughput bench（M）。每项独立 commit 可回滚；修复动作细节（含回归测试口径）已写入各专项报告对应条目。

## 若属主要求二轮加查（不修代码）

1. 15 个嵌套 call 候选的静态调用图（闭环 Ch.5 §4.6）。
2. binary:part 存储点 `referenced_byte_size` 只读抽样。
3. login_attempt_ds 表 heir 可行性评估（现 one_for_one 秒级重建，可选提级）。

## 基线锚（防漂移）

- 审计基线 commit：`a5c74219f`（main，领先 origin 56）。
- 教程 clone：`/tmp/lerc-20261123`（会话临时物，可随时重 clone：`git clone --depth 1 https://github.com/ayarodionov/Learning-Erlang-from-Claude`）。
- 本轮未跑任何 make/test；若基线 commit 变化，重读 STATUS.json `baseline` 并核对 vm.args/websocket_handler 证据锚点仍有效。
