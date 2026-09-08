# Agent Hub 任务状态机契约（FSM-00 冻结）

> **权威真源**：[agent_hub_task_state_machine.json](./agent_hub_task_state_machine.json)。
> 本文档只解释该 JSON，不新增任何契约语义，不得作为第二真源引用。
> 机器校验：`python3 scripts/verify_agent_hub_task_state_machine.py docs/api-contracts/agent_hub_task_state_machine.json`（退出码 0=合法矩阵，2=违规）。
> 本契约由 FSM-00 冻结；后续 DATA-01/MCP-02 等实现任务必须以此矩阵为实现目标，不得自创状态。

## 1. 契约定位

Agent Task 是 Agent Hub 中"外部 MCP Agent / 内建 Agent 受控执行"的载体。本矩阵固定任务从提交到终点的全部合法状态迁移、允许 actor、幂等语义与副作用边界，并显式生成非法迁移补集。任何未在下表中声明的 `(状态, 动作)` 组合**默认拒绝**（`invalid_transition`），状态不变、无新副作用。

冻结面（verifier 以常量锁定，两侧任何漂移都 fail-closed）：

| 冻结项 | 值 |
|---|---|
| 状态全集（9） | submitted、working、awaiting_approval、approved、rejected、expired、completed、failed、cancelled |
| 终态全集（5） | rejected、expired、completed、failed、cancelled |
| 初始态 | submitted |
| 动作全集（10） | start、cancel、fail、expire、progress、need_approval、complete、approve、reject、resume |
| 合法边（15） | 见下表 |
| 非法补集（75） | 9 状态 × 10 动作 − 15 合法边 = 75 个 `(状态, 动作)` 对，全部默认拒绝 |

## 2. 状态说明

| 状态 | terminal | 收敛路径 |
|---|---|---|
| submitted | 否 | 任务已提交待拾取；start→working 后收敛，或直接 fail/cancel/expire 至终态 |
| working | 否 | 执行中；complete/fail/cancel 直达终态，或 need_approval 进入审批分支 |
| awaiting_approval | 否 | 等待群内人工审批；approve→approved 继续执行，reject/expire/cancel 直达终态 |
| approved | 否 | **已批准、待调度**的短暂过渡态（不是终态）；resume→working 收敛，调度确定性失败 fail→failed |
| rejected | 是 | 审批被拒；无出边，不可逆 |
| expired | 是 | 等待人工动作（拾取/审批）超时；无出边，不可逆 |
| completed | 是 | 执行成功；无出边，不可逆 |
| failed | 是 | 执行失败或调度确定性失败；无出边，不可逆 |
| cancelled | 是 | 任务被取消；无出边，不可逆 |

`approved` 不是终态是本契约的关键语义：批准本身只是"放行"，真正的副作用发生在 approved→working 的调度执行中，因此才有"批准后至多执行一次"的可兑现承诺。

## 3. 合法迁移矩阵（15 条边）

| # | from | action | to | 允许 actor | 幂等语义（摘要） | 副作用（摘要） |
|---|---|---|---|---|---|---|
| 1 | submitted | start | working | agent_worker | 重复 start 已在 working 则按已到位处理 | 无外部副作用 |
| 2 | submitted | cancel | cancelled | task_owner, admin | 终态后重复被默认拒绝 | 终态可靠群消息 |
| 3 | submitted | fail | failed | agent_worker, system | 终态只生效一次 | 记录原因 + 终态可靠群消息 |
| 4 | submitted | expire | expired | system | 定时器只触发一次 | 记录超时 + 终态可靠群消息 |
| 5 | working | progress | working | agent_worker | 自环：进度上报不改状态，事件天然幂等 | 仅过渡态实时观察事件（ephemeral） |
| 6 | working | need_approval | awaiting_approval | agent_worker | 待审登记只落一次（原子占位） | 可靠审批卡片 + 启动审批超时定时器 |
| 7 | working | complete | completed | agent_worker | 终态只生效一次，结果不覆写 | 记录结果 + 终态可靠群消息 |
| 8 | working | fail | failed | agent_worker, system | 终态只生效一次，原因不覆写 | 终态可靠群消息；执行超时按 fail(timeout) 归并此边 |
| 9 | working | cancel | cancelled | task_owner, admin | 终态只生效一次 | 协作取消信号 + 终态可靠群消息 + 回收待审 |
| 10 | awaiting_approval | approve | approved | human_approver | **first-writer-wins**；重复/反向一律 already_decided | 决定记录 + 决定通知；不在此时执行工具 |
| 11 | awaiting_approval | reject | rejected | human_approver | 同 approve；拒绝后绝不执行 | 决定记录 + 决定通知 |
| 12 | awaiting_approval | expire | expired | system | 与并发决定同样 first-writer-wins | 终态可靠群消息 + 回收待审 |
| 13 | awaiting_approval | cancel | cancelled | task_owner, admin | 终态只生效一次；与并发决定 first-writer-wins | 终态可靠群消息 + 回收待审 |
| 14 | approved | resume | working | system | 按 execution 幂等键只调度一次 | 触发受控工具执行，幂等键下传 |
| 15 | approved | fail | failed | system | 调度确定性失败只落一次 | 记录调度失败 + 终态可靠群消息 |

每个 `(from, action)` 至多一条出边（确定性迁移）：同一状态收到同一动作，目标状态唯一。完整逐字段语义以 JSON `transitions` 数组为准。

## 4. 审批规则（并发与重复）

- **仲裁**：first-writer-wins。并发场景下第一个成功落盘的决定获胜；其后任何重复（同向或反向）的 approve/reject 一律返回 `already_decided`：状态不二次迁移、决定通知不重复投递、绝不触发工具执行（`duplicate_decision.state_change=false`、`new_side_effects=false`）。
- **审批只走一次**：`approved` 之后不存在第二次生效的 approve；批准后至多执行一次（见执行语义）。
- **审批人约束**：必须是群内有权限成员（成员关系以服务端权威数据为准，不信事件携带的成员列表）；不得是任务所属 agent 本人（防自我审批架空人工闸门）；审批身份必须由服务端已认证会话（如 JWT current_uid）派生，禁止从客户端请求体透传。approve/reject 边的 `allowed_actors` 恒为 `["human_approver"]`。
- **过期即关闭**：awaiting_approval 超时进入 expired（终态）后，补到的批准/拒绝一律 `invalid_transition`，绝不补执行。expire 与并发决定竞争时同样 first-writer-wins。

## 5. 过期规则

`expired` 仅表示"等待人工动作超时"：`submitted` 超时未拾取、`awaiting_approval` 超时未决。执行中的超时（如 worker 心跳丢失）归并 `working + fail(timeout)`，不引入新的语义分支。expire 只能由 system 定时器从这两个源状态触发。

## 6. Terminal 规则

五个终态无出边、不可逆。终态上的一切动作都属于非法补集，默认拒绝（`invalid_transition`）：状态不变、无新副作用。实现层可把"重复同一终态请求"（例如对已 completed 的任务再发 complete）映射为幂等 already-in-place 响应，但这在状态机层面等价于拒绝迁移，不得改变状态或产生副作用。到达终态时必须回收该任务残留的待审登记，防止永久挂起。

## 7. 重启恢复

服务重启后**从持久化状态重放**，恢复过程本身不自动产生新的外部副作用（`restart_recovery.auto_new_side_effects=false`）。仅当挂起执行的工具可证明按幂等键去重时才允许恢复重试；无法证明幂等的执行采用 at-most-once，结果不确定时进入人工复核。

> 现状差距：当前 PoC 的状态与审批保存在 ETS，重启即丢失；本语义由 DATA-01 引入持久化后兑现（见 §10 现状差异）。

## 8. 执行语义（副作用与交付保证）

- **幂等键下沉**：每个会产生外部副作用的执行必须携带平台生成的幂等键，并将该键下传给工具/外部系统；任务状态、审批与投递的幂等键需有数据库唯一约束支撑，不能只依赖进程内 ETS。
- **调度一次**：IMBoy 对同一 execution 幂等键只调度一次；执行以 `approved → resume → working` 为唯一入口。
- **恢复重试的条件**：只有工具可证明幂等（按幂等键去重）时，崩溃恢复才允许重试。
- **默认保证**：无法证明幂等时为 at-most-once；结果不确定时进入人工复核，绝不自动重复副作用。
- **禁止的承诺**：不对外承诺无条件的"恰好一次"投递语义——本契约只承诺"按幂等键只调度一次 + 可证明幂等时可恢复重试；否则 at-most-once + 人工复核"。四份交付文件（JSON/md/verifier/tests）文本中均不得出现该类无法兑现的承诺字样；verifier 内置对应文本禁令作为机器检查项之一（禁令关键词在源码中以拼接方式构造，故源文件本身亦不含该字样）。

## 9. 非法迁移补集（默认拒绝）

补集定义（见 JSON `illegal_transition_policy`）：

```
illegal_complement = (states × actions_universe) − legal_edges
                   = 9 × 10 − 15 = 75 个 (状态, 动作) 对
```

代表性补集对（全部被 `evaluate()` 拒绝、返回 None）：

- 终态上的一切动作：`completed+approve`、`rejected+reject`、`expired+approve`、`cancelled+complete`、`failed+cancel`
- 非源头的审批/调度：`approved+approve`（已批准不能再批）、`working+approve`（未请求审批不可直接批）、`submitted+resume`（未经批准不可调度）
- 跨阶段跳跃：`working+start`、`submitted+complete`

verifier 逐对枚举补集并断言全部被拒；JSON 中 `default_outcome=rejected`、`state_change=false`、`side_effect=false` 削弱任一项都会 fail-closed。

## 10. 现状差异（现有代码 vs 本契约）

以下差异以任务卡九状态契约为目标记录，本卡不改任何 `src/` 产品代码；兑现归 DATA-01（持久任务）、MCP-02（task tools）等后续任务：

| # | 现状（事实来源） | 目标契约 | 兑现任务 |
|---|---|---|---|
| 1 | `agent_task_observer` 是事件观察器而非状态机：`emit/1` 不校验 from→to，任意 status 事件都接受（未知 status 静默忽略） | 迁移必须按矩阵校验，未声明组合默认拒绝 | DATA-01 |
| 2 | 现有词汇含 `progress` 事件类型 | progress 建模为 working 自环（边 #5），不是独立状态 | 已在矩阵内对齐 |
| 3 | `approved`/`rejected` 目前只是 ETS 决定记录，批准后无 approved→working 调度链路（demo 场景到卡片为止） | approved 是非终态，经 resume 调度执行 | DATA-01/MCP-02 |
| 4 | 无 `expired` 状态：审批无超时定时器；`barrel_mcp_tasks` 的 TTL sweep 只清理记录不做状态迁移 | submitted/awaiting_approval 超时进入终态 expired | DATA-01 |
| 5 | `barrel_mcp_tasks` 初始态直接 working（不建模 submitted），终态再迁移返回幂等 ok | 初始态 submitted；终态后再迁移默认拒绝（实现层可映射为幂等 already-in-place，等价于拒绝） | MCP-02 |
| 6 | MCP 任务词汇（barrel_mcp_tasks 注释）：submitted/working/completed/failed/cancelled，无审批三态 | 九状态是其超集：增加 awaiting_approval/approved/rejected/expired | 本契约 |
| 7 | 状态与审批存 ETS，服务重启丢失（demo driver 亦无真实 task bridge） | 从持久化状态重放恢复 | DATA-01 |
| 8 | 审批仲裁已实现 first-writer-wins（ets:insert_new 原子占位）+ already_decided + 群成员/非本人授权判据 | 与契约一致，作为既有语义保留 | 已对齐 |

## 11. 变更规约

本矩阵是冻结契约：任何状态、动作、边、终态、审批/过期/重启/执行语义的变更，都必须先修改本 JSON 与 verifier 冻结常量、同步测试正反例，并重新运行任务卡的全部 Verify 命令；只改文档不改 JSON 与 verifier 的行为无效。实现任务（DATA-01 等）发现矩阵缺口时，回到 FSM-00 走契约变更，不得在实现层私扩状态。
