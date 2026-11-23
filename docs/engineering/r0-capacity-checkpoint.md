# R0 检查点 / CHECKPOINT

> 最后更新：2026-11-23 · R0 续接三件套之一（`RESUME-R0.md` → `r0-capacity-status.json` → 本文件）

## 当前阶段

**阶段 D 评审门：设计已出，等待属主批准决策点 D0–D8。未修改任何代码或配置，未进入阶段 E。**

## 已完成

- **A**：读取 5 份第一轮审计状态文件；核对 git 基线（main = 审计锚点 `a5c74219f`，对三远端均 56/0）；7 个 worktree 在 R0 文件上全部干净，无并发修改；建立独立分支与 worktree；第一轮 10 份审计文档逐字节带入。
- **B1**：VM 参数全链路追踪——5 个来源 3 套数值；`+P`/`+Q` 向上取整到 2 的幂；覆盖优先级实测（`ERL_FLAGS` > args_file > `ERL_AFLAGS` > 未文档化的 `ERL_MAX_PORTS`）。
- **B2**：生产核验标注为 **PRODUCTION_NOT_VERIFIED**（未获授权入口，未尝试连接）。
- **B3**：隔离微实验取得容量模型数据（证据见 `r0-probe-evidence/`）。
- **C / D**：`r0-capacity-remediation-design.md`（10 个必备章节 + 勘误 + 决策点）。

## 对第一轮审计的关键修正（详见设计文档 §11）

1. `websocket_options/0` 的两个 `infinity` 是流级死配置。
2. ranch `max_connections` 约束不了已建立的 WebSocket（cowboy 在升级时调用 `ranch:remove_connection/1`）→ 需要**应用层 WS 准入**。
3. `+P 20480` 实际为 32768。
4. 蓝绿 / native-shell 路径的 VM 上限远高于 docker 镜像路径。
5. compose 的 `ERL_MAX_PORTS` 是死配置。
6. `imboy_ws_connections_current` 不含任何已建立的 WS。

## 未完成（均需属主批准后才能开始）

| 阶段 | 内容 |
|---|---|
| D | 属主批准 D0–D8；回填生产只读核验结果（可选但强烈建议） |
| E | E1 VM 参数默认值；E2 监听器显式选项 + 删除死键 + WS 准入；E3 install.sh / compose 注入；E4 测试 T1–T8 |
| F | 容量阶梯 R1–R6（隔离本地环境） |
| G | 验收；`r0-capacity-remediation-report.md`、`r0-capacity-test-results.md`、`r0-capacity-rollback.md`；申请合并 |

## 测试结果

- `make` / EUnit / release 构建：**未运行**（A–D 为只读审查与设计）。
- 微实验 4 轮（客户端退出码均 0）：结果与证据索引见 `r0-capacity-status.json` 的 `test_results` 与 `r0-probe-evidence/README.md`。

## 未解决问题

见 `r0-capacity-status.json` 的 `open_issues`（生产未核验、Docker 镜像产物未检视、真实 imboy 基线未测、本机单客户端 ~16.2k 上限、`python3 websockets` 未安装、syn 注册面待核实、cookie 占位值另立任务）。

## 当前 Git 状态

- 分支 `fix/r0-capacity-limits-20261123`，worktree `_task_worktrees/R0_CAPACITY_20261123/imboy`，基于 `a5c74219f`；**未推送、无 PR**。
- 分支上仅有文档提交（`git log --oneline main..HEAD` 可核）。
- 主检出零改动：main @ `a5c74219f`，未跟踪 `docs/engineering/`（第一轮审计原件）与 `docs/research/`；主检出 `_rel/` 上有 `prodready_e2e` 节点在运行，**不得在主检出执行任何构建**。

## 下一步动作（默认）

等待属主批准。若获批准，从阶段 E1 开始（先在 worktree 内准备构建环境：主检出不可用于构建）。
