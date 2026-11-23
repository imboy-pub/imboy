# RESUME — R0 容量治理（P1-1：VM 上限 × WebSocket 连接上限）

> R0 车道的**唯一续接入口**。任何新会话从这里开始。
> 工作位置：分支 `fix/r0-capacity-limits-20261123` @ `_task_worktrees/R0_CAPACITY_20261123/imboy`（基于 main `a5c74219f`）。

## 续接协议

1. 依序读：本文件 → `r0-capacity-status.json` → `r0-capacity-checkpoint.md` → `r0-capacity-remediation-design.md`。
2. 检查实际 git 与 worktree 状态，对照文件验证上次进度；只继续未完成的工作。
3. 不重复已完成阶段；不覆盖未提交修改；不使用 `git reset --hard` / `git clean`。
4. **当前处于等待属主审批的评审门：未经明确批准 D0–D8，禁止自行进入阶段 E。**
5. 主检出（`/Users/leeyi/project/imboy.pub/imboy`）上有 `prodready_e2e` release 节点在 `_rel/` 运行，且第一轮审计原件以未跟踪状态留在那里——**不要在主检出构建、移动或删除任何文件**。

## 一句话现状

阶段 D 评审门：设计文档已出，**未改任何代码/配置**。阶段 B 取证推翻了任务书的一处前提——ranch `max_connections` 约束不了已建立的 WebSocket（cowboy 升级时调用 `ranch:remove_connection/1`），给 WS 设上限必须做应用层准入，需属主批准范围调整（D0）。

## 文件地图

| 文件 | 内容 |
|---|---|
| `r0-capacity-remediation-design.md` | 评审稿：证据 / 核验状态 / 容量模型 / 候选参数 / 注入方案 / 兼容性 / 风险 / 测试 / 回滚 / 勘误 / **决策点 D0–D8** |
| `r0-capacity-status.json` | 机器可读状态 |
| `r0-capacity-checkpoint.md` | 检查点 |
| `r0-probe-evidence/` | 微实验源码、4 轮运行报告、标志速测结果 |
| `erlang-*.md/json`、`RESUME.md` | 第一轮审计原件（保留；勘误以追加方式标注） |

阶段 G 产出（尚未创建）：`r0-capacity-remediation-report.md`、`r0-capacity-test-results.md`、`r0-capacity-rollback.md`。

## 待属主批准的决策（设计文档 §12）

| # | 决策 | 推荐 |
|---|---|---|
| D0 | 范围调整：新增应用层 WS 准入（`websocket_handler:init/2` 热路径） | 同意 |
| D1 | WS 上限：8,192 / **20,000** / 50,000 | 20,000 |
| D2 | docker/dev 默认 `+P`/`+Q` = **65536/65536**；蓝绿与 native-shell 不改数值 | 同意 |
| D3 | `+S`：**保持现状 + 可注入** / 交给 ERTS 缺省 / 固定 4 | 保持 |
| D4 | HTTP 阶段上限：**显式钉住 1024×10** / 收紧到 2,048 | 钉住 |
| D5 | WS 拒绝方式：**升级前 HTTP 503 + Retry-After** / 升级后 1013 | 503 |
| D6 | 同批修复 WS 连接数指标 + 水位告警 | 同意 |
| D7 | 同批删除 compose 死配置 `ERL_MAX_PORTS` | 同意 |
| D8 | docker 路径 cookie 占位值（W6-A02）另立任务 | 另立 |

另请属主（可选但强烈建议）在生产节点执行设计文档 §2 的只读命令并回填，以便把状态从 PRODUCTION_NOT_VERIFIED 升级。

## 获批后从哪开始

1. 在 worktree 内准备构建环境（主检出不可用）。
2. 阶段 E，按逻辑单元独立提交：E1 VM 参数默认值 → E2 监听器显式选项 + 删除死键 + WS 准入 → E3 install.sh / compose 注入 → E4 测试 T1–T8。
3. 阶段 F 容量阶梯（隔离本地环境；>16k 需多客户端节点或 Linux 压测机；`bench_websocket.sh` 依赖的 `python3 websockets` 本机未安装，需先解决）。
4. 阶段 G 验收 → 三份收尾文档 → 申请合并（不擅自合并、不直接推 main）。
