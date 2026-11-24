# RESUME — R0 容量治理（P1-1：VM 上限 × WebSocket 连接上限）

> R0 车道的**唯一续接入口**。任何新会话从这里开始。
> 工作位置：分支 `fix/r0-capacity-limits-20261123` @ `_task_worktrees/R0_CAPACITY_20261123/imboy`（基于 main `a5c74219f`，8 提交至 `6825219c2`）。

## 续接协议

1. 依序读：本文件 → `r0-capacity-status.json` → `r0-capacity-checkpoint.md` → `r0-capacity-remediation-report.md` → `r0-capacity-test-results.md`。
2. 检查实际 git 与 worktree 状态；不重复已完成工作；不覆盖未提交修改；不用 `git reset --hard`/`git clean`。
3. **PR 已创建待属主审阅：禁止自行合并/推送其他分支。**
4. 主检出（`/Users/leeyi/project/imboy.pub/imboy`）上有 `prodready_e2e` 节点运行（曾在阶段 E 被误杀两次、均已恢复）——不要在主检出构建；进程清理只用**精确 PID**（argv 模式会误中主检出节点，教训在 test-results §4.1）。

## 一句话现状

R0 全阶段完成：属主 D0–D8 已批（设计 §13），实施 8 提交含进程生命周期修复（6825219c2）；机制验证 V1–V13 全过；阶梯 2k–12k 客户端口径 + 服务端稳态天花板 ≈1.7k（recorder 三轮复现）；**20,000 绝对值未验证，不得写成实测容量**。

## 文件地图（全部在 docs/engineering/）

| 文件 | 内容 |
|---|---|
| `r0-capacity-remediation-design.md` v1.1 | 设计 + §13 批准记录 + §11 勘误（takeover 换进程等） |
| `r0-capacity-remediation-report.md` | 总结报告：D0–D8 实施对照 + 两大技术发现 + 遗留建议 |
| `r0-capacity-test-results.md` | V1–V13 机制矩阵 + 阶梯 + 限制声明 + 事故披露 |
| `r0-capacity-rollback.md` | 逐单元 revert + 运行期回退矩阵 |
| `r0-capacity-status.json` | 机器可读终态 |
| `r0-probe-evidence/runs/` | 阶梯日志、recorder 轨迹 |
| 其余 erlang-*/RESUME.md | 第一轮审计原件（保留） |

## 待属主动作

1. **审阅 PR 并决定是否合并**（属主专属动作）。
2. （可选）生产节点执行设计 §2 只读命令回填 → 升级 PRODUCTION_NOT_VERIFIED。
3. （可选）遗留任务立项：syn 陈旧行、/metrics JSON 直方图崩溃、bench 鉴权契约刷新、imboy_online_users 语义、镜像构建后 vm.args 检视。
