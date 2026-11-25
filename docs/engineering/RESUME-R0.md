# RESUME — R0 容量治理（P1-1：VM 上限 × WebSocket 连接上限）

> R0 车道的**唯一续接入口**。任何新会话从这里开始。
> 状态（2026-11-25）：PR #78 的 review 修正已推送；合并状态以 GitHub PR #78 为准。属主指令为"合并成功后清理本会话 worktree 与分支"，清理完成后本文件即车道的历史记录。
> 工作位置（清理前）：分支 `fix/r0-capacity-limits-20261123` @ `_task_worktrees/R0_CAPACITY_20261123/imboy`（基于 main `a5c74219f`）。

## 续接协议

1. 依序读：本文件 → `r0-capacity-status.json` → `r0-capacity-remediation-report.md`（§2.1、§3.1）→ `r0-capacity-test-results.md`（§1、§4、§5）。
2. 检查实际 git 与 worktree 状态；不重复已完成工作；不覆盖未提交修改；不用 `git reset --hard`/`git clean`。
3. 合并与后续动作以属主指令为准（2026-11-25 属主指令：review、修复、提交、合并到 main、合并后清理 worktree 与分支）。
4. 主检出（`/Users/leeyi/project/imboy.pub/imboy`）上有 `prodready_e2e` 节点运行（HTTP :9800）——不要在主检出构建；进程清理只用**精确 PID**，停止节点用 RPC `init:stop`（release 带 `-heart`）。隔离 release 节点用 `VMARGS_PATH` 指向改了 `-name` 的 vm.args（`NAME=` 在 `foreground` 模式不生效）。

## 一句话现状

R0 全阶段完成并经 PR review 修正：属主 D0–D8 已批（设计 §13）；实施含进程生命周期修复（6825219c2）与 review 修正（282c3c57c `.env` 可 source、ff15a3eae 定时器句柄）；机制验证 V1–V17 全过；阶梯 2k–12k 客户端口径 + 服务端稳态天花板 ≈1.7k（recorder 三轮复现）；**20,000 绝对值未验证，不得写成实测容量**。

## 文件地图（全部在 docs/engineering/）

| 文件 | 内容 |
|---|---|
| `r0-capacity-remediation-design.md` v1.1 | 设计 + §13 批准记录 + §11 勘误（takeover 换进程、`.env` 引号、定时器句柄等） |
| `r0-capacity-remediation-report.md` | 总结报告：D0–D8 实施对照 + 技术发现（含 review 两项）+ 遗留建议 |
| `r0-capacity-test-results.md` | V1–V17 机制矩阵 + 阶梯 + 限制声明 + 事故披露 + PR review 归因 |
| `r0-capacity-rollback.md` | 逐单元 revert + 运行期回退矩阵 |
| `r0-capacity-status.json` | 机器可读状态 |
| `r0-probe-evidence/runs/` | 阶梯日志、recorder 轨迹 |
| 其余 erlang-*/RESUME.md | 第一轮审计原件（保留） |

## 待属主动作

1. **PR #78 的合并与评审后续**（以 GitHub 为准）。
2. （可选）生产节点执行设计 §2 只读命令回填 → 升级 PRODUCTION_NOT_VERIFIED。
3. （可选）遗留任务立项：syn 陈旧行、/metrics JSON 直方图崩溃、bench 鉴权契约刷新、imboy_online_users 语义、镜像构建后 vm.args 检视、`msg_rate_logic` 定时器守卫、main 基线 CI 红灯（其他车道）。
