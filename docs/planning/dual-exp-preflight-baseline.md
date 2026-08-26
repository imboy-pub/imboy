# Preflight 0 — 三仓基线与隔离记录（双体验 v2.5.2）

> 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md`（v2.5.2）
> 模式：**全自动无人值守（unsafe_experiment）**——Gate 0 / Gate W 由用户在 goal 指令中明确授权跳过人工确认；产出不得声称 Release / 工程 DoD / 客户验收。
> 执行时间：2026-08-26 19:27 +0800

## 1. 三仓基线表

| 仓库 | 基线 HEAD | 分支 | 基线状态 | 执行分支 | 隔离方式 |
|---|---|---|---|---|---|
| imboy（后端主仓） | `d4153c30d069e3cf38d7326bf876c0b9a1eb70a1` | main | **干净**（porcelain 空） | `dual-exp-v21` | 主工作区切分支 |
| imboyapp（Flutter） | `5c06cc7be8599943150a2f5affa524e6e255fc3a` | main | **脏**（用户并发 WIP：数十文件 staged+unstaged，含 `lib/component/**`、`assets/migrations/*`、`ios/**`） | `dual-exp-v21` | **git worktree 隔离**（`.worktrees/imboyapp`） |
| imboyadmin（管理后台） | `97b60cb77d95b117bf4d0355227b69a41ef44ee2` | main | **干净**（porcelain 空） | `dual-exp-v21` | 主工作区切分支 |

基线日期：2026-08-26。远端：三仓 origin 均指向 gitee.com/imboy-pub/*（另有 github/gitcode 多远端）。

## 2. 用户 WIP 保护（不得 reset/restore/clean）

- imboyapp 主工作区的全部未提交改动**原样保留**，本计划所有 imboyapp 工作在 worktree `/Users/leeyi/project/imboy.pub/.worktrees/imboyapp`（干净检出 5c06cc7b）进行。
- 附加收益：lefthook `dart-analyze` 扫全项目的门禁在 worktree 内只看到本计划改动，不再被用户主区 WIP 拦截（此为 2026-08-24 已知问题）。
- imboy / imboyadmin 切分支时工作区干净，无 WIP 需保护。

## 3. 合并责任人

- **主会话（编排者）**为唯一合并责任人：所有 subagent 只在指定仓库/目录提交（pathspec commit），不执行 merge/rebase/push。
- 全程不 push、不 force push、不 merge 到 main；`dual-exp-v21` 分支保留待人工决定（无人值守红线）。

## 4. 共享文件合并顺序

| 共享文件 | 唯一合并时序 | 规则来源 |
|---|---|---|
| `imboy/src/imboy_router.erl` | 仅 WP4 归档子项（T7）按 T4/T6a/T6b 提交的路由片段清单统一合并 | 计划 §8 agent_rules |
| `imboyapp/lib/config/router/*`（workspace_routes 挂载） | WP5（T8）独占创建；WP6 只读引用不改挂载点 | T8 OWN |
| `imboyapp/lib/page/workspace/` 目录 | WP5（T9）与 WP6（T10a/T10b）子目录不重叠：`project/` 归 WP6，其余归 WP5；两者串行派发规避冲突 | T9/T10a OWN |
| `imboyadmin` 菜单注册 | 仅 T11b 统一完成 | T11 OWN |
| `imboy/priv/migrations/` | 仅 WP2（T3）写入；后续 WP 只读 | T3 OWN |

## 5. Preflight 结论

四项（基线表 / 隔离方式 / 合并责任人 / 共享文件合并顺序）齐备 → **通过**，进入 WP0。

无人值守代价声明（用户已知悉）：
1. Gate 0 被跳过 = 无真实客户验证即投全部工程（计划 §五 NO-GO 保护失效）。
2. Recon（WP0）无人复核 = R2/R3 结论若错，错误地基会一路建到 WP8 验收才暴露。
