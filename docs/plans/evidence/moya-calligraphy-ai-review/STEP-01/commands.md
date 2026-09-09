# STEP-01 命令记录

所有命令在标注的工作目录执行，采集时间 2026-09-09 18:17:23 +0800。

| # | 工作目录 | 命令 | 退出码 | 摘要 |
|---|---|---|---|---|
| 1 | /Users/leeyi/project/imboy.pub/imboy | `git rev-parse --show-toplevel` | 0 | `/Users/leeyi/project/imboy.pub/imboy` |
| 2 | 同上 | `git rev-parse HEAD` | 0 | `5b7e2055f71087930eb4e896d1ce0f895140d614` |
| 3 | 同上 | `git branch --show-current` | 0 | `main` |
| 4 | 同上 | `git remote -v` | 0 | gitcode + gitee（记录，不操作） |
| 5 | 同上 | `git status --short` | 0 | 8 个 `??` 未跟踪，无已跟踪修改 |
| 6 | 同上 | `ls priv/migrations/ \| sort \| tail -5` | 0 | 最新 `00000094_moderation_appeal` |
| 7 | /Users/leeyi/project/imboy.pub/moya | `git rev-parse --show-toplevel` | 0 | `/Users/leeyi/project/imboy.pub/moya` |
| 8 | 同上 | `git rev-parse HEAD` | 0 | `9644a2e804ecabc232fd313d87f7825339b4aceb` |
| 9 | 同上 | `git branch --show-current` | 0 | `main` |
| 10 | 同上 | `git remote -v` | 0 | 空输出（无 remote） |
| 11 | 同上 | `git status --short` | 0 | 2 个 `??`（moyalogo PNG ×2） |
| 12 | /Users/leeyi/project/imboy.pub | `git rev-parse --show-toplevel` | 128 | `fatal: not a git repository`（预期，证明聚合根非 git 仓） |
| 13 | 同上 | `date '+%Y-%m-%d %H:%M:%S %z'` | 0 | `2026-09-09 18:17:23 +0800` |
| 14 | imboy | `grep -n "organization" priv/migrations/00000076_workspace_foundation.up.sql` | 0 | 第 8 行注释确认"I11：不引入 Organization 层"历史事实 |

无写操作、无 git 变更命令、无数据库连接。
