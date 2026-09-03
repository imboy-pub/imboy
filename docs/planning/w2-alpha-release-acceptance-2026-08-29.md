# IMBoy Channel-first-class W2 — Alpha Release 验收报告（当前 HEAD）

> 版本：1.0 | 日期：2026-08-29 | Owner：总控 Agent（ZC-11）
> 计划：`docs/planning/imboy-channel-firstclass-w2-alpha-release-execution-plan.md`
> 台账：`docs/planning/channel-firstclass-w2-execution-ledger.md`（各卡完整证据链）
> **总判定：`release-candidate / 自动化全绿`。READY_FOR_ALPHA_RELEASE 待 ZC-12 人工门（H2/H3/H4）——本报告不含任何真机/真人/生产证据（尚不存在，如实标注 BLOCKED）。**

---

## 一、代码态（三仓均未 commit/push，Base SHA 未变）

| 仓 | Base SHA（=HEAD） | 未提交 W2 产物 |
|---|---|---|
| imboy（后端，VERSION=`1.0.0-alpha.70`） | `db41789a` | 43 项（迁移 00000081 成对、三卡四层 26 文件、ZC-05 整合、ZC-09R 修复、Demo B W2 脚本与报告、独立审查报告、CHANGELOG/VERSION/契约） |
| imboyapp（pubspec `1.0.0-alpha.16+6`） | `a18e89e5` | 29 项（W2 四页 + API/Model/Provider + i18n + 路由 + 58 用例 + error_code.dart 再生；其中 26 文件呈 index-staged 状态，内容与工作树一致） |
| imboyadmin（package.json `1.0.0-alpha.16`） | `46c7e10` | 5 项功能改动（service + ProjectDetailPage 治理 Tabs + 2 测试文件；evidence 副产物另计） |

## 二、Scope Contract 十二项 × 证据矩阵（计划 §6 口径）

| # | 能力 | Schema | Backend | Flutter | Admin | Tests | Demo | 人工证据 |
|---|---|---|---|---|---|---|---|---|
| 1 | project_member 表+DB 子集约束 | ✅ 00000081 | ✅ 四层 | ✅ 成员页 | ✅ 治理 Tab | ✅ schema 11 + member 55 | ✅ 66 断言内 | ⛔ 真机 BLOCKED |
| 2 | Member 管理 API（幂等） | — | ✅ | ✅ | 只读 | ✅ 含并发竞争 | ✅ | ⛔ |
| 3 | W2 授权模型（403/Guest） | ✅ 触发器 | ✅ | ✅ ForbiddenView | ✅ fail-closed | ✅ 三层实证 | ✅ B 直访 403 | ⛔ |
| 4 | project_milestone 表 | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ⛔ |
| 5 | Milestone API（reach 幂等） | — | ✅ | ✅ | ✅ | ✅ 含并发 | ✅ | ⛔ |
| 6 | project_channel_rel 表 | ✅ | ✅ | ✅ | ✅ | ✅ 跨 WS 23503 | ✅ | ⛔ |
| 7 | Channel 关联 API | — | ✅ | ✅ | ✅ | ✅ 幂等/并发 | ✅ | ⛔ |
| 8 | Pinned 聚合（排公告） | 零新表 ✅ | ✅ | ✅ | 只读 ✅ | ✅ | ✅ | ⛔ |
| 9 | Resources（links+授权） | ✅ links 列 | ✅ | API 层 ✅（UI 入口无，见台账 ZC-06） | 只读 ✅ | ✅ | ✅ | ⛔ |
| 10 | Activity（事件 CHECK 扩展） | ✅ | ✅ | ✅ | 只读 ✅ | ✅ 无正文 | ✅ | ⛔ |
| 11 | Related Posts | 零新表 ✅ | ✅ | ✅ | 只读 ✅ | ✅ 有界 | ✅ | ⛔ |
| 12 | 三端闭环+文档版本 | — | ✅ | ✅ | ✅ | ✅ | ✅ 双遍 | ⛔（=ZC-12） |

> Resources 的 Flutter 侧 links 编辑 UI 未做（TDD 用例未要求，API 层已实现）——不阻塞证据矩阵（Admin 侧只读 + Demo 已覆盖 update-links 放行验证）。

## 三、自动化验收总账（ZC-10，2026-08-29 实测）

| 仓 | 验证 | 结果 |
|---|---|---|
| imboy | `make eunit-local` 全量 | **6490 passed / 0 failed** |
| imboy | compile / 模块边界 / Contract Gate | 全 PASS（630 endpoints 真源一致） |
| imboy | Demo B W2 双遍（ZC-09R 后重验） | **66/66 ALL PASS ×2**，teardown 残留=0 |
| imboyapp | `dart analyze lib` | No issues（零基线保持） |
| imboyapp | `flutter test` 全量 | **5940 passed / 0 failed**（239 skipped 既有 quarantine） |
| imboyadmin | `bun test` / build / lint | **1410/0**，全 0 |
| imboyadmin | E2E | 100 passed + avatar 1 failed（**Base SHA 干净 worktree 硬对照同样 failed → 既有环境型，非本计划新增**） |

独立安全审查（ZC-09）：首判 FAIL（HIGH=1 空转判绿 67 用例）→ ZC-09R 修复（三套件真实执行 35/21/14 绿 + reach 并发守卫 + actor 复检 + owner 读 fail-closed + admin COUNT）→ 复核 **PASS，CRITICAL=0 / HIGH=0**。MEDIUM 豁免状态：M-4/M-5/M-6 + LOW×8 待用户书面豁免（M-2/M-3/M-7/M-1 已修复）。

## 四、人工门状态（计划 §1.3）

| Gate | 状态 | 说明 |
|---|---|---|
| H0 工作树 | ✅ 放行（2026-08-29 用户"继续"） | 三仓 Base SHA 与产物归属确认 |
| H1 W2 Scope | ✅ 放行（同上） | 十二项 Contract 签认 |
| H2 外部测试 | ⛔ **BLOCKED** | 待用户提供：两台真机、测试账号、测试人员及联系方式 |
| H3 生产等价演练 | ⛔ **BLOCKED** | 待用户确认环境/数据来源/访问授权 |
| H4 Release | ⛔ **BLOCKED** | 待用户确认 git 身份、目标远端、tag 与发布渠道 |

## 五、已知限制与遗留（不阻塞 release-candidate）

1. avatar E2E 既有环境型失败（ai_agent 上传链路，Base SHA 已复现）——独立排查项；
2. Admin 治理写端点未冻结（治理面只读，缺口清单在台账 ZC-07 节）；
3. 读权限语义差异（ws_owner 治理读权是否延伸 milestone/channel）待用户拍板；
4. M-4/M-5/M-6 空转存量与吞错形态——待豁免或后续治理；
5. `imboy_ctl user create` 40 字符静默失败、milestone due_date 元组序列化——ZC-08 发现，待立项；
6. imboyapp 26 文件 index-staged 异常（内容无害，commit 时注意 `git diff --cached`）；
7. 三仓共 268+ 笔历史提交未 push（既有状态，本计划不处理）。

## 七、H4 提交执行准备（2026-08-29 补充）

已产出 `docs/planning/w2-h4-commit-execution-plan.md`：三仓共 13 笔提交的完整命令序列（imboy 9 / imboyapp 2 / imboyadmin 2，逐笔 pathspec + message，按计划固定合并顺序），push 与 tag 单独分离为第三重授权批次。**未执行**——等待用户确认 git 身份、目标远端与执行方式。

## 六、Release 判定

**当前 = `release-candidate / BLOCKED(H2,H3,H4)`。**
自动化门全绿、独立审查 PASS、迁移演练两轮独立通过、Demo 双遍全绿；真机双人 Demo、真人 30 秒理解测试、生产等价迁移演练、干净部署、push/tag 授权全部待人工（ZC-12）。在这些证据产生之前，任何人不得声称 Alpha Release 正式发布。

## 八、当前 HEAD 勘误（2026-09-03）

本报告第一至七节保留 2026-08-29 的历史验收快照；当前事实以本节与执行台账最新追加卡为准。

- 后端 HEAD 为 `6ac390f9`，本轮 5 个提交已落库、工作树干净；“三仓均未 commit”和旧 Base SHA 不再代表当前状态。
- Workspace Owner 的治理读取语义已统一：可读取 Workspace 内 Project/Milestone/Channel 治理资源；内容写仍要求 active Project Member。原“待用户拍板”限制已解除。
- Project 列表、Task、Channel、Milestone 的成员权限已在共享权限层收敛；COUNT 失败按 fail-closed 处理；旧重复权限 helper 已删除。
- 当前后端全量 `make eunit-local`：**6723 passed / 0 failed**（exit 0）；此前 C1/C2 或 34/59 失败数字仅是历史基线，不得用于描述当前 HEAD。
- M-4/M-5 已闭环：分页 COUNT、成员/里程碑/频道事务查询错误显式上抛，成功写后的成员与里程碑回读均在事务内完成；DB 故障不再伪装成空列表、403/404 或成功提交。
- `imboyapp` 当前存在 22 项用户暂存改动，本轮未修改；`imboyadmin` 工作树干净。
- Release 判定仍为 **`release-candidate / BLOCKED(H2 残余, H3, H4)`**；本地代码和测试不能替代第二台真机、Push 外部凭据、3 人理解测试、生产等价演练及对外发布授权。
