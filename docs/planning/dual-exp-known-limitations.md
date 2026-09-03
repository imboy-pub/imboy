# 双体验 v2.5.2 — 公开已知限制清单（WP8/T15 收口）

> 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md` §七 T15④ / §9.4
> 分支：imboy `dual-exp-v21` | Flutter worktree `.worktrees/imboyapp` `dual-exp-v21` | imboyadmin `dual-exp-v21`
> 模式：**全自动无人值守（unsafe_experiment）**。本清单为 Day-1 Bar 的"可告知缺陷 100% 入册"载体；
> 不代表 Release / 工程 DoD / 客户验收结论。
> 收集范围：WP0-WP7 各任务卡交付物、Decision Brief、T3 迁移演练报告、WP8/T13-T15 实测结果。

---

## A. 归档写守卫（T7）覆盖边界

### A1. 数据库写路径已事务化；对象存储仍有跨系统边界
Workspace 数据库写路径现统一复用 `ensure_writable_tx/2`、`write_tx/2` 或
`write_tx_or_skip/2`：业务写与 `SELECT ... FOR UPDATE` 在同一事务内完成，归档与数据库写
线性化。派生已读计数也在事务内守卫，归档后读取仍成功但不再更新计数。

**残留风险**：群文件/相册的对象存储上传无法与 PostgreSQL 归档事务形成跨系统原子提交。
最终附件落库有事务守卫，不会在归档后新增 Workspace 数据库记录；但上传已完成、落库被归档
拒绝时可能留下未被引用的对象，需由现有对象清理策略回收。

### A2. Personal 路径与 Workspace 写入口的边界
- webhook incoming 复用 `channel_logic_message:publish_message/3`，最终由
  `channel_ds:publish_message` 的事务守卫覆盖；
- group/channel 附件确认在事务内校验 Workspace 可写状态；
- 通用 Bot API 当前只发送 C2C 消息，属于 Personal 域，不是 Workspace 写入口；
- c2c/moment/private/public 附件按当前 schema 与 ACL 均属于 Personal 域，不做
  Workspace 归属回溯。

因此这些 Personal 路径不应计为“未接入 Workspace 守卫”。若未来允许 Bot 或上述附件类型
绑定 Workspace，必须先扩展显式归属字段与 resolver，再接入同一事务守卫。

## B. 数据迁移与升级（T3）

### B1. erlang_migrate 部分回滚缺陷（上游库缺陷）
`erlang_migrate.erl:378-381`：`apply_down` 的 `PrevVersion` 取自本次回滚 sublist 的剩余项；
部分回滚时 sublist 耗尽 → `set_version(undefined)` = 清空 tracking。**生产禁止直接用
`erlang_migrate:down(Config, N)` / `goto` 到中间版本**；必须 down 后立即
`erlang_migrate:force(Config, <目标版本>)` 校正（实测有效，见 T3 报告 §2.4）。

**上游修复已就绪（2026-08-27）**：根因实为 `down(N<all)/goto` 的 PrevVersion 从 sublist 内部推导——滚完 N 份 Rest 耗尽即 undefined，PG driver 按约定清空整条升级历史。本地 `erlang_migrate` 仓分支 `fix/partial-down-tracking`（base_version_for 贯穿 + 3 个语义钉子用例，全仓 110/110 绿）已修；imboy 切换需更新 deps.mk 的 pin 到该分支 commit 并回归 T3 演练后再用于生产回滚预案。

### B2. 00000074/75 无 down 文件（历史遗留）
74/75 为上游合并基线既有状况，非本计划引入；整体回滚到 ≤73 需手工脚本。

### B3. 索引在线创建受限
`erlang_migrate` 对每份迁移恒包事务 → `CREATE INDEX CONCURRENTLY` 不可用。
本批 4 个部分索引用普通 `CREATE INDEX`＝需发布窗口。**生产规模（行数量级/峰值
写入/锁时长）取证 BLOCKED——无人值守无生产访问授权，窗口终判须由运维在授权后补数据。**

### B4. 全量历史从零重放未被证明为"完全等价"
foundation 迁移系合并基线（非逐份原始演进），`CREATE FUNCTION` 无 OR REPLACE 的
重复执行风险在 T3/T15 演练中通过全新空库验证通过（clean replay OK），但对"曾手动
改动过 schema 的存量库"不承诺零冲突。

## C. 测试基线红灯（对账口径）

### C1. Flutter 上游共同红灯 161 条（非本计划引入）
R1 基线记录为 main@5c06cc7b（2026-08-26）5545 通过/44 失败。执行期间上游 main 演进
至 25da8328（2026-08-27 上午），该 tip 自身全量即有 **162 失败**（对照组，JSON reporter
同口径实测）。dual-exp-v21 分支修复自身引入的 2 个回归（route_registry 缺登记 4 条
Project/Tasks 路由；a11y 高可见位置 3 处 Material Icons）后，剩余失败集合与上游 tip
完全一致（161 common + 差异为测试集合本身不同），符合"失败不增（相对同期上游）"。
T13 收口复测（编排者，compact 口径）：全量 **+5565 / ~242 / -165**——通过数较 R1 基线
净增 20（新增测试贡献）；165 失败中 dual-exp 命名空间（workspace/chat_shell/project/
smoke/api）单跑 **265 用例全绿**（0 失败），与 C1 对照实验结论一致。
**对 R1 原始数字（44）的红灯扩大由上游演进贡献，归属登记如下，供验收 Agent 复核。**

### C2. 后端既有失败基线 59 条
本地 `make eunit-local` 全量存在 59 个基线失败（R1 记录）；WP8 新增 W0 schema 断言
套件当前 4/4 全绿，新增功能零失败。

### C3. CI 大面积红灯
远端 CI 长期红（跨仓既有问题，与本分支无关的具体 job 清单未逐一复核——无人值守
期间以本地全量对账为准绳）。

## D. Demo 与走查的 BLOCKED 项（Day-1 Bar 缺口）

| 项 | 状态 | 说明 |
|---|---|---|
| 两台真机 Chat 回归（Demo A 音视频） | **BLOCKED** | 无人值守环境无真实真机与人工操作者。以 T13 三仓全量对账数字中 msg/e2ee/channel/group 相关子集作 API 层证据；音频视频媒体双向结果无证据 |
| 生产 Docker 部署演练 | **BLOCKED** | 生产/类生产环境访问需人工授权（红线）；降级为本机干净库演练（imboy_t15_rehearsal 从零迁移到 78 + 启动 + init 接口验证） |
| 升级提示端到端（真机弹窗） | **BLOCKED** | 本地库 `app_version` 存在 15 条 status=1 记录（最高 vsn=1.0.9, force_update=f, min_supported_vsn=0.0.0——编排者复核修正：原记录误写 1.0.15），客户端 pubspec 版本串 `1.0.0-alpha.15+6`；1.0.9 > 1.0.0-alpha.15，链路服务端侧配置齐备，但 UI 弹窗端到端需真机 |
| 规模化生产迁移窗口判定 | **BLOCKED** | 见 B3 |

## E. 产品语义与实现偏差

### E1. invite 可选关系编排（已覆盖）
后端 Workspace Member invite 仍只建立 `workspace_member`，不把 Group Member 或 Channel
Subscriber 隐式合并进同一接口。Flutter 邀请向导已提供“加入 General”和“订阅
Announcements”两个独立选项：主邀请成功后分别执行、分别显示结果，失败项可单独重试。
这既保持三种关系的权限边界，也避免邀请后必须再到多个页面逐项操作。

### E2. Task assignee 的 Project Member 边界（已修复）
W2 下任务负责人必须同时是 active Workspace Member 与 active Project Member；创建任务和
变更负责人均在同一事务内复用该校验，不再允许把仅属于 Workspace、未加入当前 Project 的
用户指派为负责人。未指派任务仍允许，非法指派由 API 400 拒绝并透出消息。

### E3. change_role 最后 Owner 并发保护（已修复）
`change_role` 先锁定 Workspace 行，再在同一事务内重新验证操作者角色、目标成员状态与
active Owner 数量，避免并发降级最后 Owner，也避免等待锁期间操作者已被降级后继续执行治理
操作。修复提交：`d4cbe404`。

### E4. 频道创建后首帖的创建者角色兜底（已覆盖）
当前 `channel_admin_repo:get_role/2` 直接查询数据库，不经过 `imboy_cache`；即使角色查询
返回 0，`channel_logic_common:get_user_role/2` 仍会按频道 `creator_uid`/`owner_id` 将创建者
识别为 Owner。发布回归测试覆盖“角色为 0 后立即首帖成功”。Demo B 的 3 秒重试保留为
兼容防护，历史 transcript 中的失败不能作为当前 main 仍存在缓存竞态的证据。

### F. 其他
- **produce 规模迁移 BLOCKED 待人工**：发布窗口/锁预算终判需生产数据（B3/D 表重申）。
- 本仓库 license 提示"试用期已结束，降级社区版"（日志可见）——社区版运行不影响
  功能演练，但商业化档位相关开关与验收无关项未启用。

---

*最后更新：2026-09-03（WS-02/WS-03 修订；验收 Agent 应对本清单逐条复核后并入 dual-exp-acceptance.md）*
