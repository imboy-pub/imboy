# 双体验 v2.5.2 — 公开已知限制清单（WP8/T15 收口）

> 计划：`.claude/PRPs/plans/imboy-dual-experience-v21-channel-firstclass.plan.md` §七 T15④ / §9.4
> 分支：imboy `dual-exp-v21` | Flutter worktree `.worktrees/imboyapp` `dual-exp-v21` | imboyadmin `dual-exp-v21`
> 模式：**全自动无人值守（unsafe_experiment）**。本清单为 Day-1 Bar 的"可告知缺陷 100% 入册"载体；
> 不代表 Release / 工程 DoD / 客户验收结论。
> 收集范围：WP0-WP7 各任务卡交付物、Decision Brief、T3 迁移演练报告、WP8/T13-T15 实测结果。

---

## A. 归档写守卫（T7）覆盖边界

### A1. "检查-写窗口"残留路径（R3 #9/#10/#11/#13/#17）
`workspace_guard` 提供两档守卫：
- `ensure_writable_tx/2`（事务版）：业务写同一事务内 `SELECT ... FOR UPDATE` 与归档线性化，
  **无漏写窗口**——已接入 `project_ds`、`project_task_ds`、`channel_ds`、`msg_c2g_repo` 四个主写入口；
- `ensure_writable/1`（自动提交版）：读状态与业务写不在同一事务，存在**检查-写窗口**
  （归档可插在检查之后、写入之前）。按 WP4 决策仅用于 R3 清单中 #9-#13/#17 这类
  无法进同事务的最小可行接入点（如部分 REST 写 handler 的 logic 层前置检查）。
  **残留风险**：归档瞬时并发下这类路径可能漏拒一次写（下次写会被拒）。渗透测试
  （`workspace_guard_tests`/`workspace_boundary_tests`）覆盖拒绝行为本身，不覆盖窗口竞态；
  窗口路径的最终清零需逐条改造为事务版，列为后续工程项。

### A2. 未接入守卫的写路径（约 10 条）
R3 穷举表中除上述已接入点外仍有少量写路径未接 980 守卫（完整逐条清单以 WP4 会话
报告为准，落盘版待补）。已知代表性遗漏：
- c2c/moment/private **附件授权路径**不做 workspace 回溯，`workspace_resolver` 中标注
  `TODO(T7)`（当前回退 personal 放行——经任务卡授权的简化；归档的 workspace 下历史
  附件可能仍可被既有直链访问）；
- webhook incoming 入站（token 即凭证，设计上免 JWT）不做归档守卫（wp3 边界测试
  明确将其排除在守卫外）；
- bot 发消息路径（api_token 认证）未接守卫。
**缓解**：上述路径均为"归档后台资源可被既有凭证触达"，不产生新资源越权；
完整收口计划待人工排期。

## B. 数据迁移与升级（T3）

### B1. erlang_migrate 部分回滚缺陷（上游库缺陷）
`erlang_migrate.erl:378-381`：`apply_down` 的 `PrevVersion` 取自本次回滚 sublist 的剩余项；
部分回滚时 sublist 耗尽 → `set_version(undefined)` = 清空 tracking。**生产禁止直接用
`erlang_migrate:down(Config, N)` / `goto` 到中间版本**；必须 down 后立即
`erlang_migrate:force(Config, <目标版本>)` 校正（实测有效，见 T3 报告 §2.4）。

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
套件 5 用例全绿，新增功能零失败。

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

### E1. invite 可选项语义偏差
Workspace Member 邀请接口的角色可选 `owner/member/guest`（服务端校验角色合法性），
但邀请时**不提供"附带入群/订阅意向"等可选项**（三条关系只能事后显式建立）——
与早期草稿中"invite 附带选项"表述存在语义偏差；现行契约：邀请只建 workspace_member。
（若产品期望"接受邀请时勾选加入 General/订阅 Announcements"，属后续迭代。）

### E2. Task assignee 的 active 校验在应用层（W0）
schema 层仅保证 user 存在外键；active Workspace Member 校验由 T6b 应用层同事务
执行（W0 无 project_member 表，DB 层触发器只管 project.owner）。指派已移除成员由
API 400 拒绝并透出消息（有测试覆盖）。

### E5. change_role 最后 Owner 保护的并发预检窗口（V1-F3 裁决入册）
两路并发 demote 最后 Owner 均可在事务外读到 owner 计数<=1 通过预检；结构性修复需事务内
FOR UPDATE 计数，改造收益低于锁代价，V0 登记为知情取舍。DB 无兜底触发器覆盖角色变更。

### E4. 频道创建后首帖的角色读缓存竞态窗口
频道创建（admin 行同事务落库）后立即以创建者身份发帖，偶发命中 get_role 读缓存旧值
被拒"只有管理员可以发布消息"，数秒内重试成功。Demo B 已加重试自愈；底层为既有
imboy_cache 失效策略域，非本计划引入。

### E3.
c2c/moment/private 附件不经 workspace_resolver 解析 workspace 归属。

### F. 其他
- **produce 规模迁移 BLOCKED 待人工**：发布窗口/锁预算终判需生产数据（B3/D 表重申）。
- 本仓库 license 提示"试用期已结束，降级社区版"（日志可见）——社区版运行不影响
  功能演练，但商业化档位相关开关与验收无关项未启用。

---

*最后更新：2026-08-27（WP8 执行会话生成；验收 Agent 应对本清单逐条复核后并入 dual-exp-acceptance.md）*
