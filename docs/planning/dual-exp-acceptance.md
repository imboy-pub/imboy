# IMBoy 双体验 v2.5.2 — 合并验收报告（V2 独立验收人）

> **历史快照说明（2026-09-03）**：本报告主体记录 2026-08-27 的 W0 验收，W0 的 defer/分支/SHA/测试数字不得当作当前 main 状态。当前归档守卫与已知限制以
> `dual-exp-known-limitations.md` 为准；后续 W2 和体验修复以 `dual-exp-final-report.md` §八为准。

- **验收人**：第三个独立会话（程序性隔离，未参与实现、未参与 V1 质量 review 与 V3 安全 review），只审不改业务代码
- **日期**：2026-08-27 | **分支**：imboy `dual-exp-v21` HEAD=`9078b4c5`；imboyapp worktree `.worktrees/imboyapp` `dual-exp-v21` HEAD=`a84d76bd`；imboyadmin `dual-exp-v21` HEAD=`68396eb`
- **对象规模**（本人 git diff --stat main...HEAD 实测）：imboy 101 文件 +13820/−579；imboyapp 62 文件 +10474/−4；imboyadmin 15 文件（68396eb 单提交可见）
- **模式**：**unsafe_experiment**。本报告只能断言 **local / rehearsal** 级结论；**不得**引申为客户验收、Release 或工程 DoD。Gate 0 与 Gate W 为无人值守代行（见 Decision Brief），均待人工签认。
- **红线遵守**：主区未提交的 E2EE WIP 文件（SECURITY.md/vm.args/msg_c2c/c2g/olm 等 M 状态文件）全程未触碰、未 stage、未提交；未 push；本会话新建探针数据已归档自清理（§四备注）。
- **重要过程事实**：本会话实测发现 `_rel` release 内 beam 为陈旧副本（group_member_ds.beam mtime 15:20，缺 HEAD 的 join-980 守卫），与任务简报"_rel 已含 HEAD 修复"不符。已执行 `IMBOYENV=local make rel` 重建 release 后再验证（md5(ebin)=md5(_rel)=3678821b419699399ca07bc9ebb1c767）。以下栏 3 全部 curl 证据来自重建后的干净 release。
- **构建口径说明**：本地 release/eunit 均从含用户 E2EE WIP 的脏工作树编译（WIP 未入库、不属本计划产物）；全部验收探针位于 workspace/channel/group/project 路径，与 WIP 文件（e2ee/olm/msg_c2c）无交集，不影响本报告结论。

---

## 〇、总表（✅/❌ 统计）

| 组别 | 判定 |
|---|---|
| V1/V3 六项 HIGH/MEDIUM 修复（读 diff `9078b4c5` 逐项） | **6/6 ✅**（另 ff648951 内 5 处直通契约 + resolver 归一并验） |
| `make app` 零警告复验 | ✅ EXIT=0，`grep -ci warning`=0 |
| Flutter analyze 新文件 0 新增 | ✅ 146 条全落 integration_test/ 存量，workspace/chat_shell/workspace_shell 非 test 命中 0 |
| Flutter 双体验子集复跑 | ✅ +189 通过 / ~80 跳过 / **0 失败**（chat_shell+workspace+workspace_shell+smoke 四目录） |
| 后端关键套件复跑（8 模块，本人取数） | **59 过 / 1 挂**：唯一挂＝w0_schema_contract_tests 的存量回填断言（16 行为 T14 演练残留数据所致的环境敏感红，非产品缺陷，见 F-N3） |
| Demo B 连续两遍复跑（本人） | ✅ ×2 **ALL PASS 22 步 54 断言**，EXIT=0 |
| 安全抽查复跑（非成员 403 组 / 归档后写 980 组 / join 980 守卫） | ✅ 全过（§三 10 步序列 + 修正 uid 补充 3 步） |
| §9.2 P0 W0 相关逐条 | ✅ 15 条｜⚠️ 降级达成 1 条（能部署）｜N/A(W0 裁剪) 1 条（Files 聚合）｜UI/真机层 BLOCKED 如实标注于对应条目 |
| §9.4 Day-1 Bar | 拿得出手 ✅／能让别人真实使用 ✅(API 闭环)／首日缺陷 ✅／能部署 ⚠️ 降级达成 |
| T15 双报告占位符/空壳检查 | ✅ 已填充、非空壳；F-N1 已由 `5a33f754` 的双遍 ALL PASS transcript 闭合 |

---

## 一、栏 1 · 代码质量复核

### 1.1 V1/V3 六项 HIGH/MEDIUM 修复逐项验证（证据：`git show 9078b4c5` + ff648951 + 行为复测）

| # | 来源 | 修复点 | diff 证据 | 行为复测 |
|---|---|---|---|---|
| 1 | V1-F1(HIGH) member_invite atom 键与二进制键撞名重复 status | `src/api/workspace_handler.erl:279,281` 改 `Member#{status_flag => changed/unchanged}` | diff 可见两处替换 | ✅ 探针 [4r]：响应 payload 仅一个 `"status":"active"` 键且 `status_flag:"changed"`（python 解析 top-level keys=['status']） |
| 2 | V3-F1(HIGH) `channel_repo:list_workspace_channels/2` 漏 export | `src/repo/channel_repo.erl` 新增 `-export([list_workspace_channels/2]).` | diff 一行 | ✅ 探针 [2]：非成员 GET /workspaces/:id/channels 返回 `{"code":403,…}`（HTTP 信封 200/envelope 403），不再是空 body 500 |
| 3 | V3-F2(HIGH) `normalize_assignee` 返回 atom `nil` 致 int8 编码崩连接 | `src/ds/project_task_ds.erl:250-252` spec 与实现改 `null` | diff 含注释"epgsql int8 列不接受 atom nil" | ✅ Demo B P13（两次 ALL PASS）：建任务**不带 assignee_id** 默认路径成功创建（脚本 P13 请求体仅 title）；服务重启后零 error.log 崩溃迹象（本会话探针未触发任何 500） |
| 4 | V3-F3(MEDIUM) 归档后入群未拒 | `src/api/group_member_handler.erl:203+` 新增 `{error,980}` 分支返回 980；`src/ds/group_member_ds.erl:174+` ensure_workspace_membership 内补 `SELECT status FROM workspace` 归档检查 throw {abort_tx,980} | diff 两文件 | ✅ 探针 [8r]：active 工作区成员在归档后 join General → `{"code":980,"msg":"工作区已归档，禁止加入其群组"}`
| 5 | V1-F2(MEDIUM) ds→logic 反向依赖 | `project_task_ds.erl:157` 改调**本地** `legal_transition/2`（`:27` 新增导出、`:191-192` 本地实现）；logic 层保留单向转调 DS（project_task_logic.erl:196-197）——依赖方向恢复 Handler→Logic→DS 单向 | grep 实测两文件函数定义与调用点 | （编译期行为由 make app 零警告佐证） |
| 6 | V1-F3(MEDIUM) 最后 Owner 并发预检窗口 | **已于 2026-09-03 结构性修复**：`change_role` 锁 Workspace 行后在同一事务内重验操作者、目标成员和 active Owner 数量 | `d4cbe404` + `workspace_logic_tests` 28/28 | ✅ 当前 main；历史“知情取舍”结论作废 |

**连带复验（ff648951 提交内，属同一修复链）**：
- remove/archive/restore/admin_archive/admin_restore 五处 with_tx 结果匹配改直通契约（`Result when is_map(Result)` / `{ok, Result} when is_map(Result)`），diff 于 workspace_logic.erl 四个 hunk + workspace_ds：本人读 diff 确认 5/5；
- resolver 归一：`workspace_resolver.erl` 新增 `ensure_member_ok/2` 并替换 3 个便捷门调用点（ensure_channel_member_access / ensure_group_member_access / guard_group_notice_id）——与 day1-walkthrough D2 闭环一致；
- 行为佐证：Demo B P18-P20（归档）、P7e（恢复）、P15/P16（移除冲突/成功）在我亲自两遍运行中全过——若直通契约错配回归，这两段必现 500。

### 1.2 `make app` 零警告复验

- 本人命令输出：`make app` → **EXIT=0**；日志 `grep -ci warning` = **0**。

### 1.3 Flutter（worktree a84d76bd）

- `flutter analyze --no-pub`：**146 issues found**（与 V1 记录数一致），条目逐一核对文件路径，100% 位于 `integration_test/` 既有测试文件（样例 inference_failure 系列）；以 `page/workspace|chat_shell|workspace_shell` 过滤并剔除 *_test.dart 后命中 **0** —— 新文件零新增告警，维持 V1 结论。
- 测试子集复跑（dual-exp 命名空间）：`flutter test test/unit_test/page/chat_shell test/unit_test/page/workspace test/unit_test/page/workspace_shell test/unit_test/smoke` → **+189 通过 / ~80 跳过 / 0 失败，exit 0**。其中含分支新增测试 17 个文件（chat_shell 6 / workspace 3 / workspace 项目 2 / workspace_shell 5 / smoke route_registry）。bootstrap switch 证据文件：`chat_shell_bootstrap_test.dart`、`experience_provider_test.dart`（21 用例）均在此轮通过。
- 说明：known-limitations C1 声称的"265 用例全绿"含 api 目录等更宽集合，我选取了核心命名空间自证；两者均为绿，口径差异如实记录。

---

## 二、栏 2 · 产品与 Day-1（§9.2 P0 逐条，Gate W=W0 口径）

| §9.2 条目（W0 相关摘译） | 判定 | 一行证据（验收人自取） |
|---|---|---|
| ① experience=chat 与 workspace 均正常运行 | ✅(API/测试级)，UI 层 BLOCKED | 服务端真相源链路齐备：`product_experience.erl:29-36`(effective/effective_binary)、`index_handler.erl:79-82`(init 白名单下发)、`imboy_env.erl:704-712`(env 覆盖，非法降级 chat)；Flutter 切壳测试 `chat_shell_bootstrap_test.dart` + `experience_provider_test.dart` 在我的子集复跑中通过；真机双壳走查 BLOCKED（见 §六） |
| ② Message/User/File/E2EE/WebSocket/Group/Channel 全共用，无第二套实现 | ✅ | workspace 视图直接 import 既有内核：`workspace_channel_detail_page.dart:21` import `page/channel/channel_detail_page.dart`；后端消息收发沿用 msg_c2g_repo/channel_message_repo 原 unmodified 内核（diff 中仅加守卫挂接）；WS c2g 由 Demo B 步骤 [11] 真帧发送 PASS×2 |
| ③ Chat 核心 Demo 全绿，无回归（对照 R1 基线） | ✅（模块级） | R1 基线 5804/59（R1.1 记录）；我复跑的后端 8 关键模块 59 过/1 挂（唯一挂见 F-N3，与本计划代码无关）；全量对账未在本会话重跑（~40 分钟成本），继承 C2 口径如实标注 |
| ④ Workspace MVP 六能力覆盖（W0 收敛）＋导航用 Members | ✅(Files 聚合 N/A) | 生命周期 archive/restore 探针 [5]/[10] 实测；Branding 字段在 create workspace 响应中回显（探针请求带 branding.name="V2ACC"，模板创建时同事务落库）；资源清单 project/group/channel/members 列表均有 API 与测试；计费锚点 `workspace.owner_id` 只读（T3 迁移⑥）；导航措辞 `workspace_shell_page.dart:42 membersLabel: t.workspace.navMembers`（zh='成员'、members 页全称"工作区成员"）；**Files 聚合不在 Gate W 十二项内 → N/A(W0)**，此处与 §9.2 原文列表存在裁剪偏差，已计入 Scope Contract 表说明 |
| ⑤ Project 能力按 Scope Contract 闭环（Tasks 四态+回退必选） | ✅ | 五证见 §四 Scope Contract #1；Demo B [13]-[14] 两遍：todo→doing→review→done→回退→done 全过；w0_now_tables_present + project_task_flow_test 在位 |
| ⑥ Workspace 六能力完整（三角色/最小权限/Branding/资源清单/生命周期/计费锚点）五证齐全 | ✅ | 三角色矩阵 `workspace_logic_tests` **16/16**（我复跑数字）；最小权限探针 [1][2] 403；Branding 见④；生命周期⑤；owner_id 见④ |
| ⑦ 双 scope XOR 成立＋存量回填＋channel↔project 同校验 | ✅（schema 契约层） | w0_schema_contract_tests 4/5：XOR CHECK 约束断言过、defer 表缺席断言过、now 表在位断言过；存量回填断言在共享库上因演练残留红（F-N3 定性：**group 表 0 违例、channel 全部 16 行违例均为迁移之后产生的 DemoB-W0 演练工件，44 行基线 personal 行完好**） |
| ⑧ 四元语义成立；Group Notice 保留；Channel 视图无聊天输入；Pinned/Overview 不聚合 Notice | ✅ | `workspace_channel_detail_page.dart` 结构=讨论引导横幅+ChannelDetailPage（复用 Publish 内核），全文件 "ChatInput" 3 处命中全为"不引用聊天组件"的注释/契约（:7/:12/:30），ValueKey `workspace-channel-detail-dm-entry` 固化；workspace 页面 grep GroupNotice 零引用；Activity 无正文=W0 无聚合端点（§五 defer 五项） |
| ⑨ 四关系边界清楚；加入 WS 不自动加入下级 | ✅ | Demo B [5]-[8] 两遍：邀请后 DB 核查 active 群成员=0、订阅=0，显式入群/订阅各自独立过 |
| ⑩ Group Member ⊆ Workspace Member 两档强制 | ✅ | 三层：DB 触发器 `trg_group_member_ws_subset`（DEFERRABLE，T3 §2.2 SUBSET-1..5）＋应用层探针 [3r] `{"code":409,"msg":"workspace_membership_required…"}` ＋ `group_member_workspace_subset_tests` **3/3**（我复跑）；W0 外的第二档（Project Member ⊆ WM）N/A |
| ⑪ Project 成员深度与 W0 一致（无 project_member 表） | ✅ | 直查 pg_tables：8 张候选表中恰 5 张存在（workspace/member/project/task/event），project_member/milestone/rel 不存在；w0 断言套件 DEFERRED_TABLES 五张禁表逐一断言通过 |
| ⑫ 直接入口不可绕过；非成员 403；Guest 只读；叠加权限仍生效 | ✅ | 探针 [1][2] 双 403「非工作区成员」；by_custom_id 通道经 resolver 成员门（V3 §① 50+ 黑盒用例）；Guest 只读矩阵在 workspace_logic_tests 16 例内 |
| ⑬ 移除冲突 fail-closed；无冲突级联禁用并审计；重邀不自动恢复 | ✅ | Demo B [15] 409 membership_conflict＋回滚核查、[16] 成功移除＋审计清单＋级联禁用、[17] 重邀后群成员=0 且订阅=0 —— 两遍 ALL PASS 均含此三步 |
| ⑭ Template 幂等原子 | ✅ | `workspace_template_tests` **5/5**（我复跑）＋ Demo B [3][4] created/existing 同一 workspace_id 两遍 |
| ⑮ Archive 并发线性化；漏写为零；读取保留；personal 不受影响；恢复放行 | ✅ | `workspace_archive_concurrency_tests` **2/2**（场景 A/B 先拿锁者胜＋980 断言，真库 FOR UPDATE）；我的探针 [6][7]=980、[21 读] 由 Demo [20]-[21] 读取正常覆盖、[9] personal 对照 code=0、[10] restore 后放行 code=0 |
| ⑯ 只改 IMBOY_PRODUCT_EXPERIENCE 受控重启即切换；服务端唯一真相源 | ✅(local)；生产编排 BLOCKED | deploy-rehearsal §4 受控重启 ≥6 次（本机）；digest 纯函数单测钉死（product_experience_tests **9/9** 我复跑，含 golden 向量）；dart-define 仅显式开发覆盖（experience_provider.dart 注释 :17 安装级语义）；生产 Docker/Helm 演练无授权 BLOCKED |
| ⑰ 安装级配置：Admin 只读、config_version 可复现、与 product_profile 无混淆 | ✅ | `adm_admin_handler config_product_experience` GET-only（V3 §③ 实测无凭据/普通 JWT 均 706）；init 仅透出 effective_product_experience+config_version 白名单键（index_handler.erl:76-82 静态复核）；命名隔离：product_profile 模块 0 个新同名文件（grep src/lib 仅 product_experience.erl） |
| ⑱ Admin 四元运营面 T11b；scope 归属显示；与写守卫联动；非管理员 403 | ✅ | `imboyadmin/src/pages/workspaces/{WorkspaceList,WorkspaceDetail,ProjectList,ProjectDetail}Page.tsx` 在库（detail/list 分页结构+审计位）；`channels/ChannelListPage.tsx`、`groups/GroupListPage.tsx` 含 scope 呈现；归档联动经 `workspace_logic.admin_archive/admin_restore`（与探针同路径族）；adm_acl fail-closed 七 action 前置（V3 §③ 实测证据，未推翻点） |
| ⑲ 干净环境仅按文档完成部署并跑通 Demo B | ⚠️ 降级达成 | deploy-rehearsal 自报降级（编排者执行+空库演练），降级授权不在验收 Agent 权限内 → 维持 ⚠️：文档驱动重放的六个分项里"真·未参与实施者独立部署"一格由我以文档评审+依赖复跑间接补强（§七"能部署"），完整口径留待人工 |
| ⑳ 首日旅程阻断缺陷=0；可告知缺陷 100% 入册 | ✅ | day1-walkthrough D1-D5 全部修复且带回归（D1/D2/D3 对应测试在我的套件复跑中绿）；D6 脚本侧修正后由我两遍 ALL PASS 直接证实；E4 已确认 `get_role` 为数据库直读，并由创建者角色兜底及发布回归测试覆盖；Demo 重试保留为兼容防护 |

### T15 双报告复核（逐占位符/空壳/数字一致性）

- `dual-exp-deploy-rehearsal.md`：6 节全部实质填充（步骤命令可复制、明确降级声明、配置接口 digest 有外部复算锚点 49654a9fffa39c0d/684f363bd3176f1f、升级回滚命令具体到 erlang_migrate:force 语义）；无 TBD/TODO 占位。数字与我复验一致处：版本 78/f、五轮 init 取证、受控重启 ≥6 次（本机）无法独立重现历史次数，但其算法与切换语义已被我的 product_experience_tests 9/9 与静态链路复核替代印证。
- `dual-exp-day1-walkthrough.md`：12 项走查清单状态列无空洞；D1-D6 缺陷表含定级与修复指向；"ALL PASS 22 步 54 断言"与**我本人两遍复跑结果完全一致**；升级提示链路给出 app_version 只读查询细节。F-N1 已由 `5a33f754` 写入的 run1/run2 双遍 ALL PASS transcript 闭合。
- `dual-exp-t3-migration-rehearsal.md` 与 `dual-exp-known-limitations.md`：抽查关键数字（134→139 表、26/26 与 518/518 回填、down 事故 force 恢复、B1 上游缺陷定位到 erlang_migrate.erl:378-381）与我直查库内实况一致（version=78 dirty=f）；known-limitations 的重复附件边界已归并至 A2，E 节编号已恢复连续。

---

## 三、栏 3 · 安全复核（V3 正面七项抽二复跑 + join 980 守卫生效验证）

验收人亲自以 curl 序列对重建后的 release（127.0.0.1:9800，envelope 语义 `{code,msg,payload}`）复测：

**组 A · 非成员边界 403（复跑 V3 ①）**
- 探针 [1]：非成员(uid=4 token) GET `/api/v1/workspaces/<id>` → `{"code":403,"msg":"非工作区成员，禁止访问该资源"}` ✅
- 探针 [2]：非成员 GET `/api/v1/workspaces/<id>/channels` → 同文案 403（**同时证明 V3-F1 修复后主列表端点不再 500**）✅
- 探针 [3r]：非成员 POST `/api/v1/group_member/join {"gid":<ws群>,"member_uids":[4]}` → `{"code":409,"msg":"workspace_membership_required：群成员必须先是该工作区的 active 工作区成员"}` ✅（子集约束应用层档）

**组 B · 归档后写 980（复跑 V3 ④）**
- 探针 [6] Owner POST `/workspaces/:id/projects` → `{"code":980,"msg":"工作区已归档，写操作被拒绝"}` ✅
- 探针 [7] Owner POST `/channel/:cid/message` → 980 同上 ✅
- 探针 [9] personal 对照：archived 窗口内 B 创建个人频道并发帖 → `code:0` 正常（guard personal 直通无误伤）✅
- 探针 [10] restore 后 Owner 发帖 → `code:0` 放行 ✅

**组 C · join 980 守卫生效（V3-F3 修复的行为级新证）**
- 准备：Owner 建 ws2、邀请 uid=4 为 member（此时该成员尚未加入 General 群）→ 归档 ws2
- 探针 [8r]：该 active 工作区成员 POST `/group_member/join {"gid":<General>,"member_uids":[4]}` → `{"code":980,"msg":"工作区已归档，禁止加入其群组"}` ✅ —— 守卫分支（group_member_handler `{error,980}` + group_member_ds 归档 throw）在真实 HTTP 层生效
- 注：首轮探针 [8] 曾用错误 uid（118 非真实用户 id）得到业务码 1 提示，系无效输入所致，不计缺陷；修正后如上。

**其余正面项**：Admin fail-closed（706 门）、config_version/init 白名单、SQL 注入面、越权改角色/转移 Owner 四项，采信 V3 已做的黑盒+静态双证据，本次复核未发现与其矛盾的新事实。2026-09-03 当前 main 已将 Workspace 数据库写路径收口到事务守卫；Personal 附件与 C2C Bot 不再误列为 Workspace 缺口，详见 known-limitations §A1/A2。对象存储上传与 PostgreSQL 事务之间仍可能产生未引用对象，这是保留的跨系统边界。

**过程安全备注**：验收产生的两个 V2ACC 工作区已归档自清理；探针个人频道与 Group 消息残留同步记录于 F-N3 残留台账建议人工统一清库策略（license 社区版上限使删除账号不可行，archive-only）。

---

## 四、Scope Contract 五证表（Gate W=W0 十二项）

"now 七项"五证 = schema / API(五层) / Flutter / 测试 / Demo；"defer 五项"三证 = 无 schema / 无占位 UI / 无完成声明。

| # | 能力 | 档位 | schema | API(五层 handler→logic→ds/repo/router) | Flutter | 测试 | Demo |
|---|---|---|---|---|---|---|---|
| 1 | Project Tasks 四态(+project_event 随建) | now | `00000078_project_foundation` 3 表＋复合 FK/触发器（T3 §1） | project_handler/project_task_handler→project_task_logic→project_task_ds/repo；路由 `/projects`,`/tasks/:id/status` | project_page/task board/test 2 文件+route 注册（我的子集绿） | w0_now_tables_present＋task flow tests；FF `project_task_flow_test.dart` | Demo B [13][14] ×2 |
| 2 | Milestones | defer | ❌无表（pg_tables 直查+w0 断言） | ❌无端点 | ❌无 UI（lib/page/workspace、chat_shell grep milestone 0 命中） | ❌无完成声明（known-limitations 仅 defer 登记）；Admin grep milestone 0 命中 | — |
| 3 | Pinned 聚合 | defer | 零存储设计（天然满足） | ❌无端点 | ❌无 UI | 无完成声明；Channel pinned 系既有功能与本项无关（附录 C/D-C4.5） | — |
| 4 | Resources 聚合(links) | defer | project 无 links 列（w0 断言 columns 检查+T3 W0 裁剪清单） | ❌无端点 | ❌无 UI | 无完成声明 | — |
| 5 | Activity 聚合端点/视图 | defer | project_event 表依 #1 建立（Decision Brief #1 明示），**聚合端点/视图未开发**＝满足 defer 口径 | ❌无 Activity REST 端点（router grep） | ❌无视图 | 无完成声明；event 唯一写入方为 task 状态机（V1 复扫结论未推翻） | — |
| 6 | 关联 Channel 集合 | defer | ❌无 project_channel_rel 表 | ❌无绑定/解绑端点 | ❌无 UI | 无完成声明 | — |
| 7 | 成员三角色 | now | `workspace_member` 复合主键 role CHECK | invite/change_role/transfer_owner→logic/`workspace_logic`→repo | invite controller/members page | `workspace_logic_tests` 16/16（matrix 含 Guest 只读/最后 Owner 保护） | Demo B [5][15]-[17] ×2 |
| 8 | 权限(scope XOR+resolver 门) | now | `chk_channel_scope_xor/chk_group_scope_xor`（我的 pg_constraint 断言过 via 套件） | resolver 便捷门挂 channel 族/group 族/group_notice | （客户端无权限旁路 UI）boundary 由服务端裁决 | `workspace_guard_tests` 14/14＋`workspace_boundary_tests` 7/7（我复跑）＋探针组 A | Demo B 全程角色分居 |
| 9 | Branding | now | workspace.branding jsonb（76 号迁移） | create/update 回显 branding | workspace_branding_theme_test＋branding 页 | FF 测试在子集绿 | create 请求/响应含 branding |
| 10 | 资源清单 | now | scope 列+部分索引（77 号） | workspaces/:id/channels、groups、projects、overview | overview/projects 视图（page/workspace_shell+project） | overview 字段白名单测试（C 端注释不聚合 Notice）；V1 栏2 结论未推翻 | Demo [21 段] Projects 列表读取 |
| 11 | 生命周期(含归档) | now | status/archived_at/archived_by | archive/restore(+admin_* 直通契约修复后) | dispatch/archive 态呈现（workspace_dispatch_test） | `workspace_archive_concurrency_tests` 2/2（先拿锁者胜） | Demo [18]-[22] ×2；我的探针组 B/C |
| 12 | 计费锚点 owner_id | now | workspace.owner_id NOT NULL FK users | detail 出参含 owner_id（只读，无变更端点） | 模型只读消费 | owner 变更唯一通道 transfer_owner（409 守卫链由 logic tests 覆盖） | create 响应 owner_id=uid A |

**W0 硬约束执行情况**：`project_member` 禁止落表——schema 断言在位（w0 套件第 1 项）且我直查数据库确认 0 张；无 project_member UI/API（grep 后端 router 与两端前端 0 命中）；无"已完成 project_member"类声明文本。

---

## 五、§9.3 最终自检八条书面回答（每条 ≤250 字，引用本文档取证编号）

1. **Chat Experience 兼容（对照 R1 基线数字）？** 兼容判据成立但我未复跑全量对账：后端我重跑 8 关键模块 59 过/1 挂（挂见 F-N3 非代码因素）；R1 基线 5804/59 未被我推翻（工作树相对 T13 后仅有 9078b4c5 等 workspace 路径改动，已由 §1.1 六项修复逐一消除风险面）。Chat 主链路活性由 Demo B [11] WS c2g 真帧与 [12] Group Notice 两遍 PASS 直接证明；Flutter chat_shell 子集 189 过 0 挂（analyze 146 条全存量）。全量 eunit/flutter 全量对账列为遗留给 RC 前一次性复跑（时限成本考量，不影响 local 结论）。
2. **Workspace Experience 真实可运行且共用 IM 核心？** 是（local/release 级）：我从零 `make rel` 重建并以 daemon 启动对外提供服务，10+3 步 curl 探针与两遍 Demo B 全程在其上跑通；共用性以“零第二实现”三点证据支撑：Flutter 侧直接复用 ChannelDetailPage 内核（栏2②）、后端 msg/channel/group 写内核 diff 无平行实现（仅守卫挂接）、WS 收发沿用原 frame 协议。无不必要重构：101 文件增量中引擎层未翻动，符合 I6“本期内核不动”。
3. **只改一个安装配置受控重启即可切？Admin 无运行时写入口、version 可复现、无命名混淆？** 后端 truth source 单点（imboy_env override→application env→product_experience.normalize fail-safe chat），config_version=digest(effective,vsn) 纯函数单测钉死（9/9）；`/api/v1/init` 白名单仅两键；Admin `ProductExperiencePage` 只读展示，运行时写端点 0 个（source grep+V3 黑盒双证）；与 product_profile 分名且并存。生产 Docker/Helm 受控重启演练 BLOCKED（无授权），语义等价性以本机 ≥6 次 restart 报告＋纯函数性质承接——**生产切换实操仍是 Release 前必做人工动作**。
4. **Scope Contract 严格执行？哪些暂未实现（对齐 §1.4.3）？** 十二项五证表齐备（§四）：now 七项 schema/API/Flutter/测试/Demo 全有实证指针；defer 五项"无 schema、无占位 UI、无完成声明"三证成立（w0 断言套件守护禁表禁列，前端/Admin grep 0 命中）。暂未实现集合＝§1.4.3 与 defer 五项（Milestones/Pinned/Resources/Activity 聚合/关联 Channel＋整档 project_member），另有 Files 聚合按 Gate W 未纳入十二项（§9.2 行④偏差已登记）。无一项以"计划写过"为由偷建——w0_schema_contract_tests 即防复活装置（其在共享库上的环境敏感红已在 F-N3 定性并给出修复建议）。
5. **Archive 服务端强制守卫覆盖 R3 写路径？并发线性化与审计验证？personal 不受影响恢复放行？** 当前 Workspace 数据库写最终入口使用 `ensure_writable_tx`、`write_tx` 或 `write_tx_or_skip`，派生已读计数也由 `07312d0c` 在事务内冻结；旧版自动提交预检仍可作为快速失败，但最终写不依赖它保证正确性。对象存储与数据库不能跨系统原子提交，落库被归档拒绝时可能留下待回收对象。审计事件（workspace_archived/member_removed 等）在 Demo [16][18] 日志中产出；Personal 路径不受 Workspace 归档影响。
6. **四元语义＋Notice/Channel 边界＋关系全称无混用？30 秒测试？一句话说清 Project vs Group？** 无混用证据：全称纪律贯穿（WorkspaceMemberModel 枚举注释三分关系；导航 navMembers='成员'+members 页全称"工作区成员"；Group Notice 保留原命名空间，Channel 视图零聊天输入 [栏2⑧]，Overview 不聚合 Notice）。真实用户的 30 秒理解测试＝**BLOCKED**（day1 §4 已给出 W0 版四问设计供人工补做）；作为代理判据，四问的机器可验证部分（自动入群否定断言、通知独立流程）在 Demo 中两遍通过。"为什么 Project 不是 Group"现行答案＝任务不沉底、做完没一眼看清（§1.4.1 论证），商业语境下的有效性必须等 Gate 1 真人反馈，本期不作声明。
7. **成员深度与 Gate W 档位一致？两档子集三层证据？移除冲突全链验证？** 一致（W0）：无 project_member 表（⑪）；Group 子集约束三层证据齐全——DB 约束触发器（T3 SUBSET-1..5 含 removed-wm 拒绝与同事务激活放行）、应用层同事务 409（探针 [3r] 原文）、并发/集成测试 3/3（我复跑）；移除链完整：所有权/Task 冲突先阻断 fail-closed（Demo [15] 409＋事务回滚核查）→ 无冲突级联禁用＋审计（[16]）→ 重邀不自动恢复（[17] DB 断言 0/0），两遍连续。project_member 相关 W1/W2 项全部 defer 且无偷建（§四硬约束段）。
8. **首日闭环全修复或入册？部署演练确由未参与实现者在干净环境完成？下一商业验证场景？** 缺口管理合规：走查期 D1-D5 全修复带回归（对应测试均在我复跑绿），D6 脚本口径修正，E4 已由创建者角色兜底及发布回归测试覆盖；**部署演练的原始口径（未参与实现者独立完成）未达成**——rehearsal §1 自报由编排者执行，我把"文档能否自足"作为验收内容复核（六节无占位、命令可复现），并以未参与实现的身份实际重建 release/启动/演练主链路间接补强，但"干净全新物理环境从文档零答疑装到底"仍差最后一步，**如实降级**。下一最值得验证的商业场景：付费 PoC 选 1 家小团队以 "Workspace 归档→季度结算→restore 留档" 结合 "Project Tasks 周报流" 做 Demo B 子集验收（Gate 1 口径），用真实数据反推 Template 幂等与归档写守卫的价值感知，同时在合同附件中约定生产行数量级补测（解除 R2.5/B3 的生产 BLOCKED）。

## §9.4 Day-1 Bar 四项判定

| 项 | 判定 | 依据（编号溯源） |
|---|---|---|
| **拿得出手**（Demo 两遍无人工干预；无占位符/调试残留/控制台报错） | ✅ | 本人两遍连跑 ALL PASS 22 步 54 断言 ×2、EXIT=0/0；演示注册路径遇 license 上限时走幂等演示账号（INFO 提示非报错）；双遍 transcript 已由 `5a33f754` 留档；本会话探针未触发任何 500/崩溃 |
| **能部署**（未参与实现者干净环境仅按文档部署） | ⚠️ 降级达成 | rehearal §1 自认编排者执行+空库演练，"未参与实施者格"以我方独立复核+依赖实战接管部分补强；生产 Docker/Helm 无授权 BLOCKED；**完整口径留待人工**（与条目⑲一致） |
| **能让别人真实使用**（试点环境注册/邀请/权限/附件/升级提示闭环） | ✅(API 闭环)+BLOCKED(UI) | 注册→Template→邀请→显式入群/订阅→任务指派→归档保护→恢复全 API 级闭环（Demo ×2＋探针）；升级提示服务端前置（app_version≥vsn 配置在位，day1 §3）；group/channel 附件确认已在事务内守卫，Personal 附件按 schema/ACL 不回溯 Workspace；**真机 UI 弹窗与附件上传实走 BLOCKED** |
| **首日无显性缺陷**（走查全绿或 100% 入册） | ✅ | 走查发现即修即录：D1-D5 修复带回归（套件复跑绿）、D6 脚本侧、E4 创建者角色兜底带发布回归；known-limitations 可逐条核查；F-N1/F-N2' 文档治理项已闭合，F-N2/F-N3 仍待处理 |

---

## 六、BLOCKED 清单（如实汇总，全部延续各源报告披露，无一为我新发现时被隐瞒）

1. 生产/类生产环境 Docker/Helm 部署演练（人工授权红线）——rehearsal §1、known-limitations D 表。
2. 生产行数量级/峰值写入/索引发布窗口终判（R2.5/B3；牵涉 4 个部分索引 CREATE INDEX 需窗口）。
3. 真机/UI 层走查：双壳真机体验、音视频双向媒体、Push 厂商通道弹窗、附件真机上传、升级提示端到端弹窗（day1 §1 第 8-12 项）。
4. 30 秒真人理解测试 3 人样本（day1 §4 设计已就绪待人工执行）。
5. 后端全量 eunit / Flutter 全量对账在本验收会话未整体重跑（时间预算取舍；以上限口径与模块级证据替代，RC 前建议一次性复核）。
6. CI 远端大面积红灯既有问题（C3，未在本计划范围修复）。
7. 对象存储上传与 PostgreSQL 归档事务无法原子提交；落库被拒后未引用对象的清理效果仍需运维证据。

## 七、本轮验收新发现（F-N 系列；均非 CRITICAL/HIGH）

- **F-N1 (MEDIUM·文档治理，已闭合)**：`5a33f754` 已用双遍 `ALL PASS (steps=22 assertions=54)` 的脱敏 transcript 替换旧失败快照；当前代码另由创建者角色兜底发布回归覆盖。
- **F-N2 (MEDIUM·运维卫生)**：`_rel` 内 beam 相对 HEAD 陈旧且不一致（group_member_ds/workspace_handler 为 15:20 版、channel_repo 18:44 版），任何人直接起旧 release 会得出与 HEAD 不符的安全/功能行为（V3 环境备注早已预警）。本会话已重建修复。**要求整改**：收尾流程固化"改码后必须 `make rel` 再演示"，或在 bin 启动前做 beam 摘要比对。
- **F-N3 (MEDIUM·测试环境设计)**：`w0_schema_contract_tests.legacy_rows_all_personal_test_`（line 142）对共享库 imboy_v1 断言"channel 全行 personal/NULL"，而 T14 历次 Demo 留下 18 个 DemoB-W0-* 工作区与其 16 条 Announcements 频道（全部产生于迁移之后）必然破坏该断言 → 当前实跑 4/5。demo 脚本无清理阶段（也无 delete 端点可清，仅 archive）。产品层面无恙：XOR 双向 0 违例、group 表 0 违例、44 条迁移前基线行完好。**要求整改**：断言改为"迁移前基线行保持 personal（created_at < 迁移时刻）或将残留判据从断言剥离至对账工具"，并为 demo 增加 teardown（至少 archive+DB 备注标签）。
- **F-N2' (LOW·排版，已闭合)**：known-limitations 原 §E3 重复附件边界已由 A2 完整承载并删除，Owner 并发保护调整为 §E3，E1-E4 编号恢复连续。

## 八、最终判定

**ACCEPTED（local / rehearsal 级）**

- 理由：CRITICAL/HIGH 清零（V1-F1、V1-F2、V3-F1、V3-F2 修复并有行为级复证；V1-F3 经裁决知情取舍入册 E3；V3-F3 修复并经归档 join 场景复证）；关键测试矩阵在我手中全部可绿（除 F-N3 环境敏感一条且已根因定位）；Golden Demo 以第三方身份复现双遍 ALL PASS；三类安全边界抽查零失败；Scope Contract 十二项五证/三证齐备；已知限制与 BLOCKED 披露诚实完整。
- **边界重申（unsafe_experiment 不解除）**：本判定 ≠ 客户验收 ≠ Release ≠ 工程 DoD；生产迁移窗口（B3/R2.5）、真机 Day-1 Bar 四项、30 秒理解测试、Gate 0/Gate W 的人工签认、生产受控重启演练均在 Release 前必须由人工完成的清单内。
- 附带整改要求（不阻塞 ACCEPTED(local)，但进入 Release 候选前须闭合）：F-N2 收尾流程固化、F-N3 断言口径修正。F-N1/F-N2' 已闭合。

---
*方法学备注：所有结论基于本人执行的命令与直查输出（git show/grep、make、erl、psql@4323、curl@9800、flutter），对前序会话（V1/V3/T13-T15/WP 会话）的结论仅在"我方复核未推翻"的前提下引用并显式标注；测试数据：新建 2 个 V2ACC 工作区已归档、2 个探针个人频道与若干消息残留已列入 F-N3 台账建议、演示账号 A/B 未触碰、用户 E2EE WIP 未触碰。*
