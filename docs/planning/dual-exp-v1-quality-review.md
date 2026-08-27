# IMBoy 双体验 v2.5.2 — V1 独立代码质量 Review 报告

- Reviewer: V1 独立质量 Reviewer（程序性隔离，未参与实现；只审不改）
- 日期: 2026-08-27
- 审查范围:
  - 后端 `/Users/leeyi/project/imboy.pub/imboy` 分支 `dual-exp-v21` vs `main`（101 文件, +13791/-579）
  - Flutter worktree `/Users/leeyi/project/imboy.pub/.worktrees/imboyapp`（62 文件, +10474）
  - Admin `/Users/leeyi/project/imboy.pub/imboyadmin`（15 文件, +1714）
- 排除: 主工作区未提交的 E2EE WIP（SECURITY.md/vm.args/msg_c2c/c2g/olm 等 M 状态文件），非本计划产物，未触碰
- 已知背景知悉: WP8 五缺陷修复（ff648951）、Gate W=W0 defer 红线、T7 检查-写窗口与约 10 条未接入路径为声明取舍

---

## 一、Findings 总表

| # | Severity | 位置 | 栏目 | 描述 | 建议修法（一行） |
|---|----------|------|------|------|------------------|
| F1 | HIGH | imboy `src/api/workspace_handler.erl:279,281`（配合 `src/logic/workspace_logic.erl:210-222`） | 一致性 | member_invite 成功响应把 atom 键 `status => changed/unchanged` 注入含二进制键 `<<"status">> => <<"active">>` 的 Member map，jsone 序列化输出重复 `"status"` JSON 键且顺序不定（maps 遍历序非确定）→ 客户端读到的 status 在 `active/changed/unchanged` 间随机；task 侧同类场景已用 `status_flag` 避键（project_task_handler.erl:69），workspace 侧独漏。每次邀请调用都会构造此响应 | 改用 `Member#{status_flag => changed}` 或先 `maps:remove(<<"status">>, Member)` |
| F2 | MEDIUM | imboy `src/ds/project_task_ds.erl:157` | 代码质量 | DS→LOGIC 反向依赖：`project_task_logic:legal_transition/2` 被 DS 层调用——全仓唯一一处 ds→logic（grep 验证无先例）；logic 层同名函数又转调 DS（project_task_logic.erl:196-197），形成双向互调。check_module_boundaries.sh 只管 handler 层，检测不到此违规 | 把状态机纯函数仅保留在 project_task_ds，删除 logic→DS 转调外的 DS→logic 引用（change_status 事务内复检改调本地函数） |
| F3 | MEDIUM | imboy `src/logic/workspace_logic.erl:350-357` | 代码质量 | change_role 最后 Owner 保护（count_by_role + update_role_tx）在自动提交连接上检查、with_tx 内写——检查-写窗口。并发双 demote 最后 Owner 可同时通过计数检查 → 工作区零 active Owner。00000077 的 trg_workspace_member_remove_guard 只兜"移除端"，不兜角色变更；该窗口未登记进 known-limitations A1/A2（A1 只声明归档守卫路径） | count_by_role 移入 update_role_tx 同事务并以 FOR UPDATE 行锁线性化（复用 workspace_guard 模式），或至少补登记 known-limitations |
| F4 | LOW/MEDIUM | imboy `src/logic/workspace_logic.erl:415-419` | 一致性 | transfer_owner 的 with_tx Fun 内 `ok = workspace_member_repo:update_role_tx(...)` 裸匹配：目标在预检后被并发移除时返回 `{error, member_not_active}` → badmatch → elib_pg 归一 `{error,{db_exception,error,{badmatch,_}}}` → 用户收 500「转移失败」而非语义码 409（不 crash 进程：with_conn ROLLBACK + 归一已验证）。属 WP8 "错误 {ok,X} 匹配"同类漏网（check-then-act 变体） | case 匹配 member_not_active 映射 {error,{409,...}}，去掉裸 `=` |
| F5 | LOW | imboy `src/logic/workspace_logic.erl`（818 行） | 代码质量 | 超"文件 < 800 行"规范上限（wc -l = 818）。Admin 段（648-818）与其余治理段耦合在同一文件 | 将 admin_page/admin_detail/admin_archive/admin_restore 段拆至独立模块（如 workspace_admin_logic） |
| F6 | LOW | imboy `scripts/check_module_boundaries.sh:13-27` | 代码质量 | 新增 workspace_handler / project_handler / project_task_handler / adm_workspace_handler 未纳入 boundary-managed 清单，边界脚本对全部新入口失守（当前四个 handler 实现合规、直调 logic，仅守门缺位） | 四者加入 BOUNDARY_HANDLERS 与 allowed_modules_for_handler（auth_ds/config_ds + 各自 logic） |
| F7 | LOW | imboy `src/lib/workspace_resolver.erl:145` | 代码质量 | lib→logic 反向引用（workspace_resolver → workspace_logic:ensure_member/2）。项目有 5 处旧先例（e2ee_kt_merkle 等），不判违规记一致性观察：resolver 是所有受保护入口的前置门，会拉起整个 logic 模块栈 | 可选：ensure_member 的成员存在性检查下沉为 resolver 内 repo 直查，角色校验仍归 logic |
| F8 | LOW | imboy `src/ds/workspace_ds.erl:430,489-491`；`src/ds/project_ds.erl:178,232-234`；`src/repo/project_task_repo.erl:84` | 代码质量 | 整数 ID 拼 IN(...) 子句与 LIMIT/OFFSET 直拼（join_int_ids 等）。均经 integer_to_binary 类型保证不可注入、表名/白名单常量拼接另计，但与"所有 SQL 参数化"字面规范有出入 | IN 列表可用 `WHERE workspace_id = ANY($1::bigint[])` 全参数化 |
| F9 | LOW | `.worktrees/imboyapp` 约 13 处 EdgeInsets 直拼 | 代码质量 | 新页面少量 EdgeInsets硬编码绕过 AppSpacing Token（workspace_create_page:1、workspace_members_page:1、workspace_projects_page:4、workspace_view_widgets:2、task_form_page:1、project_tasks_view:4）；颜色侧合规（AppColors/AppSpacing 为主，Colors.transparent 为常规豁免） | 统一替换 AppSpacing 常量 |
| F10 | INFO | imboy C 端 workspace/project/task handler 出参 | 一致性 | C 端 TSID 以 JSON number 下发（DB integer 直出）而 adm_workspace_handler 做 tsid_keys_to_bin 转 string——两种形态并存。C 端符合 AGENTS.md「JSON integer 传输 + 前端 safeParseBigIntJson」既有约定：Flutter EntityId/entityIdOf 与 Admin EntityId(string) 均已正确落地，无精度回转风险。仅登记形态差异供后续统一评估 | 如需统一，后端 workspace 行出参统一走 tsid_keys_to_bin |

**无 CRITICAL finding。**

---

## 二、三栏检查结果

### 栏 1 · 代码质量

- **分层单向依赖**: Handler→Logic→DS→Repo 总体成立。4 个新 handler 全部只调 logic/auth_ds/config_ds ✓。违规见 F2（DS→LOGIC）、F7（lib→logic，有先例降级观察）。logic 层直调 user_repo 有项目既有先例（bot_logic/red_packet_logic 同款），不算违规。
- **SQL 参数化**: 全部值参数化，0 处字符串插值注入面；ILIKE keyword 用参数拼接 `%kw%` 于参数侧 ✓。整数拼接备注 F8。
- **行数/编译**: workspace_logic.erl 818 行超限（F5）；其余文件均 <800、函数均 <50 行 ✓。`make compile` exit 0，**零警告** ✓。
- **Flutter analyze**: 146 issue 全部位于既有测试文件（与 known-limitations C1 口径一致），workspace/chat_shell/project 新文件 **0 新增** ✓。
- **命名一致性**: 「工作区成员 Workspace Member」全称贯穿后端文案/API/i18n 与 Flutter（WorkspaceMemberModel 枚举注释显式区分三种关系）✓。

### 栏 2 · 产品与结构

- **defer 五能力（milestone/pinned/resources/activity/project_member）**: diff 全量 grep——所有命中均为注释中的 defer 声明或迁移 COMMENT；无 schema（00000076-78 未建相关表/列）、无 API 端点、无 UI 页面。测试 w0_schema_contract_tests 另做断言守护 ✓。Channel pinned 置顶是既有功能与 workspace Pinned 无关 ✓。
- **Workspace Channel 视图无聊天输入框**: workspace_channel_detail_page.dart 结构 = 讨论引导横幅 + ChannelDetailPage（Publish 模型），无 ChatInput 引用；测试锚点 `workspace-channel-detail-dm-entry` 固化契约 ✓。
- **Pinned/Overview 不聚合 Group Notice**: 后端 overview 仅返回 project_count/group_count/channel_count/member_preview ✓；Flutter WorkspaceOverview 注释显式声明不聚合并渲染空态 ✓。
- **Activity 无消息正文**: W0 无 Activity 端点；project_event 表唯一写入方为 project_task_ds:change_status（event_type='task_status'，payload 仅 from/to/actor，同事务原子写入，grep 验证无其他 writer）✓。
- **守卫挂载**: channel 系列（handler/message/comment/admin/webhook by token 校验后）+ group detail/msg_page + group_notice 全部挂 resolver 门；msg_c2g_repo 写消息事务首语句 FOR UPDATE 锁 workspace 行 ✓。group_member_handler join_with_capacity 顺带修复了「丢弃 with_tx 结果假成功」的存量 bug（改 409 workspace_membership_required）✓。A2 声明的 ~10 条未接入路径抽查属实（attachment TODO/webhook 入站/bot token）✓。

### 栏 3 · 一致性专项

- **with_tx 契约**: 全部 27 个 with_tx 调用文件扫描——create_template 裸 map 匹配、invite 三元组 `{ok,changed,_}`、archive/restore `{ok,Map}`、subscription changed/noop、group_member JoinTx error 归一全部正确 ✓；唯一漏网为 F4（badmatch 降级 500，非 crash）。
- **update/3→4**: grep 全仓 5 处调用点均为 update(Table, Map, WhereSql, WhereParams) 四参签名 ✓ 无漏网。
- **resolver 角色归一**: ensure_member_ok 归一 `{ok,_Role} → ok` 契约一致 ✓。
- **TSID 链路**: Erlang 注册 workspace/project/project_task/project_event 生成器（workspace_member 复合主键有意不注册，注释说明）✓；Flutter EntityId=String 包装 + entityIdOf(int/num/String 均入) 全链无 int.parse 回转 ✓；Admin EntityId string 无 Number(id) 回转 ✓。Admin/C 端下发形态差异见 F10。

### 附带验证

- 路由: `/api/v1/workspaces/mine` 注册于 `:workspace_id` 通配之前防遮蔽；全部走 JWT 默认门；admin 面 adm_acl fail-closed（workspaces:read/update）+ adm_operation_log 审计（异常吞掉不阻断主流程，注释了理由）✓。
- 迁移 00000076-78: IF NOT EXISTS 幂等式、XOR CHECK（scope/workspace_id 互斥）、DEFERRABLE INITIALLY DEFERRED 双触发器、down 文件顺序注释清楚、"group" 保留字双引号处理 ✓。
- demo 脚本 dual_exp_demo_b.sh: PGPASSWORD 环境注入无硬编码、trap 清理临时文件、自管退出码（set -u，无 set -e 属风格选择）✓。
- error_code.hrl 980 注册与守卫唯一错误码闭环 ✓；product_experience fail-safe chat 与 product_profile fail-closed 语义刻意区分并有注释论证 ✓。

---

## 三、结论

**不存在 CRITICAL finding；HIGH 清单存在（F1 ×1）。**

- **建议进入修复批次的项**: F1（HIGH，一次修复 + 测试锚点即可）
- **建议同批顺手修的项**: F2、F3、F4（后端三处，改动局部且有现成模式可套）
- **可择期/批量处理**: F5-F10（规范收敛类）

评估: 整体质量高于平均水平——分层、幂等、权限矩阵、defer 纪律、TSID 精度链路执行到位；WP8 的五类缺陷经全量复扫未见同型残留（F4 为弱同型变体）。F1 是本次 diff 中唯一可能产生用户可见随机行为的缺陷。
