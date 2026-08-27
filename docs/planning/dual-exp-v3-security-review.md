# V3 独立安全 Review — imboy dual-exp-v21（双体验 v2.5.2）

- Reviewer 角色：程序性隔离的独立安全审查（未参与实现），只审不改
- 对象：分支 `dual-exp-v21` vs `main`（HEAD=79bc2ee8），101 文件 / +13791 −579
- 日期：2026-08-27 | 环境：本地 9800（imboy_dev@127.0.0.1，release 1.0.0-alpha.69）+ imboy_v1@127.0.0.1:4323
- 排除且未触碰：主工作区 E2EE WIP 文件（SECURITY.md / vm.args / msg_* / olm_* 等 M 状态文件）
- 测试数据已自清理（workspace 109339938524497920、用户 C 109340077068650496、B 的 personal channel 全部删除；演示账号 A/B 未动）

---

## 0. 结论行

**CRITICAL：无。HIGH：2 项（均为功能可用性断裂 + 连接池稳定性，非权限绕过类安全问题）。MEDIUM：1。LOW：2。**

安全边界本身（Workspace Member 边界、Guest 只读、子集约束、admin fail-closed、init 白名单、SQL 注入面）经实测与静态复核全部成立，未发现任何越权读写路径。

---

## 1. Findings

| # | Severity | 位置（file:line / 复现命令） | 影响 | 建议修法 |
|---|----------|------------------------------|------|----------|
| F1 | **HIGH**（功能断裂，非安全越权） | `src/repo/channel_repo.erl:228-237` 实现 `list_workspace_channels/2` 但 `-export` 段（L7-33）漏导出。复现：`curl -H "Authorization:$TA" http://127.0.0.1:9800/api/v1/workspaces/<ws_id>/channels` → 空 body HTTP 500（Owner/Guest 同）；error.log：`call to undefined function channel_repo:list_workspace_channels(...)`；对 HEAD 干净 `erlc` 后 `module_info(exports)` 亦无该项 | Workspace Experience 主列表端点对所有角色稳定 500，请求进程连带 lager handler 崩溃 | export 列表补 `-export([list_workspace_channels/2]).`（一行）；建议加“repo 函数必须被 export 或显式私有”的编译期检查/dialyzer 门 |
| F2 | **HIGH**（功能断裂 + 连接池稳定性） | `src/ds/project_task_ds.erl:250-252` `normalize_assignee(_)` 返回 atom `nil` 作为 SQL 参数。复现：member token `POST /api/v1/projects/:pid/tasks` body `{"title":"x"}`（不带 assignee_id）→ HTTP 500；error.log：`{integer_overflow,int8,nil}` 于 `epgsql_codec_integer:encode`，epgsql 连接进程 crash（后续 ROLLBACK noproc）。带 `"assignee_id":>0` 则创建成功（实测 id=109341705326823424） | “创建任务”默认最常见路径（不指派）全量不可用；每次失败还打掉一根 epgsql 连接（pool 有耗尽连锁风险）。数据完整性靠 PG 会话终止自动回滚保住 | NULL 参数改用 epgsql 的 `null` 语义（或 elib_pg_sql 统一 NULL 占位）；spec 中 `nil` 改 `null`；补一条“无 assignee 创建任务”的非 mock E2E 断言 |
| F3 | MEDIUM | 归档后入群未拒。复现序列（实测）：owner `POST /api/v1/workspaces/:id/archive` → 成功后 member `POST /api/v1/group_member/join {"gid":<ws群>,"member_uids":[self]}` → `code:0` 入群成功 | archived 工作区内仍可建立新的群成员关系，违背“归档冻结协作写”语义；`dual-exp-known-limitations.md` §A2 只点名 attachment/webhook/bot，“group join 是否在内”因 A2 引用的 WP4 清单未落盘而无法核对 | `group_member_logic` join/add 写路径接 `workspace_guard:ensure_writable({group,Gid})`；并将已知取舍 A2 清单落盘逐条核对 |
| F4 | LOW | `src/ds/workspace_ds.erl:414-421`、`src/ds/project_ds.erl:158-170` admin 搜索 keyword 以 `"%"+kw+"%"` 参数化 ILIKE，未转义 `%`/`_` 通配符 | 仅搜索语义/性能滥用（kw=`%%` 全表模糊匹配），**不是 SQL 注入**（值始终走 `$n` 参数） | 如需精确语义：转义 `%`,`_`,`\`后再包裹通配符 |
| F5 | LOW | `src/adm/adm_workspace_handler.erl:60` dispatch catch-all `dispatch(_, _Method, Req0, _State) -> Req0.` 对未知 action 返回原始 Req0（无响应体） | 不构成越权（路由表只注册 7 个合法 action）；仅产生空/异常响应，不利于排障 | fallback 显式 `elib_response:error(Req0, <<"Not Found">>, 404)` |

### 环境备注（不计 findings）
本地 release 的 beam 相对 HEAD 陈旧（如缺 list_workspace_channels、旧版 project_task_repo/group_notice_logic），本次审查中对两处 HIGH 均以 **HEAD 干净源码独立编译/静态复核** 排除了“仅环境陈旧”的可能后才定级；F1 已通过对 HEAD 版 erlc + exports 反证确认。多轮 WP 会话共享同一常驻节点、依赖手工热加载 beam 的做法易造成“绿灯假象”，建议部署演练统一走干净重建。

---

## 2. 专项清单逐项取证

### ① Workspace Member 边界 — PASS
用户 C（uid=109340077068650496，非成员）直 ID 访问 workspace `109339938524497920` / General 群 `109339938534983680` / Announcements 频道 `109339938570635264`：

读侧（均 `{"code":403,"msg":"非工作区成员，禁止访问该资源"}`）：workspaces/:id detail、overview、members、channels、groups、branding GET、group/detail?gid、group/msg_page?gid、channel/:id show、channel/:id/messages。
写侧（均 403）：workspace update/invite/archive/transfer_owner、POST projects、channel/:id/message POST、subscribe、group_notice/add。
by_custom_id 直访通道：`guard_channel_custom_id` 对命中 workspace scope 的频道同样执行成员边界（未命中则走既有 404，实测 nonexistent → “频道不存在”）。
handler 接线：`src/lib/workspace_resolver.erl`（resolver + ensure_channel/group_member_access）+ 各 handler init/guard；非成员也无法自邀（invite 被 Owner 门 403）。

### ② 子集约束三层证据 — PASS
(a) DB 触发器（pg_trigger 实查 imboy_v1）：
```
trg_group_member_ws_subset        ON group_member       (constraint trigger, DEFERRABLE INITIALLY DEFERRED)
trg_workspace_member_remove_guard ON workspace_member   (constraint trigger)
chk_channel_scope_xor / chk_group_scope_xor (CHECK)，fk_*_workspace 外键
```
(b) 应用层 409：非成员 C `POST /api/v1/group_member/join {"gid":<ws群>,"member_uids":[C]}` → `{"code":409,"msg":"workspace_membership_required：群成员必须先是该工作区的 active 工作区成员"}`。
(c) 并发竞争测试文件存在：`test/integration/workspace_archive_concurrency_tests.erl`（提交 c132da43，场景 A/B 先拿锁者胜 + 980 断言；真库真事务 FOR UPDATE 线性化）。本次审查未复跑 eunit（需完整构建环境），对其断言逻辑做静态复核一致。

### ③ admin fail-open 反例扫描 — PASS（fail-closed）
- `src/adm/adm_workspace_handler.erl`：list/detail/members（`workspaces:read`）、archive/restore（`workspaces:update`）、project_list/project_detail（`workspaces:read`）7 个 action **逐一** `adm_acl:ensure_permission` 前置；dispatch fallback 见 F5。
- `adm_acl:ensure_permission`：`has_permission` 要求 `AdmUserId > 0`（缺失/0 → false → 403）；`permissions/1` DB 异常时显式返回 `[]` → 403。**无 adm_user_id 即 403，无静默放行路径。**
- `adm_admin_handler config_product_experience`：GET only + `settings:view`；无任何运行时写接口（纯展示 configured/effective/source）。
- 实测：无凭据 & 普通 user JWT 调 `/api/adm/workspace/list|archive`、`/api/adm/admin/config/product-experience` 全部 `{"code":706,"msg":"Need to log in again"}`。

### ④ 归档守卫边界 — PASS（附 F3）
Owner archive → 之后实测：project 创建 **980**、task 创建 **980**、频道发帖（Owner，穿过既有订阅/admin 校验后到达 guard）**980**、group_notice add（Owner）**980**、消息文本 envelope 均 `{"code":980,"msg":"工作区已归档，写操作被拒绝"}`。
personal 对照：archived 期间 member B 创建 personal channel `code:0` 正常（guard 对 personal 直通，无误伤）。
restore → active 后 Owner 发帖恢复 `code:0`。
例外见 F3（join 未接守卫）。事务级证明由 `ensure_writable_tx`（SELECT…FOR UPDATE，锁查询失败 fail-closed 503）+ 场景②(c) 测试承担。

### ⑤ SQL 注入面 — PASS
全部新增 repo/ds/logic 逐一扫描：用户可控输入一律 `$n` 参数化（含 ILIKE 的 keyword）；拼接进 SQL 文本的标识符仅来自 `*_repo:tablename()` 内部常量（`"group"` 双引号处理正确）、内部字面量 Where/ExtraWhere、`normalize_admin_status` 白名单（仅认 active/archived/all）与 `join_int_ids` 整数拼接（F8 类，integer_to_binary 类型保证）。F8 类整数拼接共 4 处（workspace_ds:443/463-482、project_ds:182 等）均已确认为 int-only。唯一低危残留为 F4 通配符。

### ⑥ 越权改角色 / 转移 Owner — PASS
Member(B) 调 invite/remove/role/archive/transfer_owner → 全部 403「仅工作区 Owner 可执行该操作」；
Guest 自提权 role=owner → 403；
Owner transfer_owner 目标 Guest → 409「主 Owner 转移目标不能是 Guest」（目标已是 owner/非 active 亦分别 409）；
最后 Owner 保护：change_role 使 owner 数 <1 → 409「至少保留一名 Owner」（静态复核 change_role_checked + count_by_role）。

### ⑦ config_version/init 白名单 — PASS
`src/api/index_handler.erl:76-82`：Data map 新增键仅 `effective_product_experience`（product_experience:effective_binary()，输出域 {chat,workspace}）与 `config_version`（sha256 前 16 hex），无其他 application env 键透出；feed 具体 product_experience.erl 的 normalize 对非法值降级 chat（fail-safe）。抽复核 T1 eunit（test/lib/product_experience_tests.erl 140 行，含 golden 向量）在位。

### ⑧ known-limitations §A 如实性 — PASS
§A1 「检查-写窗口」表述与代码一致：`channel_logic_common:guard_channel_writable/1` → `workspace_guard:ensure_writable/1`（自动提交版）确实用于 comment(5 处)/reaction/message(reaction 4 处)/subscription(2 处)/stats(2 处) 等 REST 写 logic 层前置（spot check channel_comment_logic:43-51 create_comment 中 guard 先于 do_create_comment；channel_logic_subscription:38-45 subscribe 先于 subscribe_by_join_policy），模块头注释明示窗口风险与归属 WP4 报告；tx 版接线于 project_ds/project_task_ds/channel_ds/msg_c2g_repo 四个主写入口（保证表结构与调用点核实）。§A2 的“约 10 条未接入路径”清单因指向未落盘的 WP4 会话报告无法逐条核对（与 F3 相关性见上）。

---

## 3. CRITICAL/HIGH 明确清单

- CRITICAL：**无**
- HIGH：
  - F1 `channel_repo:list_workspace_channels/2` 漏 export → workspace 频道列表全员 500（HEAD 实锤）
  - F2 `project_task_ds:normalize_assignee` 返回 atom `nil` → 无 assignee 建任务必 500 且崩连接（HEAD 实锤）

两项均为安全隔离之外的功能/稳定性断裂，建议合并前必修（各为一行级修复），并补非 mock 的端到端断言防回归。

---
*审查方法：静态审阅（diff 全量 stat + 关键 12 文件精读 + grep 拼接/fail-open 模式）+ 本地 9800 黑盒 curl 序列 50+ 用例 + psql 直接取证；运行节点缺口模块用 HEAD 干净源码独立编译复核排除环境污染。未修改任何仓库内文件（热修复仅注入内存/ebin 副本用于解除阻塞，报告交付前测试数据已清理）。*
