# IMBoy A-01 Privileged Message Access Hardening Checklist

> 实施计划 [IMBoy Overseas Compliance Implementation Plan.md](./IMBoy%20Overseas%20Compliance%20Implementation%20Plan.md) Task A-01。
> Gap Matrix 对应行：Admin RBAC（export 共享 read 权限、无职责分离）、Admin access audit（消息读取无逐次审计）。
> 状态：**后端+Admin UI 已实施（2026-09-07），A-02 角色基线未做**。

## Goal（计划原文要点）

消除无审计的宽私信读取：默认元数据口径；拆分 `messages:metadata:read` / `messages:content:read` / `messages:export`；内容访问须绑定 case/ticket + reason；list/detail/export 逐次不可变审计；绝不暴露 E2EE 明文。

## 实施内容（全部完成）

### 权限三分（src/adm/adm_index_handler.erl + adm_message_handler.erl）

- [x] `messages:read`：保留为**元数据级**读（历史语义，向后兼容，存量角色自动降级为最小口径）。
- [x] `messages:metadata:read`：显式别名，与 `messages:read` 等效（ensure_metadata_permission 二次回退）。
- [x] `messages:content:read`：内容级（payload）读，独立权限。
- [x] `messages:export`：CSV 导出，独立于读权限（此前导出复用 messages:read——即 Gap Matrix 指出的共享缺陷）。
- [x] super_admin（role 1）授予全部三键；ops_admin（2）/audit_admin（3）保持仅 `messages:read`——**收权是有意的职责分离**（审计角色不再能导出）。

### 工单制内容门（resolve_content_access/3）

- [x] 内容可见 = policy `audit_mode=full` **且** 具 `messages:content:read` **且** 请求携带有效举报工单 `ticket`（report_ticket 存在性校验）**且** `reason` 非空。
- [x] 任一条件不满足 → 静默降级 metadata（payload 置空），不报错（默认元数据口径）。
- [x] 携带 ticket 但缺 reason → 硬错误「查看消息内容必须附处理原因」（不静默降级，避免操作者误以为已读到内容）。
- [x] 携带 ticket 但工单不存在 → 硬错误「举报工单不存在」。
- [x] 行为收紧注记：导出在 `audit_mode=none` 下旧实现会输出全量 payload，新实现一律仅元数据（fail-safe 方向）。

### 逐次访问审计（audit_access/4 → adm_operation_log_ds:insert/6）

- [x] list / detail / export 三端点每次成功授权的访问都写 `admin_operation_logs`：
  action = `message_list_access` / `message_detail_access` / `message_export_access`，
  detail 含 `effective_mode`、`content_accessed`、`ticket_id`、`reason`、`total`（list/export）、`filters` 摘要（scope/uid/会话/时间窗/关键词）、`msg_id`（detail）。
- [x] **审计写失败 = fail-closed**：拒绝本次访问（「审计写入失败，访问被拒绝」），不返回任何数据；export 在流式输出前先计数+审计，审计失败不吐任何字节。
- [x] 审计摘要不含任何消息 payload。

### E2EE 边界

- [x] 内容门返回的 payload 为服务端存量密文信封（strict 模式），本任务不新增任何服务端解密能力。

### Admin UI（imboyadmin 仓）

- [x] RolePermissionPage 权限目录补三键并澄清 `messages:read` 为仅元数据。
- [x] MessageListPage 筛选栏新增「举报工单ID / 处理原因」输入，随 list/detail/export 请求透传（`MessageListParams.ticket/reason`）。

## 测试证据（2026-09-07）

| 门 | 结果 |
|---|---|
| `make eunit-local t=adm_message_handler_tests` | **21/21**（新增 8：alias 放行降级 / 工单+原因放行+审计断言 / 缺 content 权限降级 / 缺原因硬错误 / 工单不存在硬错误 / 审计失败 fail-closed+零数据查询 / 导出权限独立 403 / 旧 permission_denied 兼容） |
| adm_index_handler_tests / adm_acl_tests / adm_admin_handler_tests | 8/8 + 16/16 + 7/7 |
| imboyadmin `tsc --noEmit` | 干净 |
| imboyadmin `bun run test` | 1441/1441 |
| imboyadmin `bun run lint` | 本批文件干净；**既有红**：ReportActionPanel.test.tsx 6 处 no-unused-vars（HEAD 上已存在，非本批引入，未动） |

## 已知边界与后续

- **测试坑**（复核配方）：`.eunit/` 陈旧 app beam 会遮蔽新编译产物（本批 6 个假失败均由它起）——改 src 后单跑先 `rm .eunit/<mod>*.beam`；meck 同名同 arity 多条目仅最后一条生效，多子句须合并单 fun。
- **A-02（Admin/Moderator 角色基线）未做**：least-privilege 种子角色、self-grant 禁止、权限矩阵 UI 负向测试——下一可执行项。
- R-04（申诉，LEGAL 门控）未做；内容访问与举报工单的**深度绑定**（消息须归属于工单对象）属 R 线后续强化，本批只做存在性校验。
- 真机/浏览器端到端走查（工单输入 → 内容解锁 → 审计行落库）待用户或后续批次。
