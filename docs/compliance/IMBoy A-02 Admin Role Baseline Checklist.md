# IMBoy A-02 Admin/Moderator Role Baseline Checklist

> 实施计划 Task A-02。前置：A-01 特权消息访问加固（已完成）。
> 状态：**已实施（2026-09-07）**。

## Goal（计划原文要点）

种子最小权限角色（Platform Admin / Moderator / Security Admin / Support），不替换 Workspace/Group 角色；禁止自我授权/提权；敏感导出与 IP 访问分离。验收=权限矩阵回答各角色能否 report-review、user-data view、content removal、ban/unban、IP view、export、evidence、policy、audit logs；每角色负向测试。

## 实施内容

### 种子角色（imboy/src/adm/adm_index_handler.erl role_acl/1）

| id | 角色 | 定位 | 关键权限 | 明确不授 |
|---|---|---|---|---|
| 1 | super_admin | Platform Admin（既有） | 全集 | — |
| 2 | ops_admin | 业务运营（既有） | 业务读写 | messages:content:read / export、admins:* 、roles:update |
| 3 | audit_admin | 审计（既有） | 只读+日志 | 同上 |
| 4 | **moderator**（新） | 内容审核 | reports:handle、moments:report:handle/delete、messages:metadata:read、feedback:read | messages:content:read、export、users:update/delete、roles:update、admins:assign_role |
| 5 | **security_admin**（新） | 安全治理 | users:update（ban/unban）、messages:content:read（A-01 工单制取证）、logs:view、roles:view、logout_applications:read | messages:export、feedback:reply、moments:delete、admins:assign_role、roles:update |
| 6 | **support**（新） | 客服 | users:read、feedback:reply、messages:metadata:read | messages:content:read、export、reports:handle、users:update |

- 内置角色集合 1..6：`builtin_role_ids/0`，停用/删除守卫（validate_mutable_role、role_exists）同步扩展。
- 角色权限矩阵正负断言进 adm_index_handler_tests（moderator/security/support 三用例，含最小权限负向项）。

### 防自我提权（三道门，全部后端强制）

1. **角色分配**（adm_admin_handler:guard_assign_escalation/3）：
   - 不能给自己分配角色（self-grant 通道封死）；
   - 目标角色权限集 ⊆ 操作者自身权限集——自定义角色即使持 `admins:assign_role` 也无法 assign 出超自己权限的角色（含 role 1）。
2. **权限保存**（adm_role_handler:guard_permission_escalation/3）：
   - 不能修改自己所属角色的权限；
   - 新权限集 ⊆ 操作者自身权限集（角色 1 权限锚定拒绝仍最优先）。
3. **停用管理员**（adm_admin_handler:disable_action）：
   - 不能停用自己（自锁通道封死）；
   - **修复真缺陷**：旧实现直连 `adm_user_ds:update` 绕过了 `adm_user_logic:update_status` 的「不能禁用超级管理员」守卫——已改走 Logic 层（同时修复分层违规）。

### 侧边栏/路由白名单同步

- 后端 default_sidebar_config：dashboard/users/moments/reports/feedback/messages/groups-context/logout-applications/logs/roles 十个菜单条目 roles 按矩阵扩展 4/5/6。
- 前端 App.tsx PermissionRoute 白名单同步同口径；RolePermissionPage SYSTEM_ROLE_IDS 扩到 1..6 并补三个角色模板（与后端权限集一致，super_admin 模板自动全量继承含 A-01 三新键）。

### 敏感分离现状

- 导出：`messages:export` 已独立（A-01）；用户数据导出（P-01）尚未建——将来必须独立键，不复用 read。
- IP 访问：当前后台无 IP 查看暴露面（登录日志在 user_log/login_attempt 表，未开 admin 端点）；将来若开须独立键（如 `security:ip:read`）并默认不授任何内置角色。

## 测试证据（2026-09-07）

| 门 | 结果 |
|---|---|
| adm_index_handler_tests | **11/11**（+3 角色矩阵正负用例） |
| adm_role_handler_tests | **10/10**（+2：own-role 拒绝 / superset 拒绝） |
| adm_admin_handler_tests | **11/11**（+4：assign self 拒 / assign superset 拒 / disable self 拒 / disable 走 logic 守卫且不触 ds） |
| adm_acl_tests（回归） | 16/16 |
| imboyadmin tsc / test | 干净 / 1441 全绿 |

## 已知边界与后续

- 已存在的自定义角色若持 `roles:update`/`admins:assign_role`，其行为会被新守卫收紧（只能授子集）——预期行为，对 super_admin 无影响。
- R-04 申诉链（LEGAL 门控）、user-data export（P-01）、IP 审计面均未建；本清单的"分离"指键位预留与不越界原则。
- Admin UI 浏览器端到端走查（新角色登录→菜单可见性→403 边界）待后续批次。
